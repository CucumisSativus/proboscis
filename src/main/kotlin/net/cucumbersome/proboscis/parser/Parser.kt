package net.cucumbersome.proboscis.parser

import arrow.core.Either
import net.cucumbersome.proboscis.Token
import net.cucumbersome.proboscis.lexer.Lexer

data class ParserError(val message: String, val position: TokenPosition) {
  override fun toString(): String = "$message at line ${position.line}, column ${position.column}"
}

sealed interface ParseStatementResult<T : Node> {
  val lexer: Lexer

  fun <U : Node> flatMap(f: (statement: T, lexer: Lexer) -> ParseStatementResult<out U>): ParseStatementResult<out U> =
    when (this) {
      is SuccessfulParseStatementResult -> f(this.statement, this.lexer)
      is FailedParseStatementResult -> this
    }

  fun mapError(f: (errors: List<ParserError>, lexer: Lexer) -> ParseStatementResult<out T>): ParseStatementResult<out T> =
    when (this) {
      is SuccessfulParseStatementResult -> this
      is FailedParseStatementResult -> f(this.errors, this.lexer)
    }
}

data class SuccessfulParseStatementResult<T : Node>(val statement: T, override val lexer: Lexer) :
  ParseStatementResult<T>

data class FailedParseStatementResult(val errors: List<ParserError>, override val lexer: Lexer) :
  ParseStatementResult<Nothing>

data class ProgramParseError(val errors: List<ParserError>)

class Parser(val lexer: Lexer) {
  fun parseProgram(): Either<ProgramParseError, Program> {
    tailrec fun parseProgram(
      currentLexer: Lexer,
      statements: List<Node>,
      errors: List<ParserError>
    ): Either<ProgramParseError, Program> {
      val (token, newLexer) = currentLexer.nextToken()
      return when (token) {
        Token.Companion.Eof -> if (errors.isEmpty()) {
          Either.Right(Program(statements))
        } else {
          Either.Left(ProgramParseError(errors))
        }

        Token.Companion.Let -> {
          when (val parseResult = parseLetStatement(newLexer)) {
            is SuccessfulParseStatementResult -> parseProgram(
              parseResult.lexer,
              statements + parseResult.statement,
              errors
            )

            is FailedParseStatementResult -> parseProgram(parseResult.lexer, statements, errors + parseResult.errors)
          }
        }

        Token.Companion.Return -> {
          when (val parseResult = parseReturnStatement(newLexer)) {
            is SuccessfulParseStatementResult -> parseProgram(
              parseResult.lexer,
              statements + parseResult.statement,
              errors
            )

            is FailedParseStatementResult -> parseProgram(parseResult.lexer, statements, errors + parseResult.errors)
          }
        }

        else -> {
          when (val parseResult = parseExpressionStatement(currentLexer)) {
            is SuccessfulParseStatementResult -> parseProgram(
              parseResult.lexer,
              statements + parseResult.statement,
              errors
            )

            is FailedParseStatementResult -> parseProgram(parseResult.lexer, statements, errors + parseResult.errors)
          }
        }
      }
    }
    return parseProgram(lexer, emptyList(), emptyList())
  }

  private fun parseReturnStatement(lexer: Lexer): ParseStatementResult<out ReturnStatement> {
    return parseExpressionStatement(lexer).flatMap { expression, newLexer ->
      SuccessfulParseStatementResult(
        ReturnStatement(expression, Token.Companion.Return, TokenPosition(lexer)),
        newLexer
      )
    }
  }

  private fun parseLetStatement(lexer: Lexer): ParseStatementResult<out LetStatement> {
    val errors = mutableListOf<ParserError>()
    val (identifierToken, newLexer) = lexer.nextToken()
    if (identifierToken !is Token.Companion.Identifier) {
      errors.add(ParserError("Expected identifier, got $identifierToken", TokenPosition(lexer)))
    }

    val (equalToken, newLexer2) = newLexer.nextToken()
    if (equalToken != Token.Companion.Assign) {
      errors.add(ParserError("Expected =, got $equalToken", TokenPosition(newLexer2)))
    }

    val parseExpressionResult = parseExpressionStatement(newLexer2)
    if (parseExpressionResult is FailedParseStatementResult) {
      errors.addAll(parseExpressionResult.errors)
      return FailedParseStatementResult(errors, parseExpressionResult.lexer)
    }
    val (expression, newLexer3) = parseExpressionResult as SuccessfulParseStatementResult<out Expression>

    return if (errors.isNotEmpty() || identifierToken !is Token.Companion.Identifier) {
      FailedParseStatementResult(errors, newLexer3)
    } else {
      SuccessfulParseStatementResult(
        LetStatement(
          Identifier(identifierToken.value, identifierToken, TokenPosition(lexer)),
          expression,
          Token.Companion.Let,
          TokenPosition(lexer)
        ),
        newLexer3
      )
    }
  }

  private fun parseExpressionStatement(lexer: Lexer): ParseStatementResult<out Expression> {
    return parseExpression(lexer, Precedence.Lowest).flatMap { expression, newLexer ->
      if (newLexer.nextTokenIs(Token.Companion.Semicolon)) {
        SuccessfulParseStatementResult(expression, newLexer.nextToken().second)
      } else {
        SuccessfulParseStatementResult(expression, newLexer)
      }
    }
  }

  private fun parseExpression(lexer: Lexer, precedence: Precedence): ParseStatementResult<out Expression> {
    val prefix = getPrefix(lexer)
    if (prefix is FailedParseStatementResult) {
      return prefix
    }
    val left = prefix as SuccessfulParseStatementResult<out Expression>

    tailrec fun iterate(iterationLexer: Lexer, leftExpression: Expression): ParseStatementResult<out Expression> {
      if (iterationLexer.nextTokenIs(Token.Companion.Semicolon) || precedence >= nextPrecedence(iterationLexer)) {
        return SuccessfulParseStatementResult(leftExpression, iterationLexer)
      }
      val (operatorToken, lexerAfterOperator) = iterationLexer.nextToken()
      if (InfixOperator.fromToken(operatorToken) == null) {
        return SuccessfulParseStatementResult(leftExpression, iterationLexer)
      }
      val newLeft = parseInfixExpression(iterationLexer, leftExpression)
      return if (newLeft is SuccessfulParseStatementResult) {
        iterate(newLeft.lexer, newLeft.statement)
      } else {
        newLeft
      }
    }

    return iterate(left.lexer, left.statement)
  }

  private fun getPrefix(lexer: Lexer): ParseStatementResult<out Expression> {
    val (token, newLexer) = lexer.nextToken()
    return when (token) {
      is Token.Companion.Bang, is Token.Companion.Minus -> parsePrefixExpression(lexer)
      is Token.Companion.Identifier -> SuccessfulParseStatementResult(
        Identifier(token.value, token, TokenPosition(newLexer)),
        newLexer
      )

      is Token.Companion.IntValue -> SuccessfulParseStatementResult(
        IntegerLiteral(token.value, token, TokenPosition(newLexer)),
        newLexer
      )

      is Token.Companion.True -> SuccessfulParseStatementResult(
        BooleanLiteral(true, token, TokenPosition(newLexer)),
        newLexer
      )

      is Token.Companion.False -> SuccessfulParseStatementResult(
        BooleanLiteral(false, token, TokenPosition(newLexer)),
        newLexer
      )

      is Token.Companion.LeftParen -> parseGroupedExpression(newLexer)
      else -> FailedParseStatementResult(
        listOf(ParserError("Unexpected prefix token $token", TokenPosition(lexer))),
        newLexer
      )
    }
  }

  private fun parseGroupedExpression(lexer: Lexer): ParseStatementResult<out Expression> {
    val result = parseExpression(lexer, Precedence.Lowest)
    if (result is FailedParseStatementResult) {
      return result
    }

    return result.flatMap { expression, newLexer ->
      if (!newLexer.nextTokenIs(Token.Companion.RightParen)) {
        FailedParseStatementResult(
          listOf(ParserError("Expected ), got ${newLexer.nextToken().first}", TokenPosition(newLexer))),
          newLexer
        )
      }
      SuccessfulParseStatementResult(expression, newLexer.nextToken().second)
    }
  }

  private fun parsePrefixExpression(lexer: Lexer): ParseStatementResult<out PrefixExpression> {
    val (token, newLexer) = lexer.nextToken()
    val operator = PrefixOperator.fromToken(token) ?: return FailedParseStatementResult(
      listOf(ParserError("Unexpected prefix token $token", TokenPosition(lexer))),
      newLexer
    )

    return parseExpression(newLexer, Precedence.Prefix).flatMap { expression, newLexer2 ->
      SuccessfulParseStatementResult(
        PrefixExpression(operator, expression, token, TokenPosition(newLexer)),
        newLexer2
      )
    }
  }

  private fun parseInfixExpression(lexer: Lexer, left: Expression): ParseStatementResult<out InfixExpression> {
    val (token, newLexer) = lexer.nextToken()
    val operator = InfixOperator.fromToken(token) ?: return FailedParseStatementResult(
      listOf(ParserError("Unexpected infix token $token", TokenPosition(lexer))),
      newLexer
    )
    val precedence = infixOperatorPrecedence(operator)
    val parseExpressionResult = parseExpression(newLexer, precedence)

    return parseExpressionResult.flatMap { expression, newLexer2 ->
      SuccessfulParseStatementResult(
        InfixExpression(left, operator, expression, token, TokenPosition(newLexer)),
        newLexer2
      )
    }
  }

  private fun nextPrecedence(lexer: Lexer): Precedence {
    val operator = InfixOperator.fromToken(lexer.nextToken().first)
    return if (operator != null) {
      infixOperatorPrecedence(operator)
    } else {
      Precedence.Lowest
    }
  }

  companion object {
    enum class Precedence(val value: Int) {
      Lowest(0), Equals(1), LessGreater(2), Sum(3), Product(4), Prefix(5), Call(6)
    }

    fun infixOperatorPrecedence(infixOperator: InfixOperator): Precedence {
      return when (infixOperator) {
        InfixOperator.PLUS, InfixOperator.MINUS -> Precedence.Sum
        InfixOperator.ASTERISK, InfixOperator.SLASH -> Precedence.Product
        InfixOperator.LESS_THAN, InfixOperator.GREATER_THAN -> Precedence.LessGreater
        InfixOperator.EQUAL, InfixOperator.NOT_EQUAL -> Precedence.Equals
      }
    }
  }
}
