package net.cucumbersome.proboscis.parser

import arrow.core.Either
import net.cucumbersome.proboscis.Token
import net.cucumbersome.proboscis.lexer.Lexer

data class ParserError(val message: String, val position: TokenPosition) {
  override fun toString(): String = "$message at line ${position.line}, column ${position.column}"
}

sealed interface ParseStatementResult<T : Statement> {
  val lexer: Lexer
}

data class SuccessfulParseStatementResult<T : Statement>(val statement: T, override val lexer: Lexer) :
  ParseStatementResult<T>

data class FailedParseStatementResult(val errors: List<ParserError>, override val lexer: Lexer) :
  ParseStatementResult<Nothing>

sealed interface ParseExpressionResult<T : Expression> {
  val lexer: Lexer
}

data class SuccessfulParseExpressionResult<T : Expression>(val expression: T, override val lexer: Lexer) :
  ParseExpressionResult<T>

data class FailedParseExpressionResult(val errors: List<ParserError>, override val lexer: Lexer) :
  ParseExpressionResult<Nothing>

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
          val parseResult = parseLetStatement(newLexer)
          when (parseResult) {
            is SuccessfulParseStatementResult -> parseProgram(
              parseResult.lexer,
              statements + parseResult.statement,
              errors
            )

            is FailedParseStatementResult -> parseProgram(parseResult.lexer, statements, errors + parseResult.errors)
          }
        }

        Token.Companion.Return -> {
          val parseResult = parseReturnStatement(newLexer)
          when (parseResult) {
            is SuccessfulParseStatementResult -> parseProgram(
              parseResult.lexer,
              statements + parseResult.statement,
              errors
            )

            is FailedParseStatementResult -> parseProgram(parseResult.lexer, statements, errors + parseResult.errors)
          }
        }

        else -> {
          val parseResult = parseExpressionStatement(currentLexer)
          when (parseResult) {
            is SuccessfulParseExpressionResult -> parseProgram(
              parseResult.lexer,
              statements + parseResult.expression,
              errors
            )

            is FailedParseExpressionResult -> parseProgram(parseResult.lexer, statements, errors + parseResult.errors)
          }
        }
      }
    }
    return parseProgram(lexer, emptyList(), emptyList())
  }

  private fun parseReturnStatement(lexer: Lexer): ParseStatementResult<out ReturnStatement> {
    val errors = mutableListOf<ParserError>()
    val parseExpressionResult = parseExpressionStatement(lexer)
    if (parseExpressionResult is FailedParseExpressionResult) {
      errors.addAll(parseExpressionResult.errors)
      return FailedParseStatementResult(errors, parseExpressionResult.lexer)
    }
    val (expression, newLexer) = parseExpressionResult as SuccessfulParseExpressionResult<out Expression>
    return SuccessfulParseStatementResult(
      ReturnStatement(expression, Token.Companion.Return, TokenPosition(lexer)),
      newLexer
    )
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
    if (parseExpressionResult is FailedParseExpressionResult) {
      errors.addAll(parseExpressionResult.errors)
      return FailedParseStatementResult(errors, parseExpressionResult.lexer)
    }
    val (expression, newLexer3) = parseExpressionResult as SuccessfulParseExpressionResult<out Expression>

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

  private fun parseExpressionStatement(lexer: Lexer): ParseExpressionResult<out Expression> {
    val parseExpressionResult = parseExpression(lexer, Precedence.Lowest)
    if (parseExpressionResult is FailedParseExpressionResult) {
      return parseExpressionResult
    }
    val (expression, newLexer) = parseExpressionResult as SuccessfulParseExpressionResult<out Expression>
    return if (newLexer.nextTokenIs(Token.Companion.Semicolon)) {
      SuccessfulParseExpressionResult(expression, newLexer.nextToken().second)
    } else {
      SuccessfulParseExpressionResult(expression, newLexer)
    }
  }

  private fun parseExpression(lexer: Lexer, precedence: Precedence): ParseExpressionResult<out Expression> {
    val (token, newLexer) = lexer.nextToken()
    return when (token) {
      is Token.Companion.Bang, is Token.Companion.Minus -> parsePrefixExpression(lexer)
      is Token.Companion.Identifier -> SuccessfulParseExpressionResult(
        Identifier(token.value, token, TokenPosition(newLexer)),
        newLexer
      )

      is Token.Companion.IntValue -> SuccessfulParseExpressionResult(
        IntegerLiteral(token.value, token, TokenPosition(newLexer)),
        newLexer
      )

      else -> FailedParseExpressionResult(
        listOf(ParserError("Unexpected token $token", TokenPosition(lexer))),
        newLexer
      )
    }
  }

  private fun parsePrefixExpression(lexer: Lexer): ParseExpressionResult<out PrefixExpression> {
    val (token, newLexer) = lexer.nextToken()
    val operator = PrefixOperator.fromToken(token)
      ?: return FailedParseExpressionResult(
        listOf(ParserError("Unexpected prefix token $token", TokenPosition(lexer))),
        newLexer
      )
    val parseExpressionResult = parseExpression(newLexer, Precedence.Prefix)
    if (parseExpressionResult is FailedParseExpressionResult) {
      return parseExpressionResult
    }
    val (expression, newLexer2) = parseExpressionResult as SuccessfulParseExpressionResult<out Expression>
    return SuccessfulParseExpressionResult(
      PrefixExpression(operator, expression, token, TokenPosition(newLexer)),
      newLexer2
    )
  }

  companion object {
    enum class Precedence(val value: Int) {
      Lowest(0),
      Equals(1),
      LessGreater(2),
      Sum(3),
      Product(4),
      Prefix(5),
      Call(6)
    }
  }
}
