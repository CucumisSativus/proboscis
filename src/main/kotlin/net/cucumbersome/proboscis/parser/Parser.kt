package net.cucumbersome.proboscis.parser

import arrow.core.Either
import net.cucumbersome.proboscis.Token
import net.cucumbersome.proboscis.lexer.Lexer
import net.cucumbersome.proboscis.parser.Precedence.Companion.infixOperatorPrecedence
import net.cucumbersome.proboscis.parser.Precedence.Companion.nextPrecedence

class Parser(val lexer: Lexer) {
  fun parseProgram(): Either<ProgramParseError, Program> {
    tailrec fun parseProgram(
      currentLexer: Lexer,
      statements: List<Node>,
      errors: List<ParserError>
    ): Either<ProgramParseError, Program> {
      return when (currentLexer.nextToken().first) {
        Token.Companion.Eof -> if (errors.isEmpty()) {
          Either.Right(Program(statements))
        } else {
          Either.Left(ProgramParseError(errors))
        }

        else -> {
          when (val parseResult = parseStatement(currentLexer)) {
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

  private fun parseStatement(lexer: Lexer): ParseStatementResult<out Node> {
    val (token, newLexer) = lexer.nextToken()
    return when (token) {
      Token.Companion.Let -> parseLetStatement(newLexer)
      Token.Companion.Return -> parseReturnStatement(newLexer)
      Token.Companion.LeftBrace -> parseBlockStatement(newLexer)
      else -> parseExpressionStatement(lexer)
    }
  }

  private fun parseBlockStatement(lexer: Lexer): ParseStatementResult<out BlockStatement> {
    tailrec fun iterate(
      acc: List<ParseStatementResult<out Node>>,
      currentLexer: Lexer
    ): List<ParseStatementResult<out Node>> {
      return when (currentLexer.nextToken().first) {
        Token.Companion.RightBrace, Token.Companion.Eof -> acc
        else -> {
          val parseStatementResult = parseStatement(currentLexer)
          iterate(acc + parseStatementResult, parseStatementResult.lexer)
        }
      }
    }

    val statements = iterate(emptyList(), lexer)

    val initial: ParseStatementResult<out BlockStatement> = SuccessfulParseStatementResult(
      BlockStatement(emptyList(), Token.Companion.LeftBrace, TokenPosition(lexer)),
      lexer
    )

    return statements.fold(initial) { acc, statement ->
      when (statement) {
        is SuccessfulParseStatementResult -> acc.flatMap { blockStatement, _ ->
          SuccessfulParseStatementResult(
            blockStatement.copy(statements = blockStatement.statements + statement.statement),
            statement.lexer
          )
        }

        is FailedParseStatementResult -> acc.appendErrors(statement.errors)
      }
    }.flatMap { blockStatement, lexerOnRightBrace ->
      SuccessfulParseStatementResult(blockStatement, lexerOnRightBrace.nextToken().second)
    }
  }

  private fun parseIfExpression(lexer: Lexer): ParseStatementResult<out IfExpression> {
    val errors = mutableListOf<ParserError>()

    val (leftParen, newLexer) = lexer.nextToken()
    if (leftParen != Token.Companion.LeftParen) {
      errors.add(ParserError("Expected (, got $leftParen", TokenPosition(lexer)))
    }

    val condition = parseExpressionStatement(newLexer)

    val (rightParen, newLexer2) = condition.lexer.nextToken()
    if (rightParen != Token.Companion.RightParen) {
      errors.add(ParserError("Expected ), got $rightParen", TokenPosition(condition.lexer)))
    }

    val (leftBrace, newLexer3) = newLexer2.nextToken()
    if (leftBrace != Token.Companion.LeftBrace) {
      errors.add(ParserError("Expected {, got $leftBrace", TokenPosition(newLexer2)))
    }

    val consequence = parseBlockStatement(newLexer3)
    val (elseToken, newLexer4) = consequence.lexer.nextToken()

    val res = if (elseToken == Token.Companion.Else) {
      val (elseLeftBrace, newLexer5) = newLexer4.nextToken()
      if (elseLeftBrace != Token.Companion.LeftBrace) {
        errors.add(ParserError("Expected {, got $elseLeftBrace", TokenPosition(newLexer4)))
      }
      val alternative = parseBlockStatement(newLexer5)
      map3(
        condition,
        consequence,
        alternative
      ) { con, cons, alt ->
        IfExpression(
          con,
          cons,
          alt,
          Token.Companion.If,
          TokenPosition(lexer)
        )
      }
    } else {
      map2(
        condition,
        consequence
      ) { con, cons ->
        IfExpression(
          con,
          cons,
          null,
          Token.Companion.If,
          TokenPosition(lexer)
        )
      }
    }
    return res.appendErrors(errors)
  }

  private fun parseFunctionLiteral(lexer: Lexer): ParseStatementResult<out FunctionLiteral> {
    val errors = mutableListOf<ParserError>()
    val (leftParen, newLexer) = lexer.nextToken()
    if (leftParen != Token.Companion.LeftParen) {
      errors.add(ParserError("Expected (, got $leftParen", TokenPosition(lexer)))
    }

    val parameters = parseParameterList(newLexer)

    val (leftBrace, newLexer2) = parameters.lexer.nextToken()
    if (leftBrace != Token.Companion.LeftBrace) {
      errors.add(ParserError("Expected {, got $leftBrace", TokenPosition(newLexer)))
    }
    return map2(parameters, parseBlockStatement(newLexer2)) { params, body ->
      FunctionLiteral(
        params,
        body,
        Token.Companion.Function,
        TokenPosition(lexer)
      )
    }.appendErrors(errors)
  }

  private fun parseParameterList(lexer: Lexer): ParseStatementResult<out List<Identifier>> {
    fun iterate(
      acc: List<Identifier>,
      currentLexer: Lexer
    ): ParseStatementResult<out List<Identifier>> {
      val (token, newLexer) = currentLexer.nextToken()
      return when (token) {
        Token.Companion.RightParen -> SuccessfulParseStatementResult(acc, newLexer)
        Token.Companion.Eof -> FailedParseStatementResult(
          listOf(ParserError("Expected ), got $token", TokenPosition(currentLexer))),
          currentLexer
        )

        Token.Companion.Comma -> iterate(acc, newLexer)
        is Token.Companion.Identifier ->
          iterate(acc + Identifier(token.value, token, TokenPosition(newLexer)), newLexer)

        else ->
          FailedParseStatementResult(
            listOf(ParserError("Expected identifier, got $token", TokenPosition(newLexer))),
            currentLexer
          )
      }
    }

    return iterate(emptyList(), lexer)
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
      val (operatorToken, _) = iterationLexer.nextToken()
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
      is Token.Companion.If -> parseIfExpression(newLexer)
      is Token.Companion.Function -> parseFunctionLiteral(newLexer)
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
}
