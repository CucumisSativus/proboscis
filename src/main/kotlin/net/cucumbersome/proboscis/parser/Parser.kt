package net.cucumbersome.proboscis.parser

import arrow.core.Either
import net.cucumbersome.proboscis.Token
import net.cucumbersome.proboscis.lexer.Lexer

data class ParserError(val message: String, val position: TokenPosition)

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
      statements: List<Statement>,
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

        else -> throw AssertionError("Unexpected token $token")
      }
    }
    return parseProgram(lexer, emptyList(), emptyList())
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

    val parseExpressionResult = parseExpression(newLexer2)
    if (parseExpressionResult is FailedParseExpressionResult) {
      errors.addAll(parseExpressionResult.errors)
      return FailedParseStatementResult(errors, parseExpressionResult.lexer)
    }
    val (expression, newLexer3) = parseExpressionResult as SuccessfulParseExpressionResult<out Expression>
    val (semicolonToken, newLexer4) = newLexer3.nextToken()
    if (semicolonToken != Token.Companion.Semicolon) {
      errors.add(ParserError("Expected ;, got $semicolonToken", TokenPosition(newLexer3)))
    }
    return if (errors.isNotEmpty() || identifierToken !is Token.Companion.Identifier) {
      FailedParseStatementResult(errors, newLexer4)
    } else {
      SuccessfulParseStatementResult(
        LetStatement(
          Identifier(identifierToken.value, identifierToken, TokenPosition(lexer)),
          expression,
          Token.Companion.Let,
          TokenPosition(lexer)
        ),
        newLexer4
      )
    }
  }

  private fun parseExpression(lexer: Lexer): ParseExpressionResult<out Expression> {
    val (token, newLexer) = lexer.nextToken()
    return when (token) {
      is Token.Companion.IntValue -> SuccessfulParseExpressionResult(
        IntegerLiteral(token.value, token, TokenPosition(lexer)),
        newLexer
      )

      else -> FailedParseExpressionResult(
        listOf(ParserError("Unexpected token $token", TokenPosition(lexer))),
        newLexer
      )
    }
  }
}
