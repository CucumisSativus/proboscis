package net.cucumbersome.proboscis.parser

import net.cucumbersome.proboscis.Token
import net.cucumbersome.proboscis.lexer.Lexer

class Parser(val lexer: Lexer) {
  fun parseProgram(): Program {
    tailrec fun parseProgram(currentLexer: Lexer, statements: List<Statement>): List<Statement> {
      val (token, newLexer) = currentLexer.nextToken()
      return when (token) {
        Token.Companion.Eof -> statements
        Token.Companion.Let -> {
          val (statement, usedLexer) = parseLetStatement(newLexer)
          parseProgram(usedLexer, statements + statement)
        }
        else -> throw AssertionError("Unexpected token $token")
      }
    }
    return Program(parseProgram(lexer, emptyList()))
  }
  private fun parseLetStatement(lexer: Lexer): Pair<LetStatement, Lexer> {
    val (identifierToken, newLexer) = lexer.nextToken()
    if (identifierToken !is Token.Companion.Identifier) {
      throw AssertionError("Expected identifier, got $identifierToken at position ${lexer.position}")
    }

    val (equalToken, newLexer2) = newLexer.nextToken()
    if (equalToken != Token.Companion.Assign) {
      throw AssertionError("Expected =, got $equalToken")
    }

    val (expression, newLexer3) = parseExpression(newLexer2)
    val (semicolonToken, newLexer4) = newLexer3.nextToken()
    if (semicolonToken != Token.Companion.Semicolon) {
      throw AssertionError("Expected ;, got $semicolonToken at position ${newLexer3.position}")
    }
    return Pair(
      LetStatement(
        Identifier(identifierToken.value, identifierToken, TokenPosition(lexer)),
        expression,
        Token.Companion.Let,
        TokenPosition(lexer)
      ),
      newLexer4
    )
  }

  private fun parseExpression(lexer: Lexer): Pair<Expression, Lexer> {
    val (token, newLexer) = lexer.nextToken()
    return when (token) {
      is Token.Companion.IntValue -> Pair(IntegerLiteral(token.value, token, TokenPosition(lexer)), newLexer)
      else -> throw AssertionError("Unexpected token $token")
    }
  }
}
