package net.cucumbersome.proboscis.parser

import net.cucumbersome.proboscis.Token
import net.cucumbersome.proboscis.lexer.Lexer

data class TokenPosition(val position: Int, val line: Int, val column: Int) {
  constructor(lexer: Lexer) : this(lexer.position, lexer.line, lexer.column)
}

sealed interface Node {
  val token: Token
  val tokenPosition: TokenPosition
}

sealed interface Expression : Node

data class Identifier(val value: String, override val token: Token, override val tokenPosition: TokenPosition) :
  Expression

data class IntegerLiteral(val value: Int, override val token: Token, override val tokenPosition: TokenPosition) :
  Expression

enum class PrefixOperator(val value: String) {
  BANG("!"),
  MINUS("-");

  companion object {
    fun fromToken(token: Token): PrefixOperator? = when (token) {
      Token.Companion.Bang -> BANG
      Token.Companion.Minus -> MINUS
      else -> null
    }
  }
}

enum class InfixOperator(val value: String) {
  PLUS("+"),
  MINUS("-"),
  ASTERISK("*"),
  SLASH("/"),
  LESS_THAN("<"),
  GREATER_THAN(">"),
  EQUAL("=="),
  NOT_EQUAL("!=");

  companion object {
    fun fromToken(token: Token): InfixOperator? = when (token) {
      Token.Companion.Plus -> PLUS
      Token.Companion.Minus -> MINUS
      Token.Companion.Asterisk -> ASTERISK
      Token.Companion.Slash -> SLASH
      Token.Companion.LessThan -> LESS_THAN
      Token.Companion.GreaterThan -> GREATER_THAN
      Token.Companion.Equal -> EQUAL
      Token.Companion.NotEqual -> NOT_EQUAL
      else -> null
    }
  }
}

data class InfixExpression(
  val left: Expression,
  val operator: InfixOperator,
  val right: Expression,
  override val token: Token,
  override val tokenPosition: TokenPosition
) : Expression

data class PrefixExpression(
  val operator: PrefixOperator,
  val right: Expression,
  override val token: Token,
  override val tokenPosition: TokenPosition
) : Expression

sealed interface Statement : Node

class LetStatement(
  val name: Identifier,
  val value: Expression,
  override val token: Token,
  override val tokenPosition: TokenPosition
) : Statement

class ReturnStatement(
  val returnValue: Expression,
  override val token: Token,
  override val tokenPosition: TokenPosition
) : Statement

class Program(val statements: List<Node>)
