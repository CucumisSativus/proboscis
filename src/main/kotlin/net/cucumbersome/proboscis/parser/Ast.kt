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

class Identifier(val value: String, override val token: Token, override val tokenPosition: TokenPosition) : Expression

class IntegerLiteral(val value: Int, override val token: Token, override val tokenPosition: TokenPosition) : Expression

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

class Program(val statements: List<Statement>)
