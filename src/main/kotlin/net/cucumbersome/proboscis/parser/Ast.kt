package net.cucumbersome.proboscis.parser

import net.cucumbersome.proboscis.Token

sealed interface Node {
  val token: Token
}

sealed interface Expression : Node

class Identifier(val value: String, override val token: Token) : Expression

class IntegerLiteral(val value: Int, override val token: Token) : Expression

sealed interface Statement : Node

class LetStatement(val name: Identifier, val value: Expression, override val token: Token) : Statement

class Program(val statements: List<Statement>)
