package net.cucumbersome.proboscis.parser

import net.cucumbersome.proboscis.Token
import net.cucumbersome.proboscis.lexer.Lexer

enum class Precedence(val value: Int) {
  Lowest(0), Equals(1), LessGreater(2), Sum(3), Product(4), Prefix(5), Call(6);

  companion object {
    fun infixOperatorPrecedence(infixOperator: InfixOperator): Precedence {
      return when (infixOperator) {
        InfixOperator.PLUS, InfixOperator.MINUS -> Sum
        InfixOperator.ASTERISK, InfixOperator.SLASH -> Product
        InfixOperator.LESS_THAN, InfixOperator.GREATER_THAN -> LessGreater
        InfixOperator.EQUAL, InfixOperator.NOT_EQUAL -> Equals
      }
    }

    fun nextPrecedence(lexer: Lexer): Precedence {
      val token = lexer.nextToken().first
      val operator = InfixOperator.fromToken(token)
      return if (operator != null) {
        infixOperatorPrecedence(operator)
      } else if (token == Token.Companion.LeftParen) {
        Call
      } else {
        Lowest
      }
    }
  }
}
