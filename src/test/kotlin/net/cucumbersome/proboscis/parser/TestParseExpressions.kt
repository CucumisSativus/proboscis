package net.cucumbersome.proboscis.parser

import net.cucumbersome.proboscis.Token
import kotlin.test.Test
import kotlin.test.assertEquals

class TestParseExpressions {
  @Test
  fun parseIdentifiers() {
    val input = "foobar;"

    val statements = ParserHelper.getProgram(input).statements
    assertEquals(1, statements.size)

    val expectedExpression = Identifier(
      "foobar",
      Token.Companion.Identifier("foobar"),
      TokenPosition(position = 6, line = 1, column = 6)
    )

    val obtainedStatement = statements[0]
    if (obtainedStatement !is Identifier) {
      throw AssertionError("Statement at index 0 is not an Identifier")
    }

    assertEquals(obtainedStatement, expectedExpression)
  }

  @Test
  fun parsePrefixExpressions() {
    val input = """
      !5;
      -15;
      !foobar;
      -foobar;
    """.trimIndent()

    val statements = ParserHelper.getProgram(input).statements
    assertEquals(4, statements.size)

    val tests = listOf(
      PrefixExpression(
        PrefixOperator.BANG,
        IntegerLiteral(5, Token.Companion.IntValue(5), TokenPosition(position = 2, line = 1, column = 2)),
        Token.Companion.Bang,
        TokenPosition(position = 1, line = 1, column = 1)
      ),
      PrefixExpression(
        PrefixOperator.MINUS,
        IntegerLiteral(15, Token.Companion.IntValue(15), TokenPosition(position = 7, line = 2, column = 4)),
        Token.Companion.Minus,
        TokenPosition(position = 5, line = 2, column = 2)
      ),
      PrefixExpression(
        PrefixOperator.BANG,
        Identifier(
          "foobar",
          Token.Companion.Identifier("foobar"),
          TokenPosition(position = 16, line = 3, column = 8)
        ),
        Token.Companion.Bang,
        TokenPosition(position = 10, line = 3, column = 2)
      ),
      PrefixExpression(
        PrefixOperator.MINUS,
        Identifier(
          "foobar",
          Token.Companion.Identifier("foobar"),
          TokenPosition(position = 25, line = 4, column = 8)
        ),
        Token.Companion.Minus,
        TokenPosition(position = 19, line = 4, column = 2)
      )
    )

    tests.forEachIndexed { index, operator ->
      val statement = statements[index]
      if (statement !is PrefixExpression) {
        throw AssertionError("Statement at index $index is not a PrefixExpression")
      }
      assertEquals(operator.operator, statement.operator, "Operator at index $index is not the expected one")
      assertEquals(operator.token, statement.token, "Token at index $index is not the expected one")
      assertEquals(
        operator.tokenPosition,
        statement.tokenPosition,
        "TokenPosition at index $index is not the expected one"
      )
      assertEquals(operator.right, statement.right, "Right expression at index $index is not the expected one")
    }
  }
}
