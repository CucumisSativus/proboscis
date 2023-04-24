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
      TokenPosition(position = 0, line = 1, column = 0)
    )

    val obtainedStatement = statements[0]
    if (obtainedStatement !is Identifier) {
      throw AssertionError("Statement at index 0 is not an Identifier")
    }

    assertEquals(obtainedStatement, expectedExpression)
  }
}
