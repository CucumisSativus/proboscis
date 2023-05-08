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

    tests.forEachIndexed { index, expectedOperator ->
      val statement = statements[index]
      if (statement !is PrefixExpression) {
        throw AssertionError("Statement at index $index is not a PrefixExpression")
      }
      assertEquals(expectedOperator.operator, statement.operator, "Operator at index $index is not the expected one")
      assertEquals(expectedOperator.token, statement.token, "Token at index $index is not the expected one")
      assertEquals(
        expectedOperator.tokenPosition,
        statement.tokenPosition,
        "TokenPosition at index $index is not the expected one"
      )
      assertEquals(expectedOperator.right, statement.right, "Right expression at index $index is not the expected one")
    }
  }

  @Test
  fun testInfixExpressions() {
    val input = """
      5 + 5;
      5 - 5;
      5 * 5;
      5 / 5;
      5 > 5;
      5 < 5;
      5 == 5;
      5 != 5;
    """.trimIndent()

    val expectedExpressions = listOf(
      InfixExpression(
        IntegerLiteral(5, Token.Companion.IntValue(5), TokenPosition(position = 1, line = 1, column = 1)),
        InfixOperator.PLUS,
        IntegerLiteral(5, Token.Companion.IntValue(5), TokenPosition(position = 5, line = 1, column = 5)),
        Token.Companion.Plus,
        TokenPosition(position = 3, line = 1, column = 3)
      ),
      InfixExpression(
        IntegerLiteral(5, Token.Companion.IntValue(5), TokenPosition(position = 8, line = 2, column = 2)),
        InfixOperator.MINUS,
        IntegerLiteral(5, Token.Companion.IntValue(5), TokenPosition(position = 12, line = 2, column = 6)),
        Token.Companion.Minus,
        TokenPosition(position = 10, line = 2, column = 4)
      ),
      InfixExpression(
        IntegerLiteral(5, Token.Companion.IntValue(5), TokenPosition(position = 15, line = 3, column = 2)),
        InfixOperator.ASTERISK,
        IntegerLiteral(5, Token.Companion.IntValue(5), TokenPosition(position = 19, line = 3, column = 6)),
        Token.Companion.Asterisk,
        TokenPosition(position = 17, line = 3, column = 4)
      ),
      InfixExpression(
        IntegerLiteral(5, Token.Companion.IntValue(5), TokenPosition(position = 22, line = 4, column = 2)),
        InfixOperator.SLASH,
        IntegerLiteral(5, Token.Companion.IntValue(5), TokenPosition(position = 26, line = 4, column = 6)),
        Token.Companion.Slash,
        TokenPosition(position = 24, line = 4, column = 4)
      ),
      InfixExpression(
        IntegerLiteral(5, Token.Companion.IntValue(5), TokenPosition(position = 29, line = 5, column = 2)),
        InfixOperator.GREATER_THAN,
        IntegerLiteral(5, Token.Companion.IntValue(5), TokenPosition(position = 33, line = 5, column = 6)),
        Token.Companion.GreaterThan,
        TokenPosition(position = 31, line = 5, column = 4)
      ),
      InfixExpression(
        IntegerLiteral(5, Token.Companion.IntValue(5), TokenPosition(position = 36, line = 6, column = 2)),
        InfixOperator.LESS_THAN,
        IntegerLiteral(5, Token.Companion.IntValue(5), TokenPosition(position = 40, line = 6, column = 6)),
        Token.Companion.LessThan,
        TokenPosition(position = 38, line = 6, column = 4)
      ),
      InfixExpression(
        IntegerLiteral(5, Token.Companion.IntValue(5), TokenPosition(position = 43, line = 7, column = 2)),
        InfixOperator.EQUAL,
        IntegerLiteral(5, Token.Companion.IntValue(5), TokenPosition(position = 48, line = 7, column = 7)),
        Token.Companion.Equal,
        TokenPosition(position = 46, line = 7, column = 5)
      ),
      InfixExpression(
        IntegerLiteral(5, Token.Companion.IntValue(5), TokenPosition(position = 51, line = 8, column = 2)),
        InfixOperator.NOT_EQUAL,
        IntegerLiteral(5, Token.Companion.IntValue(5), TokenPosition(position = 56, line = 8, column = 7)),
        Token.Companion.NotEqual,
        TokenPosition(position = 54, line = 8, column = 5)
      )
    )

    val statements = ParserHelper.getProgram(input).statements
    assertEquals(expectedExpressions.size, statements.size)

    expectedExpressions.forEachIndexed { index, expectedExpression ->
      val statement = statements[index]
      if (statement !is InfixExpression) {
        throw AssertionError("Statement at index $index is not an InfixExpression")
      }
      assertEquals(expectedExpression.left, statement.left, "Left expression at index $index is not the expected one")
      assertEquals(expectedExpression.operator, statement.operator, "Operator at index $index is not the expected one")
      assertEquals(
        expectedExpression.right,
        statement.right,
        "Right expression at index $index is not the expected one"
      )
      assertEquals(expectedExpression.token, statement.token, "Token at index $index is not the expected one")
      assertEquals(
        expectedExpression.tokenPosition,
        statement.tokenPosition,

        "TokenPosition at index $index is not the expected one"
      )
    }
  }

  @Test
  fun testPrecedenceParsing() {
    val tests = listOf(
      "-a * b",
      "!-a",
      "a + b + c",
      "a + b - c",
      "a * b * c",
      "a * b / c",
      "a + b / c",
      "a + b * c + d / e - f",
      "3 + 4; -5 * 5",
      "5 > 4 == 3 < 4",
      "5 < 4 != 3 > 4",
      "3 + 4 * 5 == 3 * 1 + 4 * 5",
      "true",
      "false",
      "3 > 5 == false",
      "3 < 5 == true",
      "1 + (2 + 3) + 4",
      "(5 + 5) * 2",
      "2 / (5 + 5)",
      "-(5 + 5)",
      "!(true == true)"
    )

    val expectedOutputs = listOf(
      "((-a) * b)",
      "(!(-a))",
      "((a + b) + c)",
      "((a + b) - c)",
      "((a * b) * c)",
      "((a * b) / c)",
      "(a + (b / c))",
      "(((a + (b * c)) + (d / e)) - f)",
      "(3 + 4)((-5) * 5)",
      "((5 > 4) == (3 < 4))",
      "((5 < 4) != (3 > 4))",
      "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
      "true",
      "false",
      "((3 > 5) == false)",
      "((3 < 5) == true)",
      "((1 + (2 + 3)) + 4)",
      "((5 + 5) * 2)",
      "(2 / (5 + 5))",
      "(-(5 + 5))",
      "(!(true == true))"
    )

    tests.forEachIndexed { index, test ->
      val program = ParserHelper.getProgram(test)
      val actual = program.present()
      assertEquals(expectedOutputs[index], actual)
    }
  }
}
