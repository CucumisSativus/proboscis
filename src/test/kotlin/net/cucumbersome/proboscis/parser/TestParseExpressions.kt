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

  @Test
  fun parseIfExpression() {
    val input = "if (x < y) { x }"
    val expectedExpression = IfExpression(
      InfixExpression(
        Identifier("x", Token.Companion.Identifier("x"), TokenPosition(position = 5, line = 1, column = 5)),
        InfixOperator.LESS_THAN,
        Identifier("y", Token.Companion.Identifier("y"), TokenPosition(position = 9, line = 1, column = 9)),
        Token.Companion.LessThan,
        TokenPosition(position = 7, line = 1, column = 7)
      ),
      BlockStatement(
        listOf(
          Identifier("x", Token.Companion.Identifier("x"), TokenPosition(position = 14, line = 1, column = 14))
        ),
        Token.Companion.LeftBrace,
        TokenPosition(position = 12, line = 1, column = 12)
      ),
      null,
      Token.Companion.If,
      TokenPosition(position = 2, line = 1, column = 2)
    )
    val obtainedNodes = ParserHelper.getProgram(input).statements
    assertEquals(1, obtainedNodes.size)
    val obtainedExpression = obtainedNodes[0]
    if (obtainedExpression !is IfExpression) {
      throw AssertionError("Statement is not an IfExpression")
    }
    assertEquals(expectedExpression.condition, obtainedExpression.condition)
    assertEquals(expectedExpression.consequence, obtainedExpression.consequence)
    assertEquals(expectedExpression.alternative, obtainedExpression.alternative)
    assertEquals(expectedExpression.token, obtainedExpression.token)
    assertEquals(expectedExpression.tokenPosition, obtainedExpression.tokenPosition)
  }

  @Test
  fun testIfExpressionWithAlternative() {
    val input = "if (x < y) { x } else { y }"
    val expectedExpression = IfExpression(
      InfixExpression(
        Identifier("x", Token.Companion.Identifier("x"), TokenPosition(position = 5, line = 1, column = 5)),
        InfixOperator.LESS_THAN,
        Identifier("y", Token.Companion.Identifier("y"), TokenPosition(position = 9, line = 1, column = 9)),
        Token.Companion.LessThan,
        TokenPosition(position = 7, line = 1, column = 7)
      ),
      BlockStatement(
        listOf(
          Identifier("x", Token.Companion.Identifier("x"), TokenPosition(position = 14, line = 1, column = 14))
        ),
        Token.Companion.LeftBrace,
        TokenPosition(position = 12, line = 1, column = 12)
      ),
      BlockStatement(
        listOf(
          Identifier("y", Token.Companion.Identifier("y"), TokenPosition(position = 25, line = 1, column = 25))
        ),
        Token.Companion.LeftBrace,
        TokenPosition(position = 23, line = 1, column = 23)
      ),
      Token.Companion.If,
      TokenPosition(position = 2, line = 1, column = 2)
    )
    val obtainedNodes = ParserHelper.getProgram(input).statements
    assertEquals(1, obtainedNodes.size)
    val obtainedExpression = obtainedNodes[0]
    if (obtainedExpression !is IfExpression) {
      throw AssertionError("Statement is not an IfExpression")
    }
    assertEquals(expectedExpression.condition, obtainedExpression.condition)
    assertEquals(expectedExpression.consequence, obtainedExpression.consequence)
    assertEquals(expectedExpression.alternative, obtainedExpression.alternative)
    assertEquals(expectedExpression.token, obtainedExpression.token)
    assertEquals(expectedExpression.tokenPosition, obtainedExpression.tokenPosition)
  }

  @Test
  fun testParseFunctionLiteral() {
    val input = "fn(x, y) { x + y; }"
    val expectedLiteral = FunctionLiteral(
      listOf(
        Identifier("x", Token.Companion.Identifier("x"), TokenPosition(position = 4, line = 1, column = 4)),
        Identifier("y", Token.Companion.Identifier("y"), TokenPosition(position = 7, line = 1, column = 7))
      ),
      BlockStatement(
        listOf(
          InfixExpression(
            Identifier("x", Token.Companion.Identifier("x"), TokenPosition(position = 12, line = 1, column = 12)),
            InfixOperator.PLUS,
            Identifier("y", Token.Companion.Identifier("y"), TokenPosition(position = 16, line = 1, column = 16)),
            Token.Companion.Plus,
            TokenPosition(position = 14, line = 1, column = 14)
          )
        ),
        Token.Companion.LeftBrace,
        TokenPosition(position = 10, line = 1, column = 10)
      ),
      Token.Companion.Function,
      TokenPosition(position = 2, line = 1, column = 2)
    )
    val obtainedNodes = ParserHelper.getProgram(input).statements
    assertEquals(1, obtainedNodes.size)
    val obtainedLiteral = obtainedNodes[0]
    if (obtainedLiteral !is FunctionLiteral) {
      throw AssertionError("Statement is not a FunctionLiteral")
    }
    assertEquals(expectedLiteral.parameters, obtainedLiteral.parameters)
    assertEquals(expectedLiteral.body, obtainedLiteral.body)
    assertEquals(expectedLiteral.token, obtainedLiteral.token)
    assertEquals(expectedLiteral.tokenPosition, obtainedLiteral.tokenPosition)
  }

  @Test
  fun testFunctionArguments() {
    val tests = listOf(
      Pair("fn() {};", listOf<String>()),
      Pair("fn(x) {};", listOf("x")),
      Pair("fn(x, y, z) {};", listOf("x", "y", "z"))
    )

    tests.forEachIndexed { testIndex, (input, expected) ->
      val program = ParserHelper.getProgram(input)
      assertEquals(1, program.statements.size)
      val statement = program.statements[0]
      if (statement !is FunctionLiteral) {
        throw AssertionError("Statement is not a FunctionLiteral ($testIndex)")
      }
      assertEquals(expected.size, statement.parameters.size)
      expected.forEachIndexed { parameterIndex, identifier ->
        assertEquals(identifier, statement.parameters[parameterIndex].value, "Parameter $parameterIndex ($testIndex)")
      }
    }
  }
}
