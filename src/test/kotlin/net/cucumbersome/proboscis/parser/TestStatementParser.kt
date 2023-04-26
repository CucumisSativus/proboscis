package net.cucumbersome.proboscis.parser

import net.cucumbersome.proboscis.Token
import net.cucumbersome.proboscis.parser.ParserHelper.getProgram
import net.cucumbersome.proboscis.parser.ParserHelper.testErrorHandling
import kotlin.test.Test
import kotlin.test.assertEquals

class TestStatementParser {
  @Test
  fun testParsingLetStatements() {
    val input = """
            let x = 5;
            let y = 10;
            let foobar = 838383;
    """.trimIndent()

    val statements = getProgram(input).statements
    assertEquals(3, statements.size)
    val tests = listOf(
      "x" to Pair(5, 9),
      "y" to Pair(10, 21),
      "foobar" to Pair(838383, 42)
    )
    tests.forEachIndexed { index, (expectedIdentifier, expectedValue) ->
      val statement = statements[index]
      if (statement !is LetStatement) {
        throw AssertionError("Statement at index $index is not a LetStatement")
      }
      assertEquals(Token.Companion.Let, statement.token)
      assertEquals(expectedIdentifier, statement.name.value)
      if (statement.value !is IntegerLiteral) {
        throw AssertionError("Value of statement at index $index is not an IntegerLiteral")
      }
      assertEquals(expectedValue.first, (statement.value as IntegerLiteral).value)
      assertEquals(expectedValue.second, statement.value.tokenPosition.position)
    }
  }

  @Test
  fun testParsingInvalidInput() {
    val input = """
            let x 5;
            let = 10;
            let 838383;
    """.trimIndent()

    val expectedErrors: List<ParserError> = listOf(
      ParserError("Expected =, got IntValue(value=5)", TokenPosition(position = 7, line = 1, column = 7)),
      ParserError("Unexpected token ;", TokenPosition(position = 7, line = 1, column = 7)),
      ParserError("Expected identifier, got =", TokenPosition(position = 12, line = 2, column = 4)),
      ParserError("Expected =, got IntValue(value=10)", TokenPosition(position = 17, line = 2, column = 9)),
      ParserError("Unexpected token ;", TokenPosition(position = 17, line = 2, column = 9)),
      ParserError(
        "Expected identifier, got IntValue(value=838383)",
        TokenPosition(position = 22, line = 3, column = 4)
      ),
      ParserError("Expected =, got ;", TokenPosition(position = 30, line = 3, column = 12)),
      ParserError("Unexpected token EOF", TokenPosition(position = 30, line = 3, column = 12))
    )

    return testErrorHandling(input, expectedErrors)
  }

  @Test
  fun testParsingReturnStatementOnSimpleInput() {
    val input = """
            return 5;
            return 10;
            return 838383;
    """.trimIndent()

    val statements = getProgram(input).statements

    assertEquals(3, statements.size)
    val tests = listOf(
      5 to 8,
      10 to 19,
      838383 to 34
    )
    tests.forEachIndexed { index, (expectedValue, expectedPosition) ->
      val statement = statements[index]
      if (statement !is ReturnStatement) {
        throw AssertionError("Statement at index $index is not a ReturnStatement")
      }
      assertEquals(Token.Companion.Return, statement.token)
      if (statement.returnValue !is IntegerLiteral) {
        throw AssertionError("Value of statement at index $index is not an IntegerLiteral")
      }
      assertEquals(expectedValue, (statement.returnValue as IntegerLiteral).value)
      assertEquals(expectedPosition, statement.returnValue.tokenPosition.position)
    }
  }

  @Test
  fun testParsinReturnStatementError() {
    val input = """
      return return;
      return let;
    """.trimIndent()

    val expectedErrors: List<ParserError> = listOf(
      ParserError(message = "Unexpected token return", position = TokenPosition(position = 6, line = 1, column = 6)),
      ParserError(message = "Unexpected token ;", position = TokenPosition(position = 13, line = 1, column = 13)),
      ParserError(message = "Unexpected token let", position = TokenPosition(position = 21, line = 2, column = 7)),
      ParserError(message = "Unexpected token ;", position = TokenPosition(position = 25, line = 2, column = 11))
    )

    return testErrorHandling(input, expectedErrors)
  }
}
