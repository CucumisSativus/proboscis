package net.cucumbersome.proboscis.parser

import net.cucumbersome.proboscis.Token
import net.cucumbersome.proboscis.lexer.Lexer
import kotlin.test.Test
import kotlin.test.assertEquals

class TestParser {
  @Test
  fun testParsingLetStatements() {
    val input = """
            let x = 5;
            let y = 10;
            let foobar = 838383;
    """.trimIndent()
    val lexer = Lexer.fromString(input)!!
    val parser = Parser(lexer)
    val program = parser.parseProgram()

    val statements = program.fold(
      { throw AssertionError("Error when parsing input $it") },
      { it.statements }
    )
    assertEquals(3, statements.size)
    val tests = listOf(
      "x" to Pair(5, 7),
      "y" to Pair(10, 18),
      "foobar" to Pair(838383, 35)
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
    val lexer = Lexer.fromString(input)!!
    val parser = Parser(lexer)
    val program = parser.parseProgram()
    val errors = program.fold(
      { it.errors },
      { throw AssertionError("Expected parsing to fail, but it succeeded") }
    )
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
    assertEquals(expectedErrors.size, errors.size)
    expectedErrors.forEachIndexed { index, (expectedMessage, expectedPosition) ->
      val error = errors[index]
      assertEquals(expectedMessage, error.message)
      assertEquals(expectedPosition, error.position)
    }
  }

  @Test
  fun testParsingReturnStatementOnSimpleInput() {
    val input = """
            return 5;
            return 10;
            return 838383;
    """.trimIndent()
    val lexer = Lexer.fromString(input)!!
    val parser = Parser(lexer)
    val program = parser.parseProgram()

    val statements = program.fold(
      { throw AssertionError("Error when parsing input $it") },
      { it.statements }
    )
    assertEquals(3, statements.size)
    val tests = listOf(
      5 to 6,
      10 to 16,
      838383 to 27
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
    val lexer = Lexer.fromString(input)!!
    val parser = Parser(lexer)
    val program = parser.parseProgram()
    val errors = program.fold(
      { it.errors },
      { throw AssertionError("Expected parsing to fail, but it succeeded") }
    )

    val expectedErrors: List<ParserError> = listOf(
      ParserError(message = "Unexpected token return", position = TokenPosition(position = 6, line = 1, column = 6)),
      ParserError(message = "Unexpected token ;", position = TokenPosition(position = 14, line = 1, column = 14)),
      ParserError(message = "Unexpected token let", position = TokenPosition(position = 21, line = 2, column = 7)),
      ParserError(message = "Unexpected token ;", position = TokenPosition(position = 26, line = 2, column = 12))
    )
    assertEquals(expectedErrors.size, errors.size)

    expectedErrors.forEachIndexed { index, (expectedMessage, expectedPosition) ->
      val error = errors[index]
      assertEquals(expectedMessage, error.message)
      assertEquals(expectedPosition, error.position)
    }
  }
}
