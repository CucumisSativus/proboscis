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
    val expectedErrors = listOf(
      "Expected =, got IntValue(value=5)" to TokenPosition(position = 7),
      "Unexpected token ;" to TokenPosition(position = 7),
      "Expected identifier, got =" to TokenPosition(position = 12),
      "Expected =, got IntValue(value=10)" to TokenPosition(position = 17),
      "Unexpected token ;" to TokenPosition(position = 17),
      "Expected identifier, got IntValue(value=838383)" to TokenPosition(position = 22),
      "Expected =, got ;" to TokenPosition(position = 30),
      "Unexpected token EOF" to TokenPosition(position = 30)
    )
    assertEquals(expectedErrors.size, errors.size)
    expectedErrors.forEachIndexed { index, (expectedMessage, expectedPosition) ->
      val error = errors[index]
      assertEquals(expectedMessage, error.message)
      assertEquals(expectedPosition, error.position)
    }
  }
}
