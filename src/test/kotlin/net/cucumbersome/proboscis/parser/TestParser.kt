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
    assertEquals(3, program.statements.size)
    val tests = listOf(
      "x" to 5,
      "y" to 10,
      "foobar" to 838383
    )
    tests.forEachIndexed { index, (expectedIdentifier, expectedValue) ->
      val statement = program.statements[index]
      if (statement !is LetStatement) {
        throw AssertionError("Statement at index $index is not a LetStatement")
      }
      assertEquals(Token.Companion.Let, statement.token)
      assertEquals(expectedIdentifier, statement.name.value)
      if (statement.value !is IntegerLiteral) {
        throw AssertionError("Value of statement at index $index is not an IntegerLiteral")
      }
      assertEquals(expectedValue, (statement.value as IntegerLiteral).value)
    }
  }
}
