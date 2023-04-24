package net.cucumbersome.proboscis.parser

import net.cucumbersome.proboscis.lexer.Lexer
import kotlin.test.assertEquals

object ParserHelper {

  fun getProgram(input: String): Program {
    val lexer = Lexer.fromString(input)!!
    val parser = Parser(lexer)
    return parser.parseProgram().fold(
      { throw AssertionError("Expected parsing to succeed, but it failed. Errors $it") },
      { it }
    )
  }

  fun testErrorHandling(input: String, expectedErrors: List<ParserError>) {
    val lexer = Lexer.fromString(input)!!
    val parser = Parser(lexer)
    val program = parser.parseProgram()
    val errors = program.fold(
      { it.errors },
      { throw AssertionError("Expected parsing to fail, but it succeeded") }
    )
    assertEquals(expectedErrors.size, errors.size)
    expectedErrors.forEachIndexed { index, (expectedMessage, expectedPosition) ->
      val error = errors[index]
      assertEquals(expectedMessage, error.message)
      assertEquals(expectedPosition, error.position)
    }
  }
}
