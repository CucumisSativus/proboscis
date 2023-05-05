package net.cucumbersome.proboscis.lexer

import net.cucumbersome.proboscis.Token
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class TestLexer {
  fun runLexerTest(input: String, expectedTokens: List<Token>) {
    var lexer = Lexer.fromString(input)!!
    expectedTokens.forEachIndexed { index, expectedToken ->
      val (token, newLexer) = lexer.nextToken()
      assertEquals(expectedToken, token, "Token at index $index is not as expected")
      lexer = newLexer
    }
  }

  @Test
  fun testNextTokenEasyTokens() {
    val input = "=+(){},;"
    val expectedTokens = listOf(
      Token.Companion.Assign,
      Token.Companion.Plus,
      Token.Companion.LeftParen,
      Token.Companion.RightParen,
      Token.Companion.LeftBrace,
      Token.Companion.RightBrace,
      Token.Companion.Comma,
      Token.Companion.Semicolon,
      Token.Companion.Eof
    )
    runLexerTest(input, expectedTokens)
  }

  @Test
  fun getNextTokenCompoundTokens() {
    val input = """
            let five = 5;
            let ten = 10;

            let add = fn(x, y) {
              x + y;
            };

            let result = add(five, ten);
    """.trimIndent()

    val expectedTokens = listOf(
      Token.Companion.Let,
      Token.Companion.Identifier("five"),
      Token.Companion.Assign,
      Token.Companion.IntValue(5),
      Token.Companion.Semicolon,
      Token.Companion.Let,
      Token.Companion.Identifier("ten"),
      Token.Companion.Assign,
      Token.Companion.IntValue(10),
      Token.Companion.Semicolon,
      Token.Companion.Let,
      Token.Companion.Identifier("add"),
      Token.Companion.Assign,
      Token.Companion.Function,
      Token.Companion.LeftParen,
      Token.Companion.Identifier("x"),
      Token.Companion.Comma,
      Token.Companion.Identifier("y"),
      Token.Companion.RightParen,
      Token.Companion.LeftBrace,
      Token.Companion.Identifier("x"),
      Token.Companion.Plus,
      Token.Companion.Identifier("y"),
      Token.Companion.Semicolon,
      Token.Companion.RightBrace,
      Token.Companion.Semicolon,
      Token.Companion.Let,
      Token.Companion.Identifier("result"),
      Token.Companion.Assign,
      Token.Companion.Identifier("add"),
      Token.Companion.LeftParen,
      Token.Companion.Identifier("five"),
      Token.Companion.Comma,
      Token.Companion.Identifier("ten"),
      Token.Companion.RightParen,
      Token.Companion.Semicolon,
      Token.Companion.Eof
    )
    runLexerTest(input, expectedTokens)
  }

  @Test
  fun getNextTokenSpecialCharactersAndThings() {
    val input = """
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
    """.trimIndent()

    val expectedTokens = listOf(
      Token.Companion.Bang,
      Token.Companion.Minus,
      Token.Companion.Slash,
      Token.Companion.Asterisk,
      Token.Companion.IntValue(5),
      Token.Companion.Semicolon,
      Token.Companion.IntValue(5),
      Token.Companion.LessThan,
      Token.Companion.IntValue(10),
      Token.Companion.GreaterThan,
      Token.Companion.IntValue(5),
      Token.Companion.Semicolon,
      Token.Companion.If,
      Token.Companion.LeftParen,
      Token.Companion.IntValue(5),
      Token.Companion.LessThan,
      Token.Companion.IntValue(10),
      Token.Companion.RightParen,
      Token.Companion.LeftBrace,
      Token.Companion.Return,
      Token.Companion.True,
      Token.Companion.Semicolon,
      Token.Companion.RightBrace,
      Token.Companion.Else,
      Token.Companion.LeftBrace,
      Token.Companion.Return,
      Token.Companion.False,
      Token.Companion.Semicolon,
      Token.Companion.RightBrace,
      Token.Companion.IntValue(10),
      Token.Companion.Equal,
      Token.Companion.IntValue(10),
      Token.Companion.Semicolon,
      Token.Companion.IntValue(10),
      Token.Companion.NotEqual,
      Token.Companion.IntValue(9),
      Token.Companion.Semicolon,
      Token.Companion.Eof
    )
    runLexerTest(input, expectedTokens)
  }

  @Test
  fun testNextTokenIs() {
    val input = "let a = 5;"
    val lexer = Lexer.fromString(input)!!

    assertTrue { lexer.nextTokenIs(Token.Companion.Let) }
    val (_, newLexer) = lexer.nextToken()
    assertTrue { newLexer.nextTokenIs(Token.Companion.Identifier("a")) }
  }
}
