package net.cucumbersome.proboscis.lexer

import net.cucumbersome.proboscis.Token

class Lexer private constructor(
  private val input: String,
  private val position: Int
) {

  fun nextToken(): Pair<Token, Lexer> {
    if (position >= input.length) {
      return Pair(Token.Companion.Eof, this)
    }
    val currentInput = input.drop(position)
    val whitespace = currentInput.takeWhile { it.isWhitespace() }

    if (whitespace.isNotEmpty()) {
      return advancePosition(whitespace.length).nextToken()
    }

    val character: Char = currentInput[0]
    val token = when (character) {
      '=' -> {
        if (getOptionalNextCharacter(currentInput) == '=') {
          Token.Companion.Equal
        } else {
          Token.Companion.Assign
        }
      }
      ';' -> Token.Companion.Semicolon
      '(' -> Token.Companion.LeftParen
      ')' -> Token.Companion.RightParen
      ',' -> Token.Companion.Comma
      '+' -> Token.Companion.Plus
      '{' -> Token.Companion.LeftBrace
      '}' -> Token.Companion.RightBrace
      '-' -> Token.Companion.Minus
      '!' -> {
        if (getOptionalNextCharacter(currentInput) == '=') {
          Token.Companion.NotEqual
        } else {
          Token.Companion.Bang
        }
      }
      '/' -> Token.Companion.Slash
      '*' -> Token.Companion.Asterisk
      '<' -> Token.Companion.LessThan
      '>' -> Token.Companion.GreaterTHan
      else -> {
        if (couldBeIdentifier(character)) {
          readKeywordOrIdentifier(currentInput)
        } else if (character.isDigit()) {
          readNumber(currentInput)
        } else {
          Token.Companion.Illegal(character)
        }
      }
    }
    val advanceBy = when (token) {
      is Token.Companion.Identifier -> token.value.length
      is Token.Companion.Illegal -> 0
      is Token.Companion.IntValue -> token.value.toString().length
      Token.Companion.NotEqual -> 2
      Token.Companion.Let -> 3
      Token.Companion.Function -> 2
      Token.Companion.Return -> 6
      Token.Companion.If -> 2
      Token.Companion.Else -> 4
      Token.Companion.True -> 4
      Token.Companion.False -> 5
      Token.Companion.Equal -> 2
      else -> 1
    }
    return Pair(token, advancePosition(advanceBy))
  }

  private fun getOptionalNextCharacter(currentInput: String): Char? {
    return if (currentInput.length > 1) {
      currentInput[1]
    } else {
      null
    }
  }
  private fun advancePosition(advanceBy: Int): Lexer {
    return Lexer(input, position + advanceBy)
  }

  private fun readNumber(currentInput: String): Token {
    val number = currentInput.takeWhile { it.isDigit() }
    return Token.Companion.IntValue(number.toInt())
  }

  private fun readKeywordOrIdentifier(currentInput: String): Token {
    val keyword = currentInput.takeWhile { couldBeIdentifier(it) }
    return keywords[keyword] ?: Token.Companion.Identifier(keyword)
  }
  fun couldBeIdentifier(char: Char): Boolean {
    return char.isLetter() || char == '_'
  }
  companion object {
    private val keywords = mapOf(
      "fn" to Token.Companion.Function,
      "let" to Token.Companion.Let,
      "return" to Token.Companion.Return,
      "if" to Token.Companion.If,
      "else" to Token.Companion.Else,
      "true" to Token.Companion.True,
      "false" to Token.Companion.False
    )
    fun fromString(string: String): Lexer? {
      return if (string.isEmpty()) {
        null
      } else {
        Lexer(
          string,
          0
        )
      }
    }
  }
}
