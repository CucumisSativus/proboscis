package net.cucumbersome.proboscis

sealed class Token {
    companion object {
        data class Illegal(val value: Char): Token()
        object Eof: Token()
        data class Identifier(val value: String): Token()
        data class IntValue(val value: Int): Token()

        object Assign: Token()

        object Plus: Token()
        object Comma: Token()
        object Semicolon: Token()

        object LeftParen: Token()
        object RightParen: Token()
        object LeftBrace: Token()
        object RightBrace: Token()

        object Function: Token()
        object Let: Token()
    }
}


