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
        object Bang: Token()
        object Minus: Token()

        object Slash:  Token()
        object Asterisk: Token()
        object LessThan: Token()

        object GreaterTHan: Token()

        object Return: Token()

        object If: Token()
        object Else: Token()
        object True: Token()
        object False: Token()

        object Equal: Token()
        object NotEqual: Token()
    }
}


