package net.cucumbersome.proboscis

sealed class Token {
  companion object {
    data class Illegal(val value: Char) : Token()
    object Eof : Token() {
      override fun toString(): String {
        return "EOF"
      }
    }

    data class Identifier(val value: String) : Token()
    data class IntValue(val value: Int) : Token()

    object Assign : Token() {
      override fun toString(): String {
        return "="
      }
    }

    object Plus : Token() {
      override fun toString(): String {
        return "+"
      }
    }

    object Comma : Token() {
      override fun toString(): String {
        return ","
      }
    }

    object Semicolon : Token() {
      override fun toString(): String {
        return ";"
      }
    }

    object LeftParen : Token() {
      override fun toString(): String {
        return "("
      }
    }

    object RightParen : Token() {
      override fun toString(): String {
        return ")"
      }
    }

    object LeftBrace : Token() {
      override fun toString(): String {
        return "{"
      }
    }

    object RightBrace : Token() {
      override fun toString(): String {
        return "}"
      }
    }

    object Function : Token() {
      override fun toString(): String {
        return "fn"
      }
    }

    object Let : Token() {
      override fun toString(): String {
        return "let"
      }
    }

    object Bang : Token() {
      override fun toString(): String {
        return "!"
      }
    }

    object Minus : Token() {
      override fun toString(): String {
        return "-"
      }
    }

    object Slash : Token() {
      override fun toString(): String {
        return "/"
      }
    }

    object Asterisk : Token() {
      override fun toString(): String {
        return "*"
      }
    }

    object LessThan : Token() {
      override fun toString(): String {
        return "<"
      }
    }

    object GreaterThan : Token() {
      override fun toString(): String {
        return ">"
      }
    }

    object Return : Token() {
      override fun toString(): String {
        return "return"
      }
    }

    object If : Token() {
      override fun toString(): String {
        return "if"
      }
    }

    object Else : Token() {
      override fun toString(): String {
        return "else"
      }
    }

    object True : Token() {
      override fun toString(): String {
        return "true"
      }
    }

    object False : Token() {
      override fun toString(): String {
        return "false"
      }
    }

    object Equal : Token() {
      override fun toString(): String {
        return "=="
      }
    }

    object NotEqual : Token() {
      override fun toString(): String {
        return "!="
      }
    }
  }
}
