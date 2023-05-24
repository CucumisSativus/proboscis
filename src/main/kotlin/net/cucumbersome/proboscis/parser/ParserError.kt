package net.cucumbersome.proboscis.parser

data class ParserError(val message: String, val position: TokenPosition) {
  override fun toString(): String = "$message at line ${position.line}, column ${position.column}"
}
