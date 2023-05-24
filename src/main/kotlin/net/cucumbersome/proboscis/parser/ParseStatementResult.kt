package net.cucumbersome.proboscis.parser

import net.cucumbersome.proboscis.lexer.Lexer

sealed interface ParseStatementResult<T> {
  val lexer: Lexer

  fun <U> flatMap(f: (statement: T, lexer: Lexer) -> ParseStatementResult<out U>): ParseStatementResult<out U> =
    when (this) {
      is SuccessfulParseStatementResult -> f(this.statement, this.lexer)
      is FailedParseStatementResult -> this
    }

  fun appendErrors(errors: List<ParserError>): ParseStatementResult<out T> =
    when (this) {
      is SuccessfulParseStatementResult -> {
        if (errors.isNotEmpty()) {
          FailedParseStatementResult(errors, this.lexer)
        } else {
          this
        }
      }

      is FailedParseStatementResult -> FailedParseStatementResult(this.errors + errors, this.lexer)
    }

  fun <U> appendResult(result: ParseStatementResult<out U>): ParseStatementResult<out U> =
    when (result) {
      is SuccessfulParseStatementResult -> when (this) {
        is SuccessfulParseStatementResult -> result
        is FailedParseStatementResult -> result.appendErrors(this.errors)
      }

      is FailedParseStatementResult -> this.appendErrors(result.errors) as FailedParseStatementResult
    }
}

data class SuccessfulParseStatementResult<T>(val statement: T, override val lexer: Lexer) :
  ParseStatementResult<T>

data class FailedParseStatementResult(val errors: List<ParserError>, override val lexer: Lexer) :
  ParseStatementResult<Nothing>

fun <T, U, V, R> map3(
  result1: ParseStatementResult<T>,
  result2: ParseStatementResult<U>,
  result3: ParseStatementResult<V>,
  f: (T, U, V) -> R
): ParseStatementResult<out R> =
  if (result1 is SuccessfulParseStatementResult && result2 is SuccessfulParseStatementResult && result3 is SuccessfulParseStatementResult) {
    SuccessfulParseStatementResult(f(result1.statement, result2.statement, result3.statement), result3.lexer)
  } else {
    result1.appendResult(result2).appendResult(result3) as FailedParseStatementResult
  }

fun <T, U, R> map2(
  result1: ParseStatementResult<T>,
  result2: ParseStatementResult<U>,
  f: (T, U) -> R
): ParseStatementResult<out R> =
  if (result1 is SuccessfulParseStatementResult && result2 is SuccessfulParseStatementResult) {
    SuccessfulParseStatementResult(f(result1.statement, result2.statement), result2.lexer)
  } else {
    result1.appendResult(result2) as FailedParseStatementResult
  }

