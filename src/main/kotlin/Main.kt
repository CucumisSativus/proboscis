import net.cucumbersome.proboscis.Token
import net.cucumbersome.proboscis.lexer.Lexer

fun main(args: Array<String>) {
  println("Welcome to Monkey programming language!")

  while (true) {
    print(">> ")
    val line = readLine()
    if (line == null) {
      println("Bye!")
      break
    } else if (line == "") {
      continue
    } else if (line == "exit()") {
      println("Bye!")
      return
    }
    val lexer = Lexer.fromString(line)!!

    printTokens(lexer)
  }
}

tailrec fun printTokens(lexer: Lexer) {
  val (token, newLexer) = lexer.nextToken()
  if (token == Token.Companion.Eof) {
    return
  } else {
    println(token)
    printTokens(newLexer)
  }
}
