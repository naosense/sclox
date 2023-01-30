package ci.lox

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

object Lox {
  private var hasError = false

  def main(args: Array[String]): Unit = {
    if (args.length > 1) {
      println("Usage: jlox [script]")
      System.exit(64)
    } else if (args.length == 1) {
      runFile(args(0))
    } else {
      runPrompt()
    }
  }

  private def runFile(path: String): Unit = {
    val bytes = Files.readAllBytes(Paths.get(path))
    run(new String(bytes, Charset.defaultCharset()))
    if (hasError) System.exit(65)
  }

  private def runPrompt(): Unit = {
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)
    var done = false
    while (!done) {
      print("> ")
      val line = reader.readLine()
      if (line == null) done = true
      if (!done) run(line)
      hasError = false
    }
  }

  private def run(str: String) = {
    val scanner = new Scanner(str)
    val tokens = scanner.scanTokens()
    tokens.foreach(println)
  }

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  private def report(line : Int, where: String, message: String) = {
    System.err.println(s"[line $line] Error $where: $message")
    hasError = true
  }

  def error(token: Token, message: String): Unit = {
    if (token.tpe == EOF)
      report(token.line, " at end", message)
    else
      report(token.line, s" at '${token.lexeme}'", message)
  }
}
