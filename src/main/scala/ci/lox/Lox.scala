package ci.lox

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

object Lox {
  private val interpreter = new Interpreter()
  private var hadError = false
  private var hadRuntimeError = false

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
    if (hadError) System.exit(65)
    if (hadRuntimeError) System.exit(70)
  }

  private def runPrompt(): Unit = {
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)
    var done = false
    while (!done) {
      print("> ")
      val line = reader.readLine()
      if (line == null || line == "exit") done = true
      if (!done) run(line)
      hadError = false
    }
  }

  private def run(str: String): Unit = {
    val scanner = new Scanner(str)
    val tokens = scanner.scanTokens()
    // tokens.foreach(println)
    val parser = new Parser(tokens)
    val expression = parser.parse()

    if (hadError) return
    // println(new AstPrinter().print(expression))
    interpreter.interpret(expression)
  }

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  private def report(line: Int, where: String, message: String) = {
    System.err.println(s"[line $line] Error $where: $message")
    hadError = true
  }

  def error(token: Token, message: String): Unit = {
    if (token.tpe == EOF)
      report(token.line, " at end", message)
    else
      report(token.line, s" at '${ token.lexeme }'", message)
  }

  def runtimeError(error: RuntimeError): Unit = {
    println(error.message + s"\n[line ${ error.token.line }]")
    hadRuntimeError = true
  }
}
