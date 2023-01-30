package ci.tool

import java.io.PrintWriter

class GenerateAst {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println("Usage: generate_ast <output directory>")
      System.exit(64)
    }
    val outPutDir = args(0)
  }

  private def defineAst(outputDir: String, basename: String, types: List[String]) = {
    val path = s"$outputDir/$basename.java"
    val writer = new PrintWriter(path, "UTF-8")
    writer.println("package com.craftinginterpreters.lox;")
    writer.println()
    writer.println("import java.util.List;")
    writer.println()
    writer.println(s"abstract class $basename {")
    writer.println("}")
  }

  private def defineType(writer: PrintWriter, basename: String, className: String, fieldList: String) = {
    writer.println(s" static class $className extends $basename {")
    writer.println(s"    $className() {")
    val fields = fieldList.split(", ")
    fields.foreach { field => {
      val name = field.split(" ")(1)
      writer.println(s"    this.$name = $name;")
    }}

    writer.println("     }")

    writer.println()
    fields.foreach { field => writer.println(s"  final $field;") }
    writer.println("     }")
  }
}
