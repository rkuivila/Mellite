package de.sciss.mellite

import java.io.File

object DocTest extends App {
  val file  = File.createTempFile("mellite", "doc")
  require(file.delete())
  val doc   = Document.empty(file)
  println("Ok.")
}