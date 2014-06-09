package de.sciss.mellite

import java.io.File

object DocTest extends App {
  val file  = File.createTempFile("mellite", "doc")
  require(file.delete())
  val doc   = Workspace.Confluent.empty(file)
  println("Ok.")
}