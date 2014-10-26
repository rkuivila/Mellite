package de.sciss.mellite

import java.io.File

import de.sciss.lucre.stm.store.BerkeleyDB

object DocTest extends App {
  val file  = File.createTempFile("mellite", "doc")
  require(file.delete())
  val doc   = Workspace.Confluent.empty(file, BerkeleyDB.Config())
  println("Ok.")
}