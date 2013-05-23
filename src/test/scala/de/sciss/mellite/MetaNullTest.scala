package de.sciss.mellite

import de.sciss.indeterminus.{MetaNull, Nullstellen}

object MetaNullTest extends App {
  val c = Nullstellen.Config()
  c.materialFolder = file("/Volumes/data/hhrutz/strugatzki")
  MetaNull.perform(file("/Users/hhrutz/Desktop/Indeterminus/_bak/audio/bounces/mechanik_ch1_bck.aif"),
                   file("/Users/hhrutz/Desktop/test/schokomuffin.aif"), c)
}