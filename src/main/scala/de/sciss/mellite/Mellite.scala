package de.sciss.mellite

import gui.MenuBar
import java.awt.{Dimension, EventQueue}
import javax.swing.WindowConstants
import swing.Frame

object Mellite extends App {
   val name = "Mellite"

   EventQueue.invokeLater( new Runnable { def run() { go() }})

   private def go() {
      val f = new Frame {
         title = name
         peer.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
         menuBar = MenuBar()
         size = new Dimension( 256, 256 )
         centerOnScreen()
         open()
      }
   }
}
