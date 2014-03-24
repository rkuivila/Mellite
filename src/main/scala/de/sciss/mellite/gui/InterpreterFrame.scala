/*
 *  InterpreterFrame.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss
package mellite
package gui

import java.awt.event.KeyEvent
import desktop.KeyStrokes
import impl.interpreter.{InterpreterFrameImpl => Impl}

object InterpreterFrame {
  def apply(): InterpreterFrame = Impl()

  object Action extends swing.Action("Interpreter") {
    import KeyStrokes._
    import KeyEvent._
    accelerator = Some(menu1 + VK_R)

    def apply(): Unit = InterpreterFrame()
  }

  /** The content of this object is imported into the REPL */
  object Bindings {
    def document =
      Mellite.documentHandler.activeDocument.getOrElse(sys.error("No document open"))
  }
}
trait InterpreterFrame {
  def component: desktop.Window
}