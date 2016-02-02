/*
 *  InterpreterFrame.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2016 Hanns Holger Rutz. All rights reserved.
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

import desktop.KeyStrokes
import impl.interpreter.{InterpreterFrameImpl => Impl}
import scala.swing.event.Key

object InterpreterFrame {
  def apply(): InterpreterFrame = Impl()

  object Action extends swing.Action("Interpreter") {
    import KeyStrokes._
    accelerator = Some(menu1 + Key.R)

    def apply(): Unit = InterpreterFrame()
  }

  /** The content of this object is imported into the REPL */
  object Bindings {
    def document =
      Application.documentHandler.activeDocument.getOrElse(sys.error("No document open"))

    def confluentDocument: Workspace.Confluent = document match {
      case cd: Workspace.Confluent => cd
      case other => sys.error("Not a confluent document")
    }
  }
}
trait InterpreterFrame {
  def component: desktop.Window
}