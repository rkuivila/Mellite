/*
 *  InterpreterFrame.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
    //    def document = {
    //      //      val docs = DocumentHandler.instance.allDocuments.toIndexedSeq
    //      //      if (docs.isEmpty) sys.error("No document open")
    //      //      val doc = docs.last
    //      //      if (docs.size > 1) println(s"WARNING: multiple documents open. Assuming '${doc.file.name}")
    //      //      doc
    //      DocumentViewHandler.instance.activeDocument.getOrElse(sys.error("No document open"))
    //    }
  }
}
trait InterpreterFrame {
  def component: desktop.Window
}