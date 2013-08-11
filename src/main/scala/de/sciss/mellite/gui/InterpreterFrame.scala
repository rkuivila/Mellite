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