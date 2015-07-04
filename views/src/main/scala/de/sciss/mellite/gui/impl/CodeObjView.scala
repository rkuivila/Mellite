/*
 *  CodeObjView.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui
package impl

import de.sciss.desktop
import de.sciss.icons.raphael
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.{event => evt, stm}
import de.sciss.swingplus.ComboBox
import de.sciss.synth.proc
import de.sciss.synth.proc.{Code, Obj}

import scala.swing.{Component, Label}

// -------- Code --------

object CodeObjView extends ListObjView.Factory {
  type E[S <: evt.Sys[S]] = Code.Elem[S]
  val icon        = ObjViewImpl.raphaelIcon(raphael.Shapes.Code)
  val prefix      = "Code"
  def typeID      = Code.typeID
  def hasMakeDialog   = true

  def mkListView[S <: Sys[S]](obj: Obj.T[S, Code.Elem])(implicit tx: S#Tx): CodeObjView[S] with ListObjView[S] = {
    val value   = obj.elem.peer.value
    new Impl(tx.newHandle(obj), ObjViewImpl.nameOption(obj), value)
  }

  type Config[S <: evt.Sys[S]] = ObjViewImpl.PrimitiveConfig[Code]

  def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                                 (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
    val ggValue = new ComboBox(Seq(Code.FileTransform.name, Code.SynthGraph.name))
    ObjViewImpl.primitiveConfig(window, tpe = prefix, ggValue = ggValue, prepare = ggValue.selection.index match {
      case 0 => Some(Code.FileTransform(
        """|val aIn   = AudioFile.openRead(in)
          |val aOut  = AudioFile.openWrite(out, aIn.spec)
          |val bufSz = 8192
          |val buf   = aIn.buffer(bufSz)
          |var rem   = aIn.numFrames
          |while (rem > 0) {
          |  val chunk = math.min(bufSz, rem).toInt
          |  aIn .read (buf, 0, chunk)
          |  // ...
          |  aOut.write(buf, 0, chunk)
          |  rem -= chunk
          |  // checkAbort()
          |}
          |aOut.close()
          |aIn .close()
          |""".stripMargin))

      case 1 => Some(Code.SynthGraph(
        """|val in   = ScanIn("in")
          |val sig  = in
          |ScanOut("out", sig)
          |""".stripMargin
      ))

      case _  => None
    })
  }

  def makeObj[S <: Sys[S]](config: (String, Code))(implicit tx: S#Tx): List[Obj[S]] = {
    import proc.Implicits._
    val (name, value) = config
    val peer  = Code.Expr.newVar[S](Code.Expr.newConst(value))
    val obj   = Obj(Code.Elem(peer))
    obj.name = name
    obj :: Nil
  }

  final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, Code.Elem]],
                                var nameOption: Option[String], var value: Code)
    extends CodeObjView[S]
    with ListObjView /* .Code */[S]
    with ObjViewImpl.Impl[S]
    with ListObjViewImpl.NonEditable[S] {

    type E[~ <: evt.Sys[~]] = Code.Elem[~]

    def icon    = CodeObjView.icon
    def prefix  = CodeObjView.prefix
    def typeID  = CodeObjView.typeID

    def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean = false

    def isViewable = true

    def openView(parent: Option[Window[S]])
                (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
      import de.sciss.mellite.Mellite.compiler
      val frame = CodeFrame(obj(), hasExecute = false)
      Some(frame)
    }

    def configureRenderer(label: Label): Component = {
      label.text = value.contextName
      label
    }
  }
}
trait CodeObjView[S <: evt.Sys[S]] extends ObjView[S] {
  override def obj: stm.Source[S#Tx, Code.Obj[S]]
}