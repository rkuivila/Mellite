/*
 *  MarkdownViewImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2017 Hanns Holger Rutz. All rights reserved.
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

import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import javax.swing.undo.UndoableEdit

import de.sciss.desktop.UndoManager
import de.sciss.icons.raphael
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.edit.EditVar
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{View, deferTx, requireEDT}
import de.sciss.model.impl.ModelImpl
import de.sciss.scalainterpreter.Fonts
import de.sciss.scalainterpreter.impl.CodePaneImpl
import de.sciss.swingplus.Implicits._
import de.sciss.syntaxpane.SyntaxDocument
import de.sciss.syntaxpane.syntaxkits.MarkdownSyntaxKit
import de.sciss.synth.proc.{Markdown, Workspace}

import scala.collection.breakOut
import scala.collection.immutable.{Seq => ISeq}
import scala.concurrent.Future
import scala.swing.Swing._
import scala.swing.{Action, BorderPanel, Button, Component, EditorPane, FlowPanel}

object MarkdownViewImpl {
  def apply[S <: Sys[S]](obj: Markdown[S], bottom: ISeq[View[S]])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S],
                         undoManager: UndoManager): MarkdownView[S] = {
    val varHOpt = obj match {
      case Markdown.Var(vr) =>
        Some(tx.newHandle(vr))
      case _            => None
    }
    val textValue = obj.value
    val res = new Impl[S](varHOpt, textValue, bottom = bottom)
    res.init()
  }

  private def createPane(initialText: String): PaneImpl = {
    CodePaneImpl.initKit[MarkdownSyntaxKit]()
    new PaneImpl(initialText).init()
  }

  private final class PaneImpl(protected val initialText: String)
    extends CodePaneImpl.Basic {

    val editor                : EditorPane  = CodePaneImpl.createEditorPane()
    protected def mimeType    : String      = "text/markdown"
    protected def fonts       : Fonts.List  = Fonts.defaultFonts
    protected def tabSize     : Int         = 4
  }

  private final class Impl[S <: Sys[S]](varHOpt: Option[stm.Source[S#Tx, Markdown.Var[S]]],
                                        initialText: Markdown.Value,
                                        bottom: ISeq[View[S]])
                                       (implicit undoManager: UndoManager, val workspace: Workspace[S],
                                        val cursor: stm.Cursor[S])
    extends ComponentHolder[Component] with MarkdownView[S] with ModelImpl[MarkdownView.Update] {

    private[this] var _dirty = false
    def dirty: Boolean = _dirty
    def dirty_=(value: Boolean): Unit = if (_dirty != value) {
      _dirty = value
      actionApply.enabled = value
      dispatch(MarkdownView.DirtyChange(value))
    }

    private[this] var paneImpl: PaneImpl = _
    private[this] var actionApply: Action = _

    protected def currentText: String = paneImpl.editor.text

    def dispose()(implicit tx: S#Tx): Unit = ()

    def undoAction: Action = Action.wrap(paneImpl.editor.peer.getActionMap.get("undo"))
    def redoAction: Action = Action.wrap(paneImpl.editor.peer.getActionMap.get("redo"))

    private def saveText(newTextValue: String)(implicit tx: S#Tx): Option[UndoableEdit] =
      varHOpt.map { vr =>
        val newMarkdown = Markdown.newConst[S](newTextValue)
        implicit val codeTpe = Markdown
        EditVar.Expr[S, Markdown.Value, Markdown]("Change Source Markdown", vr(), newMarkdown)
      }

    private def addEditAndClear(edit: UndoableEdit): Unit = {
      requireEDT()
      undoManager.add(edit)
      // this doesn't work properly
      // component.setDirty(value = false) // do not erase undo history

      // so let's clear the undo history now...
      paneImpl.editor.peer.getDocument.asInstanceOf[SyntaxDocument].clearUndos()
    }

    def save(): Future[Unit] = {
      requireEDT()
      val newMarkdown = currentText
      val editOpt = cursor.step { implicit tx =>
        saveText(newMarkdown)
      }
      editOpt.foreach(addEditAndClear)
      Future.successful[Unit] {}
    }

    def init()(implicit tx: S#Tx): this.type = {
      deferTx(guiInit())
      this
    }

    private def guiInit(): Unit = {
      paneImpl            = createPane(initialText)
      actionApply         = Action("Apply")(save())
      actionApply.enabled = false

      lazy val doc = paneImpl.editor.peer.getDocument.asInstanceOf[SyntaxDocument]

      doc.addPropertyChangeListener(SyntaxDocument.CAN_UNDO, new PropertyChangeListener {
        def propertyChange(e: PropertyChangeEvent): Unit = dirty = doc.canUndo
      })

      lazy val ggApply: Button = GUI.toolButton(actionApply, raphael.Shapes.Check , tooltip = "Save text changes")

      val bot1: List[Component] = if (bottom.isEmpty) Nil else bottom.map(_.component)(breakOut)
      val bot2 = HGlue :: ggApply :: bot1
      val panelBottom = new FlowPanel(FlowPanel.Alignment.Trailing)(bot2: _*)

      val top = new BorderPanel {
        add(paneImpl.component, BorderPanel.Position.Center)
        add(panelBottom       , BorderPanel.Position.South )
      }

      component = top
      paneImpl.component.requestFocus()
    }
  }
}