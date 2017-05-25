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
import javax.swing.event.{HyperlinkEvent, HyperlinkListener}
import javax.swing.undo.UndoableEdit

import de.sciss.desktop.{Desktop, KeyStrokes, UndoManager}
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
import org.pegdown.PegDownProcessor

import scala.collection.breakOut
import scala.collection.immutable.{Seq => ISeq}
import scala.swing.Swing._
import scala.swing.event.Key
import scala.swing.{Action, BorderPanel, Button, Component, EditorPane, FlowPanel, ScrollPane, Swing, TabbedPane}

object MarkdownViewImpl {
  def apply[S <: Sys[S]](obj: Markdown[S], showEditor: Boolean, bottom: ISeq[View[S]])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S],
                         undoManager: UndoManager): MarkdownView[S] = {
    val varHOpt = obj match {
      case Markdown.Var(vr) =>
        Some(tx.newHandle(vr))
      case _            => None
    }
    val textValue = obj.value
    val res = new Impl[S](varHOpt, textValue, bottom = bottom)
    res.init(showEditor = showEditor)
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

    private[this] var paneImpl    : PaneImpl    = _
    private[this] var actionApply : Action      = _
    private[this] var actionRender: Action      = _
    private[this] var editorRender: EditorPane  = _
    private[this] var tabs        : TabbedPane  = _

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

    def save(): Unit = {
      requireEDT()
      val newMarkdown = currentText
      val editOpt = cursor.step { implicit tx =>
        saveText(newMarkdown)
      }
      editOpt.foreach(addEditAndClear)
      render(newMarkdown)
    }

    private def render(text: String): Unit = {
      val mdp         = new PegDownProcessor
      val html        = mdp.markdownToHtml(text)
      editorRender.text = html
      editorRender.peer.setCaretPosition(0)
    }

    private def renderAndShow(): Unit = {
      render(currentText)
      tabs.selection.index = 1
    }

    def init(showEditor: Boolean)(implicit tx: S#Tx): this.type = {
      deferTx(guiInit(showEditor = showEditor))
      this
    }

    private def guiInit(showEditor: Boolean): Unit = {
      paneImpl            = createPane(initialText)
      actionApply         = Action("Apply")(save())
      actionRender        = Action(null   )(renderAndShow())
      actionApply.enabled = false

      lazy val doc = paneImpl.editor.peer.getDocument.asInstanceOf[SyntaxDocument]

      doc.addPropertyChangeListener(SyntaxDocument.CAN_UNDO, new PropertyChangeListener {
        def propertyChange(e: PropertyChangeEvent): Unit = dirty = doc.canUndo
      })

      val ksRender  = KeyStrokes.menu1 + Key.Enter
      val ttRender  = s"Render (${GUI.keyStrokeText(ksRender)})"

      lazy val ggApply : Button = GUI.toolButton(actionApply , raphael.Shapes.Check       , tooltip = "Save text changes")
      lazy val ggRender: Button = GUI.toolButton(actionRender, raphael.Shapes.RefreshArrow, tooltip = ttRender)

      GUI.addGlobalKeyWhenVisible(ggRender, ksRender)

      val bot1: List[Component] = if (bottom.isEmpty) Nil else bottom.map(_.component)(breakOut)
      val bot2 = HGlue :: ggApply :: ggRender :: bot1
      val panelBottom = new FlowPanel(FlowPanel.Alignment.Trailing)(bot2: _*)

      val paneEdit = new BorderPanel {
        add(paneImpl.component, BorderPanel.Position.Center)
        add(panelBottom       , BorderPanel.Position.South )
      }

      editorRender = new EditorPane("text/html", "") {
        editable      = false
        border        = Swing.EmptyBorder(8)
        preferredSize = (500, 500)

        peer.addHyperlinkListener(new HyperlinkListener {
          def hyperlinkUpdate(e: HyperlinkEvent): Unit = {
            if (e.getEventType == HyperlinkEvent.EventType.ACTIVATED) {
              // println(s"description: ${e.getDescription}")
              // println(s"source elem: ${e.getSourceElement}")
              // println(s"url        : ${e.getURL}")
              // val link = e.getDescription
              // val ident = if (link.startsWith("ugen.")) link.substring(5) else link
              // lookUpHelp(ident)

              val url = e.getURL
              if (url != null) {
                Desktop.browseURI(url.toURI)
              } else {
                val desc = e.getDescription
                println(s"TODO: Navigate to $desc")
              }
            }
          }
        })
      }

      val paneRender = new ScrollPane(editorRender)
      paneRender.peer.putClientProperty("styleId", "undecorated")

      val _tabs = new TabbedPane
      _tabs.peer.putClientProperty("styleId", "attached")
      _tabs.focusable  = false
      val pageEdit    = new TabbedPane.Page("Editor"  , paneEdit  , null)
      val pageRender  = new TabbedPane.Page("Rendered", paneRender, null)
      _tabs.pages     += pageEdit
      _tabs.pages     += pageRender
//      _tabs.pages     += pageAttr
      GUI.addTabNavigation(_tabs)

      render(initialText)

      tabs = _tabs

      component = _tabs

      if (showEditor) {
        paneImpl.component.requestFocus()
      } else {
        _tabs.selection.index = 1
      }
    }
  }
}