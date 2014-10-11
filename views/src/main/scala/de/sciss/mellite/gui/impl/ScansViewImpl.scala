/*
 *  ScansViewImpl.scala
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

package de.sciss.mellite
package gui
package impl

import java.awt.datatransfer.Transferable
import javax.swing.TransferHandler.TransferSupport
import javax.swing.TransferHandler

import de.sciss.desktop.{OptionPane, UndoManager}
import de.sciss.icons.raphael
import de.sciss.lucre.stm
import de.sciss.lucre.data
import de.sciss.lucre.stm.IdentifierMap
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.event.Sys
import de.sciss.mellite.gui.edit.{EditAddScanLink, EditAddScan}
import de.sciss.mellite.gui.impl.component.DragSourceButton
import de.sciss.swingplus.ListView
import de.sciss.synth.proc.{Grapheme, Scan, Proc}
import de.sciss.lucre.swing.deferTx

import scala.concurrent.stm.TMap
import scala.swing.TabbedPane.Page
import scala.swing.{Swing, Action, FlowPanel, Label, ScrollPane, BoxPanel, Orientation, TabbedPane, Component}
import Swing._

object ScansViewImpl {
  def apply[S <: Sys[S]](obj: Proc.Obj[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                           workspace: Workspace[S], undoManager: UndoManager): ScansView[S] = {
    new Impl(tx.newHandle(obj)) {
      protected val observer = obj.elem.peer.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          case Proc.ScanAdded   (key, scan) => addScan   (key, scan)
          case Proc.ScanRemoved (key, scan) => removeScan(key)
          case Proc.ScanChange  (key, scan, changes) =>
            changes.foreach {
              case Scan.SourceAdded   (link) => addLink   (key, link)(_.sources)
              case Scan.SourceRemoved (link) => removeLink(key, link)(_.sources)
              case Scan.SinkAdded     (link) => addLink   (key, link)(_.sinks  )
              case Scan.SinkRemoved   (link) => removeLink(key, link)(_.sinks  )
              case _ =>  // grapheme change
            }
          case _ => // graph change
        }
      }

      deferTx(guiInit())

      obj.elem.peer.scans.iterator.foreach {
        case (key, scan) => addScan(key, scan)
      }
    }
  }

  private abstract class Impl[S <: Sys[S]](objH: stm.Source[S#Tx, Proc.Obj[S]])
                                       (implicit val cursor: stm.Cursor[S], val workspace: Workspace[S],
                                        val undoManager: UndoManager)
    extends ScansView[S] with ComponentHolder[Component] {

    private sealed trait LinkView {
      def repr: String
      override def toString = repr
    }
    private final class ScanLinkView    (val repr: String, scanH: stm.Source[S#Tx, Scan[S]])
      extends LinkView

    private final class GraphemeLinkView(val repr: String, graphemeH: stm.Source[S#Tx, Grapheme[S]])
      extends LinkView

    private final class ScanSectionView(parent: ScanView, isSources: Boolean,
                                        val map: IdentifierMap[S#ID, S#Tx, LinkView]) {
      val model     = ListView.Model.empty[LinkView]
      var list      : ListView[LinkView] = _
      var component : Component = _

      def guiInit(): Unit = {
        list      = new ListView(model)
        list.peer.setTransferHandler(new TransferHandler {
          override def canImport(support: TransferSupport): Boolean =
            support.isDataFlavorSupported(ScansView.flavor)

          override def importData(support: TransferSupport): Boolean = {
            support.isDataFlavorSupported(ScansView.flavor) && {
              val drag = support.getTransferable.getTransferData(ScansView.flavor).asInstanceOf[ScansView.Drag[S]]
              drag.workspace == workspace && {
                val editOpt = cursor.step { implicit tx =>
                  val thisScanOpt   = objH     ().elem.peer.scans.get(parent.key)
                  val thatScanOpt   = drag.proc().elem.peer.scans.get(drag  .key)
                  for {
                    thisScan <- thisScanOpt
                    thatScan <- thatScanOpt
                  } yield
                    if (isSources) {
                      EditAddScanLink(source = thatScan, sourceKey = drag  .key, sink = thisScan, sinkKey = parent.key)
                    } else {
                      EditAddScanLink(source = thisScan, sourceKey = parent.key, sink = thatScan, sinkKey = drag  .key)
                    }
                }
                editOpt.foreach(undoManager.add)
                editOpt.isDefined
              }
            }
          }
        })
        list.visibleRowCount = 2
        component = new ScrollPane(list) {
          columnHeaderView = new Label(s"<html><body><i>${if (isSources) "Sources" else "Sinks"}</i></body></html>")
        }
      }
    }

    private final class ScanView(val key: String, sourcesMap: IdentifierMap[S#ID, S#Tx, LinkView],
                                                  sinksMap  : IdentifierMap[S#ID, S#Tx, LinkView]) {
      val sources   = new ScanSectionView(this, isSources = true , map = sourcesMap)
      val sinks     = new ScanSectionView(this, isSources = false, map = sinksMap  )
      var component : Component = _
      var page      : Page      = _

      def guiInit(): Unit = {
        sources.guiInit()
        sinks  .guiInit()
        component = new BoxPanel(Orientation.Vertical) {
          contents += sources.component
          contents += sinks  .component
        }
        page = new Page(key, component)
      }
    }

    protected def observer: stm.Disposable[S#Tx]

    private val scanMap = TMap.empty[String, ScanView]

    private var tab: TabbedPane = _

    private def guiAddScan(): Unit = {
      val opt   = OptionPane.textInput(message = "Scan Name", initial = "scan")
      opt.title = "Add Scan"
      opt.show(GUI.findWindow(component)).foreach { key =>
        val edit = cursor.step { implicit tx =>
          EditAddScan(objH(), key)
        }
        undoManager.add(edit)
      }
    }

    private def guiRemoveScan(): Unit = {
      println("TODO: Remove")
    }

    final protected def guiInit(): Unit = {
      tab           = new TabbedPane

      // tab.preferredSize = (400, 100)
      val ggAdd     = GUI.toolButton(Action(null)(guiAddScan   ()), raphael.Shapes.Plus , "Add Scan"   )
      val ggDelete  = GUI.toolButton(Action(null)(guiRemoveScan()), raphael.Shapes.Minus, "Remove Scan")
      val ggDrag    = new DragSourceButton() {
        protected def createTransferable(): Option[Transferable] = {
          val idx = tab.selection.index
          if (idx < 0) None else {
            val key = tab.selection.page.title
            val d = DragAndDrop.Transferable(ScansView.flavor)(ScansView.Drag[S](workspace, objH, key))
            Some(d)
          }
        }
      }

      val box = new BoxPanel(Orientation.Vertical) {
        contents += tab
        contents += new FlowPanel(/* new Label("Scans:"), */ ggAdd, ggDelete, ggDrag, HGlue)
      }
      box.preferredSize = box.minimumSize
      component = box
    }

    private def mkLinkView(link: Scan.Link[S])(implicit tx: S#Tx): LinkView = link match {
      case Scan.Link.Scan    (peer) => new ScanLinkView    (peer.toString(), tx.newHandle(peer))
      case Scan.Link.Grapheme(peer) => new GraphemeLinkView(peer.toString(), tx.newHandle(peer))
    }

    final protected def addLink(key: String, link: Scan.Link[S])(section: ScanView => ScanSectionView)
                               (implicit tx: S#Tx): Unit = {
      val linkView    = mkLinkView(link)
      val sectionView = section(scanMap(key)(tx.peer))
      deferTx {
        sectionView.model += linkView
      }
    }

    final protected def removeLink(key: String, link: Scan.Link[S])(section: ScanView => ScanSectionView)
                                  (implicit tx: S#Tx): Unit =
      scanMap.get(key)(tx.peer).foreach { scanView =>
        val sectionView = section(scanView)
        val id          = link.id
        val listViewOpt = sectionView.map.get(id)
        sectionView.map.remove(id)
        listViewOpt.foreach { listView =>
          deferTx {
            sectionView.model -= listView
          }
        }
      }

    final protected def addLinks(key: String, links: data.Iterator[S#Tx, Scan.Link[S]])
                                (section: ScanView => ScanSectionView)
                                (implicit tx: S#Tx): Unit =
      links.foreach { link =>
        addLink(key, link)(section)
      }

    final protected def addScan(key: String, scan: Scan[S])(implicit tx: S#Tx): Unit = {
      val sourcesMap  = tx.newInMemoryIDMap[LinkView]
      val sinksMap    = tx.newInMemoryIDMap[LinkView]
      val scanView    = new ScanView(key, sourcesMap, sinksMap)
      scanMap.put(key, scanView)(tx.peer)

      deferTx(scanView.guiInit())

      addLinks(key, scan.sources)(_.sources)
      addLinks(key, scan.sinks  )(_.sinks  )

      deferTx {
        tab.pages += scanView.page
      }
    }

    final protected def removeScan(key: String)(implicit tx: S#Tx): Unit = {
      val viewOpt = scanMap.remove(key)(tx.peer)
      viewOpt.foreach { scanView =>
        deferTx {
          tab.pages -= scanView.page
        }
      }
    }

    final def dispose()(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      observer.dispose()
      scanMap.foreach { case (_, scanView) =>
          scanView.sources.map.dispose()
          scanView.sinks  .map.dispose()
      }
      scanMap.clear()
    }
  }
}
