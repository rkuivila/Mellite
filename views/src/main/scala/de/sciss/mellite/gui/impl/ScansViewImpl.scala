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

import de.sciss.desktop.{OptionPane, UndoManager}
import de.sciss.icons.raphael
import de.sciss.lucre.stm
import de.sciss.lucre.data
import de.sciss.lucre.stm.IdentifierMap
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.event.Sys
import de.sciss.mellite.gui.edit.EditAddScan
import de.sciss.swingplus.ListView
import de.sciss.synth.proc.{Grapheme, Scan, Proc}
import de.sciss.lucre.swing.{deferTx, requireEDT}

import scala.concurrent.stm.TMap
import scala.swing.TabbedPane.Page
import scala.swing.{Swing, Action, FlowPanel, Label, ScrollPane, BoxPanel, Orientation, TabbedPane, Component}
import Swing._

object ScansViewImpl {
  def apply[S <: Sys[S]](obj: Proc.Obj[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                        undoManager: UndoManager): ScansView[S] = {
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

  private sealed trait LinkView[S <: Sys[S]] {
    def repr: String

    override def toString = repr
  }
  private final class ScanLinkView    [S <: Sys[S]](val repr: String, scanH: stm.Source[S#Tx, Scan[S]])
    extends LinkView[S]

  private final class GraphemeLinkView[S <: Sys[S]](val repr: String, graphemeH: stm.Source[S#Tx, Grapheme[S]])
    extends LinkView[S]

  private final class ScanSectionView[S <: Sys[S]](title: String, val map: IdentifierMap[S#ID, S#Tx, LinkView[S]]) {
    val model     = ListView.Model.empty[LinkView[S]]
    var list      : ListView[LinkView[S]] = _
    var component : Component = _

    def guiInit(): Unit = {
      list      = new ListView(model)
      component = new ScrollPane(list) {
        columnHeaderView = new Label(s"<html><body><i>$title</i></body></html>")
      }
    }
  }

  private final class ScanView[S <: Sys[S]](val key: String, sourcesMap: IdentifierMap[S#ID, S#Tx, LinkView[S]],
                                                             sinksMap  : IdentifierMap[S#ID, S#Tx, LinkView[S]]) {
    val sources   = new ScanSectionView[S]("Sources", sourcesMap)
    val sinks     = new ScanSectionView[S]("Sinks"  , sinksMap  )
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

  private abstract class Impl[S <: Sys[S]](objH: stm.Source[S#Tx, Proc.Obj[S]])
                                       (implicit val cursor: stm.Cursor[S], val undoManager: UndoManager)
    extends ScansView[S] with ComponentHolder[Component] {

    protected def observer: stm.Disposable[S#Tx]

    private val scanMap = TMap.empty[String, ScanView[S]]

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
      val box = new BoxPanel(Orientation.Vertical) {
        contents += tab
        contents += new FlowPanel(new Label("Scans:"), ggAdd, ggDelete, HGlue)
      }
      box.preferredSize = box.minimumSize
      component = box
    }

    private def mkLinkView(link: Scan.Link[S])(implicit tx: S#Tx): LinkView[S] = link match {
      case Scan.Link.Scan    (peer) => new ScanLinkView    (peer.toString(), tx.newHandle(peer))
      case Scan.Link.Grapheme(peer) => new GraphemeLinkView(peer.toString(), tx.newHandle(peer))
    }

    final protected def addLink(key: String, link: Scan.Link[S])(section: ScanView[S] => ScanSectionView[S])
                               (implicit tx: S#Tx): Unit = {
      val linkView    = mkLinkView(link)
      val sectionView = section(scanMap(key)(tx.peer))
      deferTx {
        sectionView.model += linkView
      }
    }

    final protected def removeLink(key: String, link: Scan.Link[S])(section: ScanView[S] => ScanSectionView[S])
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
                                (section: ScanView[S] => ScanSectionView[S])
                                (implicit tx: S#Tx): Unit =
      links.foreach { link =>
        addLink(key, link)(section)
      }

    final protected def addScan(key: String, scan: Scan[S])(implicit tx: S#Tx): Unit = {
      val sourcesMap  = tx.newInMemoryIDMap[LinkView[S]]
      val sinksMap    = tx.newInMemoryIDMap[LinkView[S]]
      val scanView    = new ScanView[S](key, sourcesMap, sinksMap)
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
