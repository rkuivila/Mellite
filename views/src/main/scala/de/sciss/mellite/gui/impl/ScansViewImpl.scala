///*
// *  ScansViewImpl.scala
// *  (Mellite)
// *
// *  Copyright (c) 2012-2015 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is published under the GNU General Public License v3+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.mellite
//package gui
//package impl
//
//import java.awt.datatransfer.Transferable
//import javax.swing.TransferHandler
//import javax.swing.TransferHandler.TransferSupport
//import javax.swing.undo.UndoableEdit
//
//import de.sciss.desktop.edit.CompoundEdit
//import de.sciss.desktop.{OptionPane, UndoManager, Window}
//import de.sciss.icons.raphael
//import de.sciss.lucre.stm
//import de.sciss.lucre.stm.{IdentifierMap, Sys}
//import de.sciss.lucre.swing.deferTx
//import de.sciss.lucre.swing.impl.ComponentHolder
//import de.sciss.mellite.gui.edit.{EditAddScan, EditAddScanLink, EditRemoveScan, EditRemoveScanLink}
//import de.sciss.mellite.gui.impl.component.DragSourceButton
//import de.sciss.swingplus.{ListView, PopupMenu}
//import de.sciss.synth.proc.{Grapheme, Proc}
//import org.scalautils.TypeCheckedTripleEquals
//
//import scala.concurrent.stm.TMap
//import scala.swing.Swing._
//import scala.swing.TabbedPane.Page
//import scala.swing.event.{MouseButtonEvent, MousePressed, MouseReleased}
//import scala.swing.{Action, BoxPanel, Button, Component, FlowPanel, Label, MenuItem, Orientation, ScrollPane, TabbedPane}
//
//object ScansViewImpl {
//  def apply[S <: Sys[S]](obj: Proc[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
//                                           workspace: Workspace[S], undoManager: UndoManager): ScansView[S] = {
//    new Impl(tx.newHandle(obj)) {
//      protected val observer = obj.changed.react { implicit tx => upd =>
//        upd.changes.foreach {
//// SCAN
////          case Proc.InputAdded   (key, scan) => addScan   (key, scan, isInput = true )
//          case Proc.OutputAdded  (key, scan) => ... // SCAN addScan   (key, scan, isInput = false)
//// SCAN
////          case Proc.InputRemoved (key, scan) => removeScan(key, isInput = true )
//          case Proc.OutputRemoved(key, scan) => ... // SCAN removeScan(key, isInput = false)
//// ELEM
////          case Proc.InputChange  (key, scan, changes) =>
////            changes.foreach {
////              case Scan.Added   (link) => addLink   (key, link, isInput = true )
////              case Scan.Removed (link) => removeLink(key, link, isInput = true )
////              case _ =>  // grapheme change
////            }
////          case Proc.OutputChange(key, scan, changes) =>
////            changes.foreach {
////              case Scan.Added     (link) => addLink   (key, link, isInput = false)
////              case Scan.Removed   (link) => removeLink(key, link, isInput = false)
////              case _ =>  // grapheme change
////            }
//          case _ => // graph change
//        }
//      }
//
//      deferTx(guiInit())
//
//      val proc = obj
//      proc.inputs.iterator.foreach {
//        case (key, scan) => addScan(key, scan, isInput = true )
//      }
//      proc.outputs.iterator.foreach {
//        case (key, scan) => addScan(key, scan, isInput = false)
//      }
//    }
//  }
//
//  private abstract class Impl[S <: Sys[S]](objH: stm.Source[S#Tx, Proc[S]])
//                                       (implicit val cursor: stm.Cursor[S], val workspace: Workspace[S],
//                                        val undoManager: UndoManager)
//    extends ScansView[S] with ComponentHolder[Component] { impl =>
//
//    private sealed trait LinkView {
//      def repr: String
//      override def toString = repr
//    }
//    private final class ScanLinkView    (val repr: String, val scanH: stm.Source[S#Tx, Scan[S]])
//      extends LinkView
//
//    private final class GraphemeLinkView(val repr: String, val graphemeH: stm.Source[S#Tx, Grapheme[S]])
//      extends LinkView
//
//    private final class ScanView(val key: String, val isInput: Boolean, val map: IdentifierMap[S#ID, S#Tx, LinkView]) {
//      val model     = ListView.Model.empty[LinkView]
//      var list      : ListView[LinkView] = _
//      var component : Component = _
//      var page      : Page      = _
//
//      def guiInit(): Unit = {
//        list      = new ListView(model)
//        list.peer.setTransferHandler(new TransferHandler {
//          override def canImport(support: TransferSupport): Boolean =
//            support.isDataFlavorSupported(ScansView.flavor)
//
//          override def importData(support: TransferSupport): Boolean = {
//            support.isDataFlavorSupported(ScansView.flavor) && {
//              val drag = support.getTransferable.getTransferData(ScansView.flavor).asInstanceOf[ScansView.Drag[S]]
//              import TypeCheckedTripleEquals._
//              drag.workspace === workspace && {
//                val editOpt = cursor.step { implicit tx =>
//                  val thisProc      = objH     ()
//                  val thatProc      = drag.proc()
//                  val thisScans     = if (isInput) thisProc.inputs  else thisProc.outputs
//                  val thatScans     = if (isInput) thatProc.outputs else thatProc.inputs
//                  val thisScanOpt   = thisScans.get(key)
//                  val thatScanOpt   = thatScans.get(drag.key)
//                  for {
//                    thisScan <- thisScanOpt
//                    thatScan <- thatScanOpt
//                  } yield
//                    if (isInput) {
//                      EditAddScanLink(source = thatScan, sink = thisScan)
//                    } else {
//                      EditAddScanLink(source = thisScan, sink = thatScan)
//                    }
//                }
//                editOpt.foreach(undoManager.add)
//                editOpt.isDefined
//              }
//            }
//          }
//        })
//        list.visibleRowCount = 2
//
//        val pop = new PopupMenu {
//          contents += new MenuItem(Action("Remove Link") {
//            list.selection.items.headOption.foreach {
//              case slv: ScanLinkView =>
//                implicit val cursor = impl.cursor
//                val editOpt = cursor.step { implicit tx =>
//                  val thisProc  = objH()
//                  val scans     = if (isInput) thisProc.inputs else thisProc.outputs
//                  for {
//                    thisScan <- scans.get(key)
//                  } yield {
//                    val thatScan = slv.scanH()
//                    if (isInput)
//                      EditRemoveScanLink(source = thatScan, sink = thisScan)
//                    else
//                      EditRemoveScanLink(source = thisScan, sink = thatScan)
//                  }
//                }
//                editOpt.foreach(undoManager.add)
//
//              case glv: GraphemeLinkView =>
//                println("TODO: Remove Grapheme Link")
//            }
//          })
//        }
//
//        def handleMouse(e: MouseButtonEvent): Unit =
//          if (e.triggersPopup && model.nonEmpty) pop.show(list, e.point.x, e.point.y)
//
//        list.listenTo(list.mouse.clicks)
//        list.reactions += {
//          case e: MousePressed  => handleMouse(e)
//          case e: MouseReleased => handleMouse(e)
//        }
//        component = new ScrollPane(list) {
//          columnHeaderView = new Label(s"<html><body><i>${if (isInput) "Sources" else "Sinks"}</i></body></html>")
//        }
//
//        page = new Page(key, component)
//      }
//    }
//
//    protected def observer: stm.Disposable[S#Tx]
//
//    private val scanInMap   = TMap.empty[String, ScanView]
//    private val scanOutMap  = TMap.empty[String, ScanView]
//    private var tabMap      = Map.empty[TabbedPane.Page, ScanView]
//
//    private var tab: TabbedPane = _
//
//    private class ActionAdd(isInput: Boolean) extends Action(if (isInput) "In" else "Out") {
//      def apply(): Unit = {
//        val key0  = if (isInput) "in" else "out"
//        val tpe   = s"${title}put"
//        val opt   = OptionPane.textInput(message = s"$tpe Name", initial = key0)
//        opt.title = s"Add $tpe"
//        opt.show(Window.find(component)).foreach { key =>
//          val edit = cursor.step { implicit tx =>
//            EditAddScan(objH(), key = key, isInput = isInput)
//          }
//          undoManager.add(edit)
//        }
//      }
//    }
//
//    private lazy val actionRemove = Action(null) {
//      currentViewOption.foreach { case scanView =>
//        val editOpt = cursor.step { implicit tx =>
//          val obj     = objH()
//          val proc    = obj
//          val isInput = scanView.isInput
//          val key     = scanView.key
//          val scans   = if (isInput) proc.inputs else proc.outputs
//          val edits3  = scans.get(key).fold(List.empty[UndoableEdit]) { thisScan =>
//            val edits1 = thisScan.iterator.toList.collect {
//              case Scan.Link.Scan(thatScan) =>
//                val source  = if (isInput) thatScan else thisScan
//                val sink    = if (isInput) thisScan else thatScan
//                EditRemoveScanLink(source = source, sink = sink)
//            }
//            edits1
//          }
//          edits3.foreach(e => println(e.getPresentationName))
//          val editMain = EditRemoveScan(objH(), key = key, isInput = isInput)
//          CompoundEdit(edits3 :+ editMain, "Remove Scan")
//        }
//        editOpt.foreach(undoManager.add)
//      }
//    }
//
//    private var ggDrag: Button = _
//
//    private def tabsUpdated(): Unit = {
//      val enabled = tab.pages.nonEmpty
//      actionRemove.enabled  = enabled
//      ggDrag      .enabled  = enabled
//    }
//
//    private def currentViewOption: Option[ScanView] = {
//      val idx = tab.selection.index
//      if (idx < 0) None else {
//        val page = tab.selection.page
//        tabMap.get(page)
//      }
//    }
//
//    final protected def guiInit(): Unit = {
//      tab           = new TabbedPane
//
//      // tab.preferredSize = (400, 100)
//      val ggAddIn   = GUI.toolButton(new ActionAdd(isInput = true ), raphael.Shapes.Plus, "Add Input" )
//      val ggAddOut  = GUI.toolButton(new ActionAdd(isInput = false), raphael.Shapes.Plus, "Add Output")
//      val ggDelete  = GUI.toolButton(actionRemove, raphael.Shapes.Minus, "Remove Scan")
//      ggDrag        = new DragSourceButton() {
//        protected def createTransferable(): Option[Transferable] =
//          currentViewOption.map { case scanView =>
//            DragAndDrop.Transferable(ScansView.flavor)(ScansView.Drag[S](
//              workspace, objH, scanView.key, isInput = scanView.isInput))
//          }
//      }
//
//      tabsUpdated()
//
//      val box = new BoxPanel(Orientation.Vertical) {
//        contents += tab
//        contents += new FlowPanel(ggAddIn, ggAddOut, ggDelete, ggDrag, HGlue)
//      }
//      box.preferredSize = box.minimumSize
//      component = box
//    }
//
//    private def mkLinkView(link: Scan.Link[S])(implicit tx: S#Tx): LinkView = link match {
//      case Scan.Link.Scan    (peer) => new ScanLinkView    (peer.toString(), tx.newHandle(peer))
//      case Scan.Link.Grapheme(peer) => new GraphemeLinkView(peer.toString(), tx.newHandle(peer))
//    }
//
//    final protected def addLink(key: String, link: Scan.Link[S], isInput: Boolean)
//                               (implicit tx: S#Tx): Unit = {
//      val linkView    = mkLinkView(link)
//      val scanMap     = if (isInput) scanInMap else scanOutMap
//      val scanView    = scanMap(key)(tx.peer)
//      scanView.map.put(link.peerID, linkView)
//      deferTx {
//        scanView.model += linkView
//      }
//    }
//
//    final protected def removeLink(key: String, link: Scan.Link[S], isInput: Boolean)
//                                  (implicit tx: S#Tx): Unit = {
//      val scanMap = if (isInput) scanInMap else scanOutMap
//      scanMap.get(key)(tx.peer).fold(println(s"WARNING: Scan not found: $key")) { scanView =>
//        val id          = link.peerID
//        val listViewOpt = scanView.map.get(id)
//        scanView.map.remove(id)
//        listViewOpt.fold(println(s"WARNING: Link not found: $key, $link")) { listView =>
//          deferTx {
//            scanView.model -= listView
//          }
//        }
//      }
//    }
//
//    // SCAN
////    final protected def addLinks(key: String, links: Iterator[Scan.Link[S]], isInput: Boolean)
////                                (implicit tx: S#Tx): Unit =
////      links.foreach { link =>
////        addLink(key, link, isInput = isInput)
////      }
////
////    final protected def addScan(key: String, scan: Scan[S], isInput: Boolean)(implicit tx: S#Tx): Unit = {
////      val map       = tx.newInMemoryIDMap[LinkView]
////      val scanView  = new ScanView(key = key, isInput = isInput, map = map)
////      val scanMap   = if (isInput) scanInMap else scanOutMap
////      scanMap.put(key, scanView)(tx.peer)
////
////      deferTx(scanView.guiInit())
////
////      addLinks(key, scan.iterator, isInput = isInput)
////
////      deferTx {
////        tab.pages += scanView.page
////        tabMap    += scanView.page -> scanView
////        tabsUpdated()
////      }
////    }
//
//    final protected def removeScan(key: String, isInput: Boolean)(implicit tx: S#Tx): Unit = {
//      val scanMap = if (isInput) scanInMap else scanOutMap
//      val viewOpt = scanMap.remove(key)(tx.peer)
//      viewOpt.foreach { scanView =>
//        deferTx {
//          tab.pages -= scanView.page
//          tabMap    -= scanView.page
//          tabsUpdated()
//        }
//      }
//    }
//
//    final def dispose()(implicit tx: S#Tx): Unit = {
//      implicit val itx = tx.peer
//      observer.dispose()
//      scanInMap.foreach { case (_, scanView) =>
//        scanView.map.dispose()
//      }
//      scanOutMap.foreach { case (_, scanView) =>
//        scanView.map.dispose()
//      }
//      scanInMap .clear()
//      scanOutMap.clear()
//    }
//  }
//}
