/*
 *  GlobalProcsViewImpl.scala
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
package timeline

import java.awt.datatransfer.Transferable
import javax.swing.TransferHandler.TransferSupport
import javax.swing.table.{AbstractTableModel, TableColumnModel}
import javax.swing.{DropMode, JComponent, TransferHandler}

import de.sciss.desktop
import de.sciss.desktop.edit.CompoundEdit
import de.sciss.desktop.{Menu, OptionPane, UndoManager}
import de.sciss.icons.raphael
import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.stm
import de.sciss.lucre.swing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.edit.{EditTimelineInsertObj, Edits}
import de.sciss.synth.proc.{Proc, Timeline}
import org.scalautils.TypeCheckedTripleEquals

import scala.annotation.switch
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.swing.Swing._
import scala.swing.event.{MouseButtonEvent, MouseEvent, TableRowsSelected}
import scala.swing.{Action, BorderPanel, BoxPanel, Button, Component, FlowPanel, Orientation, ScrollPane, Table}
import scala.util.Try

object GlobalProcsViewImpl {
  def apply[S <: Sys[S]](group: Timeline[S], selectionModel: SelectionModel[S, TimelineObjView[S]])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S],
                         undo: UndoManager): GlobalProcsView[S] = {

    // import ProcGroup.Modifiable.serializer
    val groupHOpt = group.modifiableOption.map(tx.newHandle(_))
    val view      = new Impl[S](/* tx.newHandle(group), */ groupHOpt, selectionModel)
    deferTx(view.guiInit())
    view
  }

  private final class Impl[S <: Sys[S]](// groupH: stm.Source[S#Tx, Timeline[S]],
                                        groupHOpt: Option[stm.Source[S#Tx, Timeline.Modifiable[S]]],
                                        tlSelModel: SelectionModel[S, TimelineObjView[S]])
                                       (implicit val workspace: Workspace[S], val cursor: stm.Cursor[S],
                                        val undoManager: UndoManager)
    extends GlobalProcsView[S] with ComponentHolder[Component] {

    private var procSeq = Vec.empty[ProcObjView.Timeline[S]]

    private def atomic[A](block: S#Tx => A): A = cursor.step(block)

    private var table: Table = _

    def tableComponent = table

    val selectionModel = SelectionModel[S, ProcObjView.Timeline[S]]

    private val tlSelListener: SelectionModel.Listener[S, TimelineObjView[S]] = {
      case SelectionModel.Update(_, _) =>
        val items = tlSelModel.iterator.flatMap {
          case pv: ProcObjView.Timeline[S] =>
            pv.outputs.flatMap {
              case (_, links) =>
                links.flatMap { link =>
                  val tgt = link.target
                  if (tgt.isGlobal) Some(tgt) else None
                }
            }
          case _ => Nil

        } .toSet

        val indices   = items.map(procSeq.indexOf(_))
        val rows      = table.selection.rows
        val toAdd     = indices.filterNot(rows   .contains)
        val toRemove  = rows   .filterNot(indices.contains)

        if (toRemove.nonEmpty) rows --= toRemove
        if (toAdd   .nonEmpty) rows ++= toAdd
    }

    // columns: name, gain, muted, bus
    private val tm = new AbstractTableModel {
      def getRowCount     = procSeq.size
      def getColumnCount  = 4

      def getValueAt(row: Int, col: Int): AnyRef = {
        val pv  = procSeq(row)
        val res = (col: @switch) match {
          case 0 => pv.name
          case 1 => pv.gain
          case 2 => pv.muted
          case 3 => pv.busOption.getOrElse(0)
        }
        res.asInstanceOf[AnyRef]
      }

      override def isCellEditable(row: Int, col: Int): Boolean = true

      override def setValueAt(value: Any, row: Int, col: Int): Unit = {
        val pv = procSeq(row)
        (col, value) match {
          case (0, name: String) =>
            atomic { implicit tx =>
              ProcActions.rename(pv.obj, if (name.isEmpty) None else Some(name))
            }
          case (1, gainS: String) =>  // XXX TODO: should use spinner for editing
            Try(gainS.toDouble).foreach { gain =>
              atomic { implicit tx =>
                ProcActions.setGain(pv.obj, gain)
              }
            }

          case (2, muted: Boolean) =>
            atomic { implicit tx =>
              ProcActions.toggleMute(pv.obj)
            }

          case (3, busS: String) =>   // XXX TODO: should use spinner for editing
            Try(busS.toInt).foreach { bus =>
              atomic { implicit tx =>
                ProcActions.setBus(pv.obj :: Nil, IntObj.newConst(bus))
              }
            }

          case _ =>
        }
      }

      override def getColumnName(col: Int): String = (col: @switch) match {
        case 0 => "Name"
        case 1 => "Gain"
        case 2 => "M" // short because column only uses checkbox
        case 3 => "Bus"
        // case other => super.getColumnName(col)
      }

      override def getColumnClass(col: Int): Class[_] = (col: @switch) match {
        case 0 => classOf[String]
        case 1 => classOf[Double]
        case 2 => classOf[Boolean]
        case 3 => classOf[Int]
      }
    }

    private def addItemWithDialog(): Unit =
      groupHOpt.foreach { groupH =>
        val op    = OptionPane.textInput(message = "Name:", initial = "Bus")
        op.title  = "Add Global Proc"
        op.show(None).foreach { name =>
          // println(s"Add proc: $name")
          atomic { implicit tx =>
            ProcActions.insertGlobalRegion(groupH(), name, bus = None)
          }
        }
      }

    private def removeProcs(pvs: Iterable[ProcObjView.Timeline[S]]): Unit =
      if (pvs.nonEmpty) groupHOpt.foreach { groupH =>
        atomic { implicit tx =>
          ProcGUIActions.removeProcs(groupH(), pvs)
        }
      }

    private def setColumnWidth(tcm: TableColumnModel, idx: Int, w: Int): Unit = {
      val tc = tcm.getColumn(idx)
      tc.setPreferredWidth(w)
      // tc.setMaxWidth      (w)
    }

    def guiInit(): Unit = {
      table             = new Table()

      // XXX TODO: enable the following - but we're loosing default boolean rendering
      //        // Table default has idiotic renderer/editor handling
      //        override lazy val peer: JTable = new JTable /* with Table.JTableMixin */ with SuperMixin
      //      }
      table.model       = tm
      // table.background  = Color.darkGray
      val jt            = table.peer
      jt.setAutoCreateRowSorter(true)
      // jt.putClientProperty("JComponent.sizeVariant", "small")
      // jt.getRowSorter.setSortKeys(...)
      //      val tcm = new DefaultTableColumnModel {
      //
      //      }
      val tcm = jt.getColumnModel
      setColumnWidth(tcm, 0, 55)
      setColumnWidth(tcm, 1, 47)
      setColumnWidth(tcm, 2, 29)
      setColumnWidth(tcm, 3, 43)
      jt.setPreferredScrollableViewportSize(177 -> 100)

      // ---- drag and drop ----
      jt.setDropMode(DropMode.ON)
      jt.setDragEnabled(true)
      jt.setTransferHandler(new TransferHandler {
        override def getSourceActions(c: JComponent): Int = TransferHandler.LINK

        override def createTransferable(c: JComponent): Transferable = {
          val selRows         = table.selection.rows
          //          if (selRows.isEmpty) null else {
          //            val sel   = selRows.map(procSeq.apply)
          //            val types = Set(Proc.typeID)
          //            val tSel  = DragAndDrop.Transferable(FolderView.selectionFlavor) {
          //              new FolderView.SelectionDnDData(document, sel, types)
          //            }
          //            tSel

          selRows.headOption.map { row =>
            val pv = procSeq(row)
            DragAndDrop.Transferable(timeline.DnD.flavor)(timeline.DnD.GlobalProcDrag(workspace, pv.objH))
          } .orNull
        }

        // ---- import ----
        override def canImport(support: TransferSupport): Boolean =
          support.isDataFlavorSupported(ListObjView.Flavor)

        override def importData(support: TransferSupport): Boolean =
          support.isDataFlavorSupported(ListObjView.Flavor) && {
            Option(jt.getDropLocation).fold(false) { dl =>
              val pv    = procSeq(dl.getRow)
              val drag  = support.getTransferable.getTransferData(ListObjView.Flavor)
                .asInstanceOf[ListObjView.Drag[S]]
              import TypeCheckedTripleEquals._
              drag.workspace === workspace && {
                drag.view match {
                  case iv: IntObjView[S] =>
                    atomic { implicit tx =>
                      val objT = iv.obj
                      val intExpr = objT
                      ProcActions.setBus(pv.obj :: Nil, intExpr)
                      true
                    }

                  case iv: CodeObjView[S] =>
                    atomic { implicit tx =>
                      val objT = iv.obj
                      import Mellite.compiler
                      ProcActions.setSynthGraph(pv.obj :: Nil, objT)
                      true
                    }

                  case _ => false
                }
              }
            }
          }
      })

      val scroll    = new ScrollPane(table)
      scroll.border = null

      val actionAdd = Action(null)(addItemWithDialog())
      val ggAdd: Button = GUI.toolButton(actionAdd, raphael.Shapes.Plus, "Add Global Process")
      // ggAdd.peer.putClientProperty("JButton.buttonType", "roundRect")

      val actionDelete = Action(null) {
        val pvs = table.selection.rows.map(procSeq)
        removeProcs(pvs)
      }
      val ggDelete: Button = GUI.toolButton(actionDelete, raphael.Shapes.Minus, "Delete Global Process")
      actionDelete.enabled = false
      // ggDelete.peer.putClientProperty("JButton.buttonType", "roundRect")

      val actionAttr: Action = Action(null) {
        if (selectionModel.nonEmpty) cursor.step { implicit tx =>
          selectionModel.iterator.foreach { view =>
            AttrMapFrame(view.obj)
          }
        }
      }

      val actionEdit: Action = Action(null) {
        if (selectionModel.nonEmpty) cursor.step { implicit tx =>
          selectionModel.iterator.foreach { view =>
            if (view.isViewable) view.openView(None)  //  /// XXX TODO - find window
          }
        }
      }

      val ggAttr = GUI.toolButton(actionAttr, raphael.Shapes.Wrench, "Attributes Editor")
      actionAttr.enabled = false

      val ggEdit = GUI.toolButton(actionEdit, raphael.Shapes.View, "Proc Editor")
      actionEdit.enabled = false

      table.listenTo(table.selection, table.mouse.clicks)
      table.reactions += {
        case TableRowsSelected(_, _, _) =>
          val range   = table.selection.rows
          val hasSel  = range.nonEmpty
          actionDelete.enabled = hasSel
          actionAttr  .enabled = hasSel
          actionEdit  .enabled = hasSel
          // println(s"Table range = $range")
          val newSel = range.map(procSeq(_))
          selectionModel.iterator.foreach { v =>
            if (!newSel.contains(v)) {
              // println(s"selectionModel -= $v")
              selectionModel -= v
            }
          }
          newSel.foreach { v =>
            if (!selectionModel.contains(v)) {
              // println(s"selectionModel += $v")
              selectionModel += v
            }
          }

        case e: MouseButtonEvent if e.triggersPopup => showPopup(e)
      }

      tlSelModel addListener tlSelListener

      val pBottom = new BoxPanel(Orientation.Vertical)
      if (groupHOpt.isDefined) {
        // only add buttons if group is modifiable
        pBottom.contents += new FlowPanel(ggAdd, ggDelete)
      }
      pBottom.contents += new FlowPanel(ggAttr, ggEdit)

      component = new BorderPanel {
        add(scroll , BorderPanel.Position.Center)
        add(pBottom, BorderPanel.Position.South )
      }
    }

    private def showPopup(e: MouseEvent): Unit = desktop.Window.find(component).foreach { w =>
      val hasGlobal = selectionModel.nonEmpty
      val hasTL     = tlSelModel    .nonEmpty
      if (hasGlobal) {
        import Menu._
        // val itSelect      = Item("select"        )("Select Connected Regions")(selectRegions())
        val itDup         = Item("duplicate"     )("Duplicate")(duplicate(connect = false))
        val itDupC        = Item("duplicate-con" )("Duplicate with Connections")(duplicate(connect = true))
        val itConnect     = Item("connect"       )("Connect to Selected Regions")(connectToRegions())
        val itDisconnect  = Item("disconnect"    )("Disconnect from Selected Regions")(disconnectFromSelectedRegions())
        val itDisconnectA = Item("disconnect-all")("Disconnect from All Regions")(disconnectFromAllRegions())
        if (groupHOpt.isEmpty) {
          itDup .disable()
          itDupC.disable()
        }
        if (!hasTL) {
          itConnect   .disable()
          itDisconnect.disable()
        }

        val pop = Popup().add(itDup).add(itDupC).add(Line).add(itConnect).add(itDisconnect).add(itDisconnectA)
        pop.create(w).show(component, e.point.x, e.point.y)
      }
    }

//    private def selectRegions(): Unit = {
//      val itGlob = selectionModel.iterator
//      cursor.step { implicit tx =>
//        val scans = itGlob.flatMap { inView =>
//          inView.obj.scans.get("in")
//        }
//        tlSelModel.
//      }
//    }

    private def duplicate(connect: Boolean): Unit = groupHOpt.foreach { groupH =>
      val itGlob = selectionModel.iterator
      val edits = cursor.step { implicit tx =>
        val tl = groupH()
        val it = itGlob.map { inView =>
          val inObj   = inView.obj
          val span    = inView.span   // not necessary to copy
          val outObj  = ProcActions.copy[S](inObj)
          // `copy` has already connected all scans.
          // So disconnect them if necessary.
          if (!connect) outObj match {
            case procObj: Proc[S] =>
              val proc    = procObj
              ???! // SCAN
//              proc.inputs.iterator.foreach { case (_, scan) =>
//                scan.iterator.foreach(scan.remove(_))
//              }
//              proc.outputs.iterator.foreach { case (_, scan) =>
//                scan.iterator.foreach(scan.remove(_))
//              }
            case _ =>
          }
          EditTimelineInsertObj("Global Proc", tl, span, outObj)
        }
        it.toList   // tricky, need to unwind transactional iterator
      }
      val editOpt = CompoundEdit(edits, "Duplicate Global Procs")
      editOpt.foreach(undoManager.add)
    }

    private def connectToRegions(): Unit = {
      val seqGlob = selectionModel.iterator.toSeq
      val seqTL   = tlSelModel    .iterator.toSeq
      val plGlob  = seqGlob.size > 1
      val plTL    = seqTL  .size > 1
      ???! // SCAN
//      val edits   = cursor.step { implicit tx =>
//        val it = for {
//          outView <- seqTL
//          inView  <- seqGlob
//          in      = inView.obj
//          out     <- (outView.obj match { case p: Proc[S] => Some(p); case _ => None }) // Proc.unapply(outView.obj)
//          source  <- out.outputs.get(Proc.scanMainOut)
//          sink    <- in .inputs .get(Proc.scanMainIn )
//          if Edits.findLink(out = out, in = in).isEmpty
//        } yield Edits.addLink(sourceKey = "out", source = source, sinkKey = "in", sink = sink)
//        it.toList   // tricky, need to unwind transactional iterator
//      }
//      val editOpt = CompoundEdit(edits,
//        s"Connect Global ${if (plGlob) "Procs" else "Proc"} to Selected ${if (plTL) "Regions" else "Region"}")
//      editOpt.foreach(undoManager.add)
    }

    private def disconnectFromSelectedRegions(): Unit = {
      val seqGlob = selectionModel.iterator.toSeq
      val seqTL   = tlSelModel    .iterator.toSeq
      val plGlob  = seqGlob.size > 1
      val plTL    = seqTL  .size > 1
      ???! // SCAN
//      val edits   = cursor.step { implicit tx =>
//        val it = for {
//          outView <- seqTL
//          inView  <- seqGlob
//          in      = inView.obj
//          out     <- (outView.obj match { case p: Proc[S] => Some(p); case _ => None }) // Proc.unapply(outView.obj)
//          (source, sink) <- Edits.findLink(out = out, in = in)
//        } yield Edits.removeLink(source = source, sink = sink)
//        it.toList   // tricky, need to unwind transactional iterator
//      }
//      val editOpt = CompoundEdit(edits,
//        s"Disconnect Global ${if (plGlob) "Procs" else "Proc"} from Selected ${if (plTL) "Regions" else "Region"}")
//      editOpt.foreach(undoManager.add)
    }

    private def disconnectFromAllRegions(): Unit = {
      val seqGlob = selectionModel.iterator.toList
      val plGlob  = seqGlob.size > 1
      val edits   = cursor.step { implicit tx =>
        seqGlob.flatMap { inView =>
          val in = inView.obj
          ???! // SCAN
//          in.inputs.get(Proc.scanMainIn).toList.flatMap { sink =>
//            sink.iterator.collect {
//              case Scan.Link.Scan(source) => EditRemoveScanLink(source = source, sink = sink)
//            } .toList
//          }
        }
      }
      val editOpt = CompoundEdit(edits,
        s"Disconnect Global ${if (plGlob) "Procs" else "Proc"} from All Regions")
      editOpt.foreach(undoManager.add)
    }

    def dispose()(implicit tx: S#Tx): Unit = deferTx {
      tlSelModel removeListener tlSelListener
    }

    def add(proc: ProcObjView.Timeline[S]): Unit = {
      val row   = procSeq.size
      procSeq :+= proc
      tm.fireTableRowsInserted(row, row)
    }

    def remove(proc: ProcObjView.Timeline[S]): Unit = {
      val row   = procSeq.indexOf(proc)
      procSeq   = procSeq.patch(row, Vec.empty, 1)
      tm.fireTableRowsDeleted(row, row)
    }

    def iterator: Iterator[ProcObjView.Timeline[S]] = procSeq.iterator

    def updated(proc: ProcObjView.Timeline[S]): Unit = {
      val row   = procSeq.indexOf(proc)
      tm.fireTableRowsUpdated(row, row)
    }
  }
}