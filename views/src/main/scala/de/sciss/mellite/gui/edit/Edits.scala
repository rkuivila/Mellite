/*
 *  Edits.scala
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
package edit

import javax.swing.undo.UndoableEdit

import de.sciss.desktop.edit.CompoundEdit
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.{Expr, Int => IntEx, String => StringEx, Long => LongEx}
import de.sciss.lucre.bitemp.{SpanLike => SpanLikeEx}
import de.sciss.lucre.stm
import de.sciss.lucre.swing.edit.EditVar
import de.sciss.mellite.ProcActions.Resize
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc
import de.sciss.synth.proc.{Code, ExprImplicits, StringElem, Scan, SynthGraphs, Proc, ObjKeys, IntElem, Obj}

import scala.collection.breakOut
import scala.util.control.NonFatal

object Edits {
  def setBus[S <: Sys[S]](objects: Iterable[Obj[S]], intExpr: Expr[S, Int])
                         (implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] = {
    val name  = "Set Bus"
    import IntEx.serializer
    val edits: List[UndoableEdit] = objects.map { obj =>
      EditAttrMap.expr(name, obj, ObjKeys.attrBus, Some(intExpr)) { ex =>
        IntElem(IntEx.newVar(ex))
      }
    } (breakOut)
    CompoundEdit(edits, name)
  }

  def setSynthGraph[S <: Sys[S]](procs: Iterable[Proc.Obj[S]], codeElem: Code.Obj[S])
                                (implicit tx: S#Tx, cursor: stm.Cursor[S],
                                 compiler: Code.Compiler): Option[UndoableEdit] = {
    val code = codeElem.elem.peer.value
    code match {
      case csg: Code.SynthGraph =>
        val sg = try {
          csg.execute {}  // XXX TODO: compilation blocks, not good!
        } catch {
          case NonFatal(e) =>
            e.printStackTrace()
            return None
        }

        val scanKeys: Set[String] = sg.sources.collect {
          case proc.graph.ScanIn   (key)    => key
          case proc.graph.ScanOut  (key, _) => key
          // case proc.graph.ScanInFix(key, _) => key
        } (breakOut)
        // sg.sources.foreach(println)
        if (scanKeys.nonEmpty) log(s"SynthDef has the following scan keys: ${scanKeys.mkString(", ")}")

        val editName = "Set Synth Graph"

        val attrNameOpt = codeElem.attr.get(ObjKeys.attrName)
        val edits: List[UndoableEdit] = procs.flatMap { p =>
          val graphEx = SynthGraphs.newConst[S](sg)  // XXX TODO: ideally would link to code updates
          import SynthGraphs.{serializer, varSerializer}
          val edit1   = EditVar.Expr(editName, p.elem.peer.graph, graphEx)
          // p.elem.peer.graph() = graphEx
          var _edits = if (attrNameOpt.isEmpty) edit1 :: Nil else {
            // p.attr.put(ObjKeys.attrName, attrName)
            val edit2 = EditAttrMap("Set Object Name", p, ObjKeys.attrName, attrNameOpt)
            edit1 :: edit2 :: Nil
          }
          val scans = p.elem.peer.scans
          val toRemove = scans.iterator.collect {
            case (key, scan) if !scanKeys.contains(key) && scan.sinks.isEmpty && scan.sources.isEmpty => key
          }
          if (toRemove.nonEmpty) _edits ++= toRemove.map { key =>
            // scans.remove(key) // unconnected scans which are not referred to from synth def
            EditRemoveScan(p, key)
          } .toList

          val existing = scans.iterator.collect {
            case (key, _) if scanKeys contains key => key
          }
          val toAdd = scanKeys -- existing.toSet
          if (toAdd.nonEmpty) _edits ++= toAdd.map { key =>
            // scans.add(key)
            EditAddScan(p, key)
          } .toList

          _edits
        } (breakOut)

        CompoundEdit(edits, editName)

      case _ => None
    }
  }

  def setName[S <: Sys[S]](obj: Obj[S], nameOpt: Option[Expr[S, String]])
                          (implicit tx: S#Tx, cursor: stm.Cursor[S]): UndoableEdit = {
    import StringEx.serializer
    val edit = EditAttrMap.expr("Rename Object", obj, ObjKeys.attrName, nameOpt) { ex =>
      StringElem(StringEx.newVar(ex))
    }
    edit
  }

  def addLink[S <: Sys[S]](sourceKey: String, source: Scan[S], sinkKey: String, sink: Scan[S])
                          (implicit tx: S#Tx, cursor: stm.Cursor[S]): UndoableEdit = {
    log(s"Link $sourceKey / $source to $sinkKey / $sink")
    // source.addSink(Scan.Link.Scan(sink))
    EditAddScanLink(source = source /* , sourceKey */ , sink = sink /* , sinkKey */)
  }

  def removeLink[S <: Sys[S]](sourceKey: String, source: Scan[S], sinkKey: String, sink: Scan[S])
                             (implicit tx: S#Tx, cursor: stm.Cursor[S]): UndoableEdit = {
    log(s"Unlink $sourceKey / $source from $sinkKey / $sink")
    // source.removeSink(Scan.Link.Scan(sink))
    EditRemoveScanLink(source = source /* , sourceKey */ , sink = sink /* , sinkKey */)
  }

  def linkOrUnlink[S <: Sys[S]](out: Proc.Obj[S], in: Proc.Obj[S])
                               (implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] = {
    val outsIt  = out.elem.peer.scans.iterator // .toList
    val insSeq0 = in .elem.peer.scans.iterator.toIndexedSeq

    // if there is already a link between the two, take the drag gesture as a command to remove it
    val existIt = outsIt.flatMap { case (srcKey, srcScan) =>
      srcScan.sinks.toList.flatMap {
        case Scan.Link.Scan(peer) => insSeq0.find(_._2 == peer).map {
          case (sinkKey, sinkScan) => (srcKey, srcScan, sinkKey, sinkScan)
        }

        case _ => None
      }
    }

    if (existIt.hasNext) {
      val (sourceKey, source, sinkKey, sink) = existIt.next()
      val edit = removeLink(sourceKey, source, sinkKey, sink)
      Some(edit)

    } else {
      // XXX TODO cheesy way to distinguish ins and outs now :-E ... filter by name
      val outsSeq = out.elem.peer.scans.iterator.filter(_._1.startsWith("out")).toIndexedSeq
      val insSeq  = insSeq0                     .filter(_._1.startsWith("in"))

      if (outsSeq.isEmpty || insSeq.isEmpty) return None  // nothing to patch

      if (outsSeq.size == 1 && insSeq.size == 1) {    // exactly one possible connection, go ahead
        val (sourceKey, source) = outsSeq.head
        val (sinkKey  , sink  ) = insSeq .head
        val edit = addLink(sourceKey, source, sinkKey, sink)
        Some(edit)

      } else {  // present dialog to user
        log(s"Possible outs: ${outsSeq.map(_._1).mkString(", ")}; possible ins: ${insSeq.map(_._1).mkString(", ")}")
        println(s"Woop. Multiple choice... Dialog not yet implemented...")
        None
      }
    }
  }

  def resize[S <: Sys[S]](span: Expr[S, SpanLike], obj: Obj[S], amount: Resize, minStart: Long)
                         (implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] =
    Expr.Var.unapply(span).flatMap { vr =>
      import amount.{deltaStart, deltaStop}
      val oldSpan   = span.value
      // val minStart  = timelineModel.bounds.start
      val dStartC   = if (deltaStart >= 0) deltaStart else oldSpan match {
        case Span.HasStart(oldStart) => math.max(-(oldStart - minStart) , deltaStart)
        case _ => 0L
      }
      val dStopC   = if (deltaStop >= 0) deltaStop else oldSpan match {
        case Span.HasStop (oldStop)   => math.max(-(oldStop  - minStart + 32 /* MinDur */), deltaStop)
        case _ => 0L
      }

      if (dStartC != 0L || dStopC != 0L) {
        val imp = ExprImplicits[S]
        import imp._

        val (dStartCC, dStopCC) = (dStartC, dStopC)

        // XXX TODO -- the variable contents should ideally be looked at
        // during the edit performance

        val oldSpan = vr()
        val newSpan = oldSpan.value match {
          case Span.From (start)  => Span.From (start + dStartCC)
          case Span.Until(stop )  => Span.Until(stop  + dStopCC )
          case Span(start, stop)  =>
            val newStart = start + dStartCC
            Span(newStart, math.max(newStart + 32 /* MinDur */, stop + dStopCC))
          case other => other
        }

        if (newSpan == oldSpan) None else {
          import SpanLikeEx.{serializer => spanSer, varSerializer => spanVarSer}
          import LongEx    .{serializer => longSer, varSerializer}
          val name  = "Resize"
          val edit0 = EditVar.Expr(name, vr, newSpan)
          val edit1Opt = if (dStartCC == 0L) None else
            for {
              objT <- Proc.Obj.unapply(obj)
              (Expr.Var(time), audio) <- ProcActions.getAudioRegion(objT)
            } yield {

              for {
                scan <- objT.elem.peer.scans.get(Proc.Obj.graphAudio)
                Scan.Link.Grapheme(g) <- scan.sources.toList.headOption
              } {
                val list = g.debugList()
                println(list)
              }

              val newAudioSpan = time() - dStartCC
              EditVar.Expr(name, time, newAudioSpan)
            }
          CompoundEdit(edit0 :: edit1Opt.toList, name)
        }
      } else None
    }

  //  def unlinkAndRemove[S <: Sys[S]](timeline: Timeline.Modifiable[S], span: Expr[S, SpanLike], obj: Obj[S])
  //                                  (implicit tx: S#Tx, cursor: stm.Cursor[S]): UndoableEdit = {
  //    obj match {
  //      case Proc.Obj(proc) =>
  //        def deleteLinks(map: ProcView.LinkMap[S])(fun: (String, Scan[S], String, Scan[S]) => Unit): Unit =
  //          for {
  //            (thisKey, links) <- map
  //            ProcView.Link(thatView, thatKey) <- links
  //            thisScan <- proc.elem.peer.scans.get(thisKey)
  //            thatScan <- thatView.proc.elem.peer.scans.get(thatKey)
  //          } {
  //            fun(thisKey, thisScan, thatKey, thatScan)
  //          }
  //
  //        deleteLinks(pv.inputs) { (thisKey, thisScan, thatKey, thatScan) =>
  //          ProcActions.removeLink(sourceKey = thatKey, source = thatScan, sinkKey = thisKey, sink = thisScan)
  //        }
  //        deleteLinks(pv.outputs) { (thisKey, thisScan, thatKey, thatScan) =>
  //          ProcActions.removeLink(sourceKey = thisKey, source = thisScan, sinkKey = thatKey, sink = thatScan)
  //        }
  //
  //      case _ =>
  //    }
  //
  //    // group.remove(span, obj)
  //    EditTimelineRemoveObj("Object", timeline, span, obj)
  //  }
}