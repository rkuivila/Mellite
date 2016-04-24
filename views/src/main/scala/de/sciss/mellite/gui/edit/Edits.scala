/*
 *  Edits.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2016 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.lucre.expr.{StringObj, IntObj, LongObj, SpanLikeObj}
import de.sciss.lucre.{expr, stm}
import de.sciss.lucre.stm.{Sys, Obj}
import de.sciss.lucre.swing.edit.EditVar
import de.sciss.mellite.ProcActions.{Move, Resize}
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.{SynthGraph, proc}
import de.sciss.synth.proc.{SynthGraphObj, Code, ObjKeys, Proc}

import scala.collection.breakOut
import scala.util.control.NonFatal

object Edits {
  def setBus[S <: Sys[S]](objects: Iterable[Obj[S]], intExpr: IntObj[S])
                         (implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] = {
    val name  = "Set Bus"
    implicit val intTpe = IntObj
    val edits: List[UndoableEdit] = objects.map { obj =>
      EditAttrMap.expr[S, Int, IntObj](name, obj, ObjKeys.attrBus, Some(intExpr))
//      { ex =>
//        IntObj(IntObj.newVar(ex))
//      }
    } (breakOut)
    CompoundEdit(edits, name)
  }

  def setSynthGraph[S <: Sys[S]](procs: Iterable[Proc[S]], codeElem: Code.Obj[S])
                                (implicit tx: S#Tx, cursor: stm.Cursor[S],
                                 compiler: Code.Compiler): Option[UndoableEdit] = {
    val code = codeElem.value
    code match {
      case csg: Code.SynthGraph =>
        val sg = try {
          csg.execute {}  // XXX TODO: compilation blocks, not good!
        } catch {
          case NonFatal(e) =>
            e.printStackTrace()
            return None
        }

        var scanInKeys  = Set.empty[String]
        var scanOutKeys = Set.empty[String]

        sg.sources.foreach {
          case proc.graph.ScanIn   (key)    => scanInKeys  += key
          case proc.graph.ScanOut  (key, _) => scanOutKeys += key
          case proc.graph.ScanInFix(key, _) => scanInKeys  += key
          case _ =>
        }

        // sg.sources.foreach(println)
        if (scanInKeys .nonEmpty) log(s"SynthDef has the following scan in  keys: ${scanInKeys .mkString(", ")}")
        if (scanOutKeys.nonEmpty) log(s"SynthDef has the following scan out keys: ${scanOutKeys.mkString(", ")}")

        val editName    = "Set Synth Graph"
        val attrNameOpt = codeElem.attr.get(ObjKeys.attrName)
        val edits       = List.newBuilder[UndoableEdit]

        procs.foreach { p =>
          val graphEx = SynthGraphObj.newConst[S](sg)  // XXX TODO: ideally would link to code updates
          implicit val sgTpe = SynthGraphObj
          val edit1   = EditVar.Expr[S, SynthGraph, SynthGraphObj](editName, p.graph, graphEx)
          edits += edit1
          if (attrNameOpt.nonEmpty) {
            val edit2 = EditAttrMap("Set Object Name", p, ObjKeys.attrName, attrNameOpt)
            edits += edit2
          }

          ???! // SCAN
//          def check(scans: Scans[S], keys: Set[String], isInput: Boolean): Unit = {
//            val toRemove = scans.iterator.collect {
//              case (key, scan) if !keys.contains(key) && scan.isEmpty => key
//            }
//            if (toRemove.nonEmpty) toRemove.foreach { key =>
//              edits += EditRemoveScan(p, key = key, isInput = isInput)
//            }
//
//            val existing = scans.iterator.collect {
//              case (key, _) if keys contains key => key
//            }
//            val toAdd = keys -- existing.toSet
//            if (toAdd.nonEmpty) toAdd.foreach { key =>
//              edits += EditAddScan(p, key = key, isInput = isInput)
//            }
//          }
//
//          val proc = p
//          check(proc.inputs , scanInKeys , isInput = true )
//          check(proc.outputs, scanOutKeys, isInput = false)
        }

        CompoundEdit(edits.result(), editName)

      case _ => None
    }
  }

  def setName[S <: Sys[S]](obj: Obj[S], nameOpt: Option[StringObj[S]])
                          (implicit tx: S#Tx, cursor: stm.Cursor[S]): UndoableEdit = {
    implicit val stringTpe = StringObj
    val edit = EditAttrMap.expr[S, String, StringObj]("Rename Object", obj, ObjKeys.attrName, nameOpt)
//    { ex =>
//      StringObj(StringObj.newVar(ex))
//    }
    edit
  }

  // SCAN
//  def addLink[S <: Sys[S]](sourceKey: String, source: Scan[S], sinkKey: String, sink: Scan[S])
//                          (implicit tx: S#Tx, cursor: stm.Cursor[S]): UndoableEdit = {
//    log(s"Link $sourceKey / $source to $sinkKey / $sink")
//    // source.addSink(Scan.Link.Scan(sink))
//    EditAddScanLink(source = source /* , sourceKey */ , sink = sink /* , sinkKey */)
//  }
//
//  def removeLink[S <: Sys[S]](source: Scan[S], sink: Scan[S])
//                             (implicit tx: S#Tx, cursor: stm.Cursor[S]): UndoableEdit = {
//    log(s"Unlink $source from $sink")
//    // source.removeSink(Scan.Link.Scan(sink))
//    EditRemoveScanLink(source = source /* , sourceKey */ , sink = sink /* , sinkKey */)
//  }
//
//  def findLink[S <: Sys[S]](out: Proc[S], in: Proc[S])
//                           (implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[(Scan[S], Scan[S])] = {
//    val outsIt  = out.outputs.iterator // .toList
//    val insSeq0 = in .inputs .iterator.toIndexedSeq
//
//    // if there is already a link between the two, take the drag gesture as a command to remove it
//    val existIt = outsIt.flatMap { case (srcKey, srcScan) =>
//      srcScan.iterator.toList.flatMap {
//        case Scan.Link.Scan(peer) => insSeq0.find(_._2 == peer).map {
//          case (sinkKey, sinkScan) => (srcKey, srcScan, sinkKey, sinkScan)
//        }
//
//        case _ => None
//      }
//    }
//    if (existIt.isEmpty) None else {
//      val (_ /* sourceKey */, source, _ /* sinkKey */, sink) = existIt.next()
//      Some((source, sink))
//    }
//  }

  def linkOrUnlink[S <: Sys[S]](out: Proc[S], in: Proc[S])
                               (implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] = {
    ???! // SCAN
//    val outsIt  = out.outputs.iterator // .toList
//    val insSeq0 = in .inputs .iterator.toIndexedSeq
//
//    // if there is already a link between the two, take the drag gesture as a command to remove it
//    val existIt = outsIt.flatMap { case (srcKey, srcScan) =>
//      srcScan.iterator.toList.flatMap {
//        case Scan.Link.Scan(peer) => insSeq0.find(_._2 == peer).map {
//          case (sinkKey, sinkScan) => (srcKey, srcScan, sinkKey, sinkScan)
//        }
//
//        case _ => None
//      }
//    }
//
//    findLink(out = out, in = in).fold[Option[UndoableEdit]] {
//      // XXX TODO cheesy way to distinguish ins and outs now :-E ... filter by name
//      val outsSeq = out.outputs.iterator.filter(_._1.startsWith("out")).toIndexedSeq
//      val insSeq  = insSeq0                       .filter(_._1.startsWith("in"))
//
//      if (outsSeq.isEmpty || insSeq.isEmpty) return None  // nothing to patch
//
//      if (outsSeq.size == 1 && insSeq.size == 1) {    // exactly one possible connection, go ahead
//        val (sourceKey, source) = outsSeq.head
//        val (sinkKey  , sink  ) = insSeq .head
//        val edit = addLink(sourceKey, source, sinkKey, sink)
//        Some(edit)
//
//      } else {  // present dialog to user
//        log(s"Possible outs: ${outsSeq.map(_._1).mkString(", ")}; possible ins: ${insSeq.map(_._1).mkString(", ")}")
//        println(s"Woop. Multiple choice... Dialog not yet implemented...")
//        None
//      }
//    } { case (source, sink) =>
//      val edit = removeLink(/* sourceKey, */ source, /* sinkKey, */ sink)
//      Some(edit)
//    }
  }

  def resize[S <: Sys[S]](span: SpanLikeObj[S], obj: Obj[S], amount: Resize, minStart: Long)
                         (implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] =
    SpanLikeObj.Var.unapply(span).flatMap { vr =>
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
        // val imp = ExprImplicits[S]

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

        import de.sciss.equal.Implicits._
        val newSpanEx = SpanLikeObj.newConst[S](newSpan)
        if (newSpanEx === oldSpan) None else {
          val name  = "Resize"
          implicit val spanLikeTpe = SpanLikeObj
          val edit0 = EditVar.Expr[S, SpanLike, SpanLikeObj](name, vr, newSpanEx)
          val edit1Opt = if (dStartCC == 0L) None else obj match {
            case objT: Proc[S] =>
              for {
                // objT <- Proc.unapply(obj)
                (LongObj.Var(time), g, audio) <- ProcActions.getAudioRegion2(objT)
              } yield {
                // XXX TODO --- crazy work-around. BiPin / Grapheme
                // must be observed, otherwise underlying SkipList is not updated !!!
                val temp = g.changed.react(_ => _ => ())
                import expr.Ops._
                val newAudioSpan = time() - dStartCC
                implicit val longTpe = LongObj
                val res = EditVar.Expr[S, Long, LongObj](name, time, newAudioSpan)
                temp.dispose()
                res
              }
            case _ => None
          }
          CompoundEdit(edit0 :: edit1Opt.toList, name)
        }
      } else None
    }

  def move[S <: Sys[S]](span: SpanLikeObj[S], obj: Obj[S], amount: Move, minStart: Long)
                         (implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] = {
    var edits = List.empty[UndoableEdit]

    import amount._
    if (deltaTrack != 0) {
      // in the case of const, just overwrite, in the case of
      // var, check the value stored in the var, and update the var
      // instead (recursion). otherwise, it will be some combinatorial
      // expression, and we could decide to construct a binary op instead!
      // val expr = ExprImplicits[S]

      import expr.Ops._
      val newTrack: IntObj[S] = obj.attr.$[IntObj](TimelineObjView.attrTrackIndex) match {
        case Some(IntObj.Var(vr)) => vr() + deltaTrack
        case other => other.fold(0)(_.value) + deltaTrack
      }
      import de.sciss.equal.Implicits._
      val newTrackOpt = if (newTrack === IntObj.newConst[S](0)) None else Some(newTrack)
      implicit val intTpe = IntObj
      val edit = EditAttrMap.expr[S, Int, IntObj]("Adjust Track Placement", obj,
        TimelineObjView.attrTrackIndex, newTrackOpt)
//      { ex =>
//          IntObj(IntObj.newVar(ex))
//        }
      edits ::= edit
    }

    val oldSpan   = span.value
    // val minStart  = canvas.timelineModel.bounds.start
    val deltaC    = if (deltaTime >= 0) deltaTime else oldSpan match {
      case Span.HasStart(oldStart)  => math.max(-(oldStart - minStart)         , deltaTime)
      case Span.HasStop (oldStop)   => math.max(-(oldStop  - minStart + 32 /* MinDur */), deltaTime)
      case _                        => 0  // e.g., Span.All
    }
    val name  = "Move"
    if (deltaC != 0L) {
      // val imp = ExprImplicits[S]
      span match {
        case SpanLikeObj.Var(vr) =>
          // s.transform(_ shift deltaC)
          import expr.Ops._
          val newSpan = vr() shift deltaC
          implicit val spanLikeTpe = SpanLikeObj
          val edit    = EditVar.Expr[S, SpanLike, SpanLikeObj](name, vr, newSpan)
          edits ::= edit
        case _ =>
      }
    }

    CompoundEdit(edits, name)
  }

  def unlinkAndRemove[S <: Sys[S]](timeline: proc.Timeline.Modifiable[S], span: SpanLikeObj[S], obj: Obj[S])
                                  (implicit tx: S#Tx, cursor: stm.Cursor[S]): UndoableEdit = {
    val scanEdits = obj match {
      case objT: Proc[S] =>
//        val proc  = objT
//        // val scans = proc.scans
//        val edits1 = proc.inputs.iterator.toList.flatMap { case (key, scan) =>
//          scan.iterator.collect {
//            case Scan.Link.Scan(source) =>
//              removeLink(source, scan)
//          }.toList
//        }
//        val edits2 = proc.outputs.iterator.toList.flatMap { case (key, scan) =>
//          scan.iterator.collect {
//            case Scan.Link.Scan(sink) =>
//              removeLink(scan, sink)
//          } .toList
//        }
//        edits1 ++ edits2
        ???! // SCAN

      case _ => Nil
    }
    val objEdit = EditTimelineRemoveObj("Object", timeline, span, obj)
    CompoundEdit(scanEdits :+ objEdit, "Remove Object").get // XXX TODO - not nice, `get`
  }
}