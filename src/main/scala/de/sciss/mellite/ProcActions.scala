/*
 *  ProcActions.scala
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

package de.sciss
package mellite

import lucre.expr.Expr
import span.{Span, SpanLike}
import de.sciss.synth.proc.{ProcGroup, Obj, SynthGraphs, ExprImplicits, Elem, ProcKeys, Scan, Grapheme, Proc, StringElem, DoubleElem, IntElem, BooleanElem, ProcElem}
import de.sciss.lucre.bitemp.{BiGroup, BiExpr}
import de.sciss.audiowidgets.TimelineModel
import de.sciss.synth.proc
import scala.util.control.NonFatal
import collection.breakOut
import de.sciss.lucre.expr.{Int => IntEx, Boolean => BooleanEx, Double => DoubleEx, Long => LongEx, String => StringEx}
import de.sciss.lucre.bitemp.{Span => SpanEx}
import proc.Implicits._
import de.sciss.lucre.event.Sys

object ProcActions {
  private val MinDur    = 32

  // scalac still has bug finding ProcGroup.Modifiable
  private type ProcGroupMod[S <: Sys[S]] = ProcGroup.Modifiable[S] // BiGroup.Modifiable[S, Proc[S], Proc.Update[S]]

  final case class Resize(deltaStart: Long, deltaStop: Long)

  /** Queries the audio region's grapheme segment start and audio element. */
  def getAudioRegion[S <: Sys[S]](span: Expr[S, SpanLike], proc: Obj.T[S, ProcElem])
                                 (implicit tx: S#Tx): Option[(Expr[S, Long], Grapheme.Elem.Audio[S])] = {
    span.value match {
      case Span.HasStart(frame) =>
        for {
          scan <- proc.elem.peer.scans.get(ProcKeys.graphAudio)
          Scan.Link.Grapheme(g) <- scan.sources.toList.headOption
          BiExpr(time, audio: Grapheme.Elem.Audio[S]) <- g.at(frame)
        } yield (time, audio)

      case _ => None
    }
  }

  def resize[S <: Sys[S]](span: Expr[S, SpanLike], proc: Obj.T[S, ProcElem],
                          amount: Resize, timelineModel: TimelineModel)
                         (implicit tx: S#Tx): Unit = {
    import amount._

    val oldSpan   = span.value
    val minStart  = timelineModel.bounds.start
    val dStartC   = if (deltaStart >= 0) deltaStart else oldSpan match {
      case Span.HasStart(oldStart)  => math.max(-(oldStart - minStart)         , deltaStart)
      case _ => 0L
    }
    val dStopC   = if (deltaStop >= 0) deltaStop else oldSpan match {
      case Span.HasStop (oldStop)   => math.max(-(oldStop  - minStart + MinDur), deltaStop)
      case _ => 0L
    }

    if (dStartC != 0L || dStopC != 0L) {
      val imp = ExprImplicits[S]
      import imp._

      val (dStartCC, dStopCC) = getAudioRegion(span, proc) match {
        //        case Some((gtime, audio)) => // audio region
        //          val gtimeV  = gtime.value
        //          val audioV  = audio.value
        //          dStartC

        case _ => // other proc
          (dStartC, dStopC)
      }

      span match {
        case Expr.Var(s) =>
          s.transform { oldSpan =>
            oldSpan.value match {
              case Span.From (start)  => Span.From (start + dStartCC)
              case Span.Until(stop )  => Span.Until(stop  + dStopCC )
              case Span(start, stop)  =>
                val newStart = start + dStartCC
                Span(newStart, math.max(newStart + MinDur, stop + dStopCC))
              case other => other
            }
          }
      }
    }
  }

  /** Changes or removes the name of a process.
    *
    * @param proc the proc to rename
    * @param name the new name or `None` to remove the name attribute
    */
  def rename[S <: Sys[S]](proc: Obj.T[S, ProcElem], name: Option[String])(implicit tx: S#Tx): Unit = {
    val attr  = proc.attr
    val imp   = ExprImplicits[S]
    import imp._
    name match {
      case Some(n) =>
        attr.expr[String](ProcKeys.attrName) match {
          case Some(Expr.Var(vr)) => vr() = n
          case _                  => attr.put(ProcKeys.attrName, StringElem(StringEx.newVar(n)))
        }

      case _ => attr.remove(ProcKeys.attrName)
    }
  }

  /** Makes a copy of a proc. Copies the graph and all attributes, creates scans with the same keys
    * and connects _outgoing_ scans.
    *
    * @param proc the process to copy
    * @param span the process span. if given, tries to copy the audio grapheme as well.
    * @return
    */
  def copy[S <: Sys[S]](proc: Obj.T[S, ProcElem], span: Option[Expr[S, SpanLike]])
                       (implicit tx: S#Tx): Obj.T[S, ProcElem] = {
    val pNew    = Proc[S]
    val res     = Obj(ProcElem(pNew))
    pNew.graph() = proc.elem.peer.graph
    proc.attr.iterator.foreach { case (key, attr) =>
      val attrOut = attr.mkCopy()
      res.attr.put(key, attrOut)
    }
    proc.elem.peer.scans.keys.foreach(pNew.scans.add)
    span.foreach { sp =>
      ProcActions.getAudioRegion(sp, proc).foreach { case (time, audio) =>
        val imp = ExprImplicits[S]
        import imp._
        val scanw       = pNew.scans.add(ProcKeys.graphAudio)
        val grw         = Grapheme.Modifiable[S]
        val gStart      = LongEx  .newVar(time        .value)
        val audioOffset = LongEx  .newVar(audio.offset.value)  // XXX TODO
        val audioGain   = DoubleEx.newVar(audio.gain  .value)
        val gElem       = Grapheme.Elem.Audio(audio.artifact, audio.value.spec, audioOffset, audioGain)
        val bi: Grapheme.TimedElem[S] = BiExpr(gStart, gElem)
        grw.add(bi)
        scanw addSource grw
      }
    }

    // connect outgoing scans
    proc.elem.peer.scans.iterator.foreach {
      case (key, scan) =>
        val sinks = scan.sinks
        if (sinks.nonEmpty) {
          pNew.scans.get(key).foreach { scan2 =>
            scan.sinks.foreach { link =>
              scan2.addSink(link)
            }
          }
        }
    }

    res
  }

  def setGain[S <: Sys[S]](proc: Obj.T[S, ProcElem], gain: Double)(implicit tx: S#Tx): Unit = {
    val attr  = proc.attr
    val imp   = ExprImplicits[S]
    import imp._

    if (gain == 1.0) {
      attr.remove(ProcKeys.attrGain)
    } else {
      attr.expr[Double](ProcKeys.attrGain) match {
        case Some(Expr.Var(vr)) => vr() = gain
        case _                  => attr.put(ProcKeys.attrGain, DoubleElem(DoubleEx.newVar(gain)))
      }
    }
  }

  def adjustGain[S <: Sys[S]](proc: Obj.T[S, ProcElem], factor: Double)(implicit tx: S#Tx): Unit = {
    if (factor == 1.0) return

    val attr  = proc.attr
    val imp   = ExprImplicits[S]
    import imp._

    attr.expr[Double](ProcKeys.attrGain) match {
      case Some(Expr.Var(vr)) => vr.transform(_ * factor)
      case other =>
        val newGain = other.map(_.value).getOrElse(1.0) * factor
        attr.put(ProcKeys.attrGain, DoubleElem(DoubleEx.newVar(newGain)))
    }
  }

  def setBus[S <: Sys[S]](procs: Iterable[Obj.T[S, ProcElem]], intExpr: Expr[S, Int])(implicit tx: S#Tx): Unit = {
    val attr    = IntElem(intExpr)
    procs.foreach { proc =>
      proc.attr.put(ProcKeys.attrBus, attr)
    }
  }

  def toggleMute[S <: Sys[S]](proc: Obj.T[S, ProcElem])(implicit tx: S#Tx): Unit = {
    val imp   = ExprImplicits[S]
    import imp._

    val attr = proc.attr
    attr.expr[Boolean](ProcKeys.attrMute) match {
      // XXX TODO: BooleanEx should have `not` operator
      case Some(Expr.Var(vr)) => vr.transform { old => val vOld = old.value; !vOld }
      case _                  => attr.put(ProcKeys.attrMute, BooleanElem(BooleanEx.newVar(true)))
    }
  }

  def setSynthGraph[S <: Sys[S]](procs: Iterable[Obj.T[S, ProcElem]], codeElem: Obj.T[S, Code.Elem])
                                (implicit tx: S#Tx): Boolean = {
    val code = codeElem.elem.peer.value
    code match {
      case csg: Code.SynthGraph =>
        try {
          val sg = csg.execute {}  // XXX TODO: compilation blocks, not good!

          val scanKeys: Set[String] = sg.sources.collect {
            case proc.graph.scan.In   (key, _) => key
            case proc.graph.scan.Out  (key, _) => key
            case proc.graph.scan.InFix(key, _) => key
          } (breakOut)
          // sg.sources.foreach(println)
          if (scanKeys.nonEmpty) log(s"SynthDef has the following scan keys: ${scanKeys.mkString(", ")}")

          val attrNameOpt = codeElem.attr.get(ProcKeys.attrName)
          procs.foreach { p =>
            p.elem.peer.graph() = SynthGraphs.newConst[S](sg)  // XXX TODO: ideally would link to code updates
            attrNameOpt.foreach(attrName => p.attr.put(ProcKeys.attrName, attrName))
            val scans = p.elem.peer.scans
            val toRemove = scans.iterator.collect {
              case (key, scan) if !scanKeys.contains(key) && scan.sinks.isEmpty && scan.sources.isEmpty => key
            }
            toRemove.foreach(scans.remove(_)) // unconnected scans which are not referred to from synth def
            val existing = scans.iterator.collect {
              case (key, _) if scanKeys contains key => key
            }
            val toAdd = scanKeys -- existing.toSet
            toAdd.foreach(scans.add)
          }
          true

        } catch {
          case NonFatal(e) =>
            e.printStackTrace()
            false
        }

      case _ => false
    }
  }

  /** Inserts a new audio region proc into a given group.
    *
    * @param group      the group to insert the proc into
    * @param time       the time offset in the group
    * @param track      the track to associate with the proc, or `-1` to have the track undefined
    * @param grapheme   the grapheme carrying the underlying audio file
    * @param selection  the selection with respect to the grapheme. This is the span inside the underlying audio file,
    *                   whereas the proc will be placed in the group aligned with `time`.
    * @param bus        an optional bus to assign
    * @return           a tuple consisting of the span expression and the newly created proc.
    */
  def insertAudioRegion[S <: Sys[S]](
      group     : ProcGroupMod[S],
      time      : Long,
      track     : Int,
      grapheme  : Grapheme.Elem.Audio[S],
      selection : Span,
      bus       : Option[Expr[S, Int]]) // stm.Source[S#Tx, Element.Int[S]]])
     (implicit tx: S#Tx): (Expr[S, Span], Obj.T[S, ProcElem]) = {

    val imp = ExprImplicits[S]
    import imp._

    val spanV   = Span(time, time + selection.length)
    val span    = SpanEx.newVar[S](spanV)
    val proc    = Proc[S]
    val obj     = Obj(ProcElem(proc))
    val attr    = obj.attr
    if (track >= 0) attr.put(ProcKeys.attrTrack, IntElem(IntEx.newVar(track)))
    bus.foreach { busEx =>
      val bus = IntElem(busEx)
      attr.put(ProcKeys.attrBus, bus)
    }

    val scanIn  = proc.scans.add(ProcKeys.graphAudio)
    /* val scanOut = */ proc.scans.add(ProcKeys.scanMainOut)
    val grIn    = Grapheme.Modifiable[S]

    // we preserve data.source(), i.e. the original audio file offset
    // ; therefore the grapheme element must start `selection.start` frames
    // before the insertion position `drop.frame`
    val gStart  = LongEx.newVar(time - selection.start)  // wooopa, could even be a bin op at some point
    val bi: Grapheme.TimedElem[S] = BiExpr(gStart, grapheme)
    grIn.add(bi)
    scanIn addSource grIn
    proc.graph() = SynthGraphs.tape
    group.add(span, obj)

    (span, obj)
  }

  def insertGlobalRegion[S <: Sys[S]](
      group     : ProcGroupMod[S],
      name      : String,
      bus       : Option[Expr[S, Int]]) // stm.Source[S#Tx, Element.Int[S]]])
     (implicit tx: S#Tx): Obj.T[S, ProcElem] = {

    val imp = ExprImplicits[S]
    import imp._

    val proc    = Proc[S]
    val obj     = Obj(ProcElem(proc))
    val attr    = obj.attr
    val nameEx  = StringEx.newVar[S](StringEx.newConst(name))
    attr.put(ProcKeys.attrName, StringElem(nameEx))

    group.add(Span.All, obj) // constant span expression
    obj
  }


  private def addLink[S <: Sys[S]](sourceKey: String, source: Scan[S], sinkKey: String, sink: Scan[S])
                                  (implicit tx: S#Tx): Unit = {
    log(s"Link $sourceKey / $source to $sinkKey / $sink")
    source.addSink(Scan.Link.Scan(sink))
  }

  def removeLink[S <: Sys[S]](sourceKey: String, source: Scan[S], sinkKey: String, sink: Scan[S])
                             (implicit tx: S#Tx): Unit = {
    log(s"Unlink $sourceKey / $source from $sinkKey / $sink")
    source.removeSink(Scan.Link.Scan(sink))
  }

  def linkOrUnlink[S <: Sys[S]](out: Obj.T[S, ProcElem], in: Obj.T[S, ProcElem])(implicit tx: S#Tx): Boolean = {
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
      val (srcKey, srcScan, sinkKey, sinkScan) = existIt.next()
      removeLink(srcKey, srcScan, sinkKey, sinkScan)
      true

    } else {
      // XXX TODO cheesy way to distinguish ins and outs now :-E ... filter by name
      val outsSeq = out.elem.peer.scans.iterator.filter(_._1.startsWith("out")).toIndexedSeq
      val insSeq  = insSeq0                     .filter(_._1.startsWith("in"))

      if (outsSeq.isEmpty || insSeq.isEmpty) return false   // nothing to patch

      if (outsSeq.size == 1 && insSeq.size == 1) {    // exactly one possible connection, go ahead
        val (srcKey , src ) = outsSeq.head
        val (sinkKey, sink) = insSeq .head
        addLink(srcKey, src, sinkKey, sink)
        true

      } else {  // present dialog to user
        log(s"Possible outs: ${outsSeq.map(_._1).mkString(", ")}; possible ins: ${insSeq.map(_._1).mkString(", ")}")
        println(s"Woop. Multiple choice... Dialog not yet implemented...")
        false
      }
    }
  }
}