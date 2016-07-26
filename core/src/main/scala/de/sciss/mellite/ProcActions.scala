/*
 *  ProcActions.scala
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

package de.sciss
package mellite

import java.{util => ju}

import de.sciss.lucre.expr
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, IntObj, LongObj, SpanLikeObj, StringObj}
import de.sciss.lucre.stm.{Copy, Obj, Sys}
import de.sciss.span.Span
import de.sciss.synth.proc.{AudioCue, Code, ObjKeys, Proc, SynthGraphObj, Timeline}
import de.sciss.synth.ugen.{BinaryOpUGen, Constant, UnaryOpUGen}
import de.sciss.synth.{GE, Lazy, Rate, SynthGraph, UGenSpec, proc}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.existentials
import scala.util.control.NonFatal

object ProcActions {
  private val MinDur    = 32

  // scalac still has bug finding Timeline.Modifiable
  // private type TimelineMod[S <: Sys[S]] = Timeline.Modifiable[S] // BiGroup.Modifiable[S, Proc[S], Proc.Update[S]]
  private type TimelineMod[S <: Sys[S]] = Timeline.Modifiable[S]

  final case class Resize(deltaStart: Long, deltaStop: Long)
  final case class Move  (deltaTime : Long, deltaTrack: Int, copy: Boolean)

  /** Queries the audio region's grapheme segment start and audio element. */
  def getAudioRegion[S <: Sys[S]](proc: Proc[S])(implicit tx: S#Tx): Option[AudioCue.Obj[S]] =
    proc.attr.$[AudioCue.Obj](Proc.graphAudio)

  def resize[S <: Sys[S]](span: SpanLikeObj[S], obj: Obj[S],
                          amount: Resize, minStart: Long /* timelineModel: TimelineModel */)
                         (implicit tx: S#Tx): Unit = {
    import amount._

    val oldSpan   = span.value
    // val minStart  = timelineModel.bounds.start
    val dStartC   = if (deltaStart >= 0) deltaStart else oldSpan match {
      case Span.HasStart(oldStart)  => math.max(-(oldStart - minStart)         , deltaStart)
      case _ => 0L
    }
    val dStopC   = if (deltaStop >= 0) deltaStop else oldSpan match {
      case Span.HasStop (oldStop)   => math.max(-(oldStop  - minStart + MinDur), deltaStop)
      case _ => 0L
    }

    if (dStartC != 0L || dStopC != 0L) {

      val (dStartCC, dStopCC) = (dStartC, dStopC)

      span match {
        case SpanLikeObj.Var(s) =>
          val oldSpan = s()
          val newSpan = {
            oldSpan.value match {
              case Span.From (start)  => Span.From (start + dStartCC)
              case Span.Until(stop )  => Span.Until(stop  + dStopCC )
              case Span(start, stop)  =>
                val newStart = start + dStartCC
                Span(newStart, math.max(newStart + MinDur, stop + dStopCC))
              case other => other
            }
          }
          s() = newSpan

        case _ =>
      }

      if (dStartCC != 0L) obj match {
        case objT: Proc[S] =>
          for {
            audioCue <- getAudioRegion[S](objT)
          } {
            audioCue match {
              case AudioCue.Obj.Shift(peer, amt) =>
                import expr.Ops._
                amt match {
                  case LongObj.Var(amtVr) =>
                    amtVr() = amtVr() + dStartCC
                  case _ =>
                    AudioCue.Obj.Shift(peer, LongObj.newVar[S](amt + dStartCC))
                }
              case other =>
                AudioCue.Obj.Shift(other, LongObj.newVar[S](dStartCC))
            }
          }
        case _ =>
      }
    }
  }

  /** Changes or removes the name of a process.
    *
    * @param obj the proc to rename
    * @param name the new name or `None` to remove the name attribute
    */
  def rename[S <: Sys[S]](obj: Obj[S], name: Option[String])(implicit tx: S#Tx): Unit = {
    val attr  = obj.attr
    name match {
      case Some(n) =>
        attr.$[StringObj](ObjKeys.attrName) match {
          case Some(StringObj.Var(vr)) => vr() = n
          case _                  => attr.put(ObjKeys.attrName, StringObj.newVar(n))
        }

      case _ => attr.remove(ObjKeys.attrName)
    }
  }

  /** Makes a copy of a proc. Copies the graph and all attributes.
    * Currently has no way to determine where the outputs of a `Proc` leading to,
    * so they are not rewired.
    *
    * @param obj the process to copy
    */
  def copy[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): Obj[S] = {
    val context = Copy[S, S]
    context.putHint(obj, Proc.hintFilterLinks, (_: Proc[S]) => false) // do not duplicate linked objects
    val res = context(obj)
    context.finish()
    res
  }

  def setGain[S <: Sys[S]](proc: Proc[S], gain: Double)(implicit tx: S#Tx): Unit = {
    val attr  = proc.attr

    if (gain == 1.0) {
      attr.remove(ObjKeys.attrGain)
    } else {
      attr.$[DoubleObj](ObjKeys.attrGain) match {
        case Some(DoubleObj.Var(vr)) => vr() = gain
        case _                  => attr.put(ObjKeys.attrGain, DoubleObj.newVar(gain))
      }
    }
  }

  def adjustGain[S <: Sys[S]](obj: Obj[S], factor: Double)(implicit tx: S#Tx): Unit = {
    if (factor == 1.0) return

    val attr  = obj.attr

    attr.$[DoubleObj](ObjKeys.attrGain) match {
      case Some(DoubleObj.Var(vr)) =>
        import expr.Ops._
        vr() = vr() * factor
      case other =>
        val newGain = other.fold(1.0)(_.value) * factor
        attr.put(ObjKeys.attrGain, DoubleObj.newVar(newGain))
    }
  }

  def setBus[S <: Sys[S]](objects: Iterable[Obj[S]], intExpr: IntObj[S])(implicit tx: S#Tx): Unit = {
    objects.foreach { proc =>
      proc.attr.put(ObjKeys.attrBus, intExpr)
    }
  }

  def toggleMute[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): Unit = {
    val attr = obj.attr
    attr.$[BooleanObj](ObjKeys.attrMute) match {
      // XXX TODO: BooleanObj should have `not` operator
      case Some(BooleanObj.Var(vr)) => 
        val old   = vr()
        val vOld  = old.value
        vr()      = !vOld
      case _ => attr.put(ObjKeys.attrMute, BooleanObj.newVar(true))
    }
  }

  def setSynthGraph[S <: Sys[S]](procs: Iterable[Proc[S]], codeElem: Code.Obj[S])
                                (implicit tx: S#Tx, compiler: Code.Compiler): Boolean = {
    val code = codeElem.value
    code match {
      case csg: Code.SynthGraph =>
        try {
          val sg = csg.execute {}  // XXX TODO: compilation blocks, not good!

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

          ???! // SCAN
//          val attrNameOpt = codeElem.attr.get(ObjKeys.attrName)
//          procs.foreach { p =>
//            p.graph() = SynthGraphObj.newConst[S](sg)  // XXX TODO: ideally would link to code updates
//            attrNameOpt.foreach(attrName => p.attr.put(ObjKeys.attrName, attrName))
//
//            def check(scans: Scans.Modifiable[S], keys: Set[String]): Unit = {
//              val toRemove = scans.iterator.collect {
//                case (key, scan) if !keys.contains(key) && scan.isEmpty => key
//              }
//              toRemove.foreach(scans.remove) // unconnected scans which are not referred to from synth def
//              val existing = scans.iterator.collect {
//                  case (key, _) if keys contains key => key
//                }
//              val toAdd = keys -- existing.toSet
//              toAdd.foreach(scans.add)
//            }
//
//            val proc = p
//            check(proc.inputs , scanInKeys )
//            check(proc.outputs, scanOutKeys)
//          }
          true

        } catch {
          case NonFatal(e) =>
            e.printStackTrace()
            false
        }

      case _ => false
    }
  }

  def mkAudioRegion[S <: Sys[S]](time      : Span,
                                 audioCue  : AudioCue.Obj[S],
                                 gOffset   : Long)
     (implicit tx: S#Tx): (SpanLikeObj /* SpanObj */[S], Proc[S]) = {

    // val srRatio = grapheme.spec.sampleRate / Timeline.SampleRate
    val spanV   = time // Span(time, time + (selection.length / srRatio).toLong)
    val span    = SpanLikeObj /* SpanObj */.newVar[S](spanV)
    val p       = Proc[S]

    // val scanIn  = proc.inputs .add(Proc.graphAudio )
    /*val sOut=*/ p.outputs.add(Proc.mainOut)
    // val grIn    = Grapheme[S] //(audioCue.value.spec.numChannels)

    // we preserve data.source(), i.e. the original audio file offset
    // ; therefore the grapheme element must start `selection.start` frames
    // before the insertion position `drop.frame`

    // require(gOffset == 0L, s"mkAudioRegion - don't know yet how to handle cue offset time")
    import proc.Ops._
    // This is tricky. Ideally we would keep `audioCue.offset`,
    // but then editing heuristics become a nightmare. Therefore,
    // now just flatten the thing...

//    val gOffset1    = (gOffset: LongObj[S]) + audioCue.offset
//    val audioCueOff = audioCue.replaceOffset(LongObj.newVar(gOffset1))
//    val gOffset1    = gOffset + audioCue.value.offset
//    val audioCueOff = audioCue.replaceOffset(LongObj.newVar(gOffset1))

    // XXX TODO -- cheese louise, the heuristics will get nasty
    val audioCueOff = if (gOffset == 0L) audioCue else {
      audioCue match {
        case AudioCue.Obj.Shift(peer, amt) =>
          AudioCue.Obj.Shift(peer, LongObj.newVar[S](amt + gOffset))
        case other =>
          AudioCue.Obj.Shift(other, LongObj.newVar[S](gOffset))
      }
    }

    // val gStart  = LongObj.newVar(time - selection.start)
    // require(time.start == 0, s"mkAudioRegion - don't know yet how to handle relative grapheme time")
    // val gStart = LongObj.newVar[S](-gOffset)
    // val bi: Grapheme.TimedElem[S] = (gStart, grapheme) // BiExpr(gStart, grapheme)
    // grIn.add(gStart, audioCue)
    // scanIn add grIn
    p.attr.put(Proc.graphAudio, audioCueOff /* grIn */)
    p.graph() = SynthGraphObj.tape
    (span, p)
  }

  /** Inserts a new audio region proc into a given group.
    *
    * @param group      the group to insert the proc into
    * @param time       the time span on the outer timeline
    * @param audioCue   the grapheme carrying the underlying audio file
    * @param gOffset    the selection start with respect to the grapheme.
    *                   This is inside the underlying audio file (but using timeline sample-rate),
    *                   whereas the proc will be placed in the group aligned with `time`.
    * @return           a tuple consisting of the span expression and the newly created proc.
    */
  def insertAudioRegion[S <: Sys[S]](group     : TimelineMod[S],
                                     time      : Span,
                                     audioCue  : AudioCue.Obj[S],
                                     gOffset   : Long)
                                    (implicit tx: S#Tx): (SpanLikeObj /* SpanObj */[S], Proc[S]) = {
    val res @ (span, obj) = mkAudioRegion(time, audioCue, gOffset)
    group.add(span, obj)
    res
  }

  def insertGlobalRegion[S <: Sys[S]](
      group     : TimelineMod[S],
      name      : String,
      bus       : Option[IntObj[S]]) // stm.Source[S#Tx, Element.Int[S]]])
     (implicit tx: S#Tx): Proc[S] = {

    val proc    = Proc[S]
    val obj     = proc // Obj(Proc.Elem(proc))
    val attr    = obj.attr
    val nameEx  = StringObj.newVar[S](StringObj.newConst(name))
    attr.put(ObjKeys.attrName, nameEx)

    group.add(Span.All, obj) // constant span expression
    obj
  }

  // SCAN
//  private def addLink[S <: Sys[S]](sourceKey: String, source: Scan[S], sinkKey: String, sink: Scan[S])
//                                  (implicit tx: S#Tx): Unit = {
//    log(s"Link $sourceKey / $source to $sinkKey / $sink")
//    source.add(Scan.Link.Scan(sink))
//  }
//
//  def removeLink[S <: Sys[S]](sourceKey: String, source: Scan[S], sinkKey: String, sink: Scan[S])
//                             (implicit tx: S#Tx): Unit = {
//    log(s"Unlink $sourceKey / $source from $sinkKey / $sink")
//    source.remove(Scan.Link.Scan(sink))
//  }

  def linkOrUnlink[S <: Sys[S]](out: Proc[S], in: Proc[S])(implicit tx: S#Tx): Boolean = {
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
//    if (existIt.hasNext) {
//      val (srcKey, srcScan, sinkKey, sinkScan) = existIt.next()
//      removeLink(srcKey, srcScan, sinkKey, sinkScan)
//      true
//
//    } else {
//      // XXX TODO cheesy way to distinguish ins and outs now :-E ... filter by name
//      val outsSeq = out.outputs.iterator.filter(_._1.startsWith("out")).toIndexedSeq
//      val insSeq  = insSeq0                       .filter(_._1.startsWith("in"))
//
//      if (outsSeq.isEmpty || insSeq.isEmpty) return false   // nothing to patch
//
//      if (outsSeq.size == 1 && insSeq.size == 1) {    // exactly one possible connection, go ahead
//        val (srcKey , src ) = outsSeq.head
//        val (sinkKey, sink) = insSeq .head
//        addLink(srcKey, src, sinkKey, sink)
//        true
//
//      } else {  // present dialog to user
//        log(s"Possible outs: ${outsSeq.map(_._1).mkString(", ")}; possible ins: ${insSeq.map(_._1).mkString(", ")}")
//        println(s"Woop. Multiple choice... Dialog not yet implemented...")
//        false
//      }
//    }
  }

  private final case class ArgAssign(name: Option[String], shape: UGenSpec.SignalShape, value: Any)

  private final class GraphLine(val elemName: String, val constructor: String, val args: Vec[ArgAssign]) {
    var uses    = Set.empty[String]
    var valName = Option.empty[String]
  }

  def extractSource(g: SynthGraph): String = {
    val ugenMap = UGenSpec.standardUGens

    var lines   = Vec.empty[GraphLine]
    val lazyMap = new ju.IdentityHashMap[Lazy, GraphLine]

    def mkLine(elem: Product): GraphLine = {
      val elemName  = elem.productPrefix
      val argVals   = elem.productIterator.toIndexedSeq

      val line = ugenMap.get(elemName).fold[GraphLine] {
        val ins = argVals.map(ArgAssign(None, UGenSpec.SignalShape.Generic, _))

        new GraphLine(elemName = elemName, constructor = "apply", args = ins)
      } { spec =>
        val (rate: Rate, rateMethod: UGenSpec.RateMethod, argVals1: Vec[Any]) = spec.rates match {
          case UGenSpec.Rates.Implied(r, m) => (r, m, argVals)
          case UGenSpec.Rates.Set(_) =>
            argVals.head match {
              case r: Rate => (r, UGenSpec.RateMethod.Default, argVals.tail)
            }
        }
        val rateMethodName = rateMethod match {
          case UGenSpec.RateMethod.Alias (name) => name
          case UGenSpec.RateMethod.Custom(name) => name
          case UGenSpec.RateMethod.Default      => rate.methodName
        }
        val ins = (spec.args zip argVals1).map { case (arg, argVal) =>
          val shape = arg.tpe match {
            case UGenSpec.ArgumentType.GE(sh, _) => sh
            case _ => UGenSpec.SignalShape.Generic
          }
          ArgAssign(Some(arg.name), shape, argVal)
        }
        new GraphLine(elemName = elemName, constructor = rateMethodName, args = ins)
      }
      line
    }

    g.sources.zipWithIndex.foreach { case (elem, elemIdx) =>
      val line      = mkLine(elem)
      lines       :+= line
      lazyMap.put(elem, line)
      val elemName  = elem.productPrefix

      line.args.foreach {
        case ArgAssign(argNameOpt, _, argVal: Lazy) =>
          val ref = lazyMap.get(argVal)
          if (ref == null) {
            val argValS = argVal.productPrefix
            val argS = argNameOpt.fold(argValS)(n => s"$n = $argValS")
            Console.err.println(s"Missing argument reference for $argS in $elemName in line $elemIdx")
          } else {
            val argName = argNameOpt.getOrElse("unnamed")
            ref.uses   += argName
          }

        case ArgAssign(argNameOpt, _, argVal: Product) if argVal.productPrefix == "GESeq" => // XXX TODO -- quite hackish
          val elems = argVal.productIterator.next().asInstanceOf[Vec[GE]]
          elems.foreach {
            case elem: Lazy =>
              val ref = lazyMap.get(elem)
              if (ref == null) {
                val argValS = elem.productPrefix
                val argS = argNameOpt.fold(argValS)(n => s"$n = $argValS")
                Console.err.println(s"Missing argument reference for $argS in $elemName seq in line $elemIdx")
              } else {
                ref.uses   += "unnamed"
              }

            case _ =>
          }

        case _ =>
      }
    }

    def uncapitalize(in: String): String = if (in.isEmpty) in else
      in.updated(0, Character.toLowerCase(in.charAt(0)))

    // assign preliminary val-names
    lines.foreach { line =>
      val uses = line.uses
      if (uses.nonEmpty) (uses - "unnamed").toList match {
        case single :: Nil if single != "unnamed" => line.valName = Some(single)
        case multiple =>
          val nameUp0 = if (line.elemName == "BinaryOpUGen") {
            val x = line.args.head.value.getClass.getName
            x.substring(0, x.length - 1)
          } else line.elemName

          val di      = nameUp0.lastIndexOf('$')
          val nameUp  = nameUp0.substring(di + 1)
          val nameLo  = uncapitalize(nameUp)
          line.valName = Some(nameLo)
      }
    }
    // make sure val-names are unique
    lines.zipWithIndex.foreach { case (line, li) =>
      line.valName.foreach { name0 =>
        val same = lines.filter(_.valName == line.valName)
        // cf. https://issues.scala-lang.org/browse/SI-9353
        val si9353 = lines.iterator.zipWithIndex.exists { case (line1, lj) =>
          lj < li && line1.args.exists(_.name == line.valName)
        }
        if (same.size > 1 || si9353) {
          same.zipWithIndex.foreach { case (line1, i) =>
            line1.valName = Some(s"${name0}_$i")
          }
        }
      }
    }
    // calc indentation
    val maxValNameSz0 = (0 /: lines)((res, line) => line.valName.fold(res)(n => math.max(n.length, res)))
    val maxValNameSz  = maxValNameSz0 | 1 // odd

    def mkLineSource(line: GraphLine): String = {
      val numArgs = line.args.size
      val args    = line.args.zipWithIndex.map { case (arg, ai) =>
        def mkString(x: Any): String = x match {
          case Constant(c) =>
            import UGenSpec.SignalShape._
            arg.shape match {
              case Int | Trigger | Gate | Switch if c == c.toInt => c.toInt.toString
              case DoneAction => synth.DoneAction(c.toInt).toString
              case _ => if (c.isPosInfinity) "inf" else if (c.isNegInfinity) "-inf" else c.toString
            }

          case l: Lazy =>
            val line1 = Option(lazyMap.get(l)).getOrElse(mkLine(l))
            line1.valName.getOrElse(mkLineSource(line1))

          case sq: Product if sq.productPrefix == "GESeq" =>
            val peer = sq.productIterator.next().asInstanceOf[Vec[GE]]
            peer.map(mkString).mkString("Seq[GE](", ", ", ")")

          case s: String =>
            // cf. https://stackoverflow.com/questions/9913971/scala-how-can-i-get-an-escaped-representation-of-a-string
            import scala.reflect.runtime.{universe => ru}
            val escaped = ru.Literal(ru.Constant(s)).toString()
            escaped

          case other =>
            other.toString
        }
        val valString = mkString(arg.value)
        if (numArgs == 1) valString else arg.name.fold(valString) { argName =>
          if (ai == 0 && argName == "in") valString else s"$argName = $valString"
        }
      }
      val invoke = if (line.elemName == "BinaryOpUGen") {
        line.args.head.value match {
          case op: BinaryOpUGen.Op =>
            val opS = uncapitalize(op.name)
            val Seq(_, a0, b) = args
            // XXX TODO --- stupid workaround for ScalaCollider #52
            val a = if ((opS == "min" || opS == "max") && line.args(1).value.isInstanceOf[Constant])
              s"Constant($a0)"
            else a0
            s"$a $opS $b"
        }
      } else if (line.elemName == "UnaryOpUGen") {
        line.args.head.value match {
          case op: UnaryOpUGen.Op =>
            val opS = uncapitalize(op.name)
            val Seq(_, a) = args
            s"$a.$opS"
        }
      } else {
        val cons      = if (line.constructor == "apply") "" else s".${line.constructor}"
        val elemName  = line.elemName.replace('$', '.')
        val select    = s"$elemName$cons"
        if (args.isEmpty && cons.nonEmpty) select else args.mkString(s"$select(", ", ", ")")
      }
      line.valName.fold(invoke) { valName =>
        val pad = " " * (maxValNameSz - valName.length)
        s"val $valName$pad = $invoke"
      }
    }

    // turn to source
    val linesS = lines.map(mkLineSource)
    linesS.mkString("\n")
  }
}