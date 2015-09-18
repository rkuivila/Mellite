package de.sciss.mellite

import java.io.FileInputStream

import de.sciss.file._
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.{DoubleObj, IntObj, LongObj}
import de.sciss.lucre.stm.Sys
import de.sciss.span.Span
import de.sciss.synth.Curve
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{Code, CurveObj, FadeSpec, Folder, Grapheme, ObjKeys, Proc, Scan, Scans, SynthGraphObj, Timeline}
import play.api.libs.json.{JsArray, JsBoolean, JsNumber, JsObject, JsString, JsUndefined, Json}

import scala.collection.breakOut
import scala.util.{Failure, Success, Try}

/** Hackish import for .json files written with Mellite v0.3.x */
object ImportJSON {
  def apply[S <: Sys[S]](folder: Folder[S], jsonFile: File)(implicit tx: S#Tx): Timeline[S] = {
    val fi = new FileInputStream(jsonFile)
    val json = try {
      val arr = new Array[Byte](fi.available())
      fi.read(arr)
      Json.parse(arr)
    } finally {
      fi.close()
    }

    val JsArray(locsJSON    ) = json \ "locations"
    val JsArray(audioJSON   ) = json \ "audio"
    val JsArray(regionsJSON ) = json \ "regions"

    val sampleRateIn  = 44100.0  // XXX TODO --- hardcoded in Mellite v0.3.x
    val srFactor      = Timeline.SampleRate / sampleRateIn

    val locsIDs: Map[Int, ArtifactLocation[S]] = locsJSON.map { locJSON =>
      val JsNumber(idB  ) = locJSON \ "id"
      val id = idB.toInt
      val JsString(dirS ) = locJSON \ "directory"
      val dir   = file(dirS)
      val loc   = ArtifactLocation.newVar[S](dir)
      loc.name  = dir.name

      folder.addLast(loc)

      (id, loc)
    } (breakOut)

    val audioIDs: Map[Int, Grapheme.Expr.Audio[S]] = audioJSON.map { a =>
      val JsNumber(idB    ) = a \ "id"
      val id = idB.toInt
      val JsNumber(locRef ) = a \ "locRef"
      val loc = locsIDs(locRef.toInt)
      val JsString(child  ) = a \ "file"
      val offset = a \ "offset" match {
        case JsNumber(offsetB)  => offsetB.toLong
        case JsUndefined()      => 0L
      }
      val gain = a \ "gain" match {
        case JsNumber(gainB)  => gainB.toDouble
        case JsUndefined()    => 1.0
      }
      val artifact  = Artifact(loc, Artifact.Child(child))
      val f         = artifact.value
      val spec      = AudioFile.readSpec(f)
      val audio     = Grapheme.Expr.Audio[S](artifact, spec,
        offset = LongObj.newVar(offset), gain = DoubleObj.newVar(gain))
      audio.name    = f.base

      (id, audio)
    } (breakOut)

    if (audioIDs.nonEmpty) {
      val aFolder   = Folder[S]
      aFolder.name  = "audio-files"
      val elems = audioIDs.valuesIterator.toIndexedSeq.sortBy(_.value.artifact.name.toUpperCase)
      elems.foreach(aFolder.addLast)
      folder.addLast(aFolder)
    }

    val tl  = Timeline[S]
    tl.name = "timeline"

    val regionIDs: Map[Int, Proc[S]] = regionsJSON.map { r =>
      val proc  = Proc[S]
      val attr  = proc.attr

      val JsNumber(idB) = r \ "id"
      val id = idB.toInt

      r \ "name" match {
        case JsString(name) => proc.name = name
        case JsUndefined() =>
      }

      def frameIn(b: BigDecimal): Long = (b.toLong * srFactor + 0.5).toLong

      val span = (r \ "start", r \ "stop") match {
        case (JsNumber(startB), JsNumber(stopB)) => Span(frameIn(startB), frameIn(stopB))
        case (JsNumber(startB), JsUndefined()  ) => Span.From (frameIn(startB))
        case (JsUndefined()   , JsNumber(stopB)) => Span.Until(frameIn(stopB ))
        case (JsUndefined()   , JsUndefined()  ) => Span.All
      }

      r \ "bus" match {
        case JsNumber(busB) => ProcActions.setBus(proc :: Nil, IntObj.newVar[S](busB.toInt))
        case JsUndefined() =>
      }

      r \ "gain" match {
        case JsNumber(gainB) => ProcActions.setGain(proc, gainB.toDouble)
        case JsUndefined() =>
      }

      r \ "muted" match {
        case JsBoolean(b) => if (b) ProcActions.toggleMute(proc)
        case JsUndefined() =>
      }

      r \ "track" match {
        case JsNumber(trackB) => attr.put("track-index", IntObj.newVar[S](trackB.toInt * 2))  // greater spacing
        case JsUndefined() =>
      }

      def mkFade(keyIn: String, keyOut: String): Unit = r \ keyIn match {
        case fd @ JsObject(_) =>
          val JsNumber(lenB) = fd \ "length"
          val len   = frameIn(lenB)
          val curve = fd \ "curve" match {
            case JsNumber(cB)   => Curve.parametric(cB.toFloat)
            case JsUndefined()  => Curve.linear
          }
          // val fade = FadeSpec(numFrames = len, curve = curve)
          // .newVar[S](fade))
          attr.put(keyOut, FadeSpec.Obj[S](LongObj.newVar(len), CurveObj.newVar(curve), DoubleObj.newVar(0.0)))

        case JsUndefined() =>
      }

      mkFade("fadeIn" , ObjKeys.attrFadeIn )
      mkFade("fadeOut", ObjKeys.attrFadeOut)

      r \ "audio" match {
        case a @ JsObject(_) =>
          val gOffset = a \ "offset" match {
            case JsNumber(offB) => frameIn(offB)
            case JsUndefined()  => 0L
          }
          val JsNumber(idRefB) = a \ "idRef"
          val grapheme = audioIDs(idRefB.toInt)

          val scanIn  = proc.inputs .add(Proc.graphAudio )
          /*val sOut=*/ proc.outputs.add(Proc.scanMainOut)
          val grIn    = Grapheme[S](grapheme.value.spec.numChannels)
          val gStart = LongObj.newVar(-gOffset)
          grIn.add(gStart, grapheme)
          scanIn add grIn
          proc.graph() = SynthGraphObj.tape

        case JsUndefined() =>
          // XXX TODO --- try to read source code
          val graphFile = jsonFile.parent / "graphs" / s"${proc.name}.scala"
          if (graphFile.isFile) {
            val fin = new FileInputStream(graphFile)
            try {
              val arr   = new Array[Byte](fin.available())
              fin.read(arr)
              val text  = new String(arr, "UTF-8")
              val code  = Code.SynthGraph(text)
              attr.put(Proc.attrSource, Code.Obj.newVar[S](code))
              implicit val compiler = Mellite.compiler
              val graphT = Try(code.execute(()))
              graphT match {
                case Success(graph) => proc.graph() = graph
                case Failure(ex) =>
                  println(s"Failed to compile ${graphFile.name}")
              }

            } finally {
              fin.close()
            }
          }
      }

      tl.add(span, proc)

      (id, proc)

    } (breakOut)

    regionsJSON.foreach { r =>
      val JsNumber(idB) = r \ "id"
      val id    = idB.toInt
      val proc  = regionIDs(id)

      def mkLinks(field: String)(thisScans: Proc[S] => Scans.Modifiable[S])
                                (thatScans: Proc[S] => Scans.Modifiable[S]): Unit =
        r \ field match {
          case JsObject(pairs) =>
            pairs.foreach { tup =>
              val (thisKey, JsArray(targets)) = tup
              targets.foreach { targetJSON =>
                val JsNumber(thatIDB) = targetJSON \ "idRef"
                val that    = regionIDs(thatIDB.toInt)
                val JsString(thatKey) = targetJSON \ "key"
                val thisScan  = thisScans(proc).add(thisKey)
                val thatScan  = thatScans(that).add(thatKey)
                thisScan.add(Scan.Link.Scan(thatScan))
              }
            }
          case JsUndefined() =>
        }

      mkLinks("inputs" )(_.inputs )(_.outputs)

      // N.B. --- actually NOT! link addition is bi-directional,
      // so if we have established all the inputs, then all
      // corresponding outputs are set as well.

      // mkLinks("outputs")(_.outputs)(_.inputs )
    }

    folder.addLast(tl)
    tl
  }
}