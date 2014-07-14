package de.sciss.mellite
package gui
package impl.timeline

import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Source
import de.sciss.mellite.gui.TimelineObjView.Factory
import de.sciss.span.SpanLike
import de.sciss.synth.proc.{StringElem, ObjKeys, Proc, Obj}
import de.sciss.lucre.expr.{String => StringEx, Expr}

object TimelineObjViewImpl {
  private val sync = new AnyRef

  def addFactory(f: Factory): Unit = sync.synchronized {
    val tid = f.typeID
    if (map.contains(tid)) throw new IllegalArgumentException(s"View factory for type $tid already installed")
    map += tid -> f
  }

  def factories: Iterable[Factory] = map.values

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): TimelineObjView[S] = {
    val tid = obj.elem.typeID
    // getOrElse(sys.error(s"No view for type $tid"))
    map.get(tid).fold(Generic(obj))(f => f(obj.asInstanceOf[Obj.T[S, f.E]]))
  }

  private var map = Map[Int, Factory](
    Proc.typeID -> ProcView
  )

  // -------- Generic --------

  object Generic {
    def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): TimelineObjView[S] = {
      val nameOption = obj.attr.get(ObjKeys.attrName).flatMap {
        case StringElem.Obj(sObj) => Some(sObj.elem.peer.value)
        case _ => None
      }
      new Generic.Impl(tx.newHandle(obj), nameOption)
    }

    private final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj[S]], var nameOption: Option[String])
      extends TimelineObjView[S] {

//      def prefix: String = "Generic"
//      def typeID: Int = 0
//
//      def value: Any = ()
//
//      def configureRenderer(label: Label): Component = label
//
//      def isUpdateVisible(update: Any)(implicit tx: S#Tx): _Boolean = false
//
//      def icon: Icon = Generic.icon
      def span: Source[S#Tx, Expr[S, SpanLike]] = ???

      var trackIndex: Int = _
      var spanValue: SpanLike = _
      var trackHeight: Int = _
    }
  }
}
