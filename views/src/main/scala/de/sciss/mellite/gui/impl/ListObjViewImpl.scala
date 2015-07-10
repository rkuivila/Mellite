package de.sciss.mellite
package gui
package impl

import javax.swing.Icon
import javax.swing.undo.UndoableEdit

import de.sciss.icons.raphael
import de.sciss.lucre.expr.{Boolean => BooleanEx, Expr, ExprType}
import de.sciss.lucre.swing.edit.EditVar
import de.sciss.lucre.swing.{Window, deferTx}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.{event => evt, stm}
import de.sciss.model.Change
import de.sciss.synth.proc.{Confluent, Elem, Obj}
import org.scalautils.TypeCheckedTripleEquals

import scala.language.higherKinds
import scala.swing.{CheckBox, Component, Label}
import scala.util.Try

object ListObjViewImpl {
  private val sync = new AnyRef

  def addFactory(f: ListObjView.Factory): Unit = sync.synchronized {
    val tid = f.typeID
    if (map.contains(tid)) throw new IllegalArgumentException(s"View factory for type $tid already installed")
    map += tid -> f
  }

  def factories: Iterable[ListObjView.Factory] = map.values

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): ListObjView[S] = {
    val tid = obj.elem.typeID
    // getOrElse(sys.error(s"No view for type $tid"))
    map.get(tid).fold[ListObjView[S]](GenericObjView.mkListView(obj)) { f =>
      f.mkListView(obj.asInstanceOf[Obj.T[S, f.E]])
    }
  }

  private var map = scala.Predef.Map[Int, ListObjView.Factory](
    ObjViewImpl.String          .typeID -> ObjViewImpl.String,
    IntObjView                  .typeID -> IntObjView,
    ObjViewImpl.Long            .typeID -> ObjViewImpl.Long,
    ObjViewImpl.Double          .typeID -> ObjViewImpl.Double,
    ObjViewImpl.Boolean         .typeID -> ObjViewImpl.Boolean,
    ObjViewImpl.Color           .typeID -> ObjViewImpl.Color,
    AudioGraphemeObjView        .typeID -> AudioGraphemeObjView,
    ArtifactLocationObjView     .typeID -> ArtifactLocationObjView,
    ObjViewImpl.Artifact        .typeID -> ObjViewImpl.Artifact,
    ObjViewImpl.Recursion       .typeID -> ObjViewImpl.Recursion,
    ObjViewImpl.Folder          .typeID -> ObjViewImpl.Folder,
    ProcObjView                 .typeID -> ProcObjView,
    ObjViewImpl.Timeline        .typeID -> ObjViewImpl.Timeline,
    CodeObjView                 .typeID -> CodeObjView,
    ObjViewImpl.FadeSpec        .typeID -> ObjViewImpl.FadeSpec,
    ActionView                  .typeID -> ActionView,
    ObjViewImpl.Ensemble        .typeID -> ObjViewImpl.Ensemble,
    ObjViewImpl.Nuages          .typeID -> ObjViewImpl.Nuages
  )

  /** A trait that when mixed in provides `isEditable` and `tryEdit` as non-op methods. */
  trait NonEditable[S <: evt.Sys[S]] {
    def isEditable: Boolean = false

    def tryEdit(value: Any)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] = None
  }

  trait EmptyRenderer[S <: evt.Sys[S]] {
    def configureRenderer(label: Label): Component = label
    def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean = false
    def value: Any = ()
  }

  trait StringRenderer {
    def value: Any

    def configureRenderer(label: Label): Component = {
      label.text = value.toString
      label
    }
  }

  trait ExprLike[S <: evt.Sys[S], A] {
    _: ListObjView[S] =>

    protected var exprValue: A

    // def obj: stm.Source[S#Tx, Obj.T[S, Elem { type Peer = Expr[S, A] }]]

    /** Tests a value from a `Change` update. */
    protected def testValue       (v: Any): Option[A]
    protected def convertEditValue(v: Any): Option[A]

    protected def exprType: ExprType[A]

    protected def expr(implicit tx: S#Tx): Expr[S, A]

    def tryEdit(value: Any)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] =
      convertEditValue(value).flatMap { newValue =>
        expr match {
          case Expr.Var(vr) =>
            import TypeCheckedTripleEquals._
            vr() match {
              case Expr.Const(x) if x === newValue => None
              case _ =>
                // val imp = ExprImplicits[S]
                // import imp._
                // vr() = newValue
                implicit val ser    = exprType.serializer   [S]
                implicit val serVr  = exprType.varSerializer[S]
                val ed = EditVar.Expr(s"Change ${humanName} Value", vr, exprType.newConst[S](newValue))
                Some(ed)
            }

          case _ => None
        }
      }

    def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean = update match {
      case Change(_, now) =>
        testValue(now).exists { valueNew =>
          deferTx {
            exprValue = valueNew
          }
          true
        }
      case _ => false
    }

    // XXX TODO - this is a quick hack for demo
    def openView(parent: Option[Window[S]])
                (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
      workspace match {
        case cf: Workspace.Confluent =>
          // XXX TODO - all this casting is horrible
          implicit val ctx = tx.asInstanceOf[Confluent#Tx]
          implicit val ser = exprType.serializer[Confluent]
          val name = AttrCellView.name[Confluent](obj.asInstanceOf[Obj[Confluent]])
            .map(n => s"History for '$n'")
          val w = new WindowImpl[Confluent](name) {
            val view = ExprHistoryView(cf, expr.asInstanceOf[Expr[Confluent, A]])
            init()
          }
          Some(w.asInstanceOf[Window[S]])
        case _ => None
      }
    }
  }

  trait SimpleExpr[S <: Sys[S], A] extends ExprLike[S, A] with ListObjView[S] {
    // _: ObjView[S] =>

    override def value: A
    protected def value_=(x: A): Unit

    protected def exprValue: A = value
    protected def exprValue_=(x: A): Unit = value = x
  }

  private final val ggCheckBox = new CheckBox()

  trait BooleanExprLike[S <: Sys[S]] extends ExprLike[S, Boolean] {
    _: ListObjView[S] =>

    def exprType = BooleanEx

    def convertEditValue(v: Any): Option[Boolean] = v match {
      case num: Boolean  => Some(num)
      case s: String     => Try(s.toBoolean).toOption
    }

    def testValue(v: Any): Option[Boolean] = v match {
      case i: Boolean  => Some(i)
      case _            => None
    }

    def configureRenderer(label: Label): Component = {
      ggCheckBox.selected   = exprValue
      ggCheckBox.background = label.background
      ggCheckBox
    }
  }
}
