/*
 *  ParamSpecObjView.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2017 Hanns Holger Rutz. All rights reserved.
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

import java.text.NumberFormat
import java.util.Locale
import javax.swing.{DefaultBoundedRangeModel, Icon, SpinnerModel, SpinnerNumberModel}

import de.sciss.audiowidgets.RotaryKnob
import de.sciss.desktop
import de.sciss.numbers
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.{OptionPane, UndoManager}
import de.sciss.icons.raphael
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Cursor, Disposable, Obj}
import de.sciss.lucre.swing.edit.EditVar
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{CellView, View, Window, deferTx, requireEDT}
import de.sciss.lucre.synth.Sys
import de.sciss.model.impl.ModelImpl
import de.sciss.nuages.{CosineWarp, DbFaderWarp, ExponentialWarp, FaderWarp, IntWarp, LinearWarp, ParamSpec, ParametricWarp, SineWarp, Warp}
import de.sciss.processor.Processor.Aborted
import de.sciss.swingplus.{ComboBox, GroupPanel, Spinner}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.Workspace

import scala.concurrent.{Future, Promise}
import scala.concurrent.stm.Ref
import scala.swing.Swing.EmptyIcon
import scala.swing.event.{SelectionChanged, ValueChanged}
import scala.swing.{Action, Alignment, BorderPanel, BoxPanel, Component, Dialog, FlowPanel, Label, Orientation, Swing, TextField}

object ParamSpecObjView extends ListObjView.Factory {
  type E[~ <: stm.Sys[~]] = ParamSpec.Obj[~]
  val icon: Icon        = ObjViewImpl.raphaelIcon(raphael.Shapes.Thermometer)
  val prefix            = "ParamSpec"
  def humanName: String = "Param Spec"
  def tpe               = ParamSpec.Obj
  def category: String  = ObjView.categOrganisation
  def hasMakeDialog     = true

  def mkListView[S <: Sys[S]](obj: ParamSpec.Obj[S])(implicit tx: S#Tx): ParamSpecObjView[S] with ListObjView[S] = {
    val value     = obj.value
    val editable  = ParamSpec.Obj.Var.unapply(obj).isDefined
    new Impl(tx.newHandle(obj), value, isEditable = editable).init(obj)
  }

  final case class Config[S <: stm.Sys[S]](name: String, init: ParamSpec)

  private final class PanelImpl(nameIn: Option[String], editable: Boolean) extends ModelImpl[Unit] {

    private[this] val ggNameOpt = nameIn.map { txt0 => new TextField(txt0, 10) }
    private[this] val ggName    = ggNameOpt.getOrElse(Swing.HStrut(4))

    private[this] val mLo       = new SpinnerNumberModel(0.0, -1.0e6, 1.0e6, 0.1)
    private[this] val mHi       = new SpinnerNumberModel(1.0, -1.0e6, 1.0e6, 0.1)
    private[this] val mCurve    = new SpinnerNumberModel(0.0, -1.0e6, 1.0e6, 0.1)
    private[this] val sqWarp    = Seq[Warp](
      LinearWarp, ExponentialWarp, ParametricWarp(0),
      CosineWarp, SineWarp, FaderWarp, DbFaderWarp, IntWarp
    )
    private[this] val nWarp     = sqWarp.map { w =>
      val n = w.getClass.getSimpleName //.toLowerCase
      val i = n.indexOf("Warp")
      if (i < 0) n else n.substring(0, i)
    }
    private[this] val mWarp     = ComboBox.Model.wrap(nWarp)
    private[this] val ggUnit    = new TextField(4)

    def nameOption: Option[String] = ggNameOpt.flatMap { gg =>
      val s0 = gg.text
      if (s0.isEmpty) None else Some(s0)
    }

    def name: String = nameOption.getOrElse("")

    def name_=(value: String): Unit =
      ggNameOpt.foreach(_.text = value)

    def spec: ParamSpec = {
      val warpI = mWarp.selectedItem.fold(-1)(nWarp.indexOf)
      val warp0 = if (warpI < 0) LinearWarp else sqWarp(warpI)
      val warp  = warp0 match {
        case p @ ParametricWarp(_) => p.copy(curvature = mCurve.getNumber.doubleValue())
        case other => other
      }
      ParamSpec(
        lo = mLo.getNumber.doubleValue(), hi = mHi.getNumber.doubleValue(),
        warp = warp, unit = ggUnit.text
      )
    }

    def spec_=(value: ParamSpec): Unit = {
      mLo.setValue(value.lo)
      mHi.setValue(value.hi)
      val warpNorm = value.warp match {
        case p @ ParametricWarp(c) =>
          mCurve.setValue(c)
          p.copy(curvature = 0.0)
        case other => other
      }
      val warpI = sqWarp.indexOf(warpNorm)
      mWarp.selectedItem = if (warpI < 0) None else Some(nWarp(warpI))
      ggUnit.text = value.unit

      updateExampleAndCurve()
    }

    private[this] val ggRotIn   = new RotaryKnob(new DefaultBoundedRangeModel(500, 0, 0, 1000))
    private[this] val ggRotOut  = new RotaryKnob(new DefaultBoundedRangeModel(500, 0, 0, 1000))
    private[this] var rotIsIn   = true
    private[this] var rotIsSet  = false
    private[this] val ggIn      = new TextField(8)
    private[this] val ggOut     = new TextField(8)
    ggOut.editable  = false
    ggIn .editable  = false

    private[this] val fmtDec  = NumberFormat.getNumberInstance(Locale.US)
    fmtDec.setMaximumFractionDigits(5)
    private[this] val fmtInt  = NumberFormat.getIntegerInstance(Locale.US)

    private def updateExample(fire: Boolean = true): ParamSpec = {
      rotIsSet = true
      try {
        if (rotIsIn) updateExampleFromIn (fire = fire)
        else         updateExampleFromOut(fire = fire)
      } finally {
        rotIsSet = false
      }
    }

    private def updateExampleFromIn(fire: Boolean): ParamSpec = {
      import numbers.Implicits._
      val spc     = spec
      val in      = ggRotIn.value * 0.001
      val out     = spc.map(in)
      ggRotOut.value = out.linlin(spc.lo, spc.hi, 0, 1000).toInt
      val inS     = fmtDec.format(in)
      val fmt     = if (spc.warp == IntWarp) fmtInt else fmtDec
      val outS    = fmt.format(out)
      ggIn  .text = inS
      ggOut .text = outS
      if (fire) dispatch(())
      spc
    }

    private def updateExampleFromOut(fire: Boolean): ParamSpec = {
      import numbers.Implicits._
      val spc     = spec
      val out     = ggRotOut.value.linlin(0, 1000, spc.lo, spc.hi)
      val in      = spc.inverseMap(out)
      ggRotIn.value = (in * 1000).toInt
      val inS     = fmtDec.format(in)
      val fmt     = if (spc.warp == IntWarp) fmtInt else fmtDec
      val outS    = fmt.format(out)
      ggIn  .text = inS
      ggOut .text = outS
      if (fire) dispatch(())
      spc
    }

    ggRotIn.listenTo(ggRotIn)
    ggRotIn.reactions += {
      case ValueChanged(_) if !rotIsSet =>
        rotIsIn = true
        updateExample()
    }

    ggRotOut.listenTo(ggRotOut)
    ggRotOut.reactions += {
      case ValueChanged(_) if !rotIsSet =>
        rotIsIn = false
        updateExample()
    }

    private def mkSpinner(m: SpinnerModel): Spinner = {
      val res = new Spinner(m)
      res.listenTo(res)
      res.reactions += {
        case ValueChanged(_) => updateExample()
      }
      res
    }

    val ggLo: Component = mkSpinner(mLo)
    val ggHi: Component = mkSpinner(mHi)

    private[this] val ggCurve   = mkSpinner(mCurve)
    private[this] val ggWarp    = new ComboBox(mWarp) {
      listenTo(selection)
      reactions += {
        case SelectionChanged(_) => updateExampleAndCurve()
      }
    }

    private[this] val lbName  = new Label(if (nameIn.isEmpty) "" else "Name:", EmptyIcon, Alignment.Right)
    private[this] val lbLo    = new Label( "Low Value:", EmptyIcon, Alignment.Right)
    private[this] val lbHi    = new Label("High Value:", EmptyIcon, Alignment.Right)
    private[this] val lbWarp  = new Label(      "Warp:", EmptyIcon, Alignment.Right)
    private[this] val lbUnit  = new Label(      "Unit:", EmptyIcon, Alignment.Right)
    private[this] val lbCurve = new Label( "Curvature:", EmptyIcon, Alignment.Right)

    private def updateExampleAndCurve(fire: Boolean = true): Unit = {
      val spc = updateExample(fire = false)
      ggCurve.enabled = editable && spc.warp.isInstanceOf[ParametricWarp]
      if (fire) dispatch(())
    }

    updateExampleAndCurve(fire = false)

    private[this] val boxDemo = new FlowPanel(ggRotIn, new Label("In:"), ggIn, new Label("Out:"), ggOut, ggRotOut)

    private[this] val boxParams = new GroupPanel {
      horizontal= Seq(
        Par(Trailing)(lbName, lbLo  , lbHi   ), Par(ggName , ggLo  , ggHi   ),
        Gap.Preferred(GroupPanel.Placement.Unrelated),
        Par(Trailing)(lbUnit, lbWarp, lbCurve), Par(ggUnit , ggWarp, ggCurve))
      vertical = Seq(
        Par(Baseline)(lbName, ggName, lbUnit , ggUnit ),
        Par(Baseline)(lbLo  , ggLo  , lbWarp , ggWarp ),
        Par(Baseline)(lbHi  , ggHi  , lbCurve, ggCurve))
    }

    private[this] val box = new BoxPanel(Orientation.Vertical)
    box.contents += boxParams
    box.contents += Swing.VStrut(8)
    box.contents += boxDemo

    if (!editable) {
      ggLo    .enabled = false
      ggHi    .enabled = false
      ggCurve .enabled = false
    }

    def component: Component = box
  }

  def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                                 (ok: Config[S] => Unit)
                                 (implicit cursor: stm.Cursor[S]): Unit = {
    val panel = new PanelImpl(nameIn = Some(prefix), editable = true)

    val pane = desktop.OptionPane.confirmation(panel.component, optionType = Dialog.Options.OkCancel,
      messageType = Dialog.Message.Question, focus = Some(panel.ggHi))
    pane.title  = s"New $humanName"
    val res = pane.show(window)

    if (res == Dialog.Result.Ok) {
      val res = Config[S](name = panel.name, init = panel.spec)
      ok(res)
    }
  }

  def makeObj[S <: Sys[S]](config: Config[S])(implicit tx: S#Tx): List[Obj[S]] = {
    val name  = config.name
    val value = config.init
    val obj = ParamSpec.Obj.newVar(ParamSpec.Obj.newConst[S](value))
    if (!name.isEmpty) obj.name = name
    obj :: Nil
  }

  private final class ViewImpl[S <: Sys[S]](objH: stm.Source[S#Tx, ParamSpec.Obj[S]], val editable: Boolean)
                                           (implicit val workspace: Workspace[S], val cursor: stm.Cursor[S],
                                            val undoManager: UndoManager)
    extends ViewHasWorkspace[S] with View.Editable[S] with ComponentHolder[Component] {

    private[this] var specValue   : ParamSpec         = _
    private[this] var panel       : PanelImpl         = _
    private[this] val _dirty      : Ref[Boolean]      = Ref(false)
    private[this] var actionApply : Action            = _
    private[this] var observer    : Disposable[S#Tx]  = _

    def init(spec0: ParamSpec.Obj[S])(implicit tx: S#Tx): this.type = {
      val spec0V = spec0.value
      deferTx(guiInit(spec0V))
      observer = spec0.changed.react { implicit tx => upd =>
        deferTx {
          specValue   = upd.now
          panel.spec  = upd.now
        }
      }
      this
    }

    private def guiInit(spec0: ParamSpec): Unit = {
      this.specValue = spec0
      panel = new PanelImpl(nameIn = None, editable = editable)
      panel.spec = spec0

      panel.addListener {
        case _ => updateDirty()
      }

      actionApply = Action("Apply") {
        save()
        updateDirty()
      }

      actionApply.enabled = false
      val ggApply = GUI.toolButton(actionApply, raphael.Shapes.Check, tooltip = "Save changes")
//      val panelBot = new BoxPanel(Orientation.Horizontal) {
//        contents += Swing.HGlue
//        contents += ggApply
//      }
      val panelBot = new FlowPanel(FlowPanel.Alignment.Trailing)(ggApply)

      component = new BorderPanel {
        add(panel.component, BorderPanel.Position.Center)
        add(panelBot       , BorderPanel.Position.South )
      }
    }

    def dirty(implicit tx: S#Tx): Boolean = _dirty.get(tx.peer)

    private def updateDirty(): Unit = {
      val specNowV = panel.spec
      val isDirty   = specValue != specNowV
      val wasDirty  = _dirty.single.swap(isDirty)
      if (isDirty != wasDirty) actionApply.enabled = isDirty
    }

    def save(): Unit = {
      requireEDT()
      val newSpec = panel.spec
      val editOpt = cursor.step { implicit tx =>
        objH() match {
          case ParamSpec.Obj.Var(pVr) =>
            val pVal  = ParamSpec.Obj.newConst[S](newSpec)
            implicit val tpe = ParamSpec.Obj
            val edit  = EditVar.Expr[S, ParamSpec, ParamSpec.Obj](s"Edit $humanName", pVr, pVal)
            Some(edit)
          case _ => None
        }
      }
      editOpt.foreach { edit =>
        undoManager.add(edit)
//        undoManager.clear()
        specValue = newSpec
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = observer.dispose()
  }

  private final class FrameImpl[S <: Sys[S]](val view: ViewImpl[S],
                                             name: CellView[S#Tx, String])
    extends WindowImpl[S](name) with Veto[S#Tx] {

//    resizable = false

    override def prepareDisposal()(implicit tx: S#Tx): Option[Veto[S#Tx]] =
      if (!view.editable && !view.dirty) None else Some(this)

    private def _vetoMessage = "The spec has been edited."

    def vetoMessage(implicit tx: S#Tx): String = _vetoMessage

    def tryResolveVeto()(implicit tx: S#Tx): Future[Unit] = {
      val p = Promise[Unit]()
      deferTx {
        val message = s"${_vetoMessage}\nDo you want to save the changes?"
        val opt = OptionPane.confirmation(message = message, optionType = OptionPane.Options.YesNoCancel,
          messageType = OptionPane.Message.Warning)
        opt.title = s"Close - $title"
        opt.show(Some(window)) match {
          case OptionPane.Result.No =>
            p.success(())

          case OptionPane.Result.Yes =>
            /* val fut = */ view.save()
            p.success(())

          case OptionPane.Result.Cancel | OptionPane.Result.Closed =>
            p.failure(Aborted())
        }
      }
      p.future
    }
  }

  private final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, ParamSpec.Obj[S]],
                                        var value: ParamSpec, val isEditable: Boolean)
    extends ParamSpecObjView[S]
      with ListObjView[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.SimpleExpr[S, ParamSpec, ParamSpec.Obj]
      with ListObjViewImpl.StringRenderer { listObjView =>

    override def obj(implicit tx: S#Tx): ParamSpec.Obj[S] = objH()

    type E[~ <: stm.Sys[~]] = ParamSpec.Obj[~]

    def factory = ParamSpecObjView

    val exprType = ParamSpec.Obj

    def expr(implicit tx: S#Tx): ParamSpec.Obj[S] = obj

    def isViewable: Boolean = true

    def convertEditValue(v: Any): Option[ParamSpec] = None

    override def openView(parent: Option[Window[S]])(implicit tx: S#Tx, workspace: Workspace[S],
                                                     cursor: Cursor[S]): Option[Window[S]] = {
      implicit val undo: UndoManager = new UndoManagerImpl  // XXX TODO --- actually not used
      val _obj  = obj
      val view  = new ViewImpl[S](objH, editable = isEditable).init(_obj)
      val nameView = AttrCellView.name(_obj)
      val fr    = new FrameImpl[S](view, nameView).init()
      Some(fr)
    }
  }
}
trait ParamSpecObjView[S <: stm.Sys[S]] extends ObjView[S] {
  override def obj(implicit tx: S#Tx): ParamSpec.Obj[S]
}