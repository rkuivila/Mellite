package de.sciss.mellite.gui

import java.awt.Component
import java.awt.event.ActionListener
import javax.swing.{SpinnerNumberModel, ComboBoxEditor, JSpinner, AbstractSpinnerModel}

import scala.swing.{Swing, ComboBox}

class SpinnerComboBox[A](value0: A, minimum: A, maximum: A, step: A, items: Seq[A])(implicit num: Numeric[A])
  extends ComboBox[A](items) {

  private val sm: AbstractSpinnerModel =
    (value0.asInstanceOf[AnyRef], minimum.asInstanceOf[AnyRef], maximum.asInstanceOf[AnyRef], step.asInstanceOf[AnyRef]) match {
      case (n: Number, min: Comparable[_], max: Comparable[_], s: Number) =>
        new SpinnerNumberModel(n, min, max, s)
      case _ =>
        new AbstractSpinnerModel {
          private var _value = value0

          def getValue: AnyRef = _value.asInstanceOf[AnyRef]
          def setValue(value: Any): Unit = _value = value.asInstanceOf[A]

          def getNextValue    : AnyRef = clip(num.plus (_value, step)).asInstanceOf[AnyRef]
          def getPreviousValue: AnyRef = clip(num.minus(_value, step)).asInstanceOf[AnyRef]
        }
    }

  private def clip(in: A): A = num.max(minimum, num.min(maximum, in))

  private val sp = new JSpinner(sm)
  // sp.setBorder(null)

  private object editor extends ComboBoxEditor {
    def getEditorComponent: Component = sp

    def getItem: AnyRef = sp.getValue
    def setItem(value: Any): Unit = sp.setValue(value)

    def selectAll(): Unit = sp.getEditor match {
      case ed: JSpinner.DefaultEditor => ed.getTextField.selectAll()
      case _ =>
    }

    def addActionListener(l: ActionListener): Unit = sp.getEditor match {
      case ed: JSpinner.DefaultEditor => ed.getTextField.addActionListener(l)
      case _ =>
    }

    def removeActionListener(l: ActionListener): Unit = sp.getEditor match {
      case ed: JSpinner.DefaultEditor => ed.getTextField.removeActionListener(l)
      case _ =>
    }
  }

  peer.setEditor(editor)
  peer.setEditable(true)
  border = Swing.EmptyBorder(0, 0, 0, 4)

  def value: A          = sp.getValue.asInstanceOf[A]
  def value_=(value: A) = sp.setValue(clip(value))
}
