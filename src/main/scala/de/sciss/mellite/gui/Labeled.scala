package de.sciss.mellite.gui

final case class Labeled[A](value: A)(label: String) {
  override def toString = label
}