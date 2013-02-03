package de.sciss.mellite
package gui

import de.sciss.synth.proc.Sys
import swing.Component
import impl.{GroupViewImpl => Impl}

object GroupView {
  def apply[S <: Sys[S]](root: Elements[S])(implicit tx: S#Tx): GroupView[S] = Impl(root)
}
trait GroupView[S <: Sys[S]] {
  def component: Component
}