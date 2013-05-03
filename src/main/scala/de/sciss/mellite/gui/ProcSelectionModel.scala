package de.sciss.mellite
package gui

import de.sciss.mellite.gui.impl.TimelineProcView
import de.sciss.synth.proc.Sys

trait ProcSelectionModel[S <: Sys[S]] {
  def contains(view: TimelineProcView[S]): Boolean
  def +=(view: TimelineProcView[S]): Unit
  def -=(view: TimelineProcView[S]): Unit
  def clear(): Unit
}