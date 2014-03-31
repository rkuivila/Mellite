package de.sciss.lucre

import de.sciss.lucre.confluent.Sys

object Escape {
  def durableTx[S <: Sys[S]](system: S)(implicit tx: S#Tx): S#D#Tx = system.durableTx(tx)
}
