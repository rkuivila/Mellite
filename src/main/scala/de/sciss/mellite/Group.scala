package de.sciss.mellite

import de.sciss.synth.proc.Sys

trait Group[ S <: Sys[ S ]] {
   def elements( implicit tx: S#Tx ) : Elements[ S ]
}