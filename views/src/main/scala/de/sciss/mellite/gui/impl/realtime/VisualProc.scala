/*
 *  VisualProc.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite.gui.impl.realtime

import de.sciss.synth.proc.{Proc, Obj}
import de.sciss.lucre.stm.Source
import de.sciss.lucre.synth.Sys

object VisualProc {
  val COLUMN_DATA = "nuages.data"
}
final class VisualProc[S <: Sys[S]](var name: String, var par: Map[String, Double],
                                    val staleCursor: S#Acc, val procH: Source[S#Tx, Obj.T[S, Proc.Elem]])
