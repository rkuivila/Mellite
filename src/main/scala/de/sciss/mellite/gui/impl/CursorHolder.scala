package de.sciss.mellite.gui.impl

import de.sciss.lucre.stm.{Cursor, Sys}

trait CursorHolder[ S <: Sys[ S ]] {
   protected def cursor: Cursor[ S ]
   final protected def atomic[ A ]( fun: S#Tx => A ) : A = cursor.step( fun )
}
