package de.sciss.mellite

import java.io.File
import de.sciss.lucre.expr.{BiGroup, LinkedList}
import de.sciss.synth.proc.Proc
import impl.{DocumentImpl => Impl}
import de.sciss.lucre.stm.{Cursor, Sys}

object Document {
   type Group[ S <: Sys[ S ]]          = BiGroup.Var[ S, Proc[ S ], Proc.Update[ S ]]
   type GroupUpdate[ S <: Sys[ S ]]    = BiGroup.Update[ S, Proc[ S ], Proc.Update[ S ]]
   type Groups[ S <: Sys[ S ]]         = LinkedList.Var[ S, Group[ S ], GroupUpdate[ S ]]
   type GroupsUpdate[ S <: Sys[ S ]]   = LinkedList.Update[ S, Group[ S ], GroupUpdate[ S ]]

   def read(  dir: File ) : Document[ Cf ] = Impl.read( dir )
   def empty( dir: File ) : Document[ Cf ] = Impl.empty( dir )
}
trait Document[ S <: Sys[ S ]] {
   import Document._

   def system: S
   def cursor: Cursor[ S ]
   def folder: File
   def groups( implicit tx: S#Tx ) : Groups[ S ]
}
