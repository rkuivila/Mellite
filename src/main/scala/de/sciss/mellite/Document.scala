package de.sciss.mellite

import java.io.File
import de.sciss.lucre.expr.{BiGroup, LinkedList}
import de.sciss.synth.proc.Proc
import impl.{DocumentImpl => Impl}

object Document {
   type Group        = BiGroup.Var[ Cf, Proc[ Cf ], Proc.Update[ Cf ]]
   type GroupUpdate  = BiGroup.Update[ Cf, Proc[ Cf ], Proc.Update[ Cf ]]
   type Groups       = LinkedList.Var[ Cf, Group, GroupUpdate ]
   type GroupsUpdate = LinkedList.Update[ Cf, Group, GroupUpdate ]

   def read(  dir: File ) : Document = Impl.read( dir )
   def empty( dir: File ) : Document = Impl.empty( dir )
}
trait Document {
   import Document._

   def system: Cf
   def folder: File
   def groups( implicit tx: Cf#Tx ) : Groups
}
