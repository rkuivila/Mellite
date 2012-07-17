package de.sciss.mellite

import java.io.File
import de.sciss.lucre.expr.{BiGroup, LinkedList}
import de.sciss.synth.proc.Proc

trait Document {
   def folder: File
   def groups( implicit tx: Cf#Tx ) : LinkedList.Var[ Cf, BiGroup.Var[ Cf, Proc[ Cf ], Proc.Update[ Cf ]], BiGroup.Update[ Cf, Proc[ Cf ], Proc.Update[ Cf ]]]
}
