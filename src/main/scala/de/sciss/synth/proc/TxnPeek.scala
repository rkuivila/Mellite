//package de.sciss.synth.proc
//
//import concurrent.stm.atomic
//
//object TxnPeek {
//  def apply[A](fun: Txn => A): A = atomic { tx =>
//    val ptx = Txn.applyPlain(tx)
//    fun(ptx)
//  }
//}
