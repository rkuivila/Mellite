package de.sciss.mellite

import concurrent.stm.{Txn => STMTxn, TxnLocal}
import de.sciss.lucre.stm.Txn
import java.awt.{Toolkit, EventQueue}
import collection.immutable.{IndexedSeq => IIdxSeq}
import javax.swing.KeyStroke

package object gui {
   private val guiCode           = TxnLocal( init = IIdxSeq.empty[ () => Unit ], afterCommit = handleGUI )
   private lazy val primaryMod   = Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

   private def handleGUI( seq: IIdxSeq[ () => Unit ]) {
      def exec() {
         seq.foreach { fun =>
            try {
               fun()
            } catch {
               case e: Throwable => e.printStackTrace()
            }
         }
      }

      if( EventQueue.isDispatchThread ) exec() else EventQueue.invokeLater( new Runnable { def run() { exec() }})
   }

   def guiFromTx( body: => Unit )( implicit tx: Txn[ _ ]) {
//      STMTxn.afterCommit( _ => body )( tx.peer )
      guiCode.transform( _ :+ (() => body) )( tx.peer )
   }


//   def primaryMenuKey( ch: Char )  : KeyStroke = KeyStroke.getKeyStroke( ch, primaryMod )
   def primaryMenuKey( code: Int ) : KeyStroke = KeyStroke.getKeyStroke( code, primaryMod )
}