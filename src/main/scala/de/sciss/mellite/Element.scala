package de.sciss.mellite

import de.sciss.lucre.{DataOutput, Writable}
import de.sciss.synth.proc.Sys
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.Mutable
import de.sciss.synth.expr.{Doubles, Strings, Ints}

/*
 * Elements
 * - Proc
 * - ProcGroup
 * - primitive expressions
 * -
 */

object Element {
   def int[ S <: Sys[ S ]]( init: Expr[ S, Int ], name: Option[ String ] = None )
                          ( implicit tx: S#Tx ) : Element[ S, Int ] = {
      val expr = Ints.newVar( init )
      mkElem( expr, name )
   }

   def double[ S <: Sys[ S ]]( init: Expr[ S, Double ], name: Option[ String ] = None )
                             ( implicit tx: S#Tx ) : Element[ S, Double ] = {
      val expr = Doubles.newVar( init )
      mkElem( expr, name )
   }

   def string[ S <: Sys[ S ]]( init: Expr[ S, String ], name: Option[ String ] = None )
                             ( implicit tx: S#Tx ) : Element[ S, String ] = {
      val expr = Strings.newVar( init )
      mkElem( expr, name )
   }

   private def mkElem[ S <: Sys[ S ], A ]( expr: Expr.Var[ S, A ], name: Option[ String ])
                                         ( implicit tx: S#Tx ) : Element[ S, A ] = {
      val id      = tx.newID()
      val nameEx  = StringOptions.newVar[ S ]( StringOptions.newConst( name ))
      new Impl( id, nameEx, expr )
   }

   private final class Impl[ S <: Sys[ S ], A ]( val id: S#ID, val name: Expr.Var[ S, Option[ String ]],
                                                 val expr: Expr.Var[ S, A ])
   extends Element[ S, A ] with Mutable.Impl[ S ] {
      protected def writeData( out: DataOutput ) {
         name.write( out )
         expr.write( out )
      }

      protected def disposeData()( implicit tx: S#Tx ) {
         name.dispose()
         expr.dispose()
      }
   }
}
trait Element[ S <: Sys[ S ], A ] extends Mutable[ S#ID, S#Tx ] {
   def name: Expr.Var[ S, Option[ String ]]
   def expr: Expr.Var[ S, A ]
//   def attributes: Map[ String, Any ]
}