package de.sciss.mellite

import de.sciss.lucre.{DataOutput, Writable}
import de.sciss.synth.proc.Sys
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.{Disposable, Mutable}
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
                          ( implicit tx: S#Tx ) : Element[ S, Expr.Var[ S, Int ]] = {
      val expr = Ints.newVar[ S ]( init )
      mkElem( expr, name )
   }

   def double[ S <: Sys[ S ]]( init: Expr[ S, Double ], name: Option[ String ] = None )
                             ( implicit tx: S#Tx ) : Element[ S, Expr.Var[ S, Double ]] = {
      val expr = Doubles.newVar( init )
      mkElem( expr, name )
   }

   def string[ S <: Sys[ S ]]( init: Expr[ S, String ], name: Option[ String ] = None )
                             ( implicit tx: S#Tx ) : Element[ S, Expr.Var[ S, String ]] = {
      val expr = Strings.newVar(  init )
      mkElem( expr, name )
   }

//   def group[ S <: Sys[ S ]]( init: Expr[ S, Group[ S ]], name: Option[ String ] = None )
//                            ( implicit tx: S#Tx ) : Element[ S, Group[ S ]] = {
//      val expr = Groups.newVar( init )
//      mkElem( expr, name )
//   }

   // A[ ~ <: Sys[ ~ ] forSome { type ~ }]
   private def mkElem[ S <: Sys[ S ], A ](
      elem: A with Writable with Disposable[ S#Tx ], name: Option[ String ])( implicit tx: S#Tx ) : Element[ S, A ] = {
      val id      = tx.newID()
      val nameEx  = StringOptions.newVar[ S ]( StringOptions.newConst( name ))
      new Impl[ S, A ]( id, nameEx, elem )
   }

   private final class Impl[ S <: Sys[ S ], A ](
      val id: S#ID, val name: Expr.Var[ S, Option[ String ]], val elem: A with Writable with Disposable[ S#Tx ])
   extends Element[ S, A ] with Mutable.Impl[ S ] {
      protected def writeData( out: DataOutput ) {
         name.write( out )
         elem.write( out )
      }

      protected def disposeData()( implicit tx: S#Tx ) {
         name.dispose()
         elem.dispose()
      }
   }
}
trait Element[ S <: Sys[ S ], A ] extends Mutable[ S#ID, S#Tx ] {
   def name: Expr.Var[ S, Option[ String ]]
   def elem: A // Expr.Var[ S, A ]
//   def attributes: Map[ String, Any ]
}