package de.sciss.mellite

import de.sciss.lucre.{DataInput, stm, DataOutput, Writable}
import de.sciss.synth.proc.{InMemory, Sys}
import de.sciss.lucre.expr.Expr
import stm.{MutableSerializer, Disposable, Mutable}
import de.sciss.synth.expr.{BiTypeImpl, Doubles, Strings, Ints}
import de.sciss.lucre.event.EventLike
import annotation.switch

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
//      val expr = Ints.newVar[ S ]( init )
      mkExpr[ S, Int ]( Ints, init, name )
   }

   def double[ S <: Sys[ S ]]( init: Expr[ S, Double ], name: Option[ String ] = None )
                             ( implicit tx: S#Tx ) : Element[ S, Expr.Var[ S, Double ]] = {
//      val expr = Doubles.newVar( init )
      mkExpr[ S, Double ]( Doubles, init, name )
   }

   def string[ S <: Sys[ S ]]( init: Expr[ S, String ], name: Option[ String ] = None )
                             ( implicit tx: S#Tx ) : Element[ S, Expr.Var[ S, String ]] = {
//      val expr = Strings.newVar(  init )
      mkExpr( Strings, init, name )
   }

//   def group[ S <: Sys[ S ]]( init: Expr[ S, Group[ S ]], name: Option[ String ] = None )
//                            ( implicit tx: S#Tx ) : Element[ S, Group[ S ]] = {
//      val expr = Groups.newVar( init )
//      mkElem( expr, name )
//   }

   def serializer[ S <: Sys[ S ]] : stm.Serializer[ S#Tx, S#Acc, Element[ S, _ ]] = anySer.asInstanceOf[ Ser[ S ]]

   private final val anySer = new Ser[ InMemory ]

   private final class Ser[ S <: Sys[ S ]] extends stm.Serializer[ S#Tx, S#Acc, Element[ S, _ ]] {
      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Element[ S, _ ] = {
         val id      = tx.readID( in, access )
         val typeID  = in.readInt()
         val nameEx  = StringOptions.readVar[ S ]( in, access )
         val expr    = (typeID: @switch) match {
            case Ints.typeID     => Ints.readVar[    S ]( in, access )
            case Doubles.typeID  => Doubles.readVar[ S ]( in, access )
            case Strings.typeID  => Strings.readVar[ S ]( in, access )
         }
         new Impl( id, nameEx, typeID, expr )
      }

      def write( elem: Element[ S, _ ], out: DataOutput) {
         elem.write( out )
      }
   }

   // A[ ~ <: Sys[ ~ ] forSome { type ~ }]
   private def mkExpr[ S <: Sys[ S ], A ]( biType: BiTypeImpl[ A ],
      init: Expr[ S, A ], name: Option[ String ])( implicit tx: S#Tx ) : Element[ S, Expr.Var[ S, A ]] = {
      val expr    = biType.newVar[ S ]( init )
      val id      = tx.newID()
      val nameEx  = StringOptions.newVar[ S ]( StringOptions.newConst( name ))
      new Impl( id, nameEx, biType.typeID, expr )
   }

   private final class Impl[ S <: Sys[ S ], A ](
      val id: S#ID, val name: Expr.Var[ S, Option[ String ]], typeID: Int, val elem: A with Writable with Disposable[ S#Tx ])
   extends Element[ S, A ] with Mutable.Impl[ S ] {
      protected def writeData( out: DataOutput ) {
         out.writeInt( typeID )
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
//   def changed: EventLike[ S, Any, A ]
   def name: Expr.Var[ S, Option[ String ]]
   def elem: A // Expr.Var[ S, A ]
//   def attributes: Map[ String, Any ]
}