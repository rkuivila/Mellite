package de.sciss.mellite

object Test extends App {
   val a = List( 1, 2, 3 )
   val b = List( 4, 5, 6 )

   val c = a ::: b
   println( c )
}
