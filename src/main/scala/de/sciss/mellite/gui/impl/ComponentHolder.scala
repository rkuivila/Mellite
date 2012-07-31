package de.sciss.mellite
package gui
package impl

trait ComponentHolder[ C ] {
   final protected var comp: C = _
   final def component: C = {
      requireEDT()
      val res = comp
      if( res == null ) sys.error( "Called component before GUI was initialized" )
      res
   }
}
