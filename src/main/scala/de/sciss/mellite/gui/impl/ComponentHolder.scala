package de.sciss.mellite
package gui
package impl

trait ComponentHolder[ C ] {
   protected var comp: C = _
   def component: C = {
      requireEDT()
      val res = comp
      if( res == null ) sys.error( "Called component before GUI was initialized" )
      res
   }
}
