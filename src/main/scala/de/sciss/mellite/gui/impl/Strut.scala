package de.sciss.mellite
package gui
package impl

import swing.Component
import javax.swing.JComponent
import java.awt.Dimension

object Strut {
   def horizontal( size: Int ) : Component = apply( size, 0 )
   def vertical( size: Int ) : Component = apply( 0, size )

   def apply( horizontal: Int, vertical: Int ) : Component = Component.wrap( new JComponent {
      setPreferredSize( new Dimension( horizontal, vertical ))
   })
}
