package de.sciss.mellite
package gui

import swing.{Action, MenuItem, Menu}

object MenuBar {
   def apply() : swing.MenuBar = {
      val mb = new swing.MenuBar {
         contents += new Menu( "File" ) {
            contents += new MenuItem( ActionNewFile )
            contents += new MenuItem( ActionOpenFile )
            contents += new Menu( "Open Recent" ) {
               // ...
            }
         }
      }
      mb
   }
}
