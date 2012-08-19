package de.sciss.mellite
package gui

import swing.{Action, AbstractButton}

object DoClickAction {
   def apply( button: AbstractButton ) : Action = Action.apply( null )( button.doClick() )
}
