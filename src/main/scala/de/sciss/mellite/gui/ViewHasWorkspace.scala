/*
 *  ViewHasWorkspace.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite.gui

import de.sciss.lucre.event.Sys
import de.sciss.lucre.swing.{View => SView}
import de.sciss.mellite.Workspace

trait ViewHasWorkspace[S <: Sys[S]] extends SView.Cursor[S] {
  def workspace: Workspace[S]
}
