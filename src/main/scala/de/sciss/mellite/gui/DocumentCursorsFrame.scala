/*
 *  DocumentCursorsFrame.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss
package mellite
package gui

import impl.document.{CursorsFrameImpl => Impl}
import synth.proc

object DocumentCursorsFrame {
  type S = proc.Confluent
  type D = S#D
  def apply(document: ConfluentDocument)(implicit tx: D#Tx): DocumentCursorsFrame = Impl(document)
}
trait DocumentCursorsFrame /* [S <: Sys[S]] */ {
  def component: desktop.Window
  def view: DocumentCursorsView
  def document : ConfluentDocument // Document[S]
}

trait DocumentCursorsView extends DocumentView[DocumentCursorsFrame.S]