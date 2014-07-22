///*
// *  VisualInstantPresentation.scala
// *  (Mellite)
// *
// *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is published under the GNU General Public License v3+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.mellite
//package gui
//
//import de.sciss.lucre.stm.Cursor
//import de.sciss.synth.proc.Transport
//import impl.realtime.{PanelImpl => Impl}
//import de.sciss.lucre.synth.Sys
//import de.sciss.lucre.swing.View
//
//object InstantGroupPanel {
//  def apply[S <: Sys[S]](document: Workspace[S], transport: Transport[S])
//                        (implicit tx: S#Tx, cursor: Cursor[S]): InstantGroupPanel[S] =
//    Impl(document, transport)
//}
//
//trait InstantGroupPanel[S <: Sys[S]] extends View[S]
