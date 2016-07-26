/*
 *  TimelineRendering.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui

import java.awt.geom.Path2D
import java.awt.{Paint, Rectangle, Stroke}

import de.sciss.sonogram

/** Paint support. */
trait TimelineRendering extends sonogram.PaintController {
  /** For general use. */
  def shape1                      : Path2D
  /** For general use. */
  def shape2                      : Path2D

  def pntFadeFill                 : Paint
  def pntFadeOutline              : Paint

  def pntBackground               : Paint

  def pntNameShadowDark           : Paint
  def pntNameShadowLight          : Paint
  def pntNameDark                 : Paint
  def pntNameLight                : Paint

  def pntRegionBackground         : Paint
  def pntRegionBackgroundMuted    : Paint
  def pntRegionBackgroundSelected : Paint

  def pntRegionOutline            : Paint
  def pntRegionOutlineSelected    : Paint

  def regionTitleHeight           : Int
  def regionTitleBaseline         : Int

  def pntInlet                    : Paint
  def pntInletSpan                : Paint
  def strokeInletSpan             : Stroke

  def ttMoveState                 : TrackTool.Move
  def ttResizeState               : TrackTool.Resize
  def ttGainState                 : TrackTool.Gain
  def ttFadeState                 : TrackTool.Fade
  def ttFunctionState             : TrackTool.Function

  /* Of current drawing operation. */
  def clipRect                    : Rectangle

  var sonogramBoost               : Float

  /* General purpose. */

  /** Size guaranteed to be even. */
  def intArray1 : Array[Int]
  /** Size guaranteed to be even. */
  def intArray2 : Array[Int]
}