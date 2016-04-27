/*
 *  TimelineRenderingImpl.scala
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
package impl
package timeline

import java.awt.geom.Path2D
import java.awt.image.{BufferedImage, ImageObserver}
import java.awt.{BasicStroke, LinearGradientPaint, Paint, Rectangle, Stroke, TexturePaint, Color => JColor}

import scala.swing.Component

object TimelineRenderingImpl {
  private final val pntFadeFill: Paint = {
    val img = new BufferedImage(4, 2, BufferedImage.TYPE_INT_ARGB)
    img.setRGB(0, 0, 4, 2, Array(
      0xFF05AF3A, 0x00000000, 0x00000000, 0x00000000,
      0x00000000, 0x00000000, 0xFF05AF3A, 0x00000000
    ), 0, 4)
    new TexturePaint(img, new Rectangle(0, 0, 4, 2))
  }
  private final val pntFadeOutline : Paint = new JColor(0x05, 0xAF, 0x3A)

  private val colrRegionOutline           = new JColor(0x68, 0x68, 0x68)
  private val colrRegionOutlineSelected   = JColor.blue

  private val pntRegionBackground: Paint = new LinearGradientPaint(0f, 1f, 0f, 62f,
    Array[Float](0f, 0.23f, 0.77f, 1f), Array[JColor](new JColor(0x5E, 0x5E, 0x5E), colrRegionOutline,
      colrRegionOutline, new JColor(0x77, 0x77, 0x77)))
  private val pntRegionBackgroundMuted: Paint = new JColor(0xFF, 0xFF, 0xFF, 0x60)
  private val pntRegionBackgroundSelected: Paint = new LinearGradientPaint(0f, 1f, 0f, 62f,
    Array[Float](0f, 0.23f, 0.77f, 1f), Array[JColor](new JColor(0x00, 0x00, 0xE6), colrRegionOutlineSelected,
      colrRegionOutlineSelected, new JColor(0x1A, 0x1A, 0xFF)))

  private[this] val pntBgAquaPixels: Array[Int] = Array(
    0xFFF0F0F0, 0xFFF0F0F0, 0xFFF0F0F0, 0xFFF0F0F0, 0xFFF0F0F0, 0xFFF0F0F0, 0xFFF0F0F0, 0xFFF0F0F0,
    0xFFECECEC, 0xFFECECEC, 0xFFECECEC, 0xFFECECEC, 0xFFECECEC, 0xFFECECEC, 0xFFECECEC, 0xFFECECEC
  )
  private[this] val pntBgDarkPixels: Array[Int] = Array(
    0xFF0F0F0F, 0xFF0F0F0F, 0xFF0F0F0F, 0xFF0F0F0F, 0xFF0F0F0F, 0xFF0F0F0F, 0xFF0F0F0F, 0xFF0F0F0F,
    0xFF131313, 0xFF131313, 0xFF131313, 0xFF131313, 0xFF131313, 0xFF131313, 0xFF131313, 0xFF131313
  )

  private val pntBackgroundDark: Paint = {
    val imgDark = new BufferedImage(4, 4, BufferedImage.TYPE_INT_ARGB)
    imgDark.setRGB(0, 0, 4, 4, pntBgDarkPixels, 0, 4)
    new TexturePaint(imgDark, new Rectangle(0, 0, 4, 4))
  }

  private val pntBackgroundLight: Paint = {
    val imgLight = new BufferedImage(4, 4, BufferedImage.TYPE_INT_ARGB)
    imgLight.setRGB(0, 0, 4, 4, pntBgAquaPixels, 0, 4)
    new TexturePaint(imgLight, new Rectangle(0, 0, 4, 4))
  }

  private val pntNameShadowDark : Paint   = new JColor(0, 0, 0, 0x80)
  private val pntNameShadowLight: Paint   = new JColor(0xFF, 0xFF, 0xFF, 0x80)
  private val pntNameDark       : Paint   = JColor.white
  private val pntNameLight      : Paint   = JColor.black

  private val pntInlet          : Paint   = JColor.gray
  private val pntInletSpan      : Paint   = JColor.gray
  private val strkInletSpan     : Stroke  = new BasicStroke(1f, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10f,
    Array[Float](0.5f, 1.5f), 0f)
}

final class TimelineRenderingImpl(component: Component, isDark: Boolean) extends TimelineRendering {
  import timeline.{TimelineRenderingImpl => Impl}

  val shape1                      : Path2D            = new Path2D.Float()
  val shape2                      : Path2D            = new Path2D.Float()

  val pntFadeFill                 : Paint             = Impl.pntFadeFill
  val pntFadeOutline              : Paint             = Impl.pntFadeOutline

  val pntBackground               : Paint             = if (isDark) Impl.pntBackgroundDark else Impl.pntBackgroundLight
  
  val pntNameShadowDark           : Paint             = Impl.pntNameShadowDark
  val pntNameShadowLight          : Paint             = Impl.pntNameShadowLight
  val pntNameDark                 : Paint             = Impl.pntNameDark
  val pntNameLight                : Paint             = Impl.pntNameLight

  val pntRegionBackground         : Paint             = Impl.pntRegionBackground
  val pntRegionBackgroundMuted    : Paint             = Impl.pntRegionBackgroundMuted
  val pntRegionBackgroundSelected : Paint             = Impl.pntRegionBackgroundSelected

  val pntRegionOutline            : Paint             = Impl.colrRegionOutline
  val pntRegionOutlineSelected    : Paint             = Impl.colrRegionOutlineSelected
  
  val regionTitleHeight           : Int               = 15
  val regionTitleBaseline         : Int               = 12

  val pntInlet                    : Paint             = Impl.pntInlet
  val pntInletSpan                : Paint             = Impl.pntInletSpan
  val strokeInletSpan             : Stroke            = Impl.strkInletSpan

  var ttMoveState                 : TrackTool.Move    = TrackTool.NoMove
  var ttResizeState               : TrackTool.Resize  = TrackTool.NoResize
  var ttGainState                 : TrackTool.Gain    = TrackTool.NoGain
  var ttFadeState                 : TrackTool.Fade    = TrackTool.NoFade
  var ttFunctionState             : TrackTool.Function= TrackTool.NoFunction

  val clipRect                    : Rectangle         = new Rectangle

  // ---- sonogram ----

  var sonogramBoost: Float = 1.0f

  def imageObserver: ImageObserver = component.peer

  def adjustGain(amp: Float, pos: Double): Float = amp * sonogramBoost
}