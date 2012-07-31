package de.sciss.mellite.gui.impl

import javax.swing.Icon
import java.awt.{RenderingHints, LinearGradientPaint, Paint, Graphics2D, Shape, Color, Graphics, Component}
import java.awt.geom.{Path2D, GeneralPath, Rectangle2D}

object PlayStopIcon {
   sealed trait State
   case object Play extends State
   case object Stop extends State

   private val gradFrac = Array[ Float ]( 0.0f, 0.48f, 0.52f, 1.0f )
}
class PlayStopIcon( size: Int = 24, init: PlayStopIcon.State = PlayStopIcon.Play ) extends Icon {
   import PlayStopIcon._
   var state: State = init
   private var colrVar: Color = _

   private var paint: Paint = _
   private val gradColr = new Array[ Color ]( 4 )

   def color: Color = colrVar
   def color_=( value: Color ) {
      colrVar  = value
      val hsb  = Color.RGBtoHSB( value.getRed, value.getGreen, value.getBlue, null )
      val hue  = hsb( 0 )
      val sat  = hsb( 1 )
      val bri  = hsb( 2 )
      val bcl  = math.min( 1f, bri + 0.25f )
      val bfl  = math.max( 0f, bcl - 0.3f )
      gradColr( 0 ) = Color.getHSBColor( hue, sat, bcl )
      gradColr( 1 ) = Color.getHSBColor( hue, sat, bcl - 0.05f )
      gradColr( 2 ) = Color.getHSBColor( hue, sat, bfl + 0.1f )
      gradColr( 3 ) = Color.getHSBColor( hue, sat, bfl )
      paint    = new LinearGradientPaint( 0f, 0f, 0f, size - 1, gradFrac, gradColr )
   }

   color_=( Color.black )

   private val shpPlay: Shape = {
      val sz1  = size - 1
      val ext  = sz1 * 0.7937
      val off  = (sz1 - ext) * 0.5
      val gp   = new GeneralPath( Path2D.WIND_NON_ZERO, 3 )
      gp.moveTo( 0.0, off )
      gp.lineTo( sz1, off + ext * 0.5 )
      gp.lineTo( 0.0, off + ext )
      gp.closePath()
      gp
   }
   private val shpStop: Shape = {
      val ext  = size * 0.7071
      val off  = (size - ext) * 0.5
      new Rectangle2D.Double( off, off, ext, ext )
   }

   def paintIcon( c: Component, g: Graphics, x: Int, y: Int ) {
      val g2 = g.asInstanceOf[ Graphics2D ]
      val atOrig = g2.getTransform
      g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON )
      g2.translate( x, y )
      g2.setPaint( paint )
      g2.fill( if( state == Play ) shpPlay else shpStop )
      g2.setTransform( atOrig )
   }

   def getIconWidth: Int = size

   def getIconHeight: Int = size
}