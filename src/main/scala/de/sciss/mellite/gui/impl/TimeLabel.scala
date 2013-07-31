package de.sciss.mellite
package gui
package impl

import javax.swing.JComponent
import java.awt.{Font, Graphics, Graphics2D, Dimension}

object TimeLabel {
//   private val prefSz   = new Dimension( 126, 20 )
   private val prefSz   = new Dimension( 112, 16 )
   private lazy val fnt = {
      val is   = Mellite.getClass.getResourceAsStream( "Receiptional Receipt.ttf" ) // "LCDML___.TTF"
      require( is != null, "Font resource not found" )   // fucking Java
      val res = Font.createFont( Font.TRUETYPE_FONT, is ).deriveFont( 13f )
      is.close()
      res
   }
}
class TimeLabel extends JComponent {
  import TimeLabel._

  private var millisVar = 0L
  private var textVar = ""

  //   def text: String = textVar
  //   def text_=( value: String ): Unit = {
  //      textVar = value
  //      repaint()
  //   }

  recalcText()

  def millis: Long = millisVar

  def millis_=(value: Long): Unit = {
    millisVar = value
    recalcText()
    repaint()
  }

  private def recalcText(): Unit = {
    val neg     = millisVar < 0
    val millis0 = if( neg ) -millisVar else millisVar
    val millis  = millis0 % 1000
    val secs0   = millis0 / 1000
    val secs    = secs0 % 60
    val mins0   = secs0 / 60
    val mins    = mins0 % 60
    val hours   = math.min( 99, mins0 / 60 )
    val sb      = new StringBuilder( 12 )
    sb.append( if( neg ) '-' else { if( hours <= 9 ) ' ' else ((hours / 10) + 48).toChar })
    sb.append( ((hours % 10) + 48).toChar )
    sb.append( ':' )
    sb.append( ((mins / 10) + 48).toChar )
    sb.append( ((mins % 10) + 48).toChar )
    sb.append( ':' )
    sb.append( ((secs / 10) + 48).toChar )
    sb.append( ((secs % 10) + 48).toChar )
    sb.append( '.' )
    sb.append( ((millis / 100) + 48).toChar )
    sb.append( (((millis / 10) % 10) + 48).toChar )
    sb.append( ((millis % 10) + 48).toChar )
    textVar     = sb.toString()
  }

  override def getPreferredSize: Dimension = prefSz

  override def paintComponent(g: Graphics): Unit = {
    val g2 = g.asInstanceOf[Graphics2D]
    g2.setFont(fnt)
    val x = ((getWidth - 112) >> 1) + 2
    val y = ((getHeight - 16) >> 1) + 16
    g2.drawString(textVar, x, y)
  }
}
