/*
 *  Color.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite

import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.impl.ExprTypeImplA
import de.sciss.lucre.expr.{Expr => _Expr}
import de.sciss.mellite.impl.ColorImpl
import de.sciss.model
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer, Serializer}
import de.sciss.synth.proc
import de.sciss.synth.proc.Obj

import scala.collection.immutable.{IndexedSeq => Vec}

object Color {
  final val typeID = 22

  private final val COOKIE = 0x436F // 'Co'

  implicit object serializer extends ImmutableSerializer[Color] {
    def write(c: Color, out: DataOutput): Unit = {
      out.writeShort(COOKIE)
      out.writeByte(c.id)
      if (c.id >= 16) out.writeInt(c.rgba)
    }

    def read(in: DataInput): Color = {
      val cookie = in.readShort()
      if (cookie != COOKIE) sys.error(s"Unexpected cookie $cookie, expected $COOKIE")
      val id = in.readByte()
      if (id < 16) Palette(id)
      else User(in.readInt())
    }
  }

  object Expr extends ExprTypeImplA[Color] {
    def typeID = Color.typeID

    def readValue (              in : DataInput ): Color  = Color.serializer.read (       in )
    def writeValue(value: Color, out: DataOutput): Unit   = Color.serializer.write(value, out)
  }
  sealed trait Expr[S <: Sys[S]] extends _Expr[S, Color]

  // ---- Elem ----

  implicit object Elem extends proc.Elem.Companion[Elem] {
    def typeID = Color.Expr.typeID

    def apply[S <: Sys[S]](peer: _Expr[S, Color])(implicit tx: S#Tx): Color.Elem[S] =
      ColorImpl(peer)

    object Obj {
      def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, Color.Elem]] =
        if (obj.elem.isInstanceOf[Color.Elem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, Color.Elem]])
        else None
    }

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Color.Elem[S]] =
      ColorImpl.serializer[S]
  }
  trait Elem[S <: Sys[S]] extends proc.Elem[S] {
    type Peer       = _Expr[S, Color]
    type PeerUpdate = model.Change[Color]
    type This       = Elem[S]
  }

  /** Palette of sixteen predefined colors. */
  val Palette: Vec[Color] = Vector(
    Predefined( 0, "Dark Blue"  , rgba = 0xFF00235C),
    Predefined( 1, "Light Blue" , rgba = 0xFF007EFA),
    Predefined( 2, "Cyan"       , rgba = 0xFF62F2F5),
    Predefined( 3, "Mint"       , rgba = 0xFF34AC71),
    Predefined( 4, "Green"      , rgba = 0xFF3CEA3B),
    Predefined( 5, "Yellow"     , rgba = 0xFFEEFF00),
    Predefined( 6, "Dark Beige" , rgba = 0xFF7D654B),
    Predefined( 7, "Light Beige", rgba = 0xFFAA9B72),
    Predefined( 8, "Orange"     , rgba = 0xFFFF930D),
    Predefined( 9, "Red"        , rgba = 0xFFFF402E),
    Predefined(10, "Maroon"     , rgba = 0xFF8D0949),
    Predefined(11, "Fuchsia"    , rgba = 0xFFFF06D8),
    Predefined(12, "Purple"     , rgba = 0xFFBC00E6),
    Predefined(13, "Black"      , rgba = 0xFF000000),
    Predefined(14, "Silver"     , rgba = 0xFFA9BBC0),
    Predefined(15, "White"      , rgba = 0xFFFFFFFF)
  )

  private final case class Predefined(id: Int, name: String, rgba: Int) extends Color

  final case class User(rgba: Int) extends Color {
    def name = "User"
    def id = 16
  }
}
sealed trait Color {
  /** Value consisting of the alpha component in bits 24-31, the red component in bits 16-23,
    * the green component in bits 8-15, and the blue component in bits 0-7.
    *
    * So technically the bits are sorted as 'ARGB'
    */
  def rgba: Int

  /** The identifier is used for serialization. Predefined
    * colors have an id smaller than 16, user colors have an id of 16.
    */
  def id: Int

  /** Either predefined name or `"User"` */
  def name: String
}
