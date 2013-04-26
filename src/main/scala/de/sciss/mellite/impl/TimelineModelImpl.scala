package de.sciss.mellite
package impl

import de.sciss.model.impl.ModelImpl
import de.sciss.span.Span
import de.sciss.lucre.event.Change
import de.sciss.span.Span.SpanOrVoid

final class TimelineModelImpl(val span: Span, val sampleRate: Double)
  extends TimelineModel with ModelImpl[TimelineModel.Update] {

  import TimelineModel._

  private var _visi = span
  private var _pos  = span.start
  private var _sel  = Span.Void: SpanOrVoid

  def visible = _visi
  def visible_=(value: Span) {
    val oldSpan = _visi
    if (oldSpan != value) {
      _visi = value
      val visiCh  = Change(oldSpan, value)
      //      val oldPos  = _pos
      //      if (oldPos < value.start || oldPos > value.stop) {
      //        _pos = math.max(value.start, math.min(value.stop, _pos))
      //      }
      dispatch(Visible(this, visiCh))
    }
  }

  def position = _pos
  def position_=(value: Long) {
    val oldPos = _pos
    if (oldPos != value) {
      _pos      = value
      val posCh = Change(oldPos, value)
      dispatch(Position(this, posCh))
    }
  }

  def selection = _sel
  def selection_=(value: SpanOrVoid) {
    val oldSel = _sel
    if (oldSel != value) {
      _sel  = value
      val selCh = Change(oldSel, value)
      dispatch(Selection(this, selCh))
    }
  }
}