package de.sciss.mellite
package impl

import de.sciss.model.impl.ModelImpl
import de.sciss.span.Span
import de.sciss.lucre.event.Change

final class TimelineModelImpl(val span: Span, val sampleRate: Double)
  extends TimelineModel with ModelImpl[TimelineModel.Update] {

  import TimelineModel._

  private var _visi = span
  private var _pos  = span.start

  def visible = _visi
  def visible_=(value: Span) {
    val oldSpan = _visi
    if (oldSpan != value) {
      _visi = value
      val visiCh  = Change(oldSpan, value)
      val oldPos  = _pos
      if (oldPos < value.start || oldPos > value.stop) {
        _pos = math.max(value.start, math.min(value.stop, _pos))
      }
      val posCh = Change(oldPos, _pos)
      dispatch(Update(this, visiCh, posCh))
    }
  }

  def position = _pos
  def position_=(value: Long) {
    val oldPos = _pos
    if (oldPos != value) {
      _pos      = value
      val posCh = Change(oldPos, value)
      dispatch(Update(this, Change(_visi, _visi), posCh))
    }
  }
}