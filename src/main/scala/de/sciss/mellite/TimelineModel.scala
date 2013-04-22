package de.sciss.mellite

import de.sciss.model.Model
import de.sciss.span.Span
import de.sciss.lucre.event.Change

object TimelineModel {
  final case class Update(model: TimelineModel, visible: Change[Span], position: Change[Long])

  type Listener = Model.Listener[Update]
}
trait TimelineModel extends Model[TimelineModel.Update] {
  def span: Span
  def sampleRate: Double

  var visible: Span
  var position: Long
}