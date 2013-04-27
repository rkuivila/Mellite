package de.sciss.mellite

import de.sciss.model.Model
import de.sciss.span.Span
import de.sciss.lucre.event.Change
import de.sciss.span.Span.SpanOrVoid

object TimelineModel {
  sealed trait Update { def model: TimelineModel }
  final case class Visible  (model: TimelineModel, span:   Change[Span])       extends Update
  final case class Position (model: TimelineModel, frame:  Change[Long])       extends Update
  final case class Selection(model: TimelineModel, span:   Change[SpanOrVoid]) extends Update
  final case class Bounds   (model: TimelineModel, span:   Change[Span])       extends Update

  type Listener = Model.Listener[Update]
}
trait TimelineModel extends Model[TimelineModel.Update] {
  def sampleRate: Double

  var visible: Span
  var position: Long
  var selection: SpanOrVoid
  var bounds: Span
}