package de.sciss.mellite.gui.impl

class ScrollBarImpl extends swing.ScrollBar {
  me =>
  peer.addAdjustmentListener(new java.awt.event.AdjustmentListener {
    def adjustmentValueChanged(e: java.awt.event.AdjustmentEvent) {
      publish(new swing.event.ValueChanged(me))
    }
  })
}