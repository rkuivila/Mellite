# New features

Regions works like markers in EisK, but with extending objects.
Markers should be implemented with BiPin, regions with Timeline
or BiGroup{?}. For viewing audio-cue objects, these would be
stored with a conventional key in the attr-map.

## High priority

- [ ] views: regions

## Medium priority

- [ ] views: markers
- [ ] timeline-model: super-type with open-ended span-like

## Low priority

- [ ] views: store state (e.g. window position, zoom levels)
- [ ] undo-manager: add non-significant edits (scroll, zoom)

# Old features in EisK not in Mllt

## High priority

- [X] keyboard control: ffwd, rewind
- [ ] op: timeline insertion follows playback

## Medium priority

- [ ] scroll-with-playhead
- [ ] tools: pointer tool
- [ ] keyboard control: ctrl-space play selection

## Low priority

- [ ] sonogram: adjustable or auto-adaptive resolution
- [ ] waveform (time domain) display
- [ ] keyboard control: shift-space play half speed
- [ ] keyboard control: shift-alt-space play double speed
- [ ] tools: zoom tool
- [ ] tools: cross hair (pick frequencies)
- [ ] channel headers
- [ ] channel meters
- [ ] channel y-axis

