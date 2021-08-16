import { Chunk } from './chunk';

export type HistoryEvent =
// The index it was before being deleted (the index currently occupied by the
// one *following* it, if any)
| { type: 'delete', index: number, chunk: Chunk }
// The index it has after being inserted (the index previously occupied by the
// one now following it). Could be an ID, but HistoryEvents should track with
// indices perfectly
// No, we don't need the chunk to undo it. But we need to be able to re-do it!
// If we haven't un-done this, we need the chunk anyway; if we've inserted and
// deleted, we need it for delete anyway
| { type: 'insert', index: number, chunk: Chunk }
| { type: 'clear', chunks: Chunk[], firstTechnicallyOutdatedSegment: number };
