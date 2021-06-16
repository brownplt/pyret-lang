export type ErrorDisplay = 
  | { $name: "paragraph", dict: { 'contents': any } }
  | { $name: "bulleted-sequence", dict: { 'contents': any } }
  | { $name: "v-sequence", dict: { 'contents': any } }
  | { $name: "h-sequence", dict: { 'contents': any, 'sep': string } }
  | {
    $name: "h-sequence-sep",
    dict: { 'contents': any, 'sep': string, 'last': string }
  }
  | { $name: "embed", dict: { 'val': any } }
  | { $name: "text", dict: { 'str': string } }
  | { $name: "loc", dict: { 'loc': any } }
  | {
    $name: "maybe-stack-loc",
    dict: 
      {
        'n': Number,
        'user-frames-only': boolean,
        'contents-with-loc': any,
        'contents-without-loc': ErrorDisplay
      }
  }
  | { $name: "code", dict: { 'contents': ErrorDisplay } }
  | { $name: "cmcode", dict: { 'loc': any } }
  | {
    $name: "loc-display",
    dict: { 'loc': any, 'style': string, 'contents': ErrorDisplay }
  }
  | { $name: "optional", dict: { 'contents': ErrorDisplay } }
  | {
    $name: "highlight",
    dict: { 'contents': ErrorDisplay, 'locs': any, 'color': Number }
  }

