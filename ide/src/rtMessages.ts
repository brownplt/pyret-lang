export type SpyMessage = {
  tag: 'spy-message',
  message: true,
  value: any,
  loc: string,
};

export type SpyValue = {
  tag: 'spy-value',
  value: {
    key: string,
    value: any
  },
  loc: string,
};

export interface RTMessages {
  messages: RTMessage[],
  outdated: boolean,
}

export interface RTMessage {
  data: RawRTMessage,
  key: string,
}

export type RawRTMessage = SpyMessage | SpyValue;

export function isSpyValue(a: RawRTMessage): a is SpyValue {
  return a.tag === 'spy-value';
}

export function isSpyMessage(a: RawRTMessage): a is SpyMessage {
  return a.tag === 'spy-message';
}

export function makeRTMessage(m: rawRTMessage): RTMessage {
  return {
    data: m,
    // TODO(alex): better key scheme?
    key: window.performance().now().toString(),
  };
}
