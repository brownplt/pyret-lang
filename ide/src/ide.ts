export type IDE = {
  dispatchSpyMessage: (loc: string, message: string | undefined) => void,
  dispatchSpyValue: (loc: string, key: string, value: any) => void,
}
