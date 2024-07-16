export type Srcloc = 
  | [string]
  | [string, number, number, number, number, number, number]

export interface SpyObjectResult {
  messageResult: SpyMessageResult,
  exprResult: SpyExprResult,
}

export interface SpyMessageResult {
  message: string,
  loc: Srcloc,
}

export interface SpyExprResult {
  key: string,
  expr: () => any,
  loc: Srcloc
}

export interface CheckResult {
  success: boolean,
  path: string,
  loc: Srcloc,
  lhs: CheckExprEvalResult,
  rhs: CheckExprEvalResult,
  exception?: any,
}

export interface CheckExprEvalResult {
  value: any,
  exception: boolean,
  exception_val: any,
}

export interface CheckTestResult {
  success: boolean,
  lhs: CheckExprEvalResult,
  rhs: CheckExprEvalResult,
}

// NOTE(alex): An opaque type
export enum PyretList { }
export class ExhaustiveSwitchError extends Error {
  constructor(v: never, message?: string) {
    super(`Switch is not exhaustive on \`${JSON.stringify(v)}\`: ${message}`);
  }
}
