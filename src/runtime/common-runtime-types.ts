export interface SpyExpr {
  key: string,
  expr: () => any,
  loc: string
}

export interface SpyObject {
  message: () => string,
  loc: string,
  exprs: SpyExpr[],
}

export interface CheckResult {
  success: boolean,
  path: string,
  loc: string,
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
