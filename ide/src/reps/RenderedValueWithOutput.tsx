import React from 'react';
import type * as R from '../../../src/runtime/runtime';
import { runStopify, getAsyncRuntime } from '../runner';
import ValueSkeletonWidget from './ValueSkeletonWidget';

type RVWOProps = { value: R.PyretValue };
type RVWOState = { value: R.ValueSkeleton };

/**
 * Some context needs to call $tooutput to render each value.
 * Since $tooutput can call back into Pyret, and hence stopified code, it needs
 * to use runStopify. That's asynchronous.
 *
 * As a result, this component needs to manage the props update (containing the
 * Pyret values) and transform them to ValueSkeletons correctly asynchronously.
 * The actual rendering logic happens more synchronously in ValueSkeleonRenderer
 * after the conversion happens here.
 */
export default class RenderedValueWithOutput extends React.Component<RVWOProps, RVWOState> {
  constructor(props : RVWOProps) {
    super(props);
    this.state = { value: { $name: 'vs-str', s: 'unassigned value' } };
  }

  async componentDidMount() {
    const runtime = getAsyncRuntime();
    const { value } = this.props;
    const vs : any = await runStopify(() => runtime.$tooutput(value));
    this.setState({ value: vs });
  }

  componentDidUpdate(prevProps : RVWOProps, prevState : RVWOState) {
    if (prevProps.value !== this.props.value) {
      console.log('Updating state in RenderedValueWithOutput', prevProps, this.props, prevState, this.state);
      const runtime = getAsyncRuntime();
      const { value } = this.props;
      const vsp : any = runStopify(() => runtime.$tooutput(value));
      vsp.then((vs : R.ValueSkeleton) => this.setState({ value: vs }));
    }
  }

  render() {
    if (this.state === null) {
      return '...';
    }
    const { value } = this.state;
    return <ValueSkeletonWidget value={value} />;
  }
}

/*
class ErrorBoundary extends React.Component<any, any> {
  constructor(props : any) {
    super(props);
    this.state = { hasError: false };
  }

  static getDerivedStateFromError() {    // Update state so the next render will show the fallback UI.
    return { hasError: true };
  }

  componentDidCatch(error : any, errorInfo : any) {    // You can also log the error to an error reporting service
    console.log(error, errorInfo);
  }

  render() {
    if (this.state.hasError) {      // You can render any custom fallback UI
      return <h1>Something went wrong.</h1>;
    }
    return this.props.children;
  }
}
*/
