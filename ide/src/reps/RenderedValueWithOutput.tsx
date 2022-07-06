import React from 'react';
import type * as R from '../../../src/runtime/runtime';
import { runStopify, getAsyncRuntime } from '../runner';
import ValueSkeletonWidget from './ValueSkeletonWidget';

type RVWOProps = { value: R.PyretValue };
type RVWOState = { value: R.ValueSkeleton };

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

  static async getDerivedStateFromProps(props : RVWOProps) : Promise<RVWOState> {
    const runtime = getAsyncRuntime();
    const { value } = props;
    const vs : any = await runStopify(() => runtime.$tooutput(value));
    return { value: vs };
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
