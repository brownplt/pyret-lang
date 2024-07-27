import React from 'react';
import type * as R from '../../../src/runtime/runtime';
import { 
  runStopify, 
  getAsyncRuntime, 
  getAsyncModuleByName,
  builtinPath
} from '../runner';
import ValueSkeletonWidget from './ValueSkeletonWidget';
import type * as C from '../../../src/runtime-arr/color.arr';
import ColorWidget from './ColorWidget';

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

  componentDidMount() {
    const runtime = getAsyncRuntime() as typeof R;
    const { value } = this.props;
    const vsp = runStopify(() => runtime.$tooutput(value));
    vsp.then((vs) => this.setState({ value: vs.value }));
  }

  shouldComponentUpdate(nextProps : RVWOProps, nextState : RVWOState) {
    const result = nextProps.value !== this.props.value || nextState.value !== this.state.value;
    return result;
  }

  componentDidUpdate(prevProps : RVWOProps, prevState : RVWOState) {
    if (prevProps.value !== this.props.value) {
      console.log('Updating state in RenderedValueWithOutput', prevProps, this.props, prevState, this.state);
      const runtime = getAsyncRuntime() as typeof R;
      const { value } = this.props;
      const vsp = runStopify(() => runtime.$tooutput(value));
      vsp.then((vs) => this.setState({ value: vs.value }));
    }
  }

  render() {
    if (this.state === null) {
      return '...';
    }
    const { value } = this.state;
    const Color = getAsyncModuleByName(builtinPath('color.arr')) as typeof C;
    if (Color['is-color'](this.props.value)) {
      return <ColorWidget color={this.props.value} skeleton={value} />
    } else {
      return <ValueSkeletonWidget value={value} />;
    }
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
