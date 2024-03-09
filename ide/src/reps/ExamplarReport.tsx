
import React from 'react';
// TODO(joe): is this a bad import to have in the view? Should we have a more intermediate datatype like ChunkResults or no?
import { CompileAndRunResult } from '../control';
import { FaBug, FaBugSlash } from "react-icons/fa6";
import CodeEmbed from '../CodeEmbed';
import { CMEditor, parseLocation } from '../utils';
import { UninitializedEditor } from '../chunk';

type ExamplarResult = { success: boolean, result: CompileAndRunResult };

type ExamplarReportProps = {
    wheatResults: ExamplarResult[],
    chaffResults: ExamplarResult[],
    editor: UninitializedEditor | CMEditor
};
type ExamplarReportState = {};

function resultSummary(wheatResultArray: ExamplarResult[], chaffResultArray: ExamplarResult[]) {
  function numFailures(resultArray: any[]) {
    const fails = resultArray.filter((result) => !result.success);
    return fails.length;
  }
  const numWheats = wheatResultArray.length;
  const numChaffs = chaffResultArray.length;
  const wheatFails = numFailures(wheatResultArray);
  const chaffFails = numFailures(chaffResultArray);
  const wheatSuccs = numWheats - wheatFails;
  const chaffSuccs = numChaffs - chaffFails;
  let introMessage = '';
  let wheatMessage = '';
  let chaffMessage = '';
  //
  if (wheatFails === 0 && chaffSuccs === 0) {
    introMessage = 'Congratulations! Your tests are correct and comprehensive.';
  } else if (wheatFails === 0 && chaffSuccs >= 1) {
    introMessage = 'Your tests are correct but not comprehensive.';
  } else if (wheatFails > 0) {
    introMessage = 'Sorry. Your tests are incorrect.';
  }
  //
  if (wheatFails === 0 && numWheats === 1) {
    wheatMessage = 'The only wheat succeeded.';
  } else if (wheatFails === 1 && numWheats === 1) {
    wheatMessage = 'The only wheat failed.';
  } else if (wheatFails === 0) {
    wheatMessage = `All ${numWheats} wheats succeeded.`;
  } else if (wheatFails > 0 && wheatFails === numWheats) {
    wheatMessage = `All ${numWheats} wheats failed.`;
  } else if (wheatFails > 0) {
    wheatMessage = `Only ${wheatSuccs} out of ${numWheats} wheats succeeded.`;
  }
  //
  if (chaffFails === 1 && numChaffs === 1) {
    chaffMessage = 'You caught the only chaff.';
  } else if (chaffFails === 0 && numChaffs === 1) {
    chaffMessage = 'You didn\'t catch the only chaff.';
  } else if (chaffFails === 0) {
    chaffMessage = `You didn't catch any of the ${numChaffs} chaffs.`;
  } else if (chaffFails > 0 && chaffFails === numChaffs) {
    chaffMessage = `You caught all ${numChaffs} chaffs.`;
  } else if (chaffFails > 0) {
    chaffMessage = `You caught only ${chaffFails} out of ${numChaffs} chaffs.`;
  }
  //
  // eslint-disable-next-line
  return `${introMessage}\n${wheatMessage}\n${chaffMessage}`;
}

function missingBug() {
  return <span className="examplar-bug-icon missed">
    <FaBugSlash style={{margin: "auto"}} color="#111" size="2em"></FaBugSlash>
  </span>}
function caughtBug() {
  return <span className="examplar-bug-icon caught">
    <FaBug style={{margin: "auto"}} color="#111" size="2em"></FaBug>
  </span>
}
function chaffWidget(chaffResults: ExamplarResult[]) {
  return <div>
    {
      chaffResults.map(cr => {
        if(cr.success) { return missingBug(); }
        return caughtBug();
      })
    }
  </div>
}

function failingWheatTests(wheatResults: ExamplarResult[]) {
    const failResults = wheatResults.flatMap(wr => {
        if(wr.result.type !== 'run-result') { return []; }
        console.log("Wheat result: ", wr);
        const checks = wr.result.result.result.$checks;
        const failed = checks.filter((c : any) => c.success === false);
        if(failed.length === 0) { return []; }
        const failingLoc = failed[0].loc;
        return [failingLoc];
    });
    return failResults;
}

function firstFailingWheatTest(wheatResults : ExamplarResult[]) {
    const failResults = failingWheatTests(wheatResults);
    if(failResults.length === 0) {
        console.error("Tried to get failing location for successful wheat results");
        throw new Error("Tried to get failing location for successful wheat results");
    }
    console.log(failResults);
    return failResults[0];
}


function wheatFailureEmbed(location : any, editor : any) {
    return <CodeEmbed
        from={{ line: location['start-line'] - 1, ch: 0}}
        to={{ line: location['end-line'], ch: 999999 }}
        text={editor.getRange({ line: location['start-line'] - 1, ch: 0 }, { line: location['end-line'] - 1, ch: 999999 })}
        editor={editor}
        failure={{}}>
    </CodeEmbed>;
}

function showFirstWheatFailure(wheatResults : ExamplarResult[], editor : any) {
    const first = parseLocation(firstFailingWheatTest(wheatResults));
    console.log("wheatFailure ", first);
    return <div>This test is invalid (it did not match the behavior of a wheat):
        <div>{wheatFailureEmbed(first, editor)}</div>
    </div>;
}


export default class ExamplarReportWidget extends React.Component<ExamplarReportProps, ExamplarReportState> {
  render() {
    const { wheatResults, chaffResults, editor } = this.props;
    const failures = failingWheatTests(wheatResults);
    if(failures.length !== 0) {
        return <div>{showFirstWheatFailure(wheatResults, editor)}</div>
    }
    return (
      <div>{chaffWidget(chaffResults)}<p>{resultSummary(wheatResults, chaffResults)}</p></div>
    );
  }
}
