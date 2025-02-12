
import React from 'react';
// TODO(joe): is this a bad import to have in the view? Should we have a more intermediate datatype like ChunkResults or no?
import { CompileAndRunResult } from '../control';
import { FaBug, FaBugSlash } from "react-icons/fa6";
import CodeEmbed from '../CodeEmbed';
import { CMEditor, parseLocation } from '../utils';
import { UninitializedEditor } from '../chunk';
import FailureComponent from '../FailureComponent';
import { RenderedCheckResultsAndSummary } from '../../../src/runtime/checker';
import { Failure } from '../failure';

type ExamplarResult = { success: boolean, result: CompileAndRunResult };

type ExamplarReportProps = {
    wheatResults: ExamplarResult[],
    chaffResults: ExamplarResult[],
    editor: UninitializedEditor | CMEditor,
    hintMessage: string,
    qtmVariations: number
};
type ExamplarReportState = {};

function resultSummary(wheatResultArray: ExamplarResult[], chaffResultArray: ExamplarResult[], hintMessage: string, qtmVariations: number) {
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
  let qtmMessage = '';
  let introMessage = '';
  let wheatMessage = '';
  let chaffMessage = '';
  //
  if (numWheats === 0) {
    introMessage = 'There are no wheats.';
  } else if (wheatFails === 0) {
    if (numChaffs === 0) {
      introMessage = 'Your tests passed but there are no chaffs.';
    } else if (chaffSuccs === 0) {
      introMessage = 'Congratulations! Your tests are correct and comprehensive.';
    } else {
      introMessage = 'Your tests are correct but not comprehensive.';
    }
  } else {
    introMessage = 'Sorry, your tests are incorrect.';
  }
  //
  if (numWheats > 0 && wheatFails === 0 && numChaffs > 0) {
    // chaffMessage is set only if there's
    // at least one wheat, no wheat failures, and at least one chaff
    if (chaffSuccs === 0) {
      if (numChaffs === 1) {
        chaffMessage = 'Your test caught the only chaff.';
      } else {
        chaffMessage = `Your test caught all ${numChaffs} chaffs.`;
      }
    } else {
      if (chaffSuccs === numChaffs) {
        if (numChaffs === 1) {
          chaffMessage = 'Your didn\'t catch the only chaff.';
        } else {
          chaffMessage = `Your didn\'t catch any of the ${numChaffs} chaffs.`;
        }
      } else {
        chaffMessage = `Your tests caught only ${chaffFails} of the ${numChaffs} chaffs.`;
      }
    }
  }
  //
  // eslint-disable-next-line
  if (qtmVariations >= 0) {
    qtmMessage = `Quartermaster: ${qtmVariations} variants of input/output found. `;
  }
  if (introMessage !== '') {
    if (wheatMessage !== '' || chaffMessage !== '' || hintMessage !== '') {
      introMessage += ' ';
    }
  }
  if (wheatMessage !== '') {
    if (chaffMessage !== '' || hintMessage !== '') {
      wheatMessage += ' ';
    }
  }
  if (chaffMessage !== '') {
    if (hintMessage !== '') {
      chaffMessage += ' ';
    }
  }
  if (hintMessage !== '') {
    hintMessage += ' ';
  }
  return `${qtmMessage}${introMessage}${wheatMessage}${chaffMessage}${hintMessage}`;
}

function missingBug() {
  return <span className="examplar-bug-icon missed">
    <FaBug style={{margin: "auto"}} color="#111" size="2em"></FaBug>
  </span>}
function caughtBug() {
  return <span className="examplar-bug-icon caught">
    <FaBugSlash style={{margin: "auto"}} color="#111" size="2em"></FaBugSlash>
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
        // fixme: check that .$renderedChecks exists
        if (!wr.result.result.result.$renderedChecks) { return []; }
        const checks = wr.result.result.result.$renderedChecks as RenderedCheckResultsAndSummary;
        // fixme: check that .renderedChecks exists
        if (!checks.renderedChecks) { return []; }
        const failed = checks.renderedChecks.flatMap((c) => c.testResults.filter((tr) => tr.$name !== 'success'));
        if(failed.length === 0) { return []; }
        return [failed[0].rendered];
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


function showFirstWheatFailure(wheatResults : ExamplarResult[], hintMessage: string, editor : any) {
    const firstFail = firstFailingWheatTest(wheatResults);
    console.log("wheatFailure ", firstFail);
    return <div>This test is invalid (it did not match the behavior of a wheat):
        <div>{hintMessage}</div>
        <FailureComponent failure={firstFail as Failure}/>
    </div>;
}


export default class ExamplarReportWidget extends React.Component<ExamplarReportProps, ExamplarReportState> {
  render() {
    const { wheatResults, chaffResults, hintMessage, qtmVariations, editor } = this.props;
    const failures = failingWheatTests(wheatResults);
    if(failures.length !== 0) {
        return <div>{showFirstWheatFailure(wheatResults, hintMessage, editor)}</div>
    }
    return (
      <div>{chaffWidget(chaffResults)}<p>{resultSummary(wheatResults, chaffResults, hintMessage, qtmVariations)}</p></div>
    );
  }
}
