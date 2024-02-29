
import React from 'react';
// TODO(joe): is this a bad import to have in the view? Should we have a more intermediate datatype like ChunkResults or no?
import { CompileAndRunResult } from '../control';
import { FaBug, FaBugSlash } from "react-icons/fa6";


type ExamplarResult = { success: boolean, result: CompileAndRunResult };

type ExamplarReportProps = {
    wheatResults: ExamplarResult[],
    chaffResults: ExamplarResult[]
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
  return <span style={{
    width: "3em",
    height: "3em",
    display: "inline-flex",
    alignItems: "center",
    borderRadius: "1.5em",
    backgroundColor: "#ddd"}
  }>
    <FaBugSlash style={{margin: "auto"}} color="#111" size="2em" background-color="rgb(44, 139, 219)"></FaBugSlash>
  </span>}
function caughtBug() {
  return <span style={{
    width: "3em",
    height: "3em",
    display: "inline-flex",
    alignItems: "center",
    borderRadius: "1.5em",
    backgroundColor: "rgb(44, 139, 219)"}
  }>
    <FaBug style={{margin: "auto"}} color="#111" size="2em" background-color="rgb(44, 139, 219)"></FaBug>
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



export default class ExamplarReportWidget extends React.Component<ExamplarReportProps, ExamplarReportState> {
  render() {
    const { wheatResults, chaffResults } = this.props;
    return (
      <div>{chaffWidget(chaffResults)}<p>{resultSummary(wheatResults, chaffResults)}</p></div>
    );
  }
}
