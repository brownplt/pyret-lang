import React from "react";
import PropTypes from "prop-types";
// import styled from "@emotion/styled";

import { RangeDescriptorPropTypes } from "./rep-serialization-core-types-proptypes";
import { Ell, RepBaseText, propLabelColor, Truncator } from "./value-summary";

import { truncateString } from "../shared/truncate-string";
// import { isValidIdentifier } from "../../shared/utils/is-valid-js-identifier";

// rule out a few things right away with a quick regex:
// cannot contain <space> ; = :
const forbiddenChars = / |;|=|:|@/;
// Above and below are from iodide project shared/utils
export function isValidIdentifier(identifier) {
  if (identifier.search(forbiddenChars) > -1) {
    return false;
  }
  try {
    // (luna): lol isn't this a security vulnerability?
    eval(`var ${identifier};`); // eslint-disable-line no-eval
    return true;
  } catch (err) {
    return false;
  }
}

const PathLabelText = RepBaseText;/*styled(RepBaseText)`
  color: ${propLabelColor};
`*/;

const isNumericIndexString = s => s && Number(s) >= 0 && s !== "Infinity";

const MAX_TINY_PATH_LABEL_LENGTH = 20;
const TINY_PATH_LABEL_TRUNCATION_LENGTH = 14;

const TerminalPathLabelRep = ({ pathLabel, tiny, mappingDelim }) => {
  const quote =
    isValidIdentifier(pathLabel) || isNumericIndexString(pathLabel) ? "" : '"';
  const { stringValue, isTruncated } = tiny
    ? truncateString(
        pathLabel,
        MAX_TINY_PATH_LABEL_LENGTH,
        TINY_PATH_LABEL_TRUNCATION_LENGTH
      )
    : { stringValue: pathLabel, isTruncated: false };

  return (
    <>
      <PathLabelText>
        {quote}
        <Truncator string={stringValue} {...{ isTruncated }} />
        {quote}
        {mappingDelim || ":"}{" "}
      </PathLabelText>
    </>
  );
};
TerminalPathLabelRep.propTypes = {
  pathLabel: PropTypes.string.isRequired,
  tiny: PropTypes.bool,
  mappingDelim: PropTypes.string
};

export class PathLabelRep extends React.Component {
  static propTypes = {
    pathLabel: PropTypes.oneOfType([PropTypes.string, RangeDescriptorPropTypes])
      .isRequired,
    tiny: PropTypes.bool,
    mappingDelim: PropTypes.string
  };
  render() {
    const { pathLabel, tiny, mappingDelim } = this.props;

    if (typeof pathLabel === "string") {
      return <TerminalPathLabelRep {...{ pathLabel, tiny, mappingDelim }} />;
    }

    const { min, max } = pathLabel;
    return tiny ? (
      <Ell>⋯{max - min} more⋯</Ell>
    ) : (
      <PathLabelText>{`[${min} ⋯ ${max}]`}</PathLabelText>
    );
  }
}
