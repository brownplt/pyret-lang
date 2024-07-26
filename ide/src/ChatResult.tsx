import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { UnControlled as ReactCM } from 'react-codemirror2';
import { State } from './state';

import {
    Chunk,
    ChunkResults,
    isInitializedEditor,
    UninitializedEditor,
} from './chunk';

import {
    isLocation,
    isTrace,
    isCheckResults,
} from './rhsObject';
import RHSObjectComponent from './RHSObjectComponent';
import FailureComponent from './FailureComponent';
import { CMEditor } from './utils';
import { FullCheckResults } from './FullCheckResults';
import TimestampContext from './TimestampContext';

type ChatResultProps = {
    editor: UninitializedEditor | CMEditor,
    results: ChunkResults,
    id: string,
    technicallyOutdated: boolean
}

class ChatResult extends React.Component<ChatResultProps, any> {

    render() {
        let chunkResultsPart = <></>;
        const { technicallyOutdated, editor: chunkEditor, results, id, } = this.props;
        if (results.status === 'failed' && isInitializedEditor(chunkEditor)) { // NOTE(joe): repeat check to satisfy tag check
            chunkResultsPart = (
                <div className="chat-result">
                    {results.failures.map((failure, i) => (
                        <div
                            // eslint-disable-next-line
                            key={i}
                            className="chatitor-rhs chatitor-rhs-error"
                            title={technicallyOutdated ? 'value might be changed by earlier definition changes' : ''}
                        >
                            <FailureComponent failure={failure} id={id} editor={chunkEditor} />
                        </div>
                    ))}
                </div>
            );
        } else if (results.status === 'succeeded') {
            const rhsObjects = results.objects;
            const partiallyOutdated = technicallyOutdated;
            // TODO(luna): more principled
            // const isDataDefinition = rhsObjects.filter((r) => !isLocation(r)).length === 0
            //         && rhsObjects.filter((r) => isLocation(r) && r.name.startsWith('is-')).length > 0;
            const shown = rhsObjects.filter((r) => (
                // location for function is mostly noise
                !(isLocation(r) && typeof r.value === 'function')
                // location for constant variant of datatype
                && !(isLocation(r) && typeof r.value === 'object' && '$name' in r.value && r.value.$name === r.name)
                // checks handled separately and grouped
                && !isCheckResults(r)
                // undefined shows up sometimes go figure
                && !(isTrace(r) && typeof r.value === 'undefined')));
            const values = shown.map((val) => (
                <RHSObjectComponent
                    editor={chunkEditor}
                    key={val.key ?? 'no key for val?'}
                    rhsObject={val}
                    isSelected={false}
                    className="chatitor-rhs"
                    title={partiallyOutdated ? 'value might be changed by earlier definition changes' : ''}
                />
            ));
            const checkResults = rhsObjects.filter(isCheckResults);
            const checkSummary = checkResults[0]?.renderedCheckBlockResults.renderedChecks.length > 0
                && (
                    <>
                    <FullCheckResults editor={chunkEditor} checks={checkResults[0].renderedCheckBlockResults}>
                    </FullCheckResults>
                    </>
                );
            chunkResultsPart = (
                <div className="chat-result">
                    {...values}
                    {checkSummary}
                </div>
            );
        }
        return <TimestampContext.Provider value={results.end}>
            { chunkResultsPart }
        </TimestampContext.Provider>;

    }

}

export default ChatResult;