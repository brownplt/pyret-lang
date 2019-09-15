import { Howl, Howler } from 'howler';
const RUNTIME = require('./runtime.js');

export const juliet = "helllo world";

export function getArray(path: string) {
    debugger;
    //@ts-ignore
    var audioCtx = AudioContext();
    var source;

    source = audioCtx.createBufferSource();
    //@ts-ignore
    var request = XMLHttpRequest();

    request.open('GET', path, true);

    request.responseType = 'arraybuffer';
    return RUNTIME.pauseStack(function (restarter){
        request.onload = function () {
            var audioData = request.response;

            audioCtx.decodeAudioData(audioData, function (buffer) {
                source.buffer = buffer;

                source.connect(audioCtx.destination);
                source.loop = true;
                console.log(buffer.getChannelData(0));
                restarter.resume(buffer.getChannelData(0));
            },

                function (e) {
                    restarter.error(new Error("Error with decoding audio data"));
                });

        }

        request.send();
        
    })
}

export function makesound(path: string) {
    return {
        "$brand": "sound",
        "play": () => { },
        "getArray": () => getArray(path)
    };
}