import { Howl, Howler } from 'howler';
const RUNTIME = require('./runtime.js');

export const juliet = "helllo world";

export function getBuffer(path: string):AudioBuffer {
    debugger;
    //@ts-ignore
    var audioCtx = AudioContext();
    var source;

    source = audioCtx.createBufferSource();
    //@ts-ignore
    var request = XMLHttpRequest();

    request.open('GET', path, true);

    request.responseType = 'arraybuffer';
    return RUNTIME.pauseStack(function (restarter) {
        request.onload = function () {
            var audioData = request.response;
            
            audioCtx.decodeAudioData(audioData, function (buffer) {
                source.buffer = buffer;
                source.connect(audioCtx.destination);
                source.loop = true;
                console.log(buffer.getChannelData(0));
                restarter.resume(buffer);
            },

                function (e) {
                    restarter.error(new Error("Error with decoding audio data"));
                });

        }

        request.send();

    })
}


function _makesound(sample_rate: number, data_array: number[]): Sound {
    const sound = {
        '$brand': "sound",
        'sample-rate': sample_rate,
        'data-array': data_array,
        'play': () => playSound()
    }
    return sound;
}

export function playSound() { console.log("Playing sound")}

export function _urlSound(path: string): Sound {
    var buffer = getBuffer(path);
    //var data_array = Array.from(buffer.getChannelData(0));   
    //var sample_rate = buffer.sampleRate;
    //console.log("Length:" + data_array.length);
    //console.log(data_array.slice(0,500));
    const sample_rate2 = 10;
    const data_array2 = [3,-1,10,4,10,2,1,-5,9,2,3,4,5,6,4,3,2,4,3,5,6,4,3,5,7,4,3,2,3,5,10];
    return _makesound(sample_rate2, data_array2);
}

interface Sound {
    '$brand': string,
    'sample-rate': number,
    'data-array': number[],
    'play': () => any
}