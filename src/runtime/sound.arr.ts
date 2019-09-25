import { Howl, Howler } from 'howler';
const RUNTIME = require('./runtime.js');

export const juliet = "helllo world";

export function getBuffer(path: string): AudioBuffer {
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

export function makeSound(sample_rate: number, duration: number, data_array: number[][]): Sound {
    const sound = {
        '$brand': "sound",
        'sample-rate': sample_rate,
        'duration': duration,
        'data-array': data_array,
        'play': () => playSound(sound)
    }
    return sound;
}

export function playSound(sound: Sound) {
    //@ts-ignore
    var audioCtx = AudioContext();
    var source;
    source = audioCtx.createBufferSource();
    var frameCount = sound.duration*sound['data-array'][0].length
    source.buffer = audioCtx.createBuffer(sound['data-array'].length,frameCount,sound['sample-rate']);
    source.connect(audioCtx.destination);
    source.start();
}

export function urlSound(path: string): Sound {
    var buffer = getBuffer(path);
    var numChannel = buffer.numberOfChannels;
    var data_array = new Array(numChannel);
    for (var channel = 0; channel < numChannel; channel++) {
        var channel_array = Array.from(buffer.getChannelData(channel));
        data_array[channel] = channel_array;
    }
    var sample_rate = buffer.sampleRate;
    var duration = buffer.duration;
    return makeSound(sample_rate, duration, data_array);
}

export function overlay(samples: Sound[]): Sound {
    var numSamples = samples.length;
    var maxChannels = 0;
    var maxDuration = 0;
    var sample_rate = samples[0]['sample-rate'];
    for (var i = 0; i < numSamples; i++) {
        if (samples[i]['data-array'].length > maxChannels) {
            maxChannels = samples[i]['data-array'].length;
        }
        if (samples[i].duration > maxDuration) {
            maxDuration = samples[i].duration;
        }
    }
    var frameCount = maxDuration * sample_rate;
    var mixed = new Array(maxChannels);
    console.log(mixed);
    console.log(frameCount);
    for (var m = 0; m < maxChannels; m++) {
        mixed[m] = new Array(frameCount);
        for (var n = 0; n < frameCount; n++) {
            mixed[m][n] = 0;
        }
    }
    console.log(mixed);
    for (var j = 0; j < numSamples; j++) {
        for (var srcChannel = 0; srcChannel < samples[j]['data-array'].length; srcChannel++) {
            var _in = samples[j]['data-array'][srcChannel];
            for (var i = 0; i < _in.length; i++) {
                console.log(mixed);
                mixed[srcChannel][i] += _in[i];
                console.log(mixed);
            }
        }
    }
    return makeSound(sample_rate, maxDuration, mixed);
}

interface Sound {
    '$brand': string,
    'sample-rate': number,
    'duration': number,
    'data-array': number[][],
    'play': () => any
}