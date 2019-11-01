const RUNTIME = require('./runtime.js');
const jsnums = require("./js-numbers.js");

export function getBufferFromURL(path: string): AudioBuffer {
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

export function getArrayFromSound(sound: Sound): number[][] {
    return sound['data-array'];
}

export function makeSound(sample_rate: number, duration: number, data_array: number[][]): Sound {
    var fixed_data = new Array(data_array.length);
    var fixed_duration = jsnums.toFixnum(duration);
    for (var channel = 0; channel < data_array.length; channel++) {
        var channel_data = data_array[channel];
        var fixed_channel = new Array(channel_data.length);
        for (var j = 0; j < channel_data.length; j++ ) {
            fixed_channel[j] = jsnums.toFixnum(channel_data[j])
        }
    fixed_data[channel] = fixed_channel;   
    }
    const sound = {
        '$brand': "sound",
        'sample-rate': sample_rate,
        'duration': fixed_duration,
        'data-array': fixed_data
    }
    return sound;
}

export function getSoundFromURL(path: string): Sound {
    var buffer = getBufferFromURL(path);
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

export function getSoundFromAudioBuffer(buffer: AudioBuffer): Sound {
    var numChannel = buffer.numberOfChannels;
    var data_array = new Array(numChannel);
    for (var channel = 0; channel < numChannel; channel++) {
        var channel_array = Array.from(buffer.getChannelData(channel));
        data_array[channel] = channel_array;
    }
    var sample_rate = buffer.sampleRate;
    var duration = buffer.duration;
    console.log(data_array);
    return makeSound(sample_rate, duration, data_array);
}

export function createSound(channels: number, sample_rate: number, duration: number, data_array: number[][]) {
    //@ts-ignore
    var audioCtx = AudioContext();
    var myArrayBuffer = audioCtx.createBuffer(channels, duration, sample_rate);

    for (var channel = 0; channel < myArrayBuffer.numberOfChannels; channel++) {
        // This gives us the actual array that contains the data
        var nowBuffering = myArrayBuffer.getChannelData(channel);
        for (var i = 0; i < myArrayBuffer.length; i++) {
            nowBuffering[i] = data_array[channel][i];
        }
    }
    var source = audioCtx.createBufferSource();
    source.buffer = myArrayBuffer;
    source.connect(audioCtx.destination);
    return makeSound(sample_rate, duration, data_array);
}

function checkSampleRate(samples: Sound[]): boolean {
    var i = 0
    var sample_rate = samples[i]["sample-rate"];
    for (var i = 1; i < samples.length; i++) {
        if (samples[i]["sample-rate"] != sample_rate) {
            return false;
        }
    }
    return true;
}

export function overlay(samples: Sound[]): Sound {
    if (!checkSampleRate(samples)) {
        throw new Error("samples rates not equal for all samples");
    }
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
    for (var m = 0; m < maxChannels; m++) {
        mixed[m] = new Array(frameCount);
        for (var n = 0; n < frameCount; n++) {
            mixed[m][n] = 0;
        }
    }
    for (var j = 0; j < numSamples; j++) {
        for (var srcChannel = 0; srcChannel < samples[j]['data-array'].length; srcChannel++) {
            var _in = samples[j]['data-array'][srcChannel];
            for (var i = 0; i < _in.length; i++) {
                mixed[srcChannel][i] += _in[i];
            }
        }
    }
    return createSound(mixed.length, sample_rate, maxDuration, mixed);
}

export function concat(samples: Sound[]): Sound {
    if (!checkSampleRate(samples)) {
        throw new Error("samples rates not equal for all samples");
    }
    var numSamples = samples.length;
    var maxChannels = 0;
    var totalDuration = 0;
    var sample_rate = samples[0]['sample-rate'];
    for (var i = 0; i < numSamples; i++) {
        if (samples[i]['data-array'].length > maxChannels) {
            maxChannels = samples[i]['data-array'].length;
        }
        totalDuration += samples[i].duration;
    }
    var frameCount = totalDuration * sample_rate;
    var mixed = new Array(maxChannels);
    for (var m = 0; m < maxChannels; m++) {
        mixed[m] = new Array(frameCount);
        for (var n = 0; n < frameCount; n++) {
            mixed[m][n] = 0;
        }
    }
    var index = 0;
    for (var j = 0; j < numSamples; j++) {
        var fc = samples[j].duration * sample_rate;
        for (var srcChannel = 0; srcChannel < samples[j]['data-array'].length; srcChannel++) {
            var _in = samples[j]['data-array'][srcChannel];
            for (var i = 0; i < _in.length; i++) {
                mixed[srcChannel][index + i] = _in[i];
            }
        }
        index += fc;
    }
    return createSound(mixed.length, sample_rate, totalDuration, mixed);
}

export function setPlaybackSpeed(sample: Sound, rate: number): Sound {
    var sample_rate = sample['sample-rate'];
    var duration = sample.duration;
    var arr = sample['data-array'];
    var rate_fixed = jsnums.toFixnum(rate);
    var new_sample_rate = sample_rate * rate_fixed;
    var new_dur = duration/rate_fixed;
    return createSound(arr.length, new_sample_rate, new_dur, arr);
}

export function shorten(sample: Sound, start: number, end: number): Sound {
    var start_fixed = jsnums.toFixnum(start);
    var end_fixed = jsnums.toFixnum(end);
    if (end_fixed > sample.duration || start_fixed > sample.duration || end_fixed < start_fixed) {
        throw new Error("invalid start or end");
    }
    var sample_rate = sample['sample-rate'];
    var arr = sample['data-array'];
    var new_arr = new Array(arr.length);
    for (var channel = 0; channel < arr.length; channel++) {
        new_arr[channel] = arr[channel].slice(start_fixed * sample_rate, end_fixed * sample_rate);
        return makeSound(sample_rate, end_fixed - start_fixed, new_arr);
    }
}

export function denormalizeSound(audioBuffer: AudioBuffer): Sound {
    //@ts-ignore
    var audioCtx = AudioContext();
    var convolver = audioCtx.createConvolver();
    
    convolver.normalize = false;
    convolver.buffer = audioBuffer;
    
    return getSoundFromAudioBuffer(convolver.buffer);
}


interface Sound {
    '$brand': string,
    'sample-rate': number,
    'duration': number,
    'data-array': number[][]
}