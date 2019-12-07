const RUNTIME = require('./runtime.js');
const jsnums = require("./js-numbers.js");
const toneMap = {
    B8:7902.133,
    A8:7040,
    G8:6271.927,
    F8:5587.652,
    E8:5274.041,
    D8:4698.636,
    C8:4186.009,
    B7:3951.066,
    A7:3520,
    G7:3135.963,
    F7:2793.826,
    E7:2637.02,
    D7:2349.318,
    C7:2093.005,
    B6:1975.533,
    A6:1760,
    G6:1567.982,
    F6:1396.913,
    E6:1318.51,
    D6:1174.659,
    C6:1046.502,
    B5:987.7666,
    A5:880,
    G5:783.9909,
    F5:698.4565,
    E5:659.2551,
    D5:587.3295,
    C5:523.2511,
    B4:493.8833,
    A4:440,
    G4:391.9954,
    F4:349.2282,
    E4:329.6276,
    D4:293.6648,
    C4:261.6256,
    B3:246.9417,
    A3:220,
    G3:195.9977,
    F3:174.6141,
    E3:164.8138,
    D3:146.8324,
    C3:130.8128,
    B2:123.4708,
    A2:110,
    G2:97.99886,
    F2:87.30706,
    E2:82.40689,
    D2:73.41619,
    C2:65.40639,
    B1:61.73541,
    A1:55,
    G1:48.99943,
    F1:43.65353,
    E1:41.20344,
    D1:36.7081,
    C1:32.7032,
    B0:30.86771,
    A0:27.5,
    G0:24.49971,
    F0:21.82676,
    E0:20.60172,
    D0:18.35405,
    C0:16.3516
};
    

function getBufferFromURL(path: string): AudioBuffer {
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

function getArrayFromSound(sound: Sound): number[][] {
    return sound['data-array'];
}

function getChannelDataFromSound(sound: Sound, channel: number): number[] {
    return sound['data-array'][channel];
}

function checkDuration(data_array: number[][]): boolean {
    var dur = data_array[0].length;
    for (var i = 1; i < data_array.length; i++) {
        if (data_array[i].length != dur) {
            return false;
        }
    }
    return true;
}

function checkDataEntries(data_array: number[][]): boolean {
    for (var i = 0; i < data_array.length; i++) {
        for (var j = 0; j < data_array[i].length; j++) {
            if (data_array[i][j]>1 || data_array[i][j]<-1) {
                return false;
            }
        }    
    }
    return true;
}

function makeSingleChannelSound(sample_rate: number, data_array: number[]): Sound {
    if (typeof data_array[0] != "number") {
        throw new Error("Invalid data array! Use makeMultiChannelSound to create a multi-channel sound");
    }
    var arr = new Array(1);
    arr[0] = data_array;
    return makeSound(sample_rate, arr);
}

function makeMultiChannelSound(sample_rate: number, data_array: number[][]): Sound {
    if (typeof data_array[0] === "number") {
        throw new Error("Invalid data array! Use makeSingleChannelSound to create a single-channel sound");
    }
    return makeSound(sample_rate, data_array);
}

function makeSound(sample_rate: number, data_array: number[][]): Sound {
    if(data_array.length==0 || sample_rate==0)
        throw new Error("Parameters to sound are empty, hence - inavlid!");
    if(sample_rate<3000 || sample_rate>384000)
        throw new Error("Invalid sample rate! Choose a sample rate within the range [3000, 384000].");
    if(!checkDuration(data_array))
        throw new Error("Invalid data array! All channels in the data array should have the same length!");
    if(!checkDataEntries(data_array))
        throw new Error("Invalid data array! All entries in the data array should be within the range [-1, 1]!");
    var fixed_data = new Array(data_array.length);
    var fixed_sample_rate = jsnums.toFixnum(sample_rate);
    for (var channel = 0; channel < data_array.length; channel++) {
        var channel_data = data_array[channel];
        var fixed_channel = new Array(channel_data.length);
        for (var j = 0; j < channel_data.length; j++ ) {
            fixed_channel[j] = jsnums.toFixnum(channel_data[j])
        }
    fixed_data[channel] = fixed_channel;   
    }
    var fixed_duration = fixed_data[0].length/fixed_sample_rate;
    const sound = {
        '$brand': "sound",
        'sample-rate': fixed_sample_rate,
        'duration': fixed_duration,
        'data-array': fixed_data
    }
    return sound;
}

function getGDriveLink(path: string): string {
    var splitted = path.split("/"); 
    console.log(splitted);
    var id;
    for (var s in splitted) {
        console.log(s);
        if (splitted[s].includes("id=")) {
            console.log(splitted[s]);
            id = splitted[s].split("id=")[1];
            console.log(id);
        }
    }
    return "https://cors-anywhere.herokuapp.com/https://drive.google.com/uc?export=download&id="+id;
}

function getSoundFromURL(path: string): Sound {
    if (path.length==0) {
        throw new Error("URL is empty, hence invalid!!");
    }
    if (path.includes("drive.google.com")) {
        path = getGDriveLink(path);
    }
    console.log(path);
    var buffer = getBufferFromURL(path);
    var numChannel = buffer.numberOfChannels;
    var data_array = new Array(numChannel);
    for (var channel = 0; channel < numChannel; channel++) {
        var channel_array = Array.from(buffer.getChannelData(channel));
        data_array[channel] = channel_array;
    }
    var sample_rate = buffer.sampleRate;
    return makeSound(sample_rate, data_array);
}


function getSoundFromAudioBuffer(buffer: AudioBuffer): Sound {
    if (buffer.length==0) {
        throw new Error("Buffer is empty, hence invalid!!");
    }
    var numChannel = buffer.numberOfChannels;
    var data_array = new Array(numChannel);
    for (var channel = 0; channel < numChannel; channel++) {
        var channel_array = Array.from(buffer.getChannelData(channel));
        data_array[channel] = channel_array;
    }
    var sample_rate = buffer.sampleRate;
    console.log(data_array);
    return makeSound(sample_rate, data_array);
}

function createSound(channels: number, sample_rate: number, duration: number, data_array: number[][]) {
    if(sample_rate==0 || channels==0 || data_array.length==0) {
        throw new Error("One or more parameters to sound are empty, hence - invalid!!");
    }  
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
    return makeSound(sample_rate, data_array);
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

function overlay(samples: Sound[]): Sound {
    if(samples.length==0) {
        throw new Error("Set of sound samples are empty, hence - invalid!!");
    }
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
    var frameCount = Math.round(maxDuration*sample_rate);
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
    return makeSound(sample_rate, mixed);
}

function concat(samples: Sound[]): Sound {
    if(samples.length==0) {
        throw new Error("Set of sound samples are empty, hence - invalid!!");
    }
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
    var frameCount = Math.round(totalDuration*sample_rate);
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
    return makeSound(sample_rate, mixed);
}

function setPlaybackSpeed(sample: Sound, rate: number): Sound {
    var sample_rate = sample['sample-rate'];
    var arr = sample['data-array'];
    if(arr.length==0) {
        throw new Error("Sound sample is empty, hence - invalid!!");
    }
    var rate_fixed = jsnums.toFixnum(rate);
    var new_sample_rate = sample_rate * rate_fixed;
    return makeSound(new_sample_rate, arr);
}

function shorten(sample: Sound, start: number, end: number): Sound {
    var start_fixed = jsnums.toFixnum(start);
    var end_fixed = jsnums.toFixnum(end);
    if (end_fixed > sample.duration || start_fixed > sample.duration || end_fixed < start_fixed) {
        throw new Error("invalid start or end");
    }
    var sample_rate = sample['sample-rate'];
    var arr = sample['data-array'];
    if(arr.length==0) {
        throw new Error("Sound sample is empty, hence - invalid!!");
    }
    var new_arr = new Array(arr.length);
    for (var channel = 0; channel < arr.length; channel++) {
        new_arr[channel] = arr[channel].slice(Math.round(start_fixed*sample_rate), Math.round(end_fixed*sample_rate));
        return makeSound(sample_rate, new_arr);
    }
}

function denormalizeSound(sample: Sound): Sound {
    //@ts-ignore
    var audioCtx = AudioContext();
    var myArrayBuffer = audioCtx.createBuffer(sample['data-array'].length, sample.duration, sample['sample-rate']);

    for (var channel = 0; channel < myArrayBuffer.numberOfChannels; channel++) {
        // This gives us the actual array that contains the data
        var nowBuffering = myArrayBuffer.getChannelData(channel);
        for (var i = 0; i < myArrayBuffer.length; i++) {
            nowBuffering[i] = sample["data-array"][channel][i];
        }
    }
    var audioBuffer =  getBufferFromURL
    if(audioBuffer.length==0) {
        throw new Error("Buffer is empty, hence - invalid!!");
    }

    var convolver = audioCtx.createConvolver();
    
    convolver.normalize = false;
    convolver.buffer = audioBuffer;
    
    return getSoundFromAudioBuffer(convolver.buffer);
}

//https://teropa.info/blog/2016/08/04/sine-waves.html
function getSineWave(): Sound {
    const REAL_TIME_FREQUENCY = 440; 
    const ANGULAR_FREQUENCY = REAL_TIME_FREQUENCY * 2 * Math.PI;

    //@ts-ignore
    let audioContext = AudioContext();
    let myBuffer = audioContext.createBuffer(1, 88200, 44100);
    let myArray = myBuffer.getChannelData(0);
    for (let sampleNumber = 0 ; sampleNumber < 88200 ; sampleNumber++) {
        myArray[sampleNumber] = generateSample(sampleNumber);
    }

    function generateSample(sampleNumber) {
        let sampleTime = sampleNumber / 44100;
        let sampleAngle = sampleTime * ANGULAR_FREQUENCY;
        return Math.sin(sampleAngle);
    }

    return getSoundFromAudioBuffer(myBuffer);
}

//https://teropa.info/blog/2016/08/04/sine-waves.html
function getTone(key: string): Sound {
    const REAL_TIME_FREQUENCY = toneMap[key]; 
    console.log(REAL_TIME_FREQUENCY);
    if(REAL_TIME_FREQUENCY==null) {
        throw new Error("Given Octave doesn't exist! Please try a valid tone such as C8, A4 etc.");
    }
    const ANGULAR_FREQUENCY = REAL_TIME_FREQUENCY * 2 * Math.PI;

    //@ts-ignore
    let audioContext = AudioContext();
    let myBuffer = audioContext.createBuffer(1, 22050, 44100);
    let myArray = myBuffer.getChannelData(0);
    for (let sampleNumber = 0 ; sampleNumber < 22050 ; sampleNumber++) {
        myArray[sampleNumber] = generateSample(sampleNumber);
    }

    function generateSample(sampleNumber) {
        let sampleTime = sampleNumber / 44100;
        let sampleAngle = sampleTime * ANGULAR_FREQUENCY;
        return Math.sin(sampleAngle);
    }

    return getSoundFromAudioBuffer(myBuffer);
}


function getCosineWave(): Sound {
    const REAL_TIME_FREQUENCY = 440; 
    const ANGULAR_FREQUENCY = REAL_TIME_FREQUENCY * 2 * Math.PI;

    //@ts-ignore
    let audioContext = AudioContext();
    let myBuffer = audioContext.createBuffer(1, 88200, 44100);
    let myArray = myBuffer.getChannelData(0);
    for (let sampleNumber = 0 ; sampleNumber < 88200 ; sampleNumber++) {
        myArray[sampleNumber] = generateSample(sampleNumber);
    }

    function generateSample(sampleNumber) {
        let sampleTime = sampleNumber / 44100;
        let sampleAngle = sampleTime * ANGULAR_FREQUENCY;
        return Math.cos(sampleAngle);
    }

    return getSoundFromAudioBuffer(myBuffer);
}

function fade(sound: Sound): Sound {
    var sample_rate = sound['sample-rate'];
    var duration = sound['duration'];
    var k = Math.log(0.01)/(sample_rate*duration);
    var data_array = sound['data-array'];
    if(data_array.length==0) {
        throw new Error("Sound sample is empty, hence - invalid!!");
    }
    for (var channel = 0; channel < data_array.length; channel++) {
        for(var i=0; i < data_array[channel].length; i++) {
            data_array[channel][i] = data_array[channel][i] * Math.exp(i*k);
        }
    }
    return makeSound(sample_rate, data_array);
}

function removeVocals(sound: Sound): Sound {
    var sample_rate = sound['sample-rate'];
    var data_array = sound['data-array'];
    if(data_array.length==0) {
        throw new Error("Sound sample is empty, hence - invalid!!");
    }
    var channel1 = 0;
    var channel2 = 1;
    var diff = 0.0;
    for(var i=0; i < data_array[channel1].length; i++) {
        diff = Math.abs(data_array[channel1][i]/data_array[channel2][i]);
        if (diff > 0.7 && diff < 1.5) {
            data_array[channel1][i]=0;
            data_array[channel2][i]=0;
        }
    }
    return makeSound(sample_rate, data_array);
}

interface Sound {
    '$brand': string,
    'sample-rate': number,
    'duration': number,
    'data-array': number[][]
}

module.exports = {
    "get-array-from-sound": getArrayFromSound,
    "get-channel-data-from-sound": getChannelDataFromSound,
    "make-single-channel-sound": makeSingleChannelSound,
    "make-multi-channel-sound": makeMultiChannelSound,
    "get-sound-from-url": getSoundFromURL,
    "overlay": overlay,
    "concat": concat,
    "set-playback-speed": setPlaybackSpeed,
    "shorten": shorten,
    "denormalize-sound": denormalizeSound,
    "get-tone": getTone,
    "get-sine-wave": getSineWave,
    "get-cosine-wave": getCosineWave,
    "fade": fade,
    "remove-vocals": removeVocals
};