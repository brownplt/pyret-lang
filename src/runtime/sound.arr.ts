import {Howl, Howler} from 'howler';

export const juliet = "helllo world";


   
export function demo(path: string) {
    var sound = new Howl({
        src: [path]
    });
    sound.play();
    return true;
}

export function makesound(path: string) {
    return {
        "$brand": "sound",
        "play": () => demo(path)
    };
}