import { strict as assert } from 'assert';
import { setup } from './util/setup';
import * as TextMode from './util/TextMode';
import * as RHS from './util/RHS';
import * as Control from './util/Control';
import { ChunkState } from './util/ChunkMode';
import { KeyboardInput } from './util/Keyboard';
import { Key } from 'selenium-webdriver';


const TEST_TIMEOUT = 20000;
const COMPILER_TIMEOUT = 10000; // ms, for each compiler run
const STARTUP_TIMEOUT = 6000;


describe("Testing simple IDE programs", () => {

  jest.setTimeout(TEST_TIMEOUT);

  let driver;
  let ideURL;
  let refreshPagePerTest;
  let chunkState: ChunkState;
  let keyboard: KeyboardInput;

  beforeEach(() => {
    let setupResult = setup();
    driver = setupResult.driver;
    ideURL = setupResult.ideURL;
    refreshPagePerTest = setupResult.refreshPagePerTest;
    chunkState = new ChunkState(driver);
    keyboard = new KeyboardInput(driver);
    return driver.get(ideURL);
  });

  afterEach(() => {
    // return driver.quit();
  });

  test("scratch", async function(done) {
    await chunkState.appendToChunk(0, "FOO\n");
    await chunkState.appendToChunk(0, "BAR");
    await chunkState.appendToChunk(1, "BAZ");
    await chunkState.appendToChunk(0, "BEE");

    await keyboard.arrowDown();
    await keyboard.keySequence("QUX");
    await keyboard.arrowUp();
    await keyboard.arrowLeft(9);
    await keyboard.keySequence("1337");
    await keyboard.arrowRight(9 + 4);
    await keyboard.enter();

    // await chunkState.insertChunkAfter(0, "BAR");
    // await chunkState.appendToChunk(0, "BAZ");
    await done();
  });
});
