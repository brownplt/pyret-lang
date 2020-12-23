import { strict as assert } from 'assert';
import { setup } from './util/setup';
import * as TextMode from './util/TextMode';
import * as RHS from './util/RHS';
import * as Control from './util/Control';
import { ChunkState } from './util/ChunkMode';
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

  beforeEach(() => {
    let setupResult = setup();
    driver = setupResult.driver;
    ideURL = setupResult.ideURL;
    refreshPagePerTest = setupResult.refreshPagePerTest;
    chunkState = new ChunkState(driver);
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

    await chunkState.arrowDown();
    await chunkState.keySequence("QUX");
    await chunkState.arrowUp();
    await chunkState.arrowLeft(9);
    await chunkState.keySequence("1337");
    await chunkState.arrowRight(9 + 4);
    await chunkState.enter();

    // await chunkState.insertChunkAfter(0, "BAR");
    // await chunkState.appendToChunk(0, "BAZ");
    await done();
  });
});
