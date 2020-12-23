import * as Selenium from 'selenium-webdriver';
import {
  WebElement,
  By,
  Key,
} from 'selenium-webdriver';

export class ChunkState {
  num_chunks: number;
  driver: any;

  // Assumes starting
  constructor(driver, init?: { num_chunks: number}) {
    this.driver = driver;

    if (init) {
      this.num_chunks = init.num_chunks;
    } else {
      this.num_chunks = 1;
    }
  }

  async appendToChunk(chunkIndex: number, input: string) {
    let currChunk = await this.getChunk(chunkIndex);

    let codeLines = await currChunk.findElements(By.className("CodeMirror-line"));
    let codeLine = codeLines.pop();
    await codeLine.click();

    let inputArea = await currChunk.findElement(By.css("textarea"));

    await inputArea.sendKeys(input);
  }

  async selectLocalLine(chunkIndex: number, line: number) {
    let currChunk = await this.getChunk(chunkIndex);

    let codeLines = await currChunk.findElements(By.className("CodeMirror-line"));
    let codeLine = codeLines.pop();
    await codeLine.click();

    let inputArea = await currChunk.findElement(By.css("textarea"));
    // await inputArea.sendKeys(input);
    return inputArea;
  }

  async getChunk(chunkIndex: number) {
    let chunkList = await this.driver.findElements(By.className("CodeMirror"));

    if (chunkIndex >= chunkList.length) {
      throw new Error(`Attempting to append to chunk ${chunkIndex} but there are only ${chunkList.length} chunks`);
    }

    return chunkList[chunkIndex];
  }

  async keySequence(input: string) {
    const actions = this.driver.actions({async: true});
    actions.sendKeys(input);
    await actions.perform();
  }

  async keyN(k: any, n?: number) {
    const actions = this.driver.actions({async: true});
    const kb = actions.keyboard();

    n = (n === undefined) ? 1 : n;

    for (let i = 0; i < n; i++) {
      actions.keyDown(k).pause(10, kb).keyUp(k);
    }

    await actions.perform();
  }

  async arrowDown(n?: number) {
    await this.keyN(Key.ARROW_DOWN, n);
  }

  async arrowUp(n?: number) {
    await this.keyN(Key.ARROW_UP, n);
  }

  async arrowLeft(n?: number) {
    await this.keyN(Key.ARROW_LEFT, n);
  }

  async arrowRight(n?: number) {
    await this.keyN(Key.ARROW_RIGHT, n);
  }

  async enter(n?: number) {
    await this.keyN(Key.ENTER, n);
  }
}
