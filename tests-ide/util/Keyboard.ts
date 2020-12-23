import { Key, } from 'selenium-webdriver';

export class KeyboardInput {
  driver: any;

  constructor(driver) {
    this.driver = driver;
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
