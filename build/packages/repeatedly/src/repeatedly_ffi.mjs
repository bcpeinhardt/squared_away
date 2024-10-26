class Repeater {
  constructor(delayMs, state, fun) {
    this.fun = fun;
    this.state = state;
    this.i = 0;
    this.intervalId = setInterval(() => {
      this.tick();
    }, delayMs);
  }

  tick() {
    this.state = this.fun(this.state, this.i);
    this.i += 1;
  }
}

export function call(delayMs, state, fun) {
  return new Repeater(delayMs, state, fun);
}

export function stop(repeater) {
  clearInterval(repeater.intervalId);
}

export function replace(repeater, fun) {
  repeater.fun = fun;
}

export function update_state(repeater, f) {
  repeater.state = f(repeater.state);
}
