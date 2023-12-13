export default class TimedExecutor {
  constructor() {
    this.lastExecuted = {};
  }

  // Executes the given function if the specified delay has passed since the last call
  execute(id, func, delay = 200) {
    let now = Date.now();

    // Check if the function has been executed before or if the delay has passed
    if (!this.lastExecuted[id] || now - this.lastExecuted[id] > delay) {
      func(); // Execute the function
      this.lastExecuted[id] = now; // Update the last executed time
    }
  }
}
