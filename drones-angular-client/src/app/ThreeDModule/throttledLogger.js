export default class ThrottledLogger {
  constructor(defaultInterval = 1000) {
    this.defaultInterval = defaultInterval;
    this.lastPrintTimes = {};
  }

  canPrint(id, interval) {
    const currentTime = Date.now();
    if (!this.lastPrintTimes[id] || currentTime - this.lastPrintTimes[id] > interval) {
      this.lastPrintTimes[id] = currentTime;
      return true;
    }
    return false;
  }

  log(id, message, object, interval = this.defaultInterval) {
    if (this.canPrint(id, interval)) {
      if (object) {
        console.log(message, object);
      } else {
        console.log(message);
      }
    }
  }

  warn(id, message, object, interval = this.defaultInterval) {
    if (this.canPrint(id, interval)) {
      if (object) {
        console.warn(message, object);
      } else {
        console.warn(message);
      }
    }
  }

  error(id, message, object, interval = this.defaultInterval) {
    if (this.canPrint(id, interval)) {
      if (object) {
        console.error(message, object);
      } else {
        console.error(message);
      }
    }
  }
}
