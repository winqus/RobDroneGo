class AssetLoadingManager {
  constructor() {
    this.loadingCount = 0;
    this.onAllLoadedCallbacks = [];
    this.hasStartedLoading = false; // Indicates if any loading has started
  }

  startLoading() {
    this.loadingCount++;
    this.hasStartedLoading = true;
  }

  finishedLoading() {
    this.loadingCount--;
    if (this.loadingCount === 0 && this.hasStartedLoading) {
      this.onAllLoadedCallbacks.forEach((callback) => callback());
    }
  }

  onAllLoaded(callback) {
    if (this.loadingCount === 0 && this.hasStartedLoading) {
      callback();
    } else {
      this.onAllLoadedCallbacks.push(callback);
    }
  }
}

const globalAssetManager = new AssetLoadingManager();
export default globalAssetManager;
