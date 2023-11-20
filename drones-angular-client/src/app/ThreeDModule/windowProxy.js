export default class WindowProxy {
  constructor(enableOverwrites, parentContainer) {
    return new Proxy(window, {
      get: (target, prop) => {
        // Bind functions to the original window object
        if (typeof target[prop] === 'function') {
          return target[prop].bind(window);
        }

        // Custom behavior for specific properties
        if (enableOverwrites) {
          if (prop === 'innerWidth') {    

            if (parentContainer) {
              return parentContainer.clientWidth;
            }
            
            return target.innerWidth;
          } else if (prop === 'innerHeight') {

            if (parentContainer) {
              return parentContainer.clientHeight;
            }

            return target.innerHeight;
          }
        }

        // Default behavior for all other properties/methods
        return target[prop];
      }
    });
  }
}

