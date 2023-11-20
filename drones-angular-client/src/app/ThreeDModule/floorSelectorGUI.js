import * as lil from 'lil-gui';

export class FloorSelectorGUI {
  constructor(container, options = {}) {
    this.container = container; // The DOM element where GUI will be placed
    this.gui = new lil.GUI({ autoPlace: false, ...options });

    // Style adjustments
    this.gui.domElement.style.position = 'absolute';
    this.gui.domElement.style.top = '50%';
    this.gui.domElement.style.left = '50%';
    this.gui.domElement.style.transform = 'translate(-50%, -50%)';

    this.container.appendChild(this.gui.domElement);
  }

  addLabel(text) {
    const label = { [text]: '(choose from the list below)' };
    const controller = this.gui.add(label, text);
    const labelElement = controller.domElement.previousElementSibling;

    if (labelElement) {
      labelElement.remove(); // Remove the text field only if it exists
    }

    controller.domElement.style.border = 'none'; // Optional: remove the border if any
    controller.domElement.style.pointerEvents = 'none'; // Make it non-interactive
  }

  addFloorButtons(floors, onFloorSelect) {
    floors.forEach((floor) => {
      this.gui.add({ [floor]: () => onFloorSelect(floor) }, floor);
    });
  }

  destroy() {
    if (this.gui.domElement.parentNode) {
      this.gui.domElement.parentNode.removeChild(this.gui.domElement);
    }

    this.gui.destroy();
    this.container = null;
    this.gui = null;
  }
}
