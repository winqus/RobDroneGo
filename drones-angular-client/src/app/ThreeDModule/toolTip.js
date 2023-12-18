import * as THREE from 'three';
//import { cartesianToCell, cellToCartesian } from '../ThreeDModule/Helpers/coordinateUtils.js';
import ThrottledLogger from './throttledLogger.js';

export default class PickHelper {
  constructor(maze) {
    this.raycaster = new THREE.Raycaster();
    this.pickedObject = null;
    this.pickedObjectSavedColor = 0;
    this.tooltip = document.getElementById('tooltip');

    this.intersectionSphere = new THREE.Mesh(
      new THREE.SphereGeometry(0.1, 32, 32), // Small sphere
      new THREE.MeshBasicMaterial({ color: 0xff0000 }), // Red color for visibility
    );
    this.intersectionSphere.visible = false; // Start with the sphere hidden
    this.hideToolTip();
    this.firstTimeAdded = true;

    this.maze = maze;

    window.pickHelper = this; // FOR DEBUGGING
    this.throttledLogger = new ThrottledLogger();
    this.renderRaycast = false;
  }

  pick(normalizedPosition, scene, camera) {
    if (this.pickedObject) {
      this.pickedObject = undefined;
    }

    // cast a ray through the frustum
    this.raycaster.setFromCamera(normalizedPosition, camera);

    // Update the ray line for visualization
    const origin = camera.position;
    const direction = new THREE.Vector3();
    this.raycaster.ray.at(1, direction); // Get direction from ray
    direction.sub(origin).normalize();
    direction.multiplyScalar(100); // Length of the line
    direction.add(origin);

    const intersectedObjects = this.raycaster.intersectObjects(scene.children);
    const objectNamesToCheck = ['door', 'passage', 'elevator'];

    const filteredIntersectedObjects = intersectedObjects.filter((object) => objectNamesToCheck.includes(object.object.name));

    if (filteredIntersectedObjects.length) {
      // pick the first object. It's the closest one
      let intersection = filteredIntersectedObjects[0];

      // Retrieve the intersection point
      let intersectPoint = intersection.point;
      this.pickedObject = intersection.object;

      if (this.tooltip) {
        const cellPosition = this.maze.cartesianToCell(intersectPoint);
        let toolTipInfoAssigned = false;

        if (this.pickedObject.name === 'passage') {
          for (let i = 0; i < this.pickedObject.parent.mazeData.exitLocations.passages.length; i++) {
            const passageInfo = this.pickedObject.parent.mazeData.exitLocations.passages[i];
            const isAdjacentCell = Math.abs(cellPosition[0] - passageInfo.cellPosition[0]) <= 1 && Math.abs(cellPosition[1] - passageInfo.cellPosition[1]) <= 1;

            if (isAdjacentCell) {
              this.tooltip.innerHTML = `Passage to ${passageInfo.destination.buildingCode}${passageInfo.destination.floorNumber}`;
              toolTipInfoAssigned = true;
            }
          }
        } else if (this.pickedObject.name === 'elevator') {
          for (let i = 0; i < this.pickedObject.parent.mazeData.exitLocations.elevators.length; i++) {
            const elevatorInfo = this.pickedObject.parent.mazeData.exitLocations.elevators[i];
            const isAdjacentCell = Math.abs(cellPosition[0] - elevatorInfo.cellPosition[0]) <= 1 && Math.abs(cellPosition[1] - elevatorInfo.cellPosition[1]) <= 1;

            // Join the floors elevator shows, exclude the one you're in
            const excludedFloorNumber = this.pickedObject.parent.mazeData.floorNumber;
            const filteredFloorNumbers = elevatorInfo.connectedFloorNumbers.filter((floorNumber) => floorNumber !== excludedFloorNumber);
            const showElevatorInfo = filteredFloorNumbers.join(', ');

            if (isAdjacentCell) {
              this.tooltip.innerHTML = `Elevator to floors ${showElevatorInfo}`;
              toolTipInfoAssigned = true;
            }
          }
        } else if (this.pickedObject.name === 'door') {
          for (const roomInfo of this.pickedObject.parent.roomData) {
            if (!roomInfo.doorPosition) {
              continue;
            }

            const isAdjacentCell = Math.abs(cellPosition[1] - roomInfo.doorPosition[0]) <= 1 && Math.abs(cellPosition[0] - roomInfo.doorPosition[1]) <= 1;

            if (isAdjacentCell) {
              this.tooltip.innerHTML = `Room ${roomInfo.name} (<i>${roomInfo.category}</i>)`;
              toolTipInfoAssigned = true;
            }
          }
        } else {
          this.tooltip.innerHTML = 'error';
        }

        if (!toolTipInfoAssigned) {
          this.tooltip.innerHTML = '<i>no info</i>';
        }

        this.showToolTip();
      }

      if (this.renderRaycast === true) {
        if (this.firstTimeAdded) {
          scene.add(this.intersectionSphere);
          this.firstTimeAdded = false;
        }
        this.intersectionSphere.position.copy(intersectPoint); // Position the sphere at the intersection point
        this.intersectionSphere.visible = true; // Make the sphere visible
        // this.showToolTip();
      }
    } else {
      // Hide the sphere if there are no intersections
      this.intersectionSphere.visible = false;
      this.hideToolTip();
    }
  }
  showToolTip() {
    this.tooltip.style.display = 'block';
  }

  hideToolTip() {
    this.tooltip.style.display = 'none';
  }
}
