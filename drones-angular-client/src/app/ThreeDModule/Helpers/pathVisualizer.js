import * as THREE from 'three';

export default class PathVisualizer {
  constructor(scene, radius = 0.05, color = 0xff0000) {
    this.scene = scene;
    this.radius = radius;
    this.color = color;
    this.spheres = [];
  }

  createSphereAt(position, radius = this.radius, color = this.color) {
    const geometry = new THREE.SphereGeometry(radius, 32, 32);
    const material = new THREE.MeshBasicMaterial({ color: color });
    const sphere = new THREE.Mesh(geometry, material);
    sphere.position.copy(position);
    return sphere;
  }

  visualizePath(pathPoints, radius = this.radius, color = this.color) {
    // Clear existing spheres
    this.spheres.forEach((sphere) => this.scene.remove(sphere));
    this.spheres = [];

    // Create new spheres
    pathPoints.forEach((point) => {
      const sphere = this.createSphereAt(point, radius, color);
      this.scene.add(sphere);
      this.spheres.push(sphere);
    });
  }

  cleanUp() {
    this.spheres.forEach((sphere) => this.scene.remove(sphere));
    this.spheres = [];
  }
}
