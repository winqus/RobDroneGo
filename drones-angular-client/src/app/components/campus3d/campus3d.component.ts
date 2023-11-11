import { Component, ElementRef, OnDestroy, OnInit, ViewChild } from '@angular/core';
import * as THREE from 'three';


@Component({
  selector: 'app-campus3d',
  templateUrl: './campus3d.component.html',
  styleUrls: ['./campus3d.component.css']
})
export class Campus3dComponent implements OnInit, OnDestroy {
  @ViewChild('canvasContainer', { static: true }) canvasContainer!: ElementRef;

  private scene!: THREE.Scene;
  private camera!: THREE.PerspectiveCamera;
  private renderer!: THREE.WebGLRenderer;
  private cube!: THREE.Mesh;

  ngOnInit(): void {
    this.initThree();
    this.animate();
    window.addEventListener('resize', this.onWindowResize.bind(this), false);
  }

  ngOnDestroy(): void {
    window.removeEventListener('resize', this.onWindowResize.bind(this));
    if (this.renderer) {
      this.renderer.dispose();
    }
  }

  private initThree(): void {
    this.scene = new THREE.Scene();
    this.camera = new THREE.PerspectiveCamera(75, this.getAspectRatio(), 0.1, 1000);
    this.camera.position.z = 5;

    this.renderer = new THREE.WebGLRenderer();
    const containerBounds = this.canvasContainer.nativeElement.getBoundingClientRect();
    this.renderer.setSize(containerBounds.width, containerBounds.height);
    this.canvasContainer.nativeElement.appendChild(this.renderer.domElement);

    const geometry = new THREE.BoxGeometry(1, 1, 1);
    const material = new THREE.MeshBasicMaterial({ color: 0x00ff00 });
    this.cube = new THREE.Mesh(geometry, material);
    this.scene.add(this.cube);
  }

  private animate(): void {
    requestAnimationFrame(() => this.animate());
    this.cube.rotation.x += 0.01;
    this.cube.rotation.y += 0.01;
    this.renderer.render(this.scene, this.camera);
  }

  private getAspectRatio(): number {
    const containerBounds = this.canvasContainer.nativeElement.getBoundingClientRect();
    return containerBounds.width / containerBounds.height;
  }

  private onWindowResize(): void {
    const aspectRatio = this.getAspectRatio();
    this.camera.aspect = aspectRatio;
    this.camera.updateProjectionMatrix();

    const containerBounds = this.canvasContainer.nativeElement.getBoundingClientRect();
    this.renderer.setSize(containerBounds.width, containerBounds.height);
  }
}
