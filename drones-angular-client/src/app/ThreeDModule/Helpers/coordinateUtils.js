import * as THREE from 'three';

// Convert cell [row, column] coordinates to cartesian (x, y, z) coordinates
export function cellToCartesian(position, halfSize, scale) {
  return new THREE.Vector3((position[1] - halfSize.width + 0.5) * scale.x, 0.0, (position[0] - halfSize.depth + 0.5) * scale.z);
}

// Convert cartesian (x, y, z) coordinates to cell [row, column] coordinates
export function cartesianToCell(position, halfSize, scale) {
  return [Math.floor(position.z / scale.z + halfSize.depth), Math.floor(position.x / scale.x + halfSize.width)];
}
