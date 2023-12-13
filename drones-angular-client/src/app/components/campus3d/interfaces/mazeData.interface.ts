import Room from 'src/app/core/models/room.model';
import { MapCell } from './mapCell.enum';

export interface MazeFullConfig {
  maze: Maze;
  player: Player;
  ground: Ground;
  wall: Wall;
  passageWall: Wall;
  elevatorWall: Wall;
  doorWall: Wall;
  roomData?: (Room & { doorPosition?: [number, number] })[];
}

export default interface MazePartialConfig {
  maze: Maze;
  ground: Ground;
  wall: Wall;
  player: Player;
}

export interface MazeAndPlayerConfig {
  maze: Maze;
  player: Player;
}

export interface Base3dData {
  ground: Ground;
  wall: Wall;
  passageWall: Wall;
  elevatorWall: Wall;
  doorWall: Wall;
}

export interface Maze {
  buildingCode: string;
  floorNumber: number;
  size: Size;
  map: MapCell[][];
  exitLocations: ExitLocations;
}

export interface Size {
  width: number;
  depth: number;
}

export interface ExitLocations {
  passages: Passage[];
  elevators: Elevator[];
}
export interface ExitLocationEvent {
  type: 'passage' | 'elevator';
  details: Passage | Elevator;
  exitFloorNumber: number;
  exitBuildingCode: string;
}

export interface Passage {
  cellPosition: [number, number];
  entracePositionOffset: [number, number];
  destination: Destination;
}

export interface Destination {
  buildingCode: string;
  floorNumber: number;
}

export interface Elevator {
  cellPosition: [number, number];
  entracePositionOffset: [number, number];
  connectedFloorNumbers: number[];
}

export interface Player {
  initialPosition: [number, number];
  initialDirection: number;
}

export interface Ground {
  size: Size3D;
  segments: Segments;
  primaryColor: string;
  maps: TextureMaps;
  wrapS: number;
  wrapT: number;
  repeat: UVMap;
  magFilter: number;
  minFilter: number;
  secondaryColor: string;
}

export interface Wall {
  segments: Segments2D;
  primaryColor: string;
  maps: TextureMaps;
  wrapS: number;
  wrapT: number;
  repeat: UVMap;
  magFilter: number;
  minFilter: number;
  secondaryColor: string;
}

export interface Size3D {
  width: number;
  height: number;
  depth: number;
}

export interface Segments {
  width: number;
  height: number;
  depth: number;
}

export interface Segments2D {
  width: number;
  height: number;
}

export interface TextureMap {
  url: string;
  intensity?: number;
  scale?: number | Scale2D;
  bias?: number;
  rough?: number;
  type?: number;
}

export interface TextureMaps {
  color: TextureMap;
  ao: TextureMap;
  displacement: TextureMap;
  normal: TextureMap;
  bump: TextureMap;
  roughness: TextureMap;
}

export interface Scale2D {
  x: number;
  y: number;
}

export interface UVMap {
  u: number;
  v: number;
}

export interface Vector3 {
  x: number;
  y: number;
  z: number;
}
