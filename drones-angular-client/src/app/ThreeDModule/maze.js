import * as THREE from 'three';
import { OBB } from 'three/addons/math/OBB.js';
import * as BufferGeometryUtils from 'three/addons/utils/BufferGeometryUtils.js';
import globalAssetManager from './assetLoadingManager.js';
import Ground from './ground.js';
import MapCell from './mapCell.js';
import { merge } from './merge.js';
import Wall from './wall.js';
/*
 * parameters = {
 *  url: String,
 *  designCredits: String,
 *  texturesCredits: String,
 *  scale: Vector3,
 *  helpersColor: Color
 * }
 */
/*
// Change need to be made to mapCell.enum.ts as well
const MapCell = {
  NoNorthWallNoWestWall: 0,
  NoNorthWallYesWestWall: 1,
  YesNorthWallNoWestWall: 2,
  YesNorthWallYesWestWall: 3,
  DoorNorth: 4,
  DoorWest: 5,
  PassageNorth: 6,
  PassageWest: 7,
  ElevatorNorth: 8,
  ElevatorSouth: 9,
  ElevatorWest: 10,
  ElevatorEast: 11,
};
*/
export default class Maze extends THREE.Group {
  constructor(parameters, customMazeLoaderParams) {
    super();
    merge(this, parameters);
    this.loaded = false;
    const { customMazeloaderService, mazeUrl, elevatorUrl, onLoadMaze, onMazeProgress, onMazeError } = customMazeLoaderParams;
    globalAssetManager.startLoading();

    this.onLoad = function (configData) {
      const normalMapTypes = [THREE.TangentSpaceNormalMap, THREE.ObjectSpaceNormalMap];
      const wrappingModes = [THREE.ClampToEdgeWrapping, THREE.RepeatWrapping, THREE.MirroredRepeatWrapping];
      const magnificationFilters = [THREE.NearestFilter, THREE.LinearFilter];
      const minificationFilters = [
        THREE.NearestFilter,
        THREE.NearestMipmapNearestFilter,
        THREE.NearestMipmapLinearFilter,
        THREE.LinearFilter,
        THREE.LinearMipmapNearestFilter,
        THREE.LinearMipmapLinearFilter,
      ];

      // Store the maze's size, map and exit location
      this.size = configData.maze.size;
      this.halfSize = { width: this.size.width / 2.0, depth: this.size.depth / 2.0 };
      this.map = configData.maze.map;
      this.mazeData = configData.maze;
      this.roomData = configData.roomData;

      // Old stuff left for compatibility (testability)
      if (configData.maze.exitLocation) {
        this.exitLocation = this.cellToCartesian(configData.maze.exitLocation);
      }

      // Adaption of new maze data structure
      if (configData.maze.exitLocations) {
        this.exitLocations = configData.maze.exitLocations;

        for (const passage of this.exitLocations.passages) {
          passage.entrancePosition3d = this.cellToCartesian([passage.cellPosition[0] + passage.entracePositionOffset[0], passage.cellPosition[1] + passage.entracePositionOffset[1]]);
        }

        for (const elevator of this.exitLocations.elevators) {
          elevator.entrancePosition3d = this.cellToCartesian([elevator.cellPosition[0] + elevator.entracePositionOffset[0], elevator.cellPosition[1] + elevator.entracePositionOffset[1]]);
        }
      }

      // Create the helpers
      this.helper = new THREE.Group();

      // Create the ground
      const ground = new Ground({
        size: new THREE.Vector3(configData.ground.size.width, configData.ground.size.height, configData.ground.size.depth),
        segments: new THREE.Vector3(configData.ground.segments.width, configData.ground.segments.height, configData.ground.segments.depth),
        materialParameters: {
          color: new THREE.Color(parseInt(configData.ground.primaryColor, 16)),
          mapUrl: configData.ground.maps.color.url,
          aoMapUrl: configData.ground.maps.ao.url,
          aoMapIntensity: configData.ground.maps.ao.intensity,
          displacementMapUrl: configData.ground.maps.displacement.url,
          displacementScale: configData.ground.maps.displacement.scale,
          displacementBias: configData.ground.maps.displacement.bias,
          normalMapUrl: configData.ground.maps.normal.url,
          normalMapType: normalMapTypes[configData.ground.maps.normal.type],
          normalScale: new THREE.Vector2(configData.ground.maps.normal.scale.x, configData.ground.maps.normal.scale.y),
          bumpMapUrl: configData.ground.maps.bump.url,
          bumpScale: configData.ground.maps.bump.scale,
          roughnessMapUrl: configData.ground.maps.roughness.url,
          roughness: configData.ground.maps.roughness.rough,
          wrapS: wrappingModes[configData.ground.wrapS],
          wrapT: wrappingModes[configData.ground.wrapT],
          repeat: new THREE.Vector2(configData.ground.repeat.u, configData.ground.repeat.v),
          magFilter: magnificationFilters[configData.ground.magFilter],
          minFilter: minificationFilters[configData.ground.minFilter],
        },
        secondaryColor: new THREE.Color(parseInt(configData.ground.secondaryColor, 16)),
      });
      this.add(ground);

      // Create a wall
      var wall = new Wall({
        groundHeight: configData.ground.size.height,
        segments: new THREE.Vector2(configData.wall.segments.width, configData.wall.segments.height),
        materialParameters: {
          color: new THREE.Color(parseInt(configData.wall.primaryColor, 16)),
          mapUrl: configData.wall.maps.color.url,
          aoMapUrl: configData.wall.maps.ao.url,
          aoMapIntensity: configData.wall.maps.ao.intensity,
          displacementMapUrl: configData.wall.maps.displacement.url,
          displacementScale: configData.wall.maps.displacement.scale,
          displacementBias: configData.wall.maps.displacement.bias,
          normalMapUrl: configData.wall.maps.normal.url,
          normalMapType: normalMapTypes[configData.wall.maps.normal.type],
          normalScale: new THREE.Vector2(configData.wall.maps.normal.scale.x, configData.wall.maps.normal.scale.y),
          bumpMapUrl: configData.wall.maps.bump.url,
          bumpScale: configData.wall.maps.bump.scale,
          roughnessMapUrl: configData.wall.maps.roughness.url,
          roughness: configData.wall.maps.roughness.rough,
          wrapS: wrappingModes[configData.wall.wrapS],
          wrapT: wrappingModes[configData.wall.wrapT],
          repeat: new THREE.Vector2(configData.wall.repeat.u, configData.wall.repeat.v),
          magFilter: magnificationFilters[configData.wall.magFilter],
          minFilter: minificationFilters[configData.wall.minFilter],
        },
        secondaryColor: new THREE.Color(parseInt(configData.wall.secondaryColor, 16)),
      });
      // Create a passage wall
      var passageWall;
      if (configData.passageWall) {
        passageWall = new Wall({
          groundHeight: configData.ground.size.height,
          segments: new THREE.Vector2(configData.passageWall.segments.width, configData.passageWall.segments.height),
          materialParameters: {
            color: new THREE.Color(parseInt(configData.passageWall.primaryColor, 16)),
            mapUrl: configData.passageWall.maps.color.url,
            aoMapUrl: configData.passageWall.maps.ao.url,
            aoMapIntensity: configData.passageWall.maps.ao.intensity,
            displacementMapUrl: configData.passageWall.maps.displacement.url,
            displacementScale: configData.passageWall.maps.displacement.scale,
            displacementBias: configData.passageWall.maps.displacement.bias,
            normalMapUrl: configData.passageWall.maps.normal.url,
            normalMapType: normalMapTypes[configData.passageWall.maps.normal.type],
            normalScale: new THREE.Vector2(configData.passageWall.maps.normal.scale.x, configData.passageWall.maps.normal.scale.y),
            bumpMapUrl: configData.passageWall.maps.bump.url,
            bumpScale: configData.passageWall.maps.bump.scale,
            roughnessMapUrl: configData.passageWall.maps.roughness.url,
            roughness: configData.passageWall.maps.roughness.rough,
            wrapS: wrappingModes[configData.passageWall.wrapS],
            wrapT: wrappingModes[configData.passageWall.wrapT],
            repeat: new THREE.Vector2(configData.passageWall.repeat.u, configData.passageWall.repeat.v),
            magFilter: magnificationFilters[configData.passageWall.magFilter],
            minFilter: minificationFilters[configData.passageWall.minFilter],
          },
          secondaryColor: new THREE.Color(parseInt(configData.passageWall.secondaryColor, 16)),
        });
      }

      // Create a elevator wall
      var elevatorWall;
      if (configData.elevatorWall) {
        elevatorWall = new Wall({
          groundHeight: configData.ground.size.height,
          segments: new THREE.Vector2(configData.elevatorWall.segments.width, configData.elevatorWall.segments.height),
          materialParameters: {
            color: new THREE.Color(parseInt(configData.elevatorWall.primaryColor, 16)),
            mapUrl: configData.elevatorWall.maps.color.url,
            aoMapUrl: configData.elevatorWall.maps.ao.url,
            aoMapIntensity: configData.elevatorWall.maps.ao.intensity,
            displacementMapUrl: configData.elevatorWall.maps.displacement.url,
            displacementScale: configData.elevatorWall.maps.displacement.scale,
            displacementBias: configData.elevatorWall.maps.displacement.bias,
            normalMapUrl: configData.elevatorWall.maps.normal.url,
            normalMapType: normalMapTypes[configData.elevatorWall.maps.normal.type],
            normalScale: new THREE.Vector2(configData.elevatorWall.maps.normal.scale.x, configData.elevatorWall.maps.normal.scale.y),
            bumpMapUrl: configData.elevatorWall.maps.bump.url,
            bumpScale: configData.elevatorWall.maps.bump.scale,
            roughnessMapUrl: configData.elevatorWall.maps.roughness.url,
            roughness: configData.elevatorWall.maps.roughness.rough,
            wrapS: wrappingModes[configData.elevatorWall.wrapS],
            wrapT: wrappingModes[configData.elevatorWall.wrapT],
            repeat: new THREE.Vector2(configData.elevatorWall.repeat.u, configData.elevatorWall.repeat.v),
            magFilter: magnificationFilters[configData.elevatorWall.magFilter],
            minFilter: minificationFilters[configData.elevatorWall.minFilter],
          },
          secondaryColor: new THREE.Color(parseInt(configData.elevatorWall.secondaryColor, 16)),
        });
      }
      // Create a elevator wall
      var doorWall;
      if (configData.doorWall) {
        doorWall = new Wall({
          groundHeight: configData.ground.size.height,
          segments: new THREE.Vector2(configData.doorWall.segments.width, configData.doorWall.segments.height),
          materialParameters: {
            color: new THREE.Color(parseInt(configData.doorWall.primaryColor, 16)),
            mapUrl: configData.doorWall.maps.color.url,
            aoMapUrl: configData.doorWall.maps.ao.url,
            aoMapIntensity: configData.doorWall.maps.ao.intensity,
            displacementMapUrl: configData.doorWall.maps.displacement.url,
            displacementScale: configData.doorWall.maps.displacement.scale,
            displacementBias: configData.doorWall.maps.displacement.bias,
            normalMapUrl: configData.doorWall.maps.normal.url,
            normalMapType: normalMapTypes[configData.doorWall.maps.normal.type],
            normalScale: new THREE.Vector2(configData.doorWall.maps.normal.scale.x, configData.doorWall.maps.normal.scale.y),
            bumpMapUrl: configData.doorWall.maps.bump.url,
            bumpScale: configData.doorWall.maps.bump.scale,
            roughnessMapUrl: configData.doorWall.maps.roughness.url,
            roughness: configData.doorWall.maps.roughness.rough,
            wrapS: wrappingModes[configData.doorWall.wrapS],
            wrapT: wrappingModes[configData.doorWall.wrapT],
            repeat: new THREE.Vector2(configData.doorWall.repeat.u, configData.doorWall.repeat.v),
            magFilter: magnificationFilters[configData.doorWall.magFilter],
            minFilter: minificationFilters[configData.doorWall.minFilter],
          },
          secondaryColor: new THREE.Color(parseInt(configData.doorWall.secondaryColor, 16)),
        });
      }

      // Build the maze
      /*
       *  this.map[][] | Description
       * --------------+----------------------
       *       0       | No North wall, No West wall
       *       1       | No North wall, Yes West wall
       *       2       | Yes North wall, No West wall
       *       3       | Yes North wall, Yes West wall
       *       4       | Door North
       *       5       | Door West
       *       6       | Passage North
       *       7       | Passage West
       *       8       | Elevator North
       *       9       | Elevator South
       *      10       | Elevator West
       *      11       | Elevator East
       */
      // TODO refactor build maze if needed
      let geometry;
      // let geometries = [];
      // geometries[0] = [];
      // geometries[1] = [];
      let regularGeometries = [[], []]; // For regular walls
      let doorGeometries = [[], []]; // For door walls
      let passageGeometries = [[], []]; // For passage walls
      let elevatorGeometries = [[], []]; // For elevator walls
      let doorGeometriesExist = false;
      let passageGeometriesExist = false;
      let elevatorGeometriesExist = false;
      this.aabb = [];
      for (let i = 0; i <= this.size.depth; i++) {
        // In order to represent the southmost walls, the map depth is one row greater than the actual maze depth
        this.aabb[i] = [];
        for (let j = 0; j <= this.size.width; j++) {
          // In order to represent the eastmost walls, the map width is one column greater than the actual maze width
          this.aabb[i][j] = [];

          // handling north walls
          if (this.map[i][j] == MapCell.YesNorthWallNoWestWall || this.map[i][j] == MapCell.YesNorthWallYesWestWall) {
            this.aabb[i][j][0] = new THREE.Box3();
            for (let k = 0; k < 2; k++) {
              let geometries = regularGeometries;
              geometry = wall.geometries[k].clone();
              geometry.applyMatrix4(new THREE.Matrix4().makeTranslation(j - this.halfSize.width + 0.5, 0.25, i - this.halfSize.depth));
              geometry.computeBoundingBox();
              geometry.boundingBox.applyMatrix4(new THREE.Matrix4().makeScale(this.scale.x, this.scale.y, this.scale.z));
              geometries[k].push(geometry);
              this.aabb[i][j][0].union(geometry.boundingBox);
            }
            this.helper.add(new THREE.Box3Helper(this.aabb[i][j][0], this.helpersColor));
          }

          // handling west walls
          if (this.map[i][j] == MapCell.NoNorthWallYesWestWall || this.map[i][j] == MapCell.YesNorthWallYesWestWall) {
            this.aabb[i][j][1] = new THREE.Box3();
            for (let k = 0; k < 2; k++) {
              let geometries = regularGeometries;
              geometry = wall.geometries[k].clone();
              geometry.applyMatrix4(new THREE.Matrix4().makeRotationY(Math.PI / 2.0));
              geometry.applyMatrix4(new THREE.Matrix4().makeTranslation(j - this.halfSize.width, 0.25, i - this.halfSize.depth + 0.5));
              geometry.computeBoundingBox();
              geometry.boundingBox.applyMatrix4(new THREE.Matrix4().makeScale(this.scale.x, this.scale.y, this.scale.z));
              geometries[k].push(geometry);
              this.aabb[i][j][1].union(geometry.boundingBox);
            }
            this.helper.add(new THREE.Box3Helper(this.aabb[i][j][1], this.helpersColor));
          }

          // Door walls
          if ([MapCell.DoorNorth, MapCell.DoorWest].includes(this.map[i][j])) {
            doorGeometriesExist = true;
            let orientation = new THREE.Matrix4();
            let position = new THREE.Vector3(j - this.halfSize.width, 0.25, i - this.halfSize.depth);
            if (this.map[i][j] === MapCell.DoorWest) {
              orientation.makeRotationY(Math.PI / 2.0);
              position.add(new THREE.Vector3(0, 0, 0.5));
            } else {
              // DoorNorth
              position.add(new THREE.Vector3(0.5, 0, 0));
            }

            this.aabb[i][j][2] = new THREE.Box3();
            for (let k = 0; k < 2; k++) {
              let geometry = doorWall.geometries[k].clone();
              geometry.applyMatrix4(orientation);
              geometry.applyMatrix4(new THREE.Matrix4().makeTranslation(position.x, position.y, position.z));
              geometry.computeBoundingBox();
              geometry.boundingBox.applyMatrix4(new THREE.Matrix4().makeScale(this.scale.x, this.scale.y, this.scale.z));
              doorGeometries[k].push(geometry);
              this.aabb[i][j][2].union(geometry.boundingBox);
            }
            this.helper.add(new THREE.Box3Helper(this.aabb[i][j][2], this.helpersColor));
          }

          // Passage walls
          if ([MapCell.PassageNorth, MapCell.PassageWest].includes(this.map[i][j])) {
            passageGeometriesExist = true;
            let orientation = new THREE.Matrix4();
            let position = new THREE.Vector3(j - this.halfSize.width, 0.25, i - this.halfSize.depth);
            if (this.map[i][j] === MapCell.PassageWest) {
              orientation.makeRotationY(Math.PI / 2.0);
              position.add(new THREE.Vector3(0, 0, 0.5));
            } else {
              // PassageNorth
              position.add(new THREE.Vector3(0.5, 0, 0));
            }

            this.aabb[i][j][2] = new THREE.Box3();
            for (let k = 0; k < 2; k++) {
              let geometry = passageWall.geometries[k].clone();
              geometry.applyMatrix4(orientation);
              geometry.applyMatrix4(new THREE.Matrix4().makeTranslation(position.x, position.y, position.z));
              geometry.computeBoundingBox();
              geometry.boundingBox.applyMatrix4(new THREE.Matrix4().makeScale(this.scale.x, this.scale.y, this.scale.z));
              passageGeometries[k].push(geometry);
              this.aabb[i][j][2].union(geometry.boundingBox);
            }
            this.helper.add(new THREE.Box3Helper(this.aabb[i][j][2], this.helpersColor));
          }

          // Elevator walls
          if ([MapCell.ElevatorNorth, MapCell.ElevatorSouth, MapCell.ElevatorWest, MapCell.ElevatorEast].includes(this.map[i][j])) {
            elevatorGeometriesExist = true;
            let orientation = new THREE.Matrix4();
            let position = new THREE.Vector3(j - this.halfSize.width, 0.25, i - this.halfSize.depth);
            if ([MapCell.ElevatorWest, MapCell.ElevatorEast].includes(this.map[i][j])) {
              orientation.makeRotationY(Math.PI / 2.0);
              position.add(new THREE.Vector3(0, 0, 0.5));
            } else {
              // ElevatorNorth or ElevatorSouth
              position.add(new THREE.Vector3(0.5, 0, 0));
            }

            this.aabb[i][j][2] = new THREE.Box3();
            for (let k = 0; k < 2; k++) {
              let geometry = elevatorWall.geometries[k].clone();
              geometry.applyMatrix4(orientation);
              geometry.applyMatrix4(new THREE.Matrix4().makeTranslation(position.x, position.y, position.z));
              geometry.computeBoundingBox();
              geometry.boundingBox.applyMatrix4(new THREE.Matrix4().makeScale(this.scale.x, this.scale.y, this.scale.z));
              elevatorGeometries[k].push(geometry);
              this.aabb[i][j][2].union(geometry.boundingBox);
            }
            this.helper.add(new THREE.Box3Helper(this.aabb[i][j][2], this.helpersColor));
          }
        }
      }

      // let mergedGeometry, mesh;
      // for (let i = 0; i < 2; i++) {
      //   mergedGeometry = BufferGeometryUtils.mergeGeometries(geometries[i], false);
      //   mesh = new THREE.Mesh(mergedGeometry, wall.materials[i]);
      //   mesh.castShadow = true;
      //   mesh.receiveShadow = true;
      //   this.add(mesh);
      // }

      // Merge and create meshes for each type of wall
      let mergedGeometry, mesh;

      // Regular walls
      for (let i = 0; i < 2; i++) {
        mergedGeometry = BufferGeometryUtils.mergeGeometries(regularGeometries[i], false);
        mesh = new THREE.Mesh(mergedGeometry, wall.materials[i]);
        mesh.castShadow = true;
        mesh.receiveShadow = true;
        mesh.name = 'wall';
        this.add(mesh);
      }

      // Door walls
      if (doorGeometriesExist) {
        for (let i = 0; i < 2; i++) {
          mergedGeometry = BufferGeometryUtils.mergeGeometries(doorGeometries[i], false);
          mesh = new THREE.Mesh(mergedGeometry, doorWall.materials[i]);
          mesh.castShadow = true;
          mesh.receiveShadow = true;
          mesh.name = 'door';
          this.add(mesh);
        }
      }

      // Passage walls
      if (passageGeometriesExist) {
        for (let i = 0; i < 2; i++) {
          mergedGeometry = BufferGeometryUtils.mergeGeometries(passageGeometries[i], false);
          mesh = new THREE.Mesh(mergedGeometry, passageWall.materials[i]);
          mesh.castShadow = true;
          mesh.receiveShadow = true;
          mesh.name = 'passage';
          this.add(mesh);
        }
      }

      // Elevator walls
      if (elevatorGeometriesExist) {
        for (let i = 0; i < 2; i++) {
          mergedGeometry = BufferGeometryUtils.mergeGeometries(elevatorGeometries[i], false);
          mesh = new THREE.Mesh(mergedGeometry, elevatorWall.materials[i]);
          mesh.castShadow = true;
          mesh.receiveShadow = true;
          mesh.name = 'elevator';
          this.add(mesh);
        }
      }

      // Store the player's initial position and direction
      // TODO refactor initial player position and direction if needed
      this.initialPosition = this.cellToCartesian(configData.player.initialPosition);
      this.initialDirection = configData.player.initialDirection;

      this.loaded = true;
      globalAssetManager.finishedLoading();
    };

    // TODO remove resource % loaded loggging if needed
    const onProgress = function (url, xhr) {
      // console.log("Resource '" + url + "' " + ((100.0 * xhr.loaded) / xhr.total).toFixed(0) + '% loaded.');
    };

    const onError = function (url, error) {
      console.error("Error loading resource '" + url + "' (" + error + ').');
    };

    // The cache must be enabled; additional information available at https://threejs.org/docs/api/en/loaders/FileLoader.html
    THREE.Cache.enabled = true;

    // Create a resource file loader
    // const loader = new THREE.FileLoader();
    const loader = customMazeloaderService;

    // Set the response type: the resource file will be parsed with JSON.parse()
    // loader.setResponseType("json");

    // Load a maze description resource file
    // TODO proxy the resource file if needed
    this.url = mazeUrl;
    this.elevatorUrl = elevatorUrl;
    loader.loadElevators(
      //Resource URL
      this.url,
      this.elevatorUrl,

      // onLoad callback
      (mazeData) => {
        // onLoadMaze(description);
        this.onLoad(mazeData);
        return onLoadMaze(mazeData);
      },

      // onProgress callback
      (xhr) => {
        onProgress(this.url, xhr);
        return onMazeProgress(xhr);
      },

      // onError callback
      (error) => {
        onError(this.url, error);
        return onMazeError(error);
      },
    );
  }

  // Convert cell [row, column] coordinates to cartesian (x, y, z) coordinates
  cellToCartesian(position) {
    return new THREE.Vector3((position[1] - this.halfSize.width + 0.5) * this.scale.x, 0.0, (position[0] - this.halfSize.depth + 0.5) * this.scale.z);
  }

  // Convert cartesian (x, y, z) coordinates to cell [row, column] coordinates
  cartesianToCell(position) {
    return [Math.floor(position.z / this.scale.z + this.halfSize.depth), Math.floor(position.x / this.scale.x + this.halfSize.width)];
  }

  // Detect collision with corners (method: BC/AABB)
  cornerCollision(indices, offsets, orientation, position, delta, radius, name) {
    const row = indices[0] + offsets[0];
    const column = indices[1] + offsets[1];
    try {
      if (this.map[row][column] == MapCell.YesNorthWallNoWestWall - orientation || this.map[row][column] == MapCell.YesNorthWallYesWestWall) {
        const x = position.x - (this.cellToCartesian([row, column]).x + delta.x * this.scale.x);
        const z = position.z - (this.cellToCartesian([row, column]).z + delta.z * this.scale.z);
        if (x * x + z * z < radius * radius) {
          // TODO remove collision logging if needed
          console.log('Collision with ' + name + '.');
          return true;
        }
      }
    } catch (e) {
      // console.warn('Error in cornerCollision');
    }
    return false;
  }

  // Detect collision with walls (method: BC/AABB)
  wallCollision(indices, offsets, orientation, position, delta, radius, name) {
    const row = indices[0] + offsets[0];
    const column = indices[1] + offsets[1];
    try {
      if (this.map[row][column] == MapCell.YesNorthWallNoWestWall - orientation || this.map[row][column] == MapCell.YesNorthWallYesWestWall) {
        if (orientation != 0) {
          if (Math.abs(position.x - (this.cellToCartesian([row, column]).x + delta.x * this.scale.x)) < radius) {
            // TODO remove collision logging if needed
            // console.log('Collision with ' + name + '.');
            return true;
          }
        } else {
          if (Math.abs(position.z - (this.cellToCartesian([row, column]).z + delta.z * this.scale.z)) < radius) {
            // TODO remove collision logging if needed
            // console.log('Collision with ' + name + '.');
            return true;
          }
        }
      }
    } catch (e) {
      // console.warn('Error in wallCollision', e);
    }
    return false;
  }

  // Detect collision with walls and corners (method: OBB/AABB)
  wallAndCornerCollision(indices, offsets, orientation, obb, name) {
    const row = indices[0] + offsets[0];
    const column = indices[1] + offsets[1];
    if (this.map[row][column] == MapCell.YesNorthWallNoWestWall - orientation || this.map[row][column] == MapCell.YesNorthWallYesWestWall) {
      if (obb.intersectsBox3(this.aabb[row][column][orientation])) {
        // TODO remove collision logging if needed
        console.log('Collision with ' + name + '.');
        return true;
      }
    }
    return false;
  }

  // Detect collisions
  collision(method, position, halfSize, direction) {
    const indices = this.cartesianToCell(position);
    if (method != 'obb-aabb') {
      if (
        this.wallCollision(indices, [0, 0], 0, position, { x: 0.0, z: -0.475 }, halfSize, 'north wall') || // Collision with north wall
        this.wallCollision(indices, [0, 0], 1, position, { x: -0.475, z: 0.0 }, halfSize, 'west wall') || // Collision with west wall
        this.wallCollision(indices, [1, 0], 0, position, { x: 0.0, z: -0.525 }, halfSize, 'south wall') || // Collision with south wall
        this.wallCollision(indices, [0, 1], 1, position, { x: -0.525, z: 0.0 }, halfSize, 'east wall') || // Collision with east wall
        this.cornerCollision(indices, [1, 0], 1, position, { x: -0.475, z: -0.5 }, halfSize, 'southwest corner (NS-oriented wall)') || // Collision with southwest corner (NS-oriented wall)
        this.cornerCollision(indices, [1, 1], 0, position, { x: -0.5, z: -0.525 }, halfSize, 'southeast corner (WE-oriented wall)') || // Collision with southeast corner (WE-oriented wall)
        this.cornerCollision(indices, [1, 1], 1, position, { x: -0.525, z: -0.5 }, halfSize, 'southeast corner (NS-oriented wall)') || // Collision with southeast corner (NS-oriented wall)
        this.cornerCollision(indices, [0, 1], 0, position, { x: -0.5, z: -0.475 }, halfSize, 'northeast corner (WE-oriented wall)') || // Collision with northeast corner (WE-oriented wall)
        (indices[0] > 0 &&
          (this.cornerCollision(indices, [-1, 1], 1, position, { x: -0.525, z: 0.5 }, halfSize, 'northeast corner (NS-oriented wall)') || // Collision with northeast corner (NS-oriented wall)
            this.cornerCollision(indices, [-1, 0], 1, position, { x: -0.475, z: 0.5 }, halfSize, 'northwest corner (NS-oriented wall)'))) || // Collision with northwest corner (NS-oriented wall)
        (indices[1] > 0 &&
          (this.cornerCollision(indices, [0, -1], 0, position, { x: 0.5, z: -0.475 }, halfSize, 'northwest corner (WE-oriented wall)') || // Collision with northwest corner (WE-oriented wall)
            this.cornerCollision(indices, [1, -1], 0, position, { x: 0.5, z: -0.525 }, halfSize, 'southwest corner (WE-oriented wall)'))) // Collision with southwest corner (WE-oriented wall)
      ) {
        return true;
      }
      // No collision
      return false;
    } else {
      // Create the object's oriented bounding box (OBB) in 3D space and set its orientation
      const obb = new OBB(position, halfSize);
      obb.applyMatrix4(new THREE.Matrix4().makeRotationY(direction));
      if (
        this.wallAndCornerCollision(indices, [0, 0], 0, obb, 'north wall') || // Collision with north wall
        this.wallAndCornerCollision(indices, [0, 0], 1, obb, 'west wall') || // Collision with west wall
        this.wallAndCornerCollision(indices, [1, 0], 0, obb, 'south wall') || // Collision with south wall
        this.wallAndCornerCollision(indices, [0, 1], 1, obb, 'east wall') || // Collision with east wall
        this.wallAndCornerCollision(indices, [1, 0], 1, obb, 'southwest corner (NS-oriented wall)') || // Collision with southwest corner (NS-oriented wall)
        this.wallAndCornerCollision(indices, [1, 1], 0, obb, 'southeast corner (WE-oriented wall)') || // Collision with southeast corner (WE-oriented wall)
        this.wallAndCornerCollision(indices, [1, 1], 1, obb, 'southeast corner (NS-oriented wall)') || // Collision with southeast corner (NS-oriented wall)
        this.wallAndCornerCollision(indices, [0, 1], 0, obb, 'northeast corner (WE-oriented wall)') || // Collision with northeast corner (WE-oriented wall)
        (indices[0] > 0 &&
          (this.wallAndCornerCollision(indices, [-1, 1], 1, obb, 'northeast corner (NS-oriented wall)') || // Collision with northeast corner (NS-oriented wall)
            this.wallAndCornerCollision(indices, [-1, 0], 1, obb, 'northwest corner (NS-oriented wall)'))) || // Collision with northwest corner (NS-oriented wall)
        (indices[1] > 0 &&
          (this.wallAndCornerCollision(indices, [0, -1], 0, obb, 'northwest corner (WE-oriented wall)') || // Collision with northwest corner (WE-oriented wall)
            this.wallAndCornerCollision(indices, [1, -1], 0, obb, 'southwest corner (WE-oriented wall)'))) // Collision with southwest corner (WE-oriented wall)
      ) {
        return true;
      }
      // No collision
      return false;
    }
  }

  foundExit(position) {
    if (!this.exitLocation) return;

    return Math.abs(position.x - this.exitLocation.x) < 0.5 * this.scale.x && Math.abs(position.z - this.exitLocation.z) < 0.5 * this.scale.z;
  }

  foundSomeExit(position) {
    if (!this.exitLocations) {
      return null;
    }

    // Check passages
    let passageDetectionRadiusX = 0.2 * this.scale.x;
    let passageDetectionRadiusZ = 0.2 * this.scale.z;
    for (const passage of this.exitLocations.passages) {
      if (Math.abs(position.x - passage.entrancePosition3d.x) < passageDetectionRadiusX && Math.abs(position.z - passage.entrancePosition3d.z) < passageDetectionRadiusZ) {
        return { type: 'passage', details: passage, exitFloorNumber: this.mazeData.floorNumber, exitBuildingCode: this.mazeData.buildingCode };
      }
    }

    // Check elevators
    let elevatorDetectionRadiusX = 0.2 * this.scale.x;
    let elevatorDetectionRadiusZ = 0.2 * this.scale.z;
    for (const elevator of this.exitLocations.elevators) {
      if (Math.abs(position.x - elevator.entrancePosition3d.x) < elevatorDetectionRadiusX && Math.abs(position.z - elevator.entrancePosition3d.z) < elevatorDetectionRadiusZ) {
        return { type: 'elevator', details: elevator, exitFloorNumber: this.mazeData.floorNumber, exitBuildingCode: this.mazeData.buildingCode };
      }
    }

    // No exit found
    return null;
  }
}
