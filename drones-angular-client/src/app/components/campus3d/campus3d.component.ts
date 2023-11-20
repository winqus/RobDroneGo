import { animate, state, style, transition, trigger } from '@angular/animations';
import { Component, ElementRef, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { ActivatedRoute, Params, Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { FloorSelectorGUI } from 'src/app/ThreeDModule/floorSelectorGUI.js';
import Building from 'src/app/core/models/building.model.js';
import Map from 'src/app/core/models/map.model.js';
import ThumbRaiser from '../../ThreeDModule/thumb_raiser.js';
import { AppBuildingFloorDropdownListComponent } from '../app-building-floor-dropdown-list/app-building-floor-dropdown-list.component.js';
import { CustomMazeLoaderParams } from './interfaces/customMazeLoaderParams.interface';
import { MapCell } from './interfaces/mapCell.enum';
import MazePartialConfig, { Destination, Elevator, ExitLocationEvent, MazeAndPlayerConfig, MazeFullConfig, Passage } from './interfaces/mazeData.interface.js';
import { CustomMazeLoaderService } from './services/custom-maze-loader.service';
import * as thumberRaiserParams from './threeD.config';

let thumbRaiser: any;
let animationFrameId: number | null = null;

// Starts animation loop, needs to be stopped with stopAnimation(). Be careful not to start multiple animation loops.
function animateGame() {
  animationFrameId = requestAnimationFrame(animateGame);
  // Update the game
  thumbRaiser.update();
}

// Stops animation loop, needs to be started with animateGame()
function stopAnimation() {
  if (animationFrameId !== null) {
    cancelAnimationFrame(animationFrameId);
    animationFrameId = null;
  }
}

// Creates the game object and initializes it with the given parameters
function initializeThumbRaiser(
  canvasContainer: ElementRef,
  customMazeLoaderParams: CustomMazeLoaderParams,
  gameIsRunningCallback = () => {},
  gameIsPausedCallback = () => {},
  assetsLoadedCallback = () => {},
  sceneLoadedCallback = () => {},
  sceneExitLocationCallback: (exitLocation: ExitLocationEvent) => void,
) {
  thumbRaiser = new ThumbRaiser(
    canvasContainer.nativeElement,
    customMazeLoaderParams,
    gameIsRunningCallback,
    gameIsPausedCallback,
    assetsLoadedCallback,
    sceneLoadedCallback,
    sceneExitLocationCallback,
  );
  thumbRaiser.initialize(
    thumberRaiserParams.generalParameters,
    thumberRaiserParams.audioParameters,
    thumberRaiserParams.cubeTexturesParameters,
    thumberRaiserParams.mazeParameters,
    thumberRaiserParams.playerParameters,
    thumberRaiserParams.ambientLightParameters,
    thumberRaiserParams.directionalLightParameters,
    thumberRaiserParams.spotLightParameters,
    thumberRaiserParams.flashLightParameters,
    thumberRaiserParams.shadowsParameters,
    thumberRaiserParams.fogParameters,
    thumberRaiserParams.collisionDetectionParameters,
    thumberRaiserParams.fixedViewCameraParameters,
    thumberRaiserParams.firstPersonViewCameraParameters,
    thumberRaiserParams.thirdPersonViewCameraParameters,
    thumberRaiserParams.topViewCameraParameters,
    thumberRaiserParams.miniMapCameraParameters,
  );
}

// TODO remove mazeExampleUrls when url retrieval is replaced in private initialize3d() {
const mazeExampleUrls: {
  [key: string]: {
    [key: number]: string;
  };
} = {
  B: {
    1: './assets/mazes/Loquitas_10x10.json',
    2: './assets/mazes/Loquitas_20x20.json',
    3: './assets/mazes/exampleB3Maze.json',
  },
  C: {
    2: './assets/mazes/ckarzx_20x20.json',
    3: './assets/mazes/exampleC3Maze.json',
    4: './assets/mazes/exampleC4Maze.json',
  },
};

@Component({
  selector: 'app-campus3d',
  templateUrl: './campus3d.component.html',
  styleUrls: ['./campus3d.component.css'],
  animations: [
    trigger('fadeOut', [
      state(
        'visible',
        style({
          opacity: 1,
        }),
      ),
      state(
        'hidden',
        style({
          opacity: 0,
        }),
      ),
      transition('visible => hidden', [animate('0.5s')]),
    ]),
  ],
})
export class Campus3dComponent implements OnInit, OnDestroy {
  @ViewChild('canvasContainer', { static: true }) canvasContainer!: ElementRef;

  buildingCode: string = 'B';
  floorNumber: number = 0;
  robotId: string | null = null;
  paramMapSubscription!: Subscription;
  queryParamsSubscription!: Subscription;

  loadingScreenText = 'Loading';

  sceneLoaded = false;
  assetsLoaded = false;
  showLoadingScreen = true;
  renderLoadingScreenElement = true;

  previousBuildingCode: string | null = null;
  previousFloorNumber: number | null = null;
  previousExitLocation: ExitLocationEvent | null = null;

  constructor(
    private router: Router,
    private route: ActivatedRoute,
    private mazeLoaderService: CustomMazeLoaderService,
  ) {}

  // Subscribes to changes in route params (by rerouting in code or manually).
  // When buildingCode or floorNumber changes, the 3D scene is replaced with a new one.
  // Loads initial scene based on url params.
  ngOnInit(): void {
    this.paramMapSubscription = this.route.paramMap.subscribe((params) => {
      const buildingCode = params.get('buildingCode');
      const floorNumber = params.get('floorNumber');

      if (buildingCode !== null) {
        this.buildingCode = buildingCode;
      }

      if (floorNumber !== null) {
        this.floorNumber = +floorNumber; // Convert string to number
        this.replace3dScene();
      }
    });

    this.queryParamsSubscription = this.route.queryParams.subscribe((queryParams) => {
      const robotId = queryParams['robotId'];
      if (robotId) {
        console.log(`Robot ID: ${robotId}`);
        this.robotId = robotId;
      }
    });

    this.mazeLoaderService.loadMazeBase3DData(
      './assets/mazes/baseMaze3DSettings.json',
      (_data) => {
        console.log('Base maze 3D data loaded:', _data);
      },
      (_progress) => {},
      (error) => {
        console.error('Error loading base maze 3D data:', error);
      },
    );

    // TODO - uncomment when mazeDataPostProcessor is refactored
    // this.mazeLoaderService.mapDataFromApiPreProcessor = this.mapDataFromApiPreProcessor;
    this.mazeLoaderService.mazeDataPostProcessor = this.mazeDataPostProcessor;
    this.initialize3d();
  }

  // Prepares customMazeLoaderParams for loading a map, initializes ThumbRaiser and starts animation loop
  // TODO replace with url for node API
  initialize3d() {
    const customMazeLoaderParams: CustomMazeLoaderParams = {
      customMazeloaderService: this.mazeLoaderService,
      // TODO replace with url for node API
      mazeUrl: mazeExampleUrls[this.buildingCode][this.floorNumber],
      onLoadMaze: (data) => this.onLoadMaze(data),
      onMazeProgress: (progress) => this.onMazeProgress(progress),
      onMazeError: (error) => this.onMazeError(error),
    };

    this.assetsLoaded = false;
    this.sceneLoaded = false;

    initializeThumbRaiser(
      this.canvasContainer,
      customMazeLoaderParams,
      () => this.onGameIsRunning(),
      () => this.onGameIsPaused(),
      () => this.onAssetsLoaded(),
      () => this.onSceneLoaded(),
      (exitLocation: ExitLocationEvent) => this.onSceneExitLocation(exitLocation),
    );
    animateGame();
  }

  // Called by customMazeLoaderService when base maze 3D data is to be loaded (first thing that intercepts data from backend API)
  // FIXME - Needs refactoring to be usable with backend node API
  // Should convert data from node API to MazePartialConfig or MazeAndPlayerConfig (enought, rest is added from baseMazeGroundWallSettings.json in customMazeLoaderService)
  // After refactoring uncomment line in ngOnInit() for customMazeLoaderService.mapDataFromApiPreProcessor to use this callback proxy function
  mapDataFromApiPreProcessor = (data: Map): MazePartialConfig | MazeAndPlayerConfig => {
    const buildingCode = this.buildingCode;
    const floorNumber = this.floorNumber;
    const initialPlayerPosition: [number, number] = [0.0, 0.0];
    const initialPlayerDirection = 90.0;
    const processedData: MazeAndPlayerConfig = {
      maze: {
        buildingCode,
        floorNumber,
        size: {
          width: data.size.width,
          depth: data.size.height,
        },
        map: data.map as MapCell[][],
        exitLocations: {
          passages: [],
          elevators: [],
        },
      },
      player: {
        initialPosition: initialPlayerPosition,
        initialDirection: initialPlayerDirection,
      },
    };
    return processedData;
  };

  // Called by customMazeLoaderService when maze data is to be loaded (last thing that intercepts data from backend API before start of maze generation)
  mazeDataPostProcessor = (data: MazeFullConfig) => {
    if (this.previousExitLocation) {
      if (this.previousExitLocation.type === 'passage') {
        const origin = { buildingCode: this.previousBuildingCode, floorNumber: this.previousFloorNumber } as Destination;
        const destinationPassage = data.maze.exitLocations.passages.find(
          (passage) => passage.destination.buildingCode === origin.buildingCode && passage.destination.floorNumber === origin.floorNumber,
        );
        // Updates player position and direction
        let destinationPlayerPosition = [0, 0];
        let destinationPlayerDirection = 0;
        if (destinationPassage) {
          if (data.maze.map[destinationPassage.cellPosition[0]][destinationPassage.cellPosition[1]] === MapCell.PassageNorth) {
            // Passage wall type north
            if (destinationPassage.cellPosition[0] === 0) {
              // Passage entrance/exit facing south
              destinationPlayerPosition = [destinationPassage.cellPosition[0], destinationPassage.cellPosition[1]];
              destinationPlayerDirection = 0;
            } else {
              // Passage entrance/exit facing north
              destinationPlayerPosition = [
                destinationPassage.cellPosition[0] - Math.abs(destinationPassage.entracePositionOffset[0] * 2),
                destinationPassage.cellPosition[1] - Math.abs(destinationPassage.entracePositionOffset[1] * 2),
              ];
              destinationPlayerDirection = 180;
            }
          } else {
            // Passage wall type west
            if (destinationPassage.cellPosition[1] === 0) {
              // Passage entrance/exit facing east
              destinationPlayerPosition = [destinationPassage.cellPosition[0], destinationPassage.cellPosition[1]];
              destinationPlayerDirection = 90;
            } else {
              // Passage entrance/exit facing west
              destinationPlayerPosition = [
                destinationPassage.cellPosition[0] - Math.abs(destinationPassage.entracePositionOffset[0] * 2),
                destinationPassage.cellPosition[1] - Math.abs(destinationPassage.entracePositionOffset[1] * 2),
              ];
              destinationPlayerDirection = -90;
            }
          }
        }
        // This is the player position and direction that will be used when the scene is loaded
        data.player.initialPosition = destinationPlayerPosition as [number, number];
        data.player.initialDirection = destinationPlayerDirection;
      } else if (this.previousExitLocation.type === 'elevator') {
        const destinationElevator = data.maze.exitLocations.elevators[0];
        let destinationPlayerPosition = [0, 0];
        let destinationPlayerDirection = 0;

        if (destinationElevator) {
          let mapValue = data.maze.map[destinationElevator.cellPosition[0]][destinationElevator.cellPosition[1]];
          switch (mapValue) {
            case MapCell.ElevatorNorth:
              // Elevator entrance/exit facing north
              destinationPlayerPosition = [
                destinationElevator.cellPosition[0] + destinationElevator.entracePositionOffset[0] * 2,
                destinationElevator.cellPosition[1] + destinationElevator.entracePositionOffset[1] * 2,
              ];
              destinationPlayerDirection = 180;
              break;
            case MapCell.ElevatorWest:
              // Elevator entrance/exit facing west
              destinationPlayerPosition = [
                destinationElevator.cellPosition[0] + destinationElevator.entracePositionOffset[0] * 2,
                destinationElevator.cellPosition[1] + destinationElevator.entracePositionOffset[1] * 2,
              ];
              destinationPlayerDirection = 270;
              break;
            case MapCell.ElevatorSouth:
              // Elevator entrance/exit facing south
              destinationPlayerPosition = [
                destinationElevator.cellPosition[0] + destinationElevator.entracePositionOffset[0] * 2,
                destinationElevator.cellPosition[1] + destinationElevator.entracePositionOffset[1] * 2,
              ];
              destinationPlayerDirection = 0;
              break;
            case MapCell.ElevatorEast:
              // Elevator entrance/exit facing east
              destinationPlayerPosition = [
                destinationElevator.cellPosition[0] + destinationElevator.entracePositionOffset[0] * 2,
                destinationElevator.cellPosition[1] + destinationElevator.entracePositionOffset[1] * 2,
              ];
              destinationPlayerDirection = 90;
              break;
            default:
              console.error(`Error: Invalid map cell value: ${mapValue}`);
              break;
          }
        }

        // This is the player position and direction that will be used when the scene is loaded
        data.player.initialPosition = destinationPlayerPosition as [number, number];
        data.player.initialDirection = destinationPlayerDirection;
      }
    }

    return data;
  };

  // Replaces 3D scene with a new one. Replaces the url used in map loading based on updated route params. Loads new maze in thumbRaiser.
  // TODO replace with url for node API
  replace3dScene() {
    if (!thumbRaiser?.isInitialized()) {
      return;
    }

    this.assetsLoaded = false;
    this.sceneLoaded = false;
    this.renderLoadingScreenElement = true;

    // TODO replace with url for node API
    const customMazeLoaderParams: CustomMazeLoaderParams = {
      customMazeloaderService: this.mazeLoaderService,
      mazeUrl: mazeExampleUrls[this.buildingCode][this.floorNumber],
      onLoadMaze: (data) => this.onLoadMaze(data),
      onMazeProgress: (progress) => this.onMazeProgress(progress),
      onMazeError: (error) => this.onMazeError(error),
    };

    thumbRaiser.loadNewMaze(customMazeLoaderParams);
    if (animationFrameId === null) {
      animateGame();
    }
  }

  // Navigates to a new 3D scene (triggers a new maze load; check replace3dScene())
  changeMaze = (buildingCode: string, floorNumber: number, robotId?: string): void => {
    const navigationExtras = robotId || this.robotId ? { queryParams: { robotId: robotId || this.robotId } } : {};
    this.router.navigate(['/3d/building', buildingCode, 'floor', floorNumber], navigationExtras).then(() => {
      // handle something post navigation, if needed
    });
  };

  // Handle the building and floor selection from the dropdown list
  handleBuildingFloorSelection = (selectedData: any): void => {
    const building = selectedData.building as Building;
    const floorNumber = selectedData.floorNumber as number;

    // Call the changeMaze method with the selected building and floor
    this.changeMaze(building.code, floorNumber);
  };

  // Triggered in thumbRaiser.js when player reaches an exitLocation (that's check in maze.js)
  onSceneExitLocation = (exitLocation: ExitLocationEvent) => {
    console.warn('Exit location:', exitLocation);

    this.previousBuildingCode = this.buildingCode;
    this.previousFloorNumber = this.floorNumber;
    this.previousExitLocation = exitLocation;

    if (exitLocation.type === 'passage') {
      const destination = (exitLocation.details as Passage).destination;
      this.changeMaze(destination.buildingCode, destination.floorNumber);
    } else if (exitLocation.type === 'elevator') {
      stopAnimation();
      const destination = exitLocation.details as Elevator;
      const floorSelectorLilGui = new FloorSelectorGUI(this.canvasContainer.nativeElement, { width: 300 });
      floorSelectorLilGui.addLabel('Select destination floor');

      const floorSelectionList = destination.connectedFloorNumbers.filter((floorNumber) => floorNumber !== this.floorNumber);
      floorSelectorLilGui.addFloorButtons(floorSelectionList, (floor: number) => {
        floorSelectorLilGui.destroy();
        this.changeMaze(this.buildingCode, floor, this.robotId || undefined);
      });
    }
  };

  // Not utilized at the moment
  onGameIsRunning = () => {};

  onGameIsPaused = () => {
    this.showLoadingScreen = true;
  };

  onAssetsLoaded = () => {
    console.log('Assets loaded');
    this.assetsLoaded = true;
  };

  onSceneLoaded = () => {
    this.sceneLoaded = true;
    this.showLoadingScreen = false;
    console.log('Scene loaded');
  };

  onLoadingAnimationDone = () => {
    this.renderLoadingScreenElement = this.showLoadingScreen;
  };

  // Triggered when maze json in loaded in 3D scene
  private onLoadMaze = (data: any) => {
    // Handle successful loading
    // console.log('Maze loaded, data:', data);
  };

  // Triggered while loading maze json in 3D scene
  private onMazeProgress = (event: any) => {
    // Handle loading progress
    // console.log('Maze progress:', event);
  };

  // Triggered when loading maze json in 3D scene fails
  private onMazeError = (error: any) => {
    // Handle loading error
    console.error('Maze loading error:', error);
  };

  ngOnDestroy(): void {
    if (this.paramMapSubscription) {
      this.paramMapSubscription.unsubscribe();
    }
  }
}
