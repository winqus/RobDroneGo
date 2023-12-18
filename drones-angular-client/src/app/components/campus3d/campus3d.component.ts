import { animate, state, style, transition, trigger } from '@angular/animations';
import { Component, ElementRef, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { ActivatedRoute, Params, Router } from '@angular/router';
import { error } from 'cypress/types/jquery/index.js';
import { update } from 'lodash';
import { Observable, Subscription, catchError, combineLatest, debounceTime, firstValueFrom, of, switchMap, tap, throwError } from 'rxjs';
import { API_ROUTES } from 'src/api.config';
import { FloorSelectorGUI } from 'src/app/ThreeDModule/floorSelectorGUI.js';
import Building from 'src/app/core/models/building.model.js';
import { DeliveryTask } from 'src/app/core/models/deliveryTask.model.js';
import Floor from 'src/app/core/models/floor.model.js';
import Map from 'src/app/core/models/map.model.js';
import Room from 'src/app/core/models/room.model.js';
import { TaskRequest } from 'src/app/core/models/taskRequest.model.js';
import { environment } from 'src/environments/environment';
import ThumbRaiser from '../../ThreeDModule/thumb_raiser.js';
import { isDeliveryTask } from '../../core/utilities/task-utilities';
import { FloorService } from '../../services/floor.service';
import { ModalService } from '../../services/modal.service';
import { RoomService } from '../../services/room.service';
import { TaskRequestService } from '../../services/task-request.service';
import { AppBuildingFloorDropdownListComponent } from '../app-building-floor-dropdown-list/app-building-floor-dropdown-list.component.js';
import mockedDeliveryTaskRequest from './example/data/mockedDeliveryTaskRequest.mock';
import NAVIGATION_DATA_EXAMPLE from './example/data/navigationData.example.json';
import { CustomMazeLoaderParams } from './interfaces/customMazeLoaderParams.interface';
import { MapCell } from './interfaces/mapCell.enum';
import MazePartialConfig, { Destination, Elevator, ExitLocationEvent, MazeAndPlayerConfig, MazeFullConfig, Passage } from './interfaces/mazeData.interface';
import { RobotState } from './interfaces/robotState.interface';
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
  isAutoMoving: RobotState,
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
    isAutoMoving,
  );
}

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

  buildingCode!: string;
  floorNumber!: number;
  taskId: string | null = null;
  paramsSubscription!: Subscription;

  loadingScreenText = 'Choose a floor';

  sceneLoaded = false;
  assetsLoaded = false;
  showLoadingScreen = true;
  renderLoadingScreenElement = true;

  currentElevator?: Elevator;
  currentFloorRooms?: Room[];
  currentFloor?: Floor;

  previousBuildingCode: string | null = null;
  previousFloorNumber: number | null = null;
  previousExitLocation: ExitLocationEvent | null = null;

  robotState: RobotState = {
    isAutoMoving: false,
  };

  currentTaskRequest?: TaskRequest;

  constructor(
    private router: Router,
    private route: ActivatedRoute,
    private mazeLoaderService: CustomMazeLoaderService,
    private roomService: RoomService,
    private floorService: FloorService,
    private taskService: TaskRequestService,
    private modalService: ModalService,
  ) {
    /* VISUALIZE THE ROBOT PATH */
    if (!environment.production) {
      (window as any).SHOW_ROBOT_PATH = true; // set to false if you don't want to see the robot path
    }
  }

  // Subscribes to changes in route params (by rerouting in code or manually).
  // When buildingCode or floorNumber changes, the 3D scene is replaced with a new one.
  // Loads initial scene based on url params.
  ngOnInit(): void {
    const combinedParams = combineLatest([this.route.paramMap, this.route.queryParams]);

    this.paramsSubscription = combinedParams.pipe(debounceTime(100)).subscribe(([params, queryParams]) => {
      // console.warn('CALLED PARAMS SUBSCRIPTION', params, queryParams);

      const buildingCode = params.get('buildingCode');
      const floorNumber = params.get('floorNumber');
      const taskId = queryParams['taskId'];
      const taskStep = +queryParams['step'] || undefined;

      if (buildingCode !== null) {
        this.buildingCode = buildingCode;
      }
      if (floorNumber !== null) {
        this.floorNumber = +floorNumber; // Convert string to number
      }

      if (!environment.production) {
        console.log('thumbRaiser initialized?', thumbRaiser?.isInitialized() || false);
      }

      if (taskId) {
        // console.warn(`Task ID: ${taskId}`);
        this.taskId = taskId;

        // this.taskService.getTaskRequestById(taskId).subscribe({
        const mockedTaskRequest: TaskRequest = mockedDeliveryTaskRequest;

        // TODO: Uncomment this line and remove mockedTaskRequest (and the line `of(mockedTas.....).subsc....` ) when integrating with backend.
        // this.taskService.getTaskRequestById(taskId).subscribe({
        of(mockedTaskRequest).subscribe({
          next: (taskRequest) => {
            if (isDeliveryTask(taskRequest)) {
              console.warn('Loaded task request:', taskRequest.id);
              const deliveryTask = taskRequest.task as DeliveryTask;
              if (taskRequest.navigationData) {
                this.robotState.navigationData = taskRequest.navigationData;
                this.robotState.isAutoMoving = true;

                this.robotState.navigationStep = taskStep !== undefined ? Math.max(0, Math.min(taskStep, this.robotState.navigationData?.mapPaths.length - 1)) : 0;
                this.robotState.numberOfNavigationSteps = this.robotState.navigationData?.mapPaths.length;
                this.robotState.navigationState = this.robotState.navigationStep === 0 ? 'unstarted' : 'ready';

                this.currentTaskRequest = taskRequest;
                this.buildingCode = taskRequest.navigationData.mapPaths[this.robotState.navigationStep].buildingCode;
                this.floorNumber = taskRequest.navigationData.mapPaths[this.robotState.navigationStep].floorNumber;

                const currentUrl = this.router.url;
                const routeArray = ['/3d/building', this.buildingCode, 'floor', this.floorNumber];
                const queryParams = { taskId: taskId, step: this.robotState.navigationStep };
                this.router.navigate(routeArray, {
                  relativeTo: this.route,
                  queryParams: queryParams,
                  queryParamsHandling: 'merge',
                  replaceUrl: true,
                });

                this.renderLoadingScreenElement = true;
                this.load3dScene();
              } else {
                window.alert('No navigation data found for this task');
              }
            } else {
              window.alert('Only delivery tasks are supported at the moment');
            }
          },
          error: (error: any) => {
            console.error('Error while getting task request by id:', error);
            window.alert('Task could not be loaded..');
          },
        });
      } else if (buildingCode && floorNumber) {
        this.renderLoadingScreenElement = true;
        this.load3dScene();
      }
    });

    this.mazeLoaderService.loadMazeBase3DData(
      './assets/mazes/baseMaze3DSettings.json',
      (_data) => {
        if (!environment.production) {
          console.log('Base Maze 3D Settings data loaded:', _data);
        }
      },
      (_progress) => {},
      (error) => {
        console.error('Error loading base maze 3D data:', error);
      },
    );

    this.mazeLoaderService.mapDataFromApiPreProcessor = this.mapDataFromApiPreProcessor;
    this.mazeLoaderService.mazeDataPostProcessor = this.mazeDataPostProcessor;

    window.addEventListener('robotNavigationStepFinished', this.onRobotNavigationStepFinished);
  }

  ngOnDestroy(): void {
    if (this.paramsSubscription) {
      this.paramsSubscription.unsubscribe();
    }

    window.removeEventListener('robotNavigationStepFinished', this.onRobotNavigationStepFinished);
  }

  // Called by customMazeLoaderService when base maze 3D data is to be loaded (first thing that intercepts data from backend API)
  mapDataFromApiPreProcessor = (data: Map, floorsNumber: number[]): MazePartialConfig | MazeAndPlayerConfig => {
    const buildingCode = this.buildingCode;
    const floorNumber = this.floorNumber;
    const initialPlayerPosition: [number, number] = [0.0, 0.0];
    const initialPlayerDirection = 90.0;

    let passages: Passage[] = [];
    if (data.exitLocations?.passages) {
      passages = data.exitLocations?.passages.map((passage) => {
        let entrance: [number, number] = [-0.5, 0];
        if (data.map[passage.cellPosition[0]][passage.cellPosition[1]] === MapCell.PassageWest) {
          entrance = [0, -0.5];
        }
        return {
          cellPosition: passage.cellPosition,
          entracePositionOffset: entrance,
          destination: passage.destination,
        };
      });
    }

    let elevators: Elevator[] = [];
    if (data.exitLocations?.elevators) {
      elevators = data.exitLocations?.elevators.map((elevator) => {
        let entrance: [number, number] = [-0.5, 0];
        if (data.map[elevator.cellPosition[0]][elevator.cellPosition[1]] === MapCell.ElevatorEast || data.map[elevator.cellPosition[0]][elevator.cellPosition[1]] === MapCell.ElevatorWest) {
          entrance = [0, -0.5];
        }
        return {
          cellPosition: elevator.cellPosition,
          entracePositionOffset: entrance,
          connectedFloorNumbers: floorsNumber,
        };
      });
    }

    const processedData: MazeAndPlayerConfig = {
      maze: {
        buildingCode,
        floorNumber,
        size: {
          width: data.size.width - 1,
          depth: data.size.height - 1,
        },
        map: data.map as MapCell[][],
        exitLocations: {
          passages: passages,
          elevators: elevators,
        },
      },
      player: {
        initialPosition: initialPlayerPosition,
        initialDirection: initialPlayerDirection,
      },
    };
    return processedData;
  };

  loadRoomData(): Observable<any> {
    return this.floorService.getFloorsByBuildingCode(this.buildingCode).pipe(
      switchMap((floors) => {
        const currentFloor = floors.find((floor) => floor.floorNumber === this.floorNumber);

        if (!currentFloor) {
          console.error(`Error: Floor ${this.floorNumber} not found in building ${this.buildingCode}`);
          return of(null);
        }

        this.currentFloor = currentFloor;
        return this.roomService.getRoomsByFloorId(currentFloor.id);
      }),
      tap((rooms) => {
        if (rooms) {
          this.currentFloorRooms = rooms;
          // console.warn('Room data assigned to currentFloorRooms:', this.currentFloorRooms);
        }
      }),
      catchError((error) => {
        console.error('An error occurred while loading room data:', error);
        return of(null);
      }),
    );
  }

  addFloorRoomDataToMazeData = (data: MazeFullConfig): MazeFullConfig => {
    const rooms: (Room & { doorPosition?: [number, number] })[] = [];
    const currentFloorRooms = this.currentFloorRooms;

    for (const room of currentFloorRooms || []) {
      let mapCellX = null;
      let mapCellY = null;

      for (let y = room.position.y; y <= room.position.y + room.size.length; y++) {
        for (let x = room.position.x; x <= room.position.x + room.size.width; x++) {
          if (data.maze.map[y][x] === 4 || data.maze.map[y][x] === 5) {
            mapCellX = x;
            mapCellY = y;
          }
        }
      }
      const roomData: Room & { doorPosition?: [number, number] } = {
        ...room,
        doorPosition: mapCellX !== null && mapCellY !== null ? [mapCellX, mapCellY] : undefined,
      };
      rooms.push(roomData);
    }
    data.roomData = rooms;

    return data;
  };

  // Called by customMazeLoaderService when maze data is to be loaded (last thing that intercepts data from backend API before start of maze generation)
  mazeDataPostProcessor = (data: MazeFullConfig) => {
    if (this.previousExitLocation) {
      if (this.previousExitLocation.type === 'passage') {
        // const origin = { buildingCode: this.previousBuildingCode, floorNumber: this.previousFloorNumber } as Destination;
        const origin = { buildingCode: this.previousExitLocation.exitBuildingCode, floorNumber: this.previousExitLocation.exitFloorNumber } as Destination;
        // console.warn('Origin in mazeDataPostProcessor:', origin);
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
            case MapCell.ElevatorEast:
              // Elevator entrance/exit facing west
              destinationPlayerPosition = [destinationElevator.cellPosition[0], destinationElevator.cellPosition[1]];
              destinationPlayerDirection = 90;
              break;
            case MapCell.ElevatorSouth:
              // Elevator entrance/exit facing south
              destinationPlayerPosition = [destinationElevator.cellPosition[0], destinationElevator.cellPosition[1]];
              destinationPlayerDirection = 0;
              break;
            case MapCell.ElevatorWest:
              // Elevator entrance/exit facing east
              destinationPlayerPosition = [
                destinationElevator.cellPosition[0] + destinationElevator.entracePositionOffset[0] * 2,
                destinationElevator.cellPosition[1] + destinationElevator.entracePositionOffset[1] * 2,
              ];
              destinationPlayerDirection = 270;
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

    this.addFloorRoomDataToMazeData(data);

    return data;
  };

  // Replaces 3D scene with a new one. Replaces the url used in map loading based on updated route params. Loads new maze in thumbRaiser.
  load3dScene() {
    const customMazeLoaderParams: CustomMazeLoaderParams = {
      customMazeloaderService: this.mazeLoaderService,
      mazeUrl: API_ROUTES.map.getMap(this.buildingCode, this.floorNumber),
      elevatorUrl: API_ROUTES.floor.floorWithElevator(this.buildingCode),
      onLoadMaze: (data) => this.onLoadMaze(data),
      onMazeProgress: (progress) => this.onMazeProgress(progress),
      onMazeError: (error) => this.onMazeError(error),
    };

    this.loadingScreenText = `Loading ${this.buildingCode}-${this.floorNumber}`;
    if (this.previousExitLocation) {
      if (this.previousExitLocation.type === 'passage') {
        const exitLocation = this.previousExitLocation.details as Passage;
        this.loadingScreenText = `Traversing passage to ${exitLocation.destination.buildingCode}-${exitLocation.destination.floorNumber}`;
      } else if (this.previousExitLocation.type === 'elevator') {
        this.loadingScreenText = `Elevator transit to floor ${this.floorNumber}`;
      }
    }

    if (!thumbRaiser?.isInitialized()) {
      console.warn('Initializing thumbRaiser');
      initializeThumbRaiser(
        this.canvasContainer,
        customMazeLoaderParams,
        () => this.onGameIsRunning(),
        () => this.onGameIsPaused(),
        () => this.onAssetsLoaded(),
        () => this.onSceneLoaded(),
        (exitLocation: ExitLocationEvent) => this.onSceneExitLocation(exitLocation),
        this.robotState,
      );
    }

    if (!thumbRaiser?.isInitialized()) {
      console.error('Failed to initialize thumbRaiser');
      return;
    }

    this.loadRoomData().subscribe(() => {
      console.warn('Loading 3D scene');

      thumbRaiser.loadNewMaze(customMazeLoaderParams);

      if (animationFrameId === null) {
        animateGame();
      }
    });
  }

  // Navigates to a new 3D scene (triggers a new maze load; check load3dScene())
  changeMaze = (buildingCode: string, floorNumber: number, taskId?: string, step?: number): void => {
    const navigationExtras = taskId || this.taskId ? { queryParams: { taskId: taskId || this.taskId, step: step || this.robotState.navigationStep } } : {};

    this.router.navigate(['/3d/building', buildingCode, 'floor', floorNumber], navigationExtras).then(() => {
      // handle something post navigation, if needed
      this.renderLoadingScreenElement = true;
      this.showLoadingScreen = true;
    });
  };

  // Handle the building and floor selection from the dropdown list
  handleBuildingFloorSelection = (selectedData: any): void => {
    const building = selectedData.building as Building;
    const floorNumber = selectedData.floorNumber as number;

    this.previousBuildingCode = null;
    this.previousFloorNumber = null;
    this.previousExitLocation = null;
    // Call the changeMaze method with the selected building and floor
    this.changeMaze(building.code, floorNumber);
  };

  // Triggered in thumbRaiser.js when player manually reaches an exitLocation (that's check in maze.js)
  onSceneExitLocation = (exitLocation: ExitLocationEvent) => {
    console.warn('Exit location:', exitLocation);

    this.previousBuildingCode = this.buildingCode;
    this.previousFloorNumber = this.floorNumber;
    this.previousExitLocation = exitLocation;

    if (exitLocation.type === 'passage') {
      const destination = (exitLocation.details as Passage).destination;
      this.changeMaze(destination.buildingCode, destination.floorNumber);
      stopAnimation();
    } else if (exitLocation.type === 'elevator') {
      stopAnimation();
      const destination = exitLocation.details as Elevator;
      const floorSelectorLilGui = new FloorSelectorGUI(this.canvasContainer.nativeElement, { width: 300 });
      floorSelectorLilGui.addLabel('Select destination floor');

      const floorSelectionList = destination.connectedFloorNumbers.filter((floorNumber) => floorNumber !== this.floorNumber);
      floorSelectorLilGui.addFloorButtons(floorSelectionList, (floor: number) => {
        floorSelectorLilGui.destroy();
        this.changeMaze(this.buildingCode, floor, this.taskId || undefined);
      });
    }
  };

  onRobotNavigationStepFinished = (event: Event) => {
    const updatedRobotState = (event as CustomEvent).detail as RobotState;
    // console.warn('Robot navigation step finished:', updatedRobotState);

    const navigationStepState = updatedRobotState.navigationState;
    if (navigationStepState !== 'stepFinished') {
      return;
    }

    if (this.robotState.navigationStep === this.robotState.numberOfNavigationSteps! - 1) {
      this.handleFullyCompletedNavigation();
      return;
    } else {
      this.handleFinishedNavigationStep();
    }
  };

  handleFullyCompletedNavigation = () => {
    this.robotState.navigationState = 'fullyCompleted';
    this.openNavigationFullyFinishedModal();
  };

  handleFinishedNavigationStep = () => {
    this.robotState.navigationStep = Math.min(this.robotState.navigationStep! + 1, this.robotState.numberOfNavigationSteps! - 1);
    this.robotState.navigationState = 'ready';
    const nextNavigationPlan = this.robotState.navigationData?.mapPaths[this.robotState.navigationStep!]!;
    const { buildingCode, floorNumber } = nextNavigationPlan;
    this.openConfirmationToChangeFloorModal(buildingCode, floorNumber);
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
    const checkAssetsLoaded = setInterval(() => {
      if (this.assetsLoaded === true) {
        clearInterval(checkAssetsLoaded);
        this.sceneLoaded = true;
        this.showLoadingScreen = false;

        if (this.robotState.isAutoMoving) {
          window.dispatchEvent(new CustomEvent('enableAutoMove', { detail: true }));
        }

        console.log('Scene loaded');
      }
    }, 100); // Check every 500 milliseconds
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

  openNavigationFullyFinishedModal = () => {
    this.modalService.openModal('Navigation Completed', 'The preview of task execution completed.', [
      {
        text: 'Close',
        class: 'btn',
        action: () => {
          /* Any additional close logic */
        },
        shouldClose: true,
      },
      {
        text: 'Go to Task List',
        class: 'btn btn-primary',
        action: () => {
          this.router.navigate(['/task/list']);
        },
        shouldClose: true,
      },
    ]);
  };

  openConfirmationToChangeFloorModal = (buildingCode: string, floorNumber: number) => {
    this.modalService.openModal('Change Floor', `Do you want to continue to building ${buildingCode}, floor ${floorNumber}?`, [
      {
        text: 'Go to task list',
        class: 'btn',
        action: () => {
          this.router.navigate(['/task/list']);
        },
        shouldClose: true,
      },
      {
        text: 'Continue',
        class: 'btn btn-primary',
        action: () => {
          this.changeMaze(buildingCode, floorNumber, this.taskId!, this.robotState.navigationStep);
        },
        shouldClose: true,
      },
    ]);
  };
}
