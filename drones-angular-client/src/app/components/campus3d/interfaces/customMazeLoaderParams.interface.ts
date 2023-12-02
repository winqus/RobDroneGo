import { CustomMazeLoaderService } from '../services/custom-maze-loader.service';

export interface CustomMazeLoaderParams {
  customMazeloaderService: CustomMazeLoaderService;
  mazeUrl: string;
  elevatorUrl: string;
  onLoadMaze: (data: any) => void;
  onMazeProgress: (progress: any) => void;
  onMazeError: (error: any) => void;
}
