import dotenv from 'dotenv';
import { UserRole } from './src/domain/userRole.enum';

// Set the NODE_ENV to 'development' by default
const envFound = dotenv.config();
process.env.NODE_ENV = process.env.NODE_ENV || 'development';

if (!envFound /* || envFound?.error */) {
  // This error should crash whole process (should uncomment envFound?.error), disabled for pipelines for now
  throw new Error("⚠️  Couldn't find .env file  ⚠️");
}

if (process.env.NODE_ENV === 'development') {
  const developEnvFound = dotenv.config({ path: '.env.development' });
  if (developEnvFound && !developEnvFound?.error) {
    console.log('❕  Using .env.development file to supply config environment variables  ❕');
  }
}

if (process.env.NODE_ENV === 'test') {
  const testEnvFound = dotenv.config({ path: '.env.test' });
  if (testEnvFound && !testEnvFound?.error) {
    console.log('❕  Using .env.test file to supply config environment variables  ❕');
  }
}

export default {
  /**
   * Your favorite port : optional change to 4000 by JRT
   */
  port: parseInt(process.env.PORT, 10) || 4000,

  /**
   * That long string from mlab
   */
  databaseURL:
    process.env.NODE_ENV === 'development'
      ? process.env.DEV_MONGODB_URI || process.env.MONGODB_URI || 'mongodb://127.0.0.1:27017/test'
      : process.env.NODE_ENV === 'test' // for e2e testing
      ? process.env.TEST_MONGODB_URI || 'mongodb://127.0.0.1:27017/test'
      : process.env.MONGODB_URI,
  /**
   * Your secret sauce
   */
  jwtSecret: process.env.JWT_SECRET || 'my sakdfho2390asjod$%jl)!sdjas0i secret',

  allowedEmailDomains: process.env.ALLOWED_EMAIL_DOMAINS || ['isep.ipp.pt'],

  defaultUserRole: process.env.DEFAULT_USER_ROLE || UserRole.User.valueOf(),
  userRoles: Object.values(UserRole),

  /**
   * Used by winston logger
   */
  logs: {
    level: process.env.LOG_LEVEL || 'info',
  },

  /**
   * API configs
   */
  api: {
    prefix: '/api',
  },

  publicFolder: process.env.BASE_FOLDER || '/public_folder',
  base_dir: process.env.BASE_DIR || __dirname,

  planningAPI: {
    hostname: process.env.PLANNING_API_HOSTNAME || '127.0.0.1',
    port: process.env.PLANNING_API_PORT || 4400,
    basePath: process.env.PLANNING_API_BASE_PATH || '/planning-api',
  },

  controllers: {
    role: {
      name: 'RoleController',
      path: '../controllers/roleController',
    },
    building: {
      name: 'BuildingController',
      path: '../controllers/buildingController',
    },
    floor: {
      name: 'FloorController',
      path: '../controllers/floorController',
    },
    room: {
      name: 'RoomController',
      path: '../controllers/roomController',
    },
    passage: {
      name: 'PassageController',
      path: '../controllers/passageController',
    },
    robotType: {
      name: 'RobotTypeController',
      path: '../controllers/robotTypeController',
    },
    taskType: {
      name: 'TaskTypeController',
      path: '../controllers/taskTypeController',
    },
    robot: {
      name: 'RobotController',
      path: '../controllers/robotController',
    },
    file: {
      name: 'FileController',
      path: '../controllers/fileController',
    },
    planning: {
      name: 'PlanningController',
      path: '../controllers/planningController',
    },
    user: {
      name: 'UserController',
      path: '../controllers/userController',
    },
    taskRequest: {
      name: 'TaskRequestController',
      path: '../controllers/taskRequestController',
    },
  },

  repos: {
    role: {
      name: 'RoleRepo',
      path: '../repos/roleRepo',
    },
    user: {
      name: 'UserRepo',
      path: '../repos/userRepo',
    },
    building: {
      name: 'BuildingRepo',
      path: '../repos/buildingRepo',
    },
    floor: {
      name: 'FloorRepo',
      path: '../repos/floorRepo',
    },
    room: {
      name: 'RoomRepo',
      path: '../repos/roomRepo',
    },
    passage: {
      name: 'PassageRepo',
      path: '../repos/passageRepo',
    },
    robotType: {
      name: 'RobotTypeRepo',
      path: '../repos/robotTypeRepo',
    },
    taskType: {
      name: 'TaskTypeRepo',
      path: '../repos/taskTypeRepo',
    },
    robot: {
      name: 'RobotRepo',
      path: '../repos/robotRepo',
    },
    file: {
      name: 'FileRepo',
      path: '../repos/fileRepo',
    },
    taskRequest: {
      name: 'TaskRequestRepo',
      path: '../repos/taskRequestRepo',
    },
  },

  services: {
    role: {
      name: 'RoleService',
      path: '../services/roleService',
    },
    building: {
      name: 'BuildingService',
      path: '../services/buildingService',
    },
    floor: {
      name: 'FloorService',
      path: '../services/floorService',
    },
    room: {
      name: 'RoomService',
      path: '../services/roomService',
    },
    passage: {
      name: 'PassageService',
      path: '../services/passageService',
    },
    robotType: {
      name: 'RobotTypeService',
      path: '../services/robotTypeService',
    },
    taskType: {
      name: 'TaskTypeService',
      path: '../services/taskTypeService',
    },
    elevator: {
      name: 'ElevatorService',
      path: '../services/elevatorService',
    },
    robot: {
      name: 'RobotService',
      path: '../services/robotService',
    },
    file: {
      name: 'FileService',
      path: '../services/fileService',
    },
    planning: {
      name: 'PlanningService',
      path: '../services/planningService',
    },
    user: {
      name: 'UserService',
      path: '../services/userService',
    },
    taskRequest: {
      name: 'TaskRequestService',
      path: '../services/taskRequestService',
    },
  },
};
