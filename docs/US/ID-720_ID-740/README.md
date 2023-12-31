# ID720, ID740 - Task, robot data exchange between modules for planning task execution




## Diagrams
### Process View
![PV](../../Sprint%20B%20diagrams/ID720_ID740_PV.svg)

## Data Models
### taskDTO
```javascript
interface TaskRequest {
  id: string;
  status: TaskStatus;
  requesterEmail: string;
  task: DeliveryTask | SurveillanceTask;
  requestCreatedDateTime: string;
  navigationData?: NavigationPlan;
}
```

### Planning Data DTO
```javascript
interface NavigationPlan {
  floorsConnectionsCost: number;
  floorsPaths: FloorPath[];
  mapPathCount: number;
  mapPaths: MapPath[];
}

interface FloorPath {
  fromBuilding: string;
  fromFloorNumber: string;
  toBuilding: string;
  toFloorNumber: string;
  type: string;
}

interface MapPath {
  buildingCode: string;
  cost: number;
  floorNumber: number;
  path: PathPoint[];
}

interface PathPoint {
  col: number;
  row: number;
}

```

### PlanningStatus DTO
```javascript
interface TaskPlanningStatus {
  message: string;
  state: 'unstarted' | 'planning' | 'planned' | 'error';
}
```

### PlanningResponse DTO
```javascript
type PlanningResponse = TaskPlan | PlanningError;

interface TaskPlan {
  data: TaskPlanData[];
}

interface TaskPlanData {
  costOfChange: number;
  robotId: string;
  status: string;
  tasks: string;
  type: 'taskPlan';
}

interface PlanningError {
  error: string;
}
```


### GET Route filtering query params
```javascript
type TaskFilters = {
  status?: TaskStatus;
  robotTypeName?: string;
  userEmail?: string;
};
```