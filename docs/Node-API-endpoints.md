# Node API endpoints
*(as of 13/11/2023)*

# Contents

1. [Building Endpoints](#building-endpoints)
2. [Floor Endpoints](#floor-endpoints)
3. [Passage Endpoints](#passage-endpoints)
4. [Robot Endpoints](#robot-endpoints)
5. [RobotType Endpoint](#robottype-endpoint)
6. [Roles Endpoints](#roles-endpoints)
7. [Room Endpoint](#room-endpoint)
8. [Auth and Users Endpoints](#auth-and-users-endpoints)

## Building Endpoints

- `POST /building` 
  - Body parameters:
    - `name`: Optional string
    - `code`: Required string
    - `description`: Optional string
    - `floorSizeLength`: Required number
    - `floorSizeWidth`: Required number

- `PUT /building/:id` 
  - Body parameters:
    - `name`: Optional string
    - `description`: Optional string
    - `floorSizeLength`: Optional number
    - `floorSizeWidth`: Optional number
  - Path parameters:
    - `id`: Required string

- `PATCH /building/:id`
  - Body parameters:
    - `name`: Optional string
    - `description`: Optional string
    - `floorSizeLength`: Optional number
    - `floorSizeWidth`: Optional number
  - Path parameters:
    - `id`: Required string

- `GET /building`
  - Query parameters (optional):
    - `minFloor`: Required integer (if present)
    - `maxFloor`: Required integer (if present)

- `POST /building/:code/elevator`
  - Body parameters:
    - `number`: Required number
    - `make`: Optional string
    - `model`: Optional string
    - `serialNumber`: Optional string
    - `description`: Optional string
    - `floors`: Required array of numbers
  - Path parameters:
    - `code`: Required string

- `PUT /building/:code/elevator`
  - Body parameters:
    - `make`: Required string
    - `model`: Required string
    - `serialNumber`: Required string
    - `description`: Required string
  - Path parameters:
    - `code`: Required string

- `PATCH /building/:code/elevator`
  - Body parameters:
    - `make`: Optional string
    - `model`: Optional string
    - `serialNumber`: Optional string
    - `description`: Optional string
  - Path parameters:
    - `code`: Required string

- `GET /building/all`
  - No parameters

- `GET /building/:code/elevators`
  - Path parameters:
    - `code`: Required string

- `GET /building/:code`
  - Path parameters:
    - `code`: Required string

## Floor Endpoints

- `POST /floor`
  - Body parameters:
    - `floorNumber`: Required number
    - `description`: Optional string
    - `servedByElevator`: Required boolean
    - `buildingCode`: Required string

- `PUT /floor/:floorId`
  - Body parameters:
    - `description`: Optional string
  - Path parameters:
    - `floorId`: Required string

- `PATCH /floor/:floorId`
  - Body parameters:
    - `description`: Optional string
  - Path parameters:
    - `floorId`: Required string

- `GET /floor/elevator`
  - Query parameters:
    - `buildingCode`: Required string

- `GET /floor`
  - No parameters

- `GET /floor/:buildingCode`
  - Path parameters:
    - `buildingCode`: Required string

- `PATCH /floor/:floorNumber/building/:buildingCode`
  - Body parameters:
    - `map`: Required object with nested `size` (required, with `width` and `height` numbers) and `map` (required, array of arrays of numbers)
  - Path parameters:
    - `floorNumber`: Required number
    - `buildingCode`: Required string


## Passage Endpoints

- `POST /passage`
  - Body parameters:
    - `buildingCode1`: Required string
    - `buildingCode2`: Required string
    - `floorNumber1`: Required integer
    - `floorNumber2`: Required integer

- `GET /passage`
  - Query parameters (must include both or none):
    - `buildingCode1`: Optional string
    - `buildingCode2`: Optional string

- `GET /passage/toDifferentBuildings`
  - Query parameters:
    - `buildingCode`: Required string

- `PUT /passage`
  - Body parameters:
    - `oldPassage`: Required object
      - `buildingCode1`: Required string
      - `buildingCode2`: Required string
      - `floorNumber1`: Required integer
      - `floorNumber2`: Required integer
    - `newPassage`: Required object
      - `buildingCode1`: Required string
      - `buildingCode2`: Required string
      - `floorNumber1`: Required integer
      - `floorNumber2`: Required integer


## Robot Endpoints

- `POST /robot`
  - Body parameters:
    - `code`: Required string
    - `nickname`: Required string
    - `serialNumber`: Required string
    - `description`: Optional string
    - `type`: Required string

- `PATCH /robot/:robotCode/state`
  - Body parameters:
    - `available`: Required boolean
  - Path parameters:
    - `robotCode`: Required string

- `GET /robot`
  - Query parameters (all optional):
    - `id`: Optional string
    - `name`: Optional string
    - `type`: Optional string
    - `brand`: Optional string
    - `model`: Optional string
    - `typesOfTasks`: Optional array of strings


## RobotType Endpoint

- `POST /robotType`
  - Body parameters:
    - `name`: Required string
    - `brand`: Required string
    - `model`: Required string
    - `typesOfTasks`: Optional array of strings


## Roles Endpoints

- `POST /roles`
  - Body parameters:
    - `name`: Required string

- `PUT /roles`
  - Body parameters:
    - `id`: Required string
    - `name`: Required string


## Room Endpoint

- `POST /room`
  - Body parameters:
    - `name`: Required string
    - `description`: Optional string
    - `size`: Required object
      - `width`: Required integer
      - `length`: Required integer
    - `position`: Required object
      - `x`: Required number
      - `y`: Required number
    - `category`: Required string
    - `floorId`: Required string


## Auth Endpoints

- `POST /auth/signup`
  - Body parameters:
    - `firstName`: Required string
    - `lastName`: Required string
    - `email`: Required string
    - `password`: Required string
    - `role`: Required string

- `POST /auth/signin`
  - Body parameters:
    - `email`: Required string
    - `password`: Required string

- `POST /auth/logout`
  - Requires authentication middleware
  - No body parameters

## Users Endpoint

- `GET /users/me`
  - Requires authentication and current user attachment middleware
  - No body parameters