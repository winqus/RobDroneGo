# Planning Prolog API

### Running the API
1. Install SWI-Prolog
2. Run the following command in the terminal (adjust the port number if needed):
`swipl -s src/server.pl -g "start_server(4400)"`
stop with `halt.`

or in PowerShell (opens new window):
Start-Process swipl -ArgumentList "-s src/server.pl -g `"start_server(4400)`""

or in Prolog:
`?- start_server(4400).`
3. The API should be running on port 4400.

### API Endpoints
- GET /planning-api/route (with query parameters, see example below)

### Example Request
```
	http://localhost:4400/planning-api/route?
									origin_building_code=B&
									origin_floor_number=1&
									origin_map_cell_x=1&
									origin_map_cell_y=1&
									destination_building_code=C&
									destination_floor_number=2&
									destination_map_cell_x=3&
									destination_map_cell_y=3&
									minimize_elevator_uses=true&
									minimize_building_count=true
```

### Cell types
* NoNorthWallNoWestWall = 0
* NoNorthWallYesWestWall = 1
* YesNorthWallNoWestWall = 2
* YesNorthWallYesWestWall = 3
* DoorNorth = 4
* DoorWest = 5
* PassageNorth = 6
* PassageWest = 7
* ElevatorNorth = 8
* ElevatorSouth = 9
* ElevatorWest = 10
* ElevatorEast = 11
