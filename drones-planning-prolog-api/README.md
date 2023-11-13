# Planning Prolog API
### Running the API
1. Install SWI-Prolog
2. Run the following command in the terminal (adjust the port number if needed):
`swipl -s src/server.pl -g "start_server(4400)"`
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