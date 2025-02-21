import json

def parse_path_list(path_str):
    path_data = json.loads(path_str)
    cells = [(item['col'], item['row']) for item in path_data['path']]
    return cells

def create_grid(cells, width, height):
    grid = [['.' for _ in range(width)] for _ in range(height)]

    for col, row in cells:
        if 0 <= row < height and 0 <= col < width:
            grid[row][col] = '#'

    return grid

def print_grid(grid):
    for row in grid:
        print(' '.join(row))

def main():
    path_str = """{
        "path": [
    {"col":23, "row":0},
	{"col":23, "row":1},
	{"col":23, "row":2},
	{"col":23, "row":3},
	{"col":22, "row":3},
	{"col":21, "row":3},
	{"col":20, "row":3},
	{"col":19, "row":3},
	{"col":18, "row":3},
	{"col":17, "row":4},
	{"col":16, "row":4},
	{"col":15, "row":5},
	{"col":14, "row":5},
	{"col":13, "row":6},
	{"col":12, "row":6},
	{"col":11, "row":7},
	{"col":10, "row":7}
            ]
    }"""
    
    width, height = 26, 16
    # width, height = 16, 26
    cells = parse_path_list(path_str)
    grid = create_grid(cells, width, height)
    print_grid(grid)

if __name__ == "__main__":
    main()
