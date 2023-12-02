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
                {
                    "col": 10,
                    "row": 0
                },
                {
                    "col": 9,
                    "row": 1
                },
                {
                    "col": 8,
                    "row": 2
                },
                {
                    "col": 7,
                    "row": 3
                },
                {
                    "col": 7,
                    "row": 4
                },
                {
                    "col": 7,
                    "row": 5
                },
                {
                    "col": 7,
                    "row": 6
                },
                {
                    "col": 7,
                    "row": 7
                },
                {
                    "col": 7,
                    "row": 8
                },
                {
                    "col": 7,
                    "row": 9
                },
                {
                    "col": 7,
                    "row": 10
                },
                {
                    "col": 7,
                    "row": 11
                },
                {
                    "col": 7,
                    "row": 12
                },
                {
                    "col": 7,
                    "row": 13
                },
                {
                    "col": 7,
                    "row": 14
                },
                {
                    "col": 7,
                    "row": 15
                },
                {
                    "col": 7,
                    "row": 16
                },
                {
                    "col": 7,
                    "row": 17
                },
                {
                    "col": 7,
                    "row": 18
                },
                {
                    "col": 7,
                    "row": 19
                }
            ]
    }"""
    
    # width, height = 26, 16
    width, height = 16, 26
    cells = parse_path_list(path_str)
    grid = create_grid(cells, width, height)
    print_grid(grid)

if __name__ == "__main__":
    main()
