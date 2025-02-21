def parse_prolog_list(prolog_str):
    cells = []
    prolog_str = prolog_str.strip("[]")
    prolog_list = prolog_str.split("),")
    for item in prolog_list:
        item = item.strip("cel()")
        if item:  # Check if the item is not empty
            parts = item.split(",")
            cells.append((int(parts[0]), int(parts[1])))
    return cells

def create_grid(cells, width, height):
    grid = [['.' for _ in range(width)] for _ in range(height)]

    for x, y in cells:
        if 1 <= x <= width and 1 <= y <= height:
            grid[y-1][x-1] = '#'

    return grid

def print_grid(grid):
    for row in grid:
        print(' '.join(row))

def main():
    prolog_str = "[cel(1,8),cel(1,9),cel(1,10),cel(2,9),cel(3,10),cel(4,11),cel(5,12),cel(6,12),cel(7,12),cel(8,12),cel(9,12),cel(10,12),cel(11,12),cel(11,11),cel(11,10),cel(11,9),cel(11,8),cel(11,7),cel(11,6),cel(11,5),cel(11,4),cel(12,4),cel(13,4),cel(14,4),cel(15,4),cel(16,4),cel(17,5),cel(17,6),cel(17,7),cel(17,8),cel(17,9),cel(17,10),cel(17,11),cel(17,12),cel(18,12),cel(19,12),cel(20,12),cel(21,12),cel(22,12),cel(23,11),cel(24,10),cel(25,10)]"
    width, height = 26, 16
    cells = parse_prolog_list(prolog_str)
    grid = create_grid(cells, width, height)
    
    start_cell = cells[0] if cells else None
    end_cell = cells[-1] if cells else None

    print(f"Start Cell: {start_cell}, End Cell: {end_cell}")
    print(f"Map Width x Height: {width}x{height}")
    print("Grid (top-left cell is (1, 1)):")
    print_grid(grid)

if __name__ == "__main__":
    main()
