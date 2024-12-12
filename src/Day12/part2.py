from typing import List, Tuple, Callable

def make_point(x: int, y: int) -> Tuple[int, int]:
    return (x, y)

def add_points(p1: Tuple[int, int], p2: Tuple[int, int]) -> Tuple[int, int]:
    return (p1[0] + p2[0], p1[1] + p2[1])

DIRECTIONS = [
    (0, -1),  # UP
    (1, 0),   # RIGHT
    (0, 1),   # DOWN
    (-1, 0)   # LEFT
]

def turn_clockwise(direction: int) -> int:
    return (direction + 1) % 4

def turn_counter_clockwise(direction: int) -> int:
    return (direction - 1) % 4

def is_in_bounds(point: Tuple[int, int], width: int, height: int) -> bool:
    return 0 <= point[0] < width and 0 <= point[1] < height

def get_cell(grid: List[List[str]], point: Tuple[int, int]) -> str:
    return grid[point[1]][point[0]]

def solve_part2(input_str: str) -> int:
    grid = [list(line) for line in input_str.splitlines()]
    height = len(grid)
    width = len(grid[0]) if grid else 0
    seen = [[False] * width for _ in range(height)]
    total = 0

    for y in range(height):
        for x in range(width):
            point = (x, y)

            if seen[y][x]:
                continue

            kind = get_cell(grid, point)
            todo = [point]
            edge = []
            seen[y][x] = True
            area = 0

            def check(p: Tuple[int, int]) -> bool:
                return is_in_bounds(p, width, height) and get_cell(grid, p) == kind

            while area < len(todo):
                point = todo[area]
                area += 1

                for d_idx, direction in enumerate(DIRECTIONS):
                    next_point = add_points(point, direction)

                    if check(next_point):
                        if not seen[next_point[1]][next_point[0]]:
                            todo.append(next_point)
                            seen[next_point[1]][next_point[0]] = True
                    else:
                        edge.append((point, d_idx))

            sides = 0
            for p, d in edge:
                r = DIRECTIONS[turn_clockwise(d)]
                l = DIRECTIONS[turn_counter_clockwise(d)]

                sides += int(not check(add_points(p, l)) or 
                           check(add_points(add_points(p, l), DIRECTIONS[d])))
                sides += int(not check(add_points(p, r)) or 
                           check(add_points(add_points(p, r), DIRECTIONS[d])))

            total += area * (sides // 2)

    return total

if __name__ == "__main__":
    with open("inputs/Day12/real.txt") as f:
        example_input = f.read().strip()

    result = solve_part2(example_input)
    print(f"Part 2 result: {result}")