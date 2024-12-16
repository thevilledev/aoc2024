from enum import Enum
from dataclasses import dataclass
from typing import List, Set, Tuple

class Direction(Enum):
    NORTH = (0, -1)
    EAST = (1, 0)
    SOUTH = (0, 1)
    WEST = (-1, 0)

@dataclass(frozen=True)
class Position:
    x: int
    y: int

    def move(self, direction: Direction) -> 'Position':
        dx, dy = direction.value
        return Position(self.x + dx, self.y + dy)

def parse_grid(input_str: str) -> Tuple[List[List[str]], Position, Position]:
    grid = [list(line) for line in input_str.splitlines()]
    start = end = None
    for y in range(len(grid)):
        for x in range(len(grid[0])):
            if grid[y][x] == 'S':
                start = Position(x, y)
            elif grid[y][x] == 'E':
                end = Position(x, y)
    return grid, start, end

def is_valid(pos: Position, grid: List[List[str]]) -> bool:
    return (0 <= pos.y < len(grid) and 
            0 <= pos.x < len(grid[0]) and 
            grid[pos.y][pos.x] != '#')

def find_all_optimal_paths(grid: List[List[str]], start: Position, end: Position) -> Set[Position]:
    # Track all complete routes
    routes = []
    # Track best scores for visited positions and directions
    visited = {}

    # Queue: (position, path_history, score, direction)
    queue = [(start, [start], 0, Direction.EAST)]

    while queue:
        pos, history, score, direction = queue.pop(0)

        # Found end - save the route
        if pos == end:
            routes.append((history, score))
            continue

        # Skip if we've seen better
        if (pos, direction) in visited and visited[(pos, direction)] < score:
            continue

        visited[(pos, direction)] = score

        # Try moving forward
        next_pos = pos.move(direction)
        if is_valid(next_pos, grid) and next_pos not in history:
            queue.append((next_pos, history + [next_pos], score + 1, direction))

        # Try turning left/right (only turn, don't move)
        for new_dir in [Direction.NORTH, Direction.EAST, Direction.SOUTH, Direction.WEST]:
            if new_dir != direction:
                queue.append((pos, history[:], score + 1000, new_dir))

    # Find minimum score and collect all positions from optimal paths
    if not routes:
        return set()

    min_score = min(score for _, score in routes)
    optimal_paths = [path for path, score in routes if score == min_score]

    # Collect all positions that are part of any optimal path
    return set(pos for path in optimal_paths for pos in path)

def solve(input_str: str) -> int:
    grid, start, end = parse_grid(input_str)
    optimal_positions = find_all_optimal_paths(grid, start, end)

    # Debug print
    for y in range(len(grid)):
        for x in range(len(grid[0])):
            pos = Position(x, y)
            if grid[y][x] == '#':
                print('#', end='')
            elif pos in optimal_positions:
                print('O', end='')
            else:
                print('.', end='')
        print()

    return len(optimal_positions)

if __name__ == "__main__":
    with open("inputs/Day16/real.txt") as f:
        print(solve(f.read())) 