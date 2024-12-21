from collections import deque
from functools import cache
from typing import List, Tuple

Grid = List[List[str]]
Path = List[str]
Position = Tuple[int, int]

NUMERIC_PAD: Grid = [
    ["7", "8", "9"],
    ["4", "5", "6"],
    ["1", "2", "3"],
    ["X", "0", "A"]
]

ARROW_PAD: Grid = [
    ["X", "^", "A"],
    ["<", "v", ">"]
]

DIRECTIONS: List[Tuple[Position, str]] = [
    ((0, 1), ">"),
    ((1, 0), "v"),
    ((0, -1), "<"),
    ((-1, 0), "^")
]

def locate(grid: Grid, target: str) -> Position:
    """Find coordinates of target character in grid."""
    return next((r, c) for r in range(len(grid)) 
                for c in range(len(grid[0])) 
                if grid[r][c] == target)

def find_shortest_paths(grid: Grid, start: Position, end: Position) -> List[str]:
    """Find all shortest paths between two points in grid."""
    q = deque([(start[0], start[1], "")])
    p = []
    min_len = None  # Track first path length we find

    while q:
        r, c, path = q.popleft()
        if min_len is not None and len(path) > min_len:
            continue

        if (r, c) == end:
            if min_len is None:
                min_len = len(path)
            if len(path) == min_len:
                p.append(path + 'A')
            continue

        for (dr, dc), symbol in DIRECTIONS:
            nr, nc = r + dr, c + dc
            if (0 <= nr < len(grid) and 0 <= nc < len(grid[0]) and grid[nr][nc] != 'X'):
                q.append((nr, nc, path + symbol))

    return p

@cache
def compute_arrow_sequence(input_seq: str, remaining_depth: int) -> int:
    """Compute length of arrow pad sequence recursively."""
    # Base case - no more recursion needed
    if remaining_depth == 0:
        return len(input_seq)

    current_pos = locate(ARROW_PAD, 'A')
    sequence_length = 0

    # Process each character in sequence
    for next_char in input_seq:
        target_pos = locate(ARROW_PAD, next_char)
        possible_paths = find_shortest_paths(ARROW_PAD, current_pos, target_pos)
        if possible_paths:
            # Recursively find minimum length for each possible path
            min_path_length = min(
                compute_arrow_sequence(path, remaining_depth - 1) 
                for path in possible_paths
            )
            sequence_length += min_path_length

        current_pos = target_pos

    return sequence_length

def compute_sequence_length(input_code: str, robot_depth: int) -> int:
    """Compute total sequence length for given code and depth."""
    current_position = locate(NUMERIC_PAD, 'A')
    sequence_length = 0

    for target_char in input_code:
        target_position = locate(NUMERIC_PAD, target_char)
        possible_paths = find_shortest_paths(NUMERIC_PAD, current_position, target_position)
        if possible_paths:
            sequence_length += min(compute_arrow_sequence(path, robot_depth) for path in possible_paths)
        current_position = target_position

    return sequence_length

def solve(input_data: str, robot_depth: int) -> int:
    """Solve puzzle for given input and robot depth."""
    return sum(
        int(''.join(c for c in code if c.isdigit())) * 
        compute_sequence_length(code, robot_depth)
        for code in input_data.strip().split('\n')
    )

if __name__ == "__main__":
    with open("inputs/Day21/real.txt") as f:
        data = f.read()
        print(f"\t* Part 1: {solve(data, 2)}")
        print(f"\t* Part 2: {solve(data, 25)}")
