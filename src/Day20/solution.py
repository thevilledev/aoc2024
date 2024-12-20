from collections import deque
from typing import Dict, Tuple

# Type alias for grid coordinates (row, column)
Coordinate = Tuple[int, int] 
# Possible movement directions: up, down, left, right
MOVEMENT_VECTORS = ((-1, 0), (1, 0), (0, -1), (0, 1))

def solve(file: str = "inputs/Day20/real.txt", max_wall_moves: int = 2) -> int:
    # Initialize grid and extract key elements:
    # - Convert input to 2D list
    # - Create set of wall coordinates for O(1) lookup
    # - Find start 'S' and end 'E' positions
    grid = [list(line) for line in open(file).read().splitlines()]
    rows, cols = len(grid), len(grid[0])
    walls = {(r, c) for r in range(rows) for c in range(cols) if grid[r][c] == '#'}
    start = next((r, c) for r in range(rows) for c in range(cols) if grid[r][c] == 'S')
    end = next((r, c) for r in range(rows) for c in range(cols) if grid[r][c] == 'E')

    # Calculate baseline shortest path length without cheating
    # Uses BFS since all steps have equal cost (1)
    queue = deque([(0, start[0], start[1])])  # Queue: (distance, row, col)
    seen = {}  # Tracks shortest distance to each position
    while queue:
        dist, r, c = queue.popleft()
        if (r, c) in seen: continue  # Skip if already visited
        seen[(r, c)] = dist
        if (r, c) == end: break  # Found shortest path to end
        # Explore all four directions
        for dr, dc in MOVEMENT_VECTORS:
            nr, nc = r + dr, c + dc
            if 0 <= nr < rows and 0 <= nc < cols and (nr, nc) not in walls and (nr, nc) not in seen:
                queue.append((dist + 1, nr, nc))
    legal_path_length = seen[end]

    # Helper function to calculate distances from a given position to all reachable positions
    # Uses BFS since all step costs are equal (1)
    def get_distances(origin: Coordinate) -> Dict[Coordinate, int]:
        distances = {}  # Maps positions to their distances from origin
        queue = deque([(0, origin[0], origin[1])])
        visited = set()
        while queue:
            dist, r, c = queue.popleft()
            if (r, c) in visited: continue
            visited.add((r, c))
            distances[(r, c)] = dist
            # Explore neighbors
            for dr, dc in MOVEMENT_VECTORS:
                nr, nc = r + dr, c + dc
                if 0 <= nr < rows and 0 <= nc < cols and (nr, nc) not in walls and (nr, nc) not in visited:
                    queue.append((dist + 1, nr, nc))
        return distances

    # Pre-calculate distances from start and end to all reachable positions
    # This avoids recalculating paths for each potential cheat
    from_start = get_distances(start)
    from_end = get_distances(end)

    # Find all valid shortcuts by trying each possible cheat sequence
    # A cheat sequence starts from any reachable position and can move through walls
    time_gains = []  # Track how much time each shortcut saves
    for sr in range(rows):  # Try every position as cheat start
        for sc in range(cols):
            if (sr, sc) in walls or (sr, sc) not in from_start: continue  # Must start from reachable position
            queue = deque([(sr, sc, 0)])  # Track position and steps used
            seen = set()
            while queue:
                r, c, steps = queue.popleft()
                if steps > max_wall_moves: continue  # Exceeded allowed wall moves
                # If current position is valid and reachable from end, calculate time gained
                if (r, c) not in walls and (r, c) in from_end:
                    total_time = from_start[(sr, sc)] + steps + from_end[(r, c)]
                    if total_time < legal_path_length:  # Found a faster path
                        time_gains.append(legal_path_length - total_time)
                # Continue exploring if we can still move through walls
                if steps < max_wall_moves:
                    for dr, dc in MOVEMENT_VECTORS:
                        nr, nc = r + dr, c + dc
                        if 0 <= nr < rows and 0 <= nc < cols and (nr, nc) not in seen:
                            seen.add((nr, nc))
                            queue.append((nr, nc, steps + 1))

    # Count paths that reduce time by at least 100 units
    return len([gain for gain in time_gains if gain >= 100])

if __name__ == "__main__":
    print("* Day 20")
    print(f"\t* Part 1: {solve()}")  # Default max_wall_moves=2
    print(f"\t* Part 2: {solve(max_wall_moves=20)}")  # Extended wall moves
