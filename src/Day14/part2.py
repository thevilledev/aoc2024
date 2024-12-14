from dataclasses import dataclass
from typing import List, Tuple, Set
import numpy as np
from sklearn.cluster import DBSCAN
import re
from math import gcd
from functools import reduce
from pathlib import Path

@dataclass(frozen=True)  # Make Robot immutable
class Robot:
    """Represents a robot with position and velocity."""
    x: int
    y: int
    vx: int
    vy: int

    def position_at(self, t: int, width: int, height: int) -> Tuple[int, int]:
        """Calculate position at time t with wrapping boundaries."""
        x = (self.x + self.vx * t) % width
        y = (self.y + self.vy * t) % height
        return (int(x), int(y))

class PatternDetector:
    """Detects patterns in robot movements using statistical analysis."""

    # Class constants
    ROBOT_PATTERN = re.compile(r'p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)')
    DBSCAN_PARAMS = {'eps': 3, 'min_samples': 4}
    SCORE_WEIGHTS = {
        'std_dev': 1.0,
        'density': 2.0,
        'height_ratio': 1.5,
        'cluster': 2.0
    }

    def __init__(self, width: int = 101, height: int = 103):
        self.width = width
        self.height = height
        self.robots: List[Robot] = []

    def parse_input(self, input_lines: List[str]) -> None:
        """Parse input lines into Robot objects."""
        self.robots = []
        for line in input_lines:
            if match := self.ROBOT_PATTERN.match(line):
                x, y, vx, vy = map(int, match.groups())
                self.robots.append(Robot(x, y, vx, vy))

    @staticmethod
    def _calculate_metrics(positions: List[Tuple[int, int]]) -> Tuple[float, float, float, int]:
        """Calculate statistical metrics for a set of positions."""
        if not positions:
            return float('inf'), 0, 0, 0

        pos_array = np.array(positions)
        center = np.mean(pos_array, axis=0)
        distances = np.sqrt(np.sum((pos_array - center)**2, axis=1))

        std_dev = np.std(distances)
        radius = max(distances) if distances.size > 0 else 0
        density = len(positions) / (radius**2 * np.pi) if radius > 0 else len(positions)

        y_coords = pos_array[:, 1]
        height_span = max(y_coords) - min(y_coords) if len(y_coords) > 1 else 1
        height_ratio = len(positions) / height_span if height_span != 0 else 0

        clustering = DBSCAN(**PatternDetector.DBSCAN_PARAMS).fit(pos_array)
        n_clusters = len(set(clustering.labels_) - {-1})

        return std_dev, density, height_ratio, n_clusters

    def _find_periods(self) -> Set[int]:
        """Find fundamental periods based on robot velocities."""
        def lcm(a: int, b: int) -> int:
            return abs(a * b) // gcd(a, b)

        x_periods = [self.width // abs(r.vx) for r in self.robots if r.vx != 0]
        y_periods = [self.height // abs(r.vy) for r in self.robots if r.vy != 0]

        if not (periods := x_periods + y_periods):
            return {1}

        period = reduce(lcm, periods)
        base_gcd = reduce(gcd, periods)
        return set(range(0, min(period, 10000), base_gcd))

    def _score_pattern(self, metrics: Tuple[float, float, float, int]) -> float:
        """Score a pattern based on its metrics."""
        std_dev, density, height_ratio, n_clusters = metrics
        weights = self.SCORE_WEIGHTS

        std_score = 1 / (1 + std_dev)
        cluster_score = 1 / (1 + abs(1 - n_clusters))

        return (std_score * weights['std_dev'] +
                density * weights['density'] +
                height_ratio * weights['height_ratio'] +
                cluster_score * weights['cluster'])

    def _visualize(self, positions: List[Tuple[int, int]]) -> None:
        """Visualize the pattern in ASCII art."""
        grid = np.zeros((self.height, self.width))
        for x, y in positions:
            grid[y, x] += 1

        if not (active_cells := np.nonzero(grid)):
            return

        min_row, max_row = min(active_cells[0]), max(active_cells[0])
        min_col, max_col = min(active_cells[1]), max(active_cells[1])

        print("\nPattern visualization:")
        for y in range(min_row, max_row + 1):
            print(''.join('#' if grid[y, x] > 0 else '.' 
                         for x in range(min_col, max_col + 1)))

    def find_pattern_time(self, max_time: int = 1000) -> int:
        """Find the time when robots form the most significant pattern."""
        candidate_times = self._find_periods()
        print(f"Analyzing {len(candidate_times)} potential time points")

        best_score = 0
        best_time = -1

        for t in sorted(candidate_times):
            if t % 100 == 0:
                print(f"Analyzing time {t}...")

            positions = [robot.position_at(t, self.width, self.height) 
                        for robot in self.robots]
            metrics = self._calculate_metrics(positions)
            score = self._score_pattern(metrics)

            if score > best_score:
                best_score = score
                best_time = t

        if best_time != -1:
            print(f"\nBest pattern found at time {best_time} with score {best_score:.2f}")
            self._visualize([robot.position_at(best_time, self.width, self.height) 
                           for robot in self.robots])

        return best_time

def solve_part2(input_lines: List[str]) -> int:
    """Solve part 2 of the puzzle."""
    detector = PatternDetector()
    detector.parse_input(input_lines)
    return detector.find_pattern_time()

if __name__ == "__main__":
    input_path = Path('inputs/Day14/real.txt')
    input_lines = input_path.read_text().splitlines()
    print(solve_part2(input_lines))