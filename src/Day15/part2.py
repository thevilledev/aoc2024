from typing import Dict, List, Tuple

UP, DOWN, LEFT, RIGHT = (0, -1), (0, 1), (-1, 0), (1, 0)

class Day15:
    def solve(self, input_str: str) -> float:
        # Scale up map
        scaled = ''.join({'#': '##', '.': '..', 'O': '[]', '@': '@.'}.get(ch, ch) for ch in input_str)
        map_dict, steps = self.parse(scaled)

        # Find and move robot
        robot = next(pos for pos, ch in map_dict.items() if ch == '@')
        for d in steps:
            if self.try_move(map_dict, robot, d):
                robot = (robot[0] + d[0], robot[1] + d[1])

        return sum(p[0] + 100 * p[1] for p, ch in map_dict.items() if ch in ['[', 'O'])

    def try_move(self, m: Dict[tuple, str], pos: tuple, d: tuple, changes: Dict[tuple, str] = None) -> bool:
        """Try to move from pos in direction d, tracking changes in optional changes dict"""
        if changes is None:
            changes = {}
            success = self.try_move(m, pos, d, changes)
            if success:
                m.update(changes)
            return success

        curr = changes.get(pos, m.get(pos))
        if curr == '.': return True

        next_pos = (pos[0] + d[0], pos[1] + d[1])

        if curr in ['O', '@']:
            return self._move_object(m, pos, d, changes)

        elif curr == ']':
            return self.try_move(m, (pos[0] - 1, pos[1]), d, changes)

        elif curr == '[':
            return self._move_bracket(m, pos, d, changes, next_pos)
        return False

    def _move_object(self, m: Dict, pos: tuple, d: tuple, changes: Dict) -> bool:
        """Handle movement of objects (O or @)"""
        next_pos = (pos[0] + d[0], pos[1] + d[1])
        curr = changes.get(pos, m.get(pos))
        if self.try_move(m, next_pos, d, changes):
            changes[next_pos], changes[pos] = curr, '.'
            return True
        return False

    def _move_bracket(self, m: Dict, pos: tuple, d: tuple, changes: Dict, next_pos: tuple) -> bool:
        """Handle movement of bracket pairs ([])"""
        if d == LEFT:
            return self._move_bracket_left(m, pos, d, changes)
        elif d == RIGHT:
            return self._move_bracket_right(m, pos, d, changes)
        else:  # UP/DOWN
            return self._move_bracket_vertical(m, pos, d, changes, next_pos)

    def _move_bracket_left(self, m: Dict, pos: tuple, d: tuple, changes: Dict) -> bool:
        """Handle movement of bracket pair to the left"""
        target = (pos[0] - 1, pos[1])
        if self.try_move(m, target, d, changes):
            changes[target] = '['      # Move left bracket to new position
            changes[pos] = ']'         # Replace with right bracket
            changes[pos[0] + 1, pos[1]] = '.'  # Clear rightmost position
            return True
        return False

    def _move_bracket_right(self, m: Dict, pos: tuple, d: tuple, changes: Dict) -> bool:
        """Handle movement of bracket pair to the right"""
        target = (pos[0] + 2, pos[1])
        if self.try_move(m, target, d, changes):
            changes[target] = ']'      # Place right bracket at target
            changes[pos[0] + 1, pos[1]] = '['  # Move left bracket right
            changes[pos] = '.'         # Clear original position
            return True
        return False

    def _move_bracket_vertical(self, m: Dict, pos: tuple, d: tuple, changes: Dict, next_pos: tuple) -> bool:
        """Handle movement of bracket pair up or down"""
        target = next_pos
        target_right = (target[0] + 1, target[1])
        if (self.try_move(m, target, d, changes) and 
            self.try_move(m, target_right, d, changes)):
            changes[target] = '['      # Move left bracket to target
            changes[target_right] = ']'  # Move right bracket to target
            changes[pos] = '.'         # Clear original positions
            changes[pos[0] + 1, pos[1]] = '.'
            return True
        return False

    def parse(self, input_str: str) -> Tuple[Dict[tuple, str], List[tuple]]:
        map_str, moves = input_str.split('\n\n')
        # Parse map to dict
        map_dict = {(x, y): ch 
                   for y, line in enumerate(map_str.split('\n'))
                   for x, ch in enumerate(line)}

        # Parse moves to list of directions
        dir_map = {'^': UP, 'v': DOWN, '<': LEFT, '>': RIGHT}
        steps = [dir_map[ch] for ch in moves.strip().replace('\n', '')]

        return map_dict, steps

    def part_two(self, input_str: str) -> float:
        return self.solve(input_str)

if __name__ == "__main__":
    print(Day15().part_two(open("inputs/Day15/real.txt").read()))