from math import gcd
from typing import Optional, Tuple
import logging

from math import gcd
from typing import Optional, Tuple

def solve_claw_machines(text: str) -> int:
    def solve_machine(ax: int, ay: int, bx: int, by: int, base_px: int, base_py: int) -> int:
        print(f"\nSolving machine: A({ax},{ay}), B({bx},{by}), Prize({base_px},{base_py})")
        offset = 10_000_000_000_000
        px = base_px + offset
        py = base_py + offset

        # Calculate GCD and check if solution exists
        det = ax * by - ay * bx
        if det == 0:
            print("No solution - determinant is zero")
            return 0

        print(f"det={det}")
        print(f"px={px}, py={py}")

        # First equation: ax*A + bx*B = px
        # Second equation: ay*A + by*B = py

        # Multiply first by by, second by bx and subtract
        # (ax*by - ay*bx)*A = px*by - py*bx

        A_num = px * by - py * bx
        print(f"A_num={A_num}")

        if A_num % det != 0:
            print("No integer solution exists")
            return 0

        A = A_num // det
        print(f"A={A}")

        # Calculate B using first equation: B = (px - ax*A)/bx
        # Use the equation with larger coefficient to minimize rounding errors
        if abs(bx) > abs(by):
            B = (px - ax * A) // bx
        else:
            B = (py - ay * A) // by

        print(f"B={B}")

        # Check if solution is valid and positive
        if A < 0 or B < 0:
            print("Negative solution found")
            return 0

        # Verify solution satisfies both equations exactly
        if (ax * A + bx * B == px and ay * A + by * B == py):
            tokens = 3 * A + B
            print(f"Valid solution found: A={A}, B={B}, tokens={tokens}")
            return tokens
        else:
            print("Solution verification failed")
            print(f"X eq: {ax * A + bx * B} vs {px}")
            print(f"Y eq: {ay * A + by * B} vs {py}")
            return 0

    total = 0
    for block in text.strip().split('\n\n'):
        lines = block.strip().split('\n')
        ax = int(lines[0].split('X+')[1].split(',')[0])
        ay = int(lines[0].split('Y+')[1])
        bx = int(lines[1].split('X+')[1].split(',')[0])
        by = int(lines[1].split('Y+')[1])
        px = int(lines[2].split('X=')[1].split(',')[0])
        py = int(lines[2].split('Y=')[1])

        tokens = solve_machine(ax, ay, bx, by, px, py)
        if tokens > 0:
            total += tokens
            print(f"Machine solved! Adding {tokens} tokens to total")

    print(f"\nTotal tokens needed: {total}")
    return total

if __name__ == '__main__':
    test_input = """Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"""

    ip2 = open("inputs/Day13/real.txt").read()

    result = solve_claw_machines(ip2)