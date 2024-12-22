from collections import deque
from functools import reduce

def evolve_secret(num: int) -> int:
    """All operations are done modulo 16777216 (2^24) to keep numbers manageable."""
    num = (num ^ (num * 64)) & 0xFFFFFF
    num = (num ^ (num // 32)) & 0xFFFFFF
    num = (num ^ (num * 2048)) & 0xFFFFFF
    return num

def calculate_2000th_secrets() -> int:
    """Calculate sum of 2000th secret number for each buyer."""
    with open("inputs/Day22/real.txt") as f:
        initial_secrets = [int(x) for x in f.read().splitlines()]
    return sum(reduce(lambda s, _: evolve_secret(s), range(2000), secret) for secret in initial_secrets)

def analyze_patterns() -> int:
    """Analyze patterns in the last digits of evolved secrets."""
    with open("inputs/Day22/real.txt") as f:
        initial_secrets = [int(x) for x in f.read().splitlines()]

    pattern_counts = {}

    for secret in initial_secrets:
        # Track differences between consecutive last digits
        last_4_diffs = deque(maxlen=4)
        seen_patterns = set()

        for _ in range(2000):
            prev_digit = secret % 10
            secret = evolve_secret(secret)
            curr_digit = secret % 10

            last_4_diffs.append(curr_digit - prev_digit)

            if len(last_4_diffs) == 4:
                pattern = tuple(last_4_diffs)
                if pattern not in seen_patterns:
                    seen_patterns.add(pattern)
                    pattern_counts[pattern] = pattern_counts.get(pattern, 0) + curr_digit

    return max(pattern_counts.values())

if __name__ == "__main__":
    print(f"* Day 22")
    print(f"\t* Part 1: {calculate_2000th_secrets()}")
    print(f"\t* Part 2: {analyze_patterns()}")
