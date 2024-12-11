def solve_part2(input_str: str) -> int:
    numbers = [int(x) for x in input_str.split()]

    # Track just counts in buckets based on number properties
    # Each bucket is (number, count)
    buckets = {}

    # Initialize buckets
    for n in numbers:
        buckets[n] = buckets.get(n, 0) + 1

    # Process all iterations
    for i in range(75):
        print(i)
        new_buckets = {}

        for number, count in buckets.items():
            if number == 0:
                # Rule 1: 0 becomes 1
                new_buckets[1] = new_buckets.get(1, 0) + count
            else:
                str_n = str(number)
                if len(str_n) % 2 == 0:
                    # Rule 2: Even length numbers split into two
                    half = len(str_n) // 2
                    left = int(str_n[:half])
                    right = int(str_n[half:])
                    new_buckets[left] = new_buckets.get(left, 0) + count
                    new_buckets[right] = new_buckets.get(right, 0) + count
                else:
                    # Rule 3: Multiply by 2024
                    new_num = number * 2024
                    new_buckets[new_num] = new_buckets.get(new_num, 0) + count
        
        buckets = new_buckets
    
    # Return total number of stones
    return sum(buckets.values())

def solve():
    with open("../../inputs/Day11/real2.txt") as f:
        input_str = f.read().strip()
    result = solve_part2(input_str)
    print(f"\t* Part 2: {result}")

solve()