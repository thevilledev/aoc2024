from typing import List, Tuple
from collections import defaultdict

def parse_disk_map(disk_map: str) -> List[Tuple[int, bool]]:
    """
    Parse the disk map into list of (length, is_file) tuples.
    True for file blocks, False for free space.
    """
    return [(int(c), i % 2 == 0) for i, c in enumerate(disk_map)]

def expand_to_blocks(disk_map: str) -> List[int]:
    """
    Convert the compact disk map into a list where each element represents one block.
    Numbers represent file IDs, -1 represents free space.
    """
    sections = parse_disk_map(disk_map)
    blocks = []
    file_id = 0
    
    for length, is_file in sections:
        if is_file:
            blocks.extend([file_id] * length)
            file_id += 1
        else:
            blocks.extend([-1] * length)
    
    return blocks

def get_file_info(blocks: List[int]) -> dict:
    """
    Get information about each file's size and position.
    Returns a dictionary with file IDs as keys and tuples of (start_pos, length) as values.
    """
    file_info = {}
    current_file = None
    start_pos = None
    length = 0
    
    for pos, block in enumerate(blocks + [-1]):  # Add sentinel value
        if block != current_file:
            if current_file is not None and current_file != -1:
                file_info[current_file] = (start_pos, length)
            current_file = block
            start_pos = pos
            length = 1
        else:
            length += 1
    
    return file_info

def find_leftmost_space(blocks: List[int], required_size: int) -> int:
    """
    Find the leftmost position of a continuous free space that can fit the required size.
    Returns -1 if no suitable space is found.
    """
    current_space = 0
    start_pos = -1
    
    for pos, block in enumerate(blocks):
        if block == -1:
            if current_space == 0:
                start_pos = pos
            current_space += 1
            if current_space >= required_size:
                return start_pos
        else:
            current_space = 0
            start_pos = -1
    
    return -1

def compact_disk_whole_files(blocks: List[int]) -> List[int]:
    """
    Compact the disk by moving whole files at once, starting with highest file ID.
    Returns the final state of the disk.
    """
    result = blocks.copy()
    file_info = get_file_info(result)
    
    # Process files in order of decreasing file ID
    for file_id in sorted(file_info.keys(), reverse=True):
        start_pos, length = file_info[file_id]
        
        # Try to find leftmost suitable free space
        target_pos = find_leftmost_space(result[:start_pos], length)
        
        if target_pos != -1:
            # Move the whole file
            file_blocks = [file_id] * length
            free_blocks = [-1] * length
            
            # Copy file to new position
            result[target_pos:target_pos + length] = file_blocks
            # Clear old position
            result[start_pos:start_pos + length] = free_blocks
            
            # Update file info
            file_info[file_id] = (target_pos, length)
    
    return result

def calculate_checksum(blocks: List[int]) -> int:
    """
    Calculate the filesystem checksum based on position * file ID for each block.
    """
    return sum(pos * file_id 
              for pos, file_id in enumerate(blocks) 
              if file_id != -1)

def solve_disk_compacting_part2(disk_map: str) -> int:
    """
    Main solving function for part 2 that processes the disk map and returns the checksum.
    """
    blocks = expand_to_blocks(disk_map)
    compacted_blocks = compact_disk_whole_files(blocks)
    return calculate_checksum(compacted_blocks)

# Read input from file
with open("../../inputs/Day09/real.txt", "r") as file:
    puzzle_input = file.read().strip()

# Solve part 2
result = solve_disk_compacting_part2(puzzle_input)
print(f"Part 2 Solution: {result}")

# Verify with example
example = "2333133121414131402"
example_result = solve_disk_compacting_part2(example)
print(f"Example solution (should be 2858): {example_result}")