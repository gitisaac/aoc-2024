import itertools
from typing import List, Tuple, Dict


def get_matrix(file_path: str) -> List[List[int]]:
    matrix = []
    with open(file_path, "r") as f:
        lines = f.readlines()
        for line in lines:
            matrix.append(list(line.strip()))
    return matrix


def get_antenna_positions(matrix: List[List[int]]) -> Dict[str, List[Tuple[int, int]]]:
    antenna_positions = {}
    for i, row in enumerate(matrix):
        for j, cell in enumerate(row):
            if cell != ".":
                antenna_positions[cell] = antenna_positions.get(cell, []) + [(i, j)]
    return antenna_positions


def pos_is_in_matrix(matrix: List[List[int]], i: int, j: int) -> bool:
    return i >= 0 and i < len(matrix) and j >= 0 and j < len(matrix[0])


def count_antinodes(matrix: List[List[int]], ap: Dict[str, List[Tuple[int, int]]]) -> int:
    antinode_set = set()
    for antenna, positions in ap.items():
        if len(positions) == 1:
            continue
        # Each pair of antennas (positions) can have up to 2 antinodes.
        # The antinodes are positions on the line of the positions but with a distance twice the distance between the antennas.
        # we need to count how many of these antinodes are within the matrix, they can be placed on positions occupied by other antennas.
        for pair in itertools.combinations(positions, 2):
            dy = pair[1][0] - pair[0][0]
            dx = pair[1][1] - pair[0][1]
            antinode_1 = (pair[1][0] + dy, pair[1][1] + dx)
            antinode_2 = (pair[0][0] - dy, pair[0][1] - dx)
            if pos_is_in_matrix(matrix, *antinode_1):
                antinode_set.add(antinode_1)
            if pos_is_in_matrix(matrix, *antinode_2):
                antinode_set.add(antinode_2)
    return len(antinode_set)


def count_antinodes_repeatedly(matrix: List[List[int]], ap: Dict[str, List[Tuple[int, int]]]) -> int:
    antinode_set = set()
    for antenna, positions in ap.items():
        if len(positions) == 1:
            continue
        # Each pair of antennas (positions) can have up to 2 antinodes.
        # The antinodes are positions on the line of the positions but with a distance twice the distance between the antennas.
        # we need to count how many of these antinodes are within the matrix, they can be placed on positions occupied by other antennas.
        for pair in itertools.combinations(positions, 2):
            dy = pair[1][0] - pair[0][0]
            dx = pair[1][1] - pair[0][1]
            next_dx = dx
            next_dy = dy
            # immediately add the pair positions to the antinode_set
            antinode_set.add(pair[0])
            antinode_set.add(pair[1])
            while True:
                antinode_1 = (pair[1][0] + next_dy, pair[1][1] + next_dx)
                antinode_2 = (pair[0][0] - next_dy, pair[0][1] - next_dx)
                if pos_is_in_matrix(matrix, *antinode_1):
                    antinode_set.add(antinode_1)
                if pos_is_in_matrix(matrix, *antinode_2):
                    antinode_set.add(antinode_2)
                if not pos_is_in_matrix(matrix, *antinode_1) and not pos_is_in_matrix(matrix, *antinode_2):
                    break
                next_dy += dy
                next_dx += dx
    return len(antinode_set)

if __name__ == "__main__":
    matrix = get_matrix("dec8/input.txt")
    antenna_positions = get_antenna_positions(matrix)
    print(f"[part1]: There are {count_antinodes(matrix, antenna_positions)} antinodes in the matrix.")
    print(f"[part2]: There are {count_antinodes_repeatedly(matrix, antenna_positions)} antinodes in the matrix.")
