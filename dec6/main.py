from typing import List, Tuple, Generator
import copy

DIRECTIONS_MAP = {
    '>': (0, 1),
    '<': (0, -1), 
    '^': (-1, 0), 
    'v': (1, 0)
}


def get_matrix(file_path: str) -> List[List[int]]:
    matrix = []
    with open(file_path, "r") as f:
        lines = f.readlines()
        for line in lines:
            matrix.append(list(line))
    return matrix

def find_start_and_direction(matrix: List[List[int]]) -> Tuple[Tuple[int, int], Tuple[int, int]]:
    for i, row in enumerate(matrix):
        for j, cell in enumerate(row):
            if cell in DIRECTIONS_MAP.keys():
                return (i, j), DIRECTIONS_MAP[cell]

def is_out_of_bounds(matrix: List[List[int]], i: int, j: int) -> bool:
    return i < 0 or i >= len(matrix) or j < 0 or j >= len(matrix[0])

def is_obstacle(matrix: List[List[int]], i: int, j: int) -> bool:
    return matrix[i][j] == '#'

def get_next_pos(i: int, j: int, direction: Tuple[int, int]) -> Tuple[int, int]:
    return i + direction[0], j + direction[1]

def turn_90_degrees(direction: Tuple[int, int]) -> Tuple[int, int]:
    if direction == DIRECTIONS_MAP['>']:
        return DIRECTIONS_MAP['v']
    if direction == DIRECTIONS_MAP['^']:
        return DIRECTIONS_MAP['>']
    if direction == DIRECTIONS_MAP['<']:
        return DIRECTIONS_MAP['^']
    if direction == DIRECTIONS_MAP['v']:
        return DIRECTIONS_MAP['<']

def visited_cells(matrix: List[List[int]]) -> List[Tuple[int, int]]:
    (i, j), direction = find_start_and_direction(matrix)
    print(f"start: {(i, j)}, direction: {direction}")
    visited = {}
    while True:
        previous_directions_at_cell = visited.get((i, j), [])
        if direction in previous_directions_at_cell:
            break
        previous_directions_at_cell.append(direction)
        visited[(i, j)] = previous_directions_at_cell
        n_i, n_j = get_next_pos(i, j, direction)
        if is_out_of_bounds(matrix, n_i, n_j):
            break
        if is_obstacle(matrix, n_i, n_j):
            direction = turn_90_degrees(direction)
            continue
        else:
            matrix[i][j] = "X"
        # update the i,j
        i, j = n_i, n_j
        print(f"next: {i, j}")
    for line in matrix:
        # convert the line list chars to a string
        print(''.join(line))
    return visited.keys()


def has_loop(matrix: List[List[int]], i: int, j: int, direction: Tuple[int, int]) -> bool:
    visited = {}
    while True:
        previous_directions_at_cell = visited.get((i, j), [])
        if direction in previous_directions_at_cell:
            return True
        previous_directions_at_cell.append(direction)
        visited[(i, j)] = previous_directions_at_cell
        n_i, n_j = get_next_pos(i, j, direction)
        if is_out_of_bounds(matrix, n_i, n_j):
            return False
        if is_obstacle(matrix, n_i, n_j):
            direction = turn_90_degrees(direction)
            continue
        else:
            matrix[i][j] = "X"
        # update the i,j
        i, j = n_i, n_j


def all_combinations_of_one_extra_obstruction(matrix: List[List[int]]) -> Generator[List[List[int]], None, None]:
    # return the matrix but replace one of the cells with ' with #
    for i, row in enumerate(matrix):
        for j, cell in enumerate(row):
            if cell == 'X':
                new_matrix = copy.deepcopy(matrix)
                new_matrix[i][j] = '#'
                yield new_matrix

def solve_part_1(matrix: List[List[int]]) -> int:
    print(f"answer to part1: {len(visited_cells(matrix))}")

def solve_part_2(matrix: List[List[int]], i: int, j: int, direction: Tuple[int, int]) -> int:
    count = 0
    for new_matrix in all_combinations_of_one_extra_obstruction(matrix):
        # count how many of the combinations have a loop
        if has_loop(new_matrix, i, j, direction):
            count += 1
    print(f"answer to part2: {count}")

if __name__ == "__main__":
    matrix = get_matrix("./dec6/input.txt")
    (i, j), direction = find_start_and_direction(matrix)
    solve_part_1(matrix)
    solve_part_2(matrix, i, j, direction)
