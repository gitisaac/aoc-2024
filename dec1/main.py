from typing import List, Tuple

def get_lists_from_input_file(file_path: str) -> Tuple[List[int], List[int]]:
    list_1 = []
    list_2 = []
    with open(file_path, "r") as f:
        lines = f.readlines()
        for line in lines:
            # build 2 separate lists of numbers, there are 2 numbers per line separated by 3 spaces
            numbers = line.split("   ")
            # convert the strings to integers
            num1 = int(numbers[0])
            num2 = int(numbers[1])
            list_1.append(num1)
            list_2.append(num2)
    return list_1, list_2


def solve_part_1(list_1: List[int], list_2: List[int]):
    # sort the lists
    list_1.sort()
    list_2.sort()
    accumulative_diff = 0
    for i in range(len(list_1)):
        accumulative_diff += abs(list_1[i] - list_2[i])

    print(f"answer to part1: {accumulative_diff}")


def solve_part_2(list_1: List[int], list_2: List[int]):
    dict_1 = {num: 0 for num in list_1}
    for num in list_2:
        if num in dict_1:
            dict_1[num] += 1

    accumulative_repetitions = 0
    for key, value in dict_1.items():
        #print(f"key: {key}, value: {value}")
        accumulative_repetitions += key * value

    print(f"answer to part2: {accumulative_repetitions}")


if __name__ == "__main__":
    l1, l2 = get_lists_from_input_file("./dec1/input.txt")
    solve_part_1(l1, l2)
    solve_part_2(l1, l2)
