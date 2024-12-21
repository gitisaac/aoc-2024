import re
from typing import List, Tuple

def get_input_from_file(filename: str) -> List[str]:
    with open(filename, "r") as f:
        # take each line as a string without modifying it
        return [line.strip() for line in f.readlines()]


def get_sum_of_muls(line: str) -> int:
    # find all the substrings that match the pattern 'mul(x,x)'
    sum_of_products = 0
    matches = re.findall(r"mul\((-?\d{1,3}),(-?\d{1,3})\)", line)
    for match in matches:
        num1, num2 = int(match[0]), int(match[1])
        sum_of_products += num1 * num2
    
    return sum_of_products


def solve_part_1(lines: List[str]):
    sum_of_products = 0
    for line in lines:
        sum_of_products += get_sum_of_muls(line)

    print(f"answer to part1: {sum_of_products}")
    return sum_of_products


def solve_part_2(lines: List[str]):

    def _find_substring_ranges(s: str) -> List[Tuple[int, int]]:
        dont_pattern = re.compile(r"don't\(\)")
        do_pattern = re.compile(r"do\(\)")
        ranges = []

        for dont_match in dont_pattern.finditer(s):
            dont_start = dont_match.start()
            dont_end = dont_match.end()
            # slice the string from the end of the match to the end of the string
            remaining_s = s[dont_end:]
            do_match = do_pattern.search(remaining_s)
            if do_match:
                do_end = dont_end + do_match.end()
                ranges.append((dont_start, do_end))
            else:
                # if there is no match for the do_pattern, just remove the dont_match until the end of the string
                ranges.append((dont_start, len(s)))

        return ranges
    
    def _remove_ranges_from_string(s: str, ranges: List[Tuple[int, int]]) -> str:
        # Sort ranges by starting index to handle them in order
        ranges.sort()
        result = []
        last_end = 0

        for start, end in ranges:
            # Append the part of the string before the current range
            result.append(s[last_end:start])
            # Update the last_end to the end of the current range
            last_end = end

        # Append the remaining part of the string after the last range
        result.append(s[last_end:])
        
        # Join all parts to form the final string
        return ''.join(result)

    sum_of_products = 0
    for line in lines:
        ranges = _find_substring_ranges(line)
        line = _remove_ranges_from_string(line, ranges)
        sum_of_products += get_sum_of_muls(line)
        print(f"formatted line: {line}")
        print()

    print(f"answer to part2: {sum_of_products}")
    return sum_of_products

if __name__ == '__main__':
    lines = get_input_from_file("./dec3/input.txt")
    sol1 = solve_part_1(lines)
    concatenated_lines = "".join(lines)
    solve_part_2([concatenated_lines])
    test_input = ["xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"]
    solve_part_2(test_input)
