from typing import List


def get_input_equations(file_path: str) -> List[List[int]]:
    with open(file_path, "r") as file:
        lines = []
        for line in file.readlines():
            # example line: 9687: 6 72 7 3 615, split by ":" and then by " " and join the two lists into one
            lines.append(
                list(
                    map(
                        int,
                        [line.strip().split(":")[0]]
                        + line.strip().split(":")[1].strip().split(" "),
                    )
                )
            )
    return lines


def is_equation_solvable_prod_sum(equation: List[int], sol: int, curr=0) -> int:
    """
    Check if either sum or product of element from left to right is equal to sol
    if so return sol, otherwise return 0
    """
    if curr > sol:
        return 0
    if len(equation) == 0:
        if curr == sol:
            return sol
        else:
            return 0
    return max(
        is_equation_solvable_prod_sum(equation[1:], sol, curr + equation[0]),
        is_equation_solvable_prod_sum(equation[1:], sol, curr * equation[0]),
    )


def is_equation_solvable_prod_sum_concat(equation: List[int], sol: int, curr=0, so_far="") -> int:
    """
    Check if either sum or product or concat of element from left to right is equal to sol
    if so return sol, otherwise return 0
    """
    if curr > sol:
        return 0
    if len(equation) == 0:
        if curr == sol:
            return sol
        else:
            return 0
    option_sum = is_equation_solvable_prod_sum_concat(equation[1:], sol, curr + equation[0], so_far = so_far + " + " + str(equation[0]))
    option_prod = is_equation_solvable_prod_sum_concat(equation[1:], sol, curr * equation[0], so_far = so_far + " * " + str(equation[0]))
    option_concat = is_equation_solvable_prod_sum_concat(
        equation[1:], sol, int(str(curr) + str(equation[0])), so_far = so_far + " || " + str(equation[0])
    )
    return max(
        option_sum,
        option_prod,
        option_concat,
    )


def solve_part1(equations: List[List[int]]) -> int:
    ans = 0
    for equ in equations:
        sol = equ.pop(0)
        curr = equ.pop(0)
        ans += is_equation_solvable_prod_sum(equ, sol, curr)
    return ans


def solve_part2(equations: List[List[int]]) -> int:
    ans = 0
    for equ in equations:
        sol = equ.pop(0)
        curr = equ.pop(0)
        sol = is_equation_solvable_prod_sum_concat(equ, sol, curr)
        ans += sol
    return ans


if __name__ == "__main__":
    equations = get_input_equations("./dec7/input.txt")
    print(f"Solution to part1: {solve_part1(equations)}")
    equations = get_input_equations("./dec7/input.txt")
    print(f"Solution to part2: {solve_part2(equations)}")
