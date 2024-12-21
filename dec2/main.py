from typing import List, Tuple

def get_lists_from_input_file(file_path: str) -> List[List[int]]:
    list_1 = []
    with open(file_path, "r") as f:
        lines = f.readlines()
        for line in lines:
            numbers = line.split(" ")
            list_1.append([int(num) for num in numbers])
    return list_1


def solve_part_1(reports: List[List[int]]):
    safe_reports = []
    for report in reports:
        last_num = None
        is_increasing = True
        is_safe = True
        for num in report:
            if last_num is None:
                last_num = num
                is_increasing = num - report[1] < 0
                continue
            if num < last_num and is_increasing:
                is_safe = False
                break
            if num > last_num and not is_increasing:
                is_safe = False
                break
            if num == last_num:
                is_safe = False
                break
            diff = abs(num - last_num)
            if diff > 3:
                is_safe = False
                break
            last_num = num

        safe_reports.append(is_safe)

    print(f"answer to part1: {safe_reports.count(True)}")


def solve_part_2(reports: List[List[int]]) -> int:
    def is_report_safe(report: List[int]) -> bool:
        inc_or_dec = (report==sorted(report) or report==sorted(report,reverse=True))
        is_safe = True
        for i in range(len(report)-1):
            diff = abs(report[i]-report[i+1])
            if not 1<=diff<=3:
                is_safe = False
        return inc_or_dec and is_safe
    
    safe_reports = []
    for report in reports:
        for i in range(len(report)):
            # exhaustively check all possible reports with one number removed
            report_minus_one = report[:i] + report[i+1:]
            if is_report_safe(report_minus_one):
                safe_reports.append(True)
                break
            elif i == len(report) - 1:
                safe_reports.append(False)

    print(f"answer to part2: {safe_reports.count(True)}")


if __name__ == "__main__":
    l1 = get_lists_from_input_file("./dec2/input.txt")
    solve_part_1(l1)
    solve_part_2(l1)
