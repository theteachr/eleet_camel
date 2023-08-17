import os

from pathlib import Path


def read_to_str(path):
    with open(path) as f:
        return f.read()

def merge(test_data, expected_data):
    tests = test_data.split('---\n')
    solus = expected_data.split('---\n')

    return '\n---\n\n'.join(f"{test}===\n{solu}" for test, solu in zip(tests, solus))

def main():
    for problem_dir in filter(os.path.isdir, map(Path, os.listdir())):
        test_data = read_to_str(problem_dir / "tests.txt")
        expe_data = read_to_str(problem_dir / "expected.txt")

        with open(f"{problem_dir}.txt", "w") as f:
            f.write(merge(test_data, expe_data))


if __name__ == '__main__':
    main()

