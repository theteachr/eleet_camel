# Eleet Camel

A collection of solutions to LeetCode problems in OCaml.

## Running

```
dune exec eleet_camel
```

runs all solutions against all test cases. In case of wrong solutions, you'll
see a diff in the `tests` directory.

## Adding Problems and Writing Solutions

### Setting Up Tests

- Create a directory (`d`) inside `tests` named after the problem number on LeetCode.
- Create `tests.txt` in `d`.
  - Add inputs separated by `---`.
- Create `expected.txt` in `d`.
  - Add expected outputs for inputs in `tests.txt`. Outputs should be separated by `---`.

#### Example

Writing tests for problem `1` on LeetCode.

Create the directory.

```bash
mkdir tests/1
```

Create the file for inputs.

```bash
touch tests/1/tests.txt
```

Add some inputs.

```
4 3 1 5
6 8 0 1
---
1 0
4 5 7
```

Create the file for outputs.

```bash
touch tests/1/expected.txt
```

Add expected outputs.

```
0 2 2 6
---
5 5 7
```

### Writing a Solution

#### Registering an Implementation

- Create a(n) `.ml` file inside `lib`.
- Add it as a module in the `solvers` list defined in `main.ml`.
  - The list entry should be in the form `("<problem_number>", module <Name>)`.

#### Implementation

Make the `.ml` file created as part of _registration_ satisfy the following contract.

- `type input`: Specify the type of input the main algorithm (`solve`) takes.
- `type output`: Specify the type output by the main algorithm.
- `val to_string : output -> string`: Provide a function to represent the `output` as a `string`.
- `val parse : string -> input`: Provide a function to convert the test read as a `string` into `input`.
- `val solve : intput -> output`: Write the core algorithm.
