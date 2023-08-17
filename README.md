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

- [ ] Automate this

- Create a `.txt` file inside `tests` named after the problem number on
LeetCode.
- Separate each test case by `\n---\n`.
- Add expected output for each test case, where the test and answer are
separated by `===`.

#### Example

Writing tests for problem `1` on LeetCode.

Create the file.

```bash
mkdir tests/1.txt
```

Add test cases.

```
2 7 11 15
9
===
0 1

---

3 2 4
6
===
1 2

---

3 3
6
===
0 1
```

### Writing a Solution

#### Registering an Implementation

- Create a(n) `.ml` file inside `lib`.
- Add it as a module in the `solvers` list defined in `main.ml`.
  - The list entry should be in the form `("<problem_number>", module <Name>)`.

#### Implementation

Make the `.ml` file created as part of _registration_ satisfy the following contract.

- `type input`: Specify the type of input to the main algorithm (`solve`).
- `type output`: Specify the type of output returned by the main algorithm
(`solve`).
- `val to_string : output -> string`: Provide a function to convert the
`output` to a `string`.
- `val parse : string -> input`: Provide a function to convert the test (read as
a `string`) into `input`.
- `val solve : intput -> output`: Write the core algorithm.
