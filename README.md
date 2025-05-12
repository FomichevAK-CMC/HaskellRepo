Converts a primitive subset of YAML into JSON.

Usage:
```
stack exec <input_file> [<output_file>] -- if no output file specified - prints to the stdout
```

Example.yaml:
```yaml
hello:
  apple: 1
  cucumber:
    -2.4E+12
  orange: No
world:
  - - "high\n"
    - 'midd''le'
    - low\n
  - ~
...
Another document!
```
Output (a list of 2 YAML documents):
```json
[
  {
    "hello": {
    "apple": 1,
    "cucumber": -2.4E+12,
    "orange": false
    },
    "world": [
      [
        "high\n",
        "midd'le",
        "low\\n"
      ],
      null
    ]
  },
  "Another document!"
]
```
