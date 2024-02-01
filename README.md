# Condify

Condify is a command line utility that lets you add extra syntax to any file and create multiple different versions of it. None of the extra syntax is left in the output. See the example bellow:
![Diagram explenation](./readmediagram/explenation.svg)

## How to use

```bash
condify "input.txt" -o "output%s.txt"
```

If no output is specificed condify will try its best to make something reasonable by splicing in the branch name at the start of the filename.

| Flags              | Description                                                                                       |
| ------------------ | ------------------------------------------------------------------------------------------------- |
| `--watch`          | Reruns the command whenver your input file is changed                                             |
| `-o` or `--output` | Specifies where the output should go, the branch name will be inserted at all positions with `%s` |

## When might Condify be useful?

For smaller files where you want simple changes. For anything more advanced than having variables and simple if:s then you're better of making a pythonscript.
