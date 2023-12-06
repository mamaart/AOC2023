# Day 1

## Compile run and benchmark

Compile with optimization

```sh
ghc -O2 ./day1.hs
```

### Running part1 

```sh
cat ./part1.txt | time ./day1 part1
```

```sh
# 142
# ./day1 part1  0.00s user 0.01s system 86% cpu 0.006 total
```

### Running part2 

```sh
cat ./part2.txt | time ./day1 part2
```

```sh
# 281
# ./day1 part2  0.00s user 0.01s system 87% cpu 0.007 total
```
