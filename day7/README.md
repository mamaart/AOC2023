# Day 7

Today we play cards on the camels and we need to write some semi complex comparison logic

Much of the logic in part1 is used in part2 but everything is copied over because there was some fundamental changes in the structure that would take too much time to implement in a smart way.

Refactor later...


## Compile run and benchmark

Compile with optimization

```sh
ghc -O2 ./part1.hs
ghc -O2 ./part2.hs
```

### Running part1 

```sh
cat ./day7.txt | time ./part1
```

```sh
# 6440
# ./part1  0.00s user 0.00s system 85% cpu 0.008 total
```

### Running part2 

```sh
cat ./day7.txt | time ./part2
```

```sh
# 5905
# ./part2  0.00s user 0.00s system 86% cpu 0.007 total
```
