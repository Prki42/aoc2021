package main

import (
	"aoc-2021/days/day01"
	"aoc-2021/days/day02"
	"aoc-2021/days/day04"
	"aoc-2021/days/day05"
	"aoc-2021/days/day12"
	"aoc-2021/days/day25"
	"os"
	"strconv"
)

func main() {
	args := os.Args
	if len(args) != 2 {
		panic("Example usage: go run main.go 1")
	}
	n, err := strconv.Atoi(args[1])
	if err != nil {
		panic("Argument must be a number")
	}

	switch n {
	case 1:
		day01.Day01()
	case 2:
		day02.Day02()
	case 4:
		day04.Day04()
	case 5:
		day05.Day05()
	case 12:
		day12.Day12()
	case 25:
		day25.Day25()
	}
}
