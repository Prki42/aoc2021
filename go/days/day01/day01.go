package day01

import (
	"aoc-2021/utils/files"
	"fmt"
	"strconv"
	"strings"
)

func prepare(input string) []int {
	content := strings.Split(input, "\n")
	data := make([]int, 0, len(content))
	for _, c := range content {
		num, err := strconv.Atoi(c)
		if err == nil {
			data = append(data, num)
		}
	}
	return data
}

func countIncrease(input []int) int {
	length := len(input)
	count := 0
	for i := 0; i < length-1; i++ {
		if input[i+1] > input[i] {
			count++
		}
	}
	return count
}

func sliceSum(input []int) int {
	sum := 0
	for _, n := range input {
		sum += n
	}
	return sum
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func subSlicesSum(input []int) []int {
	length := len(input)
	res := make([]int, 0, length)
	for i := 0; i < length; i++ {
		end := min(i+3, length)
		res = append(res, sliceSum(input[i:end]))
	}
	return res
}

func part1(input []int) int {
	return countIncrease(input)
}

func part2(input []int) int {
	return countIncrease(subSlicesSum(input))
}

func Day01() {
	input := prepare(files.ReadFile(1))
	fmt.Printf("(%v, %v)\n", part1(input), part2(input))
}
