package day04

import (
	"aoc-2021/utils/files"
	"fmt"
	"strconv"
	"strings"
)

type position struct {
	row int
	col int
}

type board struct {
	marked map[int]position
	table  [][]int
}

type bingo struct {
	numbers   []int
	boardsNum int
	boards    []board
}

func (b *board) mark(num int) {
	for i, row := range b.table {
		for j, val := range row {
			if val == num {
				b.marked[num] = position{i, j}
			}
		}
	}
}

func (b board) sum() int {
	sum := 0
	for _, row := range b.table {
		for _, val := range row {
			sum += val
		}
	}
	return sum
}

func (b board) win() (bool, int) {
	markedSum := 0
	rows := make(map[int]int)
	cols := make(map[int]int)
	for k, v := range b.marked {
		markedSum += k
		rows[v.row] += 1
		cols[v.col] += 1
	}

	for _, v := range rows {
		if v == 5 {
			return true, b.sum() - markedSum
		}
	}
	for _, v := range cols {
		if v == 5 {
			return true, b.sum() - markedSum
		}
	}

	return false, 0
}

func prepare(input string) bingo {
	splittedLines := strings.Split(input, "\n")
	numbersStrSlice := strings.Split(splittedLines[0], ",")
	numbers := make([]int, 0, len(numbersStrSlice))
	splittedLines = splittedLines[1:]
	for _, val := range numbersStrSlice {
		num, _ := strconv.Atoi(val)
		numbers = append(numbers, num)
	}

	boardCount := len(splittedLines) / 6
	boards := make([]board, boardCount)

	for i := 0; i < boardCount; i++ {
		boards[i].table = make([][]int, 5)
		boards[i].marked = make(map[int]position)
		for j := 0; j < 5; j++ {
			boards[i].table[j] = make([]int, 5)
		}
		boardLines := splittedLines[1+6*i : 6+6*i]
		for j, line := range boardLines {
			line = strings.TrimSpace(line)
			line = strings.ReplaceAll(line, "  ", " ")
			for k, val := range strings.Split(line, " ") {
				n, _ := strconv.Atoi(val)
				boards[i].table[j][k] = n
			}
		}
	}

	return bingo{numbers, boardCount, boards}
}

func part1(b bingo) int {
	for _, num := range b.numbers {
		for i := 0; i < b.boardsNum; i++ {
			b.boards[i].mark(num)
			won, sum := b.boards[i].win()
			if won {
				return sum * num
			}
		}
	}
	return 0
}

func part2(b bingo) int {
	score := -1
	blackList := make(map[int]bool)
	for _, num := range b.numbers {
		for i := 0; i < b.boardsNum; i++ {
			if blackList[i] {
				continue
			}
			b.boards[i].mark(num)
			won, sum := b.boards[i].win()
			if won {
				score = sum * num
				blackList[i] = true
			}
		}
	}
	return score
}

func Day04() {
	input := prepare(files.ReadFile(4))
	fmt.Printf("(%v, %v)\n", part1(input), part2(input))
}
