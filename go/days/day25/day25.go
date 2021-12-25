package day25

import (
	"aoc-2021/utils/files"
	"fmt"
	"strings"
)

func part1(input [][]byte) int {
	rowNum := len(input)
	colNum := len(input[0])
	changeCount := 1
	n := 0
	for changeCount != 0 {
		changeCount = 0
		for i := 0; i < rowNum; i++ {
			removeLast := false
			startIdx := 0
			if input[i][colNum-1] == '>' && input[i][0] == '.' {
				input[i][0] = '>'
				removeLast = true
				startIdx = 1
			}
			for j := startIdx; j < colNum-1; j++ {
				if input[i][j] != '>' {
					continue
				}
				right := (j + 1) % colNum
				if input[i][right] == '.' {
					input[i][right] = '>'
					input[i][j] = '.'
					changeCount++
					j++
				}
			}
			if removeLast {
				input[i][colNum-1] = '.'
			}
		}

		for j := 0; j < colNum; j++ {
			removeLast := false
			startIdx := 0
			if input[rowNum-1][j] == 'v' && input[0][j] == '.' {
				input[0][j] = 'v'
				removeLast = true
				startIdx = 1
			}
			for i := startIdx; i < rowNum-1; i++ {
				if input[i][j] != 'v' {
					continue
				}
				down := (i + 1) % rowNum
				if input[down][j] == '.' {
					input[down][j] = 'v'
					input[i][j] = '.'
					changeCount++
					i++
				}
			}
			if removeLast {
				input[rowNum-1][j] = '.'
			}
		}
		n++
	}
	return n
}

func prepare(input string) [][]byte {
	rows := strings.Split(input, "\n")
	grid := make([][]byte, 0)
	for _, r := range rows {
		if r == "" {
			break
		}
		grid = append(grid, []byte(r))
	}
	return grid
}

func Day25() {
	input := prepare(files.ReadFile(25))
	fmt.Println(part1(input))
}
