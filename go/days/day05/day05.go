package day05

import (
	"aoc-2021/utils/files"
	"fmt"
	"strconv"
	"strings"
)

type point struct {
	x int
	y int
}

type line struct {
	p1         point
	p2         point
	vertical   bool
	horizontal bool
}

func isHorizontal(p1, p2 point) bool {
	return p1.y == p2.y
}

func isVertical(p1, p2 point) bool {
	return p1.x == p2.x
}

func prepare(input string) []line {
	linesStr := strings.Split(input, "\n")
	lines := make([]line, 0, len(linesStr))
	for _, lineStr := range linesStr {
		if lineStr == "" {
			break
		}
		spaceSplit := strings.Split(lineStr, " ")
		p1Str := strings.Split(spaceSplit[0], ",")
		x1, _ := strconv.Atoi(p1Str[0])
		y1, _ := strconv.Atoi(p1Str[1])
		p1 := point{x1, y1}
		p2Str := strings.Split(spaceSplit[2], ",")
		x2, _ := strconv.Atoi(p2Str[0])
		y2, _ := strconv.Atoi(p2Str[1])
		p2 := point{x2, y2}
		lines = append(lines, line{p1, p2, isVertical(p1, p2), isHorizontal(p1, p2)})
	}
	return lines
}

func min(a, b int) int {
	if a > b {
		return b
	}
	return a
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

var pointMap map[point]int

func count() int {
	count := 0
	for _, v := range pointMap {
		if v > 1 {
			count++
		}
	}
	return count
}

func part1(input []line) int {
	for _, line := range input {
		if line.horizontal {
			for i := min(line.p1.x, line.p2.x); i <= max(line.p1.x, line.p2.x); i++ {
				p := point{i, line.p1.y}
				pointMap[p] += 1
			}
		} else if line.vertical {
			for i := min(line.p1.y, line.p2.y); i <= max(line.p1.y, line.p2.y); i++ {
				p := point{line.p1.x, i}
				pointMap[p] += 1
			}
		}
	}

	return count()
}

func part2(input []line) int {
	for _, line := range input {
		if (!line.horizontal) && (!line.vertical) {
			x_step := -1
			if line.p1.x < line.p2.x {
				x_step = 1
			}
			y_step := -1
			if line.p1.y < line.p2.y {
				y_step = 1
			}
			for i := 0; i <= (line.p1.x-line.p2.x)*-1*x_step; i++ {
				p := point{line.p1.x + i*x_step, line.p1.y + i*y_step}
				pointMap[p] += 1
			}
		}
	}

	return count()
}

func Day05() {
	input := prepare(files.ReadFile(5))
	pointMap = make(map[point]int)
	fmt.Printf("(%v, %v)\n", part1(input), part2(input))
}
