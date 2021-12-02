package day02

import (
	"aoc-2021/utils/files"
	"fmt"
	"strconv"
	"strings"
)

type Movement int32

const (
	Up Movement = iota
	Down
	Forward
)

type command struct {
	mov Movement
	d   int
}

func prepare(input string) []command {
	content := strings.Split(input, "\n")
	commands := make([]command, 0, len(content))
	for _, c := range content {
		commandSlice := strings.Split(c, " ")
		if len(commandSlice) != 2 {
			break
		}
		movementStr := commandSlice[0]
		d, _ := strconv.Atoi(commandSlice[1])
		var m Movement
		if movementStr == "up" {
			m = Up
		} else if movementStr == "down" {
			m = Down
		} else {
			m = Forward
		}
		commands = append(commands, command{m, d})
	}
	return commands
}

func part1(input []command) int {
	d := 0
	h := 0
	for _, cmd := range input {
		if cmd.mov == Up {
			d -= cmd.d
		} else if cmd.mov == Down {
			d += cmd.d
		} else {
			h += cmd.d
		}
	}
	return h * d
}

func part2(input []command) int {
	d := 0
	h := 0
	a := 0
	for _, cmd := range input {
		if cmd.mov == Up {
			a -= cmd.d
		} else if cmd.mov == Down {
			a += cmd.d
		} else {
			h += cmd.d
			d += a * cmd.d
		}
	}
	return h * d
}

func Day02() {
	input := prepare(files.ReadFile(2))
	fmt.Printf("(%v, %v)\n", part1(input), part2(input))
}
