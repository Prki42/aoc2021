package day12

import (
	"aoc-2021/utils/files"
	"fmt"
	"strings"
)

type Node struct {
	value     string
	isCapital bool
}

func (n *Node) ToString() string {
	return fmt.Sprintf("%v", n.value)
}

type Graph struct {
	nodes []*Node
	edges map[Node][]*Node
}

func (g *Graph) AddNode(n *Node) *Node {
	for _, node := range g.nodes {
		if n.value == node.value {
			return node
		}
	}
	g.nodes = append(g.nodes, n)
	return n
}

func (g *Graph) AddEdge(n1, n2 *Node) {
	if g.edges == nil {
		g.edges = make(map[Node][]*Node)
	}
	g.edges[*n1] = append(g.edges[*n1], n2)
	g.edges[*n2] = append(g.edges[*n2], n1)
}

func (g *Graph) ToString() string {
	str := ""
	for i := 0; i < len(g.nodes); i++ {
		str += g.nodes[i].ToString() + " -> "
		pointsTo := g.edges[*g.nodes[i]]
		for j := 0; j < len(pointsTo); j++ {
			str += pointsTo[j].ToString() + " "
		}
		str += "\n"
	}
	return str
}

func isUpper(s string) bool {
	return strings.ToUpper(s) == s
}

func prepare(input string) Graph {
	var g Graph
	g.edges = make(map[Node][]*Node)

	lines := strings.Split(input, "\n")
	for _, line := range lines {
		if line == "" {
			continue
		}
		lineSlice := strings.Split(line, "-")
		node1 := &Node{lineSlice[0], isUpper(lineSlice[0])}
		node2 := &Node{lineSlice[1], isUpper(lineSlice[1])}
		node1 = g.AddNode(node1)
		node2 = g.AddNode(node2)
		g.AddEdge(node1, node2)
	}
	return g
}

var state map[Node]int
var results []int

func (g Graph) traverse(startNode, endNode Node, count int, twice bool) int {
	if state == nil {
		state = make(map[Node]int)
	}
	if results == nil {
		results = make([]int, 0)
	}
	near := g.edges[startNode]
	state[startNode] += 1
	for _, node := range near {
		if node.value == endNode.value {
			results = append(results, count)
			continue
		}
		visited := state[*node]
		isStart := node.value == "start"
		if (!twice && visited != 0 && !node.isCapital) || isStart {
			continue
		} else if twice && visited == 1 && !node.isCapital {
			g.traverse(*node, endNode, count+1, false)
			state[*node] -= 1
		} else {
			g.traverse(*node, endNode, count+1, twice)
			state[*node] -= 1
		}
	}
	return len(results)
}

func part1(g Graph) int {
	state = nil
	results = nil
	return g.traverse(Node{"start", false}, Node{"end", false}, 0, false)
}

func part2(g Graph) int {
	state = nil
	results = nil
	return g.traverse(Node{"start", false}, Node{"end", false}, 0, true)
}

func Day12() {
	input := prepare(files.ReadFile(12))
	fmt.Printf("(%v, %v)\n", part1(input), part2(input))
}
