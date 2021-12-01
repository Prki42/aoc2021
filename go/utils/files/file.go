package files

import (
	"fmt"
	"io/ioutil"
	"strconv"
)

func ReadFile(day int) string {
	dayStr := strconv.Itoa(day)
	if len(dayStr) == 1 {
		dayStr = "0" + dayStr
	}

	filePath := fmt.Sprintf("inputs/input%s.txt", dayStr)

	file, err := ioutil.ReadFile(filePath)
	if err != nil {
		panic(err)
	}

	return string(file)
}
