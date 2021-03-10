package main

import (
	"database/sql"
	"fmt"
	"log"
	"strings"

	_ "modernc.org/sqlite"
)

type colAffinity string

const (
	integerAffinity colAffinity = "INTEGER"
	textAffinity                = "TEXT"
)

type column struct {
	name     string
	affinity colAffinity
}

type entity interface {
	tableName() string
	columns() []column
}

func createTable(e entity, db *sql.DB) error {
	var cols []string
	for _, col := range e.columns() {
		cols = append(cols, fmt.Sprintf("%s %s", col.name, string(col.affinity)))
	}
	sql := fmt.Sprintf("CREATE TABLE %s (%s)", e.tableName(), strings.Join(cols, ","))
	_, err := db.Exec(sql)
	return err
}

type test struct{}

func (t test) tableName() string { return "t" }
func (t test) columns() []column { return []column{{"i", integerAffinity}} }

var logger = log.Default()

func main() {
	db, err := sql.Open("sqlite", "test.db")
	if err != nil {
		logger.Fatalln(err)
	}

	if createTable(test{}, db); err != nil {
		logger.Fatalln(err)
	}

	if err = db.Close(); err != nil {
		logger.Fatalln(err)
	}
}
