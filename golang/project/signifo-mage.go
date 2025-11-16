//go:build mage

// Magefile for signifo library and its bootstrap, cefa.
package main

import (
  "os"

  "github.com/magefile/mage/sh"
)

// Run runs cefa binary to exercise signifo
func Run() error {
  return runIn("cefa", "go", "run", "cefa.go")
}

// Test builds and tests signifo library
func Test() error {
  return runIn("signifo", "go", "test")
}

func runIn(subdir, cmd string, args ...string) error {
  if err := os.Chdir(subdir); err != nil {
    return err
  }
  defer os.Chdir("..")
  return sh.RunV(cmd, args...)
}
