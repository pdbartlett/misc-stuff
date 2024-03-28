package signifo

import "testing"

func TestRun(t *testing.T) {
  if err := new(Server).Run(); err != nil {
    t.Errorf("Run(): want=nil; got=%v", err)
  }
}
