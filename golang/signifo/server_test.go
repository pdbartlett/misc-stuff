package signifo

import (
  "net/http"
  "testing"
)

func TestRun(t *testing.T) {
  port := ":3006"
  ch := new(Server).Run(port)
  url := "http://localhost" + port + "/" + QQQ
  _, err := http.Get(url)
  if err != nil {
    t.Errorf("http.Get(%s): unexpected error %v", url, err)
  }
  err, ok := <-ch
  if err != nil {
    t.Errorf("unexpected error read from channel%v", err)
  }
  if ok {
    t.Errorf("expected channel to be closed")
  }
}
