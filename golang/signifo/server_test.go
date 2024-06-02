package signifo

import (
  "net/http"
  "net/url"
  "testing"
)

func TestRun(t *testing.T) {
  port := ":3006"
  ch := new(Server).RunAsync(port)
  path := "http://localhost" + port + QQQ
  _, err := http.PostForm(path, make(url.Values))
  if err != nil {
    t.Errorf("http.Post(%s): unexpected error: %v", path, err)
  }
  err, ok := <-ch
  if err != nil {
    t.Errorf("unexpected error read from channel: %v", err)
  }
  if ok {
    t.Errorf("expected channel to be closed")
  }
}
