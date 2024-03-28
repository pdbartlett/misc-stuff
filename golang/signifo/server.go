package signifo

import (
  "log"
  "net/http"
)

const QQQ = "/quitquitquit"

type Server struct {}

func (s *Server) Run(addr string) <-chan error {
  ch := make(chan error)
  http.HandleFunc(QQQ, func(w http.ResponseWriter, _ *http.Request) {
    w.Write([]byte("Exiting..."))
    close(ch)
  })
  log.Printf("Listening on %s", addr)
  go func() {
    if err :=  http.ListenAndServe(addr, nil); err != nil {
      ch <- err
      close(ch)
    }
  }()
  return ch 
}
