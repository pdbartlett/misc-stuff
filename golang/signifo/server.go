package signifo

import (
  "embed"
  "log"
  "net/http"
)

var (
  //go:embed static
  staticFS embed.FS
)

const QQQ = "/quitquitquit"

type Server struct {}

func (s *Server) Run(addr string) <-chan error {
  ch := make(chan error)
  http.Handle("/static/", http.FileServerFS(staticFS))
  http.HandleFunc(QQQ, func(w http.ResponseWriter, _ *http.Request) {
    w.Write([]byte("Exiting..."))
    close(ch)
  })
  log.Printf("Listening on %s\n", addr)
  go func() {
    if err :=  http.ListenAndServe(addr, loggingHandler()); err != nil {
      ch <- err
      close(ch)
    }
  }()
  return ch 
}

type loggingResponseWriter struct {
  http.ResponseWriter
  statusCode int
}

func NewLoggingResponseWriter(w http.ResponseWriter) *loggingResponseWriter {
  return &loggingResponseWriter{w, http.StatusOK}
}

func (lrw *loggingResponseWriter) WriteHeader(code int) {
  lrw.statusCode = code
  lrw.ResponseWriter.WriteHeader(code)
}

func loggingHandler() http.Handler {
  return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
    log.Printf("<--- %s %s %s", r.RemoteAddr, r.Method, r.URL)
    lrw := NewLoggingResponseWriter(w)
    http.DefaultServeMux.ServeHTTP(lrw, r)
    log.Printf("---> %s %s %s (%d)", r.RemoteAddr, r.Method, r.URL, lrw.statusCode)
  })
}
