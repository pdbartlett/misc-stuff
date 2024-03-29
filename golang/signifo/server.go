package signifo

import (
  "embed"
  "fmt"
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
  http.Handle(get("/static/"), http.FileServerFS(staticFS))
  http.HandleFunc(post(QQQ), func(w http.ResponseWriter, _ *http.Request) {
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

func (lrw *loggingResponseWriter) WriteHeader(code int) {
  lrw.statusCode = code
  lrw.ResponseWriter.WriteHeader(code)
}

func loggingHandler() http.Handler {
  return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
    log.Printf("<--- %s %s %s", r.RemoteAddr, r.Method, r.URL)
    lrw := &loggingResponseWriter{w, http.StatusOK}
    http.DefaultServeMux.ServeHTTP(lrw, r)
    log.Printf("---> %s %s %s (%d)", r.RemoteAddr, r.Method, r.URL, lrw.statusCode)
  })
}

func get(path string) string {
  return fmt.Sprintf("GET %s", path)
}

func post(path string) string {
  return fmt.Sprintf("POST %s", path)
}