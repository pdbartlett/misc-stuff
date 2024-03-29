package signifo

import (
  "embed"
  "fmt"
  "log"
  "net/http"
  "path"
  "github.com/google/safehtml/template"
)

var (
  //go:embed *.html
  //go:embed static
  staticFS embed.FS

  //go:embed templates
  templateFS embed.FS
  templates = template.Must(
    template.New("all").ParseFS(template.TrustedFSFromEmbed(templateFS), "templates/*"))
)

const QQQ = "/quitquitquit"

type Server struct {}

func (s *Server) RunAsync(addr string) <-chan error {
  ch := make(chan error)
  http.HandleFunc("/", fileOrTemplate)
  http.HandleFunc("/static/", staticFile)
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

func fileOrTemplate(w http.ResponseWriter, r *http.Request) {
  log.Printf("Request for file or template %q", r.URL)
  base := path.Base(r.URL.Path)
  t := templates.Lookup(base)
  if t == nil {
    log.Printf("Treating %q as static file", r.URL)
    staticFile(w, r)
    return
  }
  log.Printf("Executing template %q", base)
  if err := t.Execute(w, r); err != nil {
    log.Printf("Error executing template %q: %v", base, err)
    w.WriteHeader(500)
    w.Write([]byte(fmt.Sprintf("Server error: %v", err)))
  }
}

func staticFile(w http.ResponseWriter, r *http.Request) {
  log.Printf("Request for static file %q", r.URL)
  http.FileServerFS(staticFS).ServeHTTP(w, r)
}

func post(path string) string {
  return fmt.Sprintf("POST %s", path)
}