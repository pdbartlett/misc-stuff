import wsgiref.handlers

from google.appengine.ext import webapp

import mainpage
import profilepage
import registerpage

def main():
  application = webapp.WSGIApplication([
    ('/', mainpage.Page),
    ('/profile', profilepage.Page),
    ('/register', registerpage.Page),
  ], debug=True)
  wsgiref.handlers.CGIHandler().run(application)

if __name__ == '__main__':
  main()
