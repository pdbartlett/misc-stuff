import wsgiref.handlers

from google.appengine.ext import webapp

import feeds
import mainpage
import newspage
import profilepage
import template

def main():
  application = webapp.WSGIApplication([
    ('/', mainpage.Page),
    ('/.*', template.TemplatePage),
  ], debug=True)
  wsgiref.handlers.CGIHandler().run(application)

if __name__ == '__main__':
  main()
