import template

from google.appengine.ext import webapp

class AtomFeed(webapp.RequestHandler):
  def get(self):
    self.response.headers['Content-Type'] = 'text/xml'
    self.response.out.write('<message to="world">Hello!</message>')