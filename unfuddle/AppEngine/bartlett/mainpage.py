import template

from google.appengine.ext import webapp

class Page(webapp.RequestHandler):
  def get(self):
    template.render_standard_page(self, 'main.html', {})
    