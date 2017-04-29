import os

import model

from google.appengine.ext import webapp
from google.appengine.ext.webapp import template

def render_template(file, args):
  templates = os.path.join(os.path.dirname(__file__), 'templates')
  filepath = os.path.join(templates, file)
  return template.render(filepath, args)

class TemplatePage(webapp.RequestHandler):
  def render_page(self, file, args):
    self.response.out.write(render_template(file, args))
    
class StandardPage(TemplatePage):
  def render_page(self, file, args):
    content = render_template(file, args)
    TemplatePage.render_page(self, 'base.html', {
      'content' : content,
      'profile' : model.getProfile(),
    })