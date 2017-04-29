import datetime
import os

import model
import site

from google.appengine.ext import webapp
from google.appengine.ext.webapp import template

def append_standard_args(args):
  args['today'] = datetime.date.today()
  for link in site.get_site_links():
    append_link_to_args(link, args)
  return args
    
def append_link_to_args(link, args):
  args[link.text.lower()] = link.anchor()
  for child in link.children:
    append_link_to_args(child, args)

def render_template(file, args, subdir='templates'):
  templates = os.path.join(os.path.dirname(__file__), subdir)
  if file[0] == '/':
    file = file[1:]
  filepath = os.path.join(templates, file)
  return template.render(filepath, args)

def render_template_page(handler, file, args, subdir='templates'):
  handler.response.out.write(render_template(file, args, subdir))
    
def render_standard_page(handler, file, args, subdir='templates'):
  if args == None:
    args = {}
  else:
    append_standard_args(args)
  content = render_template(file, args, subdir)
  render_template_page(handler, 'base.html', append_standard_args({
    'content' : content,
    'profile' : model.get_profile(),
    'links'   : site.get_site_links(),
  }))
  
class TemplatePage(webapp.RequestHandler):
  def get(self):
    try:
      render_standard_page(self, self.request.path + '.html', {})
    except: # probably django.template.TemplateDoesNotExist:, but difficult to import
      self.response.set_status(404, 'Not found')
      render_standard_page(self, '404.html', { 'path' : self.request.path })
