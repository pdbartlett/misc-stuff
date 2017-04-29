import cgi
import datetime
import os
import wsgiref.handlers

from google.appengine.ext import db
from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp import template

# Model classes

class Message(db.Model):
  user = db.UserProperty()
  email = db.StringProperty()
  subject = db.StringProperty()
  content = db.StringProperty(multiline=True)
  date = db.DateTimeProperty(auto_now_add=True)

  def getSummary(self):
    return '%s: %s (%s)' % (self.date, self.subject, self.email)
  
# Pages

class TemplatePage(webapp.RequestHandler):
  def __init__(self):
    self.user = users.get_current_user()
  def render(self, file, args):
    path = os.path.join(os.path.dirname(__file__), file)
    self.response.out.write(template.render(path, args))

class MainPage(TemplatePage):
  def get(self):
    TemplatePage.render(self, 'main.html', {})
    
class HeaderPage(TemplatePage):
  def get(self):
    if self.user:
      user_desc = self.user.nickname()
      user_link = users.create_logout_url('/')
      link_text = 'Logout'
    else:
      user_desc = None
      user_link = users.create_login_url('/')
      link_text = 'Login'
    TemplatePage.render(self, 'header.html', {
      'user_desc': user_desc,
      'user_link': user_link,
      'link_text': link_text,
      'is_admin':  users.is_current_user_admin()
      })
  
class WelcomePage(TemplatePage):
  def get(self):
    TemplatePage.render(self, 'welcome.html', {})
  
class ContactPage(TemplatePage):
  def get(self):
    PAGESIZE = 10
    if self.user:
      email = self.user.email()
      query = Message.all()
      if not users.is_current_user_admin():
        query.filter('user =', self.user)
      messages = query.order('-date').fetch(PAGESIZE)
      more = (query.count(PAGESIZE + 1) > PAGESIZE)
    else:
      email = ''
      messages = None
      more = False
    TemplatePage.render(self, 'contact.html', {
      'email': email,
      'admin': users.is_current_user_admin(),
      'messages': messages,
      'more': more,
      'posted': self.request.get('posted')
      })
  
  def post(self):
    message = Message()
    if users.get_current_user():
      message.user = users.get_current_user()
    message.email = self.request.get('email')
    message.subject = self.request.get('subject')
    message.content = self.request.get('content')
    message.put()
    self.redirect('/contact?posted=1')

class AdminPage(TemplatePage):
  def get(self):
    TemplatePage.render(self, 'admin.html', {})
    
# main(), etc.

def main():
  application = webapp.WSGIApplication([
    ('/', MainPage),
    ('/admin',   AdminPage),
    ('/contact', ContactPage),
    ('/header',  HeaderPage),
    ('/welcome', WelcomePage),
  ], debug=True)
  wsgiref.handlers.CGIHandler().run(application)

if __name__ == '__main__':
  main()
