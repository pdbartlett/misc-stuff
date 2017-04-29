from google.appengine.api import users
from google.appengine.ext import db

# Helper methods

def get_profile():
  profile = Profile(users.get_current_user())
  return profile

# Model classes

class Profile(db.Model):
  
  user = db.UserProperty()

  def __init__(self, from_user = None):
    self.user = from_user
    if (from_user):
      self.displayName = from_user.nickname
      self.links = '<a href="%s">Edit profile</a> | <a href="%s">Logout</a>' % ('/profile', users.create_logout_url('/'))
    else:
      self.displayName = 'Guest'
      self.links = '<a href="%s">Register</a> | <a href="%s">Login</a>' % ('/register', users.create_login_url('/'))
