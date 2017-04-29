# "Constants"

default = 'default'
shared = 'shared'

# Content-related classes

class Link:
  def __init__(self, text, href=default):
    self.text = text
    self.href = href == default and make_href(text) or href
    self.children = []
    
  def anchor(self):
    if self.href:
      return '<a href="%s">%s</a>' % (self.href, self.text)
    return self.text
    
  def render_in_list(self):
    html = '<li>' + self.anchor()
    if self.children:
      html += '<ul>'
      for child in self.children:
        html += child.render_in_list()
      html += '</ul>'
    html += '</li>'
    return html
    
  def add(self, childLink):
    self.children.append(childLink)
    
class Person:
  def __init__(self, name):
    self.name = name
    
class Page(Link):
  ownersMap = {}
  def __init__(self, name, primary, *others):
    ownership = others and shared or primary.name.lower()
    Link.__init__(self, name, make_href(ownership, name))
    self.owners = (primary, ) + others
    if not self.__class__.ownersMap.has_key(ownership):
      self.__class__.ownersMap[ownership] = []
    self.__class__.ownersMap[ownership].append(self)
    
class NewsItem:
  all = []
  def __init__(self, title, timestamp, author, content=""):
    self.title = title
    self.timestamp = timestamp
    self.author = author
    self.content = content
    self.__class__.all.append(self)
    
  def render(self):
    html = '<strong>%s</strong> - <em>by %s at %s</em>' % (
        self.title, self.author.name, self.timestamp)
    if self.content:
      html += '<br/>' + self.content
    return html
    
# Helper methods

def make_href(*components):
  normalized = [ comp.lower().replace(' ', '-') for comp in components ]
  return '/' + '/'.join(normalized)

