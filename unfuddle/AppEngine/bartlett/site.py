from sitemodel import *

def get_site_links():
  genLink = Link('General', None)
  genLink.add(Link('Home', '/'))
  links = [ genLink ]
  for person in get_people():
    parentLink = Link(person.name, None)
    links.append(parentLink)
    name = person.name.lower()
    if Page.ownersMap.has_key(name):
      for page in Page.ownersMap[name]:
        parentLink.add(page)
  if Page.ownersMap.has_key(shared):
    sharedLink = Link('Shared Interests', None)
    for page in Page.ownersMap[shared]:
      sharedLink.add(page)
    links.append(sharedLink)
  return links
  
def get_people():
  return [ paul, rosie, sophie ]
  
# Create the required data
  
paul = Person('Paul')
rosie = Person('Rosie')
sophie = Person('Sophie')
  
Page('Tiaras', rosie)