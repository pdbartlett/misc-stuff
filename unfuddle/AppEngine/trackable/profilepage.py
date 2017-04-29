import model
import template

class Page(template.StandardPage):
  def get(self):
    template.StandardPage.render_page(self, 'profile.html', { 'profile' : model.getProfile() })