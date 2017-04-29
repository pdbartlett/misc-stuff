import template

class Page(template.StandardPage):
  def get(self):
    template.StandardPage.render_page(self, 'register.html', {})