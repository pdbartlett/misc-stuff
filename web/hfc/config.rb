set :css_dir, 'stylesheets'
set :images_dir, 'images'
set :relative_links, true

activate :livereload

configure :build do
  activate :minify_css
  activate :minify_html
  activate :minify_javascript
end
