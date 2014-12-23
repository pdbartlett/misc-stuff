module SiteHelpers
  class StartWithChecker
    def initialize(prefix)
      @prefix = prefix
    end
    def ===(str)
      (not str.nil?) and str.start_with? @prefix
    end
  end

  def start_with(prefix)
    StartWithChecker.new prefix
  end
  
  def articles_for(page)
    elements = []
    case page.url
    when "/"
      title = "Featured content"
      matches = [] # TODO: implement!
    when start_with("/articles/")
      title = "Related articles"
      matches = [] # TODO: implement!
    when start_with("/sections/")
      title = "Articles"
      matches = section_query page.data["title"].downcase
    else
      title = ""
      matches = []
    end
    if not title.empty?
      elements << (content_tag :h2, title)
      if matches.empty?
        elements << (content_tag :span, "None found.")
      else
        snippets = matches.map do |article|
          partial :snippet, :locals => { :article => article }
        end
        elements.concat snippets
      end
    end
    elements.join ""
  end
  
  def section_query(name)
    sitemap.where(:section.equal => name).all
  end
  
  def sections
    sitemap.resources.select do |r|
      r.url.start_with? "/sections/"
    end
  end
  
  def webfont_families()
    families = data.site.webfonts.map do |font|
      if font.weight.nil?
        font.family
      else
        font.family + ":" + font.weight.to_s
      end
    end
    families.join("|")
  end
  
  def webfont_sass_vars()
    decls = data.site.webfonts.map do |font|
      "$" + font.varname + ": " + font.family + ", " + font.backups + ";"
    end
    decls.join("\n")
  end
end