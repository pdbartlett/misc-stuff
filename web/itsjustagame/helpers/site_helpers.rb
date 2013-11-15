module SiteHelpers
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