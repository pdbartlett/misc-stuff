module SiteHelpers
  def s_img(fragment)
    if data.site.image_root.empty?
      image_tag fragment
    else
      image_tag data.site.image_root + fragment
    end
  end
  def s_link(link_text, fragment)
    if fragment.start_with? "http"
      link_to link_text, fragment
    else
      link_to link_text, data.site.link_root + fragment
    end
  end
  def s_link_block(fragment, &block)
    link_to data.site.link_root + fragment, &block
  end
end
    