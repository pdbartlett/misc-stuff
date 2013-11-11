module SiteHelpers
  def s_img(fragment)
    image_tag fragment
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
    