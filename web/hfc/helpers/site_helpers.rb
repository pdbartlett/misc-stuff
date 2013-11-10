module SiteHelpers
  def s_img(fragment)
    image_tag fragment
  end
  def s_link(link_text, fragment)
    link_to link_text, data.site.link_root + fragment
  end
  def s_link_block(fragment, &block)
    link_to data.site.link_root + fragment, &block
  end
end
    