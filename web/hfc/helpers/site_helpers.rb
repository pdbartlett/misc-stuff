module SiteHelpers
  def s_img(fragment)
    root = data.site.image_root
    if root.nil? or root.empty?
      image_tag fragment
    else
      image_tag root + fragment
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
  def s_section(elements)
    rendered = elements.map do |element|
      if element.type == "image"
        if element.link.nil?
          s_img element.src
        else
          s_link_block element.link do
            s_img element.src
          end
        end
      end
    end
    rendered.join ""
  end
end
    