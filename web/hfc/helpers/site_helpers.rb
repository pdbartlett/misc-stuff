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
      if element.style.nil? or element.style.empty?
        span_attrs = {}
      else
        span_attrs = { "class" => element.style }
      end
      content_tag :span, span_attrs do
        case element.type
          when "partial"
            partial element.name
          when "separator"
            tag :hr
          when "heading"
            content_tag :h2, element.text
          when "image"
            _handle_image element.src, element.link
          when "image2"
            _handle_image(element.src1, element.link) + _handle_image(element.src2, element.link)
          else
            raise "Unsupported element type: " + element.type
        end
      end
    end
    rendered.join ""
  end
  def _handle_image(src, link)
    if link.nil?
      s_img src
    else
      s_link_block link do
        s_img src
      end
    end
  end
end
    