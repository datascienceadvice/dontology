-- Lua filter to automatically wrap images in a specific Word style
function Image(el)
  -- We wrap the Image element in a Span or Paragraph 
  -- but the most robust way for Word is to target the Paragraph containing the image
  return el
end

function Para(el)
  -- If a paragraph contains only an Image, apply the style
  if #el.content == 1 and el.content[1].t == "Image" then
    return pandoc.Para(el.content, {["custom-style"] = "Image-Centered"})
  end
end