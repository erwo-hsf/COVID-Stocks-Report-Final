-- Quarto/Pandoc Lua filter: counts words and stores in document metadata as `wordcount`
function Pandoc(doc)
  -- Convert the whole document to plain text (drops formatting)
  local text = pandoc.write(doc, 'plain')
  local count = 0
  for _ in text:gmatch("%S+") do
    count = count + 1
  end

  -- Store in metadata so it can be used via {{< meta wordcount >}}
  doc.meta.wordcount = pandoc.MetaString(tostring(count))
  return doc
end
