local word_count = 0

local function count_words_in_string(str)
  if not str then return 0 end
  local _, count = string.gsub(str, "%S+", "")
  return count
end

local function count_inlines(inlines)
  if not inlines then return end
  for _, inline in ipairs(inlines) do
    if inline.t == "Str" then
      if inline.text and inline.text:match("%P") then
        word_count = word_count + 1
      end
    elseif inline.t == "Code" or inline.t == "Math" then
      word_count = word_count + count_words_in_string(inline.text)
    elseif inline.t == "Cite" then
      word_count = word_count + 1
    elseif inline.t == "Span"
        or inline.t == "Emph"
        or inline.t == "Strong"
        or inline.t == "Link"
        or inline.t == "Quoted"
        or inline.t == "Strikeout"
        or inline.t == "SmallCaps"
        or inline.t == "Underline" then
      count_inlines(inline.content)
    end
  end
end

local function process_blocks(blocks)
  if not blocks then return end
  for _, block in ipairs(blocks) do
    if block.t == "Para" or block.t == "Plain" or block.t == "Header" then
      count_inlines(block.content)
    elseif block.t == "BlockQuote" or block.t == "Div" or block.t == "Note" then
      process_blocks(block.content)
    elseif block.t == "BulletList" then
      for _, item in ipairs(block.content) do
        process_blocks(item)
      end
    elseif block.t == "OrderedList" then
      local items = block.content[2]
      for _, item in ipairs(items) do
        process_blocks(item)
      end
    elseif block.t == "DefinitionList" then
      for _, def in ipairs(block.content) do
        local term = def[1]
        local defs = def[2]
        count_inlines(term)
        for _, d in ipairs(defs) do
          process_blocks(d)
        end
      end
    elseif block.t == "CodeBlock" then
      word_count = word_count + count_words_in_string(block.text)
    end
  end
end

function Pandoc(doc)
  word_count = 0
  process_blocks(doc.blocks)

  -- optional: im Meta speichern (für Debug/andere Zwecke)
  doc.meta.wordn = pandoc.MetaInlines({ pandoc.Str(tostring(word_count)) })

  -- Platzhalter im Text ersetzen
  doc = doc:walk({
    Str = function(el)
      if el.text == "WORDCOUNT" then
        return pandoc.Str(tostring(word_count))
      end
      return nil
    end
  })

  return doc
end
