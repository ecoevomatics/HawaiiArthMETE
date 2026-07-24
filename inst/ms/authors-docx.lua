-- Lua filter: render structured author/affiliation/email metadata
-- natively for docx output.
--
-- Pandoc's docx writer renders each item in meta.author as its own
-- paragraph using the built-in "Author" style, so this filter rebuilds
-- meta.author as a list of formatted inline sequences (name, superscript
-- affiliation numbers, email, orcid) followed by the affiliation legend.
-- That avoids the raw-LaTeX approach used by authors.lua (for the PDF
-- output), which docx ignores entirely.
--
-- Unlike a typical "corresponding author only" convention, every author
-- here is assumed to have an email address, so each author's line
-- includes their own email rather than a single shared correspondence
-- line.

local function affiliation_numbers(author)
  local nums = {}
  if author.affiliations then
    for _, aff in ipairs(author.affiliations) do
      table.insert(nums, tostring(aff.number))
    end
  end
  return nums
end

local function author_line(author)
  local name = pandoc.utils.stringify(author.name.literal)
  local inlines = { pandoc.Str(name) }

  local aff_nums = affiliation_numbers(author)
  if #aff_nums > 0 then
    table.insert(inlines, pandoc.Superscript({ pandoc.Str(table.concat(aff_nums, ",")) }))
  end

  local extras = {}
  if author.email then
    local email = pandoc.utils.stringify(author.email)
    table.insert(extras, pandoc.Link({ pandoc.Str(email) }, "mailto:" .. email))
  end
  if author.orcid then
    local orcid = pandoc.utils.stringify(author.orcid)
    table.insert(extras, pandoc.Link({ pandoc.Str("ORCID " .. orcid) }, "https://orcid.org/" .. orcid))
  end

  if #extras > 0 then
    table.insert(inlines, pandoc.Space())
    table.insert(inlines, pandoc.Str("("))
    for i, extra in ipairs(extras) do
      if i > 1 then
        table.insert(inlines, pandoc.Str("; "))
      end
      table.insert(inlines, extra)
    end
    table.insert(inlines, pandoc.Str(")"))
  end

  return pandoc.MetaInlines(inlines)
end

local function affiliation_line(aff)
  return pandoc.MetaInlines({
    pandoc.Superscript({ pandoc.Str(tostring(aff.number)) }),
    pandoc.Space(),
    pandoc.Str(pandoc.utils.stringify(aff.name)),
  })
end

function Meta(meta)
  if not meta["by-author"] then return meta end

  local lines = {}

  for _, author in ipairs(meta["by-author"]) do
    table.insert(lines, author_line(author))
  end

  if meta["affiliations"] then
    for _, aff in ipairs(meta["affiliations"]) do
      table.insert(lines, affiliation_line(aff))
    end
  end

  meta.author = pandoc.MetaList(lines)
  meta.authors = nil

  return meta
end
