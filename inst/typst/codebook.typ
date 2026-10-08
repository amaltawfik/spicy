// Layout of the PDF codebook of spicy::code_book(). R appends two
// dictionaries to this file, `data` (the codebook, numbers already
// formatted) and `strings` (every word the reader sees, in the language of
// the codebook), then calls `codebook(data, strings)`: the layout holds no
// text of its own. Design tokens, sizes and bands follow lssdoc.

#let base = 10pt
#let pad = (x: 4pt, y: 3pt)
#let size = (
  cover-title: 28pt, cover-subtitle: 14pt, cover-top: 2.1in, cover-block: 18pt, // title block, and the air around it
  about-field: 1.6in, // field column of the facts on the page about the data
  heading-above: 18pt, heading-below: 9pt,
  sheet: 24pt, gap: 6pt, // above a sheet, between its tables
  long-name: 7pt, // a name past its limit: 30 characters in the list and the index, 45 in a band
)

// Widow and orphan control for a table that may break across pages: the
// table ends with a zero-width column, filled here with unbreakable cells
// that span its first `head` and last `tail` body rows.
#let kept(rows, head: 3, tail: 3) = {
  let n = rows.len()
  let gap = table.cell.with(inset: 0pt, breakable: false)
  let out = ()
  for (i, r) in rows.enumerate() {
    out += r
    if n <= head + tail {
      if i == 0 { out.push(gap(rowspan: n)[]) }
    } else if i == 0 {
      out.push(gap(rowspan: head)[])
    } else if i == n - tail {
      out.push(gap(rowspan: tail)[])
    } else if i >= head and i < n - tail {
      out.push(table.cell(inset: 0pt)[])
    }
  }
  out
}

#let codebook(data, s) = {
  let c = (:)
  for (k, v) in data.colors { c.insert(k, rgb(v)) }
  let rule = 0.5pt + c.grid
  // Monospace for identifiers only (variable names, source codes). At
  // 0.85em the x-height of DejaVu Sans Mono (0.547em) comes within 8% of
  // the body font's. Its line box starts at the body font's ascender, so
  // a name and the text beside it share a baseline. Values and labels stay
  // in the body font, as in published codebooks.
  let mono(size: 0.85em, ..args) = context text(
    font: data.font_code, size: size,
    top-edge: measure(text(top-edge: "ascender", bottom-edge: "baseline", "x")).height,
    ..args,
  )
  let hdr(body) = text(weight: "bold", fill: c.primary, body)
  let anchor(i) = label("cb-var-" + str(i))
  let page-of(i) = context link(anchor(i), text(fill: c.accent, str(locate(anchor(i)).page())))
  // A long name is set smaller, never cut: readers copy names from the PDF.
  // The limit is where the name starts to squeeze its neighbours: 30 in
  // the list and the index, 45 in the band of a sheet.
  let name(n, limit: 30, weight: "regular") = mono(
    size: if n.clusters().len() > limit { size.long-name } else { 0.85 * base },
    weight: weight,
    n,
  )
  // Every table of the document has one style: a header band, horizontal
  // hairlines, no vertical rule. A row that wraps keeps its cells on its
  // first line.
  // Rows under a header that repeats on every page; with `keep`, kept()
  // holds the first and last three rows together.
  let listing(heads, widths, aligns, rows, keep: true) = table(
    columns: widths + if keep { (0pt,) } else { () },
    stroke: (x, y) => (bottom: rule), inset: pad,
    fill: (x, y) => if y == 0 { c.band },
    align: (x, y) => aligns.at(x, default: left) + top,
    table.header(..heads.map(hdr), ..if keep { ([],) } else { () }),
    ..if keep { kept(rows) } else { rows.flatten() },
  )
  // The block kept() makes of three rows cannot break: taller than a page,
  // it loses rows. The list and the index keep their rows only while every
  // label is under 160 characters and every name under 60, which keeps
  // three rows well within a page; past that, their rows break anywhere.
  // The table of declared missing values never keeps them: one row lists
  // every variable that declares its code, which can fill a page.
  let short = data.vars.all(x => x.label.clusters().len() < 160 and x.name.clusters().len() < 60)
  // Headers over values, from a dictionary whose keys name the strings.
  let facts(d) = table(
    columns: (1fr,) * d.len(), stroke: (x, y) => (bottom: rule), inset: pad, align: center + horizon,
    fill: (x, y) => if y == 0 { c.band },
    ..d.keys().map(k => hdr(s.at(k))), ..d.values(),
  )
  // A row of its own across the band of a sheet: the label, the declared
  // missing codes, the source. The keys take the width of the longest key
  // the document uses, so the values start at one edge on every sheet and
  // a long label hangs under its first line.
  let keys = (s.label,) + if data.vars.any(x => x.declared_codes != none) { (s.declared_codes,) } else { () } + if data.vars.any(x => x.source != none) { (s.source,) } else { () }
  let wide(word, body) = (table.cell(colspan: 3, align: left, context grid(
    columns: (calc.max(..keys.map(k => measure(hdr(k)).width)), 1fr), column-gutter: 10pt,
    align: left + top, hdr(word), body)),)

  set document(title: data.header, author: data.authors.map(a => a.name))
  // No ligatures: New Computer Modern would turn a "--" or "''" into a dash or a quote.
  set text(font: data.font, size: base, fill: c.text, lang: data.lang,
           ligatures: false, top-edge: "ascender", bottom-edge: "descender")
  set par(leading: 0.22em, spacing: 0.8em, justify: false)
  set page(
    paper: data.paper,
    margin: (top: 1in, bottom: 1in, left: 0.98in, right: 0.98in),
    header-ascent: 0.466in,
    footer-descent: 0.459in,
    header: context if here().page() > 1 {
      align(right, text(base - 2pt, fill: c.muted, data.header))
    },
    // The folio matches the running header: same size, same grey.
    footer: context if here().page() > 1 {
      let total = str(counter(page).final().first())
      align(right, text(base - 2pt, fill: c.muted, counter(page).display() + "\u{2009}/\u{2009}" + total))
    },
  )
  // A hairline under the heading: the type carries the hierarchy, the rule
  // only closes it (0.5pt, lighter than the strokes of the bold type).
  show heading.where(level: 1): it => block(
    width: 100%, above: size.heading-above, below: size.heading-below, sticky: true,
    inset: (bottom: 5pt), stroke: (bottom: 0.5pt + c.primary),
    text(base + 4pt, weight: "bold", fill: c.primary, it.body),
  )
  // The parts of a page under its heading: the notes and the declared
  // missing values under "About the data".
  show heading.where(level: 2): it => block(above: 16pt, below: 8pt, sticky: true,
    text(base + 1pt, weight: "bold", fill: c.primary, it.body))

  // ---- Cover ----------------------------------------------------------------
  // Reading order: the genre as a spaced capital kicker, the study as the
  // title, its subtitle, who and when; the colophon at the foot.
  // Typography of the cover: one bold element, the title; the kicker in
  // spaced capitals at text size; the subtitle, the names and the date in
  // regular weight; the affiliation smaller and muted, the ORCID link
  // smaller still, each on its own line; nothing is italic.
  // Spacing: the gaps are the v() below and nothing else (no paragraph
  // spacing, which would resolve at the size of the title). The kicker and
  // the subtitle hold close to the title; the authors stand apart.
  {
    set align(center)
    set par(leading: 0.3em, spacing: 0pt)
    v(size.cover-top)
    // Without a title, the genre is the title, written once.
    if data.genre != none and data.title != none {
      text(base + 1pt, fill: c.accent, tracking: 0.25em, upper(data.genre))
      v(size.gap + 2pt)
    }
    let title = if data.title != none { data.title } else { data.genre }
    if title != none {
      // A title on several lines, at display leading (1.15).
      par(leading: 0.15 * size.cover-title, text(size.cover-title, weight: "bold", fill: c.primary, title))
    }
    if data.subtitle != none {
      v(size.gap + 4pt)
      text(size.cover-subtitle, fill: c.muted, data.subtitle)
    }
    v(size.cover-block * 2 + 6pt)
    for (i, a) in data.authors.enumerate() {
      if i > 0 { v(size.gap + 8pt) }
      text(base + 2pt, a.name)
      if a.affiliation != "" {
        linebreak()
        text(base - 1pt, fill: c.muted, a.affiliation)
      }
      // The ORCID as its full address, as ORCID asks it to be shown.
      if a.orcid != "" {
        linebreak()
        link("https://orcid.org/" + a.orcid,
             text(base - 2pt, fill: c.accent, "https://orcid.org/" + a.orcid))
      }
    }
    v(size.cover-block + 6pt)
    text(base, fill: c.muted, data.date)
    // The cover carries identity only; the colophon names what produced it.
    let made = data.meta.last()
    place(bottom + center, text(base - 2pt, fill: c.muted, made.field + " " + made.value))
  }
  pagebreak()

  // ---- About the data: facts, notes, declared missing values ----------------
  heading(level: 1, s.about)
  table(
    columns: (size.about-field, 1fr), stroke: (x, y) => (bottom: rule),
    inset: (x, y) => (left: if x == 0 { 0pt } else { 5pt }, right: 5pt, y: 4pt), align: left + top,
    ..data.meta.map(m => (hdr(m.field), m.value)).flatten(),
  )
  v(size.gap)
  text(base - 1pt, style: "italic", fill: c.muted, s.unweighted)
  if data.notes.len() > 0 {
    heading(level: 2, s.notes)
    // A note is a paragraph; consecutive list items make one list. Prose
    // keeps a reading measure (32em, about 70 characters), a typographic
    // apostrophe and, in French, a non-breaking space before : ; ! ?
    set list(indent: 0pt, body-indent: 6pt, spacing: 5pt)
    show regex(" [:;!?]"): it => if data.lang == "fr" { "\u{a0}" + it.text.slice(1) } else { it }
    show "'": "\u{2019}"
    block(width: 32em, for n in data.notes { if n.bullet { list.item(n.text) } else { par(n.text) } })
  }
  if data.declared.len() > 0 {
    heading(level: 2, s.declared)
    listing(
      (s.code, s.label, s.variables), (auto, 1fr, 2fr), (right, left, left),
      data.declared.map(d => (d.code, d.label, mono(d.variables))),
      keep: false,
    )
  }
  pagebreak()

  // ---- List of variables ----------------------------------------------------
  heading(level: 1, s.list)
  listing(
    (s.position, s.name, s.label, s.page), (auto, auto, 1fr, auto),
    (right, left, left, right),
    data.vars.enumerate().map(((i, x)) => (
      x.pos, link(anchor(i), name(x.name)), x.label, page-of(i),
    )),
    keep: short,
  )
  pagebreak()

  // ---- One sheet per variable -----------------------------------------------
  // The heading of the section opens the first sheet, inside its block: a
  // first sheet that fits on a page but not under the heading then breaks
  // under it, instead of leaving the heading alone on its page.
  if data.vars.len() == 0 { heading(level: 1, s.sheets) }
  // One geometry for every sheet: the widest position, type, count and
  // percentage of the whole document set the fixed columns, so the bands
  // and the value tables line up from one sheet to the next.
  let longest(xs) = xs.fold("", (a, b) => if b.len() > a.len() { b } else { a })
  let rows = data.vars.map(x => x.values).flatten()
  let widest = (
    pos: longest(data.vars.map(x => str(x.pos))),
    type: longest(data.vars.map(x => x.type) + (s.type,)),
    n: longest(rows.map(r => r.n) + (s.n,)),
    pct: longest(rows.map(r => r.pct) + (s.pct_total,)),
    valid: longest(rows.map(r => r.valid) + (s.pct_valid,)),
  )
  let col(t) = measure(text(weight: "bold", t)).width + 2 * pad.x
  for (i, x) in data.vars.enumerate() {
    // The missing column only on a sheet that lists declared missing codes.
    // The count and percentage columns keep their place: they are fixed and
    // set against the right edge.
    let flagged = x.values.any(r => r.m)
    // An invisible heading: a PDF bookmark per variable, nothing on the page.
    let title = if x.label == "" { x.name } else { x.name + " \u{2013} " + x.label }
    let mark = place(hide(heading(level: 2, outlined: false, bookmarked: true, title)))
    let lab = if x.label == "" { () } else { wide(s.label, x.label) }
    let declared = if x.declared_codes == none { () } else { wide(s.declared_codes, x.declared_codes) }
    let source = if x.source == none { () } else { wide(s.source, mono(x.source)) }
    // The band carries the variable itself, position, name and type, in
    // white on band_dark (which must stay dark); its keyed rows sit on zebra.
    // A name past 45 characters would run over the type: it takes a dark
    // row of its own, across the band, under the position and the type.
    let inv(t) = text(fill: white, t)
    let long = x.name.clusters().len() > 45
    let dark = if long { 2 } else { 1 }
    let id = name(x.name, limit: 45, weight: "bold")
    let ident = if long {
      (inv(x.pos), [], inv(x.type), table.cell(colspan: 3, align: left + horizon, inv(id)))
    } else {
      (x.pos, id, x.type).map(inv)
    }
    let band = context table(
      columns: (col(widest.pos), 1fr, col(widest.type)), stroke: (x, y) => (bottom: if y >= dark { rule }), inset: pad,
      fill: (col, row) => if row < dark { c.band_dark } else { c.zebra },
      align: (col, row) => (right, left, left).at(col) + horizon,
      ..ident,
      ..lab,
      ..declared,
      ..source,
    )
    let parts = (band, facts(x.counts))
    if x.stats.len() > 0 { parts.push(facts(x.stats)) }
    // The Label column only when the variable has value labels: without
    // them (a factor), the values take the width, and the row of system
    // missing values shows its label in place of the token NA. Labelled
    // codes are numbers, set right; factor levels are text, set left.
    let labelled = x.values.any(r => r.label != "" and not r.na)
    let opt(on, ..items) = if on { items.pos() } else { () }
    let ncol = (if labelled { 2 } else { 1 }) + (if flagged { 1 } else { 0 }) + 4
    let values = context table(
      columns: ((if labelled { (auto, 1fr) } else { (1fr,) }) +
        opt(flagged, col(s.missing)) + (col(widest.n), col(widest.pct), col(widest.valid), 0pt)),
      stroke: (x, y) => (bottom: if y > 0 { rule }), inset: pad,
      fill: (col, row) => if row == 1 { c.band },
      align: (col, row) => ((if labelled { right } else { left },) + opt(labelled, left) + opt(flagged, center) + (right, right, right, left)).at(col) + top,
      table.header(
        // On the pages after the first, the header says whose values go on.
        table.cell(colspan: ncol, inset: 0pt, align: left,
          context if here().page() > locate(anchor(i)).page() {
            block(inset: pad, text(style: "italic", fill: c.muted)[#mono(x.name) (#s.continued)])
          }),
        ..((s.code,) + opt(labelled, s.label) + opt(flagged, s.missing) + (s.n, s.pct_total, s.pct_valid)).map(hdr), []),
      ..kept(x.values.map(r => {
        let f = if r.m or r.na { c.muted } else { c.text }
        ((text(fill: f, if r.na and not labelled { r.label } else { r.code }),) + opt(labelled, text(fill: f, r.label)) +
          opt(flagged, if r.m { text(weight: "bold", s.marker) } else { [] }) +
          (text(fill: f, r.n), text(fill: f, r.pct), text(fill: f, r.valid)))
      })),
    )
    let head = [#if i == 0 { heading(level: 1, s.sheets) }#metadata(i)#anchor(i)#mark]
    let sheet = {
      head
      parts.join(v(size.gap))
      if x.values.len() > 0 { v(size.gap); values }
    }
    // A sheet breaks across pages only when it does not fit on one: the band
    // then sticks to the first rows, the header of the values repeats. The
    // gaps are the same as on a sheet kept whole.
    v(size.sheet, weak: true)
    layout(room => if measure(width: room.width, sheet).height > room.height {
      block(breakable: true, {
        block(sticky: true, { head; parts.join(v(size.gap)) })
        if x.values.len() > 0 { v(size.gap); values }
      })
    } else {
      block(breakable: false, sheet)
    })
  }
  pagebreak()

  // ---- Alphabetical index ---------------------------------------------------
  heading(level: 1, s.index)
  listing(
    (s.name, s.position, s.page), (1fr, auto, auto), (left, right, right),
    data.index.map(i => {
      let x = data.vars.at(i)
      (link(anchor(i), name(x.name)), x.pos, page-of(i))
    }),
    keep: short,
  )
}
