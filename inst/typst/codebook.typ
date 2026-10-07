// Layout of the PDF codebook of spicy::code_book(). R appends two
// dictionaries to this file, `data` (the codebook, numbers already
// formatted) and `strings` (every word the reader sees, in the language of
// the codebook), then calls `codebook(data, strings)`: the layout holds no
// text of its own. Design tokens, sizes and bands follow lssdoc.

#let base = 10pt
#let pad = (x: 4pt, y: 4pt)
#let size = (
  cover-title: 26pt, cover-top: 1.4in, cover-block: 18pt, // title block, and the air around it
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
  // Monospace for identifiers only (variable names, source codes, ORCID),
  // at 0.9em so its x-height sits with the body font. Values and labels
  // stay in the body font, as in published codebooks.
  let mono(size: 0.9em, ..args) = text(font: data.font_code, size: size, ..args)
  let hdr(body) = text(weight: "bold", fill: c.primary, body)
  let anchor(i) = label("cb-var-" + str(i))
  let page-of(i) = context link(anchor(i), text(fill: c.accent, str(locate(anchor(i)).page())))
  // A long name is set smaller, never cut: readers copy names from the PDF.
  // The limit is where the name starts to squeeze its neighbours: 30 in
  // the list and the index, 45 in the band of a sheet.
  let name(n, limit: 30, weight: "regular") = mono(
    size: if n.clusters().len() > limit { size.long-name } else { 0.9 * base },
    weight: weight,
    n,
  )
  // Rows under a header that repeats on every page.
  let listing(heads, widths, aligns, rows) = table(
    columns: widths + (0pt,), stroke: (x, y) => (bottom: rule), inset: pad,
    fill: (x, y) => if y == 0 { c.band },
    align: (x, y) => aligns.at(x, default: left) + horizon,
    table.header(..heads.map(hdr), []),
    ..kept(rows),
  )
  // Headers over values, from a dictionary whose keys name the strings.
  let facts(d) = table(
    columns: (1fr,) * d.len(), stroke: rule, inset: pad, align: center + horizon,
    fill: (x, y) => if y == 0 { c.band },
    ..d.keys().map(k => hdr(s.at(k))), ..d.values(),
  )
  // A row of its own across the band of a sheet: the label, the source.
  let wide(word, body) = (table.cell(colspan: 3, align: left)[#hdr(word)#h(10pt)#body],)

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
    footer: context if here().page() > 1 {
      let total = str(counter(page).final().first())
      align(right, text(fill: c.muted, counter(page).display() + "/" + total))
    },
  )
  show heading.where(level: 1): it => block(
    width: 100%, above: size.heading-above, below: size.heading-below, sticky: true,
    inset: (bottom: 4pt), stroke: (bottom: 1pt + c.primary),
    text(base + 4pt, weight: "bold", fill: c.primary, it.body),
  )

  // ---- Cover ----------------------------------------------------------------
  // Reading order: the genre as a spaced capital kicker, the study as the
  // title, who and when; a short rule; the two facts on one line; the notes
  // across the page; the colophon at the foot.
  {
    set align(center)
    v(size.cover-top)
    if data.subtitle != none {
      text(base, weight: "bold", fill: c.accent, tracking: 0.18em, upper(data.subtitle))
      v(size.gap)
    }
    let title = if data.title != none { data.title } else { data.subtitle }
    if title != none {
      text(size.cover-title, weight: "bold", fill: c.primary, title)
    }
    v(size.cover-block)
    for a in data.authors {
      v(4pt)
      text(base + 1pt, a.name)
      if a.affiliation != "" {
        text(style: "italic", fill: c.muted, "  \u{2014}  " + a.affiliation)
      }
      if a.orcid != "" {
        linebreak()
        mono(size: base - 1pt, fill: c.muted, s.orcid + " ")
        link("https://orcid.org/" + a.orcid,
             mono(size: base - 1pt, fill: c.accent, underline(a.orcid)))
      }
    }
    v(8pt)
    text(base + 1pt, fill: c.muted, data.date)
    // The cover carries identity only; the colophon names what produced it.
    place(bottom + center, text(base - 1pt, fill: c.muted, data.meta.last().value))
  }
  pagebreak()

  // ---- About the data: facts, notes, declared missing values ----------------
  heading(level: 1, s.about)
  table(
    columns: (size.about-field, 1fr), stroke: (x, y) => (bottom: 0.5pt + c.band),
    inset: (x: 5pt, y: 4pt), align: left + top,
    ..data.meta.map(m => (hdr(m.field), m.value)).flatten(),
  )
  v(size.gap)
  text(base - 1pt, style: "italic", fill: c.muted, s.unweighted)
  if data.notes.len() > 0 {
    heading(level: 1, s.notes)
    list(indent: 0pt, body-indent: 6pt, spacing: 5pt, ..data.notes.map(n => [#n]))
  }
  if data.declared.len() > 0 {
    heading(level: 1, s.declared)
    listing(
      (s.code, s.label, s.variables), (auto, 1fr, 1fr), (left, left, left),
      data.declared.map(d => (d.code, d.label, mono(d.variables))),
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
  )
  pagebreak()

  // ---- One sheet per variable -----------------------------------------------
  heading(level: 1, s.sheets)
  // One geometry for every sheet: the widest position, type, count and
  // percentage of the whole document set the fixed columns, so the bands
  // and the value tables line up from one sheet to the next.
  let longest(xs) = xs.fold("", (a, b) => if b.len() > a.len() { b } else { a })
  let rows = data.vars.map(x => x.values).flatten()
  let widest = (
    pos: longest(data.vars.map(x => str(x.pos)) + (s.position,)),
    type: longest(data.vars.map(x => x.type) + (s.type,)),
    n: longest(rows.map(r => r.n) + (s.n,)),
    pct: longest(rows.map(r => r.pct) + (s.pct_total,)),
    valid: longest(rows.map(r => r.valid) + (s.pct_valid,)),
  )
  let col(t) = measure(text(weight: "bold", t)).width + 2 * pad.x
  // The missing column exists only when the document declares missing codes.
  let flagged = rows.any(r => r.m)
  for (i, x) in data.vars.enumerate() {
    // An invisible heading: a PDF bookmark per variable, nothing on the page.
    let title = if x.label == "" { x.name } else { x.name + " \u{2014} " + x.label }
    let mark = place(hide(heading(level: 2, outlined: false, bookmarked: true, title)))
    let lab = if x.label == "" { () } else { wide(s.label, x.label) }
    let source = if x.source == none { () } else { wide(s.source, mono(x.source)) }
    let band = context table(
      columns: (col(widest.pos), 1fr, col(widest.type)), stroke: rule, inset: pad,
      // band_dark must stay dark: the text of the band is white.
      fill: (col, row) => if row == 0 { c.band_dark } else if row == 1 { c.zebra },
      align: (col, row) => (right, left, left).at(col) + horizon,
      ..(s.position, s.name, s.type).map(h => text(weight: "bold", fill: white, h)),
      x.pos, name(x.name, limit: 45, weight: "bold"), x.type,
      ..lab,
      ..source,
    )
    let parts = (band, facts(x.counts))
    if x.stats.len() > 0 { parts.push(facts(x.stats)) }
    // Without value labels (a factor), the codes take the width.
    let labelled = x.values.any(r => r.label != "" and not r.na)
    let opt(..items) = if flagged { items.pos() } else { () }
    let values = context table(
      columns: ((if labelled { (auto, 1fr) } else { (1fr, auto) }) +
        opt(col(s.missing)) + (col(widest.n), col(widest.pct), col(widest.valid), 0pt)),
      stroke: rule, inset: pad,
      fill: (col, row) => if row == 0 { c.band },
      align: (col, row) => ((left, left) + opt(center) + (right, right, right, left)).at(col) + horizon,
      table.header(..((s.code, s.label) + opt(s.missing) + (s.n, s.pct_total, s.pct_valid)).map(hdr), []),
      ..kept(x.values.map(r => {
        let f = if r.m or r.na { c.muted } else { c.text }
        ((text(fill: f, r.code), text(fill: f, r.label)) +
          opt(if r.m { text(weight: "bold", fill: c.accent, s.marker) } else { [] }) +
          (text(fill: f, r.n), text(fill: f, r.pct), text(fill: f, r.valid)))
      })),
    )
    let head = [#metadata(i)#anchor(i)#mark]
    let sheet = {
      head
      parts.join(v(size.gap))
      if x.values.len() > 0 { v(size.gap); values }
    }
    // A sheet breaks across pages only when it does not fit on one: the band
    // then sticks to the first rows, the header of the values repeats.
    v(size.sheet, weak: true)
    layout(room => if measure(width: room.width, sheet).height > room.height {
      block(breakable: true, {
        block(sticky: true, below: size.gap, { head; parts.join(v(size.gap)) })
        if x.values.len() > 0 { values }
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
  )
}
