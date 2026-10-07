// Data codebook layout with the design tokens of lssdoc
// (R/render_theme.R, render_cover.R, render_toc.R, render_meta_table.R,
// render_layout.R, render_utils.R, render_item.R).

// ---- Tokens ----------------------------------------------------------------
#let tok = (
  primary: rgb("#133B52"),
  accent: rgb("#3A7C8C"),
  band: rgb("#E9F2F6"),
  band-dark: rgb("#1F4E5F"),
  zebra: rgb("#F4F8FA"),
  grid: rgb("#D3DCE2"),
  text: rgb("#222222"),
  muted: rgb("#6E6E6E"),
  note: rgb("#5B5B5B"),
  warning: rgb("#C45911"),
  // Overridable at compile time, like lssdoc's font / font_code arguments:
  // quarto typst compile codebook.typ --input font=Carlito
  body: (sys.inputs.at("font", default: "Calibri"), "Carlito"),
  code: (sys.inputs.at("font-code", default: "Consolas"), "DejaVu Sans Mono"),
)
#let base = 10pt
#let size = (
  meta: base - 2pt,      // running header
  help: base - 2pt,      // grey note under each fiche
  h1: base + 4pt,        // section headings
  toc: base + 1pt,       // table of contents entries
  footer: base,          // X/Y page counter
  cover-title: 22pt,
  cover-subtitle: 16pt,
)
#let rule = 0.5pt + tok.grid
#let cell-inset = (x: 4pt, y: 4pt)

#let mono(body, ..args) = text(font: tok.code, ..args, body)
#let hdr(body) = text(weight: "bold", fill: tok.primary, body)
#let anchor(name) = label("fiche-" + name)

// Widow/orphan control for tables that may break: the caller adds a
// zero-width last column; this fills it with unbreakable rowspan cells
// that keep the first `head` and the last `tail` body rows on one page.
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

// ---- Cover -----------------------------------------------------------------
#let cover(meta) = {
  set align(center)
  v(10pt)
  text(size.cover-title, weight: "bold", fill: tok.primary, meta.title)
  v(3pt)
  text(size.cover-subtitle, style: "italic", fill: tok.muted, meta.subtitle)
  v(2pt)
  text(base + 1pt, fill: tok.muted, meta.date_long)
  v(6pt)
  let rows = meta.fields.map(f => (hdr(f.key), f.value.split("\n").join(linebreak())))
  rows.push((hdr[Notes], list(
    marker: [•], indent: 0pt, body-indent: 5pt, spacing: 4pt,
    ..meta.notes.map(n => [#n]),
  )))
  table(
    columns: (1.4in, 3.2in),
    stroke: (x, y) => (bottom: 0.5pt + tok.band),
    inset: (x: 5pt, y: 4pt),
    align: left + top,
    ..rows.flatten(),
  )
}

// ---- Table of contents -------------------------------------------------------
#let toc() = {
  heading(level: 1, outlined: false)[Table des matières]
  context {
    for hd in query(heading.where(level: 1, outlined: true)) {
      let loc = hd.location()
      block(above: 4.4pt, below: 4.4pt, link(loc, text(size.toc, fill: tok.accent)[
        #text(weight: "bold", hd.body)
        #box(width: 1fr, repeat(text(fill: tok.grid)[.#h(2pt)]))
        #loc.page()
      ]))
    }
  }
}

// ---- List of variables (file order) ----------------------------------------
#let var-list(vars) = table(
  columns: (0.4in, 1.8in, 1fr, 1.1in, 0.6in, 0.8in, 0pt),
  stroke: (x, y) => (bottom: rule),
  fill: (x, y) => if y == 0 { tok.band },
  inset: cell-inset,
  align: (x, y) => (right, left, left, left, right, right).at(x, default: left) + horizon,
  table.header(..([N°], [Variable], [Libellé], [Type], [Valides], [Manquants]).map(hdr), []),
  ..kept(vars.map(x => (
    [#x.pos], link(anchor(x.name), mono(x.name)), x.label, x.type, x.valid, x.missing,
  ))),
)

// ---- One fiche ---------------------------------------------------------------
// Meta band: dark header (white bold labels), zebra body row, optional
// "Question" line (source code) under it.
#let band(x) = {
  let src = x.at("source", default: none)
  table(
    columns: (0.4in, 1.9in, 1fr, 1.15in),
    stroke: rule,
    inset: cell-inset,
    fill: (c, r) => if r == 0 { tok.band-dark } else if r == 1 { tok.zebra },
    align: (c, r) => (right, left, left, center).at(c) + horizon,
    table.header(..([N°], [Variable], [Libellé], [Type]).map(
      h => text(weight: "bold", fill: white, h))),
    [#x.pos], mono(weight: "bold", x.name), x.label, x.type,
    ..if src != none {
      (table.cell(colspan: 4, align: left + horizon)[
        #hdr[Question]#h(10pt)#mono(src)],)
    } else { () },
  )
}

// Categorical: code | label | M | n | % | valid %; declared missing codes
// (and system NA) flagged M, muted, counted in the total only.
#let cat-table(x) = {
  let aligns = (right, left, right, right, right)
  let n = x.rows.len()
  let body = x.rows.map(r => {
    let col = if r.missing { tok.muted } else { tok.text }
    let code = if r.missing {
      [#text(weight: "bold", fill: tok.warning)[M]#h(1fr)#text(fill: col, r.code)]
    } else { r.code }
    (code, text(fill: col, r.label),
     text(fill: col, r.n), text(fill: col, r.pct), text(fill: col, r.vpct))
  })
  body.push((
    table.cell(align: left + horizon, hdr[Total]),
    text(style: "italic", fill: tok.muted)[dont #x.total.n_valid valides],
    text(weight: "bold", x.total.n), x.total.pct, x.total.vpct,
  ))
  table(
    columns: (0.75in, 1fr, 0.75in, 0.7in, 0.85in, 0pt),
    stroke: rule,
    inset: cell-inset,
    align: (c, r) => aligns.at(c, default: left) + horizon,
    fill: (c, r) => if r == 0 or r == n + 1 { tok.band },
    table.header(..([Code], [Libellé], [n], [%], [% valides]).map(hdr), []),
    ..kept(body),
  )
}

#let stat-table(heads, vals) = table(
  columns: (1fr,) * heads.len(),
  stroke: rule,
  inset: cell-inset,
  align: center + horizon,
  fill: (c, r) => if r == 0 { tok.band },
  table.header(..heads.map(hdr)),
  ..vals,
)

#let fiche(x) = {
  let body = if x.kind == "categorical" { cat-table(x) }
    else if x.kind == "numeric" {
      stat-table(([Valides], [Manquants], [Moyenne], [Écart-type], [Min], [Max]),
        (x.valid, x.missing, x.stats.mean, x.stats.sd, x.stats.min, x.stats.max))
    } else if x.kind == "date" {
      stat-table(([Valides], [Manquants], [Min], [Max]),
        (x.valid, x.missing, x.stats.min, x.stats.max))
    } else {
      stat-table(([Valides], [Manquants], [Valeurs distinctes]),
        (x.valid, x.missing, x.stats.distinct))
    }
  // Invisible level-2 heading: a PDF bookmark per variable, nothing on the page.
  let mark = place(hide(heading(level: 2, outlined: false, bookmarked: true,
    x.name + " — " + x.label)))
  let head = [#metadata(x.name)#anchor(x.name)#metadata(x.name)<fiche-start>#mark]
  let note = block(above: 4pt, text(size.help, style: "italic", fill: tok.note)[
    Effectifs non pondérés, décrivant le fichier.])
  let tail = [#metadata(x.name)<fiche-end>]
  if x.at("breakable", default: false) {
    // Long fiche: may split; the band sticks to the first rows and the
    // table header repeats on every page.
    block(above: 24pt, breakable: true, {
      block(sticky: true, below: 10pt, { head; band(x) })
      body; note; tail
    })
  } else {
    block(above: 24pt, breakable: false, {
      head
      block(below: 10pt, band(x))
      body; note; tail
    })
  }
}

// ---- Index (alphabetical, page numbers resolved by Typst) -------------------
#let var-index(vars) = table(
  columns: (2.6in, 0.6in, 0.6in),
  stroke: (x, y) => (bottom: rule),
  fill: (x, y) => if y == 0 { tok.band },
  inset: cell-inset,
  align: (x, y) => (left, right, right).at(x) + horizon,
  table.header(..([Variable], [N°], [Page]).map(hdr)),
  ..vars.sorted(key: x => lower(x.name)).map(x => (
    link(anchor(x.name), mono(x.name)),
    [#x.pos],
    context link(anchor(x.name), text(fill: tok.accent, str(locate(anchor(x.name)).page()))),
  )).flatten(),
)

// ---- Document ----------------------------------------------------------------
#let codebook(data) = {
  let meta = data.meta
  set document(title: meta.title + " — " + meta.subtitle)
  set text(font: tok.body, size: base, fill: tok.text, lang: "fr",
           top-edge: "ascender", bottom-edge: "descender")
  // justify pinned: under a .qmd, Quarto's article template sets it to true
  set par(leading: 0.22em, spacing: 0.8em, justify: false)
  set page(
    paper: "a4",
    margin: (top: 1in, bottom: 1in, left: 0.98in, right: 0.98in),
    header-ascent: 0.466in,
    footer-descent: 0.459in,
    header: align(right, text(size.meta, fill: tok.muted, meta.title)),
    footer: context align(right, text(size.footer, fill: tok.muted)[
      #counter(page).display()/#counter(page).final().first()]),
  )
  show heading.where(level: 1): it => block(
    width: 100%, above: 18pt, below: 9pt, sticky: true,
    inset: (bottom: 4pt), stroke: (bottom: 1pt + tok.primary),
    text(size.h1, weight: "bold", fill: tok.primary, it.body),
  )

  cover(meta)
  pagebreak()
  toc()
  v(10pt)
  heading(level: 1)[Liste des variables]
  var-list(data.vars)
  pagebreak()
  heading(level: 1)[Fiches des variables]
  for x in data.vars { fiche(x) }
  pagebreak()
  heading(level: 1)[Index des variables]
  var-index(data.vars)
}
