# Build APA-Style Cross-Tabulation Tables From Multiple Row Variables

`table_apa()` builds a publication-ready table by crossing one grouping
variable (`group_var`) with one or many row variables (`row_vars`),
using
[`spicy::cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
internally.

## Usage

``` r
table_apa(
  data,
  row_vars,
  group_var,
  labels = NULL,
  levels_keep = NULL,
  include_total = TRUE,
  drop_na = TRUE,
  weights = NULL,
  rescale = FALSE,
  correct = FALSE,
  simulate_p = TRUE,
  simulate_B = 2000,
  percent_digits = 1,
  p_digits = 3,
  v_digits = 2,
  decimal_mark = ".",
  output = c("wide", "long", "tinytable", "flextable", "excel", "clipboard", "word"),
  style = c("auto", "raw", "report"),
  indent_text = "  ",
  indent_text_excel_clipboard = "      ",
  add_multilevel_header = TRUE,
  blank_na_wide = FALSE,
  excel_path = NULL,
  excel_sheet = "APA",
  clipboard_delim = "\t",
  word_path = NULL
)
```

## Arguments

- data:

  A data frame.

- row_vars:

  Character vector of variable names to place in rows.

- group_var:

  Single character variable name used for columns/groups.

- labels:

  Optional character labels for `row_vars` (same length).

- levels_keep:

  Optional character vector of levels to keep/order for row modalities.
  If `NULL`, all observed levels are kept.

- include_total:

  Logical; include `Total` group if available.

- drop_na:

  Logical; if `TRUE`, remove rows with NA in row/group variable before
  each cross-tabulation.

- weights:

  Optional weights. Either `NULL`, a numeric vector of length
  `nrow(data)`, or a single column name in `data`.

- rescale:

  Logical; passed to
  [`spicy::cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  to rescale weights.

- correct:

  Logical; passed to
  [`spicy::cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  (Yates correction in 2x2 chi-squared contexts).

- simulate_p:

  Logical; passed to
  [`spicy::cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).

- simulate_B:

  Integer; Monte Carlo replicates when `simulate_p = TRUE`.

- percent_digits:

  Number of digits for percentages in report outputs.

- p_digits:

  Number of digits for p-values (except `< .001`).

- v_digits:

  Number of digits for Cramer's V.

- decimal_mark:

  Decimal separator (`"."` or `","`).

- output:

  Output format: `"wide"`, `"long"`, `"tinytable"`, `"flextable"`,
  `"excel"`, `"clipboard"`, `"word"`.

- style:

  `"raw"` for machine-friendly outputs, `"report"` for formatted
  outputs, `"auto"` to select by output type.

- indent_text:

  Prefix used for modality labels in report table building.

- indent_text_excel_clipboard:

  Stronger indentation used in Excel and clipboard exports.

- add_multilevel_header:

  Logical; merge top headers in Excel export.

- blank_na_wide:

  Logical; replace NA by empty strings in wide raw output.

- excel_path:

  Path for `output = "excel"`.

- excel_sheet:

  Sheet name for Excel export.

- clipboard_delim:

  Delimiter for clipboard text export.

- word_path:

  Path for `output = "word"` or optional save path when
  `output = "flextable"`.

## Value

Depends on `output` and `style`:

- `"long"` + `"raw"`: long numeric data frame.

- `"wide"` + `"raw"`: wide numeric data frame.

- `"long"` + `"report"`: long formatted character data frame.

- `"wide"` + `"report"`: wide formatted character data frame.

- `"tinytable"`: a `tinytable` object.

- `"flextable"`: a `flextable` object.

- `"excel"` / `"clipboard"` / `"word"`: invisibly returns written
  object/path.

## Details

It supports raw data outputs (`wide`, `long`) and report-oriented
outputs (`tinytable`, `flextable`, `excel`, `clipboard`, `word`) with
multi-level headers, p-values, and Cramer's V.

Optional output engines require suggested packages:

- `tinytable` for `output = "tinytable"`

- `flextable` + `officer` for `output = "flextable"`/`"word"`

- `openxlsx` for `output = "excel"`

- `clipr` for `output = "clipboard"`

## Examples

``` r
# Build a minimal reproducible dataset
d_ex <- transform(
  mtcars,
  hes = factor(gear, labels = c("BFH", "HEdS-Geneve", "HESAV")),
  emploi_sf = ifelse(vs == 1, "Oui", "Non"),
  role_prof_recherche = ifelse(am == 1, "Oui", "Non"),
  w = mpg
)

# Raw long output (machine-friendly)
table_apa(
  data = d_ex,
  row_vars = c("emploi_sf", "role_prof_recherche"),
  group_var = "hes",
  labels = c("Emploi SF", "Role recherche"),
  output = "long",
  style = "raw"
)
#>          variable level       group  n   pct     p Cramer's V
#> 1       Emploi SF   Non         BFH 12  80.0 0.001       0.62
#> 2       Emploi SF   Non HEdS-Geneve  2  16.7 0.001       0.62
#> 3       Emploi SF   Non       HESAV  4  80.0 0.001       0.62
#> 4       Emploi SF   Non       Total 18  56.2 0.001       0.62
#> 5       Emploi SF   Oui         BFH  3  20.0 0.001       0.62
#> 6       Emploi SF   Oui HEdS-Geneve 10  83.3 0.001       0.62
#> 7       Emploi SF   Oui       HESAV  1  20.0 0.001       0.62
#> 8       Emploi SF   Oui       Total 14  43.8 0.001       0.62
#> 9  Role recherche   Non         BFH 15 100.0 0.001       0.81
#> 10 Role recherche   Non HEdS-Geneve  4  33.3 0.001       0.81
#> 11 Role recherche   Non       HESAV  0   0.0 0.001       0.81
#> 12 Role recherche   Non       Total 19  59.4 0.001       0.81
#> 13 Role recherche   Oui         BFH  0   0.0 0.001       0.81
#> 14 Role recherche   Oui HEdS-Geneve  8  66.7 0.001       0.81
#> 15 Role recherche   Oui       HESAV  5 100.0 0.001       0.81
#> 16 Role recherche   Oui       Total 13  40.6 0.001       0.81

# Raw wide output
table_apa(
  data = d_ex,
  row_vars = c("emploi_sf", "role_prof_recherche"),
  group_var = "hes",
  labels = c("Emploi SF", "Role recherche"),
  output = "wide",
  style = "raw"
)
#>         Variable Level BFH n BFH % HEdS-Geneve n HEdS-Geneve % HESAV n HESAV %
#> 1      Emploi SF   Non    12    80             2          16.7       4      80
#> 2      Emploi SF   Oui     3    20            10          83.3       1      20
#> 3 Role recherche   Non    15   100             4          33.3       0       0
#> 4 Role recherche   Oui     0     0             8          66.7       5     100
#>   Total n Total %     p Cramer's V
#> 1      18    56.2 0.002       0.62
#> 2      14    43.8 0.002       0.62
#> 3      19    59.4 0.001       0.81
#> 4      13    40.6 0.001       0.81

# Weighted example
table_apa(
  data = d_ex,
  row_vars = c("emploi_sf", "role_prof_recherche"),
  group_var = "hes",
  labels = c("Emploi SF", "Role recherche"),
  weights = "w",
  rescale = TRUE,
  simulate_p = FALSE,
  output = "long",
  style = "raw"
)
#>          variable level       group        n   pct     p Cramer's V
#> 1       Emploi SF   Non         BFH  9.00000  74.8 0.003       0.59
#> 2       Emploi SF   Non HEdS-Geneve  2.00000  14.3 0.003       0.59
#> 3       Emploi SF   Non       HESAV  4.00000  71.6 0.003       0.59
#> 4       Emploi SF   Non       Total 14.88754  46.5 0.003       0.59
#> 5       Emploi SF   Oui         BFH  3.00000  25.2 0.003       0.59
#> 6       Emploi SF   Oui HEdS-Geneve 13.00000  85.7 0.003       0.59
#> 7       Emploi SF   Oui       HESAV  2.00000  28.4 0.003       0.59
#> 8       Emploi SF   Oui       Total 17.11246  53.5 0.003       0.59
#> 9  Role recherche   Non         BFH 12.00000 100.0 0.001       0.79
#> 10 Role recherche   Non HEdS-Geneve  4.00000  28.6 0.001       0.79
#> 11 Role recherche   Non       HESAV  0.00000   0.0 0.001       0.79
#> 12 Role recherche   Non       Total 16.21652  50.7 0.001       0.79
#> 13 Role recherche   Oui         BFH  0.00000   0.0 0.001       0.79
#> 14 Role recherche   Oui HEdS-Geneve 10.00000  71.4 0.001       0.79
#> 15 Role recherche   Oui       HESAV  5.00000 100.0 0.001       0.79
#> 16 Role recherche   Oui       Total 15.78348  49.3 0.001       0.79

# \donttest{
# Optional output: tinytable
if (requireNamespace("tinytable", quietly = TRUE)) {
  table_apa(
    data = d_ex,
    row_vars = c("emploi_sf", "role_prof_recherche"),
    group_var = "hes",
    labels = c("Emploi SF", "Role recherche"),
    output = "tinytable"
  )
}
#> <!-- preamble start -->
#> <!DOCTYPE html> 
#> <html lang="en">
#>   <head>
#>     <meta charset="UTF-8">
#>     <meta name="viewport" content="width=device-width, initial-scale=1.0">
#>     <title>tinytable_kyw40u2q3g69uhqt5hsf</title>
#>     
#>   </head>
#>   <body>
#> <!-- preamble end -->
#> 
#>     <script>
#> // TinyTable JavaScript Framework
#> // Function factory for creating table-specific operations
#> window.TinyTable = {
#>   // Helper to create Tabulator instance with Luxon dependency
#>   createTabulator: function(elementId, config) {
#>     const { DateTime } = window.luxon || {};
#>     return new Tabulator(elementId, {
#>       dependencies: { DateTime },
#>       ...config
#>     });
#>   },
#> 
#>   createTableFunctions: function (tableId) {
#>     const spans = []; // {top, left, rowspan, colspan}
#> 
#>     function resolveToAnchor(i, j) {
#>       // find if (i,j) is inside any span; return that span's anchor if so
#>       for (const s of spans) {
#>         if (
#>           i >= s.top && i < s.top + s.rowspan &&
#>           j >= s.left && j < s.left + s.colspan
#>         ) {
#>           return { i: s.top, j: s.left };
#>         }
#>       }
#>       return { i, j };
#>     }
#> 
#>     return {
#>       styleCell: function (i, j, css_id) {
#>         // normalize possible string indices
#>         i = +i; j = +j;
#> 
#>         const table = document.getElementById(tableId);
#>         const { i: ai, j: aj } = resolveToAnchor(i, j);
#>         const cell = table.querySelector(`[data-row="${ai}"][data-col="${aj}"]`);
#>         if (!cell) return; // silently skip if not found (e.g., removed)
#>         cell.classList.add(css_id);
#>       },
#> 
#>       spanCell: function (i, j, rowspan, colspan) {
#>         i = +i; j = +j;
#> 
#>         const table = document.getElementById(tableId);
#>         const targetCell = table.querySelector(`[data-row="${i}"][data-col="${j}"]`);
#>         if (!targetCell) {
#>           console.warn(`Cell at (${i}, ${j}) not found.`);
#>           return;
#>         }
#> 
#>         // record span so future styleCell calls map to the anchor
#>         spans.push({ top: i, left: j, rowspan, colspan });
#> 
#>         // remove covered cells (except anchor)
#>         const cellsToRemove = [];
#>         for (let r = 0; r < rowspan; r++) {
#>           for (let c = 0; c < colspan; c++) {
#>             if (r === 0 && c === 0) continue;
#>             const cell = table.querySelector(`[data-row="${i + r}"][data-col="${j + c}"]`);
#>             if (cell) cellsToRemove.push(cell);
#>           }
#>         }
#>         cellsToRemove.forEach(cell => cell.remove());
#> 
#>         // set HTML attributes for clarity/serialization
#>         targetCell.setAttribute('rowspan', rowspan);
#>         targetCell.setAttribute('colspan', colspan);
#>       }
#>     };
#>   }
#> };
#> 
#> // =============================================================================
#> // TABULATOR CUSTOM FORMATTERS (revised)
#> // =============================================================================
#> 
#> // SVG sparkline
#> function tinytable_sparkline(cell, p = {}) {
#>   let values = cell.getValue() ?? [];
#>   if (!Array.isArray(values)) values = [values];
#>   // coerce to finite numbers & strip NaNs
#>   values = values.map(Number).filter(Number.isFinite);
#>   const n = values.length;
#>   if (n === 0) return "";
#> 
#>   const w = p.width ?? 120, h = p.height ?? 30, pad = p.pad ?? 2;
#>   const color = p.color ?? "currentColor", sw = p.strokeWidth ?? 1.5;
#>   const fill = p.fillArea ?? false;
#>   const showDots = p.showDots ?? false;
#>   const dotR = p.dotRadius ?? 1.75;
#>   const ariaLabel = p.ariaLabel ?? `sparkline with ${n} points`;
#> 
#>   // single pass for min/max
#>   let min = Infinity, max = -Infinity;
#>   for (let v of values) { if (v < min) min = v; if (v > max) max = v; }
#>   const span = (max - min) || 1;
#> 
#>   const x = i => pad + (i * (w - 2 * pad)) / Math.max(n - 1, 1);
#>   const y = v => h - pad - ((v - min) * (h - 2 * pad)) / span;
#> 
#>   let d = `M ${x(0)},${y(values[0])}`;
#>   for (let i = 1; i < n; i++) d += ` L ${x(i)},${y(values[i])}`;
#> 
#>   const NS = "http://www.w3.org/2000/svg";
#>   const svg = document.createElementNS(NS, "svg");
#>   svg.setAttribute("viewBox", `0 0 ${w} ${h}`);
#>   svg.setAttribute("width", w);
#>   svg.setAttribute("height", h);
#>   svg.setAttribute("role", "img");
#>   svg.setAttribute("aria-label", ariaLabel);
#> 
#>   const path = document.createElementNS(NS, "path");
#>   path.setAttribute("stroke", color);
#>   path.setAttribute("stroke-width", sw);
#>   path.setAttribute("vector-effect", "non-scaling-stroke");
#> 
#>   if (fill) {
#>     const area = document.createElementNS(NS, "path");
#>     area.setAttribute("d", d + ` L ${x(n - 1)},${h - pad} L ${x(0)},${h - pad} Z`);
#>     area.setAttribute("fill", color);
#>     area.setAttribute("fill-opacity", p.fillOpacity ?? 0.25);
#>     svg.appendChild(area);
#>     path.setAttribute("d", d);
#>     path.setAttribute("fill", "none");
#>   } else {
#>     path.setAttribute("d", d);
#>     path.setAttribute("fill", "none");
#>   }
#>   svg.appendChild(path);
#> 
#>   if (showDots) {
#>     const dot = (i) => {
#>       const c = document.createElementNS(NS, "circle");
#>       c.setAttribute("cx", x(i));
#>       c.setAttribute("cy", y(values[i]));
#>       c.setAttribute("r", dotR);
#>       c.setAttribute("fill", color);
#>       return c;
#>     };
#>     if (p.dots === "end") svg.appendChild(dot(n - 1));
#>     else if (p.dots === "minmax") {
#>       const iMin = values.indexOf(min), iMax = values.indexOf(max);
#>       svg.appendChild(dot(iMin)); svg.appendChild(dot(iMax));
#>     } else if (p.dots === true) {
#>       for (let i = 0; i < n; i++) svg.appendChild(dot(i));
#>     }
#>   }
#> 
#>   if (p.title) {
#>     const title = document.createElementNS(NS, "title");
#>     title.textContent = p.title;
#>     svg.appendChild(title);
#>   }
#> 
#>   return svg;
#> }
#> 
#> // Canvas histogram
#> function tinytable_histogram(cell, p = {}) {
#>   let data = cell.getValue() ?? [];
#>   if (!Array.isArray(data)) data = [data];
#>   data = data.map(Number).filter(Number.isFinite);
#>   const n = data.length;
#>   if (n === 0) return "";
#> 
#>   const cssWidth = p.width ?? 120;
#>   const cssHeight = p.height ?? 30;
#>   const color = p.color ?? "currentColor";
#>   const gap = p.gap ?? 1;         // px gap between bars
#>   const pad = p.pad ?? 0;         // inner padding
#>   const ariaLabel = p.ariaLabel ?? `histogram with ${n} observations`;
#> 
#>   // robust bins: Sturges as default; allow override
#>   const k = Math.max(1, Math.min(64, p.bins ?? Math.ceil(Math.log2(n) + 1)));
#> 
#>   // stats
#>   let min = Infinity, max = -Infinity;
#>   for (let v of data) { if (v < min) min = v; if (v > max) max = v; }
#> 
#>   // zero-range safeguard -> single centered bar
#>   const range = max - min;
#>   const binWidth = range > 0 ? range / k : 1;
#> 
#>   const bins = new Array(k).fill(0);
#>   if (range === 0) {
#>     // put all mass in the middle bin
#>     bins[Math.floor(k / 2)] = n;
#>   } else {
#>     for (let v of data) {
#>       let idx = Math.floor((v - min) / binWidth);
#>       if (idx >= k) idx = k - 1; // clamp max edge
#>       bins[idx]++;
#>     }
#>   }
#>   const maxCount = Math.max(...bins, 1);
#> 
#>   // HiDPI / Retina crispness
#>   const dpr = window.devicePixelRatio || 1;
#>   const canvas = document.createElement("canvas");
#>   canvas.width = Math.round(cssWidth * dpr);
#>   canvas.height = Math.round(cssHeight * dpr);
#>   canvas.style.width = cssWidth + "px";
#>   canvas.style.height = cssHeight + "px";
#>   canvas.setAttribute("role", "img");
#>   canvas.setAttribute("aria-label", ariaLabel);
#> 
#>   const ctx = canvas.getContext("2d");
#>   ctx.scale(dpr, dpr);
#>   ctx.clearRect(0, 0, cssWidth, cssHeight);
#>   ctx.fillStyle = color;
#> 
#>   const innerW = cssWidth - pad * 2;
#>   const innerH = cssHeight - pad * 2;
#>   const barW = innerW / k;
#> 
#>   for (let i = 0; i < k; i++) {
#>     const h = (bins[i] / maxCount) * innerH;
#>     const x = pad + i * barW;
#>     const y = cssHeight - pad - h;
#>     // 0.5 translate helps crisp vertical edges on some displays
#>     ctx.fillRect(Math.round(x) + 0.5, Math.floor(y), Math.max(0, barW - gap), Math.ceil(h));
#>   }
#> 
#>   if (p.title) canvas.title = p.title;
#>   return canvas;
#> }
#> 
#> // Unified sorter that uses hidden rank field
#> function tinytable_rank_sorter(a, b, aRow, bRow, column, dir, sorterParams) {
#>   const rankField = sorterParams.rankField;
#>   const aVal = aRow.getData()[rankField] || 0;
#>   const bVal = bRow.getData()[rankField] || 0;
#>   return aVal - bVal;
#> }
#> </script>
#> 
#>     <script>
#>       // Create table-specific functions using external factory
#>       const tableFns_kyw40u2q3g69uhqt5hsf = TinyTable.createTableFunctions("tinytable_kyw40u2q3g69uhqt5hsf");
#>       // tinytable span after
#>       window.addEventListener('load', function () {
#>           var cellsToStyle = [
#>             // tinytable style arrays after
#>           { positions: [ { i: '-1', j: 10 }, { i: '-1', j: 11 } ], css_id: 'tinytable_css_xd9nhufjb3jz27aorm22',}, 
#>           { positions: [ { i: '1', j: 2 }, { i: '2', j: 2 }, { i: '3', j: 2 }, { i: '4', j: 2 }, { i: '5', j: 2 }, { i: '1', j: 3 }, { i: '2', j: 3 }, { i: '3', j: 3 }, { i: '4', j: 3 }, { i: '5', j: 3 }, { i: '1', j: 4 }, { i: '2', j: 4 }, { i: '3', j: 4 }, { i: '4', j: 4 }, { i: '5', j: 4 }, { i: '1', j: 5 }, { i: '2', j: 5 }, { i: '3', j: 5 }, { i: '4', j: 5 }, { i: '5', j: 5 }, { i: '1', j: 6 }, { i: '2', j: 6 }, { i: '3', j: 6 }, { i: '4', j: 6 }, { i: '5', j: 6 }, { i: '1', j: 7 }, { i: '2', j: 7 }, { i: '3', j: 7 }, { i: '4', j: 7 }, { i: '5', j: 7 }, { i: '1', j: 8 }, { i: '2', j: 8 }, { i: '3', j: 8 }, { i: '4', j: 8 }, { i: '5', j: 8 }, { i: '1', j: 9 }, { i: '2', j: 9 }, { i: '3', j: 9 }, { i: '4', j: 9 }, { i: '5', j: 9 }, { i: '1', j: 10 }, { i: '2', j: 10 }, { i: '3', j: 10 }, { i: '4', j: 10 }, { i: '5', j: 10 }, { i: '1', j: 11 }, { i: '2', j: 11 }, { i: '3', j: 11 }, { i: '4', j: 11 }, { i: '5', j: 11 } ], css_id: 'tinytable_css_ety3ak3wvwd0o3p1p52k',}, 
#>           { positions: [ { i: '0', j: 2 }, { i: '6', j: 2 }, { i: '0', j: 3 }, { i: '6', j: 3 }, { i: '0', j: 4 }, { i: '6', j: 4 }, { i: '0', j: 5 }, { i: '6', j: 5 }, { i: '0', j: 6 }, { i: '6', j: 6 }, { i: '0', j: 7 }, { i: '6', j: 7 }, { i: '0', j: 8 }, { i: '6', j: 8 }, { i: '0', j: 9 }, { i: '6', j: 9 }, { i: '0', j: 10 }, { i: '6', j: 10 }, { i: '0', j: 11 }, { i: '6', j: 11 } ], css_id: 'tinytable_css_vqjqpq0ieovugm0pmqpq',}, 
#>           { positions: [ { i: '-1', j: 2 }, { i: '-1', j: 3 }, { i: '-1', j: 4 }, { i: '-1', j: 5 }, { i: '-1', j: 6 }, { i: '-1', j: 7 }, { i: '-1', j: 8 }, { i: '-1', j: 9 } ], css_id: 'tinytable_css_v35347v2fidbt6yle68y',}, 
#>           { positions: [ { i: '6', j: 1 } ], css_id: 'tinytable_css_0ms2ifte3jsb7afua9zu',}, 
#>           { positions: [ { i: '2', j: 1 }, { i: '3', j: 1 }, { i: '5', j: 1 } ], css_id: 'tinytable_css_xmhz6pra61uv1026nzio',}, 
#>           { positions: [ { i: '1', j: 1 }, { i: '4', j: 1 } ], css_id: 'tinytable_css_y27323ceq2rb3p5lgyvr',}, 
#>           { positions: [ { i: '0', j: 1 } ], css_id: 'tinytable_css_6fk2i9xqdo8uilh5ubdl',}, 
#>           { positions: [ { i: '-1', j: 1 } ], css_id: 'tinytable_css_57bsgkyz8gy5kc5d3ws2',}, 
#>           ];
#> 
#>           // Loop over the arrays to style the cells
#>           cellsToStyle.forEach(function (group) {
#>               group.positions.forEach(function (cell) {
#>                   tableFns_kyw40u2q3g69uhqt5hsf.styleCell(cell.i, cell.j, group.css_id);
#>               });
#>           });
#>       });
#>     </script>
#> 
#>     <style>
#> 
#> /* ========================================================================
#>  * tinytable CSS - Core styling for HTML table output
#>  * ======================================================================== */
#> 
#> /* ========================================================================
#>  * CSS Custom Properties (Theme Variables)
#>  * ======================================================================== */
#> .tinytable {
#>     /* Colors */
#>     --tt-text-color: inherit;
#>     --tt-line-color: black;
#>     --tt-border-color: var(--tt-line-color);
#>     --tt-striped-bg: rgba(0, 0, 0, .05);
#>     --tt-hover-bg: rgba(0, 0, 0, .075);
#>     --tt-background-color: transparent;
#>     --tt-footnote-bg: var(--tt-background-color);
#> 
#>     /* Spacing */
#>     --tt-cell-padding: .5rem;
#>     --tt-cell-padding-sm: calc(var(--tt-cell-padding) / 2);
#>     --tt-table-margin: .5rem 0;
#>     --tt-caption-padding: .5rem 0;
#>     --tt-footnote-padding: .2rem .5rem;
#>     --tt-footnote-first-padding-top: .5rem;
#>     --tt-footnote-gap: .1rem;
#>     --tt-container-padding: 10px;
#> 
#>     /* Typography */
#>     --tt-font-size-footnote: 0.875rem;
#>     --tt-font-size-caption: 1rem;
#>     --tt-font-size-sup: 0.75em;
#>     --tt-line-height-footnote: 1.25;
#>     --tt-line-height-footnote-tight: 1.2;
#>     --tt-font-weight-header: normal;
#> 
#>     /* Alignment */
#>     --tt-cell-valign: top;
#>     --tt-header-align: left;
#>     --tt-header-valign: bottom;
#>     --tt-caption-align: var(--tt-header-align);
#>     --tt-caption-side: top;
#> 
#>     /* Layout */
#>     --tt-table-width: 100%;
#> 
#>     /* Border widths */
#>     --tt-border-width: 1px;
#>     --tt-border-width-tbody: 2px;
#> }
#> 
#> /* ========================================================================
#>  * Base Table Styles
#>  * ======================================================================== */
#> .tinytable {
#>     width: var(--tt-table-width);
#>     margin: var(--tt-table-margin);
#>     border-collapse: collapse;
#>     background-color: var(--tt-background-color);
#>     color: var(--tt-text-color);
#>     font-family: inherit;
#>     -webkit-font-smoothing: antialiased;
#>     -moz-osx-font-smoothing: grayscale;
#> }
#> 
#> .tinytable * {
#>     font-family: inherit;
#> }
#> 
#> /* ========================================================================
#>  * Cell Styles
#>  * ======================================================================== */
#> .tinytable th,
#> .tinytable td {
#>     position: relative;
#>     padding: var(--tt-cell-padding);
#>     vertical-align: var(--tt-cell-valign);
#> }
#> 
#> .tinytable thead th {
#>     text-align: var(--tt-header-align);
#>     vertical-align: var(--tt-header-valign);
#>     font-weight: var(--tt-font-weight-header);
#>     border-bottom: none;
#> }
#> 
#> .tinytable thead th[align="center"] {
#>     text-align: center;
#> }
#> 
#> .tinytable tbody + tbody {
#>     border-top: var(--tt-border-width-tbody) solid var(--tt-border-color);
#> }
#> 
#> /* ========================================================================
#>  * Table Variants
#>  * ======================================================================== */
#> 
#> /* Small padding variant */
#> .tinytable-sm th,
#> .tinytable-sm td {
#>     padding: var(--tt-cell-padding-sm);
#> }
#> 
#> /* Bordered variant */
#> .tinytable-bordered {
#>     border: var(--tt-border-width) solid var(--tt-border-color);
#> }
#> 
#> .tinytable-bordered th,
#> .tinytable-bordered td {
#>     border: var(--tt-border-width) solid var(--tt-border-color);
#> }
#> 
#> /* Striped rows variant */
#> .tinytable-striped tbody tr:nth-of-type(odd) {
#>     background-color: var(--tt-striped-bg);
#> }
#> 
#> /* Hover effect variant */
#> .tinytable-hover tbody tr:hover {
#>     background-color: var(--tt-hover-bg);
#> }
#> 
#> /* Responsive wrapper */
#> .tinytable-responsive {
#>     width: var(--tt-table-width);
#>     overflow-x: auto;
#>     -webkit-overflow-scrolling: touch;
#> }
#> 
#> /* ========================================================================
#>  * Custom Border System (Pseudo-elements)
#>  * ======================================================================== */
#> .tinytable :is(td, th)[class*="tinytable_css_"] {
#>     position: relative;
#> 
#>     /* Border control variables */
#>     --border-top: 0;
#>     --border-right: 0;
#>     --border-bottom: 0;
#>     --border-left: 0;
#> 
#>     /* Border color variables */
#>     --line-color-top: currentColor;
#>     --line-color-right: currentColor;
#>     --line-color-bottom: currentColor;
#>     --line-color-left: currentColor;
#> 
#>     /* Border width variables */
#>     --line-width-top: 0.1em;
#>     --line-width-right: 0.1em;
#>     --line-width-bottom: 0.1em;
#>     --line-width-left: 0.1em;
#> 
#>     /* Border trim variables */
#>     --trim-top-left: 0%;
#>     --trim-top-right: 0%;
#>     --trim-right-top: 0%;
#>     --trim-right-bottom: 0%;
#>     --trim-bottom-left: 0%;
#>     --trim-bottom-right: 0%;
#>     --trim-left-top: 0%;
#>     --trim-left-bottom: 0%;
#> }
#> 
#> /* Top, left, and right borders using ::before */
#> .tinytable :is(td, th)[class*="tinytable_css_"]::before {
#>     content: "";
#>     position: absolute;
#>     top: var(--trim-top-left, var(--trim-top-right, 0));
#>     left: var(--trim-left-top, 0);
#>     right: var(--trim-right-top, 0);
#>     bottom: 0;
#>     pointer-events: none;
#>     z-index: 1;
#>     border-top: calc(var(--border-top) * var(--line-width-top)) solid var(--line-color-top);
#>     border-right: calc(var(--border-right) * var(--line-width-right)) solid var(--line-color-right);
#>     border-left: calc(var(--border-left) * var(--line-width-left)) solid var(--line-color-left);
#> }
#> 
#> /* Bottom border using ::after */
#> .tinytable :is(td, th)[class*="tinytable_css_"]::after {
#>     content: "";
#>     position: absolute;
#>     left: var(--trim-bottom-left, 0);
#>     right: var(--trim-bottom-right, 0);
#>     bottom: var(--trim-left-bottom, var(--trim-right-bottom, 0));
#>     height: calc(var(--border-bottom) * var(--line-width-bottom));
#>     background: var(--line-color-bottom);
#>     pointer-events: none;
#>     z-index: 2;
#> }
#> 
#> /* ========================================================================
#>  * Container and Layout Components
#>  * ======================================================================== */
#> .tinytable-container {
#>     padding: var(--tt-container-padding);
#>     overflow-x: auto;
#>     overflow-y: auto;
#>     width: auto;
#>     height: auto;
#> }
#> 
#> /* ========================================================================
#>  * Caption Styling
#>  * ======================================================================== */
#> .tinytable caption {
#>     padding: var(--tt-caption-padding);
#>     caption-side: var(--tt-caption-side);
#>     font-size: var(--tt-font-size-caption);
#>     text-align: var(--tt-caption-align);
#> }
#> 
#> /* ========================================================================
#>  * Footnotes and Table Footer
#>  * ======================================================================== */
#> 
#> .tinytable tfoot tr:first-child td {
#>     padding-top: var(--tt-footnote-first-padding-top);
#> }
#> 
#> .tinytable tfoot td {
#>     padding: var(--tt-footnote-padding);
#>     font-size: var(--tt-font-size-footnote);
#>     line-height: var(--tt-line-height-footnote);
#>     background-color: var(--tt-footnote-bg);
#> }
#> 
#> /* Remove extra gap between footnote rows */
#> .tinytable tfoot tr + tr td {
#>     padding-top: var(--tt-footnote-gap);
#> }
#> 
#> /* Make superscripts not inflate line boxes */
#> .tinytable tfoot sup {
#>     font-size: var(--tt-font-size-sup);
#>     line-height: 0;
#>     vertical-align: text-top;
#> }
#> 
#> /* Slightly tighter for wrapped footnote text */
#> .tinytable tfoot tr:nth-child(n+2) td {
#>     line-height: var(--tt-line-height-footnote-tight);
#> }
#> 
#> </style>
#>     <style>
#>     /* tinytable css entries after */
#>     #tinytable_kyw40u2q3g69uhqt5hsf td.tinytable_css_xd9nhufjb3jz27aorm22, #tinytable_kyw40u2q3g69uhqt5hsf th.tinytable_css_xd9nhufjb3jz27aorm22 {  position: relative; --border-bottom: 0; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: var(--tt-line-color); --line-color-left: var(--tt-line-color); --line-color-right: var(--tt-line-color); --line-color-top: var(--tt-line-color); --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.06em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: right }
#>     #tinytable_kyw40u2q3g69uhqt5hsf td.tinytable_css_ety3ak3wvwd0o3p1p52k, #tinytable_kyw40u2q3g69uhqt5hsf th.tinytable_css_ety3ak3wvwd0o3p1p52k { text-align: right }
#>     #tinytable_kyw40u2q3g69uhqt5hsf td.tinytable_css_vqjqpq0ieovugm0pmqpq, #tinytable_kyw40u2q3g69uhqt5hsf th.tinytable_css_vqjqpq0ieovugm0pmqpq {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: var(--tt-line-color); --line-color-left: var(--tt-line-color); --line-color-right: var(--tt-line-color); --line-color-top: var(--tt-line-color); --line-width-bottom: 0.06em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: right }
#>     #tinytable_kyw40u2q3g69uhqt5hsf td.tinytable_css_v35347v2fidbt6yle68y, #tinytable_kyw40u2q3g69uhqt5hsf th.tinytable_css_v35347v2fidbt6yle68y {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: var(--tt-line-color); --line-color-left: var(--tt-line-color); --line-color-right: var(--tt-line-color); --line-color-top: var(--tt-line-color); --line-width-bottom: 0.06em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.06em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: right }
#>     #tinytable_kyw40u2q3g69uhqt5hsf td.tinytable_css_0ms2ifte3jsb7afua9zu, #tinytable_kyw40u2q3g69uhqt5hsf th.tinytable_css_0ms2ifte3jsb7afua9zu {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: var(--tt-line-color); --line-color-left: var(--tt-line-color); --line-color-right: var(--tt-line-color); --line-color-top: var(--tt-line-color); --line-width-bottom: 0.06em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left; padding-left: 1em; padding-left: 0.8em; }
#>     #tinytable_kyw40u2q3g69uhqt5hsf td.tinytable_css_xmhz6pra61uv1026nzio, #tinytable_kyw40u2q3g69uhqt5hsf th.tinytable_css_xmhz6pra61uv1026nzio { text-align: left; padding-left: 1em; padding-left: 0.8em; }
#>     #tinytable_kyw40u2q3g69uhqt5hsf td.tinytable_css_y27323ceq2rb3p5lgyvr, #tinytable_kyw40u2q3g69uhqt5hsf th.tinytable_css_y27323ceq2rb3p5lgyvr { text-align: left }
#>     #tinytable_kyw40u2q3g69uhqt5hsf td.tinytable_css_6fk2i9xqdo8uilh5ubdl, #tinytable_kyw40u2q3g69uhqt5hsf th.tinytable_css_6fk2i9xqdo8uilh5ubdl {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: var(--tt-line-color); --line-color-left: var(--tt-line-color); --line-color-right: var(--tt-line-color); --line-color-top: var(--tt-line-color); --line-width-bottom: 0.06em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left }
#>     #tinytable_kyw40u2q3g69uhqt5hsf td.tinytable_css_57bsgkyz8gy5kc5d3ws2, #tinytable_kyw40u2q3g69uhqt5hsf th.tinytable_css_57bsgkyz8gy5kc5d3ws2 {  position: relative; --border-bottom: 0; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: var(--tt-line-color); --line-color-left: var(--tt-line-color); --line-color-right: var(--tt-line-color); --line-color-top: var(--tt-line-color); --line-width-bottom: 0.1em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.06em; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; text-align: left }
#>     </style>
#>     <div class="container">
#>       <table class="tinytable" id="tinytable_kyw40u2q3g69uhqt5hsf" style="width: auto; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
#>         
#>         <thead>
#> <tr>
#> <th scope="col" align="center" colspan=1 data-row="-1" data-col="1">Variable</th>
#> <th scope="col" align="center" colspan=2 data-row="-1" data-col="2">BFH</th>
#> <th scope="col" align="center" colspan=2 data-row="-1" data-col="4">HEdS-Geneve</th>
#> <th scope="col" align="center" colspan=2 data-row="-1" data-col="6">HESAV</th>
#> <th scope="col" align="center" colspan=2 data-row="-1" data-col="8">Total</th>
#> <th scope="col" align="center" colspan=1 data-row="-1" data-col="10">p</th>
#> <th scope="col" align="center" colspan=1 data-row="-1" data-col="11">Cramer's V</th>
#> </tr>
#>               <tr>
#>                 <th scope="col" data-row="0" data-col="1"></th>
#>                 <th scope="col" data-row="0" data-col="2">n</th>
#>                 <th scope="col" data-row="0" data-col="3">%</th>
#>                 <th scope="col" data-row="0" data-col="4">n</th>
#>                 <th scope="col" data-row="0" data-col="5">%</th>
#>                 <th scope="col" data-row="0" data-col="6">n</th>
#>                 <th scope="col" data-row="0" data-col="7">%</th>
#>                 <th scope="col" data-row="0" data-col="8">n</th>
#>                 <th scope="col" data-row="0" data-col="9">%</th>
#>                 <th scope="col" data-row="0" data-col="10"></th>
#>                 <th scope="col" data-row="0" data-col="11"></th>
#>               </tr>
#>         </thead>
#>         
#>         <tbody>
#>                 <tr>
#>                   <td data-row="1" data-col="1">Emploi SF</td>
#>                   <td data-row="1" data-col="2"></td>
#>                   <td data-row="1" data-col="3"></td>
#>                   <td data-row="1" data-col="4"></td>
#>                   <td data-row="1" data-col="5"></td>
#>                   <td data-row="1" data-col="6"></td>
#>                   <td data-row="1" data-col="7"></td>
#>                   <td data-row="1" data-col="8"></td>
#>                   <td data-row="1" data-col="9"></td>
#>                   <td data-row="1" data-col="10">.001</td>
#>                   <td data-row="1" data-col="11">.62</td>
#>                 </tr>
#>                 <tr>
#>                   <td data-row="2" data-col="1">      Non</td>
#>                   <td data-row="2" data-col="2">12</td>
#>                   <td data-row="2" data-col="3">80.0</td>
#>                   <td data-row="2" data-col="4">2</td>
#>                   <td data-row="2" data-col="5">16.7</td>
#>                   <td data-row="2" data-col="6">4</td>
#>                   <td data-row="2" data-col="7">80.0</td>
#>                   <td data-row="2" data-col="8">18</td>
#>                   <td data-row="2" data-col="9">56.2</td>
#>                   <td data-row="2" data-col="10"></td>
#>                   <td data-row="2" data-col="11"></td>
#>                 </tr>
#>                 <tr>
#>                   <td data-row="3" data-col="1">      Oui</td>
#>                   <td data-row="3" data-col="2">3</td>
#>                   <td data-row="3" data-col="3">20.0</td>
#>                   <td data-row="3" data-col="4">10</td>
#>                   <td data-row="3" data-col="5">83.3</td>
#>                   <td data-row="3" data-col="6">1</td>
#>                   <td data-row="3" data-col="7">20.0</td>
#>                   <td data-row="3" data-col="8">14</td>
#>                   <td data-row="3" data-col="9">43.8</td>
#>                   <td data-row="3" data-col="10"></td>
#>                   <td data-row="3" data-col="11"></td>
#>                 </tr>
#>                 <tr>
#>                   <td data-row="4" data-col="1">Role recherche</td>
#>                   <td data-row="4" data-col="2"></td>
#>                   <td data-row="4" data-col="3"></td>
#>                   <td data-row="4" data-col="4"></td>
#>                   <td data-row="4" data-col="5"></td>
#>                   <td data-row="4" data-col="6"></td>
#>                   <td data-row="4" data-col="7"></td>
#>                   <td data-row="4" data-col="8"></td>
#>                   <td data-row="4" data-col="9"></td>
#>                   <td data-row="4" data-col="10">< .001</td>
#>                   <td data-row="4" data-col="11">.81</td>
#>                 </tr>
#>                 <tr>
#>                   <td data-row="5" data-col="1">      Non</td>
#>                   <td data-row="5" data-col="2">15</td>
#>                   <td data-row="5" data-col="3">100.0</td>
#>                   <td data-row="5" data-col="4">4</td>
#>                   <td data-row="5" data-col="5">33.3</td>
#>                   <td data-row="5" data-col="6">0</td>
#>                   <td data-row="5" data-col="7">0.0</td>
#>                   <td data-row="5" data-col="8">19</td>
#>                   <td data-row="5" data-col="9">59.4</td>
#>                   <td data-row="5" data-col="10"></td>
#>                   <td data-row="5" data-col="11"></td>
#>                 </tr>
#>                 <tr>
#>                   <td data-row="6" data-col="1">      Oui</td>
#>                   <td data-row="6" data-col="2">0</td>
#>                   <td data-row="6" data-col="3">0.0</td>
#>                   <td data-row="6" data-col="4">8</td>
#>                   <td data-row="6" data-col="5">66.7</td>
#>                   <td data-row="6" data-col="6">5</td>
#>                   <td data-row="6" data-col="7">100.0</td>
#>                   <td data-row="6" data-col="8">13</td>
#>                   <td data-row="6" data-col="9">40.6</td>
#>                   <td data-row="6" data-col="10"></td>
#>                   <td data-row="6" data-col="11"></td>
#>                 </tr>
#>         </tbody>
#>       </table>
#>     </div>
#>     <!-- postamble start -->
#>   </body>
#> 
#> </html>
#> <!-- postamble end -->
#> <!-- hack to avoid NA insertion in last line --> 

# Optional output: Excel
if (requireNamespace("openxlsx", quietly = TRUE)) {
  table_apa(
    data = d_ex,
    row_vars = c("emploi_sf", "role_prof_recherche"),
    group_var = "hes",
    labels = c("Emploi SF", "Role recherche"),
    output = "excel",
    excel_path = tempfile(fileext = ".xlsx")
  )
}
# }
```
