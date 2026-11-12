# gt : l'attribut headers= des cellules n'est pas échappé

- **Cible** : gt 1.4.0 (CRAN) ; master 76f9ba1a (1.4.0.9000), fichier identique
- **Canal** : GitHub rstudio/gt, issue à ouvrir
- **Envoyé** : non envoyé ; mis de côté le 2026-11-12
- **Statut** : dossier prêt. Remesuré le 2026-11-12 sur gt 1.4.0 : la 1.4.0
  (#2085, #2173) aligne `headers=` sur l'id pour les espaces ; une guillemet
  ou une référence de caractère dans le nom casse encore la correspondance
  (la guillemet tronque l'attribut). Chaque bloc de code a tourné tel quel
  dans une session R neuve ; chaque phrase vérifiée contre les journaux.
  Aucun doublon ; le plus proche est #2184 (id des en-têtes de groupe).
  Deuxième défaut consigné dans le même texte : l'id du stub des lignes de
  synthèse garde le nom de groupe brut (reste de #2085).
- **Côté spicy** : contourné par .gt_safe_ids() (R/flextable_helpers.R)

---

*Texte prêt à poster, titre puis corps.*

**Titre** : `headers=` on body cells is pasted without an attribute escape: a column name with a double quote breaks the cell markup

gt 1.4.0 (CRAN), R 4.6.2, Windows 11. `R/utils_render_html.R` is identical on master (76f9ba1a, 1.4.0.9000).

## Reproducer

```r
library(gt)
df <- data.frame(a = 1, b = 2)
names(df) <- c("M (Q\"x)", "plain name")
html <- as.character(as_raw_html(gt(df), inline_css = FALSE))
cat(unlist(regmatches(html, gregexpr("<t[hd] [^>]*>", html))), sep = "\n")
```

```
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="M-(Q&quot;x)">
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="plain-name">
<td headers="M-(Q"x)" class="gt_row gt_right">
<td headers="plain-name" class="gt_row gt_right">
```

The `<th>` id is `M-(Q&quot;x)`: `valid_html_id()` maps the space to a hyphen and `htmltools::tags$th()` escapes the attribute. The `<td headers>` value gets the same hyphen but no escape, so the quote closes the attribute after `M-(Q`. The WHATWG tokenizer that browsers implement (checked with parse5 7.3.0) reads `headers="M-(Q"` plus a stray attribute named `x)"`; libxml2 reads a stray attribute `x`:

```r
xml2::xml_attrs(xml2::xml_find_first(xml2::read_html(html), "//td"))
```

```
          headers                 x             class 
           "M-(Q"                "" "gt_row gt_right" 
```

The cell's only `headers` token, `M-(Q`, points to no element.

The id and `headers` also diverge without a quote, when the name contains a character reference (no stray attribute in that case):

```r
df2 <- data.frame(x = 1)
names(df2) <- "Q&amp;A"
html2 <- as.character(as_raw_html(gt(df2), inline_css = FALSE))
cat(unlist(regmatches(html2, gregexpr("<t[hd] [^>]*>", html2))), sep = "\n")
```

```
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Q&amp;amp;A">
<td headers="Q&amp;A" class="gt_row gt_right">
```

Parsed, the id is `Q&amp;A` and the headers value is `Q&A`: no match.

`<`, `>` and `&` alone are also written raw in `headers=` (`headers="a<b"` against `id="a&lt;b"`); parsers recover and both read `a<b`, so those three show no visible defect.

## Cause

The heading ids go through `valid_html_id()` (L601) and then `htmltools::tags$th(id = ...)`, which escapes the attribute:

https://github.com/rstudio/gt/blob/76f9ba1a2ff62d51741555879037e05951b06e62/R/utils_render_html.R#L601
https://github.com/rstudio/gt/blob/76f9ba1a2ff62d51741555879037e05951b06e62/R/utils_render_html.R#L686-L699

The `headers` value goes through the same `valid_html_id()` (L1115-L1126 for the column part) but is pasted into the tag without an escape:

https://github.com/rstudio/gt/blob/76f9ba1a2ff62d51741555879037e05951b06e62/R/utils_render_html.R#L1115-L1126
https://github.com/rstudio/gt/blob/76f9ba1a2ff62d51741555879037e05951b06e62/R/utils_render_html.R#L1886-L1897

`htmltools::htmlEscape(header, attribute = TRUE)` at that point turns these two values into `M-(Q&quot;x)` and `Q&amp;amp;A`, the ids above.

## Related: summary row stub ids keep the raw group name

`summary_rows()` writes the stub `<th>` id with the raw `group_id` (no `valid_html_id()`, no escape), while the summary cells reference `valid_html_id(group_id)`:

https://github.com/rstudio/gt/blob/76f9ba1a2ff62d51741555879037e05951b06e62/R/utils_render_html.R#L2314-L2343

```r
df3 <- data.frame(g = "plain group", v = c(1, 2))
names(df3)[2] <- "plain name"
html3 <- as.character(as_raw_html(
  summary_rows(gt(df3, groupname_col = "g"), columns = "plain name", fns = list(total = ~ sum(.))),
  inline_css = FALSE
))
cat(grep("summary_stub", unlist(regmatches(html3, gregexpr("<t[hd] [^>]*>", html3))), value = TRUE), sep = "\n")
```

```
<th id="summary_stub_plain group_1" scope="row" class="gt_row gt_left gt_stub gt_summary_row gt_first_summary_row thick gt_last_summary_row">
<td headers="plain-group summary_stub_plain-group_1 plain-name" class="gt_row gt_right gt_summary_row gt_first_summary_row thick gt_last_summary_row">
```

`summary_stub_plain-group_1` matches no id, and the id itself contains a space. This is the space case of #2085 left over in the summary rows: #2173 hyphenated the `headers` side, summary cells included, but not this id. #2184 (open) covers the group heading id built from the label.
