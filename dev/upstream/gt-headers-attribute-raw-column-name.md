# gt : l'attribut headers= des cellules n'est pas échappé

- **Cible** : gt 1.4.0 (CRAN) ; master 76f9ba1a (1.4.0.9000), fichier identique
- **Canal** : GitHub rstudio/gt, issue [#2185](https://github.com/rstudio/gt/issues/2185)
- **Envoyé** : 2026-11-13, issue #2185 (mise de côté le 12, postée le 13 avec le correctif testé)
- **Statut** : postée, réponse attendue. Correctif appliqué et testé le 2026-11-13 sur
  une copie du source (5 lignes, 1 fichier ; tests de gt : 4 instantanés à
  mettre à jour, aucune régression ; tableaux à noms ordinaires identiques
  octet pour octet). Remesuré le 2026-11-12 sur gt 1.4.0 : la 1.4.0
  (#2085, #2173) aligne `headers=` sur l'id pour les espaces ; une guillemet
  ou une référence de caractère dans le nom casse encore la correspondance
  (la guillemet tronque l'attribut). Chaque bloc de code a tourné tel quel
  dans une session R neuve ; chaque phrase vérifiée contre les journaux.
  Aucun doublon ; le plus proche est #2184 (id des en-têtes de groupe).
  Deuxième défaut consigné dans le même texte : l'id du stub des lignes de
  synthèse garde le nom de groupe brut (reste de #2085).
- **Côté spicy** : contourné par .gt_safe_ids() (R/flextable_helpers.R)

---

*Texte posté le 2026-11-13, titre puis corps.*

**Titre** : headers= on body cells is pasted without an attribute escape: a column name with a double quote breaks the cell markup

## Prework

* [x] Read and agree to the [code of conduct](https://www.contributor-covenant.org/version/2/1/code_of_conduct.html) and [contributing guidelines](https://github.com/rstudio/gt/blob/master/.github/CONTRIBUTING.md).
* [x] Checked the existing issues: #2085 (closed, fixed by #2173) is the space case of `headers=`, #2184 (open) the group heading id. Neither covers the missing escape.

## Description

gt 1.4.0 (CRAN), R 4.6.2, Windows 11. `R/utils_render_html.R` is identical on master (76f9ba1a, 1.4.0.9000).

The `headers=` attribute of body `<td>` cells is pasted without an attribute escape, while the `<th id>` it must match is escaped by htmltools. A column name with a double quote or a character reference gives a `headers` value that matches no id, and the quote truncates the attribute.

## Reproducible example

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

## Expected result

The `headers` value equals the `<th>` id it refers to: `headers="M-(Q&quot;x)"` and `headers="Q&amp;amp;A"` here, `<th id="summary_stub_plain-group_1">` in the summary case below.

## Cause

The heading ids go through `valid_html_id()` (L601) and then `htmltools::tags$th(id = ...)`, which escapes the attribute:

https://github.com/rstudio/gt/blob/76f9ba1a2ff62d51741555879037e05951b06e62/R/utils_render_html.R#L601
https://github.com/rstudio/gt/blob/76f9ba1a2ff62d51741555879037e05951b06e62/R/utils_render_html.R#L686-L699

The `headers` value goes through the same `valid_html_id()` (L1115-L1126 for the column part) but is pasted into the tag without an escape:

https://github.com/rstudio/gt/blob/76f9ba1a2ff62d51741555879037e05951b06e62/R/utils_render_html.R#L1115-L1126
https://github.com/rstudio/gt/blob/76f9ba1a2ff62d51741555879037e05951b06e62/R/utils_render_html.R#L1886-L1897

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

The same mismatch shows when the group name starts with a digit, because `valid_html_id()` prefixes `a` on the `headers` side only. The committed snapshot `tests/testthat/_snaps/summary_rows.md` has, for the group `1960s`, `<th id="summary_stub_1960s_1">` next to `headers="a1960s summary_stub_a1960s_1 population"`.

## Proposed fix

Escape the pasted values with `htmltools::htmlEscape(x, attribute = TRUE)`, as the same function already does for `colspan` and `style` (L2352, L2369), and build the summary stub id with `valid_html_id()`. Applied on 76f9ba1a:

```diff
diff --git a/R/utils_render_html.R b/R/utils_render_html.R
index 6192c4c7f..e551c1cdd 100644
--- a/R/utils_render_html.R
+++ b/R/utils_render_html.R
@@ -1893,7 +1893,7 @@ render_row_data <- function(
   base_attributes <- ifelse(
     has_stub_class,
     paste0("id=\"", row_id_i, "\" ", "scope=\"", scope, "\" "),
-    paste0("headers=\"", header, "\" ")
+    paste0("headers=\"", htmltools::htmlEscape(header, attribute = TRUE), "\" ")
   )
 
   row_span_attributes <- character(n)
@@ -2293,7 +2293,7 @@ summary_rows_for_group_h <- function(
               alignment_classes,
               extra_classes,
               row_styles,
-              valid_html_id(names(summary_df)),
+              htmltools::htmlEscape(valid_html_id(names(summary_df)), attribute = TRUE),
               FUN = function(x, col_span, alignment_class, extra_class, cell_style, col_name) {
                 extra_class <- c(extra_class, summary_row_class)
 
@@ -2321,7 +2321,7 @@ summary_rows_for_group_h <- function(
                       if (summary_row_type == "grand") {
                         paste0("grand_summary_stub_", j, "\" ")
                       } else {
-                        paste0("summary_stub_", group_id, "_", j, "\" ")
+                        paste0("summary_stub_", htmltools::htmlEscape(valid_html_id(group_id), attribute = TRUE), "_", j, "\" ")
                       },
                       "scope=\"row\""
                     )
@@ -2337,8 +2337,8 @@ summary_rows_for_group_h <- function(
                         )
                       } else {
                         paste0(
-                          valid_html_id(group_id), " summary_stub_",
-                          valid_html_id(group_id), "_", j, " ", col_name, "\""
+                          htmltools::htmlEscape(valid_html_id(group_id), attribute = TRUE), " summary_stub_",
+                          htmltools::htmlEscape(valid_html_id(group_id), attribute = TRUE), "_", j, " ", col_name, "\""
                         )
                       }
                     )
```

With this, the three reproducers give `headers="M-(Q&quot;x)"`, `headers="Q&amp;amp;A"` and `<th id="summary_stub_plain-group_1">`, and every parsed `headers` token matches an id. A name with `&`, `<` or `>` now gets the character reference in `headers=`, which parses as before; an apostrophe stays raw on both sides, since `render_as_html()` turns `&#39;` back into `'`. The checked tables without those characters (plain names and names with spaces, a stub, row groups with summary and grand summary rows, spanners with `cols_label()`) render byte for byte the same, except the summary stub id of a group whose name has a space or starts with a digit, which now matches its cells. Of gt's tests, 21 files (summary rows, stubs, spanners, row operations, cell locations, styles, the gt object, HTML rendering; 2,587 expectations) pass except four HTML snapshots holding the old values: `test-summary_rows.R:1877` (`summary_stub_1960s_1`), `test-summary_rows.R:1961` (`summary_stub_Aston Martin_1`) and `test-multicolumn_stub.R:370` and `:406` (`headers="American-&-German ..."`, which parses the same as the escaped form). I can send it as a PR with the snapshot updates.

## Session info

<details>
<summary>sessionInfo() at the end of the session that ran the three examples above</summary>

```
R version 4.6.2 (2026-10-29 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26300)

Matrix products: default
  LAPACK version 3.12.1

locale:
[1] LC_COLLATE=French_Switzerland.utf8  LC_CTYPE=French_Switzerland.utf8
[3] LC_MONETARY=French_Switzerland.utf8 LC_NUMERIC=C
[5] LC_TIME=French_Switzerland.utf8

time zone: Europe/Zurich
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
[1] gt_1.4.0

loaded via a namespace (and not attached):
 [1] digest_0.6.39    R6_2.6.1         fastmap_1.2.0    tidyselect_1.2.1
 [5] magrittr_2.0.5   glue_1.8.1       tibble_3.3.1     pkgconfig_2.0.3
 [9] htmltools_0.5.9  dplyr_1.2.1      generics_0.1.4   lifecycle_1.0.5
[13] xml2_1.6.0       cli_3.6.6        sass_0.4.10      vctrs_0.7.3
[17] withr_3.0.3      compiler_4.6.2   tools_4.6.2      pillar_1.11.1
[21] rlang_1.3.0      fs_2.1.0
```

</details>
