# gt: the `headers=` attribute is built from the raw column name

Dossier for an upstream report to `rstudio/gt`. Measured on gt 1.3.0,
R 4.6.1, Windows. Register n. 73; spicy answered it locally with
`.gt_safe_ids()` (`R/flextable_helpers.R`), which keeps the public
column name and hands gt a sanitised id.

## The reproducer (no spicy)

```r
library(gt)
df <- data.frame(a = 1, b = 2)
names(df) <- c("M (Q\"x)", "plain name")
html <- as.character(gt::as_raw_html(gt::gt(df)))
cat(paste(unlist(regmatches(html, gregexpr("id=\"[^\"]*\"", html))), collapse = "\n"))
cat(grep("headers=", strsplit(html, "\n")[[1L]], value = TRUE))
```

## What comes out

The `<th>` ids (gt escapes them, and runs them through
`gt:::valid_html_id()` first -- whitespace collapses to `-`):

```
id="M-(Q&quot;x)"
id="plain-name"
```

The `headers=` attributes gt writes on the body cells, from the same
columns:

```
<td headers="M (Q" x)" class="gt_row gt_right" ...>1</td>
<td headers="plain name" class="gt_row gt_right" ...>2</td>
```

## Two defects, one cause

The column name reaches `headers=` raw. Neither
`valid_html_id()` nor an attribute escape is applied on that path.

1. **Malformed markup.** The double quote closes the attribute early.
   Everything after it is re-parsed as bare attribute names -- which is
   why the round trip above prints `headers="M (Q" x)`: `x)"` became an
   attribute of its own. A `<script>` in a column name is NOT live
   (an attribute value is not a text node), so this is a markup /
   validity defect, not an injection one.

2. **The association is broken for ordinary names too.** `headers`
   is supposed to name the `id` of the header cell. `id="plain-name"`
   against `headers="plain name"` never matches, so the accessibility
   contract `headers=` exists for is lost on every column whose name
   contains a space. That is the majority of real tables.

## Suggested fix upstream

Apply the same `valid_html_id()` mapping to the `headers=` value that
the `<th id=>` already gets, and escape it for an attribute value.
That repairs both: the ids match again, and no name can break out of
the attribute.

## What spicy does meanwhile

`.gt_safe_ids()` replaces `"`, `<`, `>`, `\` and the control characters
in the column names handed to `gt::gt()`, and is the identity on every
other name. Defect 1 is closed; defect 2 is untouched (it needs gt).
The public column name -- what `output = "data.frame"` publishes and
what `col_meta` is indexed by -- does not move, and neither does the
visible header, which gt escapes correctly on the label path.
