# pandoc : le writer docx supprime en silence les blocs raw openxml portant un xmlns

- **Cible** : pandoc 3.1.9 à 3.10.1
- **Canal** : GitHub, jgm/pandoc#11772, https://github.com/jgm/pandoc/issues/11772
- **Envoyé** : 2026-07-23
- **Statut** : fermé « not planned » le 2026-07-24 ; contournement côté spicy à conserver
- **Côté spicy** : rendu Word / Quarto des tableaux (voir dev/quarto_word_rendering_spec.md)

---

*Dossier tel qu'envoyé, conservé verbatim ci-dessous.*

# POSTEE 2026-07-24: https://github.com/jgm/pandoc/issues/11772
# Dedup fait 2026-07-24 (4 recherches, aucun doublon). Tous les faits
# ci-dessous verifies empiriquement ce jour: silently = exit 0, aucun
# message meme en --verbose; reader OK (RawBlock present dans -t native);
# reproduit 3.1.9 / 3.8.3 (Quarto) / 3.10.1 (derniere). A poster sous le
# compte d'Amal apres son go.

**Title:** docx writer silently drops raw openxml blocks whose root element carries xmlns declarations

**Body:**

A raw `{=openxml}` block is dropped from the docx output when its root element carries an `xmlns:` declaration. The drop is silent: exit code 0, no message, nothing even with `--verbose`. The identical block without the declaration is included as a native Word table.

**works.md** (the docx contains a native table):

`````
```{=openxml}
<w:tbl><w:tblPr/><w:tblGrid><w:gridCol w:w="1000"/></w:tblGrid><w:tr><w:tc><w:tcPr/><w:p><w:r><w:t>X</w:t></w:r></w:p></w:tc></w:tr></w:tbl>
```
`````

**fails.md** (same table, one attribute added; the docx contains no table):

`````
```{=openxml}
<w:tbl xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:tblPr/><w:tblGrid><w:gridCol w:w="1000"/></w:tblGrid><w:tr><w:tc><w:tcPr/><w:p><w:r><w:t>X</w:t></w:r></w:p></w:tc></w:tr></w:tbl>
```
`````

Command and check:

```
pandoc works.md -o works.docx
pandoc fails.md -o fails.docx
```

then unzip each docx and search `word/document.xml` for `<w:tbl`: present for works.docx, absent for fails.docx.

The reader side is fine: `pandoc fails.md -t native` shows the full `RawBlock (Format "openxml")` with the declaration intact, so the block is lost in the docx writer.

Bisection: each of the six namespace declarations emitted by R's flextable package (`xmlns:w`, `xmlns:r`, `xmlns:w14`, `xmlns:wp`, `xmlns:a`, `xmlns:pic`) triggers the drop on its own.

Reproduced on pandoc 3.1.9, 3.8.3, and 3.10.1 (Windows 11).

Real-world impact: flextable prefixes its openxml table fragments with these six declarations, so R Markdown / Quarto to Word pipelines rendering flextable tables lose every table without any warning. We tracked this down from a downstream R package and now work around it by stripping the declarations from the root element before pandoc sees the block. The stripped documents remain namespace-valid: five of the six namespaces are declared on pandoc's own document root, and flextable re-declares `w14` locally on the runs that use it.

Expected: either include the raw block (the declarations are legal XML, at worst redundant), or emit a warning when a raw openxml block is discarded, so document authors can see the loss.
