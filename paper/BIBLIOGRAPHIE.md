# Bibliographie de ce projet

`paper.bib` est un **fichier genere**. Ne pas l'editer a la main :
toute correction se fait dans la bibliotheque centrale, puis on regenere.

- Source de verite : `master.bib` du depot **references**
  (<https://github.com/amaltawfik/references>, prive), tag `projet-spicy`.
- Les PDF vivent dans le magasin central, hors de ce depot. `biblio-pdf/`
  est **genere et gitignore** (droits d'auteur) : il sert uniquement a
  transmettre le lot (Moodle, envoi aux co-auteurs).

## Regenerer -- a l'ouverture de chaque edition

```bash
cd ~/Documents/references
Rscript tools/make_project.R projet-spicy C:/Users/at/Documents/R/Packages/spicy/paper \
    --pdfs biblio-pdf
```

La commande ecrit le `.bib`, copie les PDF lies, et **echoue (statut 1)**
si une `@cle` citee dans ce depot n'est pas resolue. Verifier sans rien
ecrire :

```bash
Rscript tools/make_project.R --check-only C:/Users/at/Documents/R/Packages/spicy/paper
```

## Geler une edition

Committer le `paper.bib` regenere, puis poser un tag git **dans ce depot** :

```bash
git tag -a edition-AAAA -m "Edition AAAA"
```

C'est git qui porte la dimension temporelle : le tag bibliographique
`projet-spicy` n'est jamais date. `git show edition-AAAA:paper.bib` rend le
corpus exact de cette edition, et le depot recompile a l'identique.

## Declarer la bibliographie

```yaml
# Quarto / R Markdown
bibliography: paper.bib
```

```typst
#bibliography("paper.bib")
```
