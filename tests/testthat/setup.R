# The suite's baseline is the English default. A language now brings
# its locale typography (decimal mark, p leading zero), so a
# developer's ambient options(spicy.language) -- the very gesture the
# package promotes, and the one its francophone maintainer is likely
# to have set -- would turn dozens of English-pinning tests red.
# Neutralised once here; blocks that test a language, a style or a
# label override set their own, scoped with withr::local_options().
options(spicy.language = NULL, spicy.style = NULL, spicy.labels = NULL)

# The ASCII renderer splits panels on getOption("width"), and a handful
# of witnesses address rendered lines by position. Rscript runs at 80;
# an IDE console follows its pane, so devtools::test() from a console
# re-wrapped the corpus and failed layout pins spuriously. Same
# baseline for every session.
options(width = 80L)
