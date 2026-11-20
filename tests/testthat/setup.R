# The suite's baseline is the English default. A language now brings
# its locale typography (decimal mark, p leading zero), so a
# developer's ambient options(spicy.language) -- the very gesture the
# package promotes, and the one its francophone maintainer is likely
# to have set -- would turn dozens of English-pinning tests red. The
# same holds for options(spicy.var_labels), which moves every variable
# name of the descriptive tables.
# Neutralized once here; blocks that test a language, a style or a
# label override set their own, scoped with withr::local_options().
options(
  spicy.language = NULL,
  spicy.style = NULL,
  spicy.labels = NULL,
  spicy.var_labels = NULL
)

# The ASCII renderer splits panels on getOption("width"), and a handful
# of witnesses address rendered lines by position. Rscript runs at 80;
# an IDE console follows its pane, so devtools::test() from a console
# re-wrapped the corpus and failed layout pins spuriously. Same
# baseline for every session.
options(width = 80L)

# The once-per-session hint of a plain lme4::lmer() fit fires on the
# first such table of a session, so the order of the files would decide
# which snapshot or expect_silent() block met it. Spent here; the block
# that tests it resets it.
suppressMessages(inform_lmertest_hint())

# fixest takes half of the OpenMP threads it finds when its namespace
# loads, and warns on every call once another package has lowered that
# ceiling. Whether a session is in that case depends on the machine and
# on the order of the files: on the Linux coverage runner every fixest
# call warned ("Asked for 2 threads while the maximum is 1"), and the
# blocks that expect no warning failed there and nowhere else. One
# thread, set when the namespace loads, gives the same tests everywhere.
local({
  one_thread <- function(...) {
    try(fixest::setFixest_nthreads(1), silent = TRUE)
  }
  if (isNamespaceLoaded("fixest")) {
    one_thread()
  } else {
    setHook(packageEvent("fixest", "onLoad"), one_thread)
  }
})
