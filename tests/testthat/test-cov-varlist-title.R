# Coverage tests for R/varlist-title.R focusing on branches not
# exercised by test-varlist.R: zero-arg / literal-head calls, the
# transformed-source NULL fallback, non-symbol call heads, the
# anonymous-function call-name fallback, and the introspection
# loop's `next` skip for an unloaded namespace.

test_that("varlist_title() returns fallback for a zero-argument call", {
  # length(args) == 0L branch in varlist_expr_source_impl(): a call
  # with no arguments has no data frame to point at.
  expect_equal(varlist_title(quote(f())), "vl: <data>")
})

test_that("varlist_title() returns fallback when the call head is a literal", {
  # is.null(call_name) branch: a call whose head is not a symbol/call
  # (here an integer literal) yields no resolvable name.
  literal_head <- as.call(list(1L, as.symbol("df")))
  expect_equal(varlist_title(literal_head), "vl: <data>")
})

test_that("varlist_call_name() returns NULL for a non-symbol, non-call head", {
  # Directly exercises the !is.call(fun) early return.
  expect_null(varlist_call_name(1L))
  expect_null(varlist_call_name("not_a_call"))
})

test_that("varlist_title() returns fallback for a data-first verb on a literal", {
  # varlist_transformed_source() inner-NULL branch: head() is a
  # curated data-first verb, but its first argument is a literal whose
  # source cannot be resolved, so the whole expression has no name.
  expect_equal(varlist_title(quote(head(1L))), "vl: <data>")
})

test_that("varlist_title() resolves the data arg of an anonymous-function call", {
  # varlist_call_name() final fallback (parts[[1L]]): the call head is
  # itself a call -- the parenthesised anonymous function -- so its
  # name is "(", not in any special list. The single-argument path
  # then recurses into `df` and marks it transformed.
  expr <- quote((function(x) x)(df))
  expect_true(is.call(expr[[1L]]))
  expect_equal(varlist_call_name(expr[[1L]]), "(")
  expect_equal(varlist_title(expr), "vl: df*")
})

test_that("varlist_is_data_first() skips unloaded namespaces in the slow path", {
  # The introspection loop `next`s past any of dplyr/tidyr/tibble that
  # is not loaded. Force at least one to be unloaded by picking a
  # namespace that is genuinely absent; if all three happen to be
  # loaded we cannot observe the skip, so skip the test.
  pkgs <- c("dplyr", "tidyr", "tibble")
  loaded <- vapply(pkgs, isNamespaceLoaded, logical(1))
  skip_if(all(loaded), "all of dplyr/tidyr/tibble are loaded; cannot hit `next`")

  # A verb that is neither curated nor a real data-first function:
  # every loaded namespace introspects (and rejects) it, every
  # unloaded namespace is skipped via `next`, and the result is FALSE.
  expect_false("zzz_unknown_cov_verb" %in% varlist_data_first_calls())
  expect_false(varlist_is_data_first("zzz_unknown_cov_verb"))
})
