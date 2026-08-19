# The standardized-mean-difference kernels (R/smd.R).
#
# Every value below is pinned in full double precision and was produced
# by an INDEPENDENT oracle, named beside it with the command that made
# it. No suggested package is loaded here: the numbers are the contract,
# the oracles are the provenance.
#
#   tableone 0.13.2
#     t1 <- CreateTableOne(vars = c("x","bin","k3"), strata = "g",
#                          data = d, test = FALSE)
#     print(ExtractSmd(t1), digits = 22)
#   cobalt 4.6.3
#     col_w_smd(d["x"], treat = d$g, s.d.denom = "pooled", abs = FALSE)
#   survey 4.5 + tableone
#     svyCreateTableOne(vars = ..., strata = "g",
#                       data = svydesign(ids = ~1, weights = ~w, data = d))
#
# The pinned data is UNBALANCED BY CONSTRUCTION (n1 = 4, n2 = 3): Austin's
# denominator and the degrees-of-freedom pooled denominator of Cohen's d /
# Hedges' g coincide exactly at equal group sizes, so a balanced fixture
# would prove nothing about which one the column carries.

.smd_fixture <- function() {
  data.frame(
    g = factor(c("A", "A", "A", "A", "B", "B", "B"), levels = c("A", "B")),
    x = c(1, 2, 4, 5, 2, 3, 8),
    bin = factor(
      c("no", "no", "no", "yes", "yes", "no", "yes"),
      levels = c("no", "yes")
    ),
    k3 = factor(
      c("a", "a", "b", "c", "a", "b", "b"),
      levels = c("a", "b", "c")
    ),
    w = c(1, 2, 1, 3, 2, 1, 1),
    stringsAsFactors = FALSE
  )
}

test_that("the continuous kernel is Austin's, not Cohen's or Hedges'", {
  d <- .smd_fixture()
  x1 <- d$x[d$g == "A"]
  x2 <- d$x[d$g == "B"]
  got <- .smd_continuous(mean(x1), mean(x2), stats::var(x1), stats::var(x2))
  # tableone `ExtractSmd` -> 0.5100613704506704593200 (unsigned);
  # cobalt(s.d.denom = "pooled", abs = FALSE) -> +0.51006137045067045932,
  # i.e. B - A: cobalt guesses the SECOND level as "treated", spicy
  # subtracts in the order the table DISPLAYS the groups (A - B). The
  # magnitudes are the contract; do not "fix" the sign against cobalt.
  expect_equal(got, -0.51006137045067045932, tolerance = 1e-15)
  # Hand computation from the fractions, no `var()` anywhere.
  expect_identical(got, (3 - 13 / 3) / sqrt(((10 / 3) + (31 / 3)) / 2))
  # Negative controls: the two denominators the column must NOT carry.
  expect_false(isTRUE(all.equal(got, -0.5383819020581653846946))) # Cohen d
  expect_false(isTRUE(all.equal(got, -0.4533742333121392742434))) # Hedges g
})

test_that("the binary kernel takes the Bernoulli variance at n", {
  d <- .smd_fixture()
  p1 <- mean(d$bin[d$g == "A"] == "yes")
  p2 <- mean(d$bin[d$g == "B"] == "yes")
  got <- .smd_binary(p1, p2)
  # tableone `ExtractSmd` -> 0.9205746178983232885784. MECHANISM NOTE:
  # through `CreateTableOne` a two-level FACTOR does not reach
  # `StdDiff(binary = TRUE)` at all; it goes StdDiffMulti ->
  # LstMeansFromFullTable (`propTables[, -1]`) -> MultinomialVar, which
  # on a length-1 vector returns the scalar p(1-p). The two routes
  # coincide, so this oracle is produced by the MULTINOMIAL route --
  # searching tableone for `binary = TRUE` will not find it.
  expect_equal(got, -0.9205746178983232885784, tolerance = 1e-15)
  # Negative control: `var()` at n - 1 instead of p(1-p) at n, 19% off.
  b1 <- as.numeric(d$bin[d$g == "A"] == "yes")
  b2 <- as.numeric(d$bin[d$g == "B"] == "yes")
  wrong <- (p1 - p2) / sqrt((stats::var(b1) + stats::var(b2)) / 2)
  expect_equal(wrong, -0.7715167498104595900088, tolerance = 1e-15)
  expect_false(isTRUE(all.equal(got, wrong)))
})

test_that("the k-level kernel is Yang-Dalton and is quasi-invariant to the reference", {
  skip_if_not_installed("MASS")
  d <- .smd_fixture()
  p1 <- as.numeric(prop.table(table(d$k3[d$g == "A"])))
  p2 <- as.numeric(prop.table(table(d$k3[d$g == "B"])))
  got <- .smd_multinomial(p1, p2)
  # tableone `ExtractSmd` -> 1.1126972805283739109683, exact.
  expect_equal(got, 1.112697280528373910968, tolerance = 1e-15)
  # Not bounded by 1 -- which is why the column carries no
  # `value_range` and no APA leading-zero strip.
  expect_gt(got, 1)
  # Dropping the LAST level instead of the first is a different real by
  # ~1e-15, not the same one: the choice of reference is conventional
  # (tableone drops the first), and the tolerance covers the gap.
  rev_ref <- .smd_multinomial(rev(p1), rev(p2))
  expect_equal(rev_ref, 1.11269728052837302279, tolerance = 1e-15)
  expect_false(identical(rev_ref, got))
})

test_that("a declared-but-unobserved level makes S singular and ginv absorbs it", {
  skip_if_not_installed("MASS")
  d <- .smd_fixture()
  k3g <- factor(as.character(d$k3), levels = c("a", "b", "c", "z"))
  p1 <- as.numeric(prop.table(table(k3g[d$g == "A"])))
  p2 <- as.numeric(prop.table(table(k3g[d$g == "B"])))
  # Same value as without the phantom level, to the bit; tableone agrees.
  expect_identical(
    .smd_multinomial(p1, p2),
    .smd_multinomial(p1[-4L] / sum(p1[-4L]), p2[-4L] / sum(p2[-4L]))
  )
  # And `solve()` really does abort here -- the reason ginv is required.
  smat <- (.smd_multinomial_cov(p1)[-1L, -1L] +
    .smd_multinomial_cov(p2)[-1L, -1L]) /
    2
  expect_equal(det(smat), 0)
  expect_error(solve(smat))
})

test_that("levels empty in ONE group only do not singularise S", {
  skip_if_not_installed("MASS")
  # The COMMON case: group A spreads over three levels, group B sits on
  # one. det(S) = 0.0078125, `solve()` and `ginv()` agree, tableone
  # agrees: 2.449489742783177881336 (= sqrt(6)).
  p1 <- as.numeric(prop.table(table(factor(
    c("a", "b", "c", "c"),
    levels = c("a", "b", "c")
  ))))
  p2 <- as.numeric(prop.table(table(factor(
    c("a", "a", "a"),
    levels = c("a", "b", "c")
  ))))
  got <- .smd_multinomial(p1, p2)
  expect_equal(got, 2.449489742783177881336, tolerance = 1e-15)
  smat <- (.smd_multinomial_cov(p1)[-1L, -1L] +
    .smd_multinomial_cov(p2)[-1L, -1L]) /
    2
  expect_equal(det(smat), 0.0078125, tolerance = 1e-15)
  expect_equal(
    got,
    sqrt(drop(t((p1 - p2)[-1L]) %*% solve(smat) %*% (p1 - p2)[-1L])),
    tolerance = 1e-15
  )
})

test_that("a genuinely constant variable is perfectly balanced, not undefined", {
  skip_if_not_installed("MASS")
  expect_identical(.smd_multinomial(c(1, 0, 0), c(1, 0, 0)), 0)
  expect_identical(.smd_continuous(3, 3, 0, 0), 0)
  expect_identical(.smd_binary(1, 1), 0)
})

test_that("the consistency guard catches the two ginv blind spots", {
  skip_if_not_installed("MASS")
  # B2 -- each group constant on a DIFFERENT level. The bare ginv route
  # returns 0, i.e. "perfectly balanced" for the most imbalanced
  # variable there can be; tableone guards this one and returns NaN.
  b2 <- .smd_multinomial(c(1, 0, 0), c(0, 1, 0))
  expect_true(is.na(b2))
  expect_identical(.smd_undefined_reason(b2), "constant_levels")
  bare <- function(p1, p2) {
    tv <- (p1 - p2)[-1L]
    sm <- (.smd_multinomial_cov(p1)[-1L, -1L] +
      .smd_multinomial_cov(p2)[-1L, -1L]) /
      2
    sqrt(drop(t(tv) %*% MASS::ginv(sm) %*% tv))
  }
  expect_identical(bare(c(1, 0, 0), c(0, 1, 0)), 0)

  # B3 -- disjoint supports on k = 4. `all(S == 0)` is FALSE, so
  # tableone's guard never fires and it publishes the same finite
  # sqrt(2) the bare route does, where the true Mahalanobis distance is
  # infinite (ridge `solve(S + eI)` diverges as e^(-1/2), verified for
  # e = 1e-4 .. 1e-9: 70.7, 224, 707, 2236, 7071, 22360).
  p1 <- c(0.5, 0.5, 0, 0)
  p2 <- c(0, 0, 0.5, 0.5)
  b3 <- .smd_multinomial(p1, p2)
  expect_true(is.na(b3))
  expect_identical(.smd_undefined_reason(b3), "disjoint_support")
  expect_equal(bare(p1, p2), 1.414213562373095145475, tolerance = 1e-15)

  # And it fires on NOTHING else: every legitimate case above is
  # published, including the two singular ones.
  d <- .smd_fixture()
  legit <- list(
    list(c(0.5, 0.25, 0.25), c(1 / 3, 2 / 3, 0)),
    list(c(0.5, 0.25, 0.25, 0), c(1 / 3, 2 / 3, 0, 0)),
    list(c(0.25, 0.25, 0.5), c(1 / 3, 0, 2 / 3)),
    list(c(0.75, 0.25), c(1 / 3, 2 / 3)),
    list(c(1, 0, 0), c(1, 0, 0)),
    list(c(0.4, 0.3, 0.3, 0), c(0.5, 0.5, 0, 0))
  )
  for (pp in legit) {
    v <- .smd_multinomial(pp[[1L]], pp[[2L]])
    expect_null(.smd_undefined_reason(v))
    expect_false(is.na(v))
  }
})

test_that("a zero denominator with a non-zero difference is undefined, not zero", {
  z <- .smd_continuous(1, 2, 0, 0)
  expect_true(is.na(z))
  expect_identical(.smd_undefined_reason(z), "zero_denominator")
  zb <- .smd_binary(0, 1)
  expect_true(is.na(zb))
  expect_identical(.smd_undefined_reason(zb), "zero_denominator")
})

test_that("a group with no dispersion to estimate returns a SILENT NA", {
  # n = 1 (or weights summing to <= 1): `stats::sd()` / `.wtd_sd()`
  # return NA and the SMD follows, with no reason attached -- the
  # neighbouring SD cell already discloses the same fact, and a second
  # signal would double it.
  v <- .smd_continuous(3, 4, NA_real_, 2)
  expect_true(is.na(v))
  expect_null(.smd_undefined_reason(v))
  expect_null(.smd_undefined_reason(.smd_binary(NA_real_, 0.5)))
  expect_null(.smd_undefined_reason(.smd_multinomial(c(NA, NA), c(0.5, 0.5))))
})

test_that("the k = 2 multinomial kernel equals the binary kernel in magnitude", {
  skip_if_not_installed("MASS")
  # A free but strong algebraic invariant, and the reason the dispatch
  # is frozen: both kernels apply to a two-level factor and agree to the
  # BIT, but only `.smd_binary()` keeps the sign.
  d <- .smd_fixture()
  p1 <- as.numeric(prop.table(table(d$bin[d$g == "A"])))
  p2 <- as.numeric(prop.table(table(d$bin[d$g == "B"])))
  bin <- .smd_binary(p1[[2L]], p2[[2L]])
  multi <- .smd_multinomial(p1, p2)
  expect_identical(multi, abs(bin))
  expect_identical(multi - abs(bin), 0)
  expect_lt(bin, 0)
  expect_gt(multi, 0)
  # ... and the dispatch really does send two levels to the signed one.
  expect_identical(.smd_categorical_type(2L), "binary")
  expect_identical(.smd_categorical_type(3L), "multinomial")
  expect_identical(.smd_pair_dispatch(p1[[2L]], p2[[2L]], "binary"), bin)
  expect_identical(.smd_pair_dispatch(p1, p2, "multinomial"), multi)
  expect_identical(
    .smd_pair_dispatch(c(3, 10 / 3), c(13 / 3, 31 / 3), "continuous"),
    .smd_continuous(3, 13 / 3, 10 / 3, 31 / 3)
  )
})

test_that("integer weights are duplicated rows, on all three kernels", {
  skip_if_not_installed("MASS")
  # The acceptance property of decision 17, extended to the SMD for
  # free: a frequency weight IS a number of copies, so every kernel must
  # return the SMD of the expanded data. Exactly, not nearly.
  d <- .smd_fixture()
  dup <- d[rep(seq_len(nrow(d)), d$w), , drop = FALSE]

  wm <- .smd_moments_base(d$x[d$g == "A"], d$w[d$g == "A"])
  wm2 <- .smd_moments_base(d$x[d$g == "B"], d$w[d$g == "B"])
  dm <- .smd_moments_base(dup$x[dup$g == "A"])
  dm2 <- .smd_moments_base(dup$x[dup$g == "B"])
  expect_identical(
    .smd_continuous(wm[[1L]], wm2[[1L]], wm[[2L]], wm2[[2L]]),
    .smd_continuous(dm[[1L]], dm2[[1L]], dm[[2L]], dm2[[2L]])
  )
  # tableone svyCreateTableOne -> 0.1277796056598360652234 for the
  # survey::svyvar denominator: a DIFFERENT convention, kept out on
  # purpose (see the weights section of ?table_continuous).
  expect_equal(
    .smd_continuous(wm[[1L]], wm2[[1L]], wm[[2L]], wm2[[2L]]),
    -0.1358139271300619344007,
    tolerance = 1e-15
  )
  expect_equal(wm[[1L]], 3.428571428571428381105, tolerance = 1e-15)
  expect_equal(wm[[2L]], 2.952380952380952550129, tolerance = 1e-15)

  lv <- c("no", "yes")
  wp <- .smd_props_base(d$bin[d$g == "A"], lv, d$w[d$g == "A"])
  wp2 <- .smd_props_base(d$bin[d$g == "B"], lv, d$w[d$g == "B"])
  expect_identical(
    .smd_binary(wp[[2L]], wp2[[2L]]),
    .smd_binary(
      .smd_props_base(dup$bin[dup$g == "A"], lv)[[2L]],
      .smd_props_base(dup$bin[dup$g == "B"], lv)[[2L]]
    )
  )
  # tableone on the survey design -> 0.6912858353783117859592: the
  # categorical arms do NOT diverge between conventions, the Bernoulli
  # and multinomial variances being functions of the weighted
  # proportion alone.
  expect_equal(
    .smd_binary(wp[[2L]], wp2[[2L]]),
    -0.6912858353783118969815,
    tolerance = 1e-15
  )

  lv3 <- c("a", "b", "c")
  wk <- .smd_props_base(d$k3[d$g == "A"], lv3, d$w[d$g == "A"])
  wk2 <- .smd_props_base(d$k3[d$g == "B"], lv3, d$w[d$g == "B"])
  expect_identical(
    .smd_multinomial(wk, wk2),
    .smd_multinomial(
      .smd_props_base(dup$k3[dup$g == "A"], lv3),
      .smd_props_base(dup$k3[dup$g == "B"], lv3)
    )
  )
  # tableone on the survey design -> 1.3601470508735444830961.
  expect_equal(
    .smd_multinomial(wk, wk2),
    1.360147050873544483096,
    tolerance = 1e-15
  )
})

test_that("the weighted SMD is not scale-invariant in the weights, by design", {
  # Under the frequency reading a weight is a NUMBER OF COPIES, so
  # multiplying every weight by ten changes the sample size, the SD the
  # table prints, and the SMD with it. `rescale = TRUE` (weights summing
  # to n) restores scale invariance. Both are documented, neither is a
  # bug.
  d <- .smd_fixture()
  smd_of <- function(w) {
    a <- .smd_moments_base(d$x[d$g == "A"], w[d$g == "A"])
    b <- .smd_moments_base(d$x[d$g == "B"], w[d$g == "B"])
    .smd_continuous(a[[1L]], b[[1L]], a[[2L]], b[[2L]])
  }
  expect_equal(smd_of(d$w * 10), -0.1522568229837656661463, tolerance = 1e-15)
  expect_equal(
    smd_of(.prep_variable_weights(d$x, d$w, rescale = TRUE)),
    -0.1239276937407762374521,
    # 1e-12, not tighter: the rescale product w*n/W accumulates one
    # ULP apart on the macOS libm (CI measured ...64 vs ...62).
    tolerance = 1e-12
  )
})

test_that("the base adapters read the same producers the table displays", {
  d <- .smd_fixture()
  # Unweighted: `mean()` / `stats::sd()`, the two lines of
  # `compute_one()`'s unweighted branch.
  expect_identical(
    .smd_moments_base(d$x),
    c(mean(d$x), stats::sd(d$x)^2)
  )
  # Weights of one collapse to the unweighted moments -- to one ULP of
  # the variance, not byte for byte: `stats::sd()` and `.wtd_sd()` sum
  # in a different order, and the SD COLUMN of the table already carries
  # that same difference (`compute_one()` branches on `w` exactly here).
  expect_equal(
    .smd_moments_base(d$x, rep(1, nrow(d))),
    .smd_moments_base(d$x),
    tolerance = 1e-15
  )
  # A group of one has no variance to standardise by.
  expect_identical(.smd_moments_base(1), c(1, NA_real_))
  expect_identical(.smd_moments_base(numeric(0)), c(NA_real_, NA_real_))
  expect_identical(
    .smd_moments_base(c(1, 2), c(0.25, 0.25)),
    c(1.5, NA_real_)
  )
  # Proportions: `prop.table(table())` unweighted, summed weights under
  # weights, and a declared-but-unobserved level is an explicit zero.
  expect_identical(
    .smd_props_base(d$k3, c("a", "b", "c")),
    as.numeric(prop.table(table(d$k3)))
  )
  expect_identical(
    .smd_props_base(d$k3, c("a", "b", "c", "z")),
    c(as.numeric(prop.table(table(d$k3))), 0)
  )
  expect_identical(
    .smd_props_base(d$k3, c("a", "b", "c"), rep(2, nrow(d))),
    .smd_props_base(d$k3, c("a", "b", "c"))
  )
  expect_identical(
    .smd_props_base(character(0), c("a", "b")),
    c(NA_real_, NA_real_)
  )
})


test_that("a variable with fewer than two categories does not reach ginv", {
  # `MASS::ginv()` aborts on a zero-dimension matrix, and dropping the
  # reference level of a one-category profile leaves exactly that.
  # `table_categorical()` tabulates a single-level factor as a real
  # one-row block, so this is reachable from the public surface, not a
  # theoretical edge.
  expect_error(MASS::ginv(matrix(0, 0, 0)))
  expect_identical(.smd_multinomial(1, 1), 0)
  expect_identical(.smd_pair_dispatch(1, 1, "multinomial"), 0)
  expect_true(is.na(.smd_multinomial(numeric(0), numeric(0))))
  expect_null(.smd_undefined_reason(.smd_multinomial(1, 1)))
})
