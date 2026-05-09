suppressMessages(devtools::load_all())
suppressMessages(library(effectsize))
suppressMessages(library(car))

cat("\n=== Test 1: numeric + factor + numeric covariate (Type II oracle) ===\n")
fit <- lm(mpg ~ wt + factor(cyl) + am, data = mtcars)

rows <- extract_partial_effect_rows(
  fit,
  ci_level = 0.95,
  show_columns = c("partial_f2", "partial_eta2", "partial_omega2"),
  model_id = "M1",
  outcome = "mpg"
)
cat("\n--- Spicy partial-effect rows ---\n")
print(rows[, c("term", "estimate_type", "estimate", "ci_low", "ci_high",
               "statistic", "df", "p_value")], row.names = FALSE)

# Type II oracle via car::Anova
aov2 <- car::Anova(fit, type = 2)
cat("\n--- car::Anova(fit, type=2) ---\n")
print(aov2)

cat("\n--- effectsize Type II ---\n")
print(es_eta <- effectsize::eta_squared(aov2, partial = TRUE, ci = 0.95,
                                        alternative = "two.sided"))
print(es_om  <- effectsize::omega_squared(aov2, partial = TRUE, ci = 0.95,
                                          alternative = "two.sided"))
print(es_f2  <- effectsize::cohens_f_squared(aov2, partial = TRUE, ci = 0.95,
                                             alternative = "two.sided"))

# Per-term comparison
spicy_eta_uniq <- unique(rows[rows$estimate_type == "partial_eta2",
                              c("estimate", "ci_low", "ci_high")])
spicy_om_uniq  <- unique(rows[rows$estimate_type == "partial_omega2",
                              c("estimate", "ci_low", "ci_high")])
spicy_f2_uniq  <- unique(rows[rows$estimate_type == "partial_f2",
                              c("estimate", "ci_low", "ci_high")])
es_eta_df <- as.data.frame(es_eta)
es_om_df  <- as.data.frame(es_om)
es_f2_df  <- as.data.frame(es_f2)

# Order spicy rows by terms in same order as effectsize
order_terms <- es_eta_df$Parameter
cat("\nOrder check — effectsize order:", paste(order_terms, collapse=", "), "\n")
# In spicy, terms are in coef order: wt, factor(cyl), am — matches anova rows.

cat("\n--- DELTA spicy − effectsize (Type II) ---\n")
delta <- function(a, b) max(abs(a - b), na.rm = TRUE)

cat(sprintf("  η²  : pt=%.3e  lo=%.3e  hi=%.3e\n",
            delta(spicy_eta_uniq$estimate, es_eta_df$Eta2_partial),
            delta(spicy_eta_uniq$ci_low,   es_eta_df$CI_low),
            delta(spicy_eta_uniq$ci_high,  es_eta_df$CI_high)))
cat(sprintf("  ω²  : pt=%.3e  lo=%.3e  hi=%.3e\n",
            delta(spicy_om_uniq$estimate, es_om_df$Omega2_partial),
            delta(spicy_om_uniq$ci_low,   es_om_df$CI_low),
            delta(spicy_om_uniq$ci_high,  es_om_df$CI_high)))
cat(sprintf("  f²  : pt=%.3e  lo=%.3e  hi=%.3e\n",
            delta(spicy_f2_uniq$estimate, es_f2_df$Cohens_f2_partial),
            delta(spicy_f2_uniq$ci_low,   es_f2_df$CI_low),
            delta(spicy_f2_uniq$ci_high,  es_f2_df$CI_high)))


cat("\n\n=== Test 2: factor (k>2) — joint test broadcast across dummies ===\n")
mt <- mtcars
mt$cyl <- factor(mt$cyl)
fit2 <- lm(mpg ~ cyl + wt, data = mt)
rows2 <- extract_partial_effect_rows(
  fit2, ci_level = 0.95,
  show_columns = c("partial_f2", "partial_eta2", "partial_omega2"),
  model_id = "M2", outcome = "mpg"
)
print(rows2[, c("term", "estimate_type", "estimate", "ci_low", "ci_high",
                "statistic", "df", "p_value")], row.names = FALSE)
cat("\nValidation: cyl6 vs cyl8 same value? ",
    isTRUE(all.equal(
      rows2[rows2$term == "cyl6" & rows2$estimate_type == "partial_eta2", "estimate"],
      rows2[rows2$term == "cyl8" & rows2$estimate_type == "partial_eta2", "estimate"]
    )), "\n")

aov2b <- car::Anova(fit2, type = 2)
es2 <- effectsize::eta_squared(aov2b, partial = TRUE, ci = 0.95,
                               alternative = "two.sided")
cat("\nDelta vs car::Anova type=2:",
    delta(unique(rows2[rows2$estimate_type == "partial_eta2", "estimate"]),
          as.data.frame(es2)$Eta2_partial), "\n")


cat("\n\n=== Test 3: interaction term ===\n")
fit3 <- lm(mpg ~ wt * factor(cyl), data = mtcars)
rows3 <- extract_partial_effect_rows(
  fit3, ci_level = 0.95,
  show_columns = c("partial_eta2"),
  model_id = "M3", outcome = "mpg"
)
print(rows3[, c("term", "estimate_type", "estimate", "ci_low", "ci_high",
                "statistic", "df", "p_value")], row.names = FALSE)
aov3 <- car::Anova(fit3, type = 2)
es3 <- effectsize::eta_squared(aov3, partial = TRUE, ci = 0.95,
                               alternative = "two.sided")
cat("\n--- car::Anova type 2 ---\n")
print(aov3)
cat("\n--- effectsize Type II ---\n")
print(es3)


cat("\n\n=== Test 4: integration via extract_lm_phase1 ===\n")
fit4 <- lm(mpg ~ wt + factor(cyl) + am, data = mtcars)
out <- extract_lm_phase1(
  fit4, model_id = "M4",
  show_columns = c("B", "SE", "CI", "p", "partial_f2", "partial_eta2",
                   "partial_omega2"),
  show_fit_stats = c("nobs", "r2", "adj_r2")
)
print(out$coefs[, c("term", "estimate_type", "estimate", "statistic", "df",
                    "p_value")], row.names = FALSE)


cat("\n\n=== Test 5: no partial token in show_columns => empty rows ===\n")
out5 <- extract_partial_effect_rows(
  fit, ci_level = 0.95,
  show_columns = c("B", "SE"),
  model_id = "M5", outcome = "mpg"
)
cat("nrow =", nrow(out5), " (expected 0)\n")


cat("\n\n=== Test 6: bivariate y ~ x sanity ===\n")
fit6 <- lm(mpg ~ wt, data = mtcars)
rows6 <- extract_partial_effect_rows(
  fit6, ci_level = 0.95,
  show_columns = c("partial_eta2", "partial_f2", "partial_omega2"),
  model_id = "M6", outcome = "mpg"
)
print(rows6[, c("term", "estimate_type", "estimate", "ci_low", "ci_high",
                "statistic", "df", "p_value")], row.names = FALSE)
sm6 <- summary(fit6)
cat("\nDirect: R² =", sm6$r.squared,
    "; f² =", sm6$r.squared/(1-sm6$r.squared),
    "; F =", sm6$fstatistic["value"],
    "; df1 =", sm6$fstatistic["numdf"],
    "; df2 =", sm6$fstatistic["dendf"], "\n")


cat("\n\n=== Test 7: subset partial tokens (only partial_eta2) ===\n")
out7 <- extract_partial_effect_rows(
  fit, ci_level = 0.95,
  show_columns = c("B", "partial_eta2"),
  model_id = "M7", outcome = "mpg"
)
print(out7[, c("term", "estimate_type", "estimate")], row.names = FALSE)
cat("nrow =", nrow(out7), " (expected 4: wt, factor(cyl)6, factor(cyl)8, am)\n")


cat("\n\nALL TESTS DONE\n")
