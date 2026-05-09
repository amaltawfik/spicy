suppressMessages(devtools::load_all())

div <- function(name) cat("\n", strrep("=", 70), "\n", name, "\n",
                          strrep("=", 70), "\n", sep = "")

# 1. Logistic + exponentiate
div("1. Logistic + exponentiate = TRUE — OR header + delta-method SE")
fit <- glm(am ~ mpg + wt, data = mtcars, family = binomial)
out <- table_regression(fit, exponentiate = TRUE)
print(out)

# Numeric checks
td_raw <- broom::tidy(table_regression(fit))
td_exp <- broom::tidy(table_regression(fit, exponentiate = TRUE))
cat("\nArithmetic identity (B vs OR):\n")
print(data.frame(
  term = td_raw$term[td_raw$estimate_type == "B"],
  B = td_raw$estimate[td_raw$estimate_type == "B"],
  exp_B = exp(td_raw$estimate[td_raw$estimate_type == "B"]),
  spicy_OR = td_exp$estimate[td_exp$estimate_type == "B"]
))
cat("\nSE delta-method check:\n")
print(data.frame(
  term = td_raw$term[td_raw$estimate_type == "B"],
  SE_link = td_raw$std.error[td_raw$estimate_type == "B"],
  expected_SE_OR = exp(td_raw$estimate[td_raw$estimate_type == "B"]) *
                     td_raw$std.error[td_raw$estimate_type == "B"],
  spicy_SE_OR = td_exp$std.error[td_exp$estimate_type == "B"]
))

# 2. Poisson + exponentiate (IRR header)
div("2. Poisson + exponentiate = TRUE — IRR header")
fit_p <- glm(I(round(mpg)) ~ wt + cyl, data = mtcars, family = poisson)
out_p <- table_regression(fit_p, exponentiate = TRUE)
print(out_p)

# 3. lm + exponentiate = TRUE — should warn
div("3. lm + exponentiate = TRUE — should emit spicy_ignored_arg")
fit_lm <- lm(mpg ~ wt, data = mtcars)
withCallingHandlers(
  out_lm <- table_regression(fit_lm, exponentiate = TRUE),
  spicy_ignored_arg = function(c) {
    cat("[WARN] ", conditionMessage(c), "\n", sep = "")
    invokeRestart("muffleWarning")
  }
)

# 4. Gaussian glm + exponentiate — also no-op + warn
div("4. Gaussian glm + exponentiate — no-op + warn")
fit_gauss <- glm(mpg ~ wt, data = mtcars, family = gaussian)
withCallingHandlers(
  out_g <- table_regression(fit_gauss, exponentiate = TRUE),
  spicy_ignored_arg = function(c) {
    cat("[WARN] ", conditionMessage(c), "\n", sep = "")
    invokeRestart("muffleWarning")
  },
  spicy_caveat = function(c) {
    cat("[CAVEAT] ", conditionMessage(c), "\n", sep = "")
    invokeRestart("muffleWarning")
  }
)

# 5. Probit + exponentiate — generic exp(B) header (not OR)
div("5. Probit + exponentiate = TRUE — generic exp(B)")
fit_pr <- glm(am ~ mpg, data = mtcars, family = binomial(link = "probit"))
out_pr <- table_regression(fit_pr, exponentiate = TRUE)
print(out_pr)

# 6. Multi-model: logit + lm — different headers per model
div("6. Multi-model: logit + lm — per-model header (OR + B)")
fit_l <- glm(am ~ mpg, data = mtcars, family = binomial)
fit_n <- lm(mpg ~ wt, data = mtcars)
withCallingHandlers(
  out_m <- table_regression(list(fit_l, fit_n), exponentiate = TRUE),
  spicy_warning = function(c) invokeRestart("muffleWarning")
)
print(out_m)

cat("\n\nSTEP 2 SMOKE DONE.\n")
