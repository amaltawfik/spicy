## Generate .sps syntax files for the remaining 3 datasets

suppressPackageStartupMessages({ library(spicy) })

write_crosstab_sps <- function(tab, file, var_a = "a", var_b = "b") {
  df <- as.data.frame.table(tab, responseName = "freq")
  df[[1]] <- as.integer(df[[1]])
  df[[2]] <- as.integer(df[[2]])
  body <- apply(df, 1, function(r) sprintf("%s %s %s", r[1], r[2], r[3]))
  syntax <- c(
    "SET FORMAT=F12.7.",
    sprintf("DATA LIST FREE / %s %s freq.", var_a, var_b),
    "BEGIN DATA",
    body,
    "END DATA.",
    "WEIGHT BY freq.",
    sprintf("CROSSTABS /TABLES=%s BY %s", var_a, var_b),
    "  /STATISTICS=CHISQ PHI CC LAMBDA UC GAMMA D BTAU CTAU."
  )
  writeLines(syntax, file)
}

# 2x2: mtcars vs × am
data(mtcars)
tab1 <- table(factor(mtcars$vs), factor(mtcars$am))
write_crosstab_sps(tab1, "tools/validation/mtcars_2x2.sps", "vs", "am")

# 4x4: HairEyeColor marginalised over Sex
data(HairEyeColor)
tab2 <- margin.table(HairEyeColor, c(1, 2))
write_crosstab_sps(tab2, "tools/validation/HairEye.sps", "hair", "eye")

# sochealth: smoking × education
data(sochealth, package = "spicy")
tab3 <- table(sochealth$smoking, sochealth$education)
write_crosstab_sps(tab3, "tools/validation/sochealth.sps", "smoking", "education")

cat("3 .sps files written.\n")
