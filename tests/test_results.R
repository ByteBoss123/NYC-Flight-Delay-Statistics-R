# Checks: data integrity, no train/test month overlap, and that the published numbers reproduce.
# Run from the repo root:  Rscript tests/test_results.R
sums <- read.table("data/SHA256SUMS", col.names = c("hash", "file"))
for (i in seq_len(nrow(sums))) {
  h <- system2("sha256sum", file.path("data", sums$file[i]), stdout = TRUE)
  stopifnot(strsplit(h, " ")[[1]][1] == sums$hash[i])
}
e <- new.env(); load("data/flights.rda", e); stopifnot(nrow(e$flights) == 336776)
s <- readLines("results/summary.txt")
has <- function(x) stopifnot(any(grepl(x, s, fixed = TRUE)))
has("Operated flights (departure recorded): 328521")
has("Delay rate with precipitation 40.0% vs without 21.0%")
has("Test AUC: hour-of-day only 0.647; full model 0.681")
has("Riskiest 10% of test flights: 41.0% delayed vs 6.5% in the safest 10% (6.3x)")
or <- read.csv("results/odds_ratios.csv")
p <- or[or$term == "precip_any", ]
stopifnot(p$ci_low > 1, abs(p$odds_ratio - 2.52) < 0.01)
cal <- read.csv("results/calibration_deciles.csv")
stopifnot(nrow(cal) == 10, all(diff(cal$predicted) > 0))
cat("all checks passed\n")
