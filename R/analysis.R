# What drives departure delays out of New York, and how well can a simple model rank flights by delay risk?
# Weather is the observed hourly weather at the origin for the scheduled hour (a stand-in for a forecast).
# Data: nycflights13 (all 336,776 flights from JFK, LGA and EWR in 2013, with hourly airport weather).
# Base R only. Run from the repo root:  Rscript R/analysis.R
# Outputs: results/summary.txt, results/*.csv, results/*.png

set.seed(0)
load_rda <- function(name) { e <- new.env(); load(file.path("data", paste0(name, ".rda")), e); as.data.frame(get(name, e)) }
flights <- load_rda("flights"); weather <- load_rda("weather"); airlines <- load_rda("airlines")
dir.create("results", showWarnings = FALSE)
out <- character()
fp <- function(p) if (p < 1e-300) "< 1e-300" else sprintf("= %.2e", p)
say <- function(...) { line <- sprintf(...); out <<- c(out, line); cat(line, "\n") }

# ---- 1. Clean: cancelled flights (no departure time) are excluded; delayed = departed 15+ minutes late (FAA definition)
say("Flights in 2013: %d", nrow(flights))
f <- flights[!is.na(flights$dep_delay), ]
say("Operated flights (departure recorded): %d; cancelled or missing: %d", nrow(f), nrow(flights) - nrow(f))
f$delayed <- as.integer(f$dep_delay >= 15)
say("Departure delay rate (15+ min): %.1f%%", 100 * mean(f$delayed))

# join hourly weather at the origin airport for the scheduled departure hour
w <- weather[, c("origin", "year", "month", "day", "hour", "temp", "wind_speed", "precip", "visib")]
w <- w[!duplicated(w[, c("origin", "year", "month", "day", "hour")]), ]
f <- merge(f, w, by = c("origin", "year", "month", "day", "hour"), all.x = TRUE)
say("Flights matched to an hourly weather record: %d (%.1f%%)", sum(!is.na(f$visib)), 100 * mean(!is.na(f$visib)))
f <- f[!is.na(f$visib) & !is.na(f$wind_speed) & !is.na(f$temp), ]
f$precip_any <- as.integer(f$precip > 0)
f$low_vis <- as.integer(f$visib < 3)
f$hour_f <- factor(pmin(pmax(f$hour, 5), 22))          # 5am..10pm buckets
f$month_f <- factor(f$month)
f$origin <- factor(f$origin)
top <- names(sort(table(f$carrier), decreasing = TRUE))[1:8]
f$carrier_f <- factor(ifelse(f$carrier %in% top, f$carrier, "Other"))
say("Analysis set: %d flights", nrow(f))

# ---- 2. Hypothesis tests with effect sizes
say("")
say("== Hypothesis tests ==")
ct <- table(f$origin, f$delayed)
chi <- chisq.test(ct)
rates <- round(100 * prop.table(ct, 1)[, 2], 1)
say("Delay rate by airport: %s", paste(names(rates), paste0(rates, "%"), collapse = ", "))
say("Chi-square test of independence (airport vs delayed): X2 = %.1f, df = %d, p %s; Cramer's V = %.3f",
    chi$statistic, chi$parameter, fp(chi$p.value), sqrt(chi$statistic / (sum(ct) * (min(dim(ct)) - 1))))
pt <- prop.test(c(sum(f$delayed[f$precip_any == 1]), sum(f$delayed[f$precip_any == 0])),
                c(sum(f$precip_any == 1), sum(f$precip_any == 0)))
say("Delay rate with precipitation %.1f%% vs without %.1f%%; difference %.1f pts, 95%% CI [%.1f, %.1f], p %s",
    100 * pt$estimate[1], 100 * pt$estimate[2], 100 * diff(rev(pt$estimate)), 100 * pt$conf.int[1], 100 * pt$conf.int[2], fp(pt$p.value))
wt <- wilcox.test(dep_delay ~ precip_any, data = f)
say("Wilcoxon rank-sum, departure delay minutes, precip vs none: W = %.0f, p %s (medians %.0f vs %.0f min)",
    wt$statistic, fp(wt$p.value), median(f$dep_delay[f$precip_any == 1]), median(f$dep_delay[f$precip_any == 0]))
hr <- tapply(f$delayed, f$hour_f, mean)
say("Delay rate by scheduled hour: %.1f%% at %s:00 rising to %.1f%% at %s:00 (delays build up through the day)",
    100 * min(hr), names(which.min(hr)), 100 * max(hr), names(which.max(hr)))
write.csv(data.frame(hour = names(hr), delay_rate = round(as.numeric(hr), 4), flights = as.integer(table(f$hour_f))),
          "results/delay_rate_by_hour.csv", row.names = FALSE)

# ---- 3. Logistic regression: train Jan-Sep, test Oct-Dec (forecast unseen months)
say("")
say("== Logistic regression (train Jan-Sep, test Oct-Dec) ==")
tr <- f[f$month <= 9, ]; te <- f[f$month >= 10, ]
tr$month_f <- droplevels(tr$month_f)
form_full <- delayed ~ hour_f + origin + carrier_f + precip_any + low_vis + wind_speed + temp
form_base <- delayed ~ hour_f
m_full <- glm(form_full, data = tr, family = binomial())
m_base <- glm(form_base, data = tr, family = binomial())
auc <- function(y, p) { r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0); (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
eval_m <- function(m, d) { p <- predict(m, newdata = d, type = "response"); c(auc = auc(d$delayed, p), brier = mean((p - d$delayed)^2), mean_p = mean(p)) }
e_base <- eval_m(m_base, te); e_full <- eval_m(m_full, te)
say("Train %d flights (%.1f%% delayed); test %d flights (%.1f%% delayed)", nrow(tr), 100 * mean(tr$delayed), nrow(te), 100 * mean(te$delayed))
say("Test AUC: hour-of-day only %.3f; full model %.3f", e_base["auc"], e_full["auc"])
say("Test Brier score: hour only %.4f; full %.4f; always-predict-train-rate %.4f",
    e_base["brier"], e_full["brier"], mean((mean(tr$delayed) - te$delayed)^2))
say("Calibration: mean predicted %.1f%% vs actual %.1f%% on the test months", 100 * e_full["mean_p"], 100 * mean(te$delayed))
lr <- anova(m_base, m_full, test = "Chisq")
say("Likelihood-ratio test, full vs hour-only (train): deviance drop %.0f on %d df, p %s", lr$Deviance[2], lr$Df[2], fp(lr$`Pr(>Chi)`[2]))

ci <- suppressMessages(confint.default(m_full))
or <- data.frame(term = names(coef(m_full)), odds_ratio = exp(coef(m_full)), ci_low = exp(ci[, 1]), ci_high = exp(ci[, 2]),
                 p_value = summary(m_full)$coefficients[, 4])
or <- or[or$term != "(Intercept)", ]
or[, 2:5] <- lapply(or[, 2:5], function(x) signif(x, 4))
write.csv(or, "results/odds_ratios.csv", row.names = FALSE)
pick <- function(t) or[or$term == t, ]
for (t in c("precip_any", "low_vis", "wind_speed", "originJFK", "originLGA")) {
  r <- pick(t); say("Odds ratio %-11s %.2f (95%% CI %.2f-%.2f)", t, r$odds_ratio, r$ci_low, r$ci_high)
}

# decile calibration table on test months
te$p <- predict(m_full, newdata = te, type = "response")
te$decile <- cut(te$p, quantile(te$p, 0:10 / 10), include.lowest = TRUE, labels = 1:10)
cal <- aggregate(cbind(predicted = p, actual = delayed) ~ decile, data = te, FUN = mean)
cal$flights <- as.integer(table(te$decile))
cal[, 2:3] <- round(cal[, 2:3], 4)
write.csv(cal, "results/calibration_deciles.csv", row.names = FALSE)
say("Riskiest 10%% of test flights: %.1f%% delayed vs %.1f%% in the safest 10%% (%.1fx)",
    100 * cal$actual[10], 100 * cal$actual[1], cal$actual[10] / cal$actual[1])

# ---- 4. Charts
png("results/delay_by_hour.png", width = 900, height = 500, type = "cairo")
plot(as.integer(names(hr)), 100 * hr, type = "b", pch = 19, xlab = "Scheduled departure hour", ylab = "Flights delayed 15+ min (%)",
     main = "NYC departures, 2013: delays build up through the day")
invisible(dev.off())
png("results/calibration.png", width = 700, height = 600, type = "cairo")
plot(100 * cal$predicted, 100 * cal$actual, pch = 19, xlab = "Predicted delay rate (%)", ylab = "Actual delay rate (%)",
     main = "Calibration on unseen months (Oct-Dec 2013), by risk decile", xlim = c(0, 60), ylim = c(0, 60))
abline(0, 1, lty = 2)
invisible(dev.off())

writeLines(out, "results/summary.txt")
