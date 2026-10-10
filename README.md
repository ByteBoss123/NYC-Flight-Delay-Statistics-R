# NYC Flight Departure Delays: Statistical Analysis in R

**Question.** What drives departure delays at New York's three airports, how large is each effect, and how well
can a simple, explainable model rank flights by delay risk for months it has never seen?

**Data.** [nycflights13](https://github.com/tidyverse/nycflights13) (commit `df98ef2`): all **336,776** flights
departing JFK, LGA and EWR in 2013 (US Bureau of Transportation Statistics) joined to **26,115** hourly airport weather
records. Files are committed in `data/` with SHA-256 checksums. **Base R only**, no packages.

## Results at a glance (`results/summary.txt`)

| Analysis | Result |
|---|---|
| Delay rate (departed 15+ min late, FAA definition) | 22.2% of 328,521 operated flights (8,255 cancelled excluded) |
| Precipitation | 40.0% delayed vs 21.0% without; **+19.0 points, 95% CI [18.3, 19.7]** (two-proportion test, p < 1e-300) |
| Time of day | 6.3% of 5am departures delayed, rising to 37.8% at 9pm: delays build up through the day |
| Airport | EWR 25.3%, JFK 21.3%, LGA 19.5% (chi-square p = 2.8e-246, but Cramer's V = 0.06: significant, small effect) |
| Logistic regression, adjusted odds ratios | Precipitation **2.52** (95% CI 2.42-2.61); visibility under 3 miles 1.71 (1.62-1.80); JFK 0.83 vs EWR |
| Forecast on unseen months (train Jan-Sep, test Oct-Dec) | AUC 0.681 vs 0.647 for hour-of-day alone; Brier 0.148 vs 0.159 for a constant rate |
| Risk ranking | Riskiest 10% of test flights: **41.0% delayed vs 6.5%** in the safest 10% (6.3x) |
| Calibration | Mean predicted 21.5% vs actual 19.6%; slightly over-predicts the riskiest decile (45% vs 41%) |

## Method

1. **Clean and join** (`R/analysis.R`): drop cancelled flights; join observed hourly weather at the origin for the
   scheduled hour (99.5% of flights matched). Weather here is observed, so it stands in for a forecast; a production
   model would use the forecast available at planning time.
2. **Hypothesis tests with effect sizes**: chi-square with Cramer's V, two-proportion test with a confidence
   interval, Wilcoxon rank-sum for delay minutes (skewed, so not a t-test).
3. **Logistic regression**: hour of day, airport, carrier, precipitation, low visibility, wind speed, temperature.
   Odds ratios with 95% confidence intervals (`results/odds_ratios.csv`); likelihood-ratio test against the
   hour-only model.
4. **Out-of-time validation**: fit on January-September, score October-December. AUC, Brier score and a decile
   calibration table (`results/calibration_deciles.csv`, `results/calibration.png`).

## Limitations

- One year, one city. The test months (Oct-Dec) had fewer delays (19.6%) than training (23.0%).
- Delays propagate from earlier flights by the same aircraft; this model does not use the previous leg, which is
  the obvious next feature.
- Large samples make almost every difference "significant"; the effect sizes and intervals are the useful part.

## Run

```bash
Rscript R/analysis.R          # ~1 min; writes results/
Rscript tests/test_results.R  # checksums, row counts, published numbers reproduce
```
