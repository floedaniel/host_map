# =============================================================================
# 02_validation_analysis.R
# Statistical reassessment of the GBIF-count uncertainty classification.
# Input : 2_processed_data/validation_hosts.rds (from 01_validation_collect.R)
# Output: 5_outputs/validation_*.csv, 5_outputs/fig_*.png, 5_outputs/validation_results.rds
# Run from the project root:  Rscript 4_scripts/02_validation_analysis.R
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })
source("4_scripts/hostmap_functions.R")
set.seed(20261104)
out <- "5_outputs"; dir.create(out, showWarnings = FALSE)

# ---- Data -------------------------------------------------------------------
raw <- readRDS("2_processed_data/validation_hosts.rds")
d <- raw |>
  filter(is_species, !is.na(acceptedKey), !is.na(n_total_all)) |>
  arrange(desc(!is.na(ref_status))) |>
  distinct(acceptedKey, .keep_all = TRUE) |>           # one row per GBIF taxon
  mutate(n = n_default,                                  # excludes LIVING/FOSSIL specimens
         listed = !is.na(ref_status),
         status3 = factor(ifelse(listed, as.character(ref_status), "Not listed"),
                          levels = c("Absent", "Present, not established", "Established", "Not listed")),
         est = as.integer(status3 == "Established"),     # closed world: not listed = not established
         present = ifelse(status3 %in% c("Established", "Present, not established"), 1L,
                          ifelse(status3 == "Absent", 0L, NA_integer_)),
         log_n = log10(n + 1),
         prop_preserved = ifelse(n > 0, n_preserved / n, 0))
cat("Validation hosts (unique GBIF species):", nrow(d), "\n"); print(table(d$status3))

# ---- Metric helpers ------------------------------------------------------------
metrics <- function(pred, truth) {
  ok <- !is.na(pred) & !is.na(truth); p <- pred[ok]; t <- truth[ok]
  tp <- sum(p == 1 & t == 1); tn <- sum(p == 0 & t == 0); fp <- sum(p == 1 & t == 0); fn <- sum(p == 0 & t == 1)
  n <- tp + tn + fp + fn
  sens <- tp / (tp + fn); spec <- tn / (tn + fp); acc <- (tp + tn) / n
  pe <- ((tp + fp) * (tp + fn) + (tn + fn) * (tn + fp)) / n^2
  data.frame(n = n, TP = tp, FP = fp, FN = fn, TN = tn, sensitivity = sens, specificity = spec,
             PPV = tp / (tp + fp), NPV = tn / (tn + fn), accuracy = acc,
             balanced_accuracy = (sens + spec) / 2, kappa = (acc - pe) / (1 - pe))
}
auc <- function(score, truth) {                        # Mann-Whitney AUC
  ok <- !is.na(score) & !is.na(truth); s <- score[ok]; t <- truth[ok]
  r <- rank(s); n1 <- sum(t == 1); n0 <- sum(t == 0)
  (sum(r[t == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
boot_ci <- function(pred_fun, data, truth_col, B = 1000) {
  v <- replicate(B, { i <- sample(nrow(data), replace = TRUE); x <- data[i, ]
    metrics(pred_fun(x), x[[truth_col]])$balanced_accuracy })
  quantile(v, c(.025, .975), na.rm = TRUE)
}

# ---- 1. Cross-tabulation of the original classes against known status ---------
d$class_orig <- classify_count(d$n, 10, 50)
xt_all <- table(Class = d$class_orig, Status = d$status3)
print(xt_all)
write.csv(as.data.frame.matrix(xt_all), file.path(out, "validation_crosstab_original.csv"))

# Ordinal agreement on listed hosts: 0 -> Absent, 1-50 -> Present not est., >50 -> Established
ord_pred <- function(cls) factor(c("Absent", "Present, not established", "Present, not established", "Established")[as.integer(cls)],
                                 levels = levels(d$status3)[1:3])
L <- d |> filter(listed) |> mutate(status3 = droplevels(status3))
ord_tab <- table(Predicted = ord_pred(L$class_orig), Known = L$status3)
w_kappa <- irr::kappa2(data.frame(as.integer(ord_pred(L$class_orig)), as.integer(L$status3)), weight = "squared")$value
ord_agree <- mean(as.character(ord_pred(L$class_orig)) == as.character(L$status3))

# Binary readings of the original rule
bin_orig <- bind_rows(
  cbind(rule = "Documented (>=1 record) vs present in Norway [listed hosts]", metrics(as.integer(L$n >= 1), L$present)),
  cbind(rule = "Low uncertainty (>50) vs established [listed hosts]", metrics(as.integer(L$n > 50), L$est)),
  cbind(rule = "Low uncertainty (>50) vs established [all hosts]", metrics(as.integer(d$n > 50), d$est)),
  cbind(rule = "Low uncertainty (>50) vs established [hosts with >=1 record]", metrics(as.integer(d$n[d$n > 0] > 50), d$est[d$n > 0])),
  cbind(rule = ">=Moderate (>10) vs established [all hosts]", metrics(as.integer(d$n > 10), d$est)),
  cbind(rule = ">=High (>=1) vs established [all hosts]", metrics(as.integer(d$n >= 1), d$est)))
print(bin_orig[, c("rule", "n", "sensitivity", "specificity", "balanced_accuracy", "kappa")], digits = 3)

# ---- 2. Distribution of counts per status; where does misclassification occur? ---
bins <- c(-Inf, 0, 5, 10, 25, 50, 100, 250, 500, 1000, 5000, Inf)
lab <- c("0", "1-5", "6-10", "11-25", "26-50", "51-100", "101-250", "251-500", "501-1000", "1001-5000", ">5000")
bin_tab <- d |> mutate(bin = cut(n, bins, labels = lab)) |>
  group_by(bin) |>
  summarise(hosts = n(), established = sum(est), listed_not_established = sum(status3 %in% c("Absent", "Present, not established")),
            not_listed = sum(status3 == "Not listed"), prop_established = mean(est),
            median_years = median(n_years), median_counties = median(n_counties), .groups = "drop")
print(bin_tab, n = 20)
write.csv(bin_tab, file.path(out, "validation_established_by_count_bin.csv"), row.names = FALSE)
dist_tab <- d |> group_by(status3) |>
  summarise(hosts = n(), q10 = quantile(n, .1), q25 = quantile(n, .25), median = median(n),
            q75 = quantile(n, .75), q90 = quantile(n, .9), .groups = "drop")
write.csv(dist_tab, file.path(out, "validation_count_distribution_by_status.csv"), row.names = FALSE)

# Misclassified hosts under the original rule (for inspection)
mis <- d |> filter((n > 50 & est == 0) | (n <= 50 & est == 1)) |>
  select(input_name, gbif_canonical, n, n_human, n_preserved, n_living, n_years, n_counties, status3, ref_detail, pests) |>
  arrange(desc(n))
write.csv(mis, file.path(out, "validation_misclassified_original_rule.csv"), row.names = FALSE)

# ---- 3. Alternative rules, compared with repeated stratified cross-validation ---
# Each rule family has tuning parameters chosen on the training folds (max
# balanced accuracy) and is evaluated on the held-out fold.
grid_t <- c(0, 1, 2, 5, 10, 20, 30, 50, 75, 100, 150, 200, 300, 500, 1000)
grid_y <- c(1, 2, 3, 5, 7, 10, 15, 20, 30, 40, 60)
grid_c <- c(1, 2, 3, 4, 5, 7, 10, 15)
families <- list(
  `Count only (original, t = 50)` = list(grid = data.frame(t = 50), f = function(x, p) x$n > p$t),
  `Count, tuned t`                 = list(grid = data.frame(t = grid_t), f = function(x, p) x$n > p$t),
  `Distinct years only`            = list(grid = data.frame(y = grid_y), f = function(x, p) x$n_years >= p$y),
  `Counties only`                  = list(grid = data.frame(c = grid_c), f = function(x, p) x$n_counties >= p$c),
  `Count + years`                  = list(grid = expand.grid(t = grid_t, y = grid_y), f = function(x, p) x$n > p$t & x$n_years >= p$y),
  `Count + counties`               = list(grid = expand.grid(t = grid_t, c = grid_c), f = function(x, p) x$n > p$t & x$n_counties >= p$c),
  `Count + years + counties`       = list(grid = expand.grid(t = grid_t, y = grid_y, c = grid_c), f = function(x, p) x$n > p$t & x$n_years >= p$y & x$n_counties >= p$c),
  `Revised app rule (50/10/2)`     = list(grid = data.frame(t = 50, y = 10, c = 2), f = function(x, p) x$n > p$t & x$n_years >= p$y & x$n_counties >= p$c),
  `Human observations only`        = list(grid = data.frame(t = grid_t), f = function(x, p) x$n_human > p$t),
  `Preserved specimens only`       = list(grid = data.frame(t = c(0, 1, 2, 3, 5, 10, 20, 50, 100)), f = function(x, p) x$n_preserved > p$t),
  `Human + preserved (both)`       = list(grid = expand.grid(t = grid_t, p = c(0, 1, 2, 3, 5, 10, 20)), f = function(x, p) x$n_human > p$t & x$n_preserved > p$p)
)
logit_families <- list(
  `Logistic: log10(count+1)` = est ~ log_n,
  `Logistic: log count + log years + counties + preserved share` =
    est ~ log_n + log10(n_years + 1) + n_counties + prop_preserved
)

cv_eval <- function(data, k = 5, reps = 10) {
  res <- list()
  for (r in seq_len(reps)) {
    fold <- integer(nrow(data))
    for (cl in 0:1) { i <- which(data$est == cl); fold[i] <- sample(rep_len(1:k, length(i))) }
    for (f in 1:k) {
      tr <- data[fold != f, ]; te <- data[fold == f, ]
      for (nm in names(families)) {
        fam <- families[[nm]]
        ba <- vapply(seq_len(nrow(fam$grid)), function(g)
          metrics(as.integer(fam$f(tr, fam$grid[g, , drop = FALSE])), tr$est)$balanced_accuracy, 0)
        best <- fam$grid[which.max(ba), , drop = FALSE]
        m <- metrics(as.integer(fam$f(te, best)), te$est)
        res[[length(res) + 1]] <- cbind(family = nm, rep = r, fold = f,
                                        params = paste(names(best), unlist(best), sep = "=", collapse = ", "), m)
      }
      for (nm in names(logit_families)) {
        fit <- suppressWarnings(glm(logit_families[[nm]], data = tr, family = binomial))
        ptr <- predict(fit, tr, type = "response"); pte <- predict(fit, te, type = "response")
        cuts <- seq(0.05, 0.95, 0.025)
        cbest <- cuts[which.max(vapply(cuts, function(c) metrics(as.integer(ptr > c), tr$est)$balanced_accuracy, 0))]
        m <- metrics(as.integer(pte > cbest), te$est)
        m$auc <- auc(pte, te$est)
        res[[length(res) + 1]] <- cbind(family = nm, rep = r, fold = f, params = sprintf("p>%.3f", cbest), m)
      }
    }
  }
  bind_rows(res)
}

summarise_cv <- function(cv) cv |> group_by(family) |>
  summarise(ba_sd = sd(balanced_accuracy, na.rm = TRUE), balanced_accuracy = mean(balanced_accuracy, na.rm = TRUE),
            sensitivity = mean(sensitivity, na.rm = TRUE), specificity = mean(specificity, na.rm = TRUE),
            kappa = mean(kappa, na.rm = TRUE), PPV = mean(PPV, na.rm = TRUE),
            auc = if (all(is.na(auc))) NA_real_ else mean(auc, na.rm = TRUE),
            most_chosen_params = names(sort(table(params), decreasing = TRUE))[1], .groups = "drop") |>
  arrange(desc(balanced_accuracy))

# Scenario A: all hosts, closed-world (not listed = not established)
# Scenario B: hosts with >=1 Norwegian record (the cases where the rule matters)
# Scenario C: listed hosts only (no closed-world assumption)
scen <- list(A_all_hosts = d, B_hosts_with_records = filter(d, n > 0), C_listed_only = filter(d, listed))
cv_res <- lapply(scen, function(x) { x$auc <- NA_real_; cv <- cv_eval(x); cv$auc[is.na(cv$auc)] <- NA; summarise_cv(cv) })
for (s in names(cv_res)) { cat("\n== CV:", s, "\n"); print(as.data.frame(cv_res[[s]])[, c(1:4, 6, 8, 9)], digits = 3) }
for (s in names(cv_res)) write.csv(cv_res[[s]], file.path(out, sprintf("validation_cv_%s.csv", s)), row.names = FALSE)

# Single-variable discrimination (AUC) on scenario B
auc_tab <- data.frame(variable = c("count", "distinct years", "counties", "human observations", "preserved specimens", "years since 2000"),
  AUC = with(scen$B_hosts_with_records, c(auc(n, est), auc(n_years, est), auc(n_counties, est), auc(n_human, est),
                                          auc(n_preserved, est), auc(n_years_since_2000, est))))
print(auc_tab, digits = 3)
write.csv(auc_tab, file.path(out, "validation_auc_single_variables.csv"), row.names = FALSE)

# Final parameters fitted on all data (scenario B), with bootstrap CIs
fit_best <- function(nm, data) { fam <- families[[nm]]
  ba <- vapply(seq_len(nrow(fam$grid)), function(g) metrics(as.integer(fam$f(data, fam$grid[g, , drop = FALSE])), data$est)$balanced_accuracy, 0)
  list(params = fam$grid[which.max(ba), , drop = FALSE], ba = max(ba)) }
final <- lapply(names(families), function(nm) { fb <- fit_best(nm, scen$B_hosts_with_records)
  ci <- boot_ci(function(x) as.integer(families[[nm]]$f(x, fb$params)), scen$B_hosts_with_records, "est", B = 500)
  data.frame(family = nm, params = paste(names(fb$params), unlist(fb$params), sep = "=", collapse = ", "),
             in_sample_BA = fb$ba, BA_lo = ci[1], BA_hi = ci[2]) })
final <- bind_rows(final) |> arrange(desc(in_sample_BA))
print(final, digits = 3)
write.csv(final, file.path(out, "validation_final_parameters.csv"), row.names = FALSE)

# Logistic model on scenario B: coefficients (log-transformed count)
glm_b <- glm(est ~ log_n + log10(n_years + 1) + n_counties + prop_preserved, data = scen$B_hosts_with_records, family = binomial)
glm_coef <- as.data.frame(summary(glm_b)$coefficients)
print(glm_coef, digits = 3)
write.csv(glm_coef, file.path(out, "validation_logistic_coefficients.csv"))

# ---- 4. Figures (ggplot2, viridis, theme_minimal, black text) -----------------
th <- theme_minimal(base_size = 12) + theme(text = element_text(colour = "black"), axis.text = element_text(colour = "black"))
g1 <- ggplot(d, aes(status3, n + 1, colour = status3)) +
  geom_jitter(width = .25, height = 0, alpha = .45, size = 1.3) +
  geom_boxplot(fill = NA, colour = "black", outlier.shape = NA, width = .5) +
  geom_hline(yintercept = c(11, 51), linetype = 2) +
  scale_y_log10(labels = function(x) format(x - 1, big.mark = " ", scientific = FALSE)) +
  scale_colour_viridis_d(end = .9, guide = "none") +
  labs(x = "Status in Norwegian nature (Artsdatabanken)", y = "GBIF records in Norway (log scale)",
       title = "Record counts by known status", subtitle = "Dashed lines: thresholds 10 and 50") + th
ggsave(file.path(out, "fig_counts_by_status.png"), g1, width = 8, height = 5, dpi = 150, bg = "white")

g2 <- ggplot(bin_tab, aes(bin, prop_established, fill = prop_established)) +
  geom_col() + geom_text(aes(label = hosts), vjust = -.3, size = 3.2) +
  scale_fill_viridis_c(end = .9, guide = "none") + scale_y_continuous(labels = scales::percent, limits = c(0, 1.05)) +
  labs(x = "GBIF records in Norway", y = "Share established in Norway",
       title = "Share of hosts established, by record count", subtitle = "Numbers above bars: hosts per bin") + th
ggsave(file.path(out, "fig_established_by_bin.png"), g2, width = 8, height = 4.5, dpi = 150, bg = "white")

g3 <- ggplot(scen$B_hosts_with_records, aes(n_years, n_counties, colour = factor(est, labels = c("Not established", "Established")))) +
  geom_jitter(width = .3, height = .2, alpha = .6) + geom_vline(xintercept = 10, linetype = 2) + geom_hline(yintercept = 2, linetype = 2) +
  scale_colour_viridis_d(end = .8, name = NULL) + labs(x = "Distinct years with records", y = "Counties with records",
       title = "Temporal and spatial spread (hosts with at least 1 record)") + th
ggsave(file.path(out, "fig_years_counties.png"), g3, width = 8, height = 5, dpi = 150, bg = "white")

# ---- 5. Documented presence: the target the uncertainty class is meant for -----
# Listed hosts only (Artsdatabanken says present = established or present-not-
# established; absent = door-knocker 'Abroad' or 'no documented occurrence').
P <- filter(d, !is.na(present))
wilson <- function(x, n, z = 1.96) { p <- x / n
  c(lo = (p + z^2 / (2 * n) - z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / (1 + z^2 / n),
    hi = (p + z^2 / (2 * n) + z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / (1 + z^2 / n)) }
class_err <- function(cls) bind_rows(lapply(levels(cls), function(k) { i <- cls == k; a <- sum(P$present[i] == 0)
  ci <- wilson(a, sum(i)); data.frame(class = k, hosts = sum(i), absent = a, p_absent = a / sum(i), lo = ci[1], hi = ci[2]) }))
pres_orig <- class_err(cut(P$n, c(-Inf, 0, 10, 50, Inf), labels = c("0", "1-10", "11-50", ">50")))
print(pres_orig, digits = 3)

# Logistic calibration of P(present | count) on hosts with >= 1 record
P1 <- filter(P, n >= 1)
TARGETS <- c(moderate = 0.95, low = 0.99)   # P(present) at the class boundaries
thr_from_fit <- function(x) { f <- suppressWarnings(glm(present ~ log10(n + 1), data = x, family = binomial)); cf <- coef(f)
  if (cf[2] <= 0) return(c(NA, NA)); 10^((qlogis(TARGETS) - cf[1]) / cf[2]) - 1 }
thr_point <- thr_from_fit(P1)
thr_boot <- t(replicate(2000, thr_from_fit(P1[sample(nrow(P1), replace = TRUE), ])))
thr_tab <- data.frame(boundary = names(TARGETS), target_P_present = TARGETS, records_point = thr_point,
                      ci_lo = apply(thr_boot, 2, quantile, .025, na.rm = TRUE), ci_hi = apply(thr_boot, 2, quantile, .975, na.rm = TRUE))
print(thr_tab, digits = 3)
write.csv(thr_tab, file.path(out, "validation_thresholds_presence.csv"), row.names = FALSE)
T1_NEW <- floor(thr_point[1]); T2_NEW <- floor(thr_point[2])   # high: 1..T1, moderate: T1+1..T2, low: > T2
pres_new <- class_err(cut(P$n, c(-Inf, 0, T1_NEW, T2_NEW, Inf), labels = c("0", sprintf("1-%d", T1_NEW), sprintf("%d-%d", T1_NEW + 1, T2_NEW), sprintf(">%d", T2_NEW))))
print(pres_new, digits = 3)
write.csv(rbind(cbind(rule = "original 10/50", pres_orig), cbind(rule = sprintf("calibrated %d/%d", T1_NEW, T2_NEW), pres_new)),
          file.path(out, "validation_presence_error_by_class.csv"), row.names = FALSE)

# Do time, space, record type or recency add information beyond the count?
auc_pres <- data.frame(variable = c("count (excl. living)", "count incl. living specimens", "distinct years", "counties",
                                    "human observations", "preserved specimens", "last year recorded"),
  AUC = with(P1, c(auc(n, present), auc(n_total_all - 0, present), auc(n_years, present), auc(n_counties, present),
                   auc(n_human, present), auc(n_preserved, present), auc(ifelse(is.na(last_year), 0, last_year), present))))
print(auc_pres, digits = 3)
write.csv(auc_pres, file.path(out, "validation_auc_presence.csv"), row.names = FALSE)
f0 <- glm(present ~ log10(n + 1), data = P1, family = binomial)
add_tests <- bind_rows(lapply(c("log10(n_years + 1)", "n_counties", "I(n_human / n)", "I(last_year >= 2000)"), function(v) {
  f1 <- update(f0, as.formula(paste(". ~ . +", v)))
  data.frame(added_variable = v, LR_p = anova(f0, f1, test = "LRT")[2, "Pr(>Chi)"], AIC_base = AIC(f0), AIC_added = AIC(f1)) }))
print(add_tests, digits = 3)
write.csv(add_tests, file.path(out, "validation_added_variables_presence.csv"), row.names = FALSE)

g4 <- ggplot(P1, aes(n, present)) + geom_jitter(height = .03, width = 0, alpha = .5, aes(colour = factor(present))) +
  stat_smooth(method = "glm", formula = y ~ log10(x + 1), method.args = list(family = binomial), colour = "black") +
  geom_vline(xintercept = c(T1_NEW, T2_NEW) + .5, linetype = 2) + scale_x_log10() +
  scale_colour_viridis_d(end = .8, labels = c("Absent", "Present"), name = NULL) +
  labs(x = "GBIF records in Norway (log scale)", y = "P(present in Norway)",
       title = "Calibration of documented presence against Artsdatabanken",
       subtitle = sprintf("Dashed lines: calibrated thresholds (P = %.2f and %.2f)", TARGETS[1], TARGETS[2])) + th
ggsave(file.path(out, "fig_presence_calibration.png"), g4, width = 8, height = 4.5, dpi = 150, bg = "white")

saveRDS(list(presence_original = pres_orig, presence_new = pres_new, thresholds = thr_tab, t1 = T1_NEW, t2 = T2_NEW,
             auc_presence = auc_pres, added_variables = add_tests, n_presence = nrow(P), n_presence_records = nrow(P1)),
        file.path(out, "validation_presence_results.rds"))

saveRDS(list(data = d, crosstab = xt_all, ordinal_table = ord_tab, weighted_kappa = w_kappa, ordinal_agreement = ord_agree,
             binary_original = bin_orig, bins = bin_tab, distribution = dist_tab, cv = cv_res, auc = auc_tab,
             final = final, glm = glm_coef, misclassified = mis),
        file.path(out, "validation_results.rds"))
cat("\nOrdinal agreement (listed):", round(ord_agree, 3), " weighted kappa:", round(w_kappa, 3), "\n")
