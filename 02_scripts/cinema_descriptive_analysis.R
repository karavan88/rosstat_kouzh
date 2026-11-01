library(haven)

options(stringsAsFactors = FALSE)
try(Sys.setlocale("LC_CTYPE", "ru_RU.UTF-8"), silent = TRUE)

inputs <- data.frame(
  year = c(2016, 2018, 2020, 2020, 2022, 2022, 2024, 2024),
  sample = c("main", "main", "main", "additional", "main", "additional", "main", "additional"),
  file = c(
    "01_input_data/0102_process_data/IND_16_repaired.sav",
    "01_input_data/0101_raw_data/IND_18.sav",
    "01_input_data/0101_raw_data/IND_OSN_20.sav",
    "01_input_data/0101_raw_data/IND_DOP_20.sav",
    "01_input_data/0101_raw_data/IND_OSN_22.sav",
    "01_input_data/0101_raw_data/IND_DOP_22.sav",
    "01_input_data/0101_raw_data/IND_OSN_24.sav",
    "01_input_data/0101_raw_data/IND_DOP_24.sav"
  ),
  weight = c("KVZV2", "KVZV", "KVZV", NA, "KVZV", NA, "KVZV", NA),
  download_var = c("I08_05_10", rep("I08_05_11", 7)),
  stringsAsFactors = FALSE
)

vars <- c(
  "I09_03_01_01", "I09_03_01_02", "H01_01", "H01_02", "I07_01",
  "DEN_NA_DUSHU", "DOX_mean", "D02_28_01", "I08_05_10", "I08_05_11",
  "KVZV", "KVZV2"
)

raw_values <- function(x) {
  x <- haven::zap_labels(x)
  if (is.character(x)) {
    x <- trimws(x)
    x[!nzchar(x)] <- NA_character_
  }
  x
}

num_values <- function(x) suppressWarnings(as.numeric(raw_values(x)))

weight_for <- function(d, weight_name) {
  if (is.na(weight_name) || !weight_name %in% names(d)) return(NULL)
  w <- num_values(d[[weight_name]])
  if (all(is.na(w)) || all(w[!is.na(w)] <= 0)) return(NULL)
  w
}

wmean <- function(x, w = NULL) {
  ok <- is.finite(x)
  if (!is.null(w)) ok <- ok & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  if (is.null(w)) mean(x[ok]) else stats::weighted.mean(x[ok], w[ok])
}

wquantile <- function(x, probs, w = NULL) {
  ok <- is.finite(x)
  if (!is.null(w)) ok <- ok & is.finite(w) & w > 0
  if (!any(ok)) return(rep(NA_real_, length(probs)))
  x <- x[ok]
  if (is.null(w)) return(as.numeric(stats::quantile(x, probs, names = FALSE, type = 2)))
  w <- w[ok]
  o <- order(x)
  x <- x[o]
  w <- w[o]
  vapply(probs, function(p) x[which(cumsum(w) >= p * sum(w))[1]], numeric(1))
}

bind_fill <- function(rows) {
  if (!length(rows)) return(data.frame())
  cols <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(x) {
    for (nm in setdiff(cols, names(x))) x[[nm]] <- NA
    x[cols]
  })
  do.call(rbind, rows)
}

label_for <- function(x, value) {
  labs <- attr(x, "labels")
  if (is.null(labs)) return(as.character(value))
  i <- which(as.character(unname(labs)) == as.character(value))
  if (!length(i)) as.character(value) else names(labs)[i[1]]
}

cat_summary <- function(d, variable, group, weight, year, sample) {
  x <- raw_values(d[[variable]][group])
  values <- sort(unique(x[!is.na(x)]))
  if (!length(values)) return(data.frame())
  w <- if (is.null(weight)) NULL else weight[group]
  denom <- !is.na(x)
  if (!is.null(w)) denom <- denom & is.finite(w) & w > 0
  denom_n <- sum(denom)
  denom_w <- if (is.null(w)) NA_real_ else sum(w[denom])
  do.call(rbind, lapply(values, function(value) {
    chosen <- !is.na(x) & x == value
    n <- sum(chosen)
    wn <- if (is.null(w)) NA_real_ else sum(w[chosen & is.finite(w) & w > 0], na.rm = TRUE)
    data.frame(
      year = year, sample = sample, variable = variable,
      category_code = as.character(value),
      category = label_for(d[[variable]], value),
      n = n, percent_unweighted = if (denom_n) 100 * n / denom_n else NA_real_,
      percent_weighted = if (!is.null(w) && denom_w > 0) 100 * wn / denom_w else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}

attendance_rows <- list()
profile_rows <- list()
related_rows <- list()

for (i in seq_len(nrow(inputs))) {
  meta <- inputs[i, ]
  d <- haven::read_sav(meta$file, encoding = "windows-1251", col_select = tidyselect::any_of(vars))
  wt <- weight_for(d, meta$weight)
  wlabel <- if (is.null(wt)) "unweighted" else meta$weight

  cinema <- num_values(d[["I09_03_01_01"]])
  yes <- !is.na(cinema) & cinema == 1
  valid_attendance <- !is.na(cinema) & cinema %in% c(1, 2)
  yes_n <- sum(yes)
  valid_n <- sum(valid_attendance)
  valid_w <- if (is.null(wt)) NULL else valid_attendance & is.finite(wt) & wt > 0
  yes_weighted_pct <- if (is.null(wt) || !any(valid_w)) NA_real_ else
    100 * sum(wt[yes & is.finite(wt) & wt > 0], na.rm = TRUE) / sum(wt[valid_w], na.rm = TRUE)

  freq <- if ("I09_03_01_02" %in% names(d)) num_values(d[["I09_03_01_02"]]) else rep(NA_real_, nrow(d))
  freq_valid <- yes & !is.na(freq) & freq %in% c(3, 4)
  freq_denom_w <- if (is.null(wt)) NA_real_ else sum(wt[freq_valid & is.finite(wt) & wt > 0], na.rm = TRUE)
  freq_weighted_pct <- function(code) {
    if (is.null(wt) || !is.finite(freq_denom_w) || freq_denom_w <= 0) return(NA_real_)
    100 * sum(wt[freq_valid & freq == code & is.finite(wt) & wt > 0], na.rm = TRUE) / freq_denom_w
  }

  attendance_rows[[length(attendance_rows) + 1]] <- data.frame(
    year = meta$year, sample = meta$sample, file = basename(meta$file), weighting = wlabel,
    n_total = nrow(d), n_attendance_valid = valid_n, n_cinema_yes = yes_n,
    n_cinema_no = sum(!is.na(cinema) & cinema == 2),
    cinema_yes_pct_unweighted = if (valid_n) 100 * yes_n / valid_n else NA_real_,
    cinema_yes_pct_weighted = yes_weighted_pct,
    n_frequency_valid_among_yes = sum(freq_valid),
    n_frequency_regular = sum(freq_valid & freq == 3),
    frequency_regular_pct_unweighted = if (sum(freq_valid)) 100 * sum(freq_valid & freq == 3) / sum(freq_valid) else NA_real_,
    frequency_regular_pct_weighted = freq_weighted_pct(3),
    n_frequency_once_or_more = sum(freq_valid & freq == 4),
    frequency_once_or_more_pct_unweighted = if (sum(freq_valid)) 100 * sum(freq_valid & freq == 4) / sum(freq_valid) else NA_real_,
    frequency_once_or_more_pct_weighted = freq_weighted_pct(4)
  )

  for (v in c("H01_01", "I07_01")) {
    z <- cat_summary(d, v, yes, wt, meta$year, meta$sample)
    if (nrow(z)) {
      z$weighting <- wlabel
      profile_rows[[length(profile_rows) + 1]] <- z
    }
  }

  for (v in c("H01_02", "DEN_NA_DUSHU", "DOX_mean")) {
    x <- num_values(d[[v]][yes])
    w <- if (is.null(wt)) NULL else wt[yes]
    ok <- is.finite(x)
    if (!is.null(w)) ok <- ok & is.finite(w) & w > 0
    profile_rows[[length(profile_rows) + 1]] <- data.frame(
      year = meta$year, sample = meta$sample, variable = v,
      category_code = NA_character_, category = "continuous",
      n = sum(is.finite(x)), mean_unweighted = if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_,
      sd_unweighted = if (sum(is.finite(x)) > 1) stats::sd(x[is.finite(x)]) else NA_real_,
      median_unweighted = if (any(is.finite(x))) stats::median(x[is.finite(x)]) else NA_real_,
      mean_weighted = if (is.null(w) || !any(ok)) NA_real_ else wmean(x, w),
      median_weighted = if (is.null(w) || !any(ok)) NA_real_ else wquantile(x, 0.5, w),
      q25_weighted = if (is.null(w) || !any(ok)) NA_real_ else wquantile(x, 0.25, w),
      q75_weighted = if (is.null(w) || !any(ok)) NA_real_ else wquantile(x, 0.75, w),
      weighting = wlabel, stringsAsFactors = FALSE
    )
  }

  child <- raw_values(d[["D02_28_01"]])
  child_selected <- !is.na(child) & child == "1"
  child_binary <- !is.na(child) & child %in% c("0", "1")
  download_name <- meta$download_var
  download <- raw_values(d[[download_name]])
  selected_code <- if (meta$year == 2024) "1" else if (meta$year == 2016) "10" else "11"
  download_selected <- !is.na(download) & download == selected_code
  download_binary <- !is.na(download) & download %in% c("0", "1")

  for (item in c("child_cinema", "film_download")) {
    x <- if (item == "child_cinema") child else download
    selected <- if (item == "child_cinema") child_selected else download_selected
    binary_valid <- if (item == "child_cinema") child_binary else download_binary
    observed_codes <- unique(as.character(x[!is.na(x)]))
    code_is_binary <- all(c("0", "1") %in% observed_codes)
    denom <- if (code_is_binary) binary_valid else rep(FALSE, length(selected))
    related_rows[[length(related_rows) + 1]] <- data.frame(
      year = meta$year, sample = meta$sample, item = item,
      variable = if (item == "child_cinema") "D02_28_01" else download_name,
      weighting = wlabel,
      n_selected = sum(selected),
      n_binary_valid = sum(denom),
      percent_selected_unweighted = if (sum(denom)) 100 * sum(selected & denom) / sum(denom) else NA_real_,
      percent_selected_weighted = if (is.null(wt) || !sum(denom & is.finite(wt) & wt > 0)) NA_real_ else
        100 * sum(wt[selected & denom & is.finite(wt) & wt > 0], na.rm = TRUE) /
        sum(wt[denom & is.finite(wt) & wt > 0], na.rm = TRUE),
      coding_note = if (code_is_binary) "0/1 coding; percent among non-missing 0/1 records" else
        "selected-response count only; missing is not treated as no",
      stringsAsFactors = FALSE
    )
  }
}

dir.create("03_outputs", showWarnings = FALSE, recursive = TRUE)
attendance <- do.call(rbind, attendance_rows)
profile <- bind_fill(profile_rows)
related <- bind_fill(related_rows)

utils::write.csv(attendance, "03_outputs/cinema_attendance_descriptives.csv", row.names = FALSE, fileEncoding = "UTF-8", na = "")
utils::write.csv(profile, "03_outputs/cinema_goers_profile_descriptives.csv", row.names = FALSE, fileEncoding = "UTF-8", na = "")
utils::write.csv(related, "03_outputs/cinema_related_variables_descriptives.csv", row.names = FALSE, fileEncoding = "UTF-8", na = "")

cat("Wrote descriptive tables to 03_outputs.\n")
