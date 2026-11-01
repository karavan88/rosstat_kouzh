options(stringsAsFactors = FALSE)
try(Sys.setlocale("LC_CTYPE", "ru_RU.UTF-8"), silent = TRUE)

attendance <- read.csv("03_outputs/cinema_attendance_descriptives.csv", check.names = FALSE)
profile <- read.csv("03_outputs/cinema_goers_profile_descriptives.csv", check.names = FALSE, fileEncoding = "UTF-8")
related <- read.csv("03_outputs/cinema_related_variables_descriptives.csv", check.names = FALSE, fileEncoding = "UTF-8")

tex_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\\\", "\001", x, fixed = TRUE)
  replacements <- c(
    "{" = "\\{", "}" = "\\}",
    "#" = "\\#", "$" = "\\$", "%" = "\\%", "&" = "\\&",
    "_" = "\\_", "~" = "\\textasciitilde{}", "^" = "\\textasciicircum{}"
  )
  for (old in names(replacements)) x <- gsub(old, replacements[[old]], x, fixed = TRUE)
  x <- gsub("\001", "\\textbackslash{}", x, fixed = TRUE)
  x
}

fmt_num <- function(x, digits = 1) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "—", format(round(x, digits), trim = TRUE, scientific = FALSE,
                               big.mark = " ", decimal.mark = ",", nsmall = digits))
}

fmt_int <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "—", format(round(x), trim = TRUE, scientific = FALSE, big.mark = " "))
}

make_longtable <- function(data, columns, headers, widths = NULL, row_limit = Inf) {
  if (nrow(data) > row_limit) data <- data[seq_len(row_limit), , drop = FALSE]
  if (is.null(widths)) {
    spec <- paste(rep("r", length(columns)), collapse = "")
  } else {
    spec <- paste0("p{", widths, "}", collapse = "")
  }
  lines <- c(
    paste0("\\begin{longtable}{", spec, "}"),
    "\\toprule",
    paste(tex_escape(headers), collapse = " & "), "\\\\",
    "\\midrule",
    "\\endfirsthead",
    "\\toprule",
    paste(tex_escape(headers), collapse = " & "), "\\\\",
    "\\midrule",
    "\\endhead",
    paste0("\\midrule \\multicolumn{", length(columns), "}{r}{\\small Continued on next page} \\\\"),
    "\\endfoot",
    "\\bottomrule",
    "\\endlastfoot"
  )
  for (i in seq_len(nrow(data))) {
    vals <- vapply(columns, function(nm) tex_escape(data[[nm]][i]), character(1))
    lines <- c(lines, paste(vals, collapse = " & "), "\\\\")
  }
  c(lines, "\\end{longtable}")
}

main <- attendance[attendance$sample == "main", , drop = FALSE]
main <- main[order(main$year), , drop = FALSE]
get_cont <- function(year, variable, field) {
  z <- profile[profile$sample == "main" & profile$year == year & profile$variable == variable, , drop = FALSE]
  if (!nrow(z)) return(NA_real_)
  suppressWarnings(as.numeric(z[[field]][1]))
}
get_cat_pct <- function(year, variable, code) {
  z <- profile[profile$sample == "main" & profile$year == year & profile$variable == variable & profile$category_code == code, , drop = FALSE]
  if (!nrow(z)) return(NA_real_)
  suppressWarnings(as.numeric(z$percent_weighted[1]))
}
get_tertiary_pct <- function(year) {
  z <- profile[profile$sample == "main" & profile$year == year & profile$variable == "I07_01" & profile$category_code %in% c("1", "2", "3", "4"), , drop = FALSE]
  sum(as.numeric(z$percent_weighted), na.rm = TRUE)
}

overview <- data.frame(
  year = main$year,
  attendees = fmt_int(main$n_cinema_yes),
  cinema_share = fmt_num(main$cinema_yes_pct_weighted),
  mean_age = vapply(main$year, function(y) fmt_num(get_cont(y, "H01_02", "mean_weighted")), character(1)),
  women = vapply(main$year, function(y) fmt_num(get_cat_pct(y, "H01_01", "2")), character(1)),
  tertiary = vapply(main$year, function(y) fmt_num(get_tertiary_pct(y)), character(1)),
  percap_median = vapply(main$year, function(y) fmt_int(get_cont(y, "DEN_NA_DUSHU", "median_weighted")), character(1)),
  household_median = vapply(main$year, function(y) fmt_int(get_cont(y, "DOX_mean", "median_weighted")), character(1)),
  regular = fmt_num(main$frequency_regular_pct_weighted)
)

att <- attendance
att$year <- as.character(att$year)
att$n_total <- fmt_int(att$n_total)
att$n_attendance_valid <- fmt_int(att$n_attendance_valid)
att$n_cinema_yes <- fmt_int(att$n_cinema_yes)
att$n_cinema_no <- fmt_int(att$n_cinema_no)
att$cinema_yes_pct_unweighted <- fmt_num(att$cinema_yes_pct_unweighted)
att$cinema_yes_pct_weighted <- fmt_num(att$cinema_yes_pct_weighted)
att$n_frequency_valid_among_yes <- fmt_int(att$n_frequency_valid_among_yes)
att$n_frequency_regular <- fmt_int(att$n_frequency_regular)
att$frequency_regular_pct_unweighted <- fmt_num(att$frequency_regular_pct_unweighted)
att$frequency_regular_pct_weighted <- fmt_num(att$frequency_regular_pct_weighted)
att$n_frequency_once_or_more <- fmt_int(att$n_frequency_once_or_more)
att$frequency_once_or_more_pct_unweighted <- fmt_num(att$frequency_once_or_more_pct_unweighted)
att$frequency_once_or_more_pct_weighted <- fmt_num(att$frequency_once_or_more_pct_weighted)

catprof <- profile[profile$variable %in% c("H01_01", "I07_01"), , drop = FALSE]
catprof$year <- as.character(catprof$year)
catprof$n <- fmt_int(catprof$n)
catprof$percent_unweighted <- fmt_num(catprof$percent_unweighted)
catprof$percent_weighted <- fmt_num(catprof$percent_weighted)

contprof <- profile[profile$variable %in% c("H01_02", "DEN_NA_DUSHU", "DOX_mean"), , drop = FALSE]
contprof$year <- as.character(contprof$year)
for (nm in c("n", "mean_unweighted", "sd_unweighted", "median_unweighted", "mean_weighted", "median_weighted", "q25_weighted", "q75_weighted")) {
  if (nm == "n") contprof[[nm]] <- fmt_int(contprof[[nm]]) else contprof[[nm]] <- fmt_num(contprof[[nm]])
}

rel <- related
rel$year <- as.character(rel$year)
rel$n_selected <- fmt_int(rel$n_selected)
rel$n_binary_valid <- fmt_int(rel$n_binary_valid)
rel$percent_selected_unweighted <- fmt_num(rel$percent_selected_unweighted)
rel$percent_selected_weighted <- fmt_num(rel$percent_selected_weighted)

tex <- c(
  "\\documentclass[11pt]{article}",
  "\\usepackage{fontspec}",
  "\\setmainfont{Arial}",
  "\\usepackage{geometry}",
  "\\geometry{margin=1.8cm}",
  "\\usepackage{longtable,booktabs,array,pdflscape}",
  "\\usepackage{setspace}",
  "\\setstretch{1.05}",
  "\\setlength{\\parskip}{5pt}",
  "\\setlength{\\parindent}{0pt}",
  "\\begin{document}",
  "\\begin{center}",
  "{\\LARGE\\bfseries Descriptive analysis of cinema-related variables}\\\\[4pt]",
  "2016–2024 survey waves",
  "\\end{center}",
  "\\section*{Cinema attendance and attendee profile}",
  "Attendance percentages and profile statistics for the main sample use the survey weights supplied with each file: \\texttt{KVZV2} in 2016 and \\texttt{KVZV} in 2018–2024. Counts are unweighted. Income values are nominal monthly rubles.",
  "\\begin{landscape}\\small",
  make_longtable(
    overview,
    c("year", "attendees", "cinema_share", "mean_age", "women", "tertiary", "percap_median", "household_median", "regular"),
    c("Year", "Attendees, n", "Cinema yes, weighted %", "Mean age", "Women, weighted %", "Education codes 1–4, weighted %", "Median per-capita income", "Median household income", "Regular visits among attendees, weighted %"),
    c("1.0cm", "1.7cm", "2.0cm", "1.5cm", "1.7cm", "2.2cm", "2.0cm", "2.0cm", "2.2cm")
  ),
  "\\end{landscape}",
  "The weighted share reporting cinema attendance was lowest in 2020 and highest in 2024. Attendees' mean age ranged from 34.7 to 37.2 years. Women made up about 53–56 percent of attendees. Specialist/master's education was the largest single category in every wave.",
  "Income amounts are nominal and are not adjusted for inflation:",
  "\\clearpage",
  "\\section*{Income means}",
  "\\begin{tabular}{rrr}\\toprule Year & Mean per-capita income & Mean household income \\\\ \\midrule",
  paste0(overview$year, " & ", vapply(overview$year, function(y) fmt_int(get_cont(y, "DEN_NA_DUSHU", "mean_weighted")), character(1)), " & ", vapply(overview$year, function(y) fmt_int(get_cont(y, "DOX_mean", "mean_weighted")), character(1)), " \\\\"),
  "\\bottomrule\\end{tabular}",
  "\\section*{Other cinema-related variables}",
  "For 2016–2022, child cinema and film-download variables are stored as selected response options. Their positive codes are counts of selections; missing values are not interpreted as ‘no’. The 2024 files use 0/1 coding, so percentages are computed among non-missing 0/1 records. The child question applies only to the relevant child module.",
  "The frequency variable \\texttt{I09\\_03\\_01\\_02} is available in 2024 as well as 2016–2022: code 3 is regular attendance and code 4 is one or several visits. This corrects an earlier note that the variable was absent in 2024.",
  "\\begin{landscape}\\small",
  make_longtable(
    rel,
    c("year", "sample", "item", "variable", "n_selected", "n_binary_valid", "percent_selected_unweighted", "percent_selected_weighted", "coding_note"),
    c("Year", "Sample", "Item", "Variable", "Selected n", "Valid 0/1 n", "Selected, raw %", "Selected, weighted %", "Coding / denominator"),
    c("0.8cm", "1.5cm", "1.7cm", "1.8cm", "1.2cm", "1.3cm", "1.5cm", "1.5cm", "5.3cm")
  ),
  "\\end{landscape}",
  "\\clearpage",
  "\\section*{Appendix: attendance and visit frequency by file}",
  "\\begin{landscape}\\scriptsize",
  make_longtable(
    att,
    c("year", "sample", "file", "n_total", "n_attendance_valid", "n_cinema_yes", "n_cinema_no", "cinema_yes_pct_unweighted", "cinema_yes_pct_weighted", "n_frequency_regular", "frequency_regular_pct_weighted", "n_frequency_once_or_more", "frequency_once_or_more_pct_weighted"),
    c("Year", "Sample", "File", "Rows", "Valid cinema answer", "Yes n", "No n", "Yes raw %", "Yes weighted %", "Regular n", "Regular weighted %", "Once or more n", "Once or more weighted %"),
    c("0.7cm", "1.2cm", "2.1cm", "0.9cm", "1.2cm", "0.9cm", "0.9cm", "1.0cm", "1.1cm", "0.9cm", "1.1cm", "1.1cm", "1.2cm")
  ),
  "\\end{landscape}",
  "\\clearpage",
  "\\section*{Appendix: sex and education among cinema attendees}",
  "Percentages are shown both unweighted and weighted where the main-sample weight is available. Education codes 1–4 are grouped only in the overview; all source categories appear here.",
  "\\begin{landscape}\\small",
  make_longtable(
    catprof,
    c("year", "sample", "variable", "category_code", "category", "n", "percent_unweighted", "percent_weighted"),
    c("Year", "Sample", "Variable", "Code", "Category", "n", "Raw %", "Weighted %"),
    c("0.8cm", "1.1cm", "1.5cm", "0.7cm", "9.5cm", "1.2cm", "1.4cm", "1.5cm")
  ),
  "\\end{landscape}",
  "\\clearpage",
  "\\section*{Appendix: age and income among cinema attendees}",
  "\\begin{landscape}\\small",
  make_longtable(
    contprof,
    c("year", "sample", "variable", "n", "mean_unweighted", "sd_unweighted", "median_unweighted", "mean_weighted", "median_weighted", "q25_weighted", "q75_weighted"),
    c("Year", "Sample", "Variable", "n", "Mean raw", "SD raw", "Median raw", "Mean weighted", "Median weighted", "Weighted Q1", "Weighted Q3"),
    c("0.8cm", "1.1cm", "2.3cm", "1.0cm", "1.4cm", "1.2cm", "1.4cm", "1.5cm", "1.5cm", "1.3cm", "1.3cm")
  ),
  "\\end{landscape}",
  "\\clearpage",
  "Main and additional samples are not pooled. Additional samples are listed separately and unweighted because the available weight field is incomplete in those files. The analysis is descriptive and does not account for clustering or stratification, and it does not establish causal relationships.",
  "\\end{document}"
)

tex_path <- file.path(tempdir(), "cinema_descriptive_analysis.tex")
writeLines(tex, tex_path, useBytes = TRUE)
out_dir <- normalizePath("03_outputs")
log_path <- file.path(tempdir(), "cinema_xelatex.log")
status <- system2(
  Sys.which("xelatex"),
  args = c("-interaction=nonstopmode", "-halt-on-error", paste0("-output-directory=", out_dir), tex_path),
  stdout = log_path, stderr = log_path
)
if (!identical(status, 0L)) {
  cat(readLines(log_path, warn = FALSE), sep = "\n")
  stop("XeLaTeX failed to generate the PDF")
}
cat("Created 03_outputs/cinema_descriptive_analysis.pdf\n")
