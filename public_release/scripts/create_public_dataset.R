#!/usr/bin/env Rscript

# Build a public, de-identified survey dataset for repository sharing.
# Conservative defaults are used to reduce re-identification risk.

input_path <- "data/processed/df_filtered.csv"
output_dir <- "public_release/data"
output_data <- file.path(output_dir, "survey_anonymized.csv")
output_codebook <- file.path(output_dir, "codebook_public.csv")
output_report <- file.path(output_dir, "anonymization_report.txt")

if (!file.exists(input_path)) {
  stop(paste("Input file not found:", input_path))
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

df <- read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)

original_ncol <- ncol(df)
original_nrow <- nrow(df)

# Drop free-text fields and "other specify" fields by name pattern.
drop_patterns <- c("_TEXT$", "_other$", "_other\\b")
drop_idx <- rep(FALSE, ncol(df))
for (pat in drop_patterns) {
  drop_idx <- drop_idx | grepl(pat, names(df), ignore.case = TRUE)
}
pattern_drop_cols <- names(df)[drop_idx]

df_public <- df[!drop_idx]

# Detect and drop columns that appear to contain direct identifiers or narrative text.
is_email <- function(x) grepl("@", x) & grepl("\\.", x)
is_url <- function(x) grepl("https?://|www\\.", x, ignore.case = TRUE)
detect_drop <- logical(ncol(df_public))

for (i in seq_along(df_public)) {
  x_raw <- as.character(df_public[[i]])
  x <- trimws(x_raw)
  x <- x[!(is.na(x) | x == "" | x %in% c("NA", "N/A", "NaN"))]
  if (length(x) == 0) next

  # Direct identifier hints.
  has_email <- any(is_email(x))
  has_url <- any(is_url(x))

  # Narrative-text hints: long average text and high uniqueness.
  avg_len <- mean(nchar(x))
  unique_ratio <- length(unique(x)) / length(x)
  looks_free_text <- avg_len > 45 && unique_ratio > 0.4

  # Name-like fields: very high uniqueness, short text, mostly alphabetic.
  alpha_ratio <- mean(grepl("^[A-Za-z .'-]+$", x))
  looks_name_like <- unique_ratio > 0.9 && avg_len < 20 && alpha_ratio > 0.8

  if (has_email || has_url || looks_free_text || looks_name_like) {
    detect_drop[i] <- TRUE
  }
}

detected_drop_cols <- names(df_public)[detect_drop]
df_public <- df_public[!detect_drop]
drop_cols <- c(pattern_drop_cols, detected_drop_cols)

# Quasi-identifiers to coarsen if present.
quasi_id_cols <- c("Q1", "Q2", "Q8", "lmic", "Q3_national", "Q3_sub_national", "Q3_field")
k_threshold <- 5L

coarsened <- character(0)
for (col in quasi_id_cols) {
  if (!(col %in% names(df_public))) next
  x <- trimws(as.character(df_public[[col]]))
  x[x %in% c("", "NA", "N/A", "NaN")] <- NA
  tab <- table(x, useNA = "no")
  rare <- names(tab)[tab < k_threshold]
  if (length(rare) > 0) {
    x[x %in% rare] <- "WITHHELD_SMALL_CELL"
    coarsened <- c(coarsened, col)
  }
  df_public[[col]] <- x
}

# Remove exact duplicate rows to reduce accidental respondent traceability.
dedup_before <- nrow(df_public)
df_public <- unique(df_public)
dedup_removed <- dedup_before - nrow(df_public)

write.csv(df_public, output_data, row.names = FALSE, na = "")

codebook <- data.frame(
  variable = names(df_public),
  data_type = vapply(df_public, function(x) class(x)[1], character(1)),
  non_missing_n = vapply(df_public, function(x) sum(!is.na(x) & trimws(as.character(x)) != ""), integer(1)),
  unique_n = vapply(df_public, function(x) length(unique(x[!is.na(x)])), integer(1)),
  stringsAsFactors = FALSE
)
write.csv(codebook, output_codebook, row.names = FALSE)

report_lines <- c(
  "Public Dataset Anonymization Report",
  "===================================",
  paste("Input:", input_path),
  paste("Rows (original):", original_nrow),
  paste("Columns (original):", original_ncol),
  paste("Columns dropped:", length(drop_cols)),
  paste("Dropped columns:", ifelse(length(drop_cols) > 0, paste(unique(drop_cols), collapse = ", "), "None")),
  paste("Columns coarsened (k<5):", ifelse(length(coarsened) > 0, paste(unique(coarsened), collapse = ", "), "None")),
  paste("Duplicate rows removed:", dedup_removed),
  paste("Rows (public):", nrow(df_public)),
  paste("Columns (public):", ncol(df_public)),
  "",
  "Manual checks required before release:",
  "1. Confirm no remaining direct identifiers (names, emails, institutions).",
  "2. Confirm no country/profession combinations produce unique individuals.",
  "3. Confirm all qualitative quotes are excluded from public files."
)
writeLines(report_lines, con = output_report)

message("Wrote: ", output_data)
message("Wrote: ", output_codebook)
message("Wrote: ", output_report)
