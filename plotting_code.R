# Consolidated plotting code
# Includes helper functions and all figure-generation logic



split_responses <- function(df, col_name) {
  # Split the responses in the specified column
  df$temp <- sapply(df[[col_name]], function(x) as.character(unlist(strsplit(x, ','))))
  
  # Create a temporary data frame
  xx <- do.call(rbind, apply(df, 1, function(x) data.frame(x$temp)))
  xx$row <- rownames(xx)
  xx$value <- 1
  
  # Keep all values before "."
  xx$row <- sub("\\..*", "", xx$row)
  
  # Reshape the data
  xx <- spread(xx, key = x.temp, value = value)
  
  # Rename columns
  col_names <- names(xx)
  n_index <- 2
  m_index <- ncol(xx)
  selected_cols <- col_names[n_index:m_index]
  new_col_names <- paste0(col_name, ".", selected_cols)
  names(xx)[n_index:m_index] <- new_col_names
  
  # Merge with original data frame
  df <- merge(df, xx, by = "row", all.x = TRUE)
  
  # Remove temporary columns
  df$temp <- NULL
  return(df)
}

#functions to analyze survey data

summarize_columns <- function(df, group_col, summary_cols) {
  # Initialize an empty data frame to store results
  summary_df <- data.frame()
  
  # Loop through each unique value in the grouping column
  for (group_val in unique(df[[group_col]])) {
    # Filter the data for the current group value
    temp_df <- df %>% filter(.data[[group_col]] == group_val)
    
    # Calculate the number of rows in the filtered data
    n_rows <- nrow(temp_df)
    
    # Calculate the number of non-NA values for each summary column
    col_sums <- colSums(!is.na(temp_df[, summary_cols]))
    
    # Get unique values from each summary column (assuming one per column)
    unique_values <- sapply(temp_df[, summary_cols], function(x) {
      unique_x <- unique(x[!is.na(x)])
      if (length(unique_x) == 0) { 
        return("Missing") # Or another placeholder
      } else {
        return(unique_x)
      }
    })
    
    # Combine the group value, number of rows, and column sums into a vector
    summary_vector <- c(group_val, n_rows, col_sums)
    
    # Add the vector as a new row to the summary data frame
    summary_df <- rbind(summary_df, summary_vector)
  }
  
  # Set the column names of the summary data frame
  names(summary_df) <- c(group_col, "n", unique_values)
  
  # Convert the relevant columns to numeric
  summary_df[, c(2:ncol(summary_df))] <- lapply(summary_df[, c(2:ncol(summary_df))], as.numeric)
  
  # Calculate proportions by dividing each summary column by 'n'
  for (col_name in unique_values) {
    summary_df[[col_name]] <- summary_df[[col_name]] / summary_df$n 
  }
  
  # Return the data frame with proportions
  return(summary_df)
}


summarize_columns2 <- function(df, group_col, summary_cols) {
  # Initialize an empty data frame to store results
  summary_df <- data.frame()
  
  # Loop through each unique value in the grouping column
  for (group_val in unique(df[[group_col]])) {
    # Filter the data for the current group value
    temp_df <- df %>% filter(.data[[group_col]] == group_val)
    
    # Calculate the number of rows in the filtered data
    n_rows <- nrow(temp_df)
    
    # Calculate the number of non-NA values for each summary column
    col_sums <- colSums(!is.na(temp_df[, summary_cols, drop = FALSE]))
    
    # Get unique values from each summary column
    unique_values <- sapply(temp_df[, summary_cols, drop = FALSE], function(x) {
      unique_x <- unique(x[!is.na(x)])
      if (length(unique_x) == 0) {
        return("Missing")
      } else {
        return(unique_x)
      }
    })
    
    # Keep only valid columns
    valid_cols <- names(unique_values)[unique_values != "Missing"]
    valid_col_sums <- col_sums[valid_cols]
    
    # Build a named vector for this row
    summary_vector <- c(setNames(group_val, group_col), setNames(n_rows, "n"), valid_col_sums)
    
    # Convert to data frame
    summary_row <- as.data.frame(as.list(summary_vector), stringsAsFactors = FALSE)
    
    # Ensure all names match between rows (fill missing columns with NA)
    summary_df <- suppressWarnings(bind_rows(summary_df, summary_row))
  }
  
  # Convert numeric columns
  numeric_cols <- setdiff(names(summary_df), group_col)
  summary_df[numeric_cols] <- lapply(summary_df[numeric_cols], as.numeric)
  
  # Calculate proportions
  for (col_name in numeric_cols) {
    if (col_name != "n") {
      summary_df[[col_name]] <- summary_df[[col_name]] / summary_df$n
    }
  }
  
  return(summary_df)
}

summarize_columns_count <- function(df, group_col, summary_cols, section = NA) {
  # Initialize an empty data frame to store results
  summary_df <- data.frame()
  
  # Loop through each unique value in the grouping column
  for (group_val in unique(df[[group_col]])) {
    # Filter the data for the current group value
    temp_df <- df %>% filter(.data[[group_col]] == group_val)
    
    # Conditionally calculate the number of rows
    if (is.na(section)) {
      n_rows <- nrow(temp_df)
    } else {
      n_rows <- sum(!is.na(temp_df[, section]))
    }
    
    # Calculate the number of non-NA values for each summary column
    col_sums <- colSums(!is.na(temp_df[, summary_cols]))
    
    # Get unique values from each summary column (assuming one per column)
    unique_values <- sapply(temp_df[, summary_cols], function(x) unique(x[!is.na(x)]))
    
    # Combine the group value, number of rows, and column sums into a vector
    summary_vector <- c(group_val, n_rows, col_sums)
    
    # Add the vector as a new row to the summary data frame
    summary_df <- rbind(summary_df, summary_vector)
  }
  
  # Set the column names of the summary data frame
  names(summary_df) <- c(group_col, "n", unique_values)
  
  # Convert the relevant columns to numeric
  summary_df[, c(2:ncol(summary_df))] <- lapply(summary_df[, c(2:ncol(summary_df))], as.numeric)
  
  # Return the data frame with proportions
  return(summary_df)
}

summarize_columns_no_grouping <- function(df, summary_cols) {
  n_rows <- nrow(df)
  
  # Calculate the number of non-NA values for each summary column
  col_sums <- colSums(!is.na(df[, summary_cols]))
  
  # Get unique values from each summary column (assuming one per column)
  unique_values <- sapply(df[, summary_cols], function(x) unique(x[!is.na(x)])) 
  
  # Combine the number of rows and column sums into a vector
  summary_vector <- c(n_rows, col_sums)
  
  summary_df <- data.frame()
  
  # Add the vector as a new row to the summary data frame
  summary_df <- rbind(summary_df, summary_vector)
  
  # Set the column names of the summary data frame
  names(summary_df) <- c("n", unique_values)
  
  # Convert the relevant columns to numeric
  summary_df[, c(1:ncol(summary_df))] <- lapply(summary_df[, c(1:ncol(summary_df))], as.numeric)
  
  # Calculate proportions by dividing each summary column by 'n'
  for (col_name in unique_values) {
    summary_df[[col_name]] <- summary_df[[col_name]] / summary_df$n 
  }
  
  # Return the data frame with proportions
  return(summary_df)
}

summarize_columns_no_grouping_count <- function(df, summary_cols) {
  n_rows <- nrow(df)
  
  # Calculate the number of non-NA values for each summary column
  col_sums <- colSums(!is.na(df[, summary_cols]))
  
  # Get unique values from each summary column (assuming one per column)
  unique_values <- sapply(df[, summary_cols], function(x) unique(x[!is.na(x)])) 
  
  # Combine the number of rows and column sums into a vector
  summary_vector <- c(n_rows, col_sums)
  
  summary_df <- data.frame()
  
  # Add the vector as a new row to the summary data frame
  summary_df <- rbind(summary_df, summary_vector)
  
  # Set the column names of the summary data frame
  names(summary_df) <- c("n", unique_values) 
  
  # Convert the relevant columns to numeric
  summary_df[, c(1:ncol(summary_df))] <- lapply(summary_df[, c(1:ncol(summary_df))], as.numeric)
  
  # Return the data frame with counts
  return(summary_df)
}

reorder_factor_levels <- function(df, column_name, last_level) {
  
  df[[column_name]] <- as.factor(df[[column_name]])
  # Get the current levels of the factor
  levels_f <- levels(df[[column_name]])
  
  # Remove the specified level from the current levels
  levels_f <- levels_f[levels_f != last_level]
  
  # Add the specified level back at the end
  levels_f <- c(levels_f, last_level)
  
  # Re-factor the column with the new level order
  df[[column_name]] <- factor(df[[column_name]], levels = levels_f)
  
  # Return the modified data frame
  return(df)
}

# Reusable function for summarizing frequency and percent
summarise_freq_percent <- function(data, group_var) {
  data %>%
    filter(!is.na(StartDate)) %>%
    group_by({{ group_var }}) %>%
    summarise(Frequency = n(), .groups = "drop") %>%
    mutate(Percent = round(Frequency / sum(Frequency) * 100, 1))
}


# ---- Plotting workflow from plots_revised.Rmd ----


# Load packages for data handling, visualization, and formatting
library(tidyverse)
library(here)
library(readxl)
library(stringr)
library(gt)
library(openxlsx)
library(survival)
library(survminer)
library(psych)
library(viridis)
library(ggtext)
library(gridExtra)
library(RColorBrewer)
library(scales)
library(tidytext)

# Source custom functions

# Set global options
options(stringsAsFactors = FALSE)
options(scipen = 100, digits = 4)


# Load reference data: country-to-region mapping
country_dict <- readxl::read_excel("data/countries_regions.xlsx")

file_path_plots <- "figures"

# Define path to directory containing raw survey CSVs
directory_path <- "data/raw"

# Identify all CSV files in the directory
csv_files <- list.files(path = directory_path, pattern = "\\.csv$")

# Load each CSV and store in a list of data frames
data_list <- lapply(csv_files, function(file) {
  read_csv(file.path(directory_path, file))
})

# Combine all individual survey CSVs into one data frame
df <- bind_rows(data_list)

df[df == ""] <- NA
df[df == "#N/A"] <- NA
df[df == "Other. Please specify:"] <- "Other"
df[df == "None"] <- NA

# Remove metadata rows (first two) from the combined data frame
df <- df[-c(1, 2), ]

df <- merge(df, country_dict, by.x = "Q8", by.y = "name", all.x = TRUE)

responses_n <- nrow(df)

#if Q1 was not answered, we can assume the person exited the survey without answering questions
drop_outs2 <- df %>% filter(is.na(Q1))
drop_outs_sum <- nrow(drop_outs2)

df <- df %>% filter(!is.na(Q1))
df <- df %>%
  filter(!is.na(df$StartDate))


# Compute completion status for Metrics section (at least 2 responses)
sub_df_metrics <- df[,c(9,56:94)] 

filtered_df <- sub_df_metrics %>%
  mutate(metrics_section_answered = rowSums(!is.na(across(-ResponseId)))) %>%
  filter(metrics_section_answered >= 2) 

filtered_df$metrics_section_answered <- ifelse(filtered_df$metrics_section_answered > 0, 1, 0)

metrics_section <- filtered_df[,c("ResponseId", "metrics_section_answered")]

# Compute completion status for Questions section (at least 2 responses)
sub_df_questions <- df[,c(9,95:160)]

filtered_df <- sub_df_questions %>%
  mutate(questions_section_answered = rowSums(!is.na(across(-ResponseId)))) %>%
  filter(questions_section_answered >= 2) 

filtered_df$questions_section_answered <- ifelse(filtered_df$questions_section_answered > 0, 1, 0)

questions_section <- filtered_df[,c("ResponseId", 
                                          "questions_section_answered")]

# Compute completion status for Evaluation section (at least 2 responses)
sub_df_evaluation <- df[,c(9,161:164)]

filtered_df <- sub_df_evaluation %>%
  mutate(evaluation_section_answered = rowSums(!is.na(across(-ResponseId)))) %>%
  filter(evaluation_section_answered >= 2) 

filtered_df$evaluation_section_answered <- ifelse(filtered_df$evaluation_section_answered > 0, 1, 0)

evaluation_section <- filtered_df[,c("ResponseId", 
                                            "evaluation_section_answered")]

#Confidence section
sub_df_confidence <- df[,c(9,165:174)]

filtered_df <- sub_df_confidence %>%
  mutate(confidence_section_answered = rowSums(!is.na(across(-ResponseId)))) %>%
  filter(confidence_section_answered >= 1) 

filtered_df$confidence_section_answered <- ifelse(filtered_df$confidence_section_answered > 0, 1, 0)

confidence_section <- filtered_df[,c("ResponseId", 
                                            "confidence_section_answered")]

#Barriers section
sub_df_barriers <- df[,c(9,175:186)]

filtered_df <- sub_df_barriers %>%
  mutate(barriers_section_answered = rowSums(!is.na(across(-ResponseId)))) %>%
  filter(barriers_section_answered >= 2) 

filtered_df$barriers_section_answered <- ifelse(filtered_df$barriers_section_answered > 0, 1, 0)
barriers_section <- filtered_df[,c("ResponseId", 
                                        "barriers_section_answered")]


df <- df %>%
  left_join(metrics_section, by = "ResponseId") %>%
  left_join(questions_section, by = "ResponseId") %>%
  left_join(evaluation_section, by = "ResponseId") %>%
  left_join(barriers_section, by = "ResponseId")%>%
  left_join(confidence_section, by = "ResponseId")

# anyone who has not contributed to any policy dialogues around COVID-19 are excluded here
dropout <- nrow(df[(df$Q2 == "No" & df$Q4 == "No"), ])
df <- df[!(df$Q2 == "No" & df$Q4 == "No"), ] 

# summarizing Q2 and Q4 (due to many levels in diagram) - either they engaged in policy dialogues themselves or their supervisor did (Q4)

df$Q3_national <- ifelse(!is.na(df$Q3_1) | 
                              !is.na(df$Q3_7) | 
                              !is.na(df$Q3_2) | 
                              !is.na(df$Q3_6) | 
                              !is.na(df$Q3_3) | 
                              !is.na(df$Q3_4) |
                              !is.na(df$Q5_1) |
                              !is.na(df$Q5_2) |
                              !is.na(df$Q5_7) |
                              !is.na(df$Q5_6) |
                              !is.na(df$Q5_4),1,NA )

df$Q3_sub_national <- ifelse(!is.na(df$Q3_5) | 
                              !is.na(df$Q3_8) | 
                              !is.na(df$Q3_10)|
                              !is.na(df$Q5_5) |
                              !is.na(df$Q5_8) |
                              !is.na(df$Q5_10),1,NA )

df$Q3_field <- ifelse(!is.na(df$Q3_11) | 
                              !is.na(df$Q3_12),1,NA )

df$Q3_other <- ifelse(!is.na(df$Q3_16) | 
                              !is.na(df$Q3_15),1,NA )

# remove columns after aggregation
columns_to_remove <- c("Q3_1", "Q3_7", "Q3_2", "Q3_6", "Q3_3", "Q3_4",
                       "Q5_1", "Q5_2", "Q5_7", "Q5_6", "Q5_4",
                       "Q3_5", "Q3_8", "Q3_10", "Q5_5", "Q5_8", "Q5_10",
                       "Q3_11", "Q3_12","UserLanguage", "DistributionChannel",
                       "Q0","Status", "EndDate")

df <- df[ , -which(names(df) %in% columns_to_remove)] ## works as expected

df <- reorder_factor_levels(df, "Q1", "Other") 

df$q1_group <- ifelse(df$Q1 == "University/Research institution", "University/Research institution", "Other")

df <- df %>%
  mutate(Q1 = str_wrap(Q1, width = 20))

#relocate columns 
df <- df %>% 
  relocate(Q10_4, .before = Q8) %>% 
  relocate(Q10_5, .after = Q10_4) %>% 
  relocate(Q10_6, .after = Q10_5) %>% 
  relocate(income_group, .after = Q8) %>%
  relocate(metrics_section_answered, .after = income_group)  %>%
  relocate(questions_section_answered, .after = metrics_section_answered)%>%
  relocate(evaluation_section_answered, .after = questions_section_answered)%>%
  relocate(confidence_section_answered, .after = evaluation_section_answered)%>%
  relocate(barriers_section_answered, .after = confidence_section_answered) %>%
  relocate(Q3_national, .after = barriers_section_answered)%>%
  relocate(Q3_sub_national, .after = Q3_national)%>%
  relocate(Q3_field, .after = Q3_sub_national)%>%
  relocate(Q3_other, .after = Q3_field)

df$Q14_4 <- ifelse(df$Q14_4 == "Single number / Point estimate", "Point estimates", df$Q14_4)

df <- df %>%
  filter(!is.na(df$ResponseId))



# Clean up income_group
df$income_group <- ifelse(is.na(df$income_group), 
                          "Non-country-specific", 
                          df$income_group)

# Create a trimmed dataset
df_survival <- df[, c("ResponseId",
                      "income_group",
                      "metrics_section_answered",
                      "barriers_section_answered",
                      "questions_section_answered",
                      "evaluation_section_answered",
                      "confidence_section_answered",
                      "q1_group")]

# Define prettier labels for the sections
label_map <- c(
  metrics_section_answered = "Metrics",
  barriers_section_answered = "Barriers",
  questions_section_answered = "Questions",
  evaluation_section_answered = "Evaluation",
  confidence_section_answered = "Confidence"
)

# Set ordered factor levels for income group
df_survival$income_group <- factor(df_survival$income_group,
                                   levels = c("HIC", "UMIC", "LMIC", "LIC", "Non-country-specific"))

# Get total number of 1s per section
section_counts <- df_survival %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "section", values_to = "count")

# Order sections by descending count
section_counts <- section_counts %>%
  arrange(desc(count))

ordered_sections <- section_counts$section

# Sum numeric columns by income group
grouped_sum <- df_survival %>%
  group_by(income_group) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

# Convert to long format for plotting
grouped_sum_long <- grouped_sum %>%
  pivot_longer(-income_group, names_to = "section", values_to = "value")

# Apply descending section order to both datasets
grouped_sum_long$section <- factor(grouped_sum_long$section, levels = ordered_sections)
section_counts$section <- factor(section_counts$section, levels = ordered_sections)

income_group_colors <- c(
  "HIC" = "#66c2a5",
  "UMIC" = "#fc8d62",
  "LMIC" = "#8da0cb",
  "LIC" = "#e78ac3",
  "Non-country-specific" = "#a6d854"
)

# Plot
ggplot(grouped_sum_long, aes(x = section, y = value, color = income_group, group = income_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_text(data = section_counts,
            aes(x = section, y = 46, label = paste0("n = ", count)),
            inherit.aes = FALSE,
            size = 4,
            vjust = -0.5) +
  scale_x_discrete(labels = label_map) +
  scale_color_manual(values = income_group_colors) +  # ✅ consistent colors
  theme_minimal() +
  labs(
    title = "",
    x = "Survey Section",
    y = "Number of Respondents",
    color = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 0),
    legend.position = "bottom"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

ggsave(file.path(file_path_plots, "survey_dropout.pdf"), height = 6, width = 7, units = "in", dpi = 300)

ggsave(
  filename = file.path(file_path_plots, "survey_dropout.pdf"),
  height = 6,
  width = 7,
  units = "in",
  dpi = 300
)


# Wilson score CI (used in summary table)
wilson_ci <- function(x, n, level = 0.95) {
  z <- qnorm(1 - (1 - level) / 2)
  p <- x / n
  denom <- 1 + z^2 / n
  centre <- p + z^2 / (2 * n)
  width <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n)
  lo <- pmax((centre - width) / denom, 0)
  hi <- pmin((centre + width) / denom, 1)
  tibble(prop = p, ci_lo = lo, ci_hi = hi)
}

# ── Q18 QUESTION → CATEGORY MAPPING ──────────────────────────────────
# Q18 is a Qualtrics matrix: Q18_{question}_{phase_suffix}
# *** VERIFY mapping against your Qualtrics codebook ***

question_to_category <- c(
  "1"  = "NPI Impact",
  "2"  = "Herd Immunity & Reopening",
  "3"  = "Herd Immunity & Reopening",
  "4"  = "Epidemic Projections",
  "5"  = "Epidemic Projections",
  "6"  = "Epidemic Projections",
  "7"  = "Epidemic Projections",
  "8"  = "Economic Impact",
  "9"  = "Vaccination Strategies",
  "10" = "Vaccination Strategies",
  "11" = "Healthcare Resource Allocation",
  "12" = "NPI Impact"
)

phase_suffixes <- c("1", "2", "3", "4", "6")
phase_names    <- c("Discovery", "Escalation", "Acceleration",
                    "Deceleration", "Preparation")

categories_fig1 <- c("NPI Impact", "Epidemic Projections",
                      "Healthcare Resource Allocation",
                      "Vaccination Strategies",
                      "Herd Immunity & Reopening",
                      "Economic Impact")

# ── Identify Q18 columns and filter respondents ─────────────────────
q18_cols <- grep("^Q18_\\d+_\\d+$", names(df), value = TRUE)

income_group_order <- c("HIC", "UMIC", "LMIC", "LIC")

df_fig1 <- df %>%
  filter(rowSums(!is.na(across(all_of(q18_cols)))) > 0,
         income_group %in% income_group_order)

# ── Pivot to long ───────────────────────────────────────────────────
long_fig1 <- df_fig1 %>%
  select(ResponseId, income_group, all_of(q18_cols)) %>%
  pivot_longer(cols = all_of(q18_cols),
               names_to = "col_name", values_to = "selected") %>%
  filter(!is.na(selected)) %>%
  mutate(
    qnum         = sub("^Q18_(\\d+)_(\\d+)$", "\\1", col_name),
    phase_suffix = sub("^Q18_(\\d+)_(\\d+)$", "\\2", col_name),
    category     = question_to_category[qnum],
    phase        = phase_names[match(phase_suffix, phase_suffixes)]
  )

# ── Proportion: ≥1 question from category per respondent-phase ──────
presence_fig1 <- long_fig1 %>%
  distinct(ResponseId, income_group, phase, category)

n_per_ig_fig1 <- df_fig1 %>% count(income_group, name = "N_respondents")

counts_fig1 <- presence_fig1 %>%
  group_by(income_group, phase, category) %>%
  summarise(n_selected = n(), .groups = "drop")

plot_fig1 <- expand_grid(
  income_group = income_group_order,
  phase        = phase_names,
  category     = categories_fig1
) %>%
  left_join(counts_fig1, by = c("income_group", "phase", "category")) %>%
  left_join(n_per_ig_fig1, by = "income_group") %>%
  mutate(
    n_selected   = replace_na(n_selected, 0),
    prop         = n_selected / N_respondents,
    phase        = factor(phase, levels = phase_names),
    category     = factor(category, levels = categories_fig1),
    income_group = factor(income_group, levels = income_group_order),
    ig_label     = paste0(income_group, " (n = ", N_respondents, ")")
  )

ig_label_levels <- plot_fig1 %>%
  distinct(income_group, ig_label) %>%
  arrange(income_group) %>%
  pull(ig_label)
plot_fig1$ig_label <- factor(plot_fig1$ig_label, levels = ig_label_levels)

# ── Category styling ────────────────────────────────────────────────
cat_colours <- c(
  "NPI Impact"                      = "#7b3294",
  "Epidemic Projections"            = "#636363",
  "Healthcare Resource Allocation"  = "#2166ac",
  "Vaccination Strategies"          = "#d6604d",
  "Herd Immunity & Reopening"       = "#f1a340",
  "Economic Impact"                 = "#1b7837"
)
cat_shapes    <- c(16, 15, 17, 18, 25, 3)
names(cat_shapes) <- categories_fig1
cat_linetypes <- c("solid", "solid", "dashed", "solid", "dashed", "dotted")
names(cat_linetypes) <- categories_fig1

# ── Plot ────────────────────────────────────────────────────────────
ggplot(plot_fig1,
       aes(x = phase, y = prop, group = category,
           colour = category, shape = category, linetype = category)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5, fill = "white", stroke = 0.5) +
  facet_wrap(~ ig_label, ncol = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_colour_manual(values = cat_colours, name = NULL) +
  scale_shape_manual(values = cat_shapes, name = NULL) +
  scale_linetype_manual(values = cat_linetypes, name = NULL) +
  labs(x = NULL, y = "Proportion of respondents") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x        = element_text(size = 8.5, angle = 30, hjust = 1,
                                       colour = "black"),
    axis.text.y        = element_text(size = 9, colour = "black"),
    axis.title.y       = element_text(size = 10, margin = margin(r = 8)),
    strip.text         = element_text(face = "bold", size = 11),
    legend.position    = "bottom",
    legend.text        = element_text(size = 8.5),
    legend.title       = element_blank(),
    legend.key.width   = unit(1.8, "cm"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90"),
    panel.border       = element_blank(),
    plot.margin        = margin(10, 10, 5, 5)
  ) +
  guides(
    colour   = guide_legend(nrow = 2, byrow = TRUE),
    shape    = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

ggsave(file.path(file_path_plots, "Figure_1.pdf"),
       width = 11, height = 7, dpi = 300)


communicated_how_region <- summarize_columns_count(
  df = df,
  group_col = "income_group", 
  summary_cols = c("Q16_1", "Q16_2", "Q16_3", "Q16_6", 
                   "Q16_7", "Q16_8", "Q16_9", "Q16_10", 
                   "Q16_11", "Q16_12", "Q16_13", "Q16_14"), 
  section = "metrics_section_answered")

names(communicated_how_region) <- gsub("\\(.*?\\)|\\[.*?\\]", "", names(communicated_how_region))
names(communicated_how_region) <- gsub("\\s$", "", names(communicated_how_region)) 
names(communicated_how_region) <- gsub("\\)$", "", names(communicated_how_region))

# Reshape the data from wide to long format
df_long <- communicated_how_region %>% 
  pivot_longer(cols = c(3:14),
               names_to = "way_of_communication",
               values_to = "count") %>%
  group_by(income_group) %>%
  mutate(proportion = count / n) %>%
  ungroup()

df_long <- df_long %>%
  mutate(way_of_communication = str_wrap(way_of_communication, width = 30))

df_long$income_group_label <- paste0(df_long$income_group, " (n = ", df_long$n, ")")

# Step 1-2: Compute overall top 3 and bottom 3
top_bottom_labels <- df_long %>%
  group_by(way_of_communication) %>%
  summarise(avg_prop = sum(count, na.rm = TRUE)) %>%
  arrange(desc(avg_prop)) %>%
  mutate(color_group = case_when(
    row_number() <= 3 ~ "Top 3 Overall",
    row_number() > n() - 3 ~ "Bottom 3 Overall",
    TRUE ~ "Other"
  )) %>%
  select(way_of_communication, color_group)

# Step 3: Join and label
df_long <- df_long %>%
  left_join(top_bottom_labels, by = "way_of_communication") %>%
  mutate(
    income_group = factor(income_group, levels = c("HIC", "UMIC", "LMIC", "LIC", "Non-country-specific")),
    income_group_label = paste0(income_group, " (n = ", n, ")"),
    way_of_communication = reorder_within(way_of_communication, -proportion, income_group)
  ) %>%
  mutate(
    income_group_label = factor(income_group_label, levels = paste0(
      c("HIC", "UMIC", "LMIC", "LIC", "Non-country-specific"),
      " (n = ", n[match(c("HIC", "UMIC", "LMIC", "LIC", "Non-country-specific"), income_group)], ")"
    ))
  )

# Step 4: Plot
ggplot(df_long, aes(x = way_of_communication, y = proportion, fill = color_group)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Top 3 Overall" = "darkgreen", 
                               "Bottom 3 Overall" = "firebrick", 
                               "Other" = "grey70")) +
  labs(x = "Metric", y = "Proportion", fill = "") +
  facet_wrap(. ~ income_group_label, scales = "free_x", ncol = 2) +
  scale_x_reordered() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom", 
        strip.text = element_text(size = 14),
        plot.margin = margin(t = 10, r = 10, b = 30, l = 20))

ggsave(file.path(file_path_plots, "Figure_2.pdf"), width = 13.3, height = 13, dpi = 300)



metrics_counts <- df %>%
  filter(metrics_section_answered > 0) %>%
  group_by(income_group) %>%
  summarise(respondents = n())

Received_format_overall <- summarize_columns_count(
  df = df,
  group_col = "income_group",
  summary_cols = c("Q13_1", "Q13_2", "Q13_6",
                   "Q13_4", "Q13_3", "Q13_5",
                   "Q13_7"),
  section = "metrics_section_answered")

names(Received_format_overall) <- gsub("\\(.*?\\)|\\[.*?\\]", "", names(Received_format_overall))
names(Received_format_overall) <- gsub("\\s$", "", names(Received_format_overall))
names(Received_format_overall) <- gsub("\\)$", "", names(Received_format_overall))

df_long_Received <- Received_format_overall %>%
  pivot_longer(cols = c("Map":"Scientific research paper"),
               names_to = "format", values_to = "count") %>%
  mutate(type = "Received")

Preferred_format_overall <- summarize_columns_count(
  df = df,
  group_col = "income_group",
  summary_cols = c("Q14_1", "Q14_2", "Q14_6",
                   "Q14_4", "Q14_3", "Q14_5",
                   "Q14_7"),
  section = "metrics_section_answered")

names(Preferred_format_overall) <- gsub("\\(.*?\\)|\\[.*?\\]", "", names(Preferred_format_overall))
names(Preferred_format_overall) <- gsub("\\s$", "", names(Preferred_format_overall))
names(Preferred_format_overall) <- gsub("\\)$", "", names(Preferred_format_overall))
names(Preferred_format_overall)[names(Preferred_format_overall) == "Single number / Point estimate"] <- "Point estimates"

df_long_Preferred <- Preferred_format_overall %>%
  pivot_longer(cols = c("Map":"Scientific research paper"),
               names_to = "format", values_to = "count") %>%
  mutate(type = "Preferred")

df_long <- bind_rows(df_long_Received, df_long_Preferred) %>%
  left_join(metrics_counts, by = "income_group") %>%
  filter(income_group %in% c("HIC", "UMIC", "LMIC", "LIC")) %>%
  mutate(
    proportion = count / respondents,
    format     = str_wrap(format, width = 18),
    type       = factor(type, levels = c("Received", "Preferred")),
    income_group = factor(income_group, levels = c("HIC", "UMIC", "LMIC", "LIC")),
    ig_label   = paste0(income_group, " (n = ", respondents, ")")
  )

ig_label_levels <- df_long %>%
  distinct(income_group, ig_label) %>%
  arrange(income_group) %>%
  pull(ig_label)
df_long$ig_label <- factor(df_long$ig_label, levels = ig_label_levels)

# Pivot wide: one row per income_group x format, columns = Received / Preferred
df_wide <- df_long %>%
  select(ig_label, format, type, proportion) %>%
  pivot_wider(names_from = type, values_from = proportion)

# ── Colour by format type (7 formats) ──────────────────────────────
format_colours <- c(
  "Graph"                        = "#1b9e77",
  "Table"                        = "#d95f02",
  "Map"                          = "#7570b3",
  "Policy brief"                 = "#e7298a",
  "Point estimates"              = "#66a61e",
  "Interactive platform"         = "#e6ab02",
  "Scientific research\npaper"   = "#a6761d"
)

format_shapes <- c(16, 17, 15, 18, 8, 3, 4)
names(format_shapes) <- names(format_colours)

ggplot(df_wide, aes(x = Received, y = Preferred,
                    colour = format, shape = format)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60",
              linewidth = 0.4) +
  geom_point(size = 3.5, stroke = 0.5) +
  facet_wrap(~ ig_label, ncol = 2) +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_colour_manual(values = format_colours) +
  scale_shape_manual(values = format_shapes) +
  labs(x = "Proportion received", y = "Proportion preferred",
       colour = NULL, shape = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "bottom",
    legend.text      = element_text(size = 9),
    strip.text       = element_text(face = "bold", size = 11),
    axis.text        = element_text(size = 9, colour = "black"),
    axis.title       = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey90")
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE),
         shape  = guide_legend(nrow = 2, byrow = TRUE))

ggsave(file.path(file_path_plots, "Figure_3.pdf"), width = 10, height = 7, dpi = 300)


# Step 1: Summarize proportions
Q24_prop_region <- summarize_columns_count(
  df = df,
  group_col = "income_group",
  summary_cols = paste0("Q24_", 1:9),
  section = "confidence_section_answered"
)

# Step 2: Reshape and clean — FIX: drop character(0) and Other rows entirely
df_long <- Q24_prop_region %>%
  pivot_longer(cols = 3:11, names_to = "phase", values_to = "count") %>%
  filter(income_group != "Non-country-specific") %>%
  filter(!phase %in% c("character(0)", "Missing", "Other", "")) %>%
  mutate(phase = str_wrap(phase, width = 40))

# Step 3: Define income group labels and plotting variables
ordered_income_groups <- c("HIC", "UMIC", "LMIC", "LIC")

group_sizes <- df_long %>%
  select(income_group, n) %>%
  distinct()

ordered_labels <- paste0(
  ordered_income_groups, 
  " (n = ", group_sizes$n[match(ordered_income_groups, group_sizes$income_group)], ")"
)

df_summary <- df_long %>%
  mutate(
    proportion = count / n,
    label = paste0(round(proportion * 100), "%\n(", count, " / ", n, ")"),
    direction = ifelse(income_group %in% c("HIC", "UMIC"), "left", "right"),
    prop_plot = ifelse(direction == "left", -proportion, proportion),
    income_group_label = paste0(income_group, " (n = ", n, ")")
  ) %>%
  mutate(
    income_group_label = factor(income_group_label, levels = ordered_labels)
  )

# Step 4: Order phases by total proportion
phase_order <- df_summary %>%
  group_by(phase) %>%
  summarise(total_prop = sum(proportion, na.rm = TRUE)) %>%
  arrange(desc(total_prop)) %>%
  pull(phase)

df_summary <- df_summary %>%
  mutate(phase = factor(phase, levels = phase_order))

# Step 5: Define consistent color mapping
fill_colors <- setNames(brewer.pal(n = length(ordered_labels), name = "Set2"), ordered_labels)

# Step 6: Plot
ggplot(df_summary, aes(x = prop_plot, y = phase, fill = income_group_label)) +
  geom_col() +
  geom_vline(xintercept = 0, color = "black") +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    color = "black", size = 4
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = fill_colors) +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 16),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "bottom"
  )

ggsave(file.path(file_path_plots, "Figure_4.pdf"), width = 16, height = 8, dpi = 300)

# Step 1: Summarize responses by income group
Q25_prop_region <- summarize_columns_count(
  df = df, 
  group_col = "income_group", 
  summary_cols = c("Q25_1", "Q25_10", "Q25_4", "Q25_11", "Q25_5", "Q25_12", "Q25_2", "Q25_3", "Q25_7", "Q25_8", "Q25_9")
)

# Step 2: Reshape and clean — FIX: drop character(0)/Missing/Other rows
df_long <- Q25_prop_region %>%
  pivot_longer(cols = 3:13, names_to = "phase", values_to = "count") %>%
  filter(income_group != "Non-country-specific") %>%
  filter(!phase %in% c("character(0)", "Missing", "Other", "")) %>%
  filter(count > 0) %>%
  mutate(phase = str_wrap(phase, width = 62))

# Step 3: Prepare labels and plotting values
ordered_income_groups <- c("HIC", "UMIC", "LMIC", "LIC")

df_summary <- df_long %>%
  mutate(
    proportion = count / n,
    label = paste0(round(proportion * 100), "%\n(", count, " / ", n, ")"),
    direction = ifelse(income_group %in% c("HIC", "UMIC"), "left", "right"),
    prop_plot = ifelse(direction == "left", -proportion, proportion),
    income_group = factor(income_group, levels = ordered_income_groups),
    income_group_label = paste0(income_group, " (n = ", n, ")")
  )

# Step 4: Define label order and phase order
group_sizes <- df_summary %>%
  select(income_group, income_group_label) %>%
  distinct() %>%
  arrange(income_group)

ordered_labels <- group_sizes$income_group_label

df_summary <- df_summary %>%
  mutate(income_group_label = factor(income_group_label, levels = ordered_labels))

phase_order <- df_summary %>%
  group_by(phase) %>%
  summarise(total_prop = sum(proportion, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_prop)) %>%
  pull(phase)

df_summary <- df_summary %>%
  mutate(phase = factor(phase, levels = phase_order)) %>%
  arrange(phase, income_group)

# Step 5: Define consistent color mapping
income_group_colors <- setNames(
  brewer.pal(n = length(ordered_labels), "Set2"),
  ordered_labels
)

# Step 6: Plot
ggplot(df_summary, aes(x = prop_plot, y = phase, fill = income_group_label)) +
  geom_col() +
  geom_vline(xintercept = 0, color = "black") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "black", size = 4) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = income_group_colors) +
  labs(x = NULL, y = NULL, title = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 16),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "bottom"
  )

ggsave(file.path(file_path_plots, "Figure_5.pdf"), width = 15, height = 10, dpi = 300)

# ── Builds a comparison table addressing reviewer request for ────────
# "a summary table of key metrics or barriers to facilitate comparison"

# ── Top 3 metrics per income group ──────────────────────────────────
metrics_long <- communicated_how_region %>%
  pivot_longer(cols = 3:14,
               names_to = "metric", values_to = "count") %>%
  filter(income_group %in% c("HIC", "UMIC", "LMIC", "LIC")) %>%
  mutate(proportion = count / n,
         metric = str_wrap(metric, width = 40))

top_metrics <- metrics_long %>%
  group_by(income_group) %>%
  slice_max(proportion, n = 3) %>%
  summarise(
    `Top 3 valued metrics` = paste0(
      str_trunc(metric, 35), " (", scales::percent(proportion, accuracy = 1), ")",
      collapse = "; "
    ),
    .groups = "drop"
  )

# ── Top 3 confidence factors per income group ───────────────────────
conf_long <- Q24_prop_region %>%
  pivot_longer(cols = 3:11, names_to = "factor_label", values_to = "count") %>%
  filter(income_group %in% c("HIC", "UMIC", "LMIC", "LIC"),
         !factor_label %in% c("character(0)", "Missing", "Other", "")) %>%
  mutate(proportion = count / n)

top_confidence <- conf_long %>%
  group_by(income_group) %>%
  slice_max(proportion, n = 3) %>%
  summarise(
    `Top 3 confidence factors` = paste0(
      str_trunc(factor_label, 40), " (", scales::percent(proportion, accuracy = 1), ")",
      collapse = "; "
    ),
    .groups = "drop"
  )

# ── Top 3 barriers per income group ─────────────────────────────────
barriers_long <- Q25_prop_region %>%
  pivot_longer(cols = 3:13, names_to = "barrier", values_to = "count") %>%
  filter(income_group %in% c("HIC", "UMIC", "LMIC", "LIC"),
         !barrier %in% c("character(0)", "Missing", "Other", ""),
         count > 0) %>%
  mutate(proportion = count / n)

top_barriers <- barriers_long %>%
  group_by(income_group) %>%
  slice_max(proportion, n = 3) %>%
  summarise(
    `Top 3 barriers` = paste0(
      str_trunc(barrier, 40), " (", scales::percent(proportion, accuracy = 1), ")",
      collapse = "; "
    ),
    .groups = "drop"
  )

# ── Combine ─────────────────────────────────────────────────────────
summary_tbl <- top_metrics %>%
  left_join(top_confidence, by = "income_group") %>%
  left_join(top_barriers, by = "income_group") %>%
  rename(`Income group` = income_group)

# Display
summary_tbl %>%
  gt() %>%
  tab_header(
    title = "Summary of key metrics, confidence factors, and barriers by income group"
  ) %>%
  cols_width(
    `Income group` ~ px(80),
    everything() ~ px(280)
  ) %>%
  tab_style(
    style = cell_text(size = "small"),
    locations = cells_body()
  )

# Save as CSV
write_csv(summary_tbl, file.path(file_path_plots, "Summary_table_metrics_barriers.csv"))


# ---- Table 1 dot plot ----

## ============================================================================
## Table 1 — Dot plot (Cleveland-style)
## Reads directly from Qualtrics export CSV
## Proportion selecting each forecast question CATEGORY, by income group
## Pooled across all pandemic phases, with 95% Wilson score CIs
##
## Palette: RColorBrewer "Set2" (matching Figures 1-4)
## ============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(RColorBrewer)
library(readr)

# --------------------------------------------------------------------------
# 1. READ DATA
# --------------------------------------------------------------------------

raw <- read_csv("data/raw/Infectech+-+v1.0_15+February+2026_01.33.csv",
                show_col_types = FALSE)

# Drop Qualtrics header rows (row 1 = sub-header text, row 2 = import IDs)
dat <- raw %>% slice(-(1:2))

# --------------------------------------------------------------------------
# 2. COUNTRY -> INCOME GROUP (World Bank 2023)
# --------------------------------------------------------------------------

wb_income <- c(
  "Argentina" = "UMIC", "Australia" = "HIC", "Austria" = "HIC",
  "Bangladesh" = "LMIC", "Brazil" = "UMIC", "Burkina Faso" = "LIC",
  "Canada" = "HIC", "China" = "UMIC",
  "Congo (the Democratic Republic of the)" = "LIC",
  "Ethiopia" = "LIC", "France" = "HIC",
  "Gambia (the)" = "LIC", "Germany" = "HIC", "Ghana" = "LMIC",
  "India" = "LMIC", "Indonesia" = "LMIC",
  "Iran (Islamic Republic of)" = "UMIC",
  "Ireland" = "HIC", "Kenya" = "LMIC",
  "Korea (the Republic of)" = "HIC", "Kyrgyzstan" = "LMIC",
  "Lao People's Democratic Republic (the)" = "LMIC",
  "Madagascar" = "LIC", "Malawi" = "LIC", "Myanmar" = "LMIC",
  "Nigeria" = "LMIC", "Pakistan" = "LMIC",
  "Palestine, State of" = "LMIC", "Peru" = "UMIC",
  "Philippines (the)" = "LMIC", "Poland" = "HIC",
  "Senegal" = "LMIC", "Singapore" = "HIC", "Somalia" = "LIC",
  "South Africa" = "UMIC", "Sudan (the)" = "LIC",
  "Sweden" = "HIC", "Syrian Arab Republic" = "LIC",
  "Tanzania, United Republic of" = "LMIC", "Thailand" = "UMIC",
  "Timor-Leste" = "LMIC", "Tunisia" = "LMIC", "Uganda" = "LIC",
  "United Kingdom of Great Britain and Northern Ireland (the)" = "HIC",
  "United States of America (the)" = "HIC",
  "Zambia" = "LMIC", "Zimbabwe" = "LMIC"
)

dat$income_group <- wb_income[dat$Q8]

# --------------------------------------------------------------------------
# 3. Q18 QUESTION -> CATEGORY MAPPING
#    Q18 is a Qualtrics matrix: Q18_{question}_{phase}
#    Question numbers were mapped to text by cross-referencing response
#    counts against the Table 1 values in the manuscript.
#
#    *** VERIFY THIS MAPPING against your Qualtrics codebook ***
#    If any assignment is wrong, adjust the vectors below.
# --------------------------------------------------------------------------

# Human-readable question labels (for reference / optional individual-level plot)
question_labels <- c(
  "1"  = "Effectiveness of NPIs / implementing restrictions",
  "2"  = "Outcomes of lifting restrictions at different time points",
  "3"  = "Projected timeline for herd immunity / reopening schools",
  "4"  = "Projected number of cases and deaths",
  "5"  = "When is the peak of the epidemic expected",
  "6"  = "Expected duration of the epidemic wave",
  "7"  = "How do forecasted infection rates vary across regions",
  "8"  = "Economic impacts of prolonged lockdowns",
  "9"  = "Most effective strategy for prioritizing vaccine distribution",
  "10" = "How many vaccines do we need",
  "11" = "Ventilators, hospital and ICU beds needed",
  "12" = "How will changes in public behavior impact the epidemic"
)

# Map each question to the 6 Table 1 colour-coded categories
question_to_category <- c(
  "1"  = "NPI Impact",
  "2"  = "Herd Immunity & Reopening",
  "3"  = "Herd Immunity & Reopening",
  "4"  = "Epidemic Projections",
  "5"  = "Epidemic Projections",
  "6"  = "Epidemic Projections",
  "7"  = "Epidemic Projections",
  "8"  = "Economic Impact",
  "9"  = "Vaccination Strategies",
  "10" = "Vaccination Strategies",
  "11" = "Healthcare Resource Allocation",
  "12" = "NPI Impact"
)

# Phase column suffixes: 1=Discovery, 2=Escalation, 3=Acceleration,
#                         4=Deceleration, 6=Preparation
phase_suffixes <- c("1", "2", "3", "4", "6")
phase_names    <- c("Discovery", "Escalation", "Acceleration",
                    "Deceleration", "Preparation")

# --------------------------------------------------------------------------
# 4. RESHAPE TO LONG FORMAT
# --------------------------------------------------------------------------

q18_cols <- grep("^Q18_\\d+_\\d+$", names(dat), value = TRUE)

# Keep respondents with any Q18 data and a valid income group
dat$has_q18 <- rowSums(!is.na(dat[q18_cols])) > 0
d <- dat %>% filter(has_q18, !is.na(income_group))

cat("Respondents included:", nrow(d), "\n")
cat("By income group:\n")
print(table(d$income_group))

# Pivot to long: one row per respondent x question x phase selection
long <- d %>%
  select(ResponseId, income_group, all_of(q18_cols)) %>%
  pivot_longer(
    cols = all_of(q18_cols),
    names_to  = "col_name",
    values_to = "selected"
  ) %>%
  filter(!is.na(selected)) %>%
  mutate(
    qnum         = sub("^Q18_(\\d+)_(\\d+)$", "\\1", col_name),
    phase_suffix = sub("^Q18_(\\d+)_(\\d+)$", "\\2", col_name),
    category     = question_to_category[qnum],
    question     = question_labels[qnum],
    phase        = phase_names[match(phase_suffix, phase_suffixes)]
  )

# --------------------------------------------------------------------------
# 5. COMPUTE PROPORTIONS + WILSON SCORE CIs
#
#    Denominator = N_respondents x 5 phases (pooled across phases).
#    For each respondent-phase, we check if at least one question from
#    the category was selected (binary: yes/no). This avoids proportions
#    exceeding 100% when categories contain multiple questions.
#
#    Wilson score interval is recommended for binomial proportions with
#    small samples. Unlike the Wald interval (p +/- z*SE), the Wilson:
#      - Never produces values outside [0, 1]
#      - Has better coverage when p is near 0 or 1
#      - Is appropriate for the small subgroup sizes here (n = 12-39)
#
#    Formula:
#      CI = (p + z^2/2n +/- z * sqrt(p(1-p)/n + z^2/4n^2)) / (1 + z^2/n)
# --------------------------------------------------------------------------

n_per_ig <- d %>%
  count(income_group, name = "N_respondents")

# For each respondent x phase, check if AT LEAST ONE question from each
# category was selected. This avoids proportions >100% when categories
# contain multiple questions.
presence <- long %>%
  distinct(ResponseId, income_group, phase, category) %>%
  group_by(income_group, category) %>%
  summarise(n_present = n(), .groups = "drop")

# Full grid + merge
plot_df <- n_per_ig %>%
  crossing(category = unique(question_to_category)) %>%
  left_join(presence, by = c("income_group", "category")) %>%
  mutate(
    n_present = replace_na(n_present, 0),
    total_N   = N_respondents * length(phase_suffixes),
    prop      = n_present / total_N
  )

# Wilson CI
z <- qnorm(0.975)

plot_df <- plot_df %>%
  mutate(
    ci_lo = (prop + z^2 / (2 * total_N) -
               z * sqrt((prop * (1 - prop) + z^2 / (4 * total_N)) / total_N)) /
      (1 + z^2 / total_N),
    ci_hi = (prop + z^2 / (2 * total_N) +
               z * sqrt((prop * (1 - prop) + z^2 / (4 * total_N)) / total_N)) /
      (1 + z^2 / total_N),
    ci_lo = pmax(ci_lo, 0),
    ci_hi = pmin(ci_hi, 1)
  )

# --------------------------------------------------------------------------
# 6. FACTOR LEVELS AND LABELS
# --------------------------------------------------------------------------

category_order <- c(
  "NPI Impact",
  "Epidemic Projections",
  "Healthcare Resource Allocation",
  "Vaccination Strategies",
  "Herd Immunity & Reopening",
  "Economic Impact"
)

ig_order <- c("HIC", "UMIC", "LMIC", "LIC")

# Build ig_label factor with correct n for each group
ig_label_levels <- paste0(
  ig_order, " (n = ",
  n_per_ig$N_respondents[match(ig_order, n_per_ig$income_group)], ")"
)

plot_df <- plot_df %>%
  mutate(
    category     = factor(category, levels = category_order),
    income_group = factor(income_group, levels = ig_order),
    ig_label     = paste0(income_group, " (n = ", N_respondents, ")"),
    ig_label     = factor(ig_label, levels = ig_label_levels)
  )

# --------------------------------------------------------------------------
# 7. PLOT
# --------------------------------------------------------------------------

set2_cols <- brewer.pal(4, "Set2")
names(set2_cols) <- ig_label_levels

ig_shapes <- c(16, 17, 15, 18)
names(ig_shapes) <- ig_label_levels

p <- ggplot(plot_df,
            aes(x = prop, y = fct_rev(category),
                colour = ig_label, shape = ig_label)) +
  geom_pointrange(
    aes(xmin = ci_lo, xmax = ci_hi),
    position = position_dodge(width = 0.65),
    size = 0.6, linewidth = 0.5, fatten = 3
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, NA),
    breaks = seq(0, 1, 0.25),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_colour_manual(values = set2_cols, name = NULL) +
  scale_shape_manual(values = ig_shapes, name = NULL) +
  labs(x = "Proportion of respondent-phases", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y        = element_text(size = 10, colour = "black"),
    axis.text.x        = element_text(size = 9, colour = "black"),
    axis.title.x       = element_text(size = 10, margin = margin(t = 8)),
    legend.position    = "bottom",
    legend.text        = element_text(size = 9),
    legend.key.size    = unit(0.8, "lines"),
    legend.spacing.x   = unit(0.4, "cm"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted", colour = "grey80"),
    panel.grid.major.x = element_line(colour = "grey90"),
    panel.border       = element_blank(),
    plot.margin        = margin(10, 15, 5, 5)
  ) +
  guides(colour = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))
