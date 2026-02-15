# Includes helper functions, survey data prep, and statistical analyses



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


# ---- Analysis workflow from analysis.Rmd ----


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

# Source custom functions

# Set global options
options(stringsAsFactors = FALSE)
options(scipen = 100, digits = 4)


# Load reference data: country-to-region mapping
country_dict <- readxl::read_excel("data/countries_regions.xlsx")

# Define path to directory containing raw survey CSVs
directory_path <- "data"

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

sub_df <- df %>% filter(if_any(c(Q7_3, Q7_5, Q7_6), ~ !is.na(.)))

sub_df <- sub_df %>%
  filter(!is.na(sub_df$Q8))

#identifiable interview participants & remove those that can't be interviewed
df$identifiable <- ifelse(!is.na(df$Q10_6), 1, 0)
df_interviewees <- df %>% filter(!is.na(Q10_6))

write.xlsx(df_interviewees, file = "~/Library/CloudStorage/GoogleDrive-paula.christen@cema.africa/My Drive/Infectech/survey/data/processed/interview_participants_14Jan25.xlsx") 

df_qual_analysis <- df[,c('ResponseId',
                          'identifiable',
                          'Q8',
                          'income_group',
                          'metrics_section_answered',
                          'questions_section_answered',
                          'evaluation_section_answered',
                          'confidence_section_answered',
                          'barriers_section_answered',
                          'Q3_national',
                          'Q3_sub_national',
                          'Q3_field',
                          'Q3_other',
                          'Q1',
                          'q1_group',
                          'Q17#1_1_1',
                          'Q17#1_2_1',
                          'Q17#1_3_1',
                          'Q17#1_4_1',
                          'Q17#1_5_1',
                          'Q19',
                          'Q21',
                          'Q23')]

# Identify columns with qual info
cols_to_check <- which(names(df_qual_analysis) == "Q17#1_1_1"):which(names(df_qual_analysis) == "Q23")

# Remove rows with all NAs in the specified columns
df_filtered <- df_qual_analysis[!apply(df_qual_analysis[, cols_to_check], 1, function(row) all(is.na(row))), ]

df <- df %>%
  filter(!is.na(df$ResponseId))


Q1_df <- df %>%
  filter(!is.na(df$StartDate)) %>%
  dplyr::filter(!is.na(Q1)) %>%
  dplyr::group_by(q1_group) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) 

Q1_df_sum <- Q1_df %>%
  dplyr::arrange(desc(q1_group)) %>%
  dplyr::mutate(Position = round((cumsum(Percent)- 0.5*Percent),0))

#Overview of study participants in survey
Q1_df <- df %>%
  filter(!is.na(df$StartDate)) %>%
  dplyr::filter(!is.na(Q1)) %>%
  dplyr::group_by(Q1) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1)) 

# Create a contingency table
tab <- table(df$Q1, df$income_group)

# Calculate row-wise percentages
tab_percent <- prop.table(tab, margin = 1) * 100

# Combine counts and percentages into a single table
formatted_table <- matrix(
  paste0(tab, " (", sprintf("%.1f", tab_percent), "%)"),
  nrow = nrow(tab),
  dimnames = dimnames(tab)
)

# Print as a data frame for nicer formatting
table <- as.data.frame.matrix(formatted_table)
table$org <- row.names(table)


# Q3 - who they engaged with
national <- summarise_freq_percent(df, Q3_national)
subnational <- summarise_freq_percent(df, Q3_sub_national)
field <- summarise_freq_percent(df, Q3_field)

# Q7 - geographic scope of work
global <- summarise_freq_percent(df, Q7_1)
regional <- summarise_freq_percent(df, Q7_2)
national_scope <- summarise_freq_percent(df, Q7_3)
sub_national <- summarise_freq_percent(df, Q7_5)
field_scope <- summarise_freq_percent(df, Q7_6)

# Derived variable: engaged at national/subnational/field level
df$field_national_sn <- ifelse(!is.na(df$Q7_3) | 
                               !is.na(df$Q7_5) | 
                               !is.na(df$Q7_6), 1, 0)

# Summary of field_national_sn
field_national_sn_summary <- summarise_freq_percent(df, field_national_sn)



communicated_how_region <- summarize_columns_no_grouping_count(
  df = df, 
  summary_cols = c("Q16_1", "Q16_2", "Q16_3", "Q16_6", "Q16_7", "Q16_8", "Q16_9", "Q16_10", "Q16_11", "Q16_12", "Q16_13", "Q16_14")
)

names(communicated_how_region) <- gsub("\\(.*?\\)|\\[.*?\\]", "", names(communicated_how_region))
names(communicated_how_region) <- gsub("\\s$", "", names(communicated_how_region)) 
names(communicated_how_region) <- gsub("\\)$", "", names(communicated_how_region))

# Reshape the data from wide to long format
df_long <- communicated_how_region %>% 
  pivot_longer(cols = c(2:13),
               names_to = "way_of_communication",
               values_to = "count") 

df_long <- df_long %>%
  mutate(way_of_communication = str_wrap(way_of_communication, width = 20))

df_long


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
  mutate(way_of_communication = str_wrap(way_of_communication, width = 20))

# Order the phases by mean proportion in descending order
phase_order <- df_long %>%
  arrange(desc(proportion)) %>% 
  pull(way_of_communication)

df_long



# Create an empty data frame to store the results
df_matrix_no_grouping <- data.frame()

# Loop through each question from Q18_1 to Q18_12
for (i in 1:12) {
  # Construct the column names for the current question
  col_names <- paste0("Q18_", i, "_", c(1, 2, 3, 4, 6))
  
  Q18_prop <- summarize_columns_no_grouping_count(
      df = df,
      summary_cols = col_names)
    
  # Add a column for the Q18 question number
  Q18_prop$Q18_number <- i
    
  # Append the results to df_matrix
  df_matrix_no_grouping <- rbind(df_matrix_no_grouping, Q18_prop)
}

df_matrix_no_grouping$Q18_category <- ifelse(df_matrix_no_grouping$Q18_number %in% c(1,2,12), 
                                      "Non-Pharmaceutical Interventions and their Impact",
                                             ifelse(df_matrix_no_grouping$Q18_number %in% c(4,5,6,7), 
                                                    "Epidemic Projections and Modeling",
                                                    ifelse(df_matrix_no_grouping$Q18_number %in% c(3), 
                                                           "Herd Immunity and Reopening Strategies",
                                                           ifelse(df_matrix_no_grouping$Q18_number == 11, 
                                                                  "Healthcare Resource Allocation",
                                                                  ifelse(df_matrix_no_grouping$Q18_number %in% c(9,10), 
                                                                         "Vaccination Strategies",
                                                                         ifelse(df_matrix_no_grouping$Q18_number == 8, 
                                                                                "Economic Impact", 
                                                                                "Other"))))))

df_matrix_no_grouping$Q18_number <- ifelse(df_matrix_no_grouping$Q18_number == 1, 
                   "What is the effectiveness of different types of Non-pharmaceutical interventions? What are the potential outcomes of implementing restrictions at different time points?",
                   ifelse(df_matrix_no_grouping$Q18_number == 2,
                          "What are the potential outcomes of lifting restrictions at different points in time?",
                          ifelse(df_matrix_no_grouping$Q18_number == 3,
                                 "What is the projected timeline for achieving herd immunity? At what level of herd immunity can we safely reopen schools?",
                                 ifelse(df_matrix_no_grouping$Q18_number == 4,
                                        "What is the projected number of cases and deaths over the next x months?",
                                        ifelse(df_matrix_no_grouping$Q18_number == 5,
                                               "When is the peak of the epidemic expected to occur?",
                                               ifelse(df_matrix_no_grouping$Q18_number == 6,
                                                      "What is the expected duration of the epidemic wave?",
                                                      ifelse(df_matrix_no_grouping$Q18_number == 7,
                                                             "How do forecasted infection rates vary across different regions or cities?",
                                                             ifelse(df_matrix_no_grouping$Q18_number == 8,
                                                                    "What are the economic impacts of prolonged lockdowns on local businesses?",
                                                                    ifelse(df_matrix_no_grouping$Q18_number == 9,
                                                                           "What is the most effective strategy for prioritizing vaccine distribution?",
                                                                           ifelse(df_matrix_no_grouping$Q18_number == 10,
                                                                                  "How many vaccines do we need in the country/state/province?",
                                                                                  ifelse(df_matrix_no_grouping$Q18_number == 11,
                                                                                         "How many ventilators, hospital and ICU beds does each hospital need and when?",
                                                                                         "How will changes in public behavior impact the course of the epidemic?")))))))))))


questions_counts <- df %>%
  filter(questions_section_answered == 1)

denominator <- nrow(questions_counts)

df_matrix_no_grouping$n <- denominator

#The three most commonly asked questions are:
top_q <- df_matrix_no_grouping %>%
    top_n(3, n)

top_q$Q18_number


# Reshape the data from wide to long format
df_long <- df_matrix_no_grouping %>% 
  pivot_longer(cols = c("Discovery", "Escalation", "Acceleration", "Deceleration", "Preparation"), 
               names_to = "phase", 
               values_to = "count")

#The three most commonly asked questions are:
top_q <- df_long %>%
  group_by(phase) %>%
  top_n(1, count)

by_phase <- top_q[,c("phase","Q18_category", "Q18_number")]

by_phase


df_long <- df_long %>%
  mutate(proportion = count / n)

# Summarize by category and phase
heatmap_data <- df_long %>%
  group_by(Q18_category, phase) %>%
  summarise(total_prop = mean(proportion), .groups = "drop")

# Optional: set order of phases
heatmap_data$phase <- factor(
  heatmap_data$phase,
  levels = c("Discovery", "Escalation", "Acceleration", "Deceleration", "Preparation")
)

# Step 1: Calculate proportions
df_long <- df_long %>%
  mutate(proportion = count / n)

df_long <- df_matrix_no_grouping %>% 
  pivot_longer(cols = c("Discovery", "Escalation", "Acceleration", "Deceleration", "Preparation"),
               names_to = "phase",
               values_to = "proportion")


# Define the grouping columns
group_col <- "income_group"

# Create an empty data frame to store the results
df_matrix_region <- data.frame()

for (i in 1:12) {
  # Construct the column names for the current question
  col_names <- paste0("Q18_", i, "_", c(1, 2, 3, 4, 6))
  
  # Apply the summarize_columns2 function
  Q18_prop <- summarize_columns(
    df = df,
    group_col = group_col,
    summary_cols = col_names
  )
  
  # Remove "Missing" column if it exists
  if ("Missing" %in% names(Q18_prop)) {
    Q18_prop <- Q18_prop[, names(Q18_prop) != "Missing"]
  }
  
  # Add a column for the Q18 question number
  Q18_prop$Q18_number <- i
  
  # Append the result, safely handling differing column names
  df_matrix_region <- bind_rows(df_matrix_region, Q18_prop)
}

df_matrix_region$Q18_category <- ifelse(df_matrix_region$Q18_number %in% c(1,2,12), 
                                      "Non-Pharmaceutical Interventions and their Impact",
                                             ifelse(df_matrix_region$Q18_number %in% c(4,5,6,7), 
                                                    "Epidemic Projections and Modeling",
                                                    ifelse(df_matrix_region$Q18_number %in% c(3), 
                                                           "Herd Immunity and Reopening Strategies",
                                                           ifelse(df_matrix_region$Q18_number == 11, 
                                                                  "Healthcare Resource Allocation",
                                                                  ifelse(df_matrix_region$Q18_number %in% c(9,10), 
                                                                         "Vaccination Strategies",
                                                                         ifelse(df_matrix_region$Q18_number == 8, 
                                                                                "Economic Impact", 
                                                                                "Other"))))))


df_matrix_region$Q18_number <- ifelse(df_matrix_region$Q18_number == 1, 
                   "What is the effectiveness of different types of Non-pharmaceutical interventions? What are the potential outcomes of implementing restrictions at different time points?",
                   ifelse(df_matrix_region$Q18_number == 2,
                          "What are the potential outcomes of lifting restrictions at different points in time?",
                          ifelse(df_matrix_region$Q18_number == 3,
                                 "What is the projected timeline for achieving herd immunity? At what level of herd immunity can we safely reopen schools?",
                                 ifelse(df_matrix_region$Q18_number == 4,
                                        "What is the projected number of cases and deaths over the next x months?",
                                        ifelse(df_matrix_region$Q18_number == 5,
                                               "When is the peak of the epidemic expected to occur?",
                                               ifelse(df_matrix_region$Q18_number == 6,
                                                      "What is the expected duration of the epidemic wave?",
                                                      ifelse(df_matrix_region$Q18_number == 7,
                                                             "How do forecasted infection rates vary across different regions or cities?",
                                                             ifelse(df_matrix_region$Q18_number == 8,
                                                                    "What are the economic impacts of prolonged lockdowns on local businesses?",
                                                                    ifelse(df_matrix_region$Q18_number == 9,
                                                                           "What is the most effective strategy for prioritizing vaccine distribution?",
                                                                           ifelse(df_matrix_region$Q18_number == 10,
                                                                                  "How many vaccines do we need in the country/state/province?",
                                                                                  ifelse(df_matrix_region$Q18_number == 11,
                                                                                         "How many ventilators, hospital and ICU beds does each hospital need and when?",
                                                                                         "How will changes in public behavior impact the course of the epidemic?")))))))))))

df_matrix_region$total_vote_n <- df_matrix_region$Discovery + df_matrix_region$Escalation + df_matrix_region$Acceleration + df_matrix_region$Deceleration + df_matrix_region$Preparation

#The three most commonly asked questions are:
top_q <- df_matrix_region %>%
    group_by(income_group) %>%
    top_n(3, total_vote_n)

top_q

Q25_prop_overall <- summarize_columns_no_grouping_count(
  df = df, 
  summary_cols = c("Q25_1", "Q25_10", "Q25_4", "Q25_11", "Q25_5","Q25_12", "Q25_2", "Q25_3", "Q25_7", "Q25_8", "Q25_9"))

# Reshape the data from wide to long format
df_long <- Q25_prop_overall %>% 
  pivot_longer(cols = c(2:12), 
               names_to = "format", 
               values_to = "proportion")

df_long <- df_long %>%
  mutate(phase = str_wrap(format, width = 40))

# Calculate the mean proportion for each phase
df_summary <- df_long %>% 
  group_by(phase) %>% 
  summarize(mean_prop = mean(proportion))

# Order the phases by mean proportion in descending order
phase_order <- df_summary %>% 
  arrange(desc(-mean_prop)) %>% 
  pull(phase)



Q25_prop_overall <- summarize_columns_no_grouping_count(
  df = df, 
  summary_cols = c("Q25_1", "Q25_10", "Q25_4", "Q25_11", "Q25_5","Q25_12", "Q25_2", "Q25_3", "Q25_7", "Q25_8", "Q25_9"))

# Reshape the data from wide to long format
Q25_prop_overall %>% 
  pivot_longer(cols = c(2:12), 
               names_to = "format", 
               values_to = "proportion")


# ---- Results section statistical analysis ----

# =============================================================================
# Results Section Statistical Analysis
# Fisher's exact tests, Wilson CIs, and binary sensitivity analysis
# =============================================================================

library(tidyverse)
library(broom)

# --- 1. LOAD & CLEAN DATA ---------------------------------------------------

raw <- read_csv("data/raw/Infectech+-+v1.0_15+February+2026_01.33.csv",
                skip = 2,            # skip Qualtrics label + import rows
                col_names = FALSE,
                show_col_types = FALSE)

# Read headers from row 1
headers <- read_csv("data/raw/Infectech+-+v1.0_15+February+2026_01.33.csv",
                    n_max = 0, show_col_types = FALSE) |>
  names()
names(raw) <- headers

cat("Raw rows:", nrow(raw), "\n")

# --- 2. INCOME GROUP CLASSIFICATION ------------------------------------------
# World Bank 2023 classification (as used in manuscript)

wb_class <- tribble(
  ~country,                                                        ~income_group,
  "Argentina",                                                     "UMIC",
  "Australia",                                                     "HIC",
  "Austria",                                                       "HIC",
  "Bangladesh",                                                    "LMIC",
  "Brazil",                                                        "UMIC",
  "Burkina Faso",                                                  "LIC",
  "Canada",                                                        "HIC",
  "China",                                                         "UMIC",
  "Congo (the Democratic Republic of the)",                        "LIC",
  "Ethiopia",                                                      "LIC",
  "France",                                                        "HIC",
  "Gambia (the)",                                                  "LIC",
  "Germany",                                                       "HIC",
  "Ghana",                                                         "LMIC",
  "India",                                                         "LMIC",
  "Indonesia",                                                     "LMIC",
  "Iran (Islamic Republic of)",                                    "UMIC",
  "Ireland",                                                       "HIC",
  "Kenya",                                                         "LMIC",
  "Korea (the Republic of)",                                       "HIC",
  "Kyrgyzstan",                                                    "LMIC",
  "Lao People's Democratic Republic (the)",                        "LMIC",
  "Madagascar",                                                    "LIC",
  "Malawi",                                                        "LIC",
  "Myanmar",                                                       "LMIC",
  "Nigeria",                                                       "LMIC",
  "Pakistan",                                                      "LMIC",
  "Palestine, State of",                                           "LMIC",
  "Peru",                                                          "UMIC",
  "Philippines (the)",                                             "LMIC",
  "Poland",                                                        "HIC",
  "Senegal",                                                       "LMIC",
  "Singapore",                                                     "HIC",
  "Somalia",                                                       "LIC",
  "South Africa",                                                  "UMIC",
  "Sudan (the)",                                                   "LIC",
  "Sweden",                                                        "HIC",
  "Syrian Arab Republic",                                          "LIC",
  "Tanzania, United Republic of",                                  "LMIC",
  "Thailand",                                                      "UMIC",
  "Timor-Leste",                                                   "LMIC",
  "Tunisia",                                                       "LMIC",
  "Uganda",                                                        "LIC",
  "United Kingdom of Great Britain and Northern Ireland (the)",     "HIC",
  "United States of America (the)",                                "HIC",
  "Zambia",                                                        "LMIC",
  "Zimbabwe",                                                      "LMIC"
)

df <- raw |>
  filter(Q8 != "" & !is.na(Q8) & Q8 != "#N/A") |>
  left_join(wb_class, by = c("Q8" = "country")) |>
  mutate(
    income_group = factor(income_group, levels = c("LIC", "LMIC", "UMIC", "HIC")),
    # Binary grouping for sensitivity analysis
    income_binary = factor(
      if_else(income_group %in% c("LIC", "LMIC"), "LMIC+LIC", "HIC+UMIC"),
      levels = c("LMIC+LIC", "HIC+UMIC")
    )
  )

cat("Rows with valid country:", nrow(df), "\n")
cat("Income group distribution:\n")
print(table(df$income_group, useNA = "ifany"))
cat("\nBinary grouping:\n")
print(table(df$income_binary, useNA = "ifany"))

# --- 3. WHO REGION CONCENTRATION --------------------------------------------
# For the representativeness paragraph

who_regions <- tribble(
  ~country,                                                        ~who_region,
  "Argentina",     "AMRO", "Australia",      "WPRO", "Austria",        "EURO",
  "Bangladesh",    "SEARO", "Brazil",         "AMRO", "Burkina Faso",   "AFRO",
  "Canada",        "AMRO", "China",           "WPRO", 
  "Congo (the Democratic Republic of the)", "AFRO",
  "Ethiopia",      "AFRO", "France",          "EURO", "Gambia (the)",   "AFRO",
  "Germany",       "EURO", "Ghana",           "AFRO", "India",          "SEARO",
  "Indonesia",     "SEARO", "Iran (Islamic Republic of)", "EMRO",
  "Ireland",       "EURO", "Kenya",           "AFRO", 
  "Korea (the Republic of)", "WPRO",
  "Kyrgyzstan",    "EURO", 
  "Lao People's Democratic Republic (the)", "WPRO",
  "Madagascar",    "AFRO", "Malawi",          "AFRO", "Myanmar",        "SEARO",
  "Nigeria",       "AFRO", "Pakistan",        "EMRO", 
  "Palestine, State of", "EMRO",
  "Peru",          "AMRO", "Philippines (the)", "WPRO", "Poland",        "EURO",
  "Senegal",       "AFRO", "Singapore",       "WPRO", "Somalia",        "EMRO",
  "South Africa",  "AFRO", "Sudan (the)",     "EMRO", "Sweden",         "EURO",
  "Syrian Arab Republic", "EMRO", 
  "Tanzania, United Republic of", "AFRO",
  "Thailand",      "SEARO", "Timor-Leste",    "SEARO", "Tunisia",       "EMRO",
  "Uganda",        "AFRO", 
  "United Kingdom of Great Britain and Northern Ireland (the)", "EURO",
  "United States of America (the)", "AMRO",
  "Zambia",        "AFRO", "Zimbabwe",        "AFRO"
)

df <- df |> left_join(who_regions, by = c("Q8" = "country"))

cat("\n--- WHO REGION DISTRIBUTION ---\n")
region_tab <- df |>
  count(who_region, name = "n") |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  arrange(desc(n))
print(region_tab)

# --- 4. HELPER FUNCTIONS ----------------------------------------------------

# Wilson score confidence interval
wilson_ci <- function(x, n, alpha = 0.05) {
  if (n == 0) return(tibble(prop = NA_real_, lower = NA_real_, upper = NA_real_))
  p <- x / n
  z <- qnorm(1 - alpha / 2)
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  margin <- (z / denom) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
  tibble(prop = p, lower = max(0, centre - margin), upper = min(1, centre + margin))
}

# Fisher's exact test for a binary variable across income groups
fisher_by_income <- function(data, var, group_var = "income_group") {
  tab <- table(data[[group_var]], data[[var]] != "" & !is.na(data[[var]]))
  if (ncol(tab) < 2) {
    # All TRUE or all FALSE — no variation
    return(tibble(
      variable = var,
      grouping = group_var,
      p_value = NA_real_,
      note = "No variation"
    ))
  }
  ft <- fisher.test(tab, simulate.p.value = TRUE, B = 10000)
  tibble(
    variable = var,
    grouping = group_var,
    p_value = ft$p.value,
    note = ""
  )
}

# --- 5. FILTER TO RELEVANT RESPONDENTS --------------------------------------
# Q11 = "Yes" (received forecasts) for most analyses

df_forecasts <- df |> filter(Q11 == "Yes")
cat("\nRespondents who received forecasts:", nrow(df_forecasts), "\n")
cat("By income group:\n")
print(table(df_forecasts$income_group))

# --- 6. ANALYSIS: FORMAT RECEIVED (Q13) & PREFERRED (Q14) -------------------

format_vars_received <- c(
  Q13_1 = "Map", Q13_2 = "Table", Q13_6 = "Graph",
  Q13_4 = "Point estimates", Q13_3 = "Interactive platform",
  Q13_5 = "Policy brief", Q13_7 = "Scientific paper"
)

format_vars_preferred <- c(
  Q14_1 = "Map", Q14_2 = "Table", Q14_6 = "Graph",
  Q14_4 = "Point estimate", Q14_3 = "Interactive platform",
  Q14_5 = "Policy brief", Q14_7 = "Scientific paper"
)

cat("\n=== FORMAT RECEIVED: Fisher's exact tests (4-group) ===\n")
format_fisher <- map_dfr(names(format_vars_received), ~fisher_by_income(df_forecasts, .x))
format_fisher$label <- format_vars_received[format_fisher$variable]
print(format_fisher |> select(label, p_value) |> mutate(p_value = round(p_value, 4)))

cat("\n=== FORMAT RECEIVED: Fisher's exact (binary) ===\n")
format_fisher_bin <- map_dfr(names(format_vars_received), 
                             ~fisher_by_income(df_forecasts, .x, "income_binary"))
format_fisher_bin$label <- format_vars_received[format_fisher_bin$variable]
print(format_fisher_bin |> select(label, p_value) |> mutate(p_value = round(p_value, 4)))

# Wilson CIs for format received by income group
cat("\n=== FORMAT RECEIVED: Proportions with 95% Wilson CIs ===\n")
format_ci <- df_forecasts |>
  group_by(income_group) |>
  summarise(
    n = n(),
    across(all_of(names(format_vars_received)),
           ~sum(.x != "" & !is.na(.x)),
           .names = "{.col}_count"),
    .groups = "drop"
  ) |>
  pivot_longer(ends_with("_count"), names_to = "var", values_to = "count") |>
  mutate(var = str_remove(var, "_count")) |>
  rowwise() |>
  mutate(
    ci = list(wilson_ci(count, n)),
    label = format_vars_received[var]
  ) |>
  unnest(ci) |>
  mutate(across(c(prop, lower, upper), ~round(.x * 100, 1))) |>
  select(income_group, label, n, count, prop, lower, upper)

print(format_ci, n = 50)


# --- 7. ANALYSIS: METRICS COMMUNICATED (Q16) ---------------------------------

metric_vars <- c(
  Q16_1 = "Epidemic peak", Q16_2 = "Threshold exceedance",
  Q16_3 = "Uncertainty bounds", Q16_6 = "Peak timing",
  Q16_7 = "Cumulative incidence", Q16_8 = "Prevalence",
  Q16_9 = "Reproduction number", Q16_10 = "Doubling time",
  Q16_11 = "Growth rate", Q16_12 = "Attack rate",
  Q16_13 = "Intervention impact"
)

cat("\n=== METRICS: Fisher's exact tests (4-group) ===\n")
metric_fisher <- map_dfr(names(metric_vars), ~fisher_by_income(df_forecasts, .x))
metric_fisher$label <- metric_vars[metric_fisher$variable]
print(metric_fisher |> select(label, p_value) |> mutate(p_value = round(p_value, 4)))

cat("\n=== METRICS: Fisher's exact (binary) ===\n")
metric_fisher_bin <- map_dfr(names(metric_vars), 
                             ~fisher_by_income(df_forecasts, .x, "income_binary"))
metric_fisher_bin$label <- metric_vars[metric_fisher_bin$variable]
print(metric_fisher_bin |> select(label, p_value) |> mutate(p_value = round(p_value, 4)))

# Wilson CIs
cat("\n=== METRICS: Proportions with 95% Wilson CIs ===\n")
metric_ci <- df_forecasts |>
  group_by(income_group) |>
  summarise(
    n = n(),
    across(all_of(names(metric_vars)),
           ~sum(.x != "" & !is.na(.x)),
           .names = "{.col}_count"),
    .groups = "drop"
  ) |>
  pivot_longer(ends_with("_count"), names_to = "var", values_to = "count") |>
  mutate(var = str_remove(var, "_count")) |>
  rowwise() |>
  mutate(
    ci = list(wilson_ci(count, n)),
    label = metric_vars[var]
  ) |>
  unnest(ci) |>
  mutate(across(c(prop, lower, upper), ~round(.x * 100, 1))) |>
  select(income_group, label, n, count, prop, lower, upper)

print(metric_ci, n = 60)


# --- 8. ANALYSIS: CONFIDENCE FACTORS (Q24) -----------------------------------

confidence_vars <- c(
  Q24_1 = "Interaction with developers",
  Q24_2 = "Peer-reviewed evaluation",
  Q24_3 = "Existing relationship",
  Q24_4 = "Knowledge of developers' work",
  Q24_6 = "Uncertainties presented",
  Q24_7 = "Comprehensive assumptions",
  Q24_8 = "Forecast tailored to context",
  Q24_9 = "Performance of previous forecasts"
)

cat("\n=== CONFIDENCE FACTORS: Fisher's exact (4-group) ===\n")
conf_fisher <- map_dfr(names(confidence_vars), ~fisher_by_income(df_forecasts, .x))
conf_fisher$label <- confidence_vars[conf_fisher$variable]
print(conf_fisher |> select(label, p_value) |> mutate(p_value = round(p_value, 4)))

cat("\n=== CONFIDENCE FACTORS: Fisher's exact (binary) ===\n")
conf_fisher_bin <- map_dfr(names(confidence_vars), 
                           ~fisher_by_income(df_forecasts, .x, "income_binary"))
conf_fisher_bin$label <- confidence_vars[conf_fisher_bin$variable]
print(conf_fisher_bin |> select(label, p_value) |> mutate(p_value = round(p_value, 4)))

# Wilson CIs for key finding: uncertainty presentation
cat("\n=== CONFIDENCE: Uncertainties presented - Wilson CIs ===\n")
unc_ci <- df_forecasts |>
  group_by(income_group) |>
  summarise(
    n = n(),
    count = sum(Q24_6 != "" & !is.na(Q24_6)),
    .groups = "drop"
  ) |>
  rowwise() |>
  mutate(ci = list(wilson_ci(count, n))) |>
  unnest(ci) |>
  mutate(across(c(prop, lower, upper), ~round(.x * 100, 1)))
print(unc_ci)


# --- 9. ANALYSIS: BARRIERS (Q25) --------------------------------------------

barrier_vars <- c(
  Q25_1 = "Did not understand forecasts",
  Q25_10 = "Others did not understand forecasts",
  Q25_4 = "Did not understand methodology",
  Q25_11 = "Others did not understand methodology",
  Q25_5 = "Not relevant to context",
  Q25_12 = "Outcome measures not produced",
  Q25_2 = "Not provided at right time",
  Q25_3 = "Not developed by local groups",
  Q25_7 = "Inappropriate assumptions",
  Q25_8 = "Too many limitations"
)

cat("\n=== BARRIERS: Fisher's exact (4-group) ===\n")
barrier_fisher <- map_dfr(names(barrier_vars), ~fisher_by_income(df_forecasts, .x))
barrier_fisher$label <- barrier_vars[barrier_fisher$variable]
print(barrier_fisher |> select(label, p_value) |> mutate(p_value = round(p_value, 4)))

cat("\n=== BARRIERS: Fisher's exact (binary) ===\n")
barrier_fisher_bin <- map_dfr(names(barrier_vars), 
                              ~fisher_by_income(df_forecasts, .x, "income_binary"))
barrier_fisher_bin$label <- barrier_vars[barrier_fisher_bin$variable]
print(barrier_fisher_bin |> select(label, p_value) |> mutate(p_value = round(p_value, 4)))

# Wilson CIs
cat("\n=== BARRIERS: Proportions with 95% Wilson CIs ===\n")
barrier_ci <- df_forecasts |>
  group_by(income_group) |>
  summarise(
    n = n(),
    across(all_of(names(barrier_vars)),
           ~sum(.x != "" & !is.na(.x)),
           .names = "{.col}_count"),
    .groups = "drop"
  ) |>
  pivot_longer(ends_with("_count"), names_to = "var", values_to = "count") |>
  mutate(var = str_remove(var, "_count")) |>
  rowwise() |>
  mutate(
    ci = list(wilson_ci(count, n)),
    label = barrier_vars[var]
  ) |>
  unnest(ci) |>
  mutate(across(c(prop, lower, upper), ~round(.x * 100, 1))) |>
  select(income_group, label, n, count, prop, lower, upper)

print(barrier_ci, n = 60)


# --- 10. SUMMARY TABLE FOR MANUSCRIPT ----------------------------------------

cat("\n\n========================================\n")
cat("SUMMARY: Key Fisher's exact p-values\n")
cat("========================================\n\n")

all_fisher <- bind_rows(
  format_fisher |> mutate(domain = "Format received"),
  metric_fisher |> mutate(domain = "Metrics communicated"),
  conf_fisher |> mutate(domain = "Confidence factors"),
  barrier_fisher |> mutate(domain = "Barriers")
) |>
  select(domain, label, p_value) |>
  mutate(
    p_value = round(p_value, 4),
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.10  ~ ".",
      TRUE            ~ ""
    )
  )

print(all_fisher, n = 50)

cat("\n--- Binary sensitivity (HIC+UMIC vs LMIC+LIC) ---\n")
all_fisher_bin <- bind_rows(
  format_fisher_bin |> mutate(domain = "Format received"),
  metric_fisher_bin |> mutate(domain = "Metrics communicated"),
  conf_fisher_bin |> mutate(domain = "Confidence factors"),
  barrier_fisher_bin |> mutate(domain = "Barriers")
) |>
  select(domain, label, p_value) |>
  mutate(
    p_value = round(p_value, 4),
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.10  ~ ".",
      TRUE            ~ ""
    )
  )

print(all_fisher_bin, n = 50)

cat("\n\nNotes:\n")
cat("- Fisher's exact with Monte Carlo simulation (B = 10,000)\n")
cat("- Wilson score 95% CIs for proportions\n")
cat("- Binary sensitivity: HIC+UMIC vs LMIC+LIC\n")
cat("- *** p<0.001, ** p<0.01, * p<0.05, . p<0.10\n")
