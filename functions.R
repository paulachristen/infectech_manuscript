

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
