#' @title Group Summary
#' @description Calculate the means (or other function) per group to analyze how each segment behaves. It scales each variable mean into the 0 to 1 range to easily profile the groups according to its mean. It also calculates the mean regardless of the grouping. This function is also useful when you want to profile cluster results in terms of its means. It automatically adds a row representing the summary of the column regardless of the group_var categories, which is useful to compare each segment with the whole population. It will exclude all factor/character variables.
#' @param data Input data source.
#' @param group_var Variable to make the group by.
#' @param group_func Function to be used in the group by. Default is mean.
#' @return Grouped data frame.
#' @importFrom dplyr group_by summarise mutate across all_of
#' @export
group_summary <- function(data, group_var, group_func = mean) {
  # Calculate only for numeric variables
  numeric_vars <- names(data)[sapply(data, is.numeric) & names(data) != group_var]

  # Group by and calculate group_func
  grp_mean <- data %>%
    dplyr::group_by(dplyr::across(all_of(group_var))) %>%
    dplyr::summarise(dplyr::across(all_of(numeric_vars), ~ group_func(.x), .names = "{.col}")) %>%
    dplyr::mutate(dplyr::across(all_of(numeric_vars), ~ round(.x, 2)))

  # Calculate group_func for each numeric variable in the data frame, regardless of the group_var
  all_data_mean <- data %>%
    dplyr::summarise(dplyr::across(all_of(numeric_vars), ~ group_func(.x), .names = "{.col}")) %>%
    dplyr::mutate(dplyr::across(all_of(numeric_vars), ~ round(.x, 2)))

  # Add 'All_Data' row to all_data_mean
  all_data_mean[[group_var]] <- "All_Data"

  # Combine the results
  all_results <- rbind(grp_mean, all_data_mean)

  return(all_results)
}
