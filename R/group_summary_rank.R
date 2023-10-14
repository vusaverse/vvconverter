#' @title Group Summary Rank
#' @description Similar to 'group_summary' function, this one computes the rank of each value in order to quickly know what is the value in each segment that has the highest value (rank=1). 1 represents the highest number. It will exclude all factor/character variables.
#' @param data Input data source.
#' @param group_var Variable to make the group by.
#' @param group_func Function to be used in the group by. Default is mean.
#' @return Grouped data frame, showing the rank instead of the absolute values.
#' @importFrom dplyr group_by summarise mutate across dense_rank desc
#' @export
group_summary_rank <- function(data, group_var, group_func = mean) {
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

  # Calculate rank for each numeric variable in the data frame
  all_results_rank <- all_results %>% dplyr::mutate(dplyr::across(dplyr::all_of(numeric_vars), ~ dplyr::dense_rank(dplyr::desc(.x)), .names = "{.col}_rank"))

  return(all_results_rank)
}
