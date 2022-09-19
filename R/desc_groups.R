#' @title Profiling categorical variable
#' @description Calculate the means (or other function) per group to analyze how each segment behave. It scales each variable mean inti the 0 to 1 range to easily profile the groups according to its mean. It also calculate the mean regardless the grouping. This function is also useful when you want to profile cluster results in terms of its means. It automatically adds a row representing the sumarization of the column regardless the group_var categories, this is useful to compare each segement with the whole population. It will exclude all factor/character variables.
#' @param data input data source
#' @param group_var variable to make the group by
#' @param group_func the data type of this parameter is a function, not an string, this is the function to be used in the group by, the default value is: mean
#' @param add_all_data_row flag indicating if final data contains the row: 'All_Data', which is the function applied regardless the grouping. Useful to compare with the rest of the values.
#' @examples
#' # default grouping function: mean
#' desc_groups(data=mtcars, group_var="cyl")
#'
#' # using the median as the grouping function
#' desc_groups(data=mtcars, group_var="cyl", group_func=median)
#'
#' # using the max as the grouping function
#' desc_groups(data=mtcars, group_var="gear", group_func=max)
#' @return grouped data frame
#' @importFrom dplyr mutate_each_ summarise_each_ group_by_ funs select summarise_each %>%
#' @export
desc_groups <- function(data, group_var, group_func=mean, add_all_data_row=T)
{
  . <- NULL
    ## calculate only for numeric variables
    status = vvsculptor::df_inspect(data, print_results = F)
    vars_to_keep=status[status$type %in% c("integer", "numeric") & status$variable != group_var, "variable"]

    grp_mean=data %>% group_by_(group_var) %>% summarise_each_(funs(group_func), vars_to_keep) %>% mutate_each_(funs(round(.,2)), vars_to_keep)
    grp_mean=data.frame(grp_mean)

    grp_mean[,group_var]=as.character(grp_mean[,group_var])

    # select all except the group var
    a=select(grp_mean, -dplyr::one_of(group_var))

    # vars_to_keep have all num variables (excluding group_var and factor/char). Calculate 'All_Data' means per column
    data_num=select(data, dplyr::one_of(vars_to_keep))
    b=as.data.frame(data_num) %>% summarise_each(funs(group_func))

    ## putting all together: the sumarization per group plus the total per column
    all_results=rbind(a, b)
    all_results_report=all_results

    all_results_report[,group_var]=c(grp_mean[,group_var], "All_Data")

    # rearrange columns
    nc=ncol(all_results_report)
    all_results_report=all_results_report[,c(nc, 1:(nc-1))]

    # excluding all_data row if needed
    if(!add_all_data_row) {
        all_results_report=all_results_report[-nrow(all_results_report),]
    }

    return(all_results_report)
}

#' @title Profiling categorical variable (rank)
#' @description Similar to 'desc_groups' function, this one computes the rank of each value in order to quickly know what is the value in each segment that has the highest value (rank=1). 1 represent the highest number. It will exclude all factor/character variables.
#' @param data input data source
#' @param group_var variable to make the group by
#' @param group_func the data type of this parameter is a function, not an string, this is the function to be used in the group by, the default value is: mean
#' @examples
#' # default grouping function: mean
#' desc_groups_rank(data=mtcars, group_var="gear")
#'
#' # using the median as the grouping function
#' desc_groups(data=mtcars, group_var="cyl", group_func=median)
#'
#' # using the max as the grouping function
#' desc_groups_rank(data=mtcars, group_var="gear", group_func=max)
#' @return grouped data frame, showing the rank instead of the absolute values/
#' @importFrom dplyr dense_rank desc %>%
#' @export
desc_groups_rank <- function(data, group_var, group_func=mean)
{
    . <- NULL
    d_group=desc_groups(data, group_var, group_func, add_all_data_row = F)

    # excluding grouping variable, from the grouping
    all_col=colnames(d_group)
    vars_to_group=all_col[all_col!=group_var]

    # mutate each does the group by only for variables defined in vars_to_group
    d_group_rank=d_group %>% mutate_each_(funs(dense_rank(desc(.))), vars_to_group)

    return(d_group_rank)
}
