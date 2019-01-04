

library(tidyverse)

# Load data
load('wdi.rds')


fixed_mean <- function(x){
  # Returns NA if there is nothing 
  # to take the mean of. Otherwise
  # returns the mean. 
  #
  # Arguments:
  #   x: vector which can be 
  #      averaged.
  
  v <- mean(x, na.rm = T)
  if (is.nan(v)) {
    return(NA_real_)
  }
  return(v)
}


grouped_mean_imputation <- function(df, group_var, impute_var){
  # Mean imputes a column by groupl
  #
  # Creates a new column imputed_"impute_var": 
  # is 1 if the variable is either imputed, or 
  # still NA (no values in group to impute from).
  # Is 0 if observation is original.
  #
  # Arguments:
  #   df: a dataframel
  #   group_var: the variable to group by.
  #   impute_var: the variable to impute.

  
  keys_q <- enquo(group_var)
  values_q <- enquo(impute_var)
  varname <- quo_name(values_q)
  dummy_name <- paste0("imputed_", varname)
  
  df %>%
    group_by(!! keys_q) %>%
    mutate(
     !! dummy_name := case_when(is.na(!! values_q) ~ 1,
                                T ~ 0), 
     !! varname := case_when(is.na(!! values_q) ~ fixed_mean(!! values_q),
                             T ~ !! values_q)
    ) 
}




# This last bit is just to show that it works
RESULT <- grouped_mean_imputation(WDI, countryName, gov_debt)

