
# Grouped timedependent deviation from mean

# Jeg vil gerne lave en funtion, der ser på hvordan at en observations værdi afviger
# fra gennemsnittet af variablen grupperet på land, og inden for en tidsperiode

# Funktionen skal tage 4 argumenter: 
#     1) Dataframe
#     2) Variable / liste af variabler 
#     3) variable der skal grupperes på
#     4) Tidsperioden, for hvilket gennemsnittet skal udregnes. 




# I mit forsøg nedenfor, løber jeg ind i fejlen  "Error in .... object 'time2' not found"
# jeg forestilelr mig at det er mit mutate, der ikke spiller, men jeg kan ikke lige se, 
# hvorfor det ikke skulle gøre det




library(tidyverse)
library(lubridate)

# Load data
df <- readRDS(file = "data.rds")


grouped_time_mean = function(df,group_var, var, time_in_month){
  
  #df <- Dataset
  #group_var <- "p4n"
  #var <- "q1cnt"
  #time_in_month <- 6
  
  groupvar_q <- enquo(group_var)
  variable_q <- enquo(var)
  varname <- quo_name(variable_q)
  timename <- toString(time_in_month)
  dummy_name <- paste0("t_",timename, "_", varname)
  
  
  df %>% 
    group_by(!! groupvar_q) %>% 
    mutate(
      time2 = date %m-% months(time_in_month),
      !! dummy_name := variable_q - mean(!! variable_q[which(date %within% interval(date,time2))]) %>% 
        select(-"time2")
    )
}

a <-  grouped_time_mean(Dataset, p4n, q1cnt, 6)  