
# Grouped timedependent deviation from mean

# Jeg vil gerne lave en funtion, der ser på hvordan at en observations værdi afviger
# fra gennemsnittet af variablen grupperet på land, og inden for en tidsperiode

# Funktionen skal tage 4 argumenter: 
#     1) Dataframe
#     2) Variable / liste af variabler 
#     3) variable der skal grupperes på
#     4) Tidsperioden, for hvilket gennemsnittet skal udregnes. 


# I mit forsøg nedenfor, løber jeg ind i fejlen  "Error in .... object 'time2' not found"
# jeg forestiller mig at det er mit mutate, der ikke spiller, men jeg kan ikke lige se, 
# hvorfor det ikke skulle gøre det


library(tidyverse)
library(lubridate)

# Load data
df <- readRDS(file = "data.rds")



df$date <- ymd(df$date)



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
      time2 = df$date %m-% months(time_in_month),
       !! dummy_name := !! variable_q - mean(!! variable_q[which(df$date %within% interval(df$date,df$time2))])) %>% 
     select(-"time2")
  return(df)
}


#-------------------------------------------------
#Håndholdt test

test <- df %>% 
  group_by(p4n) %>% 
  mutate(time2 = date %m-% months(6)) # virker - so far so good


test <- df %>% 
  group_by(p4n) %>% 
  mutate(time2 = date %m-% months(6),
         t6_q1cnt = q1cnt - mean(q1cnt[which(df$date %within% interval(date,time))])) #Evaluation error: do not know how to convert 'x' to class <U+0093>POSIXct<U+0094>.







# ------------------------------------------------------------------------------
# Kristian proof of concept 
# SOLUTION (wrap i funktion hvis du skal bruge det 1000 gange)

# 1) Complete cases på dato
test <- df %>%
  mutate(ones = 1) %>%  # tælle-variable
  group_by(p4n) %>%    
  complete(date = seq.Date(min(date), max(date), by="day"))

# 2) Rolling mean på relevant variable + sum af tællevariable (lag for ikke at få periode t med i gns)
test2 <- test %>%
  mutate(mean_last_6_mo = RcppRoll::roll_mean(lag(q1cnt), 30*6, na.rm = TRUE, align = "right", fill = NA),
         nobs_last_6_mo = RcppRoll::roll_sum(lag(ones), 30*6, na.rm = TRUE, align = "right", fill = NA)
         )

# 3) Fjern konstruerede rækker igen
testfinal <- test2 %>%
  filter(!is.na(year))


grouped_time_mean <- function(df, group_vars, var, days=30*6) {
  test <- df %>%
    mutate(ones = 1) %>%  # tælle-variable
    group_by(!!!group_vars) %>%    
    complete(date = seq.Date(min(date), max(date), by="day"))

  # 2) Rolling mean på relevant variable + sum af tællevariable (lag for ikke at få periode t med i gns)
  test2 <- test %>%
    mutate(!!paste0("mean_last_6_mo_", quo_name(var)) := RcppRoll::roll_mean(lag(!!var), days, na.rm = TRUE, align = "right", fill = NA),
           nobs_last_6_mo = RcppRoll::roll_sum(lag(ones), days, na.rm = TRUE, align = "right", fill = NA),
           !!paste0("deviation_last_6_mo_", quo_name(var)) := !!var - !!rlang::sym(paste0("mean_last_6_mo_", quo_name(var)))
    )

  # 3) Fjern konstruerede rækker igen
  test2 %>%
    filter(!is.na(year))
}

grouped_time_mean(df, vars(p4n), quo(q1cnt))