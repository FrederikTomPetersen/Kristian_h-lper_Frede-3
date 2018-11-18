
# Grouped timedependent deviation from mean

# Jeg vil gerne lave en funtion, der ser p√• hvordan at en observations v√¶rdi afviger
# fra gennemsnittet af variablen grupperet p√• land, og inden for en tidsperiode

# Funktionen skal tage 4 argumenter: 
#     1) Dataframe
#     2) Variable / liste af variabler 
#     3) variable der skal grupperes p√•
#     4) Tidsperioden, for hvilket gennemsnittet skal udregnes. 


# I mit fors√∏g nedenfor, l√∏ber jeg ind i fejlen  "Error in .... object 'time2' not found"
# jeg forestiller mig at det er mit mutate, der ikke spiller, men jeg kan ikke lige se, 
# hvorfor det ikke skulle g√∏re det


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

# ------------------------------------------------------------------------------
# SOLUTION (wrap i funktion hvis du skal bruge det 1000 gange)

# 1) Complete cases pÂ dato
test <- df %>%
  mutate(ones = 1) %>%  # tÊlle-variable
  group_by(p4n) %>%    
  complete(date = seq.Date(min(date), max(date), by="day"))

# 2) Rolling mean pÂ relevant variable + sum af tÊllevariable (lag for ikke at fÂ periode t med i gns)
test2 <- test %>%
  mutate(mean_last_6_mo = RcppRoll::roll_mean(lag(q1cnt), 30*6, na.rm = TRUE, align = "right", fill = NA),
         nobs_last_6_mo = RcppRoll::roll_sum(lag(ones), 30*6, na.rm = TRUE, align = "right", fill = NA)
         )

# 3) Fjern konstruerede rÊkker igen
testfinal <- test2 %>%
  filter(!is.na(year))
