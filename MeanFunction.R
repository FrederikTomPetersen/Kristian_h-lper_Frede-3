
# metodisk ved jeg godt at det lidt er noget skidt at erstatte missing values med gennemsnittet
#Indtil jeg har styr på hvad jeg ellers gør er der noget læring at kunne lave en funktion, der tager en col som argument



#Der ligger en WDI.rds som kan bruges til test data



# Jeg vil egnetlig gerne lave nedenstående funktionalitet om til en funktion der tager en df og en kolonne som argumenter


WDI_Tester <-  WDI

#finder de rækker som har en værdi i kolonnen
WDI_GROUP_NOTNA <-  WDI %>% 
  group_by(countryName) %>% 
  filter(!is.na(gov_debt)) %>% 
  mutate(govdebtmean = mean(gov_debt)) 

#laver en reduceret tabel, således at mit join ikke genrerer unødvendige rækker
WDI_GROUP_NOTNA_selct <- WDI_GROUP_NOTNA %>% 
  select(countryName, govdebtmean) %>% 
  distinct(countryName,govdebtmean)

#Joiner en kolonne med middelværdierne på datasættet
WDI_Tester <-  WDI_Tester %>% 
  left_join(WDI_GROUP_NOTNA_selct)


#fylder middelværdien ud i kolonnen der hvor der er NA
WDI_Tester <-  WDI_Tester %>% 
  mutate(gov_debt = ifelse(!is.na(gov_debt),gov_debt,govdebtmean))

#############################################


# Jeg har forsøgt at løse problemet med følgende funktion.
#Desværre blvier funktionen ved med at læse argumentet y som "y" frem for den værdi der er indeholdt der i. 

#help is much appreciated 

Mean_On_Missing_WDI =function(df,col){
  z <- paste0("mean_",col) 
  x[z] <-  NA
  
  
  notna <- x %>% 
    group_by(countryName) %>% 
    filter(!is.na(col)) %>% 
    mutate(notna[,z] = mean(notna[,value.toString(col)]))
  
  
  notnaselect <-  notna %>% 
    select(countryName, z) %>% 
    distinct(countryName, z)
  
  x <- x %>% 
    left_join(notnaselect)
  
  x <- x %>% 
    mutate(x$y = ifelse(!is.na(x$col), x$y, x$col)) %>% 
    select(-x$col)
  return(x)  
}  

#Kører funktionen
WDI_Tester <- Mean_On_Missing_WDI(WDI_Tester,"gov_expenditure")   

