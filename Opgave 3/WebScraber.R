



#------------------------------------------------------------------------------------------------------------
#
#                                           Webscrape af AirBnB
#
#------------------------------------------------------------------------------------------------------------

library("rvest")
library("magrittr")
library("dplyr")

# 1) Først skal der skabes en liste over links, der skal høstes data fra
#                       #  Listen af links skal være mulig at danne med dynamiske kriterier
#                       #  Listen skal være udtømmelig, ikke et subsample


#link til side, hvor at links skal høstes fra: 
airbnb = html("https://www.airbnb.dk/s/Danmark/homes?refinement_paths%5B%5D=%2Fhomes&query=Danmark&allow_override%5B%5D=&s_tag=auLqOwaz")
airbnb = html("https://www.airbnb.dk/s/Danmark/homes?refinement_paths%5B%5D=%2Fhomes&allow_override%5B%5D=&s_tag=1nQOZ1bz")

test <-  airbnb %>% 
  html_nodes("_14csrlku") %>% #noded burde indholde en attribute med et brugerid, der skal indgå i listen over links
  html_attr() #retunere en tom liste uden fejl?!

node = "_14csrlku" # over-node
node = "._1df8dftk" #picturetext
node = "._ng4pvpo" #picture alternative node
  

airbnb = "https://www.airbnb.dk/s/Danmark/homes?refinement_paths%5B%5D=%2Fhomes&allow_override%5B%5D=&s_tag=1nQOZ1bz"
test <-  airbnb %>% 
  read_html() %>% 
  html_node("_14csrlku")%>% 
  html_attrs()





# Google eksemplet virker helt fint
google <-  html("https://news.google.com/?hl=en-US&gl=US&ceid=US%3Aen")
test <- google %>% 
  html_nodes(".boy4he") %>% 
  html_attrs()

test <- do.call(rbind.data.frame, test)


