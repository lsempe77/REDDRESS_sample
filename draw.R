library(stringdist)
library(readxl)
library(DataEditR)
library(htmlwidgets)
library(raster)
library(sf)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
require(rgdal)
library (tidyverse)
library(gmapsdistance)
library(readxl)
library(tidyverse)
library(flextable)

hf <- read.csv("GL LR SL Health Care System Public - Liberia Health Facilities.csv", comment.char="#")

st1<-c("Lofa")

st2<-c("GrandGedeh","Margibi")

hf<- hf %>% filter(Region %in%  st1 | Region %in%  st2 | Region=="Grand Gedeh")

Sampling_frame <- read_excel("Sampling frame draft_13.12.2021_From Colleen.xlsx")


# To make sure we are dealing with charts
hf$Center<-as.character(hf$Center)

variable.names(Sampling_frame)

Sampling_frame$`Facility name`<-as.character(Sampling_frame$`Facility name`)

hf_red<-hf[,c(23,25,5)]
#table(hf_red$Region)

samp_red<-Sampling_frame[,c(7,8,2)]

county<-unique(samp_red$County)
county<-county[-4]

hf_red<-hf_red %>% filter (Region %in% county)

samp_red<-samp_red %>% mutate(District = str_remove(District, " District"))

district_samp<-unique(samp_red$District)

district_samp<-district_samp[-17] %>% sort()

hf_red_samp<-unique(hf_red$District) %>%sort()

hf_red$District[1]<-"B'Hai"
hf_red$District[137:139]<-"Mambah-Kaba"
hf_red$District[20:30]<-"Tchien"

###

hf_red %>% filter (District=="Quardu Gboni")


samp_red[37,1]<-"Quardu Gboni"
samp_red[45,1]<-"Quardu Gboni"

Sampling_frame[37,7]<-"Quardu Gboni"
Sampling_frame[45,7]<-"Quardu Gboni"

#

district_samp<-unique(samp_red$District)

district_samp<-district_samp[-18] %>% sort()

hf_red_samp<-unique(hf_red$District) %>%sort()

#

hf<-hf[-128,] # Borma repeated with 130

xpmat<-hf %>% full_join(samp_red,by=c("Region"="County","District"))

# xpmat %>% rowwise()%>%
#   mutate(dl=stringdist::stringdist(Center, `Facility name`, method = "dl"),
#          lcs=stringdist::stringdist(Center, `Facility name`, method = "cosine"))%>%
#   ggplot()+geom_point(aes(dl,lcs))


te1<-xpmat %>% rowwise() %>%
  mutate(dl=stringdist::stringdist(Center, `Facility name`, method = "dl"),
         cosine=stringdist::stringdist(Center, `Facility name`, method = "cosine")) %>%
  mutate(exact=case_when(dl==0 ~ "yes",
                         T ~ "no"),
         q_exact=case_when(dl<3 | cosine <.1 ~"yes",
                           T ~ "no")) %>% group_by(Region,District,Center) %>%
  filter(cosine==min(cosine))  %>% mutate(OK=case_when(exact=="yes" ~"yes"))

#write.csv(te1,"te1.csv")

te2 <- read.csv("te2.csv")

#variable.names(te2) # matched and unmatched

te3<-te2[1:45]

#table(te3$Facility.me)

te3 <- te3 %>% filter (Facility.me!="") %>% select(45) %>% mutate(match1="yes") #subset HF matched sampling - 89

te4<-Sampling_frame %>% # Sampling non match- 55
  left_join(te3,by=c("Facility name"= "Facility.me")) %>%
  filter (is.na(match1))

##

te5<-te2 %>% filter (Facility.me=="") #34

te5<-te5[1:44]

#

lib = getData(country = "LBR", level = 2)

lib = lib %>% st_as_sf() 


te7 <- te2 %>% filter(OK=="yes")

te7 <- te7 %>% filter (Facility.me!="Dr Barcolleh Health Center")

te7[11,29]<- 6.1081
te7[11,30]<- -8.190


#table(te7$Facility.me)

m <- leaflet() %>% setView( lng= -10.2, lat = 6, zoom = 7)

marker_popup <- paste0("<strong>Health Facility: </strong>", te7$Facility.me, "<br>")



Lofa <- lib %>% filter (NAME_1=="Lofa")

Mar_GG<-lib %>% filter (NAME_1=="GrandGedeh" | NAME_1 == "Margibi")

pal <- colorFactor(palette = "Set1",domain = Lofa$NAME_2,na.color = "transparent")

pal2 <- colorFactor(palette = "Dark2",domain = Mar_GG$NAME_2,na.color = "transparent")


polygon_popup1 <- paste0("<strong>District: </strong>", Lofa$NAME_2, "<br>")

polygon_popup2 <- paste0("<strong>District: </strong>", Mar_GG$NAME_2, "<br>")


m1<-m %>% addTiles()%>% 
  addPolygons(data = Lofa, 
              color= ~pal(NAME_2),
              popup = polygon_popup1,
              weight = 1, smoothFactor = 3,
              opacity = .2, fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                           bringToFront = TRUE),
              group = "Study A") %>%
  addPolygons(data = Mar_GG, 
              color= ~pal2(NAME_2),
              popup = polygon_popup2,
              weight = 1, smoothFactor = 3,
              opacity = .2, fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              group = "Study B") %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Study A","Study B","Health Facilities"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addMarkers(lng=te7$Long,lat=te7$Lat,
             popup = marker_popup,
             group = "Health Facilities")


