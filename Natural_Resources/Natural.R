library(tidyr)
library(dplyr)
library(readxl)
library(plyr)
natural_resources = read_excel("Natural_Resources/Natural_resources_data.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
natural_resources<-data.frame(natural_resources)
natural_resources<-select(natural_resources, -Measure..Measure.)
natural_resources<-gather(natural_resources, "Year", "Number", starts_with('X'))%>%
  separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Country..Country., Species..ASFIS.species., FAO.Area, Year, Number)   
natural_resources<-plyr::rename(natural_resources, c("Country..Country."="Country"))
natural_resources<-plyr::rename(natural_resources, c("Species..ASFIS.species."="Species"))
natural_resources<-dplyr::filter(natural_resources, !(Number %in% c("NA")))
natural_resources<-dplyr::filter(natural_resources, !(Country %in% c("Totals 0 Quantity (number)")))
natural_resources$Number<-as.numeric(natural_resources$Number) ## change number column from character to numbers

## Separate out West and East Greenland
natural_resources_greenland=dplyr::filter(natural_resources, (Country %in% c("Greenland")))
natural_resources_egreenland<-dplyr::filter(natural_resources_greenland, !(FAO.Area %in% c("Atlantic, Northwest")))
natural_resources_egreenland$Country<- "East Greenland"
natural_resources_wgreenland<-dplyr::filter(natural_resources_greenland, !(FAO.Area %in% c("Atlantic, Northeast")))
natural_resources_wgreenland$Country<- "West Greenland"
natural_resources<- dplyr::filter(natural_resources, !(Country %in% c("Greenland")))
natural_resources<- dplyr::bind_rows(natural_resources, natural_resources_wgreenland, natural_resources_egreenland)

## Sum duplicate rows
natural_resources = natural_resources%>% group_by(Country, Year, Species)%>%
  dplyr::summarise(totalcatch=sum(Number, na.rm=T))%>%
 ungroup()

##Separate out countries
natural_resources_norway=dplyr::filter(natural_resources, (Country %in% c("Norway")))
natural_resources_canada=dplyr::filter(natural_resources, (Country %in% c("Canada")))
natural_resources_greenland=dplyr::filter(natural_resources, (Country %in% c("East Greenland", "West Greenland")))
natural_resources_russia=dplyr::filter(natural_resources, (Country %in% c("Russian Federation")))
natural_resources_usa=dplyr::filter(natural_resources, (Country %in% c("United States of America")))

write.csv(natural_resources, "natural_resources.csv")
write.csv(natural_resources_norway, "natural_resources_norway.csv")
write.csv(natural_resources_canada, "natural_resources_canada.csv")
write.csv(natural_resources_usa, "natural_resources_usa.csv")
write.csv(natural_resources_greenland, "natural_resources_greenland.csv")
write.csv(natural_resources_russia, "natural_resources_russia.csv")