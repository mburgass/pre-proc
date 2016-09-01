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

##import other marine mammals
narwhal = read_excel("Natural_Resources/marine_mammals.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
narwhal<- data.frame(narwhal)
narwhal<-select(narwhal, -Measure..Measure.)
narwhal<-gather(narwhal, "Year", "Number", starts_with('X'))%>%
  separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Country..Country., Species..ASFIS.species., FAO.Area, Year, Number) 
narwhal<-plyr::rename(narwhal, c("Country..Country."="Country"))
narwhal<-plyr::rename(narwhal, c("Species..ASFIS.species."="Species"))
narwhal<-dplyr::filter(narwhal, !(Number %in% c("NA")))
narwhal<-dplyr::filter(narwhal, !(Country %in% c("Totals 0 Quantity (number)","Totals 0 Quantity (tonnes)")))
narwhal$Number<-as.numeric(narwhal$Number) ## change number column from character to numbers
narwhal<-plyr::rename(narwhal, c("Number"="totalcatch"))
narwhal<-select(narwhal, -FAO.Area)

##Separate out by country
narwhal_canada=dplyr::filter(narwhal, (Country %in% c("Canada")))
narwhal_green=dplyr::filter(narwhal, (Country %in% c("Greenland")))
narwhal_usa=dplyr::filter(narwhal, (Country %in% c("United States of America")))
narwhal_russia=dplyr::filter(narwhal, (Country %in% c("Russia")))
narwhal_norway=dplyr::filter(narwhal, (Country %in% c("Norway")))



## Greenland catches equally split between East and West Greenland

narwhal_green$totalcatch<- narwhal_green$totalcatch/2 
narwhal_green$totalcatch<- round(narwhal_green$totalcatch, digits=0)
narwhal_egreen= narwhal_green
narwhal_egreen$Country<- "East Greenland"
narwhal_wgreen= narwhal_green
narwhal_wgreen$Country<- "West Greenland"
narwhal_greenland = bind_rows(narwhal_wgreen, narwhal_egreen)

##Separate out countries
natural_resources_norway=dplyr::filter(natural_resources, (Country %in% c("Norway")))
natural_resources_canada=dplyr::filter(natural_resources, (Country %in% c("Canada")))
natural_resources_greenland=dplyr::filter(natural_resources, (Country %in% c("East Greenland", "West Greenland")))
natural_resources_russia=dplyr::filter(natural_resources, (Country %in% c("Russian Federation")))
natural_resources_usa=dplyr::filter(natural_resources, (Country %in% c("United States of America")))


##add marine mammals in
natural_resources_canada<-bind_rows(natural_resources_canada, narwhal_canada)
natural_resources_canada<- arrange(natural_resources_canada, Species)
natural_resources_greenland<- bind_rows(natural_resources_greenland, narwhal_greenland)
natural_resources_greenland<- arrange(natural_resources_greenland, Country, Species)
natural_resources_usa<- bind_rows(natural_resources_usa, narwhal_usa)
natural_resources_usa<- arrange(natural_resources_usa, Species)
natural_resources_norway<- bind_rows(natural_resources_norway, narwhal_norway)
natural_resources_norway<- arrange(natural_resources_norway, Species)
natural_resources_russia<- bind_rows(natural_resources_russia, narwhal_russia)
natural_resources_russia<- arrange(natural_resources_russia, Species)
natural_resources<-bind_rows(natural_resources, narwhal)
natural_resources<- arrange(natural_resources,Country, Species)

write.csv(natural_resources, "natural_resources.csv")
write.csv(natural_resources_norway, "natural_resources_norway.csv")
write.csv(natural_resources_canada, "natural_resources_canada.csv")
write.csv(natural_resources_usa, "natural_resources_usa.csv")
write.csv(natural_resources_greenland, "natural_resources_greenland.csv")
write.csv(natural_resources_russia, "natural_resources_russia.csv")

## In excel have decided to filter out only seals, narhwal and walrus for pelts and ivory. Narwhal not present in Alaska, Beaufort or mainland Norway
##Extremely limited Seal catch in Svalbard - between 20-100 total each year, issue by individual quota.
## Canadian commercial seal hunt does not operate in Nunavut/Beaufort (Newf/St John only). Trying to find catches
##Alaska no commercial landings - trying to find subsistence data. No quota for seals or walrus. 
## Lacking quotas for all areas....
