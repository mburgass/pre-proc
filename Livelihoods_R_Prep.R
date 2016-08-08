library(tidyr)
library(dplyr)
library(readxl)
Troms_Employment = read_excel("Livelihoods/Employment_Figures/Norway/Troms_Employment_2009_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Troms_Employment2 = data.frame(Troms_Employment)
Troms_Fish = Troms_Employment2 %>% filter(Job.Type == "03 Fishing and aquaculture")
Troms_Fish$Job.Type<-"fishing"
Troms_Food = Troms_Employment2 %>% filter(Job.Type == "10 Food products")
Troms_Food$Job.Type<-"food"
Troms_Water = Troms_Employment2 %>% filter(Job.Type == "50 Water transport")
Troms_Water$Job.Type<- "transport"
Troms_Travel = Troms_Employment2 %>% filter(Job.Type == "79 Travel agency, tour operators")
Troms_Travel$Job.Type<- "tourism"

## Ocean based employment considering food products likely to be seafood related, travel tourism likely be ocean related
# Join together


Troms_Group = rbind(Troms_Fish, Troms_Food, Troms_Water, Troms_Travel)
Troms_Group<-dplyr::rename(Troms_Group, sector=Job.Type)
Troms_Group2 = gather(Troms_Group, "Year", "value", 3:8)

Troms_Jobs = Troms_Group2 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Region,Year,sector, value)%>%
  group_by(Year)%>%
  ungroup()
troms_jobs_final=Troms_Jobs[c(1,3,2,4)]
troms_jobs_final

# Nordland Jobs -----------------------------------------------------------

Nordland_Employment = read_excel("Livelihoods/Employment_Figures/Norway/Nordland_Employment_2009_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Nord_Employment2 = data.frame(Nordland_Employment)
Nord_Fish = Nord_Employment2 %>% filter(Job.Type == "03 Fishing and aquaculture")
Nord_Fish$Job.Type<-"fishing"
Nord_Food = Nord_Employment2 %>% filter(Job.Type == "10 Food products")
Nord_Food$Job.Type<-"food"
Nord_Water = Nord_Employment2 %>% filter(Job.Type == "50 Water transport")
Nord_Water$Job.Type<- "transport"
Nord_Travel = Nord_Employment2 %>% filter(Job.Type == "79 Travel agency, tour operators")
Nord_Travel$Job.Type<- "tourism"
## Ocean based employment considering food products likely to be seafood related, travel tourism likely be ocean related

# Join together
Nord_Group = rbind(Nord_Fish, Nord_Food, Nord_Water, Nord_Travel)
Nord_Group<-dplyr::rename(Nord_Group, sector=Job.Type)
Nord_Group2 = gather(Nord_Group, "Year", "value", 3:8)

Nord_Jobs = Nord_Group2 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Region,Year,sector, value)%>%
  group_by(Year)%>%
  ungroup()
nord_jobs_final=Nord_Jobs[c(1,3,2,4)]
nord_jobs_final$Region<-"Nordland"

# Finnmark Jobs -----------------------------------------------------------

Finnmark_Employment = read_excel("Livelihoods/Employment_Figures/Norway/Finnmark_Employment_2009_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Finn_Employment2 = data.frame(Finnmark_Employment)
Finn_Fish = Finn_Employment2 %>% filter(Job.Type == "03 Fishing and aquaculture")
Finn_Fish$Job.Type<-"fishing"
Finn_Food = Finn_Employment2 %>% filter(Job.Type == "10 Food products")
Finn_Food$Job.Type<-"food"
Finn_Water = Finn_Employment2 %>% filter(Job.Type == "50 Water transport")
Finn_Water$Job.Type<- "transport"
Finn_Travel = Finn_Employment2 %>% filter(Job.Type == "79 Travel agency, tour operators")
Finn_Travel$Job.Type<- "tourism"

## Ocean based employment considering food products likely to be seafood related, travel tourism likely be ocean related

# Join together
Finn_Group = rbind(Finn_Fish, Finn_Food, Finn_Water, Finn_Travel)
Finn_Group<-dplyr::rename(Finn_Group, sector=Job.Type)
Finn_Group2 = gather(Finn_Group, "Year", "value", 3:8)

Finn_Jobs = Finn_Group2 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Region,Year,sector, value)%>%
  group_by(Year)%>%
  ungroup()
Finn_jobs_final=Finn_Jobs[c(1,3,2,4)]
Finn_jobs_final$Region<-"Finnmark"

# Norway Mainland Jobs ----------------------------------------------------

norway_main_jobs = rbind(troms_jobs_final, nord_jobs_final, Finn_jobs_final)%>%
  group_by(Year, sector)%>%
  dplyr::summarize(value = sum(value, na.rm=T))%>%
  ungroup()
norway_main_jobs$rgn_id <- "Norway Mainland"
norway_mainland_jobs=norway_main_jobs[c(4,2,1,3)]
norway_mainland_jobs

# Svalbard ----------------------------------------------------------------
Svalbard_Employment = read_excel("Livelihoods/Employment_Figures/Norway/Svalbard_Employment_2008_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Sval_Employment2 = data.frame(Svalbard_Employment)%>%
  rename(sector=Sector)
Sval_Employment2<-filter(Sval_Employment2, !(sector %in% c('B Mining and quarrying', 'C-D-E Manufacturing', 'F Construction',
                                                             'G Wholesale and retail trade: repair of motor vehicles and motorcycles',
                                                             'J-K Information and communication', 'L Real estate activities',
                                                             'M Professional, scientific and technical activities', 'N Administrative and support service activities',
                                                             'O Public administration and defence', 'Q Human health and social work activities',
                                                             'S Other service activities', '00 Unspecified')))
## Ocean based employment considering main ocean related employment on Svalbard is tourism and science.

# Join together

Sval_Employment2<-gather(Sval_Employment2, "Year", "value", 2:8)


Sval_Jobs = Sval_Employment2 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Year,sector,value)
  
Sval_Jobs$rgn_id <- "Svalbard"
sval_jobs_final=Sval_Jobs[c(4,2,1,3)]
sval_jobs_final$sector<-as.character(sval_jobs_final$sector) ## change to character
sval_jobs_final[sval_jobs_final=="P Education"]<- "education"
sval_jobs_final[sval_jobs_final=="H Transportation and storage"]<-"transport"
sval_jobs_final[sval_jobs_final=="I Accommodation and food service activities"]<-"hospitality"
sval_jobs_final[sval_jobs_final=="R Arts, entertainment and recreation"]<-"tourism"

# Norway Jobs bind --------------------------------------------------------


norway_jobs = rbind(sval_jobs_final, norway_mainland_jobs)
norway_jobs$value<- round(norway_jobs$value, digits=0)
norway_jobs

##Norway workforce done manually in excel - summed together statistics on number of unemployed in each municipality with total employment from data above
##Unemployment rate then worked out as a % of unemployed in workforce.

##Norway and Svalbard Wages

norway_wages = read_excel("Livelihoods/Employment_Figures/Norway/norway_wages_2009_2015.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
norway_wages2 = data.frame(norway_wages)%>%
  rename(sector=Sector)
norway_wages2<-filter(norway_wages2, !(sector %in% c('B Mining and quarrying', 'C Manufacturing', 'D Electricity, gas, steam and air conditioning supply', 'E Water supply, sewerage, waste', 'F Construction',
                                                           'G Wholesale and retail trade: repair of motor vehicles and motorcycles',
                                                           'J Information and communication', 'K Financial and insurance activities', 'L Real estate activities',
                                                           'M Professional, scientific and technical activities', 'N Administrative and support service activities',
                                                           'O Public administration and defence', 'Q Human health and social work activities',
                                                           'S Other service activities')))

##Sectors to match those on mainland and svalbard

norway_wages2<-gather(norway_wages2, "Year", "value", 2:8)


norway_wages3 = norway_wages2 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Year,sector,value)


norway_wages3$sector<-as.character(norway_wages3$sector) ## change to character
norway_wages3[norway_wages3=="A Agriculture, forestry and fishing"]<- "fishing"
norway_wages3[norway_wages3=="P Education"]<- "education"
norway_wages3[norway_wages3=="H Transportation and storage"]<-"transport"
norway_wages3[norway_wages3=="I Accommodation and food service activities"]<-"hospitality"
norway_wages3[norway_wages3=="R Arts, entertainment and recreation"]<-"tourism"

##write.csv(norway_wages3, "le_wages_sector_year_arc2016.csv")

##Multiplied values in excel by 12 to give annual values in NOK - need to convert to USD.


# Norway GRP --------------------------------------------------------------

Norway_gdp = read_excel("Livelihoods/Economies/Raw/Norway GDP.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Norway_gdp<- tbl_df(Norway_gdp)%>%
  gather("year", "value", 3:8)
Norway_gdp[Norway_gdp=="Fishing and aquaculture"]<- "fishing"
Norway_gdp[Norway_gdp=="Ocean transport"]<-"transport"
Norway_gdp[Norway_gdp=="Accommodation and food service activities"]<-"hospitality"
Norway_gdp[Norway_gdp=="Arts, entertainment and other service activities"]<-"tourism"
Norway_gdp[Norway_gdp=="Education"]<-"education"
Norway_gdp[Norway_gdp=="Transport activities excl. ocean transport"]<-"transport"
Svalbard_gdp = filter(Norway_gdp, region %in% c("Svalbard"))
Norway_gdp<- filter(Norway_gdp, !(region %in% c("Svalbard")))
Norway_gdp<- filter(Norway_gdp, !(sector %in% c("education"))) #remove education
Norway_gdp<- Norway_gdp %>% group_by(sector, year)%>%
  dplyr::summarize(value = sum(value, na.rm=T))%>%
  ungroup()
Norway_gdp$rgn_id<- "6"
Norway_gdp$value<- Norway_gdp$value *1000000
Svalbard_gdp<- Svalbard_gdp %>% group_by(sector, year)%>%
  dplyr::summarize(value = sum(value, na.rm=T))%>%
  ungroup()
Svalbard_gdp<- filter(Svalbard_gdp, value >0)
Svalbard_gdp$value<- Svalbard_gdp$value*1000000
Svalbard_gdp$rgn_id<- "5"
Norway_gdp<- rbind(Norway_gdp, Svalbard_gdp)
##write.csv(Norway_gdp, "Norway_gdp.csv")

# Canada ------------------------------------------------------------------


# NWT ---------------------------------------------------------------------
NWT_Employment = read.csv('Livelihoods/Employment_Figures/Canada/NWT_Employment_2001_2015.csv')
NWT_Employment2 = data.frame(NWT_Employment)%>%
  rename(sector=Job.Type)
NWT_Employment2<-dplyr::filter(NWT_Employment2, (sector %in% c("Transportation and warehousing", "Accommodation and food services", "Information, culture and recreation")))
## Assume related to ship transport, tourism, ship business

NWT_Employment2<-gather(NWT_Employment2, "Year", "value", 2:16)

NWT_Employment2

NWT_Jobs = NWT_Employment2 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Year,sector,value)
NWT_Jobs$Region <- "Beaufort"
NWT_jobs_final=NWT_Jobs[c(4,2,1,3)]
NWT_jobs_final$sector<-as.character(NWT_jobs_final$sector) 
NWT_jobs_final[NWT_jobs_final=="Transportation and warehousing"]<- "transport"
NWT_jobs_final[NWT_jobs_final=="Information, culture and recreation"]<-"tourism"
NWT_jobs_final[NWT_jobs_final=="Accommodation and food services"]<-"hospitality"

##Beaufort Delta area accounts for ~14.2% of NWT employment based on data from
##C:\Users\MB4514\Documents\github\pre-proc\Livelihoods\Employment_Figures\Raw files\Canada\Community Labour Force Activity, 1986 to 2014.xlsx
##Now calculate Beaufort Delta region based on 14.2% of NWT_jobs_final

NWT_jobs_final=dplyr::mutate(NWT_jobs_final, value = (NWT_jobs_final$value/100)*14.2)
NWT_jobs_final$value<- round(NWT_jobs_final$value, digits=0)

###### Total Workforce
NWT_Employment = read.csv('Livelihoods/Employment_Figures/Canada/NWT_Employment_2001_2015.csv')
NWT_workforce = data.frame(NWT_Employment)%>%
  rename(sector=Job.Type)
NWT_workforce<- gather(NWT_workforce, "Year", "value", 2:16)
NWT_workforce<- NWT_workforce %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Year,sector,value)
NWT_workforce<- NWT_workforce%>%group_by(Year)%>%
  dplyr::summarize(value = sum(value, na.rm=T))%>%
  ungroup()
NWT_workforce$Region <- "Beaufort"
NWT_workforce<- dplyr::mutate(NWT_workforce, value = (NWT_workforce$value/100)*14.2)
NWT_workforce$value<- round(NWT_workforce$value, digits=0)


# Nunavut -----------------------------------------------------------------

Nunavut_Employment = read_excel("Livelihoods/Employment_Figures/Canada/Nunavut_Employment_2008_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Nunavut_Employment<-data.frame(Nunavut_Employment)%>% rename(sector=Job.Type)

Nunavut_Employment2 = dplyr::filter(Nunavut_Employment, (sector %in% c("Fishing, Hunting, Trapping, Mining and Quarrying", "Transportation and Warehousing", "Accommodation and Food Services")))
## Job types quite general - taken ones with most likely marine connections.

Nunavut_Employment2<-gather(Nunavut_Employment2, "Year", "value", 2:8)

Nunavut_Jobs = Nunavut_Employment2 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%
  select(Year, sector, value)
Nunavut_Jobs$Region <- "Nunavut"
Nunavut_jobs_final=Nunavut_Jobs[c(4,2,1,3)]
Nunavut_jobs_final$sector<-as.character(Nunavut_jobs_final$sector) 
Nunavut_jobs_final[Nunavut_jobs_final=="Transportation and Warehousing"]<- "transport"
Nunavut_jobs_final[Nunavut_jobs_final=="Fishing, Hunting, Trapping, Mining and Quarrying"]<-"fishing"
Nunavut_jobs_final[Nunavut_jobs_final=="Accommodation and Food Services"]<-"hospitality"

##Total Workforce
Nunavut_workforce=Nunavut_Employment
Nunavut_workforce<- gather(Nunavut_workforce, "Year", "value", 2:8)
Nunavut_workforce<- Nunavut_workforce %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Year,sector,value)
Nunavut_workforce<- Nunavut_workforce%>%group_by(Year)%>%
  dplyr::summarize(value = sum(value, na.rm=T))%>%
  ungroup()
Nunavut_workforce$Region <- "Nunavut"

## Join with beaufort
canada_workforce = rbind(NWT_workforce, Nunavut_workforce)
canada_workforce<- rename(canada_workforce, rgn_id=Region)

##Manaully added in excel to increase to total workforce based on the unemployment rate

##Added Canada wages in excel

### ---- Canada GDP ----
Nunavut_gdp = read_excel("Livelihoods/Economies/Raw/Nunavut_2004_2013.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Nunavut_gdp<- tbl_df(Nunavut_gdp) %>%
  select(-Value) %>%
  gather("year", "value", 2:11)
Nunavut_gdp<- filter(Nunavut_gdp, sector %in% c("Fishing, hunting and trapping [114] ", "Accommodation and food services [72] ", "Transportation and warehousing [48-49] "))
Nunavut_gdp[Nunavut_gdp == "Fishing, hunting and trapping [114] "]<- "fishing"
Nunavut_gdp[Nunavut_gdp == "Transportation and warehousing [48-49] "]<- "transport"
Nunavut_gdp[Nunavut_gdp == "Accommodation and food services [72] "] <- "hospitality"
Nunavut_gdp$value<-as.numeric(Nunavut_gdp$value)
Nunavut_gdp$rgn_id<- "2"

NWT_gdp = read_excel("Livelihoods/Economies/Raw/NWT_2004_2013.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
NWT_gdp<- tbl_df(NWT_gdp) %>%
  select(-Value) %>%
  gather("year", "value", 2:11)
NWT_gdp<- filter(NWT_gdp, sector %in% c("Arts, entertainment and recreation [71] ", "Accommodation and food services [72] ", "Transportation and warehousing [48-49] "))
NWT_gdp[NWT_gdp == "Arts, entertainment and recreation [71] "]<- "tourism"
NWT_gdp[NWT_gdp == "Transportation and warehousing [48-49] "]<- "transport"
NWT_gdp[NWT_gdp == "Accommodation and food services [72] "] <- "hospitality"
NWT_gdp$value<-as.numeric(NWT_gdp$value)
NWT_gdp$rgn_id<- "3"

canada_gdp = rbind(NWT_gdp, Nunavut_gdp)
canada_gdp$value<- canada_gdp$value *1000000
##write.csv(canada_gdp, "canada_gdp.csv")

## In excel adjusted NWT to take 14.2% of GRP for Beaufort Delta. 
# Canada Join -------------------------------------------------------------

canada_jobs = rbind(NWT_jobs_final, Nunavut_jobs_final)
canada_jobs<- rename(canada_jobs, rgn_id=Region)


# Greenland ---------------------------------------------------------------

## Only figures for whole country
Greenland_Employment = read_excel("Livelihoods/Employment_Figures/Greenland/Greenland_Employment_2008_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Greenland_Employment<-data.frame(Greenland_Employment)%>% rename(sector=Job.Type)
Greenland_Employment2 = dplyr::filter(Greenland_Employment, (sector %in% c("Fishing, hunting & agriculture", "Transportation", "Hotels and restaurants")))
##Jobs quite general - kept with theme of fishing/ship transport and tourism
Greenland_Employment2=rename(Greenland_Employment2, value=Jobs)

Greenland_Employment2$rgn_id <- "Greenland"
Greenland_jobs_final=Greenland_Employment2[c(4,2,1,3)]
Greenland_jobs_final[Greenland_jobs_final=="Transportation"]<- "transport"
Greenland_jobs_final[Greenland_jobs_final=="Fishing, hunting & agriculture"]<-"fishing"
Greenland_jobs_final[Greenland_jobs_final=="Hotels and restaurants"]<-"hospitality"

##Split in excel into West and East Greenland based on 80% population in West Greenland.

##Total Workforce
Greenland_workforce = Greenland_Employment
Greenland_workforce<- Greenland_workforce%>%group_by(Year)%>%
  dplyr::summarize(value = sum(Jobs, na.rm=T))%>%
  ungroup()
##write.csv(Greenland_workforce, "Greenland_workforce.csv")
##Added in unemployment figures to file to calculate total workforce. Unemployment rate then calculated and added to unemployment rate file

##Wages added manaully in excel from files

### ---- Greenland GDP ----

Greenland_gdp = read_excel("Livelihoods/Economies/Raw/Greenland_2003_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Greenland_gdp<-tbl_df(Greenland_gdp)%>%
  gather(year, value, 2:13)
Greenland_gdp[Greenland_gdp =="Shipping"]<- "transport"
Greenland_gdp[Greenland_gdp =="Agriculture, fishing, hunting, etc."]<-"fishing"
Greenland_gdp[Greenland_gdp =="Hotels and restaurants"]<-"hospitality"
Greenland_gdp$value<- Greenland_gdp$value *1000000
##write.csv(Greenland_gdp, "Greenland_gdp.csv")

##Split 80/20 between West and East Greenland in excel

# USA/Alaska --------------------------------------------------------------


# 2014 --------------------------------------------------------------------

Alaska_Employment14 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 3, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment14<-data.frame(Alaska_Employment14)
Alaska_Employment14 = dplyr::filter(Alaska_Employment14, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment14 <- dplyr::tbl_df(Alaska_Employment14)
Alaska_Employment14 <- dplyr::select(Alaska_Employment14, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment14 <- dplyr::filter(Alaska_Employment14, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment14 <- dplyr::filter(Alaska_Employment14, (NA. %in% c("NATURAL RESOURCES AND MINING", "MANUFACTURING", "TRADE, TRANSPORTATION AND UTILITIES", "LEISURE AND HOSPITALITY" )))
Alaska_Employment14$Year <- "2014"
Alaska_Employment14

Alaska_workforce14 =  read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 3, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_workforce14<-data.frame(Alaska_workforce14)
Alaska_workforce14 = dplyr::filter(Alaska_workforce14, (AREA.CODE %in% c("000185", "000188")))
Alaska_workforce14  <- dplyr::tbl_df(Alaska_workforce14)
Alaska_workforce14 <- dplyr::select(Alaska_workforce14, AREANAME, NAICS.DESCRIPTION, AVERAGE.EMPLOYMENT)
Alaska_workforce14 <- dplyr::filter(Alaska_workforce14, (NAICS.DESCRIPTION %in% c("TOTAL INDUSTRIES")))
Alaska_workforce14$Year <- "2014"

Alaska_wages14 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 3, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_wages14<-data.frame(Alaska_wages14)
Alaska_wages14 = dplyr::filter(Alaska_wages14, (AREA.CODE %in% c("000185", "000188")))
Alaska_wages14 <- dplyr::select(Alaska_wages14, AREANAME, NA.,  MONTHLY.AVG.WAGES)
Alaska_wages14<- dplyr::filter(Alaska_wages14, (NA. %in% c("MANUFACTURING", "TRADE, TRANSPORTATION AND UTILITIES", "LEISURE AND HOSPITALITY" )))
Alaska_wages14$Year <- "2014"


##Not really any marine jobs listed in these areas. National figures don't seem to be matched by the regional figures though..?


# 2013 --------------------------------------------------------------------
Alaska_Employment13 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 4, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment13<-data.frame(Alaska_Employment13)
Alaska_Employment13 = dplyr::filter(Alaska_Employment13, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment13 <- dplyr::tbl_df(Alaska_Employment13)
Alaska_Employment13 <- dplyr::select(Alaska_Employment13, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment13 <- dplyr::filter(Alaska_Employment13, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment13 <- dplyr::filter(Alaska_Employment13, (NA. %in% c("NATURAL RESOURCES AND MINING", "MANUFACTURING", "TRADE, TRANSPORTATION AND UTILITIES", "LEISURE AND HOSPITALITY" )))
Alaska_Employment13$Year <- "2013"
Alaska_Employment13

Alaska_workforce13 =  read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 4, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_workforce13<-data.frame(Alaska_workforce13)
Alaska_workforce13= dplyr::filter(Alaska_workforce13, (AREA.CODE %in% c("000185", "000188")))
Alaska_workforce13 <- dplyr::tbl_df(Alaska_workforce13)
Alaska_workforce13<- dplyr::select(Alaska_workforce13, AREANAME, NAICS.DESCRIPTION, AVERAGE.EMPLOYMENT)
Alaska_workforce13 <- dplyr::filter(Alaska_workforce13, (NAICS.DESCRIPTION %in% c("TOTAL INDUSTRIES")))
Alaska_workforce13$Year <- "2013"

Alaska_wages13 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 4, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_wages13<-data.frame(Alaska_wages13)
Alaska_wages13 = dplyr::filter(Alaska_wages13, (AREA.CODE %in% c("000185", "000188")))
Alaska_wages13 <- dplyr::select(Alaska_wages13, AREANAME, NA.,  MONTHLY.AVG.WAGES)
Alaska_wages13<- dplyr::filter(Alaska_wages13, (NA. %in% c("MANUFACTURING", "TRADE, TRANSPORTATION AND UTILITIES", "LEISURE AND HOSPITALITY" )))
Alaska_wages13$Year <- "2013"

# 2012 --------------------------------------------------------------------
Alaska_Employment12 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 5, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment12<-data.frame(Alaska_Employment12)
Alaska_Employment12 = dplyr::filter(Alaska_Employment12, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment12 <- dplyr::tbl_df(Alaska_Employment12)
Alaska_Employment12 <- dplyr::select(Alaska_Employment12, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment12 <- dplyr::filter(Alaska_Employment12, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment12 <- dplyr::filter(Alaska_Employment12, (NA. %in% c("NATURAL RESOURCES AND MINING", "MANUFACTURING", "TRADE, TRANSPORTATION AND UTILITIES", "LEISURE AND HOSPITALITY" )))
Alaska_Employment12$Year <- "2012"
Alaska_Employment12

Alaska_workforce12 =  read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 5, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_workforce12<-data.frame(Alaska_workforce12)
Alaska_workforce12= dplyr::filter(Alaska_workforce12, (AREA.CODE %in% c("000185", "000188")))
Alaska_workforce12 <- dplyr::tbl_df(Alaska_workforce12)
Alaska_workforce12<- dplyr::select(Alaska_workforce12, AREANAME, NAICS.DESCRIPTION, AVERAGE.EMPLOYMENT)
Alaska_workforce12 <- dplyr::filter(Alaska_workforce12, (NAICS.DESCRIPTION %in% c("TOTAL INDUSTRIES")))
Alaska_workforce12$Year <- "2012"

Alaska_wages12 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 5, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_wages12<-data.frame(Alaska_wages12)
Alaska_wages12 = dplyr::filter(Alaska_wages12, (AREA.CODE %in% c("000185", "000188")))
Alaska_wages12 <- dplyr::select(Alaska_wages12, AREANAME, NA.,  MONTHLY.AVG.WAGES)
Alaska_wages12<- dplyr::filter(Alaska_wages12, (NA. %in% c("MANUFACTURING", "TRADE, TRANSPORTATION AND UTILITIES", "LEISURE AND HOSPITALITY" )))
Alaska_wages12$Year <- "2012"

# 2011 --------------------------------------------------------------------
Alaska_Employment11 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 6, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment11<-data.frame(Alaska_Employment11)
Alaska_Employment11 = dplyr::filter(Alaska_Employment11, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment11 <- dplyr::tbl_df(Alaska_Employment11)
Alaska_Employment11 <- dplyr::select(Alaska_Employment11, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment11 <- dplyr::filter(Alaska_Employment11, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment11 <- dplyr::filter(Alaska_Employment11, (NA. %in% c("NATURAL RESOURCES & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
##Note slight difference in Job types i.e. use of & rather than and. To correct later
Alaska_Employment11$Year <- "2011"
Alaska_Employment11

Alaska_workforce11 =  read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 6, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_workforce11<-data.frame(Alaska_workforce11)
Alaska_workforce11= dplyr::filter(Alaska_workforce11, (AREA.CODE %in% c("000185", "000188")))
Alaska_workforce11<- dplyr::tbl_df(Alaska_workforce11)
Alaska_workforce11<- dplyr::select(Alaska_workforce11, AREANAME, NAICS.DESCRIPTION, AVERAGE.EMPLOYMENT)
Alaska_workforce11 <- dplyr::filter(Alaska_workforce11, (NAICS.DESCRIPTION %in% c("TOTAL INDUSTRIES")))
Alaska_workforce11$Year <- "2011"

Alaska_wages11 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 6, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_wages11<-data.frame(Alaska_wages11)
Alaska_wages11 = dplyr::filter(Alaska_wages11, (AREA.CODE %in% c("000185", "000188")))
Alaska_wages11 <- dplyr::select(Alaska_wages11, AREANAME, NA.,  MONTHLY.AVG.WAGES)
Alaska_wages11<- dplyr::filter(Alaska_wages11, (NA. %in% c("MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_wages11$Year <- "2011"

# 2010 --------------------------------------------------------------------
Alaska_Employment10 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 7, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment10<-data.frame(Alaska_Employment10)
Alaska_Employment10 = dplyr::filter(Alaska_Employment10, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment10 <- dplyr::tbl_df(Alaska_Employment10)
Alaska_Employment10 <- dplyr::select(Alaska_Employment10, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment10 <- dplyr::filter(Alaska_Employment10, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment10 <- dplyr::filter(Alaska_Employment10, (NA. %in% c("NATURAL RESOURCES & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment10$Year <- "2010"
Alaska_Employment10

Alaska_workforce10 =  read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 7, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_workforce10<-data.frame(Alaska_workforce10)
Alaska_workforce10= dplyr::filter(Alaska_workforce10, (AREA.CODE %in% c("000185", "000188")))
Alaska_workforce10<- dplyr::tbl_df(Alaska_workforce10)
Alaska_workforce10<- dplyr::select(Alaska_workforce10, AREANAME, NAICS.DESCRIPTION, AVERAGE.EMPLOYMENT)
Alaska_workforce10 <- dplyr::filter(Alaska_workforce10, (NAICS.DESCRIPTION %in% c("TOTAL INDUSTRIES")))
Alaska_workforce10$Year <- "2010"

Alaska_wages10 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 7, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_wages10<-data.frame(Alaska_wages10)
Alaska_wages10 = dplyr::filter(Alaska_wages10, (AREA.CODE %in% c("000185", "000188")))
Alaska_wages10 <- dplyr::select(Alaska_wages10, AREANAME, NA.,  MONTHLY.AVG.WAGES)
Alaska_wages10<- dplyr::filter(Alaska_wages10, (NA. %in% c("MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_wages10$Year <- "2010"

# 2009 --------------------------------------------------------------------
Alaska_Employment09 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 8, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment09<-data.frame(Alaska_Employment09)
Alaska_Employment09 = dplyr::filter(Alaska_Employment09, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment09 <- dplyr::tbl_df(Alaska_Employment09)
Alaska_Employment09 <- dplyr::select(Alaska_Employment09, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment09 <- dplyr::filter(Alaska_Employment09, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment09 <- dplyr::filter(Alaska_Employment09, (NA. %in% c("NATURAL RESOURCES & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment09$Year <- "2009"
Alaska_Employment09

Alaska_workforce09 =  read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 8, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_workforce09<-data.frame(Alaska_workforce09)
Alaska_workforce09= dplyr::filter(Alaska_workforce09, (AREA.CODE %in% c("000185", "000188")))
Alaska_workforce09<- dplyr::tbl_df(Alaska_workforce09)
Alaska_workforce09<- dplyr::select(Alaska_workforce09, AREANAME, NAICS.DESCRIPTION, AVERAGE.EMPLOYMENT)
Alaska_workforce09 <- dplyr::filter(Alaska_workforce09, (NAICS.DESCRIPTION %in% c("TOTAL INDUSTRIES")))
Alaska_workforce09$Year <- "2009"

Alaska_wages09 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 8, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_wages09<-data.frame(Alaska_wages09)
Alaska_wages09 = dplyr::filter(Alaska_wages09, (AREA.CODE %in% c("000185", "000188")))
Alaska_wages09 <- dplyr::select(Alaska_wages09, AREANAME, NA.,  MONTHLY.AVG.WAGES)
Alaska_wages09<- dplyr::filter(Alaska_wages09, (NA. %in% c("MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_wages09$Year <- "2009"

# 2008 --------------------------------------------------------------------
Alaska_Employment08 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 9, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment08<-data.frame(Alaska_Employment08)
Alaska_Employment08 = dplyr::filter(Alaska_Employment08, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment08 <- dplyr::tbl_df(Alaska_Employment08)
Alaska_Employment08 <- dplyr::select(Alaska_Employment08, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment08 <- dplyr::filter(Alaska_Employment08, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment08 <- dplyr::filter(Alaska_Employment08, (NA. %in% c("NATURAL RESOURCES & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment08$Year <- "2008"
Alaska_Employment08

Alaska_workforce08 =  read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 9, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_workforce08<-data.frame(Alaska_workforce08)
Alaska_workforce08= dplyr::filter(Alaska_workforce08, (AREA.CODE %in% c("000185", "000188")))
Alaska_workforce08<- dplyr::tbl_df(Alaska_workforce08)
Alaska_workforce08<- dplyr::select(Alaska_workforce08, AREANAME, NAICS.DESCRIPTION, AVERAGE.EMPLOYMENT)
Alaska_workforce08 <- dplyr::filter(Alaska_workforce08, (NAICS.DESCRIPTION %in% c("TOTAL INDUSTRIES")))
Alaska_workforce08$Year <- "2008"

Alaska_wages08 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 9, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_wages08<-data.frame(Alaska_wages08)
Alaska_wages08 = dplyr::filter(Alaska_wages08, (AREA.CODE %in% c("000185", "000188")))
Alaska_wages08 <- dplyr::select(Alaska_wages08, AREANAME, NA.,  MONTHLY.AVG.WAGES)
Alaska_wages08<- dplyr::filter(Alaska_wages08, (NA. %in% c("MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_wages08$Year <- "2008"



# 2007 --------------------------------------------------------------------
Alaska_Employment07 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 10, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment07<-data.frame(Alaska_Employment07)
Alaska_Employment07 = dplyr::filter(Alaska_Employment07, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment07 <- dplyr::tbl_df(Alaska_Employment07)
Alaska_Employment07 <- dplyr::select(Alaska_Employment07, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment07 <- dplyr::filter(Alaska_Employment07, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment07 <- dplyr::filter(Alaska_Employment07, (NA. %in% c("NATURAL RESOURCES & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment07$Year <- "2007"
Alaska_Employment07

Alaska_workforce07 =  read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 10, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_workforce07<-data.frame(Alaska_workforce07)
Alaska_workforce07= dplyr::filter(Alaska_workforce07, (AREA.CODE %in% c("000185", "000188")))
Alaska_workforce07<- dplyr::tbl_df(Alaska_workforce07)
Alaska_workforce07<- dplyr::select(Alaska_workforce07, AREANAME, NAICS.DESCRIPTION, AVERAGE.EMPLOYMENT)
Alaska_workforce07 <- dplyr::filter(Alaska_workforce07, (NAICS.DESCRIPTION %in% c("TOTAL INDUSTRIES")))
Alaska_workforce07$Year <- "2007"

Alaska_wages07 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 10, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_wages07<-data.frame(Alaska_wages07)
Alaska_wages07 = dplyr::filter(Alaska_wages07, (AREA.CODE %in% c("000185", "000188")))
Alaska_wages07 <- dplyr::select(Alaska_wages07, AREANAME, NA.,  MONTHLY.AVG.WAGES)
Alaska_wages07<- dplyr::filter(Alaska_wages07, (NA. %in% c("MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_wages07$Year <- "2007"


# 2006 --------------------------------------------------------------------
Alaska_Employment06 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 11, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment06<-data.frame(Alaska_Employment06)
Alaska_Employment06 = dplyr::filter(Alaska_Employment06, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment06 <- dplyr::tbl_df(Alaska_Employment06)
Alaska_Employment06 <- dplyr::select(Alaska_Employment06, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment06 <- dplyr::filter(Alaska_Employment06, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment06 <- dplyr::filter(Alaska_Employment06, (NA. %in% c("NATURAL RESOURCES & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment06$Year <- "2006"
Alaska_Employment06

Alaska_workforce06 =  read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 11, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_workforce06<-data.frame(Alaska_workforce06)
Alaska_workforce06= dplyr::filter(Alaska_workforce06, (AREA.CODE %in% c("000185", "000188")))
Alaska_workforce06<- dplyr::tbl_df(Alaska_workforce06)
Alaska_workforce06<- dplyr::select(Alaska_workforce06, AREANAME, NAICS.DESCRIPTION, AVERAGE.EMPLOYMENT)
Alaska_workforce06 <- dplyr::filter(Alaska_workforce06, (NAICS.DESCRIPTION %in% c("TOTAL INDUSTRIES")))
Alaska_workforce06$Year <- "2006"

Alaska_wages06 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 11, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_wages06<-data.frame(Alaska_wages06)
Alaska_wages06 = dplyr::filter(Alaska_wages06, (AREA.CODE %in% c("000185", "000188")))
Alaska_wages06 <- dplyr::select(Alaska_wages06, AREANAME, NA.,  MONTHLY.AVG.WAGES)
Alaska_wages06<- dplyr::filter(Alaska_wages06, (NA. %in% c("MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_wages06$Year <- "2006"



# 2005 --------------------------------------------------------------------
Alaska_Employment05 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 12, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment05<-data.frame(Alaska_Employment05)
Alaska_Employment05 = dplyr::filter(Alaska_Employment05, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment05 <- dplyr::tbl_df(Alaska_Employment05)
Alaska_Employment05 <- dplyr::select(Alaska_Employment05, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment05 <- dplyr::filter(Alaska_Employment05, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment05 <- dplyr::filter(Alaska_Employment05, (NA. %in% c("NATURAL RESOURCE & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment05$Year <- "2005"
Alaska_Employment05

Alaska_workforce05 =  read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 12, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_workforce05<-data.frame(Alaska_workforce05)
Alaska_workforce05= dplyr::filter(Alaska_workforce05, (AREA.CODE %in% c("000185", "000188")))
Alaska_workforce05<- dplyr::tbl_df(Alaska_workforce05)
Alaska_workforce05<- dplyr::select(Alaska_workforce05, AREANAME, NAICS.DESCRIPTION, AVERAGE.EMPLOYMENT)
Alaska_workforce05 <- dplyr::filter(Alaska_workforce05, (NAICS.DESCRIPTION %in% c("TOTAL INDUSTRIES")))
Alaska_workforce05$Year <- "2005"

Alaska_wages05 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 12, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_wages05<-data.frame(Alaska_wages05)
Alaska_wages05 = dplyr::filter(Alaska_wages05, (AREA.CODE %in% c("000185", "000188")))
Alaska_wages05 <- dplyr::select(Alaska_wages05, AREANAME, NA.,  MONTHLY.AVG.WAGES)
Alaska_wages05<- dplyr::filter(Alaska_wages05, (NA. %in% c("MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_wages05$Year <- "2005"

##Slight name change in Resource rather than Resources

# 2004 --------------------------------------------------------------------
Alaska_Employment04 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 13, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment04<-data.frame(Alaska_Employment04)
Alaska_Employment04 = dplyr::filter(Alaska_Employment04, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment04 <- dplyr::tbl_df(Alaska_Employment04)
Alaska_Employment04 <- dplyr::select(Alaska_Employment04, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment04 <- dplyr::filter(Alaska_Employment04, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment04 <- dplyr::filter(Alaska_Employment04, (NA. %in% c("NATURAL RESOURCE & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment04$Year <- "2004"
Alaska_Employment04

Alaska_workforce04 =  read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 13, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_workforce04<-data.frame(Alaska_workforce04)
Alaska_workforce04= dplyr::filter(Alaska_workforce04, (AREA.CODE %in% c("000185", "000188")))
Alaska_workforce04<- dplyr::tbl_df(Alaska_workforce04)
Alaska_workforce04<- dplyr::select(Alaska_workforce04, AREANAME, NAICS.DESCRIPTION, AVERAGE.EMPLOYMENT)
Alaska_workforce04 <- dplyr::filter(Alaska_workforce04, (NAICS.DESCRIPTION %in% c("TOTAL INDUSTRIES")))
Alaska_workforce04$Year <- "2004"

Alaska_wages04 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 13, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_wages04<-data.frame(Alaska_wages04)
Alaska_wages04 = dplyr::filter(Alaska_wages04, (AREA.CODE %in% c("000185", "000188")))
Alaska_wages04 <- dplyr::select(Alaska_wages04, AREANAME, NA.,  MONTHLY.AVG.WAGES)
Alaska_wages04<- dplyr::filter(Alaska_wages04, (NA. %in% c("MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_wages04$Year <- "2004"


# Alaska join and sort ----------------------------------------------------
Alaska_Group = rbind(Alaska_Employment04, Alaska_Employment05, Alaska_Employment06, Alaska_Employment07, Alaska_Employment08, Alaska_Employment09, Alaska_Employment10, Alaska_Employment11, Alaska_Employment12, Alaska_Employment13, Alaska_Employment14)
Alaska_Group<- select(Alaska_Group, Year, AREANAME, NA., AVERAGE.EMPLOYMENT)
Alaska_Group<-filter(Alaska_Group, !(NA. %in% c('NATURAL RESOURCE & MINING', 'NATURAL RESOURCES & MINING', 'NATURAL RESOURCES AND MINING', 'MANUFACTURING')))
##Dropped natural resource and mining and manufacturing as detailed check of data shows that it is all related to oil/gas/mining - not fishing or trapping
Alaska_Group<-select(Alaska_Group, Year, AVERAGE.EMPLOYMENT, NA.)
Alaska_Group<-dplyr::rename(Alaska_Group, value=AVERAGE.EMPLOYMENT, sector=NA.)
Alaska_Jobs=Alaska_Group
Alaska_Jobs$rgn_id<- "Alaska"
Alaska_Jobs<-Alaska_Jobs[c(4,3,1,2)]
Alaska_Jobs[Alaska_Jobs=="TRADE, TRANSPORTATION AND UTILITIES"]<- "transport" 
Alaska_Jobs[Alaska_Jobs=="TRADE, TRANS. & UTILITIES"]<- "transport"
Alaska_Jobs[Alaska_Jobs=="MANUFACTURING"]<-"manufacturing"
Alaska_Jobs[Alaska_Jobs=="LEISURE & HOSPITALITY"]<-"hospitality"
Alaska_Jobs[Alaska_Jobs=="LEISURE AND HOSPITALITY"]<-"hospitality"

Alaska_workforce = rbind(Alaska_workforce04, Alaska_workforce05, Alaska_workforce06, Alaska_workforce07, Alaska_workforce08, Alaska_workforce09, Alaska_workforce10, Alaska_workforce11, Alaska_workforce12, Alaska_workforce13, Alaska_workforce14)
Alaska_workforce$rgn_id<- "Alaska"
##write.csv(Alaska_workforce, "Alaska_workforce.csv")
##Total workforce worked out in excel by using unemployment rate for the two boroughs. Average unemployment for Alaska worked out by finding out number
##of unemployed in each borough using the unemployment rates and then working out % unemployed compared to total workforce across both boroughs.

Alaska_wages= rbind(Alaska_wages04, Alaska_wages05, Alaska_wages06, Alaska_wages07, Alaska_wages08, Alaska_wages09, Alaska_wages10, Alaska_wages11, Alaska_wages12, Alaska_wages13, Alaska_wages14)
Alaska_wages$rgn_id<- "1"
Alaska_wages<-filter(Alaska_wages, !(NA. %in% c('MANUFACTURING'))) #remove manufacturing
Alaska_wages[Alaska_wages=="TRADE, TRANSPORTATION AND UTILITIES"]<- "transport"
Alaska_wages[Alaska_wages=="TRADE, TRANS. & UTILITIES"]<- "transport"
Alaska_wages[Alaska_wages=="LEISURE & HOSPITALITY"]<-"hospitality"
Alaska_wages[Alaska_wages=="LEISURE AND HOSPITALITY"]<-"hospitality"
##write.csv(Alaska_wages, "Alaska_wages.csv")
##In excel work out a weighted average for monthly wage between the boroughs based on how many employed in that sector in that borough. Then multiply monthly values by 12 to give annual

### ---- Alaska Economies ----

Alaska_gdp = read_excel("Livelihoods/Economies/Raw/Alaska_GDP_1997_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_gdp<- tbl_df(Alaska_gdp) %>%
  select(-Area, -IndCode) %>%
  filter(Industry %in% c("Trade", "Transportation and utilities", "    Arts, entertainment, recreation, accommodation, and food services")) %>%
  gather("year", "value", 2:19)
Alaska_gdp[Alaska_gdp == "Trade"]<- "transport"
Alaska_gdp[Alaska_gdp == "Transportation and utilities"]<- "transport"
Alaska_gdp[Alaska_gdp == "    Arts, entertainment, recreation, accommodation, and food services"] <- "hospitality"
Alaska_gdp$value<-as.numeric(Alaska_gdp$value)
Alaska_gdp$value<- Alaska_gdp$value * 1000000
Alaska_gdp<- Alaska_gdp %>%
  group_by(Industry, year) %>%
  dplyr::summarize(value = sum(value, na.rm=T))%>%
  ungroup()
Alaska_gdp$rgn_id<- "1"
## need to reduce based on population size of north slope (9,687) and northwest arctic borough (7,752) = 17,439 compared to Alaska (738,432)
## 17, 439/738,432 * 100 = 2.36 %
Alaska_gdp$value<- Alaska_gdp$value * 0.236
##write.csv(Alaska_gdp, "Alaska_gdp.csv")

# Russia ------------------------------------------------------------------

###----Russian GRP----###

Russia_GRP = read_excel("Livelihoods/Economies/Raw/Russia/Russia GRP.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_GRP = tbl_df(Russia_GRP)
Russia_GRP<- filter(Russia_GRP, Region %in% c("Arhangelsk region", "Murmansk region", "Yamalo-Nenets Autonomous Okrug", "Krasnoyarsk region", "The Republic of Sakha (Yakutia)", "Chukotka Autonomous Okrug"))%>%
  gather("year", "value", 2:18)
Russia_GRP$value<- as.numeric(Russia_GRP$value)
Russia_GRP$value<- Russia_GRP$value * 1000000
Russia_GRP<- filter(Russia_GRP, year %in% c ("2008", "2009", "2010", "2011", "2012", "2013", "2014"))
Russia_GRP<- rename(Russia_GRP, region = Region)

Russia_GRP_14 = read_excel("Livelihoods/Economies/Raw/Russia/GRP per sector Russia.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_GRP_14 = tbl_df(Russia_GRP_14)
Russia_GRP_14<- filter(Russia_GRP_14, Region %in% c("Arhangelsk region", "Murmansk region", "Yamalo-Nenets Autonomous Okrug", "Krasnoyarsk region", "The Republic of Sakha (Yakutia)", "Chukotka Autonomous Okrug"))
Russia_GRP_14<- select(Russia_GRP_14, Region, Fishing, `Hotels and restaurants`, `Transport and communications`)
Russia_GRP_14$year<- "2014"
Russia_GRP_14<-Russia_GRP_14[c(5,1,2,3,4)]
Russia_GRP_14<- gather(Russia_GRP_14, "sector", "value", 3:5)

Russia_GRP_13 = read_excel("Livelihoods/Economies/Raw/Russia/GRP per sector Russia.xlsx", sheet = 2, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_GRP_13 = tbl_df(Russia_GRP_13)
Russia_GRP_13<- filter(Russia_GRP_13, Region %in% c("Arhangelsk region", "Murmansk region", "Yamalo-Nenets Autonomous Okrug", "Krasnoyarsk region", "The Republic of Sakha (Yakutia)", "Chukotka Autonomous Okrug"))
Russia_GRP_13<- select(Russia_GRP_13, Region, Fishing, `Hotels and restaurants`, `Transport and communications`)
Russia_GRP_13$year<- "2013"
Russia_GRP_13<-Russia_GRP_13[c(5,1,2,3,4)]
Russia_GRP_13<- gather(Russia_GRP_13, "sector", "value", 3:5)

Russia_GRP_12 = read_excel("Livelihoods/Economies/Raw/Russia/GRP per sector Russia.xlsx", sheet = 3, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_GRP_12 = tbl_df(Russia_GRP_12)
Russia_GRP_12<- filter(Russia_GRP_12, Region %in% c("Arhangelsk region", "Murmansk region", "Yamalo-Nenets Autonomous Okrug", "Krasnoyarsk region", "The Republic of Sakha (Yakutia)", "Chukotka Autonomous Okrug"))
Russia_GRP_12<- select(Russia_GRP_12, Region, Fishing, `Hotels and restaurants`, `Transport and communications`)
Russia_GRP_12$year<- "2012"
Russia_GRP_12<-Russia_GRP_12[c(5,1,2,3,4)]
Russia_GRP_12<- gather(Russia_GRP_12, "sector", "value", 3:5)

Russia_GRP_11 = read_excel("Livelihoods/Economies/Raw/Russia/GRP per sector Russia.xlsx", sheet = 4, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_GRP_11 = tbl_df(Russia_GRP_11)
Russia_GRP_11<- filter(Russia_GRP_11, Region %in% c("Arhangelsk region", "Murmansk region", "Yamalo-Nenets Autonomous Okrug", "Krasnoyarsk region", "The Republic of Sakha (Yakutia)", "Chukotka Autonomous Okrug"))
Russia_GRP_11<- select(Russia_GRP_11, Region, Fishing, `Hotels and restaurants`, `Transport and communications`)
Russia_GRP_11$year<- "2011"
Russia_GRP_11<-Russia_GRP_11[c(5,1,2,3,4)]
Russia_GRP_11<- gather(Russia_GRP_11, "sector", "value", 3:5)

Russia_GRP_10 = read_excel("Livelihoods/Economies/Raw/Russia/GRP per sector Russia.xlsx", sheet = 5, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_GRP_10 = tbl_df(Russia_GRP_10)
Russia_GRP_10<- filter(Russia_GRP_10, Region %in% c("Arhangelsk region", "Murmansk region", "Yamalo-Nenets Autonomous Okrug", "Krasnoyarsk region", "The Republic of Sakha (Yakutia)", "Chukotka Autonomous Okrug"))
Russia_GRP_10<- select(Russia_GRP_10, Region, Fishing, `Hotels and restaurants`, `Transport and communications`)
Russia_GRP_10$year<- "2010"
Russia_GRP_10<-Russia_GRP_10[c(5,1,2,3,4)]
Russia_GRP_10<- gather(Russia_GRP_10, "sector", "value", 3:5)

Russia_GRP_09 = read_excel("Livelihoods/Economies/Raw/Russia/GRP per sector Russia.xlsx", sheet = 6, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_GRP_09 = tbl_df(Russia_GRP_09)
Russia_GRP_09<- filter(Russia_GRP_09, Region %in% c("Arhangelsk region", "Murmansk region", "Yamalo-Nenets Autonomous Okrug", "Krasnoyarsk region", "The Republic of Sakha (Yakutia)", "Chukotka Autonomous Okrug"))
Russia_GRP_09<- select(Russia_GRP_09, Region, Fishing, `Hotels and restaurants`, `Transport and communications`)
Russia_GRP_09$year<- "2009"
Russia_GRP_09<-Russia_GRP_09[c(5,1,2,3,4)]
Russia_GRP_09<- gather(Russia_GRP_09, "sector", "value", 3:5)

Russia_GRP_08 = read_excel("Livelihoods/Economies/Raw/Russia/GRP per sector Russia.xlsx", sheet = 7, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_GRP_08 = tbl_df(Russia_GRP_08)
Russia_GRP_08<- filter(Russia_GRP_08, Region %in% c("Arhangelsk region", "Murmansk region", "Yamalo-Nenets Autonomous Okrug", "Krasnoyarsk region", "The Republic of Sakha (Yakutia)", "Chukotka Autonomous Okrug"))
Russia_GRP_08<- select(Russia_GRP_08, Region, Fishing, `Hotels and restaurants`, `Transport and communications`)
Russia_GRP_08$year<- "2008"
Russia_GRP_08<-Russia_GRP_08[c(5,1,2,3,4)]
Russia_GRP_08<- gather(Russia_GRP_08, "sector", "value", 3:5)

Russia_GRP_Sector = rbind(Russia_GRP_08, Russia_GRP_09, Russia_GRP_10, Russia_GRP_11, Russia_GRP_12, Russia_GRP_13, Russia_GRP_14)
Russia_GRP_Sector$sector[Russia_GRP_Sector$sector =="Fishing"]<- "fishing"
Russia_GRP_Sector$sector[Russia_GRP_Sector$sector =="Hotels and restaurants"]<- "hospitality"
Russia_GRP_Sector$sector[Russia_GRP_Sector$sector =="Transport and communications"]<- "transport"
Russia_GRP_Sector<- rename(Russia_GRP_Sector, region = Region)

## Take GRP for each region and % contribution from each sector to work out GRP per sector per region

Russia_GRP_total= right_join(Russia_GRP, Russia_GRP_Sector, by = c("region", "year"))
Russia_GRP_total<- mutate(Russia_GRP_total, value = (Russia_GRP_total$value.y/100)*Russia_GRP_total$value.x)%>%
  select(-value.x, -value.y)
Russia_GRP_total$value[Russia_GRP_total$region =="Chukotka Autonomous Okrug"] <-Russia_GRP_total$value[Russia_GRP_total$region =="Chukotka Autonomous Okrug"]/2.2758
Russia_GRP_total$value[Russia_GRP_total$region =="Krasnoyarsk region"] <-Russia_GRP_total$value[Russia_GRP_total$region =="Krasnoyarsk region"]/83.33333
Russia_GRP_total$value[Russia_GRP_total$region =="The Republic of Sakha (Yakutia)"] <-Russia_GRP_total$value[Russia_GRP_total$region =="The Republic of Sakha (Yakutia)"]/33.7838
Russia_GRP_total<- Russia_GRP_total%>%group_by(year, sector)%>%
  dplyr::summarize(value = sum(value, na.rm=T))%>%
  ungroup()
Russia_GRP_total$rgn_id<- "4"
Russia_GRP_total<-Russia_GRP_total[c(4,1,2,3)]
##write.csv(Russia_GRP_total, "Russia_gdp.csv")

##Work out % employment in each region so can amend jobs - Krasnoyarsk region is huge.

##Taymyrsky Dolgano-Nenetsky District in Krasnoyarsk krai = population 34,432. KK population = 2,828,187. 1.2% of the data below
## Chukotsky, Iulintsky, Chaunsky and Bilibinksy districts in Chukotka = poppulation = 22,201. Chukotka pop = 50,526. 43.94%
##Sakha coastal districts = population 28,325. sakha pop = 958, 528. 2.96%

# 2014 --------------------------------------------------------------------

Russia_Employment14 = read_excel("Livelihoods/Employment_Figures/Russia/Russia_Employment_2010_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_Employment14<-data.frame(Russia_Employment14)
Russia_Employment14 <- dplyr::tbl_df(Russia_Employment14)
Russia_Employment14 <- dplyr::filter(Russia_Employment14, !(District %in% c("North-western Federal District", "Including Nenets Autonomous Okrug", "Without Nenets", "Ural Federal District", "Siberian Federal District", "Far Eastern Federal District", "Republic of Kaerlia")))
Russia_Employment14<- mutate(Russia_Employment14, "Transport" = Transport.and...communication-of.them...communication)
Russia_Employment14<- dplyr::select(Russia_Employment14, District, Agriculture..hunting.and.forestry..fishing..fish.farming, Hotels.and...restaurants, Transport)
Russia_Employment14$Year <- "2014"
Russia_Employment14$Transport<-Russia_Employment14$Transport*1000
Russia_Employment14$Agriculture..hunting.and.forestry..fishing..fish.farming<-Russia_Employment14$Agriculture..hunting.and.forestry..fishing..fish.farming*1000
Russia_Employment14$Hotels.and...restaurants<-Russia_Employment14$Hotels.and...restaurants*1000

##Total Workforce
Russia_workforce14 = read_excel("Livelihoods/Employment_Figures/Russia/Russia_Employment_2010_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_workforce14<-data.frame(Russia_workforce14)
Russia_workforce14 <- dplyr::filter(Russia_workforce14, !(District %in% c("North-western Federal District", "Including Nenets Autonomous Okrug", "Without Nenets", "Ural Federal District", "Siberian Federal District", "Far Eastern Federal District", "Republic of Kaerlia")))
Russia_workforce14<- select(Russia_workforce14, -of.them...communication)
Russia_workforce14$Year <- "2014"
Russia_workforce14$value<- rowSums(Russia_workforce14 [,2:14])
Russia_workforce14<- select(Russia_workforce14, District, Year, value)
Russia_workforce14$value<- Russia_workforce14$value*1000

# #2013 -------------------------------------------------------------------

Russia_Employment13 = read_excel("Livelihoods/Employment_Figures/Russia/Russia_Employment_2010_2014.xlsx", sheet = 2, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_Employment13<-data.frame(Russia_Employment13)
Russia_Employment13 <- dplyr::tbl_df(Russia_Employment13)
Russia_Employment13 <- dplyr::filter(Russia_Employment13, !(District %in% c("North-western Federal District", "Including Nenets Autonomous Okrug", "Without Nenets", "Ural Federal District", "Siberian Federal District", "Far Eastern Federal District", "Republic of Kaerlia")))
Russia_Employment13<- mutate(Russia_Employment13, "Transport" = Transport.and...communication-of.them...communication)
Russia_Employment13<- dplyr::select(Russia_Employment13, District, Agriculture..hunting.and.forestry..fishing..fish.farming, Hotels.and...restaurants, Transport)
Russia_Employment13$Year <- "2013"
Russia_Employment13$Transport<-Russia_Employment13$Transport*1000
Russia_Employment13$Agriculture..hunting.and.forestry..fishing..fish.farming<-Russia_Employment13$Agriculture..hunting.and.forestry..fishing..fish.farming*1000
Russia_Employment13$Hotels.and...restaurants<-Russia_Employment13$Hotels.and...restaurants*1000

Russia_workforce13 = read_excel("Livelihoods/Employment_Figures/Russia/Russia_Employment_2010_2014.xlsx", sheet = 2, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_workforce13<-data.frame(Russia_workforce13)
Russia_workforce13 <- dplyr::filter(Russia_workforce13, !(District %in% c("North-western Federal District", "Including Nenets Autonomous Okrug", "Without Nenets", "Ural Federal District", "Siberian Federal District", "Far Eastern Federal District", "Republic of Kaerlia")))
Russia_workforce13<- select(Russia_workforce13, -of.them...communication)
Russia_workforce13$Year <- "2013"
Russia_workforce13$value<- rowSums(Russia_workforce13 [,2:14])
Russia_workforce13<- select(Russia_workforce13, District, Year, value)
Russia_workforce13$value<- Russia_workforce13$value*1000

# #2012 -------------------------------------------------------------------
Russia_Employment12 = read_excel("Livelihoods/Employment_Figures/Russia/Russia_Employment_2010_2014.xlsx", sheet = 3, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_Employment12<-data.frame(Russia_Employment12)
Russia_Employment12 <- dplyr::tbl_df(Russia_Employment12)
Russia_Employment12 <- dplyr::filter(Russia_Employment12, !(District %in% c("North-western Federal District", "Including Nenets Autonomous Okrug", "Without Nenets", "Ural Federal District", "Siberian Federal District", "Far Eastern Federal District", "Republic of Kaerlia")))
Russia_Employment12<- mutate(Russia_Employment12, "Transport" = Transport.and...communication-of.them...communication)
Russia_Employment12<- dplyr::select(Russia_Employment12, District, Agriculture..hunting.and.forestry..fishing..fish.farming, Hotels.and...restaurants, Transport)
Russia_Employment12$Year <- "2012"
Russia_Employment12$Transport<-Russia_Employment12$Transport*1000
Russia_Employment12$Agriculture..hunting.and.forestry..fishing..fish.farming<-Russia_Employment12$Agriculture..hunting.and.forestry..fishing..fish.farming*1000
Russia_Employment12$Hotels.and...restaurants<-Russia_Employment12$Hotels.and...restaurants*1000

Russia_workforce12 = read_excel("Livelihoods/Employment_Figures/Russia/Russia_Employment_2010_2014.xlsx", sheet = 3, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_workforce12<-data.frame(Russia_workforce12)
Russia_workforce12 <- dplyr::filter(Russia_workforce12, !(District %in% c("North-western Federal District", "Including Nenets Autonomous Okrug", "Without Nenets", "Ural Federal District", "Siberian Federal District", "Far Eastern Federal District", "Republic of Kaerlia")))
Russia_workforce12<- select(Russia_workforce12, -of.them...communication)
Russia_workforce12$Year <- "2012"
Russia_workforce12$value<- rowSums(Russia_workforce12 [,2:14])
Russia_workforce12<- select(Russia_workforce12, District, Year, value)
Russia_workforce12$value<- Russia_workforce12$value*1000


# #2011 -------------------------------------------------------------------
Russia_Employment11 = read_excel("Livelihoods/Employment_Figures/Russia/Russia_Employment_2010_2014.xlsx", sheet = 4, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_Employment11<-data.frame(Russia_Employment11)
Russia_Employment11 <- dplyr::tbl_df(Russia_Employment11)
Russia_Employment11 <- dplyr::filter(Russia_Employment11, !(District %in% c("North-western Federal District", "Including Nenets Autonomous Okrug", "Without Nenets", "Ural Federal District", "Siberian Federal District", "Far Eastern Federal District", "Republic of Kaerlia")))
Russia_Employment11<- mutate(Russia_Employment11, "Transport" = Transport.and...communication-of.them...communication)
Russia_Employment11<- dplyr::select(Russia_Employment11, District, Agriculture..hunting.and.forestry..fishing..fish.farming, Hotels.and...restaurants, Transport)
Russia_Employment11$Year <- "2011"
Russia_Employment11$Transport<-Russia_Employment11$Transport*1000
Russia_Employment11$Agriculture..hunting.and.forestry..fishing..fish.farming<-Russia_Employment11$Agriculture..hunting.and.forestry..fishing..fish.farming*1000
Russia_Employment11$Hotels.and...restaurants<-Russia_Employment11$Hotels.and...restaurants*1000

Russia_workforce11 = read_excel("Livelihoods/Employment_Figures/Russia/Russia_Employment_2010_2014.xlsx", sheet = 4, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_workforce11<-data.frame(Russia_workforce11)
Russia_workforce11 <- dplyr::filter(Russia_workforce11, !(District %in% c("North-western Federal District", "Including Nenets Autonomous Okrug", "Without Nenets", "Ural Federal District", "Siberian Federal District", "Far Eastern Federal District", "Republic of Kaerlia")))
Russia_workforce11<- select(Russia_workforce11, -of.them...communication)
Russia_workforce11$Year <- "2011"
Russia_workforce11$value<- rowSums(Russia_workforce11 [,2:14])
Russia_workforce11<- select(Russia_workforce11, District, Year, value)
Russia_workforce11$value<- Russia_workforce11$value*1000
# 2010 --------------------------------------------------------------------
Russia_Employment10 = read_excel("Livelihoods/Employment_Figures/Russia/Russia_Employment_2010_2014.xlsx", sheet = 5, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_Employment10<-data.frame(Russia_Employment10)
Russia_Employment10 <- dplyr::tbl_df(Russia_Employment10)
Russia_Employment10 <- dplyr::filter(Russia_Employment10, !(District %in% c("North-western Federal District", "Including Nenets Autonomous Okrug", "Without Nenets", "Ural Federal District", "Siberian Federal District", "Far Eastern Federal District", "Republic of Kaerlia")))
Russia_Employment10<- mutate(Russia_Employment10, "Transport" = Transport.and...communication-of.them...communication)
Russia_Employment10<- dplyr::select(Russia_Employment10, District, Agriculture..hunting.and.forestry..fishing..fish.farming, Hotels.and...restaurants, Transport)
Russia_Employment10$Year <- "2010"
Russia_Employment10$Transport<-Russia_Employment10$Transport*1000
Russia_Employment10$Agriculture..hunting.and.forestry..fishing..fish.farming<-Russia_Employment10$Agriculture..hunting.and.forestry..fishing..fish.farming*1000
Russia_Employment10$Hotels.and...restaurants<-Russia_Employment10$Hotels.and...restaurants*1000

Russia_workforce10 = read_excel("Livelihoods/Employment_Figures/Russia/Russia_Employment_2010_2014.xlsx", sheet = 5, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_workforce10<-data.frame(Russia_workforce10)
Russia_workforce10 <- dplyr::filter(Russia_workforce10, !(District %in% c("North-western Federal District", "Including Nenets Autonomous Okrug", "Without Nenets", "Ural Federal District", "Siberian Federal District", "Far Eastern Federal District", "Republic of Kaerlia")))
Russia_workforce10<- select(Russia_workforce10, -of.them...communication)
Russia_workforce10$Year <- "2010"
Russia_workforce10$value<- rowSums(Russia_workforce10 [,2:14])
Russia_workforce10<- select(Russia_workforce10, District, Year, value)
Russia_workforce10$value<- Russia_workforce10$value*1000

# # bind ------------------------------------------------------------------

Russia_Group = rbind(Russia_Employment10, Russia_Employment11, Russia_Employment12, Russia_Employment13, Russia_Employment14)
Russia_Group<- gather(Russia_Group, sector, value, 2:4)
Russia_Group$value<-as.numeric(Russia_Group$value)
Russia_Group$sector[Russia_Group$sector =="Agriculture..hunting.and.forestry..fishing..fish.farming"] <-"fishing"
Russia_Group$sector[Russia_Group$sector =="Hotels.and...restaurants"]<- "hospitality"
Russia_Group$sector[Russia_Group$sector =="Transport"]<- "transport"

Russia_Group$value[Russia_Group$District =="Chukotka Autonomous Okrug"] <-Russia_Group$value[Russia_Group$District =="Chukotka Autonomous Okrug"]/2.2758
Russia_Group$value[Russia_Group$District =="Krasnoyarsk region"] <-Russia_Group$value[Russia_Group$District =="Krasnoyarsk region"]/83.33333
Russia_Group$value[Russia_Group$District =="The Republic of Sakha (Yakutia)"] <-Russia_Group$value[Russia_Group$District =="The Republic of Sakha (Yakutia)"]/33.7838
Russia_Group$value<- round(Russia_Group$value, digits=0)
##Work out % employment in each region so can amend jobs - Krasnoyarsk region is huge.

##Taymyrsky Dolgano-Nenetsky District in Krasnoyarsk krai = population 34,432. KK population = 2,828,187. 1.2% of the data below
## Chukotsky, Iulintsky, Chaunsky and Bilibinksy districts in Chukotka = poppulation = 22,201. Chukotka pop = 50,526. 43.94%
##Sakha coastal districts = population 28,325. sakha pop = 958, 528. 2.96%

Russia_Jobs=Russia_Group%>%group_by(Year, sector)%>%
  dplyr::summarize(value = sum(value, na.rm=T))%>%
  ungroup()
Russia_Jobs$rgn_id <- "Russia"
Russia_Jobs<-Russia_Jobs[c(4,2,1,3)]
Russia_Jobs

##BIND RUSSIA WORKFORCE
Russia_workforce = rbind(Russia_workforce10, Russia_workforce11, Russia_workforce12, Russia_workforce13, Russia_workforce14)
Russia_workforce$value<-as.numeric(Russia_workforce$value)
Russia_workforce$value[Russia_workforce$District =="Chukotka Autonomous Okrug"] <-Russia_workforce$value[Russia_workforce$District =="Chukotka Autonomous Okrug"]/2.2758
Russia_workforce$value[Russia_workforce$District =="Krasnoyarsk region"] <-Russia_workforce$value[Russia_workforce$District =="Krasnoyarsk region"]/83.33333
Russia_workforce$value[Russia_workforce$District =="The Republic of Sakha (Yakutia)"] <-Russia_workforce$value[Russia_workforce$District =="The Republic of Sakha (Yakutia)"]/33.7838
Russia_workforce$value<- round(Russia_workforce$value, digits=0)

##write.csv(Russia_workforce, "russia_workforce.csv")

##Total workforce calculated in excel with unemployment rate. ((100-unemployment rate)/100) = % employed. value/%employed = total workforce
## total workforce * unemployment rate/100 = unemployed people for each region for each year. Sum employed and unemployed per year to work out average employment rate for Russia study site. Added to unemployment rate file.
##sum regions to give russian workforce / year

##Russia wages worked out in excel from a weighted average of regional wage data. all sectors receive same wage.


##Reduce workforce size for large regions by % of Arctic regions

le_jobs_sector_year= rbind(Russia_Jobs, Alaska_Jobs, canada_jobs, norway_jobs, Greenland_jobs_final)
le_jobs_sector_year
##write.csv(le_jobs_sector_year, "le_jobs_sector_year.csv")


# World Bank Adjusted GDP -------------------------------------------------

##Read in PPP factor
ppp_factor= read_excel("Livelihoods/World_Bank/PPP_factor.xls", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)

ppp_factor<- filter(ppp_factor, `Country Name` %in% c("Canada", "Greenland", "Norway", "Russian Federation")) %>%
  gather("year", "rate", 5:60)
ppp_factor<- select(ppp_factor, `Country Name`, year, rate)%>%
  rename(country= `Country Name`)
ppp_factor<- data.frame(ppp_factor)
ppp_factor$year<- as.integer(ppp_factor$year)

##Read in Canada

canada_gdp= read.csv("Livelihoods/Economies/csv/canada_gdp.csv") ##Canada = 2004 to 2013
canada_ppp<- filter(ppp_factor, country == "Canada")
canada_ppp<- filter(canada_ppp, year %in% c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013))
canada_gdp$rgn_id<- as.character(canada_gdp$rgn_id)
canada_gdp$sector<- as.character(canada_gdp$sector) ##tidy up data frame.
canada_adjusted= full_join(canada_gdp, canada_ppp, by="year")

canada_adjusted<- mutate(canada_adjusted, value2= value/rate)
canada_adjusted<- select(canada_adjusted, sector, year, rgn_id, value2)%>%
  rename(value=value2)

##Read in Russia
russia_gdp= read.csv("Livelihoods/Economies/csv/Russia_gdp.csv") ##russia = 2008-2014

russia_ppp<- filter(ppp_factor, country == "Russian Federation")
russia_ppp<- filter(russia_ppp, year %in% c(2008, 2009, 2010, 2011, 2012, 2013, 2014))
russia_gdp$rgn_id<- as.character(russia_gdp$rgn_id)
russia_gdp$sector<- as.character(russia_gdp$sector) ##tidy up data frame.
russia_adjusted= full_join(russia_gdp, russia_ppp, by="year")

russia_adjusted<- mutate(russia_adjusted, value2= value/rate)
russia_adjusted<- select(russia_adjusted, sector, year, rgn_id, value2)%>%
  rename(value=value2)

##Read in Norway
norway_gdp= read.csv("Livelihoods/Economies/csv/Norway_gdp.csv") ##norway = 2008-2013
norway_ppp<- filter(ppp_factor, country == "Norway")
norway_ppp<- filter(norway_ppp, year %in% c(2008, 2009, 2010, 2011, 2012, 2013))
norway_gdp$rgn_id<- as.character(norway_gdp$rgn_id)
norway_gdp$sector<- as.character(norway_gdp$sector) ##tidy up data frame.
norway_adjusted= full_join(norway_gdp, norway_ppp, by="year")

norway_adjusted<- mutate(norway_adjusted, value2= value/rate)
norway_adjusted<- select(norway_adjusted, sector, year, rgn_id, value2)%>%
  rename(value=value2)

## Read in Greenland
greenland_gdp= read.csv("Livelihoods/Economies/csv/Greenland_gdp.csv") ##greenland = 2003-2013
xchange_rate=read_excel("Livelihoods/World_Bank/Exchange_rate.xls") ##Read in exchange rate for Greenland as PPP data not available
xchange_rate<- filter(xchange_rate, `Country Name` %in% c("Greenland")) %>%
  gather("year", "rate", 5:60)
xchange_rate<- select(xchange_rate, `Country Name`, year, rate)%>%
  rename(country= `Country Name`)
xchange_rate<- data.frame(xchange_rate)
xchange_rate$year<- as.integer(xchange_rate$year)

greenland_rate<- filter(xchange_rate, country == "Greenland")
greenland_rate<- filter(greenland_rate, year %in% c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013))
greenland_gdp$rgn_id<- as.character(greenland_gdp$rgn_id)
greenland_gdp$sector<- as.character(greenland_gdp$sector) ##tidy up data frame.
greenland_adjusted= full_join(greenland_gdp, greenland_rate, by="year")

greenland_adjusted<- mutate(greenland_adjusted, value2= value/rate)
greenland_adjusted<- select(greenland_adjusted, sector, year, rgn_id, value2)%>%
  rename(value=value2)


### join together
gdp_adjusted= rbind(canada_adjusted, norway_adjusted, greenland_adjusted, russia_adjusted)
Alaska_gdp<-Alaska_gdp[c(1,2,4,3)]
gdp_adjusted<- rbind(gdp_adjusted, Alaska_gdp)


# World Bank Adjusted - Wages ---------------------------------------------

#Canada Wages - 2001-2015
canada_ppp2<- filter(ppp_factor, country == "Canada")
canada_ppp2<- filter(canada_ppp, year %in% c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013))
