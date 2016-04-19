library(tidyr)
library(dplyr)
Troms_Employment = read_excel("Livelihoods/Employment_Figures/Norway/Troms_Employment_2009_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Troms_Employment2 = data.frame(Troms_Employment)
Troms_Fish = Troms_Employment2 %>% filter(Job.Type == "03 Fishing and aquaculture")
Troms_Food = Troms_Employment2 %>% filter(Job.Type == "10 Food products")
Troms_Water = Troms_Employment2 %>% filter(Job.Type == "50 Water transport")
Troms_Travel = Troms_Employment2 %>% filter(Job.Type == "79 Travel agency, tour operators")

## Ocean based employment considering food products likely to be seafood related, travel tourism likely be ocean related
# Join together
Troms_Group = rbind(Troms_Fish, Troms_Food, Troms_Water, Troms_Travel)
Troms_Group2 = select(Troms_Group, -Job.Type)
Troms_Group2 = gather(Troms_Group, "Year", "Marine.Jobs", 3:8)
Troms_Group3 = select(Troms_Group2, -Job.Type)
Troms_Group3
Troms_Jobs = Troms_Group3 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Region,Year,Marine.Jobs)%>%
  group_by(Year)%>%
  dplyr::summarize(total_jobs = sum(Marine.Jobs, na.rm=T))%>%
  ungroup()
Troms_Jobs$Region <- "Troms"
troms_jobs_final=Troms_Jobs[c(3,1,2)]
troms_jobs_final

# Nordland Jobs -----------------------------------------------------------

Nordland_Employment = read_excel("Livelihoods/Employment_Figures/Norway/Nordland_Employment_2009_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Nord_Employment2 = data.frame(Nordland_Employment)
Nord_Fish = Nord_Employment2 %>% filter(Job.Type == "03 Fishing and aquaculture")
Nord_Food = Nord_Employment2 %>% filter(Job.Type == "10 Food products")
Nord_Water = Nord_Employment2 %>% filter(Job.Type == "50 Water transport")
Nord_Travel = Nord_Employment2 %>% filter(Job.Type == "79 Travel agency, tour operators")

## Ocean based employment considering food products likely to be seafood related, travel tourism likely be ocean related

# Join together
Nord_Group = rbind(Nord_Fish, Nord_Food, Nord_Water, Nord_Travel)
Nord_Group2 = select(Nord_Group, -Job.Type)
Nord_Group2 = gather(Nord_Group, "Year", "Marine.Jobs", 3:8)
Nord_Group3 = select(Nord_Group2, -Job.Type)
Nord_Group3

Nord_Jobs = Nord_Group3 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Region,Year,Marine.Jobs)%>%
  group_by(Year)%>%
  dplyr::summarize(total_jobs = sum(Marine.Jobs, na.rm=T))%>%
  ungroup()
Nord_Jobs$Region <- "Nordland"
Nord_jobs_final=Nord_Jobs[c(3,1,2)]
Nord_jobs_final

# Finnmark Jobs -----------------------------------------------------------

Finnmark_Employment = read_excel("Livelihoods/Employment_Figures/Norway/Finnmark_Employment_2009_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Finn_Employment2 = data.frame(Finnmark_Employment)
Finn_Fish = Finn_Employment2 %>% filter(Job.Type == "03 Fishing and aquaculture")
Finn_Food = Finn_Employment2 %>% filter(Job.Type == "10 Food products")
Finn_Water = Finn_Employment2 %>% filter(Job.Type == "50 Water transport")
Finn_Travel = Finn_Employment2 %>% filter(Job.Type == "79 Travel agency, tour operators")

## Ocean based employment considering food products likely to be seafood related, travel tourism likely be ocean related

# Join together
Finn_Group = rbind(Finn_Fish, Finn_Food, Finn_Water, Finn_Travel)
Finn_Group2 = select(Finn_Group, -Job.Type)
Finn_Group2 = gather(Finn_Group, "Year", "Marine.Jobs", 3:8)
Finn_Group3 = select(Finn_Group2, -Job.Type)
Finn_Group3

Finn_Jobs = Finn_Group3 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Region,Year,Marine.Jobs)%>%
  group_by(Year)%>%
  dplyr::summarize(total_jobs = sum(Marine.Jobs, na.rm=T))%>%
  ungroup()
Finn_Jobs$Region <- "Finnmark"
Finn_jobs_final=Finn_Jobs[c(3,1,2)]
Finn_jobs_final

# Norway Mainland Jobs ----------------------------------------------------

norway_main_jobs = rbind(troms_jobs_final, Nord_jobs_final, Finn_jobs_final)%>%
  group_by(Year)%>%
  dplyr::summarize(Marine.Jobs = sum(total_jobs, na.rm=T))%>%
  ungroup()
norway_main_jobs$Region <- "Norway Mainland"
norway_mainland_jobs=norway_main_jobs[c(3,1,2)]
norway_mainland_jobs

# Svalbard ----------------------------------------------------------------
Svalbard_Employment = read_excel("Livelihoods/Employment_Figures/Norway/Svalbard_Employment_2008_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Sval_Employment2 = data.frame(Svalbard_Employment)%>%
  rename(c("Sector"="Job.Type"))
Sval_Employment2<-filter(Sval_Employment2, !(Job.Type %in% c('B Mining and quarrying', 'C-D-E Manufacturing', 'F Construction',
                                                             'G Wholesale and retail trade: repair of motor vehicles and motorcycles',
                                                             'J-K Information and communication', 'L Real estate activities',
                                                             'M Professional, scientific and technical activities', 'N Administrative and support service activities',
                                                             'O Public administration and defence', 'Q Human health and social work activities',
                                                             'S Other service activities', '00 Unspecified')))
## Ocean based employment considering main ocean related employment on Svalbard is tourism and science.

# Join together

Sval_Employment2<-gather(Sval_Employment2, "Year", "Marine.Jobs", 2:8)
Sval_Employment2<-select(Sval_Employment2, -Job.Type)
Sval_Employment2

Sval_Jobs = Sval_Employment2 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Year,Marine.Jobs)%>%
  group_by(Year)%>%
  dplyr::summarize(Marine.Jobs = sum(Marine.Jobs, na.rm=T))%>%
  ungroup()
Sval_Jobs$Region <- "Svalbard"
sval_jobs_final=Sval_Jobs[c(3,1,2)]
sval_jobs_final

# Norway Jobs bind --------------------------------------------------------


norway_jobs = rbind(sval_jobs_final, norway_mainland_jobs)
norway_jobs$Marine.Jobs<- round(norway_jobs$Marine.Jobs, digits=0)
norway_jobs
write.csv(norway_jobs, "Norway_Jobs.csv")

# Canada ------------------------------------------------------------------


# NWT ---------------------------------------------------------------------
NWT_Employment = read.csv('Livelihoods/Employment_Figures/Canada/NWT_Employment_2001_2015.csv')
NWT_Employment2 = data.frame(NWT_Employment)
NWT_Employment2<-dplyr::filter(NWT_Employment2, (Job.Type %in% c("Transportation and warehousing", "Accommodation and food services", "Information, culture and recreation")))
## Assume related to ship transport, tourism, ship business

NWT_Employment2<-gather(NWT_Employment2, "Year", "Marine.Jobs", 2:16)
NWT_Employment2<-select(NWT_Employment2, -Job.Type)
NWT_Employment2

NWT_Jobs = NWT_Employment2 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Year,Marine.Jobs)%>%
  group_by(Year)%>%
  dplyr::summarize(Marine.Jobs = sum(Marine.Jobs, na.rm=T))%>%
  ungroup()
NWT_Jobs$Region <- "NWT"
NWT_jobs_final=NWT_Jobs[c(3,1,2)]
NWT_jobs_final

##Work out Arctic ocean employment as percentage of NWT to calculate Arctic Ocean area marine jobs.