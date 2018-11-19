library("devtools")
library("ggplot2")
library("sf")
library("rworldmap")
#library("rworldxtra")
library("viridis")
library("maps")
library("tools")
#library("ggmap")
library("dplyr")
library("ggsn")

######################################## World and US Map ######################################## 
#world <- getMap(resolution = "high")
#world <- st_as_sf(world)
wmap <- st_as_sf(map(plot = FALSE, fill = TRUE), coord_sf(crs = st_crs(2163)))
usa <- st_as_sf(map("usa", plot = FALSE, fill = TRUE), coord_sf(crs = st_crs(2163))) 

######################################## Baylisascaris ######################################## 
baylis <- read.csv("baylisascaris_cases-count.csv", stringsAsFactors = FALSE)

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) ### Note, no AK or HI but they don't have baylis
states$ID <- toTitleCase(states$ID)

all.equal(sort(states$ID), sort(baylis$States)) 
states <- left_join(states, baylis, by = c("ID" = "States"))

########### Baylisascaris Cases Maps
##### NLM
NLMtitle <- expression(paste("Reported Baylisascaris Neural Larva Migrans Cases in the United States (1973-2017)"))

nlm_map <-ggplot(data = wmap) +
  geom_sf(fill = "cornsilk") +
  geom_sf(data = states, aes(fill = Cases_NLM), color = gray(.5)) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(crs = st_crs(2163), xlim = c(-2000000, 2500000), ylim = c(-2050000, 730000))+
  ggtitle (NLMtitle) +
  theme(plot.title = element_text(hjust = 0), panel.border = element_rect(colour = "black", fill=NA, size=1), 
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "lightskyblue1")) +
  scale_fill_viridis(name="Number of \nCases")
nlm_map
ggsave("baylisascaris/figure_5_nlm_cases_map.jpg", plot = nlm_map,  width = 5.45, height = 3.25, dpi = 150, scale = 2.2) 

##### OLM
OLMtitle <- expression(paste("Reported Baylisascaris Ocular Larva Migrans Cases in the United States (1949-2015)"))

olm_map <-ggplot(data = wmap) +
  geom_sf(fill = "cornsilk") +
  geom_sf(data = states, aes(fill = Cases_OLM), color = gray(.5)) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(crs = st_crs(2163), xlim = c(-2000000, 2500000), ylim = c(-2050000, 730000))+
  ggtitle (OLMtitle) +
  theme(plot.title = element_text(hjust = 0), panel.border = element_rect(colour = "black", fill=NA, size=1), 
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "lightskyblue1")) +
  scale_fill_viridis(name="Number of \nCases OLM")
olm_map

ggsave("baylisascaris/figure_6_olm_cases_map.jpg", plot = olm_map,  width = 5.45, height = 3.25, dpi = 150, scale = 2.2) 

##### VLM
VLMtitle <- expression(paste("Reported Baylisascaris Visceral Larva Migrans Cases in the United States (1994-2015)"))

vlm_map <-ggplot(data = wmap) +
  geom_sf(fill = "cornsilk") +
  geom_sf(data = states, aes(fill = Cases_VLM), color = gray(.5)) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(crs = st_crs(2163), xlim = c(-2000000, 2500000), ylim = c(-2050000, 730000))+
  ggtitle (VLMtitle) +
  theme(plot.title = element_text(hjust = 0), panel.border = element_rect(colour = "black", fill=NA, size=1), 
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "lightskyblue1")) +
  scale_fill_viridis(name="Number of \nCases VLM")
vlm_map

ggsave("baylisascaris/vlm_cases_map.jpg", plot = vlm_map,  width = 5.45, height = 3.25, dpi = 150, scale = 2.2) 


##### OLM+NLM+VLM
totaltitle <- expression(paste("Reported Clinical Baylisascaris Cases in the United States (1949-2017)"))

total_map <-ggplot(data = wmap) +
  geom_sf(fill = "cornsilk") +
  geom_sf(data = states, aes(fill = Combined), color = gray(.4)) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(crs = st_crs(2163), xlim = c(-2000000, 2500000), ylim = c(-2050000, 730000))+
  ggtitle (totaltitle) +
  theme(plot.title = element_text(hjust = 0), panel.border = element_rect(colour = "black", fill=NA, size=1), 
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "lightskyblue1")) +
  scale_fill_viridis(name="Number of \nCases")
total_map

ggsave("baylisascaris/figure_4_baylis_cases_map.jpg", plot = total_map, width = 5.45, height = 3.25, dpi = 150, scale = 2.2)
ggsave("parasites/figure_2_baylis_cases_map.jpg", plot = total_map, width = 5.45, height = 3.25, dpi = 150, scale = 2.2) 

########### Baylisascaris procyonis prevelance
prevtitle <- expression(paste("Prevalence of ", italic("Baylisascaris procyonis"), " in the United States"))

baylis_preve_map <-ggplot(data = wmap) +
  geom_sf(fill = "cornsilk") +
  geom_sf(data = states, aes(fill = Prevalence), color = gray(.4)) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(crs = st_crs(2163), xlim = c(-2000000, 2500000), ylim = c(-2050000, 730000))+
  ggtitle (prevtitle) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), 
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "lightskyblue1")) +
  scale_fill_viridis(name="Prevalence", labels = c("Absent or Unknown", "Low (<10%)", 
  "Moderate (10%-24%)", "High (25%-49%)", "Very High (>50%)"))
baylis_preve_map

ggsave("baylisascaris/figure_2_baylis_prev_map.jpg", plot = baylis_preve_map, width = 5.45, height = 3, dpi = 150, scale = 2.2) 
# for 1,000 px: ggsave("total_cases_map.jpg", plot = total_map, width = 3.33, height=1.4, dpi = 100, scale = 3) 




######################################## Rabies ######################################## 
racc_rabies<-read.csv("20-year-raccoon-rabies-by-county.csv", header=TRUE, 
                      stringsAsFactors=FALSE, check.names = FALSE)


########## County Map of Florida, Set Caps
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("florida", counties$ID))
counties$ID <- toTitleCase(counties$ID)

ggplot(data = counties) +
  geom_sf() +
  geom_sf(data = counties, fill = NA, color = gray(.5)) +
  coord_sf(xlim = c(-88.00, -79.50), ylim = c(24.50, 31.50), expand = FALSE) 

########### Join County and Rabies
## Check county names: make them all upper case, and correct a couple of typos
counties$ID <- toupper(substr(counties$ID, 9, 100))
counties$ID[counties$ID == "DE SOTO"] <- "DESOTO"
racc_rabies$County[racc_rabies$County == "DADE"] <- "MIAMI-DADE"
all.equal(counties$ID, sort(racc_rabies$County))
#racc_rabies$County_Total[racc_rabies$County_Total == "0"] <- NA

counties <- left_join(counties, racc_rabies, by = c("ID" = "County"))

rabies_map <-ggplot(data = usa) +
  geom_sf(fill = "cornsilk") +
  geom_sf(data = counties, aes(fill = County_Total), color = gray(.5)) +
  coord_sf(xlim = c(-88, -79), ylim = c(24.8, 31.5), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle ("Raccoon-borne rabies in Florida", subtitle = "Frequency of rabid raccoons per county 1997–2018") +
  theme(plot.title = element_text(hjust = 0), panel.border = element_rect(colour = "black", fill=NA, size=1), 
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "lightskyblue1")) +
  scale_fill_viridis(name="County Total")
rabies_map

ggsave("viruses/figure_3_rabies_map.jpg", plot = rabies_map, width = 2.2, height = 1.66, dpi = 150, scale = 3) 
##### No FL Keys, even if I use a maps with the keys no data shows

########## Rabies by year 
library("cowplot")
theme_update(panel.grid.major = element_line(colour = "grey90",size = 0.2))

rbsfl.df<-data.frame(year=1997:2018,t(racc_rabies[racc_rabies$County=='TOTAL',!colnames(racc_rabies)%in%
                                                   c('County','County_Total')]))
colnames(rbsfl.df)<-c('year','cases')

rabies_by_year<-ggplot(rbsfl.df,aes(x=year,y=cases)) + 
  geom_line(stat = "identity")+
  ggtitle ("Raccoon-borne rabies in Florida", 
           subtitle = "Frequency of rabid raccoons in Florida 1997–2018")+
  xlab("Year") + ylab ("Number of cases")+
  theme(plot.title = element_text(hjust = 0), plot.background = element_rect(colour = "black", fill=NA, size=1))+
  expand_limits(y=0)
rabies_by_year
ggsave("viruses/figure_4_rabies_by_year.jpg", plot = rabies_by_year, width = 2.2, height = 1.66, dpi = 150, scale = 3)

