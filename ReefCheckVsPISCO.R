#Comparing Reef Check and PISCO fish data
#Reniel B. Cabral
#4/3/2018
#Project 1: professional scientists vs. reef check volunteers

library(sp) # A package the Maptools uses to draw polygons.
library(rgeos) # A package that Maptools uses.
library(maps) # Commands for mapping.
library(maptools)  # For working with shapefiles.

library("sp","rgdal")
library(spatstat)

library(dplyr)
library(leaflet)
library(ggplot2)
library(reshape)

library(ggpubr)#for stat_compare_means()

rm(list=ls(all=TRUE))
library(data.table)


#PATH = "H:/Jenn Project 2017" #  #Put  name of file that has both CA and OR catch here
#dataIN = fread(paste(PATH,"SBC reserve data all.txt",sep="/"))
dataIN = fread("H:/Jenn Project 2017/SBC reserve data all.txt") #this is already a calculated data of pisco and not the raw data!
head(dataIN,3)
table(dataIN$site)

country.mp <- readShapeLines("H:/MPA_project/cinms_py2/cinms_line2.shp")

#ok, we used this
MPA <- readShapeSpatial("H:/MPA_project/MPAboundary/scampa.shp")
summary(MPA)


# maps <- readShapeLines("H:/MPA_project/MLPA_studyregion/MAN_CA_MLPA_StudyRegionBoundaries.shp")
# plot(maps, axes=TRUE, col="gray")
# 
# #source:ftp://ftp.dfg.ca.gov/R7_MR/MANAGEMENT/MPA/
# MPA_Calif<-readShapeSpatial("H:/Jenn Project 2017/MPA coords/MPA_CA_Existing_160301.shp", )                            
# #gps<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# #proj4string(MPA_Calif) <- gps
# #proj4string(MPA_Calif) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# #coordinates(MPA_Calif)[,1]
# #get projection information
# #summary(MPA_Calif)
# #head(MPA_Calif)
# #plot(coordinates(MPA_Calif)[,1],coordinates(MPA_Calif)[,2], axes=TRUE)
# #  "+proj=longlat +ellps=WGS84 +datum=WGS84"))
# #plot(MPA_Calif)


dataIN<-as.data.frame(dataIN)
typeof(dataIN)
head(dataIN)

#subset
coordSAMPLE<-dataIN[c("lon_wgs84","lat_wgs84","RESERVE")]
coordSAMPLE<-unique(coordSAMPLE)
coordSAMPLE<-coordSAMPLE[complete.cases(coordSAMPLE), ] #remove rows with NAs 
head(coordSAMPLE)

#PLOT coordinates
plot(MPA, axes=TRUE, border="lightgray", col=c("azure","azure","bisque"))
points(coordSAMPLE$lat_wgs84,coordSAMPLE$lon_wgs84)
lines(country.mp, axes=TRUE, col="gray")

#PLOT ReefCheck coordinates - fish channel islands
#ReefcheckFISH<-read.csv(paste(PATH,"ReefCheck/RCCA_fish_data.csv",sep="/"))
ReefcheckFISH<-read.csv("H:/Jenn Project 2017/ReefCheck/RCCA_fish_data.csv")
head(ReefcheckFISH)

table(ReefcheckFISH$Species)
# barred sand bass black and yellow      black perch   black rockfish       blacksmith    blue rockfish         bocaccio   brown rockfish 
# 47101            47135            48719            47220            48575            49133            46786            46794 
# cabezon   china rockfish  copper rockfish        garibaldi   giant sea bass  gopher rockfish   grass rockfish       horn shark 
# 46804            46784            46846            97307             8332            47085            46796            46788 
# kelp bass   kelp greenling    kelp rockfish          lingcod          opaleye       pile perch    rainbow perch   rock greenling 
# 52309           124351            47858            46881            47152            47370            46928            46785 
# rock wrasse  rubberlip perch            sargo         senorita        sheephead    striped perch         treefish vermilion/canary 
# 142966            46905            46821            49469           143230            48195            93588            46822 
# yellowtail/olive     yoy rockfish 
# 47040            41553 
#what are the species of interets?
#barred sand bass - Paralabrax nebulifer
#black perch - Embiotoca jacksoni
#black rockfish - Sebastes melanops
#blacksmith - Chromis punctipinnis





plot(ReefcheckFISH$Lon,ReefcheckFISH$Lat)

plot(MPA, axes=TRUE, border="lightgray", col=c("azure","azure","bisque"))
points(ReefcheckFISH$Lon,ReefcheckFISH$Lat)
lines(country.mp, axes=TRUE, col="gray")

ReefCheckLocation<-unique(ReefcheckFISH[c("Lon","Lat")])
head(ReefCheckLocation)

library(htmltools)
###THIS IS FOR CHECKING WHAT MPAs TO USE (i.e., if there are ReefCheck and PISCO pairs for comparison)
leaflet() %>%
  #addProviderTiles(providers$OpenStreetMap, group = "Open Street Map") %>%
  #addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satellite Imagery") %>%
  addTiles() %>%
  #addPolygons(data = MPA, fillColor = "transparent", color = "black", weight = 2, group = "No-net zone")%>%
  addCircleMarkers(data = ReefCheckLocation, ~Lon, ~Lat, radius = 3, color = "red") %>%
  addCircleMarkers(data = coordSAMPLE, ~lat_wgs84, ~lon_wgs84, radius = 1, color = "blue") %>%
  addMarkers(data = ReefCheckLocation, ~Lon, ~Lat, popup = ~paste0(Lon,", ",Lat), label = ~paste0(Lon,", ",Lat))

#MAKE a PUBLICATION QUALITY FIGURE
leaflet() %>%
  addTiles() %>%
    addCircleMarkers(data = ReefCheckLocation, ~Lon, ~Lat, radius = 3, color = "red") %>%
    addCircleMarkers(data = coordSAMPLE, ~lat_wgs84, ~lon_wgs84, radius = 1, color = "blue")



#REN's try (4/4/2018)-------------------------------------------------------
head(ReefcheckFISH)
table(ReefcheckFISH$Amount) #ok, there are no NAs
#1. 
#isolate the four channel islands --- remove other points
ReefCheckSampleCoord<-matrix(c(
  "Anacapa_SMCA", -119.4375, 34.014729, "Anacapa",
  "Anacapa_SMR1", -119.371498, 34.015751, "Anacapa",
  "Anacapa_SMR2", -119.368385, 34.016499, "Anacapa",
  "Anacapa_SMR3", -119.362396, 34.017467, "Anacapa",
  "Anacapa_OUT", -119.364197, 34.012634, "Anacapa",
  "Scruz_Scorpion", -119.552299, 34.048515, "SCruz",
  "Scruz_Out_Front1",  -119.809998, 34.055, "SCruz",
  "Scruz_Out_Front2", -119.755997, 34.054161, "SCruz",
  "Scruz_Out_Front3", -119.702499, 34.035648, "SCruz",
  "Scruz_Out_Back1", -119.554398, 33.990665, "SCruz",
  "Scruz_Out_Back2", -119.550499, 33.998798, "SCruz",
  "SRosa_SPoint", -120.125, 33.895, "SRosa",
  "SRosa_Out", -120.103401, 33.90155, "SRosa"), nrow=4)

ReefCheckSampleCoordFin<-data.frame(t(ReefCheckSampleCoord))
names(ReefCheckSampleCoordFin) <- c("site","Lon","Lat","Island")
ReefCheckSampleCoordFin  

#merge the reefchech fish and the coordinates of interest
ReefcheckFISH_forAnalysis <- merge(ReefcheckFISH,ReefCheckSampleCoordFin,by=c("Lon","Lat"))  
table(ReefcheckFISH_forAnalysis$site)
head(ReefcheckFISH_forAnalysis)

#Assign species categories and merge
speciescategory<-matrix(c(
  "barred sand bass", "Target",
  "black and yellow", "Target",     
  "black perch"   , "Target",
  "black rockfish" , "Target",      
  "blacksmith"    , "Non-target",
  "blue rockfish", "Target",
  "bocaccio"   , "Target",
  "brown rockfish", "Target",
  "cabezon", "Target",
  "china rockfish", "Target",
  "copper rockfish", "Target",
  "garibaldi" , "Non-target",
  "giant sea bass",  "Non-target", 
  "gopher rockfish", "Target",
  "grass rockfish", "Target",
  "horn shark", "Non-target",
  "kelp bass", "Target",
  "kelp greenling", "Target", 
  "kelp rockfish", "Target",
  "lingcod", "Target",
  "opaleye", "Non-target",
  "pile perch", "Target",
  "rainbow perch", "Non-target",
  "rock greenling",  "Target",
  "rock wrasse", "Non-target",
  "rubberlip perch", "Target",
  "sargo", "Non-target",
  "senorita", "Non-target",
  "sheephead", "Target",
  "striped perch",  "Target",
  "treefish", "Target",
  "vermilion/canary", "Target",
  "yellowtail/olive", "Target",
  "yoy rockfish",  "Target"), nrow=2)
speciescategory<-data.frame(t(speciescategory))
names(speciescategory) <- c("Species","Target")
speciescategory

ReefcheckFISH_forAnalysis <- merge(ReefcheckFISH_forAnalysis,speciescategory,by="Species")  
head(ReefcheckFISH_forAnalysis)

#use small, medium, large data
table(ReefcheckFISH_forAnalysis$SizeCategory)

#replace sizes here... and remove remaining "see min_cm and max_cm"
ReefcheckFISH_forAnalysis2<-ReefcheckFISH_forAnalysis %>% mutate(SizeCategory=replace(SizeCategory, Size_cm<15 & SizeCategory=="see min_cm and max_cm","Small"))%>% 
  mutate(SizeCategory=replace(SizeCategory, Size_cm>=15 & Size_cm<=30 & SizeCategory=="see min_cm and max_cm","Medium"))%>%
  mutate(SizeCategory=replace(SizeCategory, Size_cm>30 & SizeCategory=="see min_cm and max_cm","Large"))%>%
  filter(SizeCategory!="see min_cm and max_cm")

table(ReefcheckFISH_forAnalysis2$SizeCategory)


###1. Santa Rosa
AbundanceSRosa<- ReefcheckFISH_forAnalysis2 %>% filter(Island=="SRosa") %>% 
  group_by(Year, Transect, site,Target) %>% summarize(abundance=sum(Amount))
AbundanceSRosa  

MAbundanceSRosa <- AbundanceSRosa  %>% group_by(Year, site,Target) %>% summarize(meanA=mean(abundance),sd=sd(abundance),ntrans=n())
MAbundanceSRosa

ggplot(MAbundanceSRosa,aes(x=Year, y=meanA, group=site, color=site, shape=site))+
  geom_errorbar(aes(ymin=meanA-sd,ymax=meanA+sd),width=.1)+geom_line()+geom_point(size=4)+
  theme_minimal()+labs(y="Abundance")+theme(legend.title=element_blank())+facet_grid(.~Target)


##use the small, medium, large details
AbundanceSRosa<- ReefcheckFISH_forAnalysis2 %>% filter(Island=="SRosa" & SizeCategory=="Small") %>% 
  group_by(Year, Transect, site,Target) %>% summarize(abundance=sum(Amount))
AbundanceSRosa  

MAbundanceSRosa <- AbundanceSRosa  %>% group_by(Year, site,Target) %>% summarize(meanA=mean(abundance),sd=sd(abundance),ntrans=n())
MAbundanceSRosa

p1<-ggplot(MAbundanceSRosa,aes(x=Year, y=meanA, group=site, color=site, shape=site))+
  geom_errorbar(aes(ymin=meanA-sd,ymax=meanA+sd),width=.1)+geom_line()+geom_point(size=4)+
  theme_minimal()+labs(y="Abundance")+theme(legend.title=element_blank())+facet_grid(.~Target)+ggtitle("Small (size<15cm)")
p1

AbundanceSRosa<- ReefcheckFISH_forAnalysis2 %>% filter(Island=="SRosa" & SizeCategory=="Medium") %>% 
  group_by(Year, Transect, site,Target) %>% summarize(abundance=sum(Amount))
MAbundanceSRosa <- AbundanceSRosa  %>% group_by(Year, site,Target) %>% summarize(meanA=mean(abundance),sd=sd(abundance),ntrans=n())
p2<-ggplot(MAbundanceSRosa,aes(x=Year, y=meanA, group=site, color=site, shape=site))+
  geom_errorbar(aes(ymin=meanA-sd,ymax=meanA+sd),width=.1)+geom_line()+geom_point(size=4)+
  theme_minimal()+labs(y="Abundance")+theme(legend.title=element_blank())+facet_grid(.~Target)+ggtitle("Medium (size=15-30cm)")

AbundanceSRosa<- ReefcheckFISH_forAnalysis2 %>% filter(Island=="SRosa" & SizeCategory=="Large") %>% 
  group_by(Year, Transect, site,Target) %>% summarize(abundance=sum(Amount))
MAbundanceSRosa <- AbundanceSRosa  %>% group_by(Year, site,Target) %>% summarize(meanA=mean(abundance),sd=sd(abundance),ntrans=n())
p3<-ggplot(MAbundanceSRosa,aes(x=Year, y=meanA, group=site, color=site, shape=site))+
  geom_errorbar(aes(ymin=meanA-sd,ymax=meanA+sd),width=.1)+geom_line()+geom_point(size=4)+
  theme_minimal()+labs(y="Abundance")+theme(legend.title=element_blank())+facet_grid(.~Target)+ggtitle("Large (size>30cm)")

library(gridExtra)
grid.arrange(p1,p2,p3,nrow=3)


#2. Anacapa
Head<-"Anacapa"
Abundance<- ReefcheckFISH_forAnalysis2 %>% filter(Island=="Anacapa" & !is.na(Amount)) %>% 
  group_by(Year, Transect, site,Target) %>% summarize(abundance=sum(Amount))
table(Abundance$site)

MPAin<-c("Anacapa_SMR1","Anacapa_SMR2","Anacapa_SMR3")
MPAout<-c("Anacapa_OUT")

Abundance<- Abundance %>% filter(site %in% c(MPAin,MPAout)) %>% mutate(MPA = ifelse(site %in% MPAin, "Inside", "Outside"))

MAbundance <- Abundance  %>% group_by(Year, MPA,Target) %>% summarize(meanA=mean(abundance),sd=sd(abundance),ntrans=n())

ggplot(MAbundance,aes(x=Year, y=meanA, color=MPA, shape=MPA))+
  geom_errorbar(aes(ymin=meanA-sd,ymax=meanA+sd),width=.1)+  
  
  #stat_compare_means(aes(group = MPA), label = "p.signif",label.y=10)+
  geom_line()+geom_point(size=4)+
  theme_minimal()+labs(y="Abundance")+theme(legend.title=element_blank())+facet_grid(.~Target)+ggtitle(Head)


#compare_means(MAbundance,paired= TRUE)


##use the small, medium, large details
Abundance<- ReefcheckFISH_forAnalysis2 %>% filter(Island=="Anacapa" & !is.na(Amount) & SizeCategory=="Small") %>% 
  group_by(Year, Transect, site,Target) %>% summarize(abundance=sum(Amount))
Abundance<- Abundance %>% filter(site %in% c(MPAin,MPAout)) %>% mutate(MPA = ifelse(site %in% MPAin, "Inside", "Outside"))
MAbundance <- Abundance  %>% group_by(Year, MPA,Target) %>% summarize(meanA=mean(abundance),sd=sd(abundance),ntrans=n())
p1<-ggplot(MAbundance,aes(x=Year, y=meanA, color=MPA, shape=MPA))+
  geom_errorbar(aes(ymin=meanA-sd,ymax=meanA+sd),width=.1)+geom_line()+geom_point(size=4)+
  theme_minimal()+labs(y="Abundance")+theme(legend.title=element_blank())+facet_grid(.~Target)+ggtitle("Small (size<15cm)")
Abundance<- ReefcheckFISH_forAnalysis2 %>% filter(Island=="Anacapa" & !is.na(Amount) & SizeCategory=="Medium") %>% 
  group_by(Year, Transect, site,Target) %>% summarize(abundance=sum(Amount))
Abundance<- Abundance %>% filter(site %in% c(MPAin,MPAout)) %>% mutate(MPA = ifelse(site %in% MPAin, "Inside", "Outside"))
MAbundance <- Abundance  %>% group_by(Year, MPA,Target) %>% summarize(meanA=mean(abundance),sd=sd(abundance),ntrans=n())
p2<-ggplot(MAbundance,aes(x=Year, y=meanA, color=MPA, shape=MPA))+
  geom_errorbar(aes(ymin=meanA-sd,ymax=meanA+sd),width=.1)+geom_line()+geom_point(size=4)+
  theme_minimal()+labs(y="Abundance")+theme(legend.title=element_blank())+facet_grid(.~Target)+ggtitle("Medium (size=15-30cm)")
Abundance<- ReefcheckFISH_forAnalysis2 %>% filter(Island=="Anacapa" & !is.na(Amount) & SizeCategory=="Large") %>% 
  group_by(Year, Transect, site,Target) %>% summarize(abundance=sum(Amount))
Abundance<- Abundance %>% filter(site %in% c(MPAin,MPAout)) %>% mutate(MPA = ifelse(site %in% MPAin, "Inside", "Outside"))
MAbundance <- Abundance  %>% group_by(Year, MPA,Target) %>% summarize(meanA=mean(abundance),sd=sd(abundance),ntrans=n())
p3<-ggplot(MAbundance,aes(x=Year, y=meanA, color=MPA, shape=MPA))+
  geom_errorbar(aes(ymin=meanA-sd,ymax=meanA+sd),width=.1)+geom_line()+geom_point(size=4)+
  theme_minimal()+labs(y="Abundance")+theme(legend.title=element_blank())+facet_grid(.~Target)+ggtitle("Large (size>30cm)")

grid.arrange(p1,p2,p3,nrow=3,top=Head)


#3. Santa Cruz
Head<-"Santa Cruz"
Abundance<- ReefcheckFISH_forAnalysis2 %>% filter(Island=="SCruz" & !is.na(Amount)) %>% 
  group_by(Year, Transect, site,Target) %>% summarize(abundance=sum(Amount))
table(Abundance$site)

MPAin<-c("Scruz_Scorpion")
MPAout<-c("Scruz_Out_Back1",  "Scruz_Out_Back2", "Scruz_Out_Front1", "Scruz_Out_Front2", "Scruz_Out_Front3")

Abundance<- Abundance %>% filter(site %in% c(MPAin,MPAout)) %>% mutate(MPA = ifelse(site %in% MPAin, "Inside", "Outside"))

MAbundance <- Abundance  %>% group_by(Year, MPA,Target) %>% summarize(meanA=mean(abundance),sd=sd(abundance),ntrans=n())

ggplot(MAbundance,aes(x=Year, y=meanA, color=MPA, shape=MPA))+
  geom_errorbar(aes(ymin=meanA-sd,ymax=meanA+sd),width=.1)+  
  geom_line()+geom_point(size=4)+
  theme_minimal()+labs(y="Abundance")+theme(legend.title=element_blank())+facet_grid(.~Target)+ggtitle(Head)


##use the small, medium, large details
Abundance<- ReefcheckFISH_forAnalysis2 %>% filter(Island=="SCruz" & !is.na(Amount) & SizeCategory=="Small") %>% 
  group_by(Year, Transect, site,Target) %>% summarize(abundance=sum(Amount))
Abundance<- Abundance %>% filter(site %in% c(MPAin,MPAout)) %>% mutate(MPA = ifelse(site %in% MPAin, "Inside", "Outside"))
MAbundance <- Abundance  %>% group_by(Year, MPA,Target) %>% summarize(meanA=mean(abundance),sd=sd(abundance),ntrans=n())
p1<-ggplot(MAbundance,aes(x=Year, y=meanA, color=MPA, shape=MPA))+
  geom_errorbar(aes(ymin=meanA-sd,ymax=meanA+sd),width=.1)+geom_line()+geom_point(size=4)+
  theme_minimal()+labs(y="Abundance")+theme(legend.title=element_blank())+facet_grid(.~Target)+ggtitle("Small (size<15cm)")
Abundance<- ReefcheckFISH_forAnalysis2 %>% filter(Island=="SCruz" & !is.na(Amount) & SizeCategory=="Medium") %>% 
  group_by(Year, Transect, site,Target) %>% summarize(abundance=sum(Amount))
Abundance<- Abundance %>% filter(site %in% c(MPAin,MPAout)) %>% mutate(MPA = ifelse(site %in% MPAin, "Inside", "Outside"))
MAbundance <- Abundance  %>% group_by(Year, MPA,Target) %>% summarize(meanA=mean(abundance),sd=sd(abundance),ntrans=n())
p2<-ggplot(MAbundance,aes(x=Year, y=meanA, color=MPA, shape=MPA))+
  geom_errorbar(aes(ymin=meanA-sd,ymax=meanA+sd),width=.1)+geom_line()+geom_point(size=4)+
  theme_minimal()+labs(y="Abundance")+theme(legend.title=element_blank())+facet_grid(.~Target)+ggtitle("Medium (size=15-30cm)")
Abundance<- ReefcheckFISH_forAnalysis2 %>% filter(Island=="SCruz" & !is.na(Amount) & SizeCategory=="Large") %>% 
  group_by(Year, Transect, site,Target) %>% summarize(abundance=sum(Amount))
Abundance<- Abundance %>% filter(site %in% c(MPAin,MPAout)) %>% mutate(MPA = ifelse(site %in% MPAin, "Inside", "Outside"))
MAbundance <- Abundance  %>% group_by(Year, MPA,Target) %>% summarize(meanA=mean(abundance),sd=sd(abundance),ntrans=n())
p3<-ggplot(MAbundance,aes(x=Year, y=meanA, color=MPA, shape=MPA))+
  geom_errorbar(aes(ymin=meanA-sd,ymax=meanA+sd),width=.1)+geom_line()+geom_point(size=4)+
  theme_minimal()+labs(y="Abundance")+theme(legend.title=element_blank())+facet_grid(.~Target)+ggtitle("Large (size>30cm)")

grid.arrange(p1,p2,p3,nrow=3,top=Head)


##############-------------------------------------------------------------------




###Data cleaning, analysis
#1. Remove all zeroes in the "Amount" column of the ReefCheck data. This means no observation.
ReefcheckFISH_filter<-ReefcheckFISH[which(ReefcheckFISH$Amount>0),]
head(ReefcheckFISH_filter,100)#ok, this is correct

#check the size categories
table(ReefcheckFISH_filter$SizeCategory)
#Large                Medium see min_cm and max_cm                 Small 
#6381                 23406                 56564                 17404

#print the elements of "see min_cm and max_cm"
head(ReefcheckFISH_filter %>% filter(SizeCategory=="see min_cm and max_cm"),20)

#What are the species?
table(ReefcheckFISH_filter$Species)

#what are the sites?
table(ReefcheckFISH_filter$Site)

#are the sites unique per coordinate points?
checkdata <- ReefcheckFISH_filter[c("Site", "Lat", "Lon")]
reefchecksites<-unique(checkdata) #good! Names are unique!
#save sites and coordinates
write.csv(reefchecksites, file = "C:/Users/Ren/Documents/GitHub/ReefCheckVsPISCO/reefchecksites.csv")

#plot site year
head(ReefcheckFISH_filter)
SiteYearTable<-ReefcheckFISH_filter %>% group_by(Site,Year,Month) %>% summarize(count=n())

#getting the number of sampling per site per year
SiteYearTable2<-SiteYearTable %>% group_by(Site,Year) %>% summarize(count=n())
cast(SiteYearTable2, Site ~ Year)

#getting the number of sampling points per site per year
xx<-cast(SiteYearTable, Site ~ Year)###reshape data!
xx[is.na(xx)] <- ""
xx

#one way to plot but not entirely what we want
ggplot(SiteYearTable, aes(x=Year,y=Month))+geom_point()+ facet_wrap(~Site)

##try different way of plotting
SiteMonthTable<-SiteYearTable %>% group_by(Site, Month) %>% summarise(n=n())
SiteMonthTable$MonthName<-month.abb[SiteMonthTable$Month]
as.factor(SiteMonthTable$MonthName)
#reorder levels
print(levels(SiteMonthTable$Site))
SiteMonthTableSum<-SiteMonthTable %>% group_by(Site) %>% summarise(sumn=sum(n))
myneworder<-order(SiteMonthTableSum$sumn)
SiteMonthTable$Site <- factor(SiteMonthTable$Site,levels(SiteMonthTable$Site)[myneworder])

SiteMonthTable$MonthName <- factor(SiteMonthTable$MonthName,c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
#plot data

reefchecksched<-ggplot(SiteMonthTable, aes(x=MonthName, y=Site, fill=n)) +geom_tile(colour="white", linewidth=2,width=.9, height=.9) + theme_minimal()+
  scale_fill_continuous(guide="legend",low = "yellow",high = "red", limits=c(0,max(SiteMonthTable$n)), breaks=seq(max(SiteMonthTable$n),1,-1))+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

reefchecksched
#save plot here
ggsave("H:/Jenn Project 2017/reefchecksched.tiff",plot=reefchecksched,width=6,height=11, units = "in")



####YEAR-SITE PLOT
head(SiteYearTable)
SiteYearTable$Year<-as.factor(SiteYearTable$Year)
SiteYearSampling<-SiteYearTable %>% group_by(Site, Year) %>% summarise(n=n())

#reorder levels
reorder1<-SiteYearSampling %>% group_by(Site) %>% summarise(sumn=n())
myneworder<-order(reorder1$sumn)
SiteYearSampling$Site <- factor(SiteYearSampling$Site,levels(SiteYearSampling$Site)[myneworder])


reefcheckschedYear<-ggplot(SiteYearSampling, aes(x=Year, y=Site)) +geom_tile(colour="white", linewidth=2,width=.9, height=.9) + theme_minimal()+
  scale_fill_continuous(guide="legend",low = "yellow",high = "red", limits=c(0,max(SiteYearSampling$n)), breaks=seq(max(SiteYearSampling$n),1,-1))+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

ggsave("H:/Jenn Project 2017/reefcheckschedYEAR.tiff",plot=reefcheckschedYear,width=6,height=11, units = "in")



#isolate the four channel islands --- remove other points
ReefCheckSampleCoord<-matrix(c(
  "Anacapa_SMCA", -119.4375, 34.014729, "Anacapa",
  "Anacapa_SMR1", -119.371498, 34.015751, "Anacapa",
  "Anacapa_SMR2", -119.368385, 34.016499, "Anacapa",
  "Anacapa_SMR3", -119.362396, 34.017467, "Anacapa",
  "Anacapa_OUT", -119.364197, 34.012634, "Anacapa",
  "Scruz_Scorpion", -119.552299, 34.048515, "SCruz",
  "Scruz_Out_Front1",  -119.809998, 34.055, "SCruz",
  "Scruz_Out_Front2", -119.755997, 34.054161, "SCruz",
  "Scruz_Out_Front3", -119.702499, 34.035648, "SCruz",
  "Scruz_Out_Back1", -119.554398, 33.990665, "SCruz",
  "Scruz_Out_Back2", -119.550499, 33.998798, "SCruz",
  "SRosa_SPoint", -120.125, 33.895, "SRosa",
  "SRosa_Out", -120.103401, 33.90155, "SRosa"), nrow=4)

ReefCheckSampleCoordFin<-data.frame(t(ReefCheckSampleCoord))
names(ReefCheckSampleCoordFin) <- c("site","Lon","Lat","Island")
ReefCheckSampleCoordFin  

#merge the filtered reefchech fish and the coordinates of interest
ReefcheckFISH_forAnalysis <- merge(ReefcheckFISH_filter,ReefCheckSampleCoordFin,by=c("Lon","Lat"))  
table(ReefcheckFISH_forAnalysis$site)
head(ReefcheckFISH_forAnalysis)

#Assign species categories and merge
speciescategory<-matrix(c(
  "barred sand bass", "T",
  "black and yellow", "T",     
  "black perch"   , "T",
  "black rockfish" , "T",      
  "blacksmith"    , "N",
  "blue rockfish", "T",
  "bocaccio"   , "T",
  "brown rockfish", "T",
  "cabezon", "T",
  "china rockfish", "T",
  "copper rockfish", "T",
  "garibaldi" , "N",
  "giant sea bass",  "N", 
  "gopher rockfish", "T",
  "grass rockfish", "T",
  "horn shark", "N",
  "kelp bass", "T",
  "kelp greenling", "T", 
  "kelp rockfish", "T",
  "lingcod", "T",
  "opaleye", "N",
  "pile perch", "T",
  "rainbow perch", "N",
  "rock greenling",  "T",
  "rock wrasse", "N",
  "rubberlip perch", "T",
  "sargo", "N",
  "senorita", "N",
  "sheephead", "T",
  "striped perch",  "T",
  "treefish", "T",
  "vermilion/canary", "T",
  "yellowtail/olive", "T",
  "yoy rockfish",  "T"), nrow=2)
speciescategory<-data.frame(t(speciescategory))
names(speciescategory) <- c("Species","Target")
speciescategory

ReefcheckFISH_forAnalysis <- merge(ReefcheckFISH_forAnalysis,speciescategory,by="Species")  
head(ReefcheckFISH_forAnalysis)

#1st: Remove Amount = 0 or NA
table(ReefcheckFISH_forAnalysis$Amount) #there are no NAs or Os

#----THIS IS JUST FOR CHECKING WHICH OF THE SITES ARE MONITORED FREQUENTLY 
#2006 to 2016
ReefcheckFISH_forAnalysis2006<-ReefcheckFISH_forAnalysis[which(ReefcheckFISH_forAnalysis$Year==2015),]
table(ReefcheckFISH_forAnalysis2006$site,ReefcheckFISH_forAnalysis2006$Transect)
#---Check all year
#table(ReefcheckFISH_forAnalysis$site,ReefcheckFISH_forAnalysis$Transect,ReefcheckFISH_forAnalysis$Year)

##Now, I can isolate Anacapa first as it is one of the Islands that I can do meaningful analysis
#ReefcheckFISH_forAnalysisAnacapa<-ReefcheckFISH_forAnalysis[which(ReefcheckFISH_forAnalysis$Island=="Anacapa"),]


#Anacapa
AbundanceAnacapa<- ReefcheckFISH_forAnalysis %>% filter(Island=="Anacapa", Target=="T") %>% 
  group_by(Year, Transect, site) %>% summarize(abundance=sum(Amount))
AbundanceAnacapa  

ggplot(data=AbundanceAnacapa) +
  geom_point(mapping=aes(x=Year, y=abundance, group=site, color=site))+
  geom_smooth(mapping=aes(x=Year, y=abundance, group=site, color=site))



#1. Isolate Anacapa (one of the Islands that I can do meaningful analysis) then plot abundance as a function of transect and 
#Target=="T" if target species only and "N" if non-target species
AbundanceAnacapa<- ReefcheckFISH_forAnalysis %>% filter(Island=="Anacapa", !is.na(Amount), Target=="T") %>% 
  group_by(Year, Transect, site) %>% summarize(abundance=sum(Amount))
AbundanceAnacapa  

ggplot(data=AbundanceAnacapa) +
  geom_point(mapping=aes(x=Year, y=abundance, group=site, color=site))+
  geom_smooth(mapping=aes(x=Year, y=abundance, group=site, color=site))




#2. Isolate Anacapa, arrange by transect, site, year 
AbundanceAnacapa<- ReefcheckFISH_forAnalysisAnacapa %>% filter(Island=="Anacapa", !is.na(Amount)) %>% 
  arrange(site, Transect, Year)

table(AbundanceAnacapa$Species)

write.csv(AbundanceAnacapa, file = "H:/Jenn Project 2017/AnacapaData.csv")


#3 get just the outside, plot abundance per species
AbundanceAnacapa<- ReefcheckFISH_forAnalysisAnacapa %>% filter(site=="Anacapa_OUT", !is.na(Amount)) %>% 
  group_by(Year, Species) %>% summarize(abundance=sum(Amount))
NTransectPerYear<- ReefcheckFISH_forAnalysisAnacapa %>% filter(site=="Anacapa_OUT", !is.na(Amount)) %>%
  group_by(Year) %>% summarize(ntransect=length(unique(Transect)))
AbundanceAnacapaTransect <- merge(AbundanceAnacapa,NTransectPerYear,by="Year")  
AbundanceAnacapaTransect$AbundanceMean<-AbundanceAnacapaTransect$abundance/AbundanceAnacapaTransect$ntransect
AbundanceAnacapaTransect

ggplot(data=AbundanceAnacapaTransect) +
  geom_point(mapping=aes(x=Year, y=AbundanceMean, group=Species, color=Species))+
  geom_line(mapping=aes(x=Year, y=AbundanceMean, group=Species, color=Species))




###TRY for Santa Cruz
AbundanceSCruz<- ReefcheckFISH_forAnalysis %>% filter(Island=="SCruz", !is.na(Amount), Target=="T") %>% 
  group_by(Year, Transect, site) %>% summarize(abundance=sum(Amount))
AbundanceSCruz  

ggplot(data=AbundanceSCruz) +
  geom_point(mapping=aes(x=Year, y=abundance, group=site, color=site))+
  geom_smooth(mapping=aes(x=Year, y=abundance, group=site, color=site))

###TRY for Santa Rosa
AbundanceSRosa<- ReefcheckFISH_forAnalysis %>% filter(Island=="SRosa", Target=="T") %>% 
  group_by(Year, Transect, site) %>% summarize(abundance=sum(Amount))
AbundanceSRosa  

table(AbundanceSRosa$Year,AbundanceSRosa$Transect)

#I will make a year-transect table
#head(ReefcheckFISH_forAnalysis)
#for unique(ReefcheckFISH_forAnalysis)
#mytable<-





#i will assume that there are 18 transects per survey
MAbundanceSRosa <- AbundanceSRosa  %>% group_by(Year, site) %>% summarize(meanabundance=sum(abundance)/18)
ggplot(data=MAbundanceSRosa) +
  geom_point(mapping=aes(x=Year, y=meanabundance, group=site, color=site))


AbundanceSRosa  %>% group_by(Year, site) %>% summarize(meanabundance=mean(abundance),ntransect=n())

ggplot(data=AbundanceSRosa) +
  geom_point(mapping=aes(x=Year, y=abundance, group=site, color=site))+
  geom_smooth(mapping=aes(x=Year, y=abundance, group=site, color=site))




########JUMP TO PISCO BELOW


#PLOT ReefCheck coordinates - fish westcoast
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

states <- map_data("state")
west_coast <- subset(states, region %in% c("california"))

summary(west_coast)

ggplot(data = west_coast) + 
  geom_polygon(aes(x = long, y = lat), fill = "palegreen", color = "black")+
  geom_point(data=ReefcheckFISH,aes(x=Lon,y=Lat),color="red")
#  geom_point(data=data3,aes(x=Longitude,y=Latitude),color="green")


#filter fish  to the coords 33.5 to 34.5 and -118.7 to -120.5
CINMS_ReefcheckFISH<-ReefcheckFISH[which(ReefcheckFISH$Lat > 33.5 & ReefcheckFISH$Lat < 35 & ReefcheckFISH$Lon > -121 & ReefcheckFISH$Lon < -118.75),]
#site and year
head(CINMS_ReefcheckFISH)
table(CINMS_ReefcheckFISH$Year)
table(CINMS_ReefcheckFISH$Site)
CINMS_ReefcheckFISH_Yellowbanks<-CINMS_ReefcheckFISH[CINMS_ReefcheckFISH$Site=="Yellowbanks",]
table(CINMS_ReefcheckFISH_Yellowbanks$Year)

ggplot(data = west_coast) + 
  geom_polygon(aes(x = long, y = lat), fill = "palegreen", color = "black")+
  #geom_point(data=ReefcheckFISH,aes(x=Lon,y=Lat),color="blue")+
  geom_point(data=CINMS_ReefcheckFISH,aes(x=Lon,y=Lat),color="red",size=3)+
  geom_point(data=coordSAMPLE,aes(x=lat_wgs84,y=lon_wgs84),color="black")+
  xlim(-121,-118.5)+ylim(33.5,35)


plot(MPA, axes=TRUE, border="lightgray", col=c("azure","azure","bisque"))
points(CINMS_ReefcheckFISH$Lon,CINMS_ReefcheckFISH$Lat)
#lines(country.mp, axes=TRUE, col="gray")


#PLOT ReefCheck coordinates - invert channel islands
Reefcheck<-read.csv(paste(PATH,"ReefCheck/RCCA_invert_data.csv",sep="/"))
head(Reefcheck)
plot(MPA, axes=TRUE, border="lightgray", col=c("azure","azure","bisque"))
points(Reefcheck$Lon,Reefcheck$Lat)
lines(country.mp, axes=TRUE, col="gray")

#PLOT ReefCheck coordinates - invert channel islands
Reefcheck<-read.csv(paste(PATH,"ReefCheck/RCCA_invert_data.csv",sep="/"))
head(Reefcheck)
plot(MPA, axes=TRUE, border="lightgray", col=c("azure","azure","bisque"))
points(Reefcheck$Lon,Reefcheck$Lat)
lines(country.mp, axes=TRUE, col="gray")

#PLOT ReefCheck coordinates - algae channel islands
Reefcheck<-read.csv(paste(PATH,"ReefCheck/RCCA_algae_data.csv",sep="/"))
head(Reefcheck)
plot(MPA, axes=TRUE, border="lightgray", col=c("azure","azure","bisque"))
points(Reefcheck$Lon,Reefcheck$Lat)
lines(country.mp, axes=TRUE, col="gray")



#------------------------------------------------------------------
####EXCELLENT! NOW LET US ANALYZE THE PISCO DATA
#next is PISCO

PISCOdata = fread("H:/Jenn Project 2017/PISCOdata/UCSB_FISH.txt",sep="\t")
head(PISCOdata)

#get the relevant columns: site,year, month : and summarize
PISCOdataSub<-PISCOdata %>% group_by(site,year,month) %>% summarize(count=n())
head(PISCOdataSub)


#plot
PISCOdataTable<-PISCOdataSub %>% group_by(site, month) %>% summarise(n=n())
data.frame(PISCOdataTable)
PISCOdataTable$MonthName<-month.abb[PISCOdataTable$month]
head(PISCOdataTable)
as.factor(PISCOdataTable$MonthName)
PISCOdataTable$site<-as.factor(PISCOdataTable$site)
#reorder levels
PISCOSiteMonthTableSum<-PISCOdataTable %>% group_by(site) %>% summarise(sumn=sum(n))
myneworder<-order(PISCOSiteMonthTableSum$sumn)
PISCOdataTable$site <- factor(PISCOdataTable$site,levels(PISCOdataTable$site)[myneworder])

PISCOdataTable$MonthName <- factor(PISCOdataTable$MonthName,c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
#plot data

PISCOsched<-ggplot(PISCOdataTable, aes(x=MonthName, y=site, fill=n)) +geom_tile(colour="white", linewidth=2,width=.9, height=.9) + theme_minimal()+
  scale_fill_continuous(guide="legend",low = "yellow",high = "red", limits=c(0,max(PISCOdataTable$n)), breaks=seq(max(PISCOdataTable$n),1,-1))+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

PISCOsched

#save plot here
ggsave("H:/Jenn Project 2017/PISCOsched.tiff",plot=PISCOsched,width=6,height=13, units = "in")



####YEAR-SITE PLOT
PISCOdataSub$year<-as.factor(PISCOdataSub$year)
PISCOdataSub$site<-as.factor(PISCOdataSub$site)
SiteYearSampling<-PISCOdataSub %>% group_by(site, year) %>% summarise(n=n())

#reorder levels
reorder1<-SiteYearSampling %>% group_by(site) %>% summarise(sumn=n())
myneworder<-order(reorder1$sumn)
SiteYearSampling$site <- factor(SiteYearSampling$site,levels(SiteYearSampling$site)[myneworder])

PISCOschedYear<-ggplot(SiteYearSampling, aes(x=year, y=site)) +geom_tile(colour="white", linewidth=2,width=.9, height=.9) + theme_minimal()+
  scale_fill_continuous(guide="legend",low = "yellow",high = "red", limits=c(0,max(SiteYearSampling$n)), breaks=seq(max(SiteYearSampling$n),1,-1))+theme(axis.title.x=element_blank(),axis.title.y=element_blank())

ggsave("H:/Jenn Project 2017/PISCOschedYEAR.tiff",plot=PISCOschedYear,width=8,height=13, units = "in")




####---------------------------
#compare the sampling month density distribution
#PISCO
head(PISCOdataTable)
PISCOsamplingPDF<- PISCOdataTable %>% group_by(MonthName) %>% summarize(nmonthsum=sum(n))
PISCOsamplingPDF$nmonthsum<-PISCOsamplingPDF$nmonthsum/sum(PISCOsamplingPDF$nmonthsum)

#reefcheck
head(SiteMonthTable)
ReefchecksamplingPDF<- SiteMonthTable %>% group_by(MonthName) %>% summarize(nmonthsum=sum(n))
ReefchecksamplingPDF$nmonthsum<-ReefchecksamplingPDF$nmonthsum/sum(ReefchecksamplingPDF$nmonthsum)

gg<-ggplot(ReefchecksamplingPDF, aes(x=MonthName,y=nmonthsum,group = 1))+geom_line(colour="red")+
  geom_line(data = PISCOsamplingPDF, aes(x=MonthName,y=nmonthsum,group = 1))+
  theme(axis.title.x=element_blank())+labs(y="Sampling density")

# Define and add annotation -------------------------------------
library(grid)
my_grob1 = grid.text("PISCO", x=.6,  y=0.7, gp=gpar(col="black", fontsize=12, fontface="bold"))
my_grob2 = grid.text("Reef Check", x=.45,  y=0.45, gp=gpar(col="red", fontsize=12, fontface="bold"))
samplingPDF<- gg + annotation_custom(my_grob1)+annotation_custom(my_grob2)
#save plot here
ggsave("H:/Jenn Project 2017/samplingpdf.tiff",plot=samplingPDF)
