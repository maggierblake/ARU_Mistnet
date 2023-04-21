#Script for ARU and mist nettingg comparative analysis
#Written by M.Blake; updated 4/21/23
#Includes years 2013-2020 


#Libraries
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(reshape)
library(ggpubr)
library(ggpmisc)
library(stringi)


#Bring in daat 
ARU <- read.csv("ARU_Data_2013-2020.csv")
band<- read.csv("Band_Data_2013-2020.csv")


#########################################
#A&B SITE,DAY, YEAR, LIGHT
#########################################

#Subset columns from larger data sets
Adet <- ARU %>%
  select(year, species,site, julian,light_category, type, hours,  siteeffort, yreffort, siteyreffort, effort, lc_hours)
 
Bdet <- band %>%
  select(year, species,site, julian,light_category, type, hours, siteeffort, yreffort, siteyreffort, effort)



#DF with both subsetted datasets
alldet <- bind_rows(Adet, Bdet)

#Take only species that want to use that were detected in both  
alldet <- alldet[alldet$species %in% use.spp$species,]


#Detections per day each year
Ady <- Adet %>%
  group_by(.,year,julian)%>% 
  add_tally()%>% 
  subset(.,select = - c(species,site,light_category))%>%
  unique(.)%>%
  ungroup()

Bdy <- Bdet %>%
  group_by(.,year,julian)%>% 
  add_tally()%>% 
  subset(.,select = - c(species, site, light_category))%>%
  unique(.)%>%
  ungroup()

#Detections per day each year at each site 

Asdy <- Adet %>%
  group_by(.,year,julian,site)%>% 
  add_tally()%>% 
  subset(.,select = -c(species,light_category))%>%
  unique(.)%>%
  ungroup()


Bsdy <- Bdet %>%
  group_by(.,year,julian,site)%>% 
  add_tally()%>% 
  subset(.,select = - c(species, light_category))%>%
  unique(.)%>%
  ungroup()

#Detections per site each year
Asy <- Adet %>%
  group_by(.,year,site)%>% 
  add_tally()%>% 
  subset(.,select = - c(species,light_category,julian,hours,yreffort,effort, lc_hours) )%>%
  unique(.)%>%
  ungroup()%>%
  mutate(., effhour = n/siteyreffort)

Bsy <- Bdet %>%
  group_by(.,year,site)%>% 
  add_tally()%>% 
  subset(.,select = - c(species,light_category,julian,hours,yreffort,effort) )%>%
  unique(.)%>%
  ungroup()%>%
  mutate(., effhour = n/siteyreffort)


#Detections per site
Asite <- Adet %>%
  group_by(.,site)%>% 
  add_tally()%>% 
  subset(.,select = - c(species,light_category, julian, year, hours, yreffort,siteyreffort, lc_hours) )%>%
  unique(.)%>%
  ungroup()%>%
  mutate(., effhour = n/siteeffort)

Bsite <- Bdet %>%
  group_by(.,site)%>% 
  add_tally()%>% 
  subset(.,select = - c(species,light_category, julian, year, hours, yreffort,siteyreffort) )%>%
  unique(.)%>%
  ungroup()%>%
  mutate(., effhour = n/siteeffort)


#Detections per year 
Ayr <- Adet %>%
  group_by(.,year)%>% 
  add_tally()%>% 
  subset(.,select = - c(species,light_category, julian, site, hours, siteeffort,siteyreffort, lc_hours) )%>%
  unique(.)%>%
  ungroup()%>%
  mutate(., effhour = n/yreffort)

Byr <- Bdet %>%
  group_by(.,year)%>% 
  add_tally()%>% 
  subset(.,select = - c(species,light_category, julian, site, hours, siteeffort,siteyreffort) )%>%
  unique(.)%>%
  ungroup()%>%
  mutate(., effhour = n/yreffort)



#Detections per day each year for each light category
Aldy <- Adet %>%
  group_by(.,year,julian,light_category,lc_hours)%>% 
  add_tally()%>% 
  subset(.,select = - c(species, site, siteeffort,siteyreffort, yreffort, hours))%>%
  unique(.)%>%
  ungroup()%>%
  group_by(.,year,julian,light_category)%>%
  mutate(.,xhour=sum(lc_hours))%>%
  mutate(., count= sum(n))%>%
  subset(.,select = - c(lc_hours, n))%>%
  rename(.,c( xhour = "hours",count = "n" ))%>%
  unique(.)%>%
  ungroup()


Bldy <- Bdet %>%
  group_by(.,year,julian,light_category, hours)%>% 
  add_tally()%>% 
  subset(.,select = - c(species, site, siteeffort,siteyreffort, yreffort))%>%
  unique(.)%>%
  ungroup()%>%
  group_by(.,year,julian,light_category)%>%
  mutate(.,xhour=sum(hours))%>%
  mutate(., count= sum(n))%>%
  subset(.,select = - c(hours, n))%>%
  rename(.,c( xhour = "hours",count = "n" ))%>%
  unique(.)%>%
  ungroup()

#Detections for each  light category 
Al <- Adet %>%
  group_by(.,light_category)%>% 
  add_tally()%>% 
  subset(.,select = - c(species,julian,hours, year,yreffort,siteyreffort,site, siteeffort, lc_hours))%>%
  unique(.)%>%
  ungroup()%>%
  mutate(., effhour = n/effort)

Bl <- Bdet %>%
  group_by(.,light_category)%>% 
  add_tally()%>% 
  subset(.,select = - c(species,julian,hours, year,yreffort,siteyreffort,site, siteeffort))%>%
  unique(.)%>%
  ungroup()%>%
  mutate(., effhour = n/effort)



#Detections for each site and light category 
Asl <- Adet %>%
  group_by(.,light_category, site)%>% 
  add_tally()%>% 
  subset(.,select = - c(species,julian,hours, year,yreffort,siteyreffort, lc_hours))%>%
  unique(.)%>%
  ungroup()%>%
  mutate(., effhour = n/siteeffort)

Bsl <- Bdet %>%
  group_by(.,light_category, site)%>% 
  add_tally()%>% 
  subset(.,select = - c(species,julian,hours, year,yreffort,siteyreffort))%>%
  unique(.)%>%
  ungroup()%>%
  mutate(., effhour = n/siteeffort)

#Detections for each site and light category per year
Aslyr <- Adet %>%
  group_by(.,light_category, site, year)%>% 
  add_tally()%>% 
  subset(.,select = - c(species,julian,hours,yreffort, siteeffort, lc_hours))%>%
  unique(.)%>%
  ungroup()%>%
  mutate(., effhour = n/siteyreffort)

Bslyr <- Bdet %>%
  group_by(.,light_category, site, year)%>% 
  add_tally()%>% 
  subset(.,select = - c(species,julian,hours,yreffort, siteeffort))%>%
  unique(.)%>%
  ungroup()%>%
  mutate(., effhour = n/siteyreffort)


#Detections per day each year for each site and light category
Asldy <- Adet %>%
  group_by(.,year,julian,light_category, site)%>% 
  add_tally()%>% 
  subset(.,select = - species)%>%
  unique(.)%>%
  ungroup()

Bsldy <- Bdet %>%
  group_by(.,year,julian,light_category, site)%>% 
  add_tally()%>% 
  subset(.,select = - species)%>%
  unique(.)%>%
  ungroup()



############################################
#CORRELATIONS
############################################
#Get a standard error from correlation tests
cor.test.plus <- function(x) {
  list(x, 
       Standard.Error = unname(sqrt((1 - x$estimate^2)/x$parameter)))
}


#correlations for detection per site
site <- full_join(Asdy,Bsdy, by = c("year","julian","site")) 

#site<-site[-c(399:433),]


#Data sets for each site
allsite <- site
allsite$site <- "all"

fld <- filter(site, site == "flood")
rdg <- filter(site, site == "ridge")
shp <- filter(site, site == "sheep")

#Data list 
sts <- list(allsite, fld,rdg,shp)

corr.site.day <- NULL
for(i in sts){
  
  i$nx.eff<- i$n.x/i$hours.x
  i$ny.eff<- i$n.y/i$hours.y
  
  
  #Log transform
  i$logx <- log10(i$nx.eff)
  i$logy <- log10(i$ny.eff)
  
  
  #correlation test
  corr <- cor.test(i$logx, i$logy, method="pearson")
  
  
  #Dataframe holding results
  df <- data.frame(site = unique(i$site), correlation = corr$estimate, lci = corr$conf.int[1], uci = corr$conf.int[2], pvalue = corr$p.value,se = corr$null.value)
  
  #Test with function to calculate the Standard Error
  corr <- cor.test.plus(cor.test(i$logx, i$logy, method="pearson"))
  
  #Put se in data frame
  df$se <- corr$Standard.Error
  
  #Put together results from each data set
  corr.site.day <- rbind(corr.site.day,df)
  
  
  #Graph 
  x <- ggscatter(i, x = "logx", y = "logy", 
             add = "reg.line", conf.int = TRUE, cor.method = "pearson", main = unique(i$site),
             xlab = "ARU (dtct/day)", ylab = "Banding (dtct/day)")
           
             
             
   print(x)
}


corr.site.day


#correlations for detection per day for each light category
#Data sets for each site
light <- full_join(Aldy,Bldy, by = c("year","julian"))

alight<-Aldy%>%
  group_by(.,year,julian)%>% 
  mutate(.,n=sum(n))%>% 
  mutate(., hours=sum(hours))%>%
  subset(.,select = - light_category)%>%
  unique(.)%>%
  ungroup()

alight$light_category <- "all"

alllight <-full_join(alight,Bldy, by = c("year","julian"))

alllight<-unique(alllight)

ATR <- filter(light, light_category.x == "astronomical_dawn")
CTR <- filter(light, light_category.x == "civil_dawn")
NTR <- filter(light, light_category.x == "nautical_dawn")
night <- filter(light, light_category.x == "night")


#Data sets for correlations
lgts <- list(alllight,ATR,CTR,NTR,night)

corrlight <- NULL
for(i in lgts){
 
  #i$nx.eff<- i$n.x/i$lc_hours
  i$nx.eff<- i$n.x/i$hours.x
  i$ny.eff<- i$n.y/i$hours.y
  
  

  #Log transform
  i$logx <- log10(i$nx.eff)
  i$logy <- log10(i$ny.eff)
  
  #correlation test
  corr <- cor.test(i$logx, i$logy, method="pearson")
  
  #Dataframe holding results
  df <- data.frame(site = unique(i$light_category.x), correlation = corr$estimate,lci = corr$conf.int[1], uci = corr$conf.int[2], pvalue = corr$p.value, se = corr$null.value)
 
  #Test with function to calculate the Standard Error
  corr <- cor.test.plus(cor.test(i$logx, i$logy, method="pearson"))
  
  #Put se in data frame
  df$se <- corr$Standard.Error
  
   corrlight <- rbind(corrlight,df)
  
  #Graph 
  x <- ggscatter(i, x = "logx", y = "logy", 
               add = "reg.line", conf.int = TRUE, cor.method = "pearson", main = unique(i$light_category.x),
               xlab = "ARU", ylab = "Banding")
  print(x)
}


corrlight


#correlations for detection per day for each light category and site
#Data sets for each sight and light category
sitelight <- full_join(Asldy,Bsldy, by = c("year","julian", "site"))



allsl <- full_join(Ady,Bdy, by = c("year","julian"))
allsl$light_category.x <- "all"
allsl$site <- "all"


ATRfld <- filter(sitelight, light_category.x == "astronomical_dawn" & site == "flood")
ATRrdg <- filter(sitelight, light_category.x == "astronomical_dawn" & site == "ridge")
ATRshp <- filter(sitelight, light_category.x == "astronomical_dawn" & site == "sheep")
CTRfld <- filter(sitelight, light_category.x == "civil_dawn" & site == "flood")
CTRrdg <- filter(sitelight, light_category.x == "civil_dawn" & site == "ridge")
CTRshp <- filter(sitelight, light_category.x == "civil_dawn" & site == "sheep")
NTRfld <- filter(sitelight, light_category.x == "nautical_dawn" & site == "flood")
NTRrdg <- filter(sitelight, light_category.x == "nautical_dawn" & site == "ridge")
NTRshp <- filter(sitelight, light_category.x == "nautical_dawn" & site == "sheep")
NTSfld <- filter(sitelight, light_category.x == "nautical_dusk" & site == "flood")
nightfld <- filter(sitelight, light_category.x == "night" & site == "flood")
nightrdg <- filter(sitelight, light_category.x == "night" & site == "ridge")
nightshp <- filter(sitelight, light_category.x == "night" & site == "sheep")

#Data lists light and site
stlgt <- list(allsl,
           ATRfld,
           ATRrdg,
           ATRshp,
           CTRfld,
           CTRshp,
           NTRfld,
           NTRrdg,
           NTRshp,
           nightfld,
           nightrdg,
           nightshp)
           


corrsl <- NULL
for(i in stlgt){
  
  i$nx.eff<- i$n.x/i$lc_hours
  #i$nx.eff<- i$n.x/i$hours.x
  i$ny.eff<- i$n.y/i$hours.y
  
  
  #Log transform
  i$logx <- log10(i$nx.eff)
  i$logy <- log10(i$ny.eff)
  
  #correlation test
  corr <- cor.test(i$logx, i$logy, method="pearson")
  
  
  #Dataframe holding results
  df <- data.frame(site = paste(unique(i$light_category.x),unique(i$site)), correlation = corr$estimate,lci = corr$conf.int[1], uci = corr$conf.int[2], pvalue = corr$p.value, se = corr$null.value)
  
  #Test with function to calculate the Standard Error
  corr <- cor.test.plus(cor.test(i$logx, i$logy, method="pearson"))
  
  #Put se in data frame
  df$se <- corr$Standard.Error
  
  corrsl <- rbind(corrsl,df)
  
  #Graph 
  x <- ggscatter(i, x = "logx", y = "logy", 
               add = "reg.line", conf.int = TRUE, cor.method = "pearson", main =  paste(unique(i$light_category.x),unique(i$site)),
               xlab = "ARU", ylab = "Banding")
  print(x)
}

corrsl



#Just floodplain detections per day per light category and site
#Data list for floodplain
fldlgt <- list(ATRfld,CTRfld,NTRfld, nightfld)

corrfld <- NULL
for(i in fldlgt){
  
  i$nx.eff<- i$n.x/i$lc_hours
  #i$nx.eff<- i$n.x/i$hours.x
  i$ny.eff<- i$n.y/i$hours.y
  
  
  #Log transform
  i$logx <- log10(i$nx.eff)
  i$logy <- log10(i$ny.eff)
  
  #correlation test
  corr <- cor.test(i$logx, i$logy, method="pearson")
  
  #Dataframe holding results
  df <- data.frame(site = paste(unique(i$light_category.x),unique(i$site)), correlation = corr$estimate,lci = corr$conf.int[1], uci = corr$conf.int[2], pvalue= corr$p.value, se = corr$null.value)
  
  #Test with function to calculate the Standard Error
  corr <- cor.test.plus(cor.test(i$logx, i$logy, method="pearson"))
  
  #Put se in data frame
  df$se <- corr$Standard.Error
  
  corrfld <- rbind(corrfld,df)
  
  #Graph 
  x <- ggscatter(i, x = "logx", y = "logy", 
               add = "reg.line", conf.int = TRUE, cor.method = "pearson", main =  paste(unique(i$light_category.x),unique(i$site)),
               xlab = "ARU", ylab = "Banding")
  print(x)
}

corrfld


#Correlations for floodplain detections per day per light category and site in 2013-2015
#Data sets
ATRfld1315 <- filter(ATRfld, year %in%  c(2013:2015))
CTRfld1315 <- filter(CTRfld, year %in%  c(2013:2015))
NTRfld1315 <- filter(NTRfld, year %in%  c(2013:2015))
nightfld1315 <- filter(nightfld, year %in%  c(2013:2015))

#Data list floodplain 2015-2018
fldlgt1315 <- list(ATRfld1315,CTRfld1315,NTRfld1315, nightfld1315)

corrfld1315 <- NULL
for(i in fldlgt1315){
  
  i$nx.eff<- i$n.x/i$lc_hours
  #i$nx.eff<- i$n.x/i$hours.x
  i$ny.eff<- i$n.y/i$hours.y
  
  
  #Log transform
  i$logx <- log10(i$nx.eff)
  i$logy <- log10(i$ny.eff)
  
  #correlation test
  corr <- cor.test(i$logx, i$logy, method="pearson")
  
  #Dataframe holding results
  df <- data.frame(site = paste(unique(i$light_category.x),unique(i$site), "2013-15"), correlation = corr$estimate,lci = corr$conf.int[1], uci = corr$conf.int[2], pvalue= corr$p.value, se = corr$null.value )
  
  #Test with function to calculate the Standard Error
  corr <- cor.test.plus(cor.test(i$logx, i$logy, method="pearson"))
  
  #Put se in data frame
  df$se <- corr$Standard.Error
  
  corrfld1315 <- rbind(corrfld1315,df)
  
  #Graph 
  x <- ggscatter(i, x = "logx", y = "logy", 
                 add = "reg.line", conf.int = TRUE, cor.method = "pearson", main =  paste(unique(i$light_category.x),unique(i$site), "2013-2015"),
                 xlab = "ARU", ylab = "Banding")
  print(x)
  
}
corrfld1315



#Correlations for floodplain detections per day per light category and site in 2016-2018
#Data sets
ATRfld1620 <- filter(ATRfld, year %in%  c(2016:2020))
CTRfld1620 <- filter(CTRfld, year %in%  c(2016:2020))
NTRfld1620 <- filter(NTRfld, year %in%  c(2016:2020))
nightfld1620 <- filter(nightfld, year %in%  c(2016:2020))

#Data list floodplain 2016-2018
fldlgt1620 <- list(ATRfld1620,CTRfld1620,NTRfld1620, nightfld1620)


corrfld1620 <- NULL
for(i in fldlgt1620){
  
  i$nx.eff<- i$n.x/i$lc_hours
  #i$nx.eff<- i$n.x/i$hours.x
  i$ny.eff<- i$n.y/i$hours.y
  
  
  #Log transform
  i$logx <- log10(i$nx.eff)
  i$logy <- log10(i$ny.eff)
  
  #correlation test
  corr <- cor.test(i$logx, i$logy, method="pearson")
  
  #Dataframe holding results
  df <- data.frame(site = paste(unique(i$light_category.x),unique(i$site), "2016-20"), correlation = corr$estimate,lci = corr$conf.int[1], uci = corr$conf.int[2], pvalue= corr$p.value, se = corr$null.value )
 
  #Test with function to calculate the Standard Error
  corr <- cor.test.plus(cor.test(i$logx, i$logy, method="pearson"))
  
  #Put se in data frame
  df$se <- corr$Standard.Error
  
  corrfld1620 <- rbind(corrfld1620,df)
  
  #Graph 
  x <- ggscatter(i, x = "logx", y = "logy", 
               add = "reg.line", conf.int = TRUE, cor.method = "pearson", main =  paste(unique(i$light_category.x),unique(i$site), "2016-2020"),
               xlab = "ARU", ylab = "Banding")
  print(x)

}
corrfld1620


##############################################
#STANDARIZED LOG COMPARISON
##############################################
#get rid of nas
#spp<-na.omit(spp)


#spp$n<-spp$ARU_Detects/3989.07
#spp$y<-spp$Band_Detects/2524.742

spp$n<-spp$ARU_Detects/3805
spp$y<-spp$Band_Detects/1994

#Log detections
spp$logARU <- log10(spp$n)
spp$logBand <- log10(spp$y)


#ARU SD and mean
a <- spp$logARU
a_sd <- sd(a, na.rm=TRUE)*sqrt((length(a)-1)/(length(a)))
a_mean <- mean(a,na.rm=TRUE)

#Band SD and mean
b <- spp$logBand
b_sd <- sd(b, na.rm=TRUE)*sqrt((length(b)-1)/(length(b))) 
b_mean <- mean(b, na.rm=TRUE)

#Calculate z-score
spp$ARU_z <- (spp$logARU - a_mean) / a_sd
spp$Band_z <- (spp$logBand - b_mean) / b_sd

#Correlation test for detections 
cor.test.plus(cor.test(spp$ARU_z, spp$Band_z, method="pearson"))

#Formula for graph
my.formula <- y ~ x

#Plot of standarized log detections for each species comparison with ARU and banding data
ggplot(spp, aes(x= ARU_z, y= Band_z)) + 
 geom_point() +
 theme_bw() + 
 theme(panel.border = element_blank(), panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text=element_text(size=14,family="serif"))+
 labs(x = "Standardized log(ARU detections per day)", y = "Standarized log(Mist net detections per day)")+
 geom_text(aes(label=species),hjust=.9, vjust=0, family = "serif")+
 geom_smooth(method="lm", se=FALSE, color="black", formula = my.formula ) +
 stat_poly_eq(formula = my.formula, aes(label = paste(..rr.label.., sep = "~~~"), family = "serif", size = 14), 
           parse = TRUE)



x<-lm(formula = Band_z ~ ARU_z, data = spp)


#Change graph from species code to common names
cn <- read.csv("Common_Names.csv")
cn[,1]<-as.character(cn[,1])

spp<-left_join(spp,cn,"species")



ggplot(spp, aes(x= ARU_z, y= Band_z)) + 
  geom_point(size = 3) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text=element_text(size=36,family="serif"))+
  labs(x = "Standardized log(ARU detections per day)", y = "Standarized log(Mist net detections per day)")+
  geom_text(aes(label = stringr::str_wrap(spp,5)),hjust=.5, vjust=1.2,family = "serif", size = 12)+
  geom_smooth(method="lm", se=FALSE, color="black", formula = my.formula )#+
  BAN#stat_poly_eq(formula = my.formula, aes(label = paste(..rr.label.., sep = "~~~"), family="serif"), size = 10,
               #parse = TRUE)


########################################
#NIGHT HOURS ANALYSIS
########################################
#Bringin night hours data
dat <- read.csv("Night_Hours_2012-2018.csv", stringsAsFactors=FALSE)

#Format data and create a julian date
dat$date <- as.Date(dat$date,"%m/%d/%Y")
dat$julian <- yday(dat$date)


nhrs<-night %>%
      left_join(.,dat, by = c("year", "julian")) %>%
      subset(., select = c(year, julian, n.x, night_length))
      
  

   
test <- glm(nhrs$n.x ~ nhrs$night_length)





#############################################################################################################################################
##Count of detection by method looking at site and light category that was most highly correlated from correlated detections
arusl<-alldet %>%
  filter(.$type == "ARU")%>%
  group_by(.,light_category,site, species)%>% 
  add_tally()%>% 
  subset(.,select = - c(julian, year))%>%
  unique(.)%>%
  ungroup()

bandsl<-alldet %>%
  filter(.$type == "Band")%>%
  group_by(.,light_category,site, species)%>% 
  add_tally()%>% 
  subset(.,select = - c(julian, year))%>%
  unique(.)%>%
  ungroup()


allsppsl<- full_join(arusl, bandsl,by = c("species", "site"))%>%
  rename(.,c(n.x = "ARU_Detects",n.y ="Band_Detects", light_category.x = "light_category"))%>%
  subset(.,select = -c(light_category.y, type.x, type.y))

x<-filter(allsppsl, site == "flood" & light_category == "nautical_dusk")


x$logARU<-log10(x$ARU_Detects)
x$logBand<-log10(x$Band_Detects)


#ARU
a <- x$logARU
a_sd <- sd(a, na.rm=TRUE)*sqrt((length(a)-1)/(length(a)))
a_mean <- mean(a,na.rm=TRUE)

#Band
b <- x$logBand
b_sd <- sd(b, na.rm=TRUE)*sqrt((length(b)-1)/(length(b))) 
b_mean <- mean(b, na.rm=TRUE)

x$ARU_z <- (x$logARU - a_mean) / a_sd
x$Band_z <- (x$logBand - b_mean) / b_sd

cor.test(x$ARU_z, x$Band_z, method="pearson")

ggplot(x, aes(x= ARU_z, y= Band_z,  label=species)) + 
  geom_point() +
  geom_text(aes(label=species),hjust=0, vjust=0)+
  geom_smooth(method="lm", se=FALSE)






#Remove grouped species 
 #without group species gave pretty much the same correlation and did not change r2 value  
#grp<-c("CCSP_BRSP","LISP_SWSP","SOSP_FOSP","SPSA_SOSA","YRWA_TOWA") 
# for(i in grp){ 
# spp<-filter(spp, species != i)
#}

#plot to see detections per day for each method 
#ARU
for(i in sts){
  theme_set(theme_bw())
  x<-ggplot(i, aes(x=julian, y=n.x, colour = year)) + 
    geom_point()  +
    geom_smooth( colour="black",method="loess", se=T) +
    scale_color_gradient(low = "pink", high = "red")+
    labs( subtitle = unique(i$site),
          y="Detections", 
          x="Julian Date", 
          title="ARU")
  
  print(x)
}


#Band
for(i in sts){
  theme_set(theme_bw())
  x<-ggplot(i, aes(x=julian, y=n.y, colour = year)) + 
    geom_point()  +
    geom_smooth( colour="black",method="loess", se=T) +
    labs(subtitle = unique(i$site), 
         y="Detections", 
         x="Julian Date", 
         title="Banding")
  print(x)
}


#Detections per day each year
ARU_daydet <- Adet %>%
  group_by(.,year,julian)%>% 
  add_tally()%>% 
  subset(.,select = - c(species,site,light_category, siteeffort, siteyreffort, effort, lc_hours))%>%
  unique(.)%>%
  mutate(., thours= sum(hours))%>%
  subset(.,select = -hours)%>%
  unique(.)%>%
  ungroup()%>%
  mutate(., aday= n/thours)


Band_daydet <- Bdet %>%
  group_by(.,year,julian)%>% 
  add_tally()%>% 
  subset(.,select = - c(species,site,light_category, siteeffort, siteyreffort, effort))%>%
  unique(.)%>%
  mutate(., thours= sum(hours))%>%
  subset(.,select = -hours)%>%
  unique(.)%>%
  ungroup()%>%
  mutate(., bday= n/thours)



cor.test.plus <- function(x) {
  list(x, 
       Standard.Error = unname(sqrt((1 - x$estimate^2)/x$parameter)))
}

dd<-left_join(ARU_daydet,Band_daydet, by = c("year", "julian"))

allyear<-dd

yr2013<- filter(dd, year == "2013")
yr2014<- filter(dd, year == "2014")
yr2015<- filter(dd, year == "2015")
yr2016<- filter(dd, year == "2016")
yr2017<- filter(dd, year == "2017")
yr2018<- filter(dd, year == "2018")
yr2019<- filter(dd, year == "2019")
yr2020<- filter(dd, year == "2020")

#Data list 
yrs <- list(allyear,yr2013,yr2014,yr2015,yr2016,yr2017,yr2018,yr2019,yr2020)

corr.year.day <- NULL
for(i in yrs){
  
  #i$nx.eff<- i$n.x/i$hours.x
  #i$ny.eff<- i$n.y/i$hours.y
  
  
  #Log transform
  i$logx <- log10(i$aday)
  i$logy <- log10(i$bday)
  
  #correlation test
  corr <- cor.test(i$logx, i$logy, method="pearson")
  
  #Dataframe holding results
  df <- data.frame(year = unique(i$year), correlation = corr$estimate, lci = corr$conf.int[1], uci = corr$conf.int[2], pvalue = corr$p.value,se = corr$null.value)
  
  #Test with function to calculate the Standard Error
  corr <- cor.test.plus(cor.test(i$logx, i$logy, method="pearson"))
  
  #Put se in data frame
  df$se <- corr$Standard.Error
  
  #Put together results from each data set
  corr.year.day <- rbind(corr.year.day,df)
  
  #Graph 
  x <- ggscatter(i, x = "logx", y = "logy", 
                 add = "reg.line", conf.int = TRUE, cor.method = "pearson", main = unique(i$year),
                 xlab = "ARU (dtct/day)", ylab = "Banding (dtct/day)")
  
  
  
  print(x)
}


corr.year.day







#averages per year 

av_yr<-dd%>%
  group_by(year)%>%
  mutate(., ARU_yr = mean(aday))%>%
  mutate(., Band_yr = mean(bday))%>%
  ungroup()%>%
  select(c(year,ARU_yr,Band_yr))%>%
  unique(.)

av_yr$logx <- log10(av_yr$ARU_yr)
av_yr$logy <- log10(av_yr$Band_yr)


corr <- cor.test(av_yr$logx, av_yr$logy, method="pearson")



#########################################################

#spp_per<-unique(band$species)
spp_per<-c("AMRE",  "AMRO",  "CCBRSP",  "CHSP", "COYE", "LAZB", "LISWSP", "NOWA", "PISI", "RBNU", "SAVS", "SWTH", "VESP", "WCSP", "WIWA", "YEWA")

spcode<-names(spp_per)

jdates1<- data.frame()
for (i in spp_per){
  x<- filter(ARU, species == i)
  mjdate <- median(x$julian)
  r<-quantile(x$julian, probs = c(.10, .5, .90))
  r2<-as.data.frame(r)
  z<- cbind("Species" = i,"Median" =mjdate , "P10" = r2[1,],"P50"= r2[2,],"P90" = r2[3,])
  jdates1<-rbind(jdates1,z)
}
  
  
  
  
jdates2<- data.frame()
for (i in spp_per){
  x<- filter(band, species == i)
  mjdate <- median(x$julian)
  r<-quantile(x$julian, probs = c(.10, .5, .90))
  r2<-as.data.frame(r)
  z<- cbind("Species" = i,"BMedian" =mjdate , "BP10" = r2[1,],"BP50"= r2[2,],"BP90" = r2[3,])
  jdates2<-rbind(jdates2,z)
}




jdates<- left_join(jdates1, jdates2, by= "Species")



#Median Model
model <- lm(Median ~ BMedian, data=jdates)
standard_res <- rstandard(model)
resid.mod<-resid(model)
summary(model)



#x<-model$residuals
#y<-as.numeric(as.vector(jdates$Median))

x<-as.numeric(as.vector(jdates$Median))
y<-as.numeric(as.vector(jdates$BMedian))

corr <- cor.test.plus(cor.test(y, x, method="pearson"))


#10th percentile
model10 <- lm(P10 ~ BP10, data=jdates)
standard_res <- rstandard(model10)
summary(model10)

x<-model10$residuals
y<-as.numeric(as.vector(jdates$P10))

corr <- cor.test.plus(cor.test(x, y, method="pearson"))


#50th percentile
model50 <- lm(P50 ~ BP50, data=jdates)
standard_res <- rstandard(model50)
summary(model50)

x<-model50$residuals
y<-as.numeric(as.vector(jdates$P50))

corr <- cor.test.plus(cor.test(x, y, method="pearson"))


#90th percentile
model90 <- lm(BP90 ~ P90, data=jdates)
standard_res <- rstandard(model90)
summary(model90)

x<-model90$residuals
y<-as.numeric(as.vector(jdates$BP90))

corr <- cor.test.plus(cor.test(x, y, method="pearson"))



corr

 jdates$Median<-as.numeric(jdates$Median)
 jdates$BMedian<-as.numeric(jdates$BMedian)
 jdates$P10<-as.numeric(jdates$P10)
 jdates$BP10<-as.numeric(jdates$BP10)
 jdates$BP50<-as.numeric(jdates$BP50)
 jdates$P50<-as.numeric(jdates$P50)
 jdates$P90<-as.numeric(jdates$P90)
 jdates$BP90<-as.numeric(jdates$BP90)

 #Median
 r<- jdates$BMedian - jdates$Median
 plot(r~jdates$Median)
 cor.test(r,jdates$Median, method= "pearson")
 #10th
 r<- jdates$BP10 - jdates$P10
 plot(r~jdates$P10)
 cor.test.plus(cor.test(r,jdates$P10, method= "pearson"))
 
 #50th
 r<- jdates$BP50 - jdates$P50
 plot(r~jdates$P50)
 cor.test.plus(cor.test(r,jdates$P50, method= "pearson"))
 
 #90th
 r<- jdates$BP90 - jdates$P90
 plot(r~jdates$P90)
 cor.test.plus(cor.test(r,jdates$P90, method= "pearson"))
 
 
 
 

