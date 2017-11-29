######################################
# BIOL 465 Research Project
# Grace Di Cecco
# November 2017
# Temporal stability in North American bird communities
######################################

require(traits)
require(tidyr)
require(dplyr)
require(ggplot2)
require(cowplot)
require(scales)

######### Read in datasets #########
#setwd("/Volumes/hurlbertlab/DiCecco/LTER_birdabund_seasonal/") #Mac
setwd("\\\\BioArk\\hurlbertlab\\DiCecco\\LTER_birdabund_seasonal\\") #Windows

#For each dataset: change character strings to all lowercase and remove hyphens
#improves matching rate by common name between LTER datasets and BirdLife checklist

##BirdLife checklist (ID numbers)
checklist <- read.csv("BirdLife_Checklist_V_9.1.csv", header = TRUE, stringsAsFactors = F)
checklist.subs <- checklist[,c(4,13)] #just SISRecID and common name
checklist.subs$Common_name <- iconv(checklist.subs$Common_name,"WINDOWS-1252","UTF-8") #if error invalid multibyte string
#reformat character strings
checklist.subs$Common_name <- tolower(checklist.subs$Common_name)
checklist.subs$Common_name <- gsub("-", " ", checklist.subs$Common_name)

##Migration trait data (only passerines in BBS)
traits <- read.csv("Master_RO_Correlates_20110610.csv", header = T, stringsAsFactors = F)
traits.subs <- traits[,c("CommonName", "migclass")]
traits.subs$CommonName <- tolower(traits.subs$CommonName)
traits.subs$CommonName <- gsub("-", " ", traits.subs$CommonName)

#Konza LTER
konza <- read.csv("CBP011_0.csv")
konza$COMMONNAME <- tolower(konza$COMMONNAME)
konza$COMMONNAME <- gsub("-", " ", konza$COMMONNAME)
konza.sub <- filter(konza, WATERSHED == "020B" | WATERSHED == "020C" | WATERSHED == "020D" | WATERSHED == "N20B") #unburned grassland
konza_id <- konza.sub %>%
  left_join(checklist.subs, by = c("COMMONNAME" = "Common_name")) %>%
  left_join(traits.subs, by = c("COMMONNAME" = "CommonName"))

#Parker River NWR LTER
parkriver <- read.csv("MON-EX-PRNWR-Volunteer-Birds.csv", header = TRUE)
parkriver$Common.Name <- tolower(parkriver$Common.Name)
parkriver$Common.Name <- gsub("-", " ", parkriver$Common.Name)
parkriver_id <- parkriver %>%
  filter(Census.Unit == "marsh") %>% #subset one habitat type
  left_join(checklist.subs, by = c("Common.Name" = "Common_name")) %>%
  left_join(traits.subs, by = c("Common.Name" = "CommonName"))

#Sevilleta LTER
sev <- read.table("sev017_birdpop_09251997_0.txt", header = T, sep = ",")
sev_table <- read.csv("sev-table-aou.csv") #AOU to common name table
sev_table_col <- separate(sev_table, 1, into = c("AOU", "CommonName"), sep = "   - ") #remove separators  
sev_table_col$AOU <- as.factor(sev_table_col$AOU)
sev_join <- left_join(sev, sev_table_col, by = c("species_number" = "AOU")) #join common name to AOU
sev_join$CommonName <- tolower(sev_join$CommonName)
sev_join$CommonName <- gsub("-", " ", sev_join$CommonName)
sev_id <- filter(sev_join, habitat_type == "creosote") %>% #subset one habitat type
          left_join(checklist.subs, by = c("CommonName" = "Common_name")) %>% #join ID number by common name
          left_join(traits.subs, by = c("CommonName" = "CommonName"))

#Luquillo LTER
luquillo_LT_25 <- read.csv("ALL_ElVerde_LT_25m.csv", header = TRUE)
luquillo_GT_25 <- read.csv("ALL_ElVerde_GT_25m.csv", header = TRUE)
luquillo <- rbind(luquillo_LT_25, luquillo_GT_25) #join less than 25 m and greater than 25 m data
#May and June for many years, Fall(Sept-Nov) for at least 3 years, Winter (Jan-March) for at least 3 years
#Rainy season: April-Nov
luquillo_sppnames <- read.csv("luquillo_spp_code.csv") #five letter codes to common names
luquillo_sppnames$Code <- substr(luquillo_sppnames$Code, 0, 5) #remove weird symbols
#Wide to long transformation
luquillo_long <- gather(luquillo, Species, Abundance, 11:53)
luquillo_commonnames <- left_join(na.omit(luquillo_long), luquillo_sppnames, by = c("Species" = "Code")) #add common names
luquillo_commonnames$CommonName <- tolower(luquillo_commonnames$CommonName)
luquillo_commonnames$CommonName <- gsub("-", " ", luquillo_commonnames$CommonName)
luquillo_id <- luquillo_commonnames %>%
  left_join(checklist.subs, by = c("CommonName" = "Common_name")) %>%
  left_join(traits.subs, by = c("CommonName" = "CommonName"))

###Check that all species were paired with an ID from checklist
spp_parkriver <- unique(parkriver_id[,c(6,9)]) #102 spp
spp_sev <- unique(sev_id[,8:9]) #83
spp_konza <- unique(konza_id[,c(12,21)]) #137
spp_luquillo <- unique(luquillo_id[,15:16]) #41

#number of NAs
sev.nas <- spp_sev[is.na(spp_sev$SISRecID),] #1
parkriver.nas <- spp_parkriver[is.na(spp_parkriver$SISRecID),] #0
konza.nas <- spp_konza[is.na(spp_konza$SISRecID),] #7
luquillo.nas <- spp_luquillo[is.na(spp_luquillo$SISRecID),] #3

konza_id <- konza_id[!konza_id$COMMONNAME %in% konza.nas$COMMONNAME,]
luquillo_id <- luquillo_id[!luquillo_id$CommonName %in% luquillo.nas$CommonName,]
sev_id <- sev_id[!sev_id$CommonName %in% sev.nas$CommonName,]

####make sure all sampling events for datasets have year and month
parkriver_id$date <- as.Date(parkriver_id$Date, format = "%d-%h-%y")
parkriver_id$year <- as.numeric(format(parkriver_id$date, format = "%Y"))
parkriver_id$month <- as.numeric(format(parkriver_id$date, format = "%m"))

konza_id$date <- paste0(konza_id$RECYEAR,"-",konza_id$RECMONTH,"-",konza_id$RECDAY)
konza_id$date <- as.Date(konza_id$date, format = "%Y-%m-%d")

sev_id$date <- as.Date(sev_id$day, format = "%j", origin = paste0("1.1.",sev_id$year))
sev_id$month <- as.numeric(format(sev_id$date, format = "%m"))

luquillo_id$date <- as.Date(luquillo_id$DATE, format = "%m/%d/%Y")
luquillo_id$month <- as.numeric(format(luquillo_id$date, format = "%m"))

#Subset relevant months to have even sampling effort across the year
#Sevilleta: #January/December, May/June
#Parker River : Every two months, use January, April, June, October
#Konza: January, April, June, October
#Luquillo: wet season May, dry season late Nov through late April if use months 11, 1, 2, 3 - 1990-1995
sev_id <- sev_id %>%
  filter(month == 1 | month == 12 | month == 5 | month == 6)
parkriver_id <- parkriver_id %>%
  filter(month == 1 | month == 6)
konza_id <- konza_id %>%
  filter(RECMONTH == 1 | RECMONTH == 6)
luquillo_id <- luquillo_id %>%
  filter(month == 5 | month == 1 | month == 2 | month == 11 | month == 3)

#add counts for spp in sev and konza
sev_id_count <- sev_id %>%
  group_by(year, month, day, habitat_type, stake_number, migclass, CommonName, SISRecID) %>%
  summarize(Count = n())
konza_id_count <- konza_id %>%
  group_by(RECYEAR, RECMONTH, RECDAY, WATERSHED, migclass, COMMONNAME, SISRecID) %>%
  summarize(Count = n())

######### Calculate metrics of interest #########
#habitat specialization scores
IUCNids <- unique(na.omit(c(sev_id$SISRecID, parkriver_id$SISRecID, konza_id$SISRecID, luquillo_id$SISRecID)))
finescale_habitats <- matrix(nrow = 294, ncol = 2) 
#~3 minutes runtime
for(i in 1:length(IUCNids)) {
  id <- IUCNids[i]
  finescale_habitats[i,1] <- id
  habitat <- birdlife_habitat(id)
  finescale_habitats[i,2] <- length(habitat$id)
  }
num_habitats <- data.frame(SISRecID = finescale_habitats[,1], Number_habitats = finescale_habitats[,2])
#habitats range from 0-30
#determine categories: lo medium hi specialization
hist(num_habitats$Number_habitats)
#0-5: lo
#5-15: medium
#15-30: hi (ie generalist)

sev_id_habitats <- left_join(sev_id_count, num_habitats, by = "SISRecID")
parkriver_id_habitats <- left_join(parkriver_id, num_habitats, by = "SISRecID")
konza_id_habitats <- left_join(konza_id_count, num_habitats, by = "SISRecID")
luquillo_id_habitats <- left_join(luquillo_id, num_habitats, by = "SISRecID")

#Merge relevant data into one dataframe for all four sites
sev_id_habitats$site <- rep("Sevilleta", times = length(sev_id_habitats$year))
parkriver_id_habitats$site <- rep("Parker River", times = length(parkriver_id_habitats$year))
luquillo_id_habitats$site <- rep("Luquillo", times = length(luquillo_id_habitats$YEAR))
konza_id_habitats$site <- rep("Konza", times = length(konza_id_habitats$RECYEAR))

lter.df <- data.frame(LTER = c(sev_id_habitats$site, parkriver_id_habitats$site, luquillo_id_habitats$site, konza_id_habitats$site),
                      year = c(sev_id_habitats$year,parkriver_id_habitats$year, luquillo_id_habitats$YEAR,konza_id_habitats$RECYEAR),
                      month = c(sev_id_habitats$month,parkriver_id_habitats$month, luquillo_id_habitats$month,konza_id_habitats$RECMONTH),
                      common_name = c(sev_id_habitats$CommonName,parkriver_id_habitats$Common.Name, luquillo_id_habitats$CommonName,konza_id_habitats$COMMONNAME),
                      count = c(sev_id_habitats$Count,parkriver_id_habitats$Count, luquillo_id_habitats$Abundance,konza_id_habitats$Count),
                      SISRecID = c(sev_id_habitats$SISRecID,parkriver_id_habitats$SISRecID, luquillo_id_habitats$SISRecID,konza_id_habitats$SISRecID),
                      number_habitats = c(sev_id_habitats$Number_habitats,parkriver_id_habitats$Number_habitats, luquillo_id_habitats$Number_habitats,konza_id_habitats$Number_habitats),
                      migclass = c(sev_id_habitats$migclass, parkriver_id_habitats$migclass, luquillo_id_habitats$migclass, konza_id_habitats$migclass))

####Annual species richness over time
richness <- lter.df %>%
  group_by(LTER, year, month) %>%
  summarize(richness = length(unique(common_name)))

####Annual %specializations etc
#categorize hi med lo
special <- c()
for(i in 1:length(lter.df$number_habitats)) {
  num <- lter.df$number_habitats[i]
  if(is.na(num)) {
    special <- c(special, NA)
  } else if (0 < num & num <= 5) {
    special <- c(special, "low")
  } else if(5 < num & num <= 15) {
    special <- c(special, "medium")
  } else if(15 < num) {
    special <- c(special, "high")
  } 
}

lter.df$specialist <- special

annualspecial <- lter.df %>%
  group_by(LTER, year,month, specialist) %>%
  summarize(total = length(unique(common_name)))

annualspecial$specialist <- factor(annualspecial$specialist, c("low", "medium", "high"))

####Abundance trends for trait groups
abund.annual <- lter.df %>%
  group_by(LTER, year, month, specialist) %>%
  summarize(abund = sum(count))
  
####Among year BC Index 
##rainy/summer only
sites <- c("Konza", "Sevilleta", "Parker River", "Luquillo")
betadivresults <- matrix(nrow = 65, ncol = 3)
betadivresults[,1] <- c(rep(1, times = 28), rep(2, times = 6), rep(3, times = 8), rep(4, times = 23))

for(k in 1:4) {
  site <- sites[k]
    df <- lter.df %>%
    filter(LTER == site, month == 5 | month == 6)
    betadiv <- matrix(nrow = 40, ncol = 3)
    for(i in 1:(length(unique(df$year))-1)) {
      year <- unique(df$year)[i]
      spp <- unique(df$SISRecID[df$year == year+1 | df$year == year])
      beta1 <- c()
      beta2 <- c()
      for(j in 1:length(spp)) {
        species <- spp[j]
        df.spp <- filter(df, SISRecID == species)
        xj <- sum(df.spp$count[df.spp$year == year])
        xk <- sum(df.spp$count[df.spp$year == year + 1])
        beta1 <- c(beta1, abs(xj-xk))
        beta2 <- c(beta2, abs(xj + xk))
      }
      betadiv[i,1] <- site
      betadiv[i,2] <- year
      betadiv[i,3] <- sum(beta1)/sum(beta2)
      
    }
    print(length(na.omit(betadiv[,1])))
    betadivresults[betadivresults[,1] == k, ] <- na.omit(betadiv)
} 
betadiv.df <- data.frame(LTER = betadivresults[,1], year = as.numeric(betadivresults[,2]), beta = as.numeric(betadivresults[,3]))
betadiv.df <- betadiv.df[betadiv.df$beta != 1.0,] #remove years where there was missing data in the following year
missing.beta <- betadiv.df[betadiv.df$beta == 1.0,]
betadiv_summ <- betadiv.df %>%
  group_by(LTER) %>%
  summarize(mean = mean(beta), sd = sd(beta))

####Among year BC Index by %specialists
sites <- c("Konza", "Sevilleta", "Parker River", "Luquillo")
betadivresults <- matrix(nrow = 65, ncol = 5)
betadivresults[,1] <- c(rep(1, times = 28), rep(2, times = 6), rep(3, times = 8), rep(4, times = 23))

for(k in 1:4) {
  site <- sites[k]
  df <- lter.df %>%
    filter(LTER == site, month == 5 | month == 6)
  betadiv <- matrix(nrow = 40, ncol = 5)
  for(i in 1:(length(unique(df$year))-1)) {
    year <- unique(df$year)[i]
    spp <- unique(df$SISRecID[df$year == year+1 | df$year == year])
    betahi <- c()
    betamed <- c()
    betalo <- c()
    beta2 <- c()
    for(j in 1:length(spp)) {
      species <- spp[j]
      df.spp <- filter(df, SISRecID == species)
      xj <- sum(df.spp$count[df.spp$year == year])
      xk <- sum(df.spp$count[df.spp$year == year + 1])
      xjhi <- sum(df.spp$count[df.spp$year == year & df.spp$specialist == "high"])
      xkhi <- sum(df.spp$count[df.spp$year == year + 1 & df.spp$specialist == "high"])
      xkmed <- sum(df.spp$count[df.spp$year == year & df.spp$specialist == "medium"])
      xjmed <- sum(df.spp$count[df.spp$year == year + 1& df.spp$specialist == "medium"])
      xklo <- sum(df.spp$count[df.spp$year == year & df.spp$specialist == "low"])
      xjlo <- sum(df.spp$count[df.spp$year == year + 1& df.spp$specialist == "low"])
      betahi <- c(betahi, abs(xjhi-xkhi))
      betamed <- c(betamed, abs(xjmed - xkmed))
      betalo <- c(betalo, abs(xjlo - xklo))
      beta2 <- c(beta2, abs(xj + xk))
    }
    betadiv[i,1] <- site
    betadiv[i,2] <- year
    betadiv[i,3] <- sum(betahi)/sum(beta2)
    betadiv[i,4] <- sum(betamed)/sum(beta2)
    betadiv[i,5] <- sum(betalo)/sum(beta2)
  }
  print(length(na.omit(betadiv[,1])))
  betadivresults[betadivresults[,1] == k, ] <- na.omit(betadiv)
} 
betadiv.special.df <- data.frame(LTER = betadivresults[,1], 
                         year = as.numeric(betadivresults[,2]), 
                         betahi = as.numeric(betadivresults[,3]),
                         betamed = as.numeric(betadivresults[,4]),
                         betalo = as.numeric(betadivresults[,5]))
betadiv_special <- betadiv.special.df %>%
  group_by(LTER) %>%
  summarize(meanHi = mean(betahi), sdHi = sd(betahi),
            meanMed = mean(betamed), sdMed = sd(betamed),
            meanLo = mean(betalo), sdLo = sd(betalo),
            summean = sum(mean(betahi), mean(betamed), mean(betalo)))

####Within year BC Index 
##between winter and summer, all except luquillo (compare wet and dry season)
lter.df.subs <- lter.df %>%
  arrange(LTER, year, month) %>%
  mutate(season = ifelse(month == 6 | month == 5, "S", "W")) %>% #create season code
  group_by(LTER, year) %>%
  summarize(total = length(unique(season))) %>%
  filter(total == 2) %>%
  left_join(lter.df, by = c("LTER", "year"))
  
beta.within <- lter.df.subs %>%
  mutate(season = ifelse(month == 6 | month == 5, "S", "W")) %>%
  group_by(LTER, year, common_name) %>%
  summarize(xj = sum(count[season == "S"]), xk = sum(count[season == "W"])) %>%
  group_by(LTER, year) %>%
  summarize(beta = sum(abs(xj - xk))/sum(abs(xj+xk)), denom = sum(abs(xj + xk)))

beta.within.sum <- beta.within %>%
  group_by(LTER) %>%
  summarize(mean = mean(beta), sd = sd(beta))

####Within year BC Index by %specialists
beta.within.sp <- lter.df.subs %>%
  mutate(season = ifelse(month == 6 | month == 5, "S", "W")) %>%
  group_by(LTER, year, specialist, common_name) %>%
  summarize(xj = sum(count[season == "S"]), xk = sum(count[season == "W"]))%>%
  left_join(beta.within, by = c("LTER", "year")) %>%
  group_by(LTER, year, specialist) %>%
  summarize(beta = sum(abs(xj - xk))/mean(denom))

beta.within.sp.sum <- beta.within.sp %>%
  group_by(LTER, specialist) %>%
  summarize(mean = mean(beta), sd = sd(beta))
            
#### Jaccard similarity (1 - J = dissimilarity)
#among year
sites <- c("Konza", "Sevilleta", "Parker River", "Luquillo")
jacresults <- matrix(nrow = 65, ncol = 3)
jacresults[,1] <- c(rep(1, times = 28), rep(2, times = 6), rep(3, times = 8), rep(4, times = 23))

for(k in 1:4) {
  site <- sites[k]
  df <- lter.df %>%
    filter(LTER == site, month == 5 | month == 6)
  betadiv <- matrix(nrow = 40, ncol = 3)
  for(i in 1:(length(unique(df$year))-1)) {
    year <- unique(df$year)[i]
    total <- unique(df$SISRecID[df$year == year+1 | df$year == year])
    yr <- unique(df$SISRecID[df$year == year])
    yr1 <- unique(df$SISRecID[df$year == year + 1])
    shared <- yr[yr %in% yr1]
    betadiv[i,1] <- site
    betadiv[i,2] <- year
    betadiv[i,3] <- 1 - length(shared)/length(total)
  }
  print(length(na.omit(betadiv[,1])))
  jacresults[jacresults[,1] == k, ] <- na.omit(betadiv)
  }
  
jaccard.df <- data.frame(LTER = jacresults[,1], year = as.numeric(jacresults[,2]), beta = as.numeric(jacresults[,3]))
jaccard.df <- jaccard.df[jaccard.df$beta != 0,] #remove years where there was missing data in the following year
missing.jaccard <- jaccard.df[jaccard.df$beta == 0,]
jaccard_summ <- jaccard.df %>%
  group_by(LTER) %>%
  summarize(mean = mean(beta), sd = sd(beta))

#within year
jac.within <- lter.df.subs %>%
  mutate(season = ifelse(month == 6 | month == 5, "S", "W")) %>%
  group_by(LTER, year) %>%
  summarize(total = length(unique(SISRecID)), shared = length(unique(SISRecID[unique(SISRecID[season == "S"]) %in% unique(SISRecID[season == "W"])]))) %>%
  group_by(LTER, year) %>%
  summarize(beta = shared/total)

jac.within.sum <- jac.within %>%
  group_by(LTER) %>%
  summarize(mean = 1- mean(beta), sd = sd(beta))

#among year specialists
sites <- c("Konza", "Sevilleta", "Parker River", "Luquillo")
jacresults1 <- matrix(nrow = 65, ncol = 5)
jacresults1[,1] <- c(rep(1, times = 28), rep(2, times = 6), rep(3, times = 8), rep(4, times = 23))

for(k in 1:4) {
  site <- sites[k]
  df <- lter.df %>%
    filter(LTER == site, month == 5 | month == 6)
  betadiv <- matrix(nrow = 40, ncol = 5)
  for(i in 1:(length(unique(df$year))-1)) {
    year <- unique(df$year)[i]
    total <- unique(df$SISRecID[df$year == year+1 | df$year == year])
    yr <- unique(df$SISRecID[df$year == year])
    yr1 <- unique(df$SISRecID[df$year == year + 1])
    notshared <- yr[!(yr %in% yr1)]
    notshared.hi <- notshared[notshared %in% df$SISRecID[df$specialist == "high"]]
    notshared.med <- notshared[notshared %in% df$SISRecID[df$specialist == "medium"]]
    notshared.lo <- notshared[notshared %in% df$SISRecID[df$specialist == "low"]]
    betadiv[i,1] <- site
    betadiv[i,2] <- year
    betadiv[i,3] <- length(notshared.hi)/length(total)
    betadiv[i,4] <- length(notshared.med)/length(total)
    betadiv[i,5] <- length(notshared.lo)/length(total)
  }
  print(length(na.omit(betadiv[,1])))
  jacresults1[jacresults1[,1] == k, ] <- na.omit(betadiv)
}

jaccard.sp.df <- data.frame(LTER = jacresults1[,1], 
                         year = as.numeric(jacresults1[,2]), 
                         beta.hi = as.numeric(jacresults1[,3]),
                         beta.med = as.numeric(jacresults1[,4]),
                         beta.lo = as.numeric(jacresults1[,5]))
jaccard.sp.df <- jaccard.sp.df[jaccard.sp.df$beta.hi != 0 | jaccard.sp.df$beta.med != 0 | jaccard.sp.df$beta.lo != 0,] #remove years where there was missing data in the following year
missing.jaccard <- betadiv.df[betadiv.df$beta == 0,]
jaccard.spp_summ <- jaccard.sp.df %>%
  group_by(LTER) %>%
  summarize(mean.hi = mean(beta.hi), sd.hi = sd(beta.hi),
            mean.med = mean(beta.med), sd.med = sd(beta.med),
            mean.lo = mean(beta.lo), sd.lo = sd(beta.lo))

#within year specialists
within.total <- lter.df.subs %>%
  mutate(season = ifelse(month == 6 | month == 5, "S", "W")) %>%
  group_by(LTER, year) %>%
  summarize(total = length(unique(SISRecID)))

jac.sp.within <- lter.df.subs %>%
  left_join(within.total, by = c("LTER", "year")) %>%
  mutate(season = ifelse(month == 6 | month == 5, "S", "W")) %>%
  group_by(LTER, year, specialist) %>%
  summarize(notshared = length(unique(SISRecID[(unique(SISRecID[season == "S"]) %in% unique(SISRecID[season == "W"]))])), 
            total = mean(total.y), beta = notshared/mean(total.y))

jac.sp.within.sum <- jac.sp.within %>%
  group_by(LTER, specialist) %>%
  summarize(mean = mean(beta), sd = sd(beta))

####Diversity corrected Jaccard
#B = 1 - mean(alpha)/gamma [gamma is total # of species all time points]
#Among year
sites <- c("Konza", "Sevilleta", "Parker River", "Luquillo")
spacialresults <- matrix(nrow = 65, ncol = 5)
spacialresults[,1] <- c(rep(1, times = 28), rep(2, times = 6), rep(3, times = 8), rep(4, times = 23))

for(k in 1:4) {
  site <- sites[k]
  df <- lter.df %>%
    filter(LTER == site, month == 5 | month == 6)
  betadiv <- matrix(nrow = 40, ncol = 5)
  for(i in 1:(length(unique(df$year))-1)) {
    year <- unique(df$year)[i]
    gamma <- length(unique(df$SISRecID))
    alpha <- length(unique(df$SISRecID[df$year == year]))
    betadiv[i,1] <- site
    betadiv[i,2] <- year
    betadiv[i,3] <- gamma
    betadiv[i,4] <- alpha
    betadiv[i,5] <- 1 - alpha/gamma
  }
  print(length(na.omit(betadiv[,1])))
  spacialresults[spacialresults[,1] == k, ] <- na.omit(betadiv)
}
spatial.df <- data.frame(site = spacialresults[,1],
                         year = as.numeric(spacialresults[,2]),
                          gamma = as.numeric(spacialresults[,3]),
                         alpha = as.numeric(spacialresults[,4]),
                         beta = as.numeric(spacialresults[,5]))

spatial.sum <- spatial.df %>%
  group_by(site) %>%
  summarize(mean = mean(beta), sd = sd(beta))

#within year
spatial.within <- lter.df.subs %>%
  mutate(season = ifelse(month == 6 | month == 5, "S", "W")) %>%
  group_by(LTER, year) %>%
  summarize(gamma = length(unique(SISRecID)), 
            alpha = length(unique(SISRecID[season == "W"])), 
            beta = 1 - alpha/gamma)

spatial.within.sum <- spatial.within %>%
  group_by(LTER) %>%
  summarize(mean = mean(beta), sd = sd(beta))

#Diversity corrected Jaccard among specialists
sites <- c("Konza", "Sevilleta", "Parker River", "Luquillo")
spacialresults1 <- matrix(nrow = 65, ncol = 5)
spacialresults1[,1] <- c(rep(1, times = 28), rep(2, times = 6), rep(3, times = 8), rep(4, times = 23))

for(k in 1:4) {
  site <- sites[k]
  df <- lter.df %>%
    filter(LTER == site, month == 5 | month == 6)
  betadiv <- matrix(nrow = 40, ncol = 5)
  for(i in 1:(length(unique(df$year))-1)) {
    year <- unique(df$year)[i]
    gamma <- unique(df$SISRecID)
    alpha <- unique(df$SISRecID[df$year == year])
    newspp <- gamma[!(gamma %in% alpha)]
    turnover.hi <- newspp[newspp %in% df$SISRecID[df$specialist == "high"]]
    turnover.med <- newspp[newspp %in% df$SISRecID[df$specialist == "medium"]]
    turnover.lo <- newspp[newspp %in% df$SISRecID[df$specialist == "low"]]
    betadiv[i,1] <- site
    betadiv[i,2] <- year
    betadiv[i,3] <- length(turnover.hi)/length(gamma)
    betadiv[i,4] <- length(turnover.med)/length(gamma)
    betadiv[i,5] <- length(turnover.lo)/length(gamma)
  }
  print(length(na.omit(betadiv[,1])))
  spacialresults1[spacialresults1[,1] == k, ] <- na.omit(betadiv)
}
spatial.sp.df <- data.frame(site = spacialresults1[,1],
                         year = as.numeric(spacialresults1[,2]),
                         beta.hi = as.numeric(spacialresults1[,3]),
                         beta.med = as.numeric(spacialresults1[,4]),
                         beta.lo = as.numeric(spacialresults1[,5]))

spatial.sp.sum <- spatial.sp.df %>%
  group_by(site) %>%
  summarize(mean.hi = mean(beta.hi), sd.hi = sd(beta.hi),
            mean.med = mean(beta.med), sd.med = sd(beta.med),
            mean.lo = mean(beta.lo), sd.lo = sd(beta.lo))

#within year specialists
spatial.sp.gamma <- lter.df.subs %>%
  mutate(season = ifelse(month == 6 | month == 5, "S", "W")) %>%
  group_by(LTER, year) %>%
  summarize(gamma = length(unique(SISRecID)))

spatial.sp.within <- lter.df.subs %>%
  left_join(spatial.sp.gamma, by = c("LTER", "year")) %>%
  mutate(season = ifelse(month == 6 | month == 5, "S", "W")) %>%
  group_by(LTER, year, specialist) %>%
  summarize(new = length(unique(SISRecID[!(unique(SISRecID[season == "S"]) %in% unique(SISRecID[season == "W"]))])),
            beta = new/mean(gamma))

  
spatial.within.sp.sum <- spatial.sp.within %>%
  group_by(LTER, specialist) %>%
  summarize(mean = mean(beta), sd = sd(beta))

####### Plots #######
#trait contingency table
sppunique <- data.frame(common_name = unique(lter.df$common_name))
lter.df$specialist <- as.factor(lter.df$specialist)
lter.traits <- lter.df[,c(4,8:9)]
join.spp <- left_join(sppunique, lter.traits, by = "common_name") %>%
  left_join(traits.subs, by = c("common_name" = "CommonName")) %>%
  distinct()

table(join.spp$specialist, join.spp$migclass.x) #80/179 species covered in this (Passerines)
chisq.test(join.spp$specialist, join.spp$migclass.x, simulate.p.value =  T) #p = 0.35, independent

setwd("C:/Users/gdicecco/Desktop/git/BIOL465_finalproj/")

#Fig. 1
#species richness
richness.plotdf <- richness[richness$month == 5 | richness$month == 6 ,]
richness.mu <- richness.plotdf %>%
  group_by(LTER) %>%
  summarize(mean = mean(richness)) %>%
  left_join(richness.plotdf, by = "LTER")
ggplot(richness.mu, aes(x = year, y = richness, color = LTER)) + geom_point() + geom_line() +
  labs(x = "Year", y = "Species Richness") + theme_classic() + geom_line(aes(x = year, y = mean, color = LTER), lty = 2)

#Fig. 2
#richness generalist/specialist
annual.plot <- annualspecial[annualspecial$month != 1 & annualspecial$month != 11 & annualspecial$month != 3
                             & annualspecial$month != 2 & annualspecial$month != 12, ]
annual.plot$specialist <- factor(annual.plot$specialist, levels = c("high", "medium", "low"))
richplot <- ggplot(annual.plot, aes(x = year, y = total, fill = specialist)) + geom_col(position = "stack") + facet_grid(~LTER) +
  labs(x = "Year", y = "Species Richness", fill = "Number of Habitats Used") + 
  theme_classic() + scale_fill_discrete(name = "Number of Habitats Used", 
                              breaks = c("low", "medium", "high"), 
                              labels = c("Low", "Medium", "High")) +
  theme(strip.background = element_blank())

#annual abundance
abund.annual$specialist <- factor(abund.annual$specialist, levels = c("high", "medium", "low"))
abundplot <- ggplot(abund.annual[abund.annual$month != 1 & abund.annual$month != 11 & abund.annual$month != 3
                                 & abund.annual$month != 2,], aes(x = year, y = log10(abund), fill = specialist)) + geom_col(position = "stack") + facet_grid(~LTER) +
  labs(x = "Year", y = "Log(Abundance)", fill = "Number of Habitats Used") + 
  theme_classic() +  scale_fill_discrete(name = "Number of Habitats Used", 
                               breaks = c("low", "medium", "high"), 
                               labels = c("Low", "Medium", "High")) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

plot_grid(richplot, abundplot, nrow = 2, ncol = 1, labels = c("(a)", "(b)"), label_size = 10)

#Fig. 3
#beta div
betadiv_summ$time <- rep("Among year", times = 4)
beta.within.sum$time <- rep("Within year", times = 4)
betadiv_summ$stat <- rep("Bray-Curtis", times = 4)
beta.within.sum$stat <- rep("Bray-Curtis", times = 4)
jaccard_summ$time <- rep("Among year", times = 4)
jaccard_summ$stat <- rep("Jaccard", times = 4)
jac.within.sum$time <- rep("Within year", times = 4)
jac.within.sum$stat <- rep("Jaccard", times = 4)
spatial.sum$time <- rep("Among year", times = 4)
spatial.sum$stat <- rep("Diversity Corrected Jaccard", times = 4)
spatial.within.sum$time <- rep("Within year", times = 4)
spatial.within.sum$stat <- rep("Diversity Corrected Jaccard", times = 4)
colnames(spatial.sum)[1] <- "LTER"
colnames(spatial.within.sum)[1] <- "LTER"

lats <- data.frame(LTER = sites, lat = c(39.09, 34.35, 42.76, 18.30))
#from https://lternet.edu/sites/coordinates
betadiv_all <- rbind(betadiv_summ, beta.within.sum, 
                     jaccard_summ, jac.within.sum, 
                     spatial.sum, spatial.within.sum) %>%
  left_join(lats, by = "LTER")

amongmod <- lm(mean ~ lat, data = betadiv_all)

#Among vs within year, all site
ggplot(betadiv_all, aes(x = lat, y = mean, color = time)) + geom_point(cex = 2) + 
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.5, width = 1) +
  theme_classic() + labs(y = "Mean Dissimilarity", color = "", x = "Latitude")+ geom_smooth(method = "lm", se = F, lty = 2) +
  facet_wrap(~stat)

#Fig. 4
#Within and Among year by trait group for each LTER & beta diversity metric
#Bray curtis
betadiv_sp_sum <- rbind(data.frame(LTER = betadiv_special$LTER, mean = betadiv_special$meanHi, sd = betadiv_special$sdHi, cat = rep("high", times = 4)),
                        data.frame(LTER = betadiv_special$LTER, mean = betadiv_special$meanMed, sd = betadiv_special$sdMed, cat = rep("medium", times = 4)),
                        data.frame(LTER = betadiv_special$LTER, mean = betadiv_special$meanLo, sd = betadiv_special$sdLo, cat = rep("low", times = 4)))
betadiv_sp_sum$cat <- factor(betadiv_sp_sum$cat, levels = c("high", "medium", "low"))
amongplot <- ggplot(betadiv_sp_sum, aes(x = LTER, y = mean, color = cat)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1, position = position_dodge(width = 0.5)) +
  labs(y = "Bray-Curtis", color = "Number of Habitats Used", x = "") +
  ggtitle("Among Year") + theme_classic() + 
  theme(legend.position = c(0.05, 0.95), 
        legend.justification = c(0, 1),
        plot.title = element_text(hjust = 0.5)) + 
  scale_color_discrete(name="No. of Habitats Used",
                      breaks=c("high", "medium", "low"), 
                      labels=c("High", "Medium", "Low")) +
  ylim(-0.07,1)

beta.within.sp.sum$specialist <- factor(beta.within.sp.sum$specialist, levels = c("high", "medium", "low"))
withinplot <- ggplot(beta.within.sp.sum, aes(x = LTER, y = mean, color = specialist)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1, position = position_dodge(width = 0.5)) +
  labs(y = "", x = "") + theme_classic() + ggtitle("Within Year") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  ylim(-0.07,1)

#Diversity Corrected Jaccard
spatial.sp.all <- rbind(data.frame(LTER = spatial.sp.sum$site, mean = spatial.sp.sum$mean.hi, sd = spatial.sp.sum$sd.hi, cat = rep("high", times = 4)),
                        data.frame(LTER = spatial.sp.sum$site, mean = spatial.sp.sum$mean.med, sd = spatial.sp.sum$sd.med, cat = rep("medium", times = 4)),
                        data.frame(LTER = spatial.sp.sum$site, mean = spatial.sp.sum$mean.lo, sd = spatial.sp.sum$sd.lo, cat = rep("low", times = 4)))
spatial.sp.all$cat <- factor(spatial.sp.all$cat, levels = c("high", "medium", "low"))

sp.amongplot <- ggplot(spatial.sp.all, aes(x = LTER, y = mean, color = cat)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1, position = position_dodge(width = 0.5)) +
  labs(y = "Diversity Corrected Jaccard", color = "Number of Habitats Used", x = "") +
  theme_classic() + 
  theme(legend.position = "none")+ 
  scale_color_discrete(name="No. of Habitats Used",
                       breaks=c("high", "medium", "low"), 
                       labels=c("High", "Medium", "Low")) +
  ylim(-0.07,1)

spatial.within.sp.sum$specialist <- factor(spatial.within.sp.sum$specialist, levels = c("high", "medium", "low"))
sp.withinplot <- ggplot(spatial.within.sp.sum, aes(x = LTER, y = mean, color = specialist)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1, position = position_dodge(width = 0.5)) +
  labs(y = "", x = "") + theme_classic() +
  theme(legend.position = "none") +
  ylim(-0.15,1)

#Jaccard
jaccard.sp_all <- rbind(data.frame(LTER = jaccard.spp_summ$LTER, mean = jaccard.spp_summ$mean.hi, sd = jaccard.spp_summ$sd.hi, cat = rep("high", times = 4)),
                        data.frame(LTER = jaccard.spp_summ$LTER, mean = jaccard.spp_summ$mean.med, sd = jaccard.spp_summ$sd.med, cat = rep("medium", times = 4)),
                        data.frame(LTER = jaccard.spp_summ$LTER, mean = jaccard.spp_summ$mean.lo, sd = jaccard.spp_summ$sd.lo, cat = rep("low", times = 4)))
jaccard.sp_all$cat <- factor(jaccard.sp_all$cat, levels = c("high", "medium", "low"))

j.amongplot <- ggplot(jaccard.sp_all, aes(x = LTER, y = mean, color = cat)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1, position = position_dodge(width = 0.5)) +
  labs(y = "Jaccard", color = "Number of Habitats Used", x = "") +
  theme_classic() + 
  theme(legend.position = "none") + 
  scale_color_discrete(name="No. of Habitats Used",
                       breaks=c("high", "medium", "low"), 
                       labels=c("High", "Medium", "Low")) +
  ylim(-0.07,1)

jac.sp.within.sum$specialist <- factor(jac.sp.within.sum$specialist, levels = c("high", "medium", "low"))
j.withinplot <- ggplot(jac.sp.within.sum, aes(x = LTER, y = mean, color = specialist)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1, position = position_dodge(width = 0.5)) +
  labs(y = "", x = "") + theme_classic() +
  theme(legend.position = "none") +
  ylim(-0.15,1)


plot_grid(amongplot, withinplot, sp.amongplot, sp.withinplot, j.amongplot, j.withinplot, align = "h",
          ncol = 2, nrow = 3,
          labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"), label_size = 12)

#Figure 5
#Mean dissimilarity vs richness and abundance for trait groups
sprichness.plot <- lter.df %>%
  group_by(LTER, specialist, year) %>%
  summarize(total = length(unique(common_name))) %>%
  group_by(LTER, specialist) %>%
  summarize(avgrich = mean(total))

abund.plot <- lter.df %>%
  group_by(LTER, specialist, year) %>%
  summarize(abund = sum(count)) %>%
  group_by(LTER, specialist) %>%
  summarize(avgabund = mean(abund))

#make combined df of data for all three beta div metrics
colnames(spatial.sp.all)[4] <- "specialist"
spatial.within.sp.sum$time <- rep("Within Year", times = length(spatial.within.sp.sum$LTER))
spatial.sp.all$time <- rep("Among Year", times = length(spatial.sp.all$LTER))
spatial.within.sp.sum$stat <- rep("Diversity Corrected Jaccard",  times = length(spatial.within.sp.sum$LTER))
spatial.sp.all$stat <- rep("Diversity Corrected Jaccard",  times = length(spatial.sp.all$LTER))

jac.sp.within.sum$time <- rep("Within Year", times = length(jac.sp.within.sum$LTER))
colnames(jaccard.sp_all)[4] <- "specialist"
jaccard.sp_all$time <- rep("Among Year", times = length(jaccard.sp_all$LTER))
jac.sp.within.sum$stat <- rep("Jaccard",  times = length(jac.sp.within.sum$LTER))
jaccard.sp_all$stat <- rep("Jaccard",  times = length(jaccard.sp_all$LTER))

beta.within.sp.sum$time <- rep("Within Year", times = length(beta.within.sp.sum$LTER))
colnames(betadiv_sp_sum)[4] <- "specialist"
betadiv_sp_sum$time <- rep("Among Year", times = length(betadiv_sp_sum$LTER))
beta.within.sp.sum$stat <- rep("Bray-Curtis Index",  times = length(beta.within.sp.sum$LTER))
betadiv_sp_sum$stat <- rep("Bray-Curtis Index",  times = length(betadiv_sp_sum$LTER))

traits.all <- bind_rows(betadiv_sp_sum, beta.within.sp.sum, jaccard.sp_all, jac.sp.within.sum, spatial.sp.all, spatial.within.sp.sum) %>%
  left_join(sprichness.plot, by = c("LTER", "specialist")) %>%
  left_join(abund.plot, by = c("LTER", "specialist"))

dis.richplot <- ggplot(traits.all, aes(x = avgrich, y = mean, color = stat)) + geom_point() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.5, width = 0.2) + 
  theme_classic() + labs(x = "Mean Annual Species Richness", y = "Mean Dissimilarity", color = "Beta Diversity Metric") +
  geom_smooth(method = "lm", se = F) + ylim(-0.15,1) + facet_wrap(~time) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        legend.position = c(0.12,0.80),
        legend.background = element_rect(fill = "transparent"))

dis.abundplot <- ggplot(traits.all, aes(x = log10(avgabund), y = mean, color = stat)) + geom_point() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.5, width = 0.1) + 
  theme_classic() + labs(x = "Log(Mean Annual Abundance)", y = "Mean Dissimilarity", color = "Beta Diversity Metric") +
  geom_smooth(method = "lm", se = F) + ylim(-0.15,1) + facet_wrap(~time) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(color = "white"),
        legend.position = "none")
plot_grid(dis.richplot, dis.abundplot, ncol = 1, labels = c("(a)", "(b)"))
