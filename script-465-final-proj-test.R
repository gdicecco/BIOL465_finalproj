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

######### Read in datasets #########
setwd("\\\\BioArk\\hurlbertlab\\DiCecco\\LTER_birdabund_seasonal\\")

##BirdLife checklist (ID numbers)
checklist <- read.csv("BirdLife_Checklist_V_9.1.csv", header = TRUE)
checklist.subs <- checklist[,c(4,13)]
#reformat character strings
checklist.subs$Common_name <- tolower(checklist.subs$Common_name)
checklist.subs$Common_name <- gsub("-", " ", checklist.subs$Common_name)

#Konza LTER
konza <- read.csv("CBP011_0.csv")
konza$COMMONNAME <- tolower(konza$COMMONNAME)
konza$COMMONNAME <- gsub("-", " ", konza$COMMONNAME)
#January, April, June, October
konza_id <- left_join(konza, checklist.subs, by = c("COMMONNAME" = "Common_name"))

#Park River NWR LTER
parkriver <- read.csv("MON-EX-PRNWR-Volunteer-Birds.csv", header = TRUE)
#Every two months, use January, April, June, October
parkriver$Common.Name <- tolower(parkriver$Common.Name)
parkriver$Common.Name <- gsub("-", " ", parkriver$Common.Name)
parkriver_id <- left_join(parkriver, checklist.subs, by = c("Common.Name" = "Common_name"))

#Sevilleta LTER
sev <- read.table("sev017_birdpop_09251997_0.txt", header = T, sep = ",")
#February, May, August, December
sev_table <- read.csv("sev-table-aou.csv") #AOU to common name table
sev_table_col <- separate(sev_table, 1, into = c("AOU", "CommonName"), sep = "   - ") #remove separators  
sev_table_col$AOU <- as.factor(sev_table_col$AOU)
sev_join <- left_join(sev, sev_table_col, by = c("species_number" = "AOU")) #join common name to AOU
sev_join$CommonName <- tolower(sev_join$CommonName)
sev_join$CommonName <- gsub("-", " ", sev_join$CommonName)
sev_id <- left_join(sev_join, checklist.subs, by = c("CommonName" = "Common_name")) #join ID number by common name

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
luquillo_long_naomit <- na.omit(luquillo_long) #remove NAs
luquillo_commonnames <- left_join(luquillo_long_naomit, luquillo_sppnames, by = c("Species" = "Code")) #add common names
luquillo_commonnames$CommonName <- tolower(luquillo_commonnames$CommonName)
luquillo_commonnames$CommonName <- gsub("-", " ", luquillo_commonnames$CommonName)
luquillo_id <- left_join(luquillo_commonnames, checklist.subs, by = c("CommonName" = "Common_name"))

###Check that all species were paired with an ID from checklist
spp_parkriver <- unique(parkriver_id[,c(6,9)]) #102 spp
spp_sev <- unique(sev_id[,8:9]) #83
spp_konza <- unique(konza_id[,c(12,21)]) #137
spp_luquillo <- unique(luquillo_id[,15:16]) #41
#NAs: mainly where there are old common names being used

#number of NAs
sev.nas <- spp_sev[is.na(spp_sev$SISRecID),] #1
parkriver.nas <- spp_parkriver[is.na(spp_parkriver$SISRecID),] #0
konza.nas <- spp_konza[is.na(spp_konza$SISRecID),] #7
luquillo.nas <- spp_luquillo[is.na(spp_luquillo$SISRecID),] #3

#make sure all sampling events for datasets have year and month
parkriver_id$date <- as.Date(parkriver_id$Date, format = "%d-%h-%y")
parkriver_id$year <- as.numeric(format(parkriver_id$date, format = "%Y"))
parkriver_id$month <- as.numeric(format(parkriver_id$date, format = "%m"))

konza_id$date <- paste0(konza_id$RECYEAR,"-",konza_id$RECMONTH,"-",konza_id$RECDAY)
konza_id$date <- as.Date(konza_id$date, format = "%Y-%m-%d")

sev_id$date <- as.Date(sev_id$day, format = "%j", origin = paste0("1.1.",sev_id$year))
sev_id$month <- as.numeric(format(sev_id$date, format = "%m"))

luquillo_id$date <- as.Date(luquillo_id$DATE, format = "%m/%d/%Y")
luquillo_id$month <- as.numeric(format(luquillo_id$date, format = "%m"))

#add counts for spp in sev
sev_id_count <- sev_id %>%
  group_by(year, month, day, habitat_type, stake_number, CommonName, SISRecID) %>%
  summarize(Count = n())
konza_id_count <- konza_id %>%
  group_by(RECYEAR, RECMONTH, RECDAY, WATERSHED, COMMONNAME, SISRecID) %>%
  summarize(Count = n())

#assign season to each sampling event


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
#0-10: hi
#10-20: medium
#20-30: lo #ie generalist

sev_id_habitats <- left_join(sev_id_count, num_habitats, by = "SISRecID")
parkriver_id_habitats <- left_join(parkriver_id, num_habitats, by = "SISRecID")
konza_id_habitats <- left_join(konza_id_count, num_habitats, by = "SISRecID")
luquillo_id_habitats <- left_join(luquillo_id, num_habitats, by = "SISRecID")

sev_id_habitats$site <- rep("Sevilleta", times = length(sev_id_habitats$year))
parkriver_id_habitats$site <- rep("Park River", times = length(parkriver_id_habitats$year))
luquillo_id_habitats$site <- rep("Luquillo", times = length(luquillo_id_habitats$YEAR))
konza_id_habitats$site <- rep("Konza", times = length(konza_id_habitats$RECYEAR))

#Merge relevant data into one dataframe for all four sites
lter.df <- data.frame(LTER = c(sev_id_habitats$site, parkriver_id_habitats$site, luquillo_id_habitats$site, konza_id_habitats$site),
                      year = c(sev_id_habitats$year,parkriver_id_habitats$year, luquillo_id_habitats$YEAR,konza_id_habitats$RECYEAR),
                      month = c(sev_id_habitats$month,parkriver_id_habitats$month, luquillo_id_habitats$month,konza_id_habitats$RECMONTH),
                      common_name = c(sev_id_habitats$CommonName,parkriver_id_habitats$Common.Name, luquillo_id_habitats$CommonName,konza_id_habitats$COMMONNAME),
                      count = c(sev_id_habitats$Count,parkriver_id_habitats$Count, luquillo_id_habitats$Abundance,konza_id_habitats$Count),
                      SISRecID = c(sev_id_habitats$SISRecID,parkriver_id_habitats$SISRecID, luquillo_id_habitats$SISRecID,konza_id_habitats$SISRecID),
                      number_habitats = c(sev_id_habitats$Number_habitats,parkriver_id_habitats$Number_habitats, luquillo_id_habitats$Number_habitats,konza_id_habitats$Number_habitats))

####Annual species richness over time
richness <- lter.df %>%
  group_by(LTER, year) %>%
  summarize(richness = length(unique(common_name)))

####Annual %specializations etc
#categorize hi med lo
special <- c()
for(i in 1:length(lter.df$number_habitats)) {
  num <- lter.df$number_habitats[i]
  if(is.na(num)) {
    special <- c(special, NA)
  } else if (0 < num & num <= 10) {
    special <- c(special, "low")
  } else if(10 < num & num <= 20) {
    special <- c(special, "medium")
  } else if(20 < num) {
    special <- c(special, "high")
  } 
}

lter.df$specialist <- special

annualspecial <- na.omit(lter.df) %>%
  group_by(LTER, year, specialist) %>%
  summarize(total = length(unique(common_name)))

annualspecial$specialist <- factor(annualspecial$specialist, c("low", "medium", "high"))

####Within year beta diversity


####Among year beta diversity
lter.df <- arrange(lter.df, LTER, year, month)

sites <- c("Konza", "Sevilleta", "Park River", "Luquillo")
betadivresults <- matrix(nrow = 70, ncol = 3)
betadivresults[,1] <- c(rep(1, times = 28), rep(2, times = 6), rep(3, times = 9), rep(4, times = 27))

for(k in 1:4) {
  site <- sites[k]
    df <- na.omit(lter.df) %>%
    filter(LTER == site)
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
betadiv.df <- data.frame(LTER = betadivresults[,1], year = as.numeric(betadivresults[,2]), beta <- as.numeric(betadivresults[,3]))

####Beta div by %specialists

####### Plots #######
blank <- theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#species richness
ggplot(richness, aes(x = year, y = richness, color = LTER)) + geom_point() + geom_line() +
  labs(x = "Year", y = "Species Richness") + blank

#annual generalist/specialist
ggplot(annualspecial, aes(x = year, y = total, fill = specialist)) + geom_col(position = "stack") + facet_grid(~LTER) +
  labs(x = "Year", y = "Species Richness", fill = "Number of Habitats Used") + blank

#annual beta div
ggplot(betadiv.df, aes(x = year, y = beta, color = LTER)) + geom_point() + geom_line() + blank

