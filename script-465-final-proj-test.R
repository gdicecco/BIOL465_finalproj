require(traits)
require(tidyr)
require(dplyr)

#########Read in datasets#########
#make sure in long format with species common names
##BirdLife checklist (ID numbers)
checklist <- read.csv("/Users/gracedicecco/Desktop/git/LTER_birdabund_seasonal/BirdLife_Checklist_V_9.1.csv", header = TRUE)
checklist.subs <- checklist[,c(4,13)]
checklist.subs$Common_name <- tolower(checklist.subs$Common_name)

#Konza LTER
konza <- read.csv("/Users/gracedicecco/Desktop/git/LTER_birdabund_seasonal/CBP011_0.csv")
konza$COMMONNAME <- tolower(konza$COMMONNAME)
#January, April, June, October
konza_id <- left_join(konza, checklist.subs, by = c("COMMONNAME" = "Common_name"))

#Park River NWR LTER
parkriver <- read.csv("/Users/gracedicecco/Desktop/git/LTER_birdabund_seasonal/MON-EX-PRNWR-Volunteer-Birds.csv", header = TRUE)
#Every two months, use January, April, June, October
parkriver$Common.Name <- tolower(parkriver$Common.Name)
parkriver_id <- left_join(parkriver, checklist.subs, by = c("Common.Name" = "Common_name"))

#Sevilleta LTER
sev <- read.table("/Users/gracedicecco/Desktop/git/LTER_birdabund_seasonal/sev017_birdpop_09251997_0.txt", header = T, sep = ",")
#February, May, August, December
sev_table <- read.csv("/Users/gracedicecco/Desktop/git/LTER_birdabund_seasonal/sev_table_aou.csv") #AOU to common name table
sev_table_col <- separate(sev_table, 1, into = c("AOU", "CommonName"), sep = "   - ") #remove separators  
sev_table_col$AOU <- as.factor(sev_table_col$AOU)
sev_join <- left_join(sev, sev_table_col, by = c("species_number" = "AOU")) #join common name to AOU
sev_join$CommonName <- tolower(sev_join$CommonName)
sev_id <- left_join(sev_join, checklist.subs, by = c("CommonName" = "Common_name")) #join ID number by common name

#Luquillo LTER
luquillo_LT_25 <- read.csv("/Users/gracedicecco/Desktop/git/LTER_birdabund_seasonal/ALL_ElVerde_LT_25m.csv", header = TRUE)
luquillo_GT_25 <- read.csv("/Users/gracedicecco/Desktop/git/LTER_birdabund_seasonal/ALL_ElVerde_GT_25m.csv", header = TRUE)
luquillo <- rbind(luquillo_LT_25, luquillo_GT_25) #join less than 25 m and greater than 25 m data
#May and June for many years, Fall(Sept-Nov) for at least 3 years, Winter (Jan-March) for at least 3 years
#Rainy season: April-Nov
luquillo_sppnames <- read.csv("/Users/gracedicecco/Desktop/git/LTER_birdabund_seasonal/luquillo_spp_code.csv") #five letter codes to common names
luquillo_sppnames$Code <- substr(luquillo_sppnames$Code, 0, 5) #remove weird symbols
#Wide to long transformation
luquillo_long <- gather(luquillo, Species, Abundance, 11:53)
luquillo_long_naomit <- na.omit(luquillo_long) #remove NAs
luquillo_commonnames <- left_join(luquillo_long_naomit, luquillo_sppnames, by = c("Species" = "Code")) #add common names
luquillo_commonnames$CommonName <- tolower(luquillo_commonnames$CommonName)
luquillo_id <- left_join(luquillo_commonnames, checklist.subs, by = c("CommonName" = "Common_name"))

###Check that all species were paired with an ID from checklist
spp_parkriver <- unique(parkriver_id[,c(6,9)])
spp_sev <- unique(sev_id[,8:9])
spp_konza <- unique(konza_id[,c(12,21)])
spp_luquillo <- unique(luquillo_id[,15:16])
#NAs: mainly where there are old common names being used

#number of NAs: 55 total across sites
sev.nas <- spp_sev[is.na(spp_sev$SISRecID),] #15 species
parkriver.nas <- spp_parkriver[is.na(spp_parkriver$SISRecID),] #9 species
konza.nas <- spp_konza[is.na(spp_konza$SISRecID),] #18 species
luquillo.nas <- spp_luquillo[is.na(spp_luquillo$SISRecID),] #13 species
