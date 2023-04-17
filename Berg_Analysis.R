##Bergmann's Rule in Caprimulgids##

bs <- "/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/"
#load(paste0(bs, "Desktop/Grad_School/R_Files/MS/BergAnalysis7.27.22.Rdata"))

library(dplyr)
select <- dplyr::select
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggpubr)
library(viridis)
library(sp)
library(sf)
library(geosphere)
library(GeoWsphere)
library(lubridate)
library(stringi)
library(stringr)
library(gtools)
library(naniar)
library(cowplot)
library(AICcmodavg)
library(MuMIn)
library(gridExtra)
library(lmtest)
library(nlme)
library(readxl)
library(chron)
library(suncalc)
#library(conflicted)
ggplot2::theme_set(theme_cowplot())

# Load and format data ----------------------------------------------------
#load(paste0(bs,"Desktop/Grad_School/MS/EWPW/Data_Whips/Analysis/R_Files/spdf.all.tr.Rdata"))
EKconiFAC <- read.csv(paste0(bs, "Desktop/Grad_School/MS/EWPW/Writing_Exit_Seminar/Bergs_Rule/Data_share/Data/EK_DataSheet_CONI_Feb16.csv")) #Notice there are 7 or 8 rows of repeat individuals 2nd wintering locations. This is removed further down w/ band_age given that it's same band # and same deployment year
EKconiBr <- read.csv("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing_Exit_Seminar/Bergs_Rule/Data_share/Data/EK_CONI_Breeding.csv")
ASewpw <- read.csv("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing_Exit_Seminar/Bergs_Rule/Data_share/Data/Tonra_Ward_EWPW_Berg_Combined.csv")
AKewpw <-read.csv(paste0(bs, "Desktop/Grad_School/MS/EWPW/Writing_Exit_Seminar/Bergs_Rule/Data_share/Data/AK_Bergs_10Apr2023.csv"))
MBewpw <-read.csv(paste0(bs, "Desktop/Grad_School/MS/EWPW/Writing_Exit_Seminar/Bergs_Rule/Data_share/Data/MB_VItz_EWPW_Massachusetts.csv"))
JHeunj <-read.csv(paste0(bs, "Desktop/Grad_School/MS/EWPW/Writing_Exit_Seminar/Bergs_Rule/Data_share/Data/JH_Finland_v6.csv"))
LJeunj <- read.csv("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing_Exit_Seminar/Bergs_Rule/Data_share/Data/Nightjar_data_Denmark_Thorup_Jacobsen/LJ_eunj_2023_DK.csv")##SEE NEW sheet w/ updated age code
GCeunj <- read.csv("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing_Exit_Seminar/Bergs_Rule/Data_share/Data/GJC_Bergmann_UK_updated.csv")
GNeunj <- read.csv("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing_Exit_Seminar/Bergs_Rule/Data_share/Data/GN_Sweden_2023-03-07.csv")
REeunj <- read.csv("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing_Exit_Seminar/Bergs_Rule/Data_share/Data/Evens_EUNI/EvensLathouwers_data_sheet.csv")
ITeunj <- read.csv("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing_Exit_Seminar/Bergs_Rule/Data_share/Data/Boano_Italy_2023-03-23.csv")
VariousGC <- read.csv("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing_Exit_Seminar/Bergs_Rule/Data_share/Data/Various_EUNI_Breeding_only.csv")

#Make necessary changes for data formatting
cols <- c("Project", "Year", "Banding.Date", "Recap.", "Banding.Time", "Species", "Site.name", "Country", "Band.Number","TagID", "Age", "Sex", "CP","BP","Fat", "Wing.Chord", "WingFlat", "Tail.Length", "Tarsus","Mass","B.Lat","B.Long","W.Lat", "W.Long", "B.dep","W.arr", "Mig.dist", "Mig.n", "Temp.res.mig")


##Handle the pecularities of each data set to allow for combination and more efficient cleaning##
#Greg Various data sets
table(VariousGC$Project)
test <- VariousGC %>% select("B.Lat", "Project", "Country") %>% filter(Project == "EDB")
Various <- VariousGC %>% mutate(Year = sapply(str_split(Banding.Date, "/"), function(x){x[3]}), 
                              Project = paste0("Greg_", Project),
                              Banding.Date = as.character(dmy(Banding.Date))) %>% 
  select(1,30, 2:29)
  
#Greg precision w/ decimals
unique(str_sub(sapply(str_split(Various$B.Lat, "[.]"), function(x){x[2]}), start= -4))


#Merge Elly's data to create a single data frame 
#EKconi2 <- smartbind(EKconiFAC, EKconiBr)
EKconi %>% filter(duplicated(BandNumber) | duplicated(BandNumber, fromLast = T)) %>% arrange(BandNumber) %>% select(BandNumber, BandDate, BandTime, W.Lat, Year)

EKconiFAC <- EKconiFAC %>% filter(Wint.Loc == 1)
EKconiFAC$uniq.ID <- with(EKconiFAC, paste0(BandNumber, "_", TagID))
EKconiBr$uniq.ID <- with(EKconiBr, paste0(BandNumber, "_", TagID))
EKconi <- merge(EKconiBr, EKconiFAC[c("uniq.ID", "W.Lat", "W.Long", "Wint.Loc", "Dist.Fall", "N.Fall", "Dist.Spring", "N.Spring", "B.dep" , "W.arr", "W.dep", "B.arr")], by = "uniq.ID", all.x = T)
EKconi <- EKconi %>% 
  mutate(Temp.res.mig = ifelse(Year == 2017 | Year == 2018, 7, 10),
                            WingChord = WingChord * 10,
                            WingChord = ifelse(BandNumber == "1212-58492", 198, WingChord), #Fix typo
                            TailLength = TailLength * 10, 
                            Mass = ifelse(Mass == 0, NA, Mass),
                            B.dep = as.character(as.Date(B.dep,    
                                            origin = as.Date(paste0(Year, "-01-01")))),
                            W.arr = as.character(as.Date(W.arr,   
                                            origin = as.Date(paste0(Year, "-01-01")))),
         BandTime = ifelse(BandNumber =="1352-97758" | BandNumber == "1352-97768", "21:15:00", BandTime)) %>%
         #Remove 12 individuals that Elly says may not be unique captures
  filter(!(SiteName == "Alberta" & B.Lat > 57 & BandNumber == "Unbanded" & is.na(W.Lat))) %>%
  select(c(2:23,25,26,32,33,28,29,36,24)) %>%
  mutate(BandNumber = ifelse(BandNumber == "Unbanded" & !is.na(TagID), TagID, BandNumber)) %>%
  mutate(BandNumber = ifelse(BandNumber == "Unbanded" | BandNumber == "-99", paste0("Unbanded", c(1:nrow(EKconi))), BandNumber))
#Ensure that the TagID replaced 'Unbanded' and that Unbandeds are now unique
EKconi %>% arrange(BandNumber) %>% select(BandNumber, TagID)

AKewpw$Project <- "Korpach"

#Greg 9 FAC birds
GCeunj <- GCeunj %>% 
  mutate(Project = "UK_GJC", 
         Wing.Chord = WingFlatStraight,
         WingFlat = "Y", 
         W.Lat = ifelse(Band.Number == "RR05040", NA, W.Lat),
         W.Long = ifelse(Band.Number == "RR05040", NA, W.Long),
         W.arr = ifelse(Band.Number == "LA42163" | Band.Number == "RR05040", NA, W.arr), 
         W.arr = ifelse(Band.Number == "LE31898", "3/11/2021", W.arr)) %>% 
  select(-WingFlatStraight)
#Greg's is only dates in the wrong format
GCeunj[c("Banding.Date", "B.dep", "W.arr")] <- lapply(GCeunj[c("Banding.Date", "B.dep", "W.arr")], function(x){as.character(dmy(x))}) #1 failing to parse is OK 

#Lars 
LJeunj <- LJeunj %>% mutate(Species = "EUNI",
                  Project = "LJdenmark", 
                  B.dep = ifelse(B.dep == "1-sep", "9/1/10", B.dep))

#Create list, adjust columns and colnames, create single dataframe
njdfs_all <- list(EKconi, ASewpw, AKewpw, MBewpw, JHeunj, LJeunj, GCeunj, GNeunj, REeunj, ITeunj, Various)
names(njdfs_all) <- c("EKconi", "ASewpw", "AKewpw", "MBewpw", "JHeunj", "LJeunj", "GCeunj", "GNeunj", "REeunj", "ITeunj", "Various")
njdfs_all <- lapply(njdfs_all, function(x){x[,c(1:29)]})
njdfs_all <- lapply(njdfs_all, setNames, cols)
capri.df <- rbind(njdfs_all[[1]], njdfs_all[[2]], njdfs_all[[3]],njdfs_all[[4]], njdfs_all[[5]], njdfs_all[[6]], njdfs_all[[7]], njdfs_all[[8]], njdfs_all[[9]], njdfs_all[[10]], njdfs_all[[11]])
#lapply(njdfs_all, function(x){head(x$Banding.Date)})

read.csv("capriElly4.16.csv")

#Adjust time & date
capri.df <- as.data.frame(capri.df %>% filter(!is.na(B.Lat)) %>% replace_with_na_all(condition = ~.x %in% c(-99,-990, 9999, "<NA>", "-", ".", "na", 'NONABAND'))) #Few individuals removed w/ no B.Lat
nrow(capri.df) #6857 rows
capri.df <- capri.df %>% mutate(Species = ifelse(capri.df$Species == "Ceur" | capri.df$Species == "European Nightjar" | capri.df$Species == "European Nigthtjar", "EUNI", capri.df$Species),
                    Band.Number = stri_replace_all_regex(capri.df$Band.Number,
                                           pattern=c('-', ' '),
                                           replacement=c(''),
                                           vectorize=FALSE))

capri.df$Banding.Time <- str_pad(capri.df$Banding.Time, 4, pad = "0")
capri.df$Banding.Time <- sapply(str_split(parse_date_time(capri.df[,c("Banding.Time")], c("HMS"), truncated = 3), " "), function(x){x[2]})
capri.df$Banding.Time <- chron(times = capri.df$Banding.Time)
capri.df$Year <- str_pad(capri.df$Year, 3, pad = "0")
capri.df$Year <- str_pad(capri.df$Year, 4, pad = "2")
#Ensure that this worked
capri.df %>% select(Project, Year) %>% mutate(ncharYr = nchar(as.character(Year))) %>% arrange(ncharYr, desc(Year))

#Format times & dates, data types#
capri.df[,c("Wing.Chord","Mass","W.Lat", "W.Long", "Mig.dist", "Year")] <- lapply(capri.df[,c("Wing.Chord","Mass","W.Lat", "W.Long", "Mig.dist", "Year")], as.numeric)
capri.df[c("Banding.Date", "B.dep", "W.arr")] <- lapply(capri.df[c("Banding.Date", "B.dep", "W.arr")], parse_date_time, c("mdy", "ymd")) 
#Create month day (all years = 2023) for understanding timing
capri.df[,c("Band.md","Bdep.md", "Warr.md")] <- lapply(capri.df[,c("Banding.Date", "B.dep", "W.arr")], format, "%m/%d")
capri.df[,c("Band.md","Bdep.md", "Warr.md")] <- lapply(capri.df[,c("Band.md","Bdep.md", "Warr.md")], as.Date, "%m/%d")
capri.df$Warr.md <- as.Date(ifelse(capri.df$Warr.md < as.POSIXct("2023-04-01"), capri.df$Warr.md + lubridate::years(1), capri.df$Warr.md)) 
nrow(capri.df) #6857 rows
str(capri.df)

#Ensure no NAs that could cause problems downstream. DELETE?
lapply(capri.df, summary)

#Take a closer look at birds with wintering data
facRed <- capri.df %>% filter(!is.na(W.Lat)) %>% select(Project, Banding.Date, Banding.Time, Year, Band.Number, Age, Sex, Mass, Wing.Chord, B.dep, W.arr) #fac reduced
lapply(facRed, function(x) {table(is.na(x))})
table(facRed$Age)
facRed %>% filter(!duplicated(Band.Number)) %>% nrow() #Should end with 189 unique individuals!

btna <- facRed %>% filter(is.na(Banding.Time)) #banding time NAs
TF <- capri.df$Band.Number %in% btna$Band.Number
capri.df[TF,] %>% arrange(Band.Number) %>% select(Project, Site.name, Banding.Date, Banding.Time, Year, Band.Number, TagID, Mass, W.Lat) %>% filter(Project == "EvensLathouwers") %>% select(Band.Number) %>% pull() %>% unique() #Wing.Chord, Mass,

facRed %>% filter(is.na(Wing.Chord))
capri.df %>% filter(Band.Number == 137257703)
capri.df %>% filter(Band.Number == 137228576)


# Age classes condense ----------------------------------------------------
#Don't run this more than once
table(capri.df$Age) #, capri.df$Species)
table(capri.df$Sex)
# #L & 1 are nestling / pullus, 3 = HY, 4 = AHY, 5 = SY, 6 = ASY, 8 = ASY
capri.df <- capri.df %>% filter(Age != "1" & Age != "L" & Age != "3") %>% #1st reduction in sample
  mutate(Age = case_when(Age == "4" ~ "Unk", #This should be adult? 
                         Age == "AHY" ~ "Unk",
                         Age == "ASY?" ~ "Unk",
                         Age == "5" ~ "Young",
                         Age == "SY" ~ "Young",
                         Age == "6" ~ "Adult",
                         Age == "4Y" ~ "Adult",
                         Age == "8" ~ "Adult",
                         Age == "A4Y" ~ "Adult",
                         Age == "A5Y" ~ "Adult",
                         Age == "ASY" ~ "Adult",
                         Age == "ATY" ~ "Adult",
                         Age == "TY" ~ "Adult"),
         Sex = trimws(Sex))
table(capri.df$Age) 

##DELETE later
#Ran through script up to this point and nothing else before creating df. Add row number here and should be a clean match.
CapDfElly <- capri.df %>% mutate(rowID = row_number()) %>% select(c("rowID", "Species", "Project", "Banding.Date", "Band.Number","B.Lat", "B.Long", "W.Lat", "W.Long")) 
nrow(CapDfElly) #5927 rows
#write.csv(CapDfElly, file = "/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing_Exit_Seminar/Bergs_Rule/CapDfElly.csv", row.names = F) 
##

nrow(capri.df)
nrow(capriMut) #No migrants
nrow(capriBAnr) #No repeats



# Data entry errors / outliers for Wing & mass ----------------------------
capri.df %>% group_by(Species) %>% summarize(mnWing = mean(Wing.Chord, na.rm = T), 
                                             mn.Mass = mean(Mass, na.rm = T))
capri.df %>% arrange(Wing.Chord) %>% select(Project, Species, Age, Wing.Chord, Mass, Year, Band.Number) %>% slice_head(n = 10)
capri.df %>% arrange(desc(Wing.Chord)) %>% select(Project, Species, Age, Wing.Chord, Mass, Year, Band.Number) %>% slice_head(n = 10)
capri.df %>% arrange(desc(Mass)) %>% select(Project, Species, Age, Wing.Chord, Mass, Year, Band.Number) %>% slice_head(n = 10)
capri.df %>% arrange(Mass) %>% select(Project, Species, Age, Wing.Chord, Mass, Year, Band.Number) %>% slice_head(n = 10)
capri.df %>% ggplot(aes(Mass, Species)) + geom_boxplot()
capri.df %>% ggplot(aes(Wing.Chord, Species)) + geom_boxplot()

##Remove individuals with crazy numbers
nrow(capri.df)
capri.df <- capri.df %>% filter(is.na(Wing.Chord) | Wing.Chord < 250 & Wing.Chord > 30)

capri.df %>% ggplot(aes(Wing.Chord)) + geom_histogram() + facet_wrap(~Species)
capri.df %>% ggplot(aes(Mass)) + geom_histogram() + facet_wrap(~Species)


# Year effect -------------------------------------------------------------
ggplot(data = capri.df, aes(Year)) +  geom_histogram() + facet_wrap(~ Species)
ggsave('Histogram_Years_Species.png')

data.frame(euni.df %>% group_by( Year, round(B.Lat, -1)) %>% count(Year))

ggplot(data = euni.df, aes(Year, B.Lat)) + geom_hex()
ggplot(data = euni.df, aes(Year, B.Lat)) + geom_point()

#What should we do about Year / the month that birds were banded? Consider model w/ species RE? 
euni.df <- capri.df %>% filter(Species == "EUNI" & Age != "Unk" & Year > 1989)
nrow(euni.df)
#There is substantial evidence that body size tracks temperature changes through time fairly rapidly (i.e., within X years) (Weeks, 2020). CONI data was collected over 5 years (2015 - 2019) and EWPW over 6 years (2017-2022), thus we did not 
with(capri.df, table(Species, Year))
summary(lm(Mass ~ scale(Year) * B.Lat + Age, data = euni.df)) 
summary(lm(Wing.Chord ~ poly(Year,1) + B.Lat + Age, data = euni.df)) 
ggplot(data = euni.df, aes(x = Year, y = Mass)) + geom_smooth(method = "lm") + geom_point(alpha = .3)
ggplot(data = euni.df, aes(x = Year, y = Wing.Chord)) + geom_point(alpha = .3) + geom_smooth()

# MOVE: Months Envi covs --------------------------------------------------
##Determine relevant months for environmental covariates## 
capri.df %>% filter(Warr.md < as.POSIXct("2024-03-01")) %>% dplyr::select(Species, Bdep.md, Warr.md) %>% group_by(Species) %>% summarize(N = n(), MeanDep = mean(Bdep.md, na.rm = T), sdDep = sd(Bdep.md, na.rm = T), MeanArr = mean(Warr.md, na.rm = T), sdArr = sd(Warr.md, na.rm = T))
#Determine dates when migrants may be present
capri.df %>% dplyr::select(Species, Project, B.Lat, Band.md, Bdep.md) %>% mutate(BlatR = round(B.Lat, -1)) %>% group_by(Species, BlatR) %>% summarize(n = n(), MinBand = min(Band.md, na.rm = T), MeanBand = mean(Band.md, na.rm = T), MaxBand = max(Band.md, na.rm = T), minDep = min(Bdep.md, na.rm = T), MeanDep = mean(Bdep.md, na.rm = T), sdDep = sd(Bdep.md, na.rm = T)) %>% mutate(Cutoff = MeanDep - sdDep, TF = Cutoff > MaxBand)
#Not an issue for CONI or EWPW. For EUNI though.. Birds leave as early as August 1 at 60+ degrees North. Average date of departure at 60N is 8-17, so let's subtract 1 sd and get cutoff date of 08-06
##For spring migration for EUNI, both Evens (2017) and Norevik (2017) agree that birds depart wintering grounds in late February, but arrival date is "early May" for Evens and May 16 for Norevik (2017), 12 birds in each paper but only 9 made it for spring migration in Evens. Given May 16 is so close to cut-off anyways, probably makes sense to exclude month of May. 
#For EWPW, cite my paper for spring departure as March 20th and arrival as April 17th, English (2017) departure = March 21, arrival = May 1. These 2 refs are in agreement.
#For CONI, Elly says to use Nov - March for winter and May - August for breeding.


# Banding date effect -----------------------------------------------------
#Ensure banding dates are reasonable
capri.df %>% filter(Species == "EUNI" ) %>% dplyr::select(Species, Project, Band.md, W.Lat, Band.Number) %>% arrange(Band.md) %>% slice_head(n = 10)
capri.df %>% filter(Species == "EUNI") %>% dplyr::select(Species, Project, Band.md, W.Lat, Band.Number) %>% arrange(desc(Band.md)) %>% slice_head(n = 10)

#Check sample sizes 
capri.df %>% filter(Species == "EUNI") %>% 
  count(Project)
capri.df %>% filter(!is.na(W.Lat) & (Band.md > as.POSIXct("2023-04-30") & !is.na(W.Lat))) %>% 
  count(Project)

#Effect of band month statistically
summary(lm(Mass ~ Band.md + B.Lat + Age, data = euni.df)) #poly() gives error for band.md
summary(lm(Wing.Chord ~ Band.md + B.Lat + Age, data = euni.df)) 
ggplot(data = euni.df, aes(x = Band.md, y = Wing.Chord)) + geom_point(alpha = .3) + geom_smooth()
ggplot(data = euni.df, aes(x = Band.md, y = Mass)) + geom_point(alpha = .3) + geom_smooth() 
euni.df %>% filter(Project == "NASKASWE") %>% mutate(Fat = as.numeric(Fat)) %>% ggplot(aes(x = Band.md, y = Fat)) + geom_smooth() + geom_point() + ggtitle("Norevik EUNI data")
ggsave('Fat_date_Norevik.png')
table(euni.df$Project)

euni.dfRes <- euni.df %>% filter(Band.md > as.POSIXct("2023-04-30") & Band.md < as.POSIXct("2023-08-06"))
#Retest effect of band month
summary(lm(Mass ~ Band.md + B.Lat + Age, data = euni.dfRes)) #poly() gives error for band.md
summary(lm(Wing.Chord ~ Band.md + B.Lat + Age, data = euni.dfRes)) #Would need to remove Age == "Unk"
ggplot(data = euni.dfRes, aes(x = Band.md, y = Wing.Chord)) + geom_point(alpha = .3) + geom_smooth()
ggplot(data = euni.dfRes, aes(x = Band.md, y = Mass)) + geom_point(alpha = .3) + geom_smooth() 

#Some way of seeing sequential variation explained? 
anova(lm(Wing.Chord ~  Band.md + B.Lat + Age , data = euniRes))


#Remove potential migrants, leaving just residents (res). 5974 individuals to 5593. If you adjust the spring date to some time in May need to ensure that no individuals w/ winter data are excluded.
euniRes <- capri.df %>% filter(Species == "EUNI" & Band.md > as.POSIXct("2023-04-30") & Band.md < as.POSIXct("2023-08-06") & is.na(W.Lat))
euniFAC <- capri.df %>% filter(Species == "EUNI" & !is.na(W.Lat))
coni.ewpw <- capri.df %>% filter(Species != "EUNI" & Band.md > as.POSIXct("2023-04-30")) 
capri.df <- rbind(euniRes, euniFAC, coni.ewpw)
#Remove Greg's birds
table(capri.df$Project)
capri.df <- capri.df %>% filter(Project != "Greg_EDB")
nrow(capri.df)

##A number of random checks to better understand data and ensure it's good to go
#See if there are repeat individuals included across multiple data sets
multProj <- capri.df %>% count(Band.Number, Project) %>% 
  count(Band.Number, sort = T) %>% 
  filter(n>1)
#Identify where there could be additional overlap 
capri.df %>% count(Project, Country, Species) %>% 
  filter(Species == "EUNI") %>% 
  count(Country)
#filter(n>1)


#This is showing no overlap in Band numbers outside of Stewart and IAN, and Greg. Both are OK 
capri.df %>% filter(Band.Number %in% multProj$Band.Number) %>% select(Project)
#Delete all of this 
JHeunj <- JHeunj %>% replace_with_na_all(condition = ~.x %in% c("na"))
jhband <- paste0("A", JHeunj$Band.Number)
table(jhband %in% Various[Various$Country == "Finland",]$Band.Number)
TF <- jhband %in% Various[Various$Country == "Finland",]$Band.Number
jhband[TF]
capri.df %>% mutate(Band.Number = ifelse(Project == "FMNH/Ceur", paste0("A", Band.Number), Band.Number)) %>% filter(Band.Number == "A770945" & Year == 2018) %>% select (Project, Year, Band.Number, B.Lat)

#Identify and remove(?) individuals w/ multiple sexes recorded
multSex <- capri.df %>% filter (Sex != "U") %>% 
  count(Band.Number, Sex) %>%
  count(Band.Number) %>% 
  filter(n>1) %>% pull(Band.Number)
capri.df %>% filter(Band.Number %in% multSex) %>% select(Band.Number, Sex, Project, W.Lat) %>% filter(Sex != "U")  %>% arrange(Band.Number) %>% count(Band.Number, Sex) %>% count(Band.Number) %>% filter(n == 2) %>% pull(Band.Number)
capri.df %>% filter(Band.Number == "LA42163")


#This is showing no overlap outside of Stewart and IAN. 
capri.df %>% filter(Band.Number %in% multProj$Band.Number) %>% select(Project)
capri.df %>% filter(Country == "Finland") %>% with(table(Year, Project))
JHeunj <- JHeunj %>% replace_with_na_all(condition = ~.x %in% c("na"))
jhband <- paste0("A", JHeunj$Band.Number)
table(jhband %in% Various[Various$Country == "Finland",]$Band.Number)
TF <- jhband %in% Various[Various$Country == "Finland",]$Band.Number
jhband[TF]
capri.df %>% mutate(Band.Number = ifelse(Project == "FMNH/Ceur", paste0("A", Band.Number), Band.Number)) %>% filter(Band.Number == "A770945" & Year == 2018) %>% select (Project, Year, Band.Number, B.Lat)


#Breeding cords are 3+ digits, winter offenders are Naskaswe & our own data! 
decimals <- sapply(str_split(capri.df$W.Lat, "[.]"), function(x){x[2]})
data.frame(capri.df$Project, capri.df$Band.Number, decimals) %>% filter(!is.na(decimals)) %>% mutate(ncharW = nchar(decimals)) %>% arrange(ncharW)

# Band.Age,  capriBA,  capriFAC -------------------------------------------
#Create Band.Age variable, and use mutate to combine morphologies by Band.Age
capri.df$Band.Age <- paste0(capri.df$Age, "_", capri.df$Band.Number) 
capriMut <- capri.df %>% group_by(Band.Age) %>% 
                         mutate(Wing.comb = mean(Wing.Chord, na.rm = TRUE), 
                                Mass.comb = mean(Mass, na.rm = TRUE))
capriMut <- capriMut %>% filter(!is.na(Banding.Time)) %>%
                         mutate(Mass.combBT = mean(Mass, na.rm = T)) %>% 
                         filter(!is.na(Mass)) %>% 
                         mutate(BT.comb = mean(Banding.Time, na.rm = T)) %>% 
                         rbind(capriMut %>% filter(is.na(Mass) | is.na(Banding.Time))) %>% 
  tidyr::fill(BT.comb, Mass.combBT, .direction = "downup") #Capri Mutated. Tail.comb = mean(Tail.Length, na.rm = TRUE)

#Ensure this worked, rows should be the same, and see individual examples
nrow(capri.df) # 5472 rows
nrow(capriMut)



#This individual of Alicia's shows it worked for mass 
capriMut %>% filter(Band.Number == "135268737") %>% select(Band.Age, Banding.Time, Wing.Chord, Mass, Mass.comb, Wing.comb, BT.comb, Mass.combBT, W.Lat) %>% arrange(Mass.combBT)
#This individual shows it worked for banding time
capriMut %>% filter(Band.Number == "114281962") %>% select(Band.Age, Banding.Time, Mass, Mass.comb, BT.comb, Mass.combBT)
mean(chron(times = c("21:45:00" ,"20:10:00", "21:00:00", "20:20:00", "20:40:00")))
#Another example
capriMut %>% filter(Band.Number == "135268713") %>% select(Band.Age, Banding.Time, Mass, Mass.comb, BT.comb, Mass.combBT)


##CapriBA has only a single row for each individual & Age combo
capriBA <- data.frame(capriMut %>% group_by(Band.Age) %>% arrange(is.na(W.Lat), Year, .by_group = TRUE) %>% slice_head()) #BA = band age
nrow(capriBA) #4455 rows

#Ensure that functions picking the right row
capriBA %>% filter(Band.Number == "135268737") %>% select(Band.Age, Banding.Time, Wing.Chord, Mass, Mass.comb, Wing.comb, BT.comb, Mass.combBT, W.Lat) 

#Visualize morphological difference between captures
capriBA %>% select(Band.Number, Wing.Chord, Wing.comb) %>% group_by(Band.Number) %>% summarize(dif = Wing.Chord - Wing.comb) %>% arrange(dif)
capriMut %>% select(Band.Number, Band.Age, Mass, Mass.comb, Species) %>% group_by(Band.Age) %>% summarize(difAvg = Mass - Mass.comb, difMax = max(Mass) - min(Mass), across()) %>% arrange(desc(difMax)) #Difference of up to 28.1g in capture weights


#Visualize duplicate birds that have multiple years of winter data
DupBirds <- data.frame(capri.df %>% group_by(Band.Age, Year) %>% arrange(W.Lat, .by_group = TRUE) %>% slice_head()) 
DupBirdsFac <- filter(DupBirds, !is.na(W.Lat))
dups <- data.frame(DupBirdsFac  %>% filter(duplicated(Band.Number) | duplicated(Band.Number, fromLast = TRUE)) %>% dplyr::select(Species, Project, Year, Band.Number, TagID, Age, W.Lat, W.Long, Wing.Chord, Mass) %>% arrange(Project, Band.Number))
#The W.Lat difference between years is never very large (see column "Wlat.diff")
df.Wlat.diff <- dups %>% group_by(Band.Number) %>% mutate(Wlat.diff = max(W.Lat) - min(W.Lat)) %>% arrange(Wlat.diff) %>% slice_head()

#Calculate distances between wintering locations
dups.sf <- st_as_sf(dups,
         coords = c("W.Long", "W.Lat"),
         crs = 4326)
uniqID <- unique(dups.sf$Band.Number)
IDs <- dist.all <- dist.all2 <- dist <- vector("list", length = length(uniqID))
for(i in 1:length(uniqID)){
  IDs[[i]] <- subset(dups.sf, Band.Number == uniqID[i])
  dist.all[[i]] <- max(st_distance(IDs[[i]]))
  #diag(dist.all[[i]]) <- NA
  #dist[i] <- max(round(colMeans(dist.all[[i]], na.rm = TRUE),2)) #In meters
}

dists <- data.frame(Band.Number = uniqID, Dists = round(unlist(dist.all) / 1000, 2)) 
df.dists <- merge(dists, df.Wlat.diff, by = "Band.Number") %>% arrange(Species, Dists)
df.dists %>% group_by(Species) %>% summarize(mdn = median(Dists), mn = mean(Dists), sd = sd(Dists))

#Remove repeat individuals 
levels(capri.fac$Age) #Notice order, this should NOT be a factor (or need to change code)
capriBA %>% count(Band.Number, Age) %>% count(Band.Number) %>% filter(n > 1) #max is 3 b/c could be Band#_Adult, Unk, and Young
#Selecting adults when possible 
capriBAnr <- capriBA %>% group_by(Band.Number) %>% arrange(is.na(W.Lat), Age) %>% slice_head() #nr = no repeats 
nrow(capriBAnr)

#If you want to remove the 10 individuals that are repeated (w/ different ages, and selects adults) following code should work. Could use a RE to account for this so left in for now
capri.fac <- filter(capriBAnr, !is.na(W.Lat)) #FAC = full annual cycle
nrow(capri.fac) #Varied 186, 193, currently 189 unique birds..

# MassCorrection ----------------------------------------------------------

#1. Read in data----
df <- capriBA %>% summarize(date = as.Date(Banding.Date), 
                            lat = B.Lat, 
                            lon = B.Long,
                            DateTime = ymd_hms(paste(as.character(Banding.Date), BT.comb)),
                            BT.comb = BT.comb)

#2. Get time relative to sunset----
df$sunset <- getSunlightTimes(data = df)$sunset #Elly had tz="America/Edmonton"
df$tsss <- as.numeric(difftime(df$DateTime, df$sunset), units="hours") #tsss = time since sunset
df %>% arrange(sunset) %>% filter(!is.na(tsss)) %>% select(sunset, DateTime, tsss, lat, lon)

capriBA %>% filter(hms(Banding.Time) > hms("06:00:00") & hms(Banding.Time) < hms("18:00:00")) %>% select(Project, Band.Number, Banding.Time, tsss, W.Lat, Age) %>% arrange(Project, Banding.Time)
capriBA$Banding.Time[c(3000:4000)]

capriBA <- cbind(capriBA, df["tsss"])
#capriBA <- capriBA %>% select(-"tsss")

#I'll need to adjust this for new set of times I think?
use <- capriBA %>% 
  mutate(tsss = as.numeric(ifelse(tsss < -12, tsss+24, tsss))) %>% 
  #arrange(tsss) %>% select(sunset, DateTime, tsss, lat, lon) #visualize
  dplyr::filter(tsss > -5, 
                tsss < 8)
hist(df$tsss)
hist(use$tsss)

#3. Look at recaptures---- ###NEED to reorder this 
recap <- use %>% 
  group_by(Band.Number, Year) %>% 
  summarize(captures = n()) %>% 
  # ungroup() %>% 
  dplyr::filter(captures > 1) %>% 
  left_join(use)

#4. Visualize----
capriBA %>% 
  mutate(Banding.Time = ifelse(Banding.Time < .5, Banding.Time + 1, Banding.Time)) %>% 
  ggplot(aes(x = Banding.Time)) + geom_histogram(color = "black") #1.00 is midnight, every .25 should be a 6 hour period

ggplot(use) +
  geom_point(aes(x=tsss, y= Mass.combBT)) +
  geom_smooth(aes(x=tsss, y= Mass.combBT)) + 
  facet_wrap(~Species)

#Just CONI
  use %>% filter(Species == "CONI") %>% ggplot() +
    geom_point(aes(x=tsss, y= Mass.combBT)) +
    geom_smooth(aes(x=tsss, y= Mass.combBT)) 

ggplot(use) +
  geom_point(aes(x=tsss, y=Mass.combBT, colour=factor(round(B.Lat, -1)))) +
  geom_smooth(aes(x=tsss, y=Mass.combBT, colour=factor(round(B.Lat, -1)))) +
  facet_wrap(~Species)

ggplot(use) +
  geom_point(aes(x=jday, y=Mass.combBT, colour=factor(round(lat, -1)))) +
  geom_smooth(aes(x=jday, y=Mass.combBT, colour=factor(round(lat, -1))), method="lm") +
  facet_wrap(~Year)

ggplot(recap) +
  geom_point(aes(x=tsss, y=Mass.combBT, colour=Band.Number)) +
  geom_line(aes(x=tsss, y=Mass.combBT, colour=Band.Number)) +
  geom_smooth(aes(x=tsss, y=Mass.combBT)) #Overall line isn't really informative ?

#5. Ok try with RE for individual----
#What about testing with additional polynomials? 
mod1 <- lmer(Mass.combBT ~ tsss + (1 | Site.name/Band.Number), data=use)
mod2 <- lmer(Mass.combBT ~ poly(tsss, 2) + (1 | Site.name/Band.Number), data=use)
AIC(mod1, mod2) #polynomial better model

#6. Try predicting---- 
#https://stats.stackexchange.com/questions/191648/using-re-form-in-predict-mermod-for-a-lmer-model
newdat <- data.frame(expand.grid(tsss = seq(round(min(use$tsss), 1), round(max(use$tsss), 1), 0.1),
                                 #                                 jday = seq(min(use$jday), max(use$jday), 1),
                                 Population = unique(use$Population)))
#Next plot helps show what this is doing here
pred <- data.frame(pred.re = predict(mod2, newdat, re.form = ~(1|Population)), #Taking population into account
                   pred = predict(mod2, newdat, re.form = ~0)) %>% #Overall effect (irrespective of RE)
  cbind(newdat)


ggplot() +
  geom_point(aes(x=tsss, y=Mass.combBT, colour=Population), data=use) +
  geom_line(aes(x=tsss, y=pred.re, colour=Population), data=pred) +
  geom_line(aes(x=tsss, y=pred), data=pred, colour="black", lwd=2)

#7. Predict to new data----
out <- use %>% 
  mutate(CorrectedMass.combBT = predict(mod2, use))

#8. Visualize----
ggplot(out) +
  geom_point(aes(x=Mass.combBT, y=CorrectedMass.combBT, colour=tsss)) +
  scale_colour_viridis_c()

cor(out$Mass.combBT, out$CorrectedMass.combBT)
write.csv(out, "/Users/ellyknight/Documents/UoA/Projects/Projects/Morphometrics/DataSheet_CONI_Breeding_Mass.combBTCorrection.csv", row.names = FALSE)






# Nuisance variables / model structure  -----------------------------------
njana <- capriBAnr ##For now..
njanaSex <- njana[njana$Sex != "U",] #Just remove these 7 individuals
nrow(njana)
nrow(EnviCovs)
#njana <- merge(subset(njdf, select = -c(B.dep, W.arr, Band.Age, Species)), EnviCovs, by = "ID") #NJ analysis
njana.w <- njanaSex[!is.na(njanaSex$Wing.Chord),] #Wing
njana.m <- njanaSex[!is.na(njanaSex$Mass),] #Mass
ewpw.w <- njana.w[njana.w$Species == "EWPW",] 
ewpw.m <- njana.m[njana.m$Species == "EWPW",] 
coni.w <- njana.w[njana.w$Species == "CONI",] 
coni.m <- njana.m[njana.m$Species == "CONI",] 
euni.w <- njana.w[njana.w$Species == "EUNI",]
euni.m <- njana.m[njana.m$Species == "EUNI",]
ewpwAge <- ewpw.w[ewpw.w$Age != "Unk",]
ewpwAgeM <- ewpw.m[ewpw.m$Age != "Unk",]
euniAge <- euni.w[euni.w$Age != "Unk",]
euniAgeM <- euni.m[euni.m$Age != "Unk",]

#Data exploration. Consider transforming response var or using glm? 
#EWPW
library(ggpubr)
hist(ewpw.w$Wing.Chord)
hist(ewpw.m$Mass) 
#A significant p value here implies that the variable does differ significantly from normal
shapiro.test(ewpw.w$Wing.Chord)
shapiro.test(ewpw.m$Mass)
#CONI
hist(coni.w$Wing.Chord) 
ggqqplot(coni.w$Wing.Chord)
shapiro.test(coni.w$Wing.Chord)
hist(coni.m$Mass) 
ggqqplot(coni.m$Mass)
shapiro.test(coni.m$Mass)
#EUNI
hist(euni.w$Wing.Chord) 
ggqqplot(euni.w$Wing.Chord)
shapiro.test(euni.w$Wing.Chord)
hist(euni.m$Mass) 
ggqqplot(euni.m$Mass)
shapiro.test(euni.m$Mass)

##Need to include B.LAT & OTHER SIGNIFICANT VARIABLES HERE? anova() command? 
#Are the nuisance variable likely to influence the outcome? For CONI sex/age, probably not a big deal given that Elly said the females are spread out from Alberta - Oregon which is a large latitudinal gradient.
njanaSex %>% group_by(Species, Sex) %>% summarize(N = n())
njana %>% group_by(Species, Age) %>% summarize(N = n())
lapply(njdf.list, function(x){table(x$Age)})
table(coni.w$Sex)
table(coni.w$Sex, coni.w$Site.name)

#CONI - Sex accounts for 6 - 7% of size
summary(lm(Wing.Chord ~ Sex, data = coni.w))
summary(lm(Wing.Chord ~ Age, data = coni.w)) 
summary(lm(Mass ~ Sex, data = coni.m))
summary(lm(Mass ~ Age, data = coni.m)) 

summary(lm(Mass ~ Banding.Time, data = coni.m)) 

#EWPW
#EWPW age is probably problematic given that it is significant 
modAgeW <- lm(Wing.Chord ~ Age, data =  ewpwAge)
summary(modAgeW) #14% of variation
summary(lm(Wing.Chord ~ Sex, data =  ewpw.w)) #9.5% of variation
modAgeM <- lm(Mass ~ Age, data =  ewpwAgeM)
summary(modAgeM) #5% of variation
summary(lm(Mass ~ Sex, data =  ewpw.m))

summary(lm(Mass ~ Year, data =  ewpw.m))
unique(ewpw.m$Year)

summary(lm(Mass ~ Banding.Time, data = ewpw.m)) 

##The heterogeneity of variance of residuals is not largely different by ages, so this is likely not an issue for model assumptions
E2W <- resid(modAgeW)
E2M <- resid(modAgeM)
boxplot(E2M ~ Age, data =  ewpwAgeM, main = "Heterogeneity EWPW Age", ylab = "Residuals")
plot(modAgeW)

##EUNI
summary(lm(Wing.Chord ~ B.Lat + Sex + Age, data = euni.w))
summary(lm(Wing.Chord ~ B.Lat + Sex + Age, data = euniAge)) #About 3% of variation
summary(lm(Mass ~ Sex, data = euni.m)) #16% of variation. MALES smaller? 
summary(lm(Mass ~ B.Lat + Sex + Age, data = euniAgeM))  #<1% of variation
summary(lm(Mass ~ B.Lat + Sex + Age, data = euniAgeM)) 

summary(lm(Mass ~ Banding.Time, data = euni.m)) 


##MEETING ELLY - ORGANIZE QUESTIONS, CHANGE ELLY HOUR VALUES, AND REMEMBER WHAT IDEA IS BEHIND THE MASS CORRECTION

##Migration distance##
capri.fac <- transform(capri.fac, Str8line = distHaversine(cbind(capri.fac$B.Long, capri.fac$B.Lat), cbind(capri.fac$W.Long, capri.fac$W.Lat)) / 1000)
TF <- capri.fac$Str8line < capri.fac$Mig.dist #13,14,19, 26
capri.fac[ !TF ,c("Str8line", "Mig.dist","Project", "Band.Number")] %>% filter(Project == "NASKASWE") #Alicia used a different system to calculate migratory distance, so the numbers are slightly different. When she recalculated differences were <20km, so this is not very important on the scale of 4-5k km. 

##Migration distance correlations by species
capri.fac %>% group_by(Species) %>% summarize(cor = cor(Str8line, Mig.dist, use = "complete.obs"))

capri.fac %>% filter(Species == "EUNI") %>% 
  ggplot(aes(x = Str8line, y = Mig.dist)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = Project)) +
  geom_abline(slope= 1, linetype = "dashed", color="Red")
  #facet_wrap(~Species)

#Migration distance models. Does the variation in temporal schedule influence migration distance? Yes! Therefore, it is better to use the straight-line distance  

capri.fac %>% group_by(Species) %>% 
  summarize(mean = mean(Temp.res.mig,na.rm = T),
            sd = sd(Temp.res.mig,na.rm = T),
            max = max(Temp.res.mig,na.rm = T),
            min = min(Temp.res.mig,na.rm = T))
summary(capri.fac$Temp.res.mig) #Ranges from 7 points a day to 1 point every 10 days
summary(lmer(Mig.dist ~ Temp.res.mig + (1| Species), data = capri.fac))

##Overall plot
ggplot(data= capri.fac, aes(x = Str8line, y = Mig.dist)) + 
  geom_smooth(aes(color = Species), method = "lm", se = F, fullrange = T) + 
  geom_point(aes(color = Species), size = 3, position = "jitter", alpha=.3) + 
  geom_abline(slope= 1, linetype = "dashed", color="Black") #+ xlab("Departure Date") + ylab("Migration\nRate (km / day)") 


#Move down to plotting section? Or create a Misc section
#Evidence for leapfrog migration in EWPW and EUNI, but not CONI. Has this been reported previously in EUNI? 
ggplot(data= capri.fac, aes(x = B.Lat, y = W.Lat)) + geom_smooth(method = "lm", se = TRUE, fullrange = F, aes(color = Species)) + geom_point(aes(color = Species), size =3, position = "jitter", alpha=.65) #+ xlab("Departure Date") + ylab("Migration\nRate (km / day)") 

#How would you test this statistically? 
summary(lmer(W.Lat ~ B.Lat + (1 | Species), data = capri.fac))
summary(lmer(W.Long ~ B.Long + (1 | Species), data = capri.fac))

#save.image(paste0(bs, "Desktop/Grad_School/R_Files/MS/BergAnalysis9.21.22.Rdata"))



# rgee and environmental data ---------------------------------------------

install.packages("googleCloudStorageR")
library(rgee)
library(reticulate)
ee_install(py_env = "rgee")
#ee_clean_pyenv()
ee_check()
ee_check_python()
ee_check_credentials()
ee_check_python_packages()
#install_miniconda()

ee_Initialize(user = "aaron.skinner@fulbrightmail.org", drive = TRUE, gcs = TRUE)



# Link envi covs w/ individuals -------------------------------------------

#Elly pulled the individuals w/ winter locs from the bigger file I sent her (near 1000 rows)
#load("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Data_share/Data/EK_Envi_Vars/Correlations/EC_workspace.Rdata")
#This njdf file needs to be the complete data frame w/ all variables (e.g. capri.fac) and including the ID column. 

njdf <- read.csv("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Data_share/Data/EK_Envi_Vars/capri.fac.id.csv") #nightjar df
njdf <- capri.fac
njdf$ID <- 1:186


elev <- read.csv("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Data_share/Data/EK_Envi_Vars/DEM.csv")
elev <- elev %>% dplyr::select(ID, season, elevation) %>% group_by(ID) %>% pivot_wider(names_from = season, names_glue = "{season}Elev", values_from = elevation) #Notice names glue so season starts the name

#WorldClim
wc <- read.csv("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Data_share/Data/EK_Envi_Vars/Wordclim.csv")
head(wc)
#MDR = mean diurnal range, BIO2. Many of these are similar to the 19 worldclim vars (good comparison to make in manuscript, as people are familiar with these vars). Notice particularly that BrTcv is not equivalent to using the cv function (i.e compCV = cv(tavg, na.rm = T) produces slightly different results). 
#Should MDR go into seasonality or temp regulation... Elly & I think TR, b/c seasonality is ACROSS the entirety of the season, MDR is within each month.
br.wc <- wc %>% group_by(ID) %>% filter(season == "Breed", covmonth >= 5 & covmonth <= 9) %>% summarize(BrPrec = mean(prec), BrCVprec = cv(prec), BrTavg = mean(tavg), BrTcv = ((sd(tavg/10))/(mean(tavg/10) + 273.15))*100, BrTmax = mean(tmax), BrTMAX = max(tmax), BrTmin = mean(tmin), BrTMIN = min(tmin), BrMDR = sum(tmax - tmin) / 5)
wi.wc <- wc %>% group_by(ID) %>% filter(season == "Winter", covmonth >= 10 | covmonth <= 4) %>% summarize(WiPrec = mean(prec, na.rm = T), WiCVprec = cv(prec, na.rm = T), WiTavg = mean(tavg, na.rm = T), WiTcv = ((sd(tavg/10, na.rm = T))/(mean(tavg/10, na.rm = T) + 273.15))*100, WiTmax = mean(tmax), WiTMAX = max(tmax), WiTmin = mean(tmin), WiTMIN = min(tmin), WiMDR = sum(tmax - tmin) / 5)
wc[wc$season == "Winter",]


#EVI
evi <- read.csv("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Data_share/Data/EK_Envi_Vars/EVI.csv")
br.evi <- evi %>% group_by(ID) %>% filter(season == "Breed", covmonth >= 5 & covmonth <= 9) %>% summarize(BrEVI = mean(EVI, na.rm = T), BrCVevi = cv(EVI, na.rm = T))
wi.evi <- evi %>% group_by(ID) %>% filter(season == "Winter", covmonth >= 10 | covmonth <= 4) %>% summarize(WiEVI = mean(EVI, na.rm = T), WiCVevi = cv(EVI, na.rm = T))
EnviCovs[EnviCovs$ID == 92,]
njdf[njdf$TagID == 2125,]
wc[wc$ID == 92,]
njana[njana$TagID == 2125,]

#Terraclim
tc <- read.csv("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Data_share/Data/EK_Envi_Vars/Terraclim.csv")
br.tc <- tc %>% group_by(ID) %>% filter(season == "Breed", covmonth >= 5 & covmonth <= 9) %>% summarize(BrAet = mean(aet, na.rm = T), BrVap = mean(vap, na.rm = T), BrSrad = mean(srad, na.rm = T))
wi.tc <- tc %>% group_by(ID) %>% filter(season == "Winter", covmonth >= 10 | covmonth <= 4) %>% summarize(WiAet = mean(aet, na.rm = T), WiVap = mean(vap, na.rm = T), WiSrad = mean(srad, na.rm = T))


dfs <- list(br.wc,  wi.wc, elev, br.evi, wi.evi, br.tc, wi.tc)
EnviCovs <- dfs %>% purrr::reduce(full_join, by = "ID")
EnviCovs <- merge(EnviCovs, njdf[,c("ID", "Species")], by = "ID")
nrow(EnviCovs) #Should be 186


#Correlations EWPW
##Just taking the covariates that actually made it into the final models..
#Breeding predictor vars
ewpwBrPred <- ewpw.m[,c("B.Lat", "B.Long", "BrTavg", "BrPrec" , "BrTcv", "BrCVevi", "BrEVI", "BrSrad", "BrMDR" , "BreedElev", "str8line")]
GGally::ggcorr(ewpwBrPred, label = T, label_size = 3, hjust = 0.75, size = 3) + ggtitle("Whip-poor-will breeding predictors")
#Winter predictor vars
ewpwWiPred <- ewpw.m[, c("WiPrec" ,  "WiTcv", "WiCVevi", "WiEVI", "WiTavg", "WiMDR", "WiSrad", "W.Long", "W.Lat", "WinterElev", "str8line")]
GGally::ggcorr(ewpwWiPred, label = T, label_size = 3, hjust = 0.75, size = 3) + ggtitle("Whip-poor-will winter predictors")

##And now w/ all Vars for EWPW
ECewpw <- EnviCovs[EnviCovs$Species == "EWPW",] #EC = EnviCovs
GGally::ggcorr(subset(ECewpw,select = -c(ID, Species)))
ggpairs(ECewpw)
CMewpw <- cor(subset(ECewpw,select = -c(ID, Species)) , use = "complete.obs") #CM = cormat
write.csv(round(apply(CMewpw, 2, function(x){ifelse(x < .5 & x > -.5, NA, x)}),2), file = "/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Data_share/Data/EK_Envi_Vars/Correlations/EC_CorrMat_ewpw.csv")

#Correlations CONI
##Just taking the covariates that actually made it into the final models..
#Breeding predictor vars
coniBrPred <- coni.m[,c("B.Lat", "B.Long", "BrTavg", "BrPrec" , "BrTcv", "BrCVevi", "BrEVI", "BrSrad", "BrMDR" , "BreedElev", "str8line")]
GGally::ggcorr(coniBrPred, label = T, label_size = 3, hjust = 0.75, size = 3) + ggtitle("Common nighthawk breeding predictors")
#Winter predictor vars
coniWiPred <- coni.m[, c("WiPrec" ,  "WiTcv", "WiCVevi", "WiEVI", "WiTavg", "WiMDR", "WiSrad", "W.Long", "W.Lat", "WinterElev", "str8line")]
GGally::ggcorr(coniWiPred, label = T, label_size = 3, hjust = 0.75, size = 3) + ggtitle("Common nighthawk winter predictors")

ggsave("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Results/Figures/Correlation Plots/ewpwBr.png", bg = "white")



ECconi <- EnviCovs[EnviCovs$Species == "CONI",]
GGally::ggcorr(subset(ECconi,select = -c(ID, Species)))
ggpairs(ECconi)
CMewpw <- cor(subset(ECconi,select = -c(ID, Species)) , use = "complete.obs") #CM = cormat
write.csv(round(apply(CMewpw, 2, function(x){ifelse(x < .5 & x > -.5, NA, x)}),2), file = "/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Data_share/Data/EK_Envi_Vars/Correlations/EC_CorrMat_coni.csv")

##EUNI 
#Breeding predictor vars
euniBrPred <- euni.m[,c("B.Lat", "B.Long", "BrTavg", "BrPrec" , "BrTcv", "BrCVevi", "BrEVI", "BrSrad", "BrMDR" , "BreedElev", "str8line")]
GGally::ggcorr(euniBrPred, label = T, label_size = 3, hjust = 0.75, size = 3) + ggtitle("European Nightjar breeding predictors")

#save.image("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Data_share/Data/EK_Envi_Vars/Correlations/EC_workspace.Rdata")


# AIC Model selection -----------------------------------------------------

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


##DELETEE
###SKIP ALL OF THIS , DELETE ONCE YOU'RE SURE NEW FILE IS GOOD.
Stew <- ASewpw %>% filter(Project == "Stewart" & !is.na(W.Lat))
table(!is.na(ASewpw[,c("W.Long")])) #99 
ASewpw2 <- subset(ASewpw, select = -c(W.Lat, W.Long))
ASewpw2$BandYr <- paste0(ASewpw2$Band.Number,"_", ASewpw2$Year)
spdf.key@data$DepYr <- as.numeric(as.character(spdf.key@data$DepYr)) - 1
spdf.key@data$BandYr <- paste0(spdf.key@data$Band.Number,"_", spdf.key@data$DepYr)
ASewpw2 <- merge(ASewpw2, spdf.key@data[,c("BandYr","W.Lat", "W.Long")], by = "BandYr", all.x = T)
ASewpw2 <- ASewpw2 %>% dplyr::select(c(cols, "Comments")) %>% arrange(is.na(W.Lat), Year)
write.csv(ASewpw2, file = "/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing_Exit_Seminar/Bergs_Rule/Data_share/Data/Tonra_Ward.csv")
unique(ASewpw[!is.na(ASewpw$W.Lat),]$Band.Number)
table(!is.na(ASewpw$W.Lat))


##SKIP -- All unnecessary
#Format MidWest EWPW data base
caps1720 <- read.csv("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Data_Whips/Data_Sheets/EWPW_caps_all_years.csv", header = TRUE)
names(caps1720)
caps1720 <- replace_with_na(data = caps1720, replace = list(Wing.Chord = 9999, Mass = 9999, Tail.Length = 9999))

caps1720[,c("")]

caps1720$Country <- rep("USA", nrow(caps1720))
caps1720$WingFlat <- rep("N", nrow(caps1720))
caps1720$uniq.ID <- paste0(caps1720$Study, "-", caps1720$Geo.Trans)
caps1720$Mig.n <- rep(NA, nrow(caps1720))
caps1720<- caps1720[, c("Project", "Year", "Banding.Date", "Band.Size", "Banding.Time", "Species", "Location", "Country", "Band.Number", "Geo.Trans", "Age", "Sex", "CP","BP","Fat", "Wing.Chord", "WingFlat", "Tail.Length", "Tarsus", "Mass", "B.Lat", "B.Long", "Mig.n", "Comment", "Study", "uniq.ID")]
#Load spdf.key
load("/Users/AaronSkinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/R_Files/MS/spdf.key.Rdata")
spdf.key@data$Temp.res.mig <- ifelse(spdf.key@data$DepYr == 17, 4 , 5)
#Merge data frames to bring in winter data for 52 relevant birds
ASewpw <- merge(x = caps1720, y = spdf.key@data[,c("uniq.ID", "W.Lat", "W.Long", "Departure", "Arrival", "mig.dist", "Temp.res.mig")], by = "uniq.ID", all.x = T)
test <- ASewpw[!is.na(ASewpw$mig.dist),]
unique(test$uniq.ID)
#Reorganize columns into appropriate order
ASewpw <- ASewpw[, c("Project", "Year", "Banding.Date", "Band.Size", "Banding.Time", "Species", "Location", "Country", "Band.Number", "Geo.Trans", "Age", "Sex", "CP","BP","Fat", "Wing.Chord", "WingFlat", "Tail.Length", "Tarsus", "Mass", "B.Lat", "B.Long", "W.Lat", "W.Long", "Departure","Arrival", "mig.dist", "Mig.n", "Temp.res.mig", "Comment")]
ASewpw$Project <- "Tonra"
AKewpw$Project <- "Can EWPW"
names(ASewpw) <- cols
ASewpw[,"Banding.Date"]
ASewpw[,c("B.dep", "W.arr")] <- lapply(format(ASewpw[,c("B.dep", "W.arr")], "%m/%d/%y"), as.character)
head(ASewpw)
#write.csv(ASewpw, file = "EWPW_Berg.csv")




# EWPW --------------------------------------------------------------------
# >>Geography ---------------------------------------------------------------
##Breeding Wing
cormatGeoB <- round(cor(ewpw.w[, c("B.Lat", "B.Long", "elev.Breed")], use = "complete.obs", method = "spearman"),2) #GeoBgraphy
#"elev.Winter", "W.Lat"
apply(cormatGeoB, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.GeoB <- apply(cormatGeoB, 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.GeoB[upper.tri(TF.GeoB, diag = T)] <- NA

globGeoB <- lm(Wing.Chord ~ Age + B.Lat + B.Long + elev.Breed, na.action = "na.fail", data =  ewpwAge) #set to na.fail 
getAllTerms(globGeoB) # terms in matrix for subset and global models must be in this order
drgGeoB <- dredge(globGeoB, subset = TF.GeoB, m.lim = c(0,3), fixed = "Age")
nrow(drgGeoB)
cand.modsGeoB <- get.models(drgGeoB, subset = T)

modNamesGeoB <- vector("character", length(cand.modsGeoB))
vifGeoB <- rep(NA, length(cand.modsGeoB))
modFormGeoB <- modsGeoB <- vector("list", length(cand.modsGeoB))
for(i in 1:length(cand.modsGeoB)){
  modFormGeoB[[i]] <- formula(cand.modsGeoB[[i]])
  print(i)
  minGeoB <- simpleCap(str_replace_all(as.character(modFormGeoB[[i]])[3], "GeoB", "")) #minus "GeoB"
  min1 <- unlist(strsplit(as.character(minGeoB), split = ' + 1 ', fixed = T)) #minus "+1"
  modNamesGeoB[i] <- paste(as.character(modFormGeoB[[i]])[2], as.character(modFormGeoB[[i]])[1], min1[1], min1[2])
  modsGeoB[[i]] <- lm(formula = modFormGeoB[[i]], data =  ewpwAge)
  if(length(coef(modsGeoB[[i]])) > 2){
    vifGeoB[[i]] <- max(car::vif(modsGeoB[[i]]))
  } 
}
max(vifGeoB, na.rm = T)
aic.tab.GeoB <- aictab(cand.set = modsGeoB, modnames = modNamesGeoB, sort = TRUE)
which(duplicated(aic.tab.GeoB$AICc))

##Winter Wing
cormatGeoW <- round(cor(ewpw.w[, c("W.Lat", "W.Long", "elev.Winter")], use = "complete.obs", method = "spearman"),2) #geography
#"elev.Winter", "W.Lat"
apply(cormatGeoW, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.GeoW <- apply(cormatGeoW, 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.GeoW[upper.tri(TF.GeoW, diag = T)] <- NA

globGeoW <- lm(Wing.Chord ~ Age + W.Lat + W.Long + elev.Winter, na.action = "na.fail", data =  ewpwAge) #set to na.fail 
getAllTerms(globGeoW) # terms in matrix for subset and global models must be in this order
drgGeoW <- dredge(globGeoW, subset = TF.GeoW, m.lim = c(0,3), fixed = "Age")
nrow(drgGeoW)
cand.modsGeoW <- get.models(drgGeoW, subset = T)

modNamesGeoW <- vector("character", length(cand.modsGeoW))
vifGeoW <- rep(NA, length(cand.modsGeoW))
modFormGeoW <- modsGeoW <- vector("list", length(cand.modsGeoW))
for(i in 1:length(cand.modsGeoW)){
  modFormGeoW[[i]] <- formula(cand.modsGeoW[[i]])
  print(i)
  minGeoW <- simpleCap(str_replace_all(as.character(modFormGeoW[[i]])[3], "GeoW", "")) #minus "GeoW"
  min1 <- unlist(strsplit(as.character(minGeoW), split = ' + 1 ', fixed = T)) #minus "+1"
  modNamesGeoW[i] <- paste(as.character(modFormGeoW[[i]])[2], as.character(modFormGeoW[[i]])[1], min1[1], min1[2])
  modsGeoW[[i]] <- lm(formula = modFormGeoW[[i]], data =  ewpwAge)
  if(length(coef(modsGeoW[[i]])) > 2){
    vifGeoW[[i]] <- max(car::vif(modsGeoW[[i]]))
  } 
}
max(vifGeoW, na.rm = T)
aic.tab.GeoW <- aictab(cand.set = modsGeoW, modnames = modNamesGeoW, sort = TRUE)
which(duplicated(aic.tab.GeoW$AICc))

##Breeding Mass
cormatGeoBM <- round(cor(ewpw.m[, c("B.Lat", "B.Long", "elev.Breed")], use = "complete.obs", method = "spearman"),2) #GeoBMgraphy
#"elev.Winter", "W.Lat"
apply(cormatGeoBM, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.GeoBM <- apply(cormatGeoBM, 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.GeoBM[upper.tri(TF.GeoBM, diag = T)] <- NA

globGeoBM <- lm(Mass ~ Age + B.Lat + B.Long + elev.Breed, na.action = "na.fail", data =  ewpwAgeM) #set to na.fail 
getAllTerms(globGeoBM) # terms in matrix for subset and global models must be in this order
drgGeoBM <- dredge(globGeoBM, subset = TF.GeoBM, m.lim = c(0,3), fixed = "Age")
nrow(drgGeoBM)
cand.modsGeoBM <- get.models(drgGeoBM, subset = T)

modNamesGeoBM <- vector("character", length(cand.modsGeoBM))
vifGeoBM <- rep(NA, length(cand.modsGeoBM))
modFormGeoBM <- modsGeoBM <- vector("list", length(cand.modsGeoBM))
for(i in 1:length(cand.modsGeoBM)){
  modFormGeoBM[[i]] <- formula(cand.modsGeoBM[[i]])
  print(i)
  minGeoBM <- simpleCap(str_replace_all(as.character(modFormGeoBM[[i]])[3], "GeoBM", "")) #minus "GeoBM"
  min1 <- unlist(strsplit(as.character(minGeoBM), split = ' + 1 ', fixed = T)) #minus "+1"
  modNamesGeoBM[i] <- paste(as.character(modFormGeoBM[[i]])[2], as.character(modFormGeoBM[[i]])[1], min1[1], min1[2])
  modsGeoBM[[i]] <- lm(formula = modFormGeoBM[[i]], data =  ewpwAgeM)
  if(length(coef(modsGeoBM[[i]])) > 2){
    vifGeoBM[[i]] <- max(car::vif(modsGeoBM[[i]]))
  } 
}
max(vifGeoBM, na.rm = T)
aic.tab.GeoBM <- aictab(cand.set = modsGeoBM, modnames = modNamesGeoBM, sort = TRUE)

##Winter Mass
cormatGeoWM <- round(cor(ewpw.m[, c("W.Lat", "W.Long", "elev.Winter")], use = "complete.obs", method = "spearman"),2) #GeoWMgraphy
#"elev.Winter", "W.Lat"
apply(cormatGeoWM, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.GeoWM <- apply(cormatGeoWM, 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.GeoWM[upper.tri(TF.GeoWM, diag = T)] <- NA

globGeoWM <- lm(Mass ~ Age + W.Lat + W.Long + elev.Winter, na.action = "na.fail", data =  ewpwAgeM) #set to na.fail 
getAllTerms(globGeoWM) # terms in matrix for subset and global models must be in this order
drgGeoWM <- dredge(globGeoWM, subset = TF.GeoWM, m.lim = c(0,3), fixed = "Age")
nrow(drgGeoWM)
cand.modsGeoWM <- get.models(drgGeoWM, subset = T)

modNamesGeoWM <- vector("character", length(cand.modsGeoWM))
vifGeoWM <- rep(NA, length(cand.modsGeoWM))
modFormGeoWM <- modsGeoWM <- vector("list", length(cand.modsGeoWM))
for(i in 1:length(cand.modsGeoWM)){
  modFormGeoWM[[i]] <- formula(cand.modsGeoWM[[i]])
  print(i)
  minGeoWM <- simpleCap(str_replace_all(as.character(modFormGeoWM[[i]])[3], "GeoWM", "")) #minus "GeoWM"
  min1 <- unlist(strsplit(as.character(minGeoWM), split = ' + 1 ', fixed = T)) #minus "+1"
  modNamesGeoWM[i] <- paste(as.character(modFormGeoWM[[i]])[2], as.character(modFormGeoWM[[i]])[1], min1[1], min1[2])
  modsGeoWM[[i]] <- lm(formula = modFormGeoWM[[i]], data =  ewpwAgeM)
  if(length(coef(modsGeoWM[[i]])) > 2){
    vifGeoWM[[i]] <- max(car::vif(modsGeoWM[[i]]))
  } 
}
max(vifGeoWM, na.rm = T)
aic.tab.GeoWM <- aictab(cand.set = modsGeoWM, modnames = modNamesGeoWM, sort = TRUE)


# >>Temp regulation (TR) -------------------------------------------------------
###Breeding Wing
cormatTRb <- round(cor( ewpwAge[, c("BrMDR", "BrSrad", "BrTavg")], use = "complete.obs", method = "spearman"),2)
apply(cormatTRb, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.TRb <- apply(cormatTRb, 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.TRb[upper.tri(TF.TRb, diag = T)] <- NA

globTRb <- lm(Wing.Chord ~ Age + BrMDR + BrSrad + BrTavg, na.action = "na.fail", data =  ewpwAge) #set to na.fail 
drgTRb <- dredge(globTRb, subset = TF.TRb, m.lim = c(0,3), fixed = "Age")
nrow(drgTRb)
cand.modsTRb <- get.models(drgTRb, subset = T)

modNamesTRb <- vector("character", length(cand.modsTRb))
vifTRb <- rep(NA, length(cand.modsTRb))
modFormTRb <- modsTRb <- vector("list", length(cand.modsTRb))
for(i in 1:length(cand.modsTRb)){
  modFormTRb[[i]] <- formula(cand.modsTRb[[i]])
  print(i)
  minTRb <- simpleCap(str_replace_all(as.character(modFormTRb[[i]])[3], "TRb", "")) #minus "TRb"
  min1 <- unlist(strsplit(as.character(minTRb), split = ' + 1 ', fixed = T)) #minus "+1"
  modNamesTRb[i] <- paste(as.character(modFormTRb[[i]])[2], as.character(modFormTRb[[i]])[1], min1[1], min1[2])
  modsTRb[[i]] <- lm(formula = modFormTRb[[i]], data =  ewpwAge)
  if(length(coef(modsTRb[[i]])) > 2){
    vifTRb[[i]] <- max(car::vif(modsTRb[[i]]))
  } 
}
max(vifTRb, na.rm = T)
aic.tab.TRb <- aictab(cand.set = modsTRb, modnames = modNamesTRb, sort = TRUE)

##Winter wing
cormatTRw <- round(cor( ewpwAge[, c("WiMDR","WiSrad", "WiTavg")], use = "complete.obs", method = "spearman"),2)
apply(cormatTRw, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.TRw <- apply(cormatTRw, 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.TRw[upper.tri(TF.TRw, diag = T)] <- NA

globTRw <- lm(Wing.Chord ~ Age + WiMDR + WiSrad + WiTavg, na.action = "na.fail", data =  ewpwAge) #set to na.fail 
drgTRw <- dredge(globTRw, subset = TF.TRw, m.lim = c(0,3), fixed = "Age")
nrow(drgTRw)
cand.modsTRw <- get.models(drgTRw, subset = T)

modNamesTRw <- vector("character", length(cand.modsTRw))
vifTRw <- rep(NA, length(cand.modsTRw))
modFormTRw <- modsTRw <- vector("list", length(cand.modsTRw))
for(i in 1:length(cand.modsTRw)){
  modFormTRw[[i]] <- formula(cand.modsTRw[[i]])
  print(i)
  minTRw <- simpleCap(str_replace_all(as.character(modFormTRw[[i]])[3], "TR", "")) #minus "TR"
  min1 <- unlist(strsplit(as.character(minTRw), split = ' + 1 ', fixed = T)) #minus "+1"
  modNamesTRw[i] <- paste(as.character(modFormTRw[[i]])[2], as.character(modFormTRw[[i]])[1], min1[1], min1[2])
  modsTRw[[i]] <- lm(formula = modFormTRw[[i]], data =  ewpwAge)
  if(length(coef(modsTRw[[i]])) > 2){
    vifTRw[[i]] <- max(car::vif(modsTRw[[i]]))
  } 
}
max(vifTRw, na.rm = T)
aic.tab.TRw <- aictab(cand.set = modsTRw, modnames = modNamesTRw, sort = TRUE)

###Breeding Mass
cormatTRbm <- round(cor(ewpwAgeM[, c("BrMDR", "BrSrad", "BrTavg")], use = "complete.obs", method = "spearman"),2)
apply(cormatTRbm, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.TRbm <- apply(cormatTRbm, 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.TRbm[upper.tri(TF.TRbm, diag = T)] <- NA

globTRbm <- lm(Mass ~ Age + BrMDR + BrSrad + BrTavg, na.action = "na.fail", data =  ewpwAgeM) #set to na.fail 
drgTRbm <- dredge(globTRbm, subset = TF.TRbm, m.lim = c(0,3), fixed = "Age")
nrow(drgTRbm)
cand.modsTRbm <- get.models(drgTRbm, subset = T)

modNamesTRbm <- vector("character", length(cand.modsTRbm))
vifTRbm <- rep(NA, length(cand.modsTRbm))
modFormTRbm <- modsTRbm <- vector("list", length(cand.modsTRbm))
for(i in 1:length(cand.modsTRbm)){
  modFormTRbm[[i]] <- formula(cand.modsTRbm[[i]])
  print(i)
  minTRbm <- simpleCap(str_replace_all(as.character(modFormTRbm[[i]])[3], "TRbm", "")) #minus "TRbm"
  min1 <- unlist(strsplit(as.character(minTRbm), split = ' + 1 ', fixed = T)) #minus "+1"
  modNamesTRbm[i] <- paste(as.character(modFormTRbm[[i]])[2], as.character(modFormTRbm[[i]])[1], min1[1], min1[2])
  modsTRbm[[i]] <- lm(formula = modFormTRbm[[i]], data =  ewpwAgeM)
  if(length(coef(modsTRbm[[i]])) > 2){
    vifTRbm[[i]] <- max(car::vif(modsTRbm[[i]]))
  } 
}
max(vifTRbm, na.rm = T)
aic.tab.TRbm <- aictab(cand.set = modsTRbm, modnames = modNamesTRbm, sort = TRUE)

##Winter Mass
cormatTRwm <- round(cor(ewpwAgeM[, c("WiMDR","WiSrad", "WiTavg")], use = "complete.obs", method = "spearman"),2)
apply(cormatTRwm, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.TRwm <- apply(cormatTRwm, 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.TRwm[upper.tri(TF.TRwm, diag = T)] <- NA

globTRwm <- lm(Mass ~ Age + WiMDR + WiSrad + WiTavg, na.action = "na.fail", data =  ewpwAgeM) #set to na.fail 
drgTRwm <- dredge(globTRwm, subset = TF.TRwm, m.lim = c(0,3), fixed = "Age")
nrow(drgTRwm)
cand.modsTRwm <- get.models(drgTRwm, subset = T)

modNamesTRwm <- vector("character", length(cand.modsTRwm))
vifTRwm <- rep(NA, length(cand.modsTRwm))
modFormTRwm <- modsTRwm <- vector("list", length(cand.modsTRwm))
for(i in 1:length(cand.modsTRwm)){
  modFormTRwm[[i]] <- formula(cand.modsTRwm[[i]])
  print(i)
  minTRwm <- simpleCap(str_replace_all(as.character(modFormTRwm[[i]])[3], "TR", "")) #minus "TR"
  min1 <- unlist(strsplit(as.character(minTRwm), split = ' + 1 ', fixed = T)) #minus "+1"
  modNamesTRwm[i] <- paste(as.character(modFormTRwm[[i]])[2], as.character(modFormTRwm[[i]])[1], min1[1], min1[2])
  modsTRwm[[i]] <- lm(formula = modFormTRwm[[i]], data =  ewpwAgeM)
  if(length(coef(modsTRwm[[i]])) > 2){
    vifTRwm[[i]] <- max(car::vif(modsTRwm[[i]]))
  } 
}
max(vifTRwm, na.rm = T)
aic.tab.TRwm <- aictab(cand.set = modsTRwm, modnames = modNamesTRwm, sort = TRUE)

# >>Productivity ------------------------------------------------------------
##Breeding Wing
cormatProdB <- round(cor( ewpwAge[, c("BrEVI", "BrPrec", "BrTavg")], use = "complete.obs", method = "spearman"),2) 
apply(cormatProdB, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.ProdB <- apply(cormatProdB, 2, function(x){ifelse(x < .5 & x > -.5, T, F)})
TF.ProdB[upper.tri(TF.ProdB, diag = T)] <- NA

globProdB <- lm(Wing.Chord ~ Age + BrEVI + BrPrec + BrTavg, na.action = "na.fail", data =  ewpwAge) #set to na.fail 
drgProdB <- dredge(globProdB, subset = TF.ProdB, m.lim = c(0,3), fixed = "Age")
nrow(drgProdB)
cand.modsProdB <- get.models(drgProdB, subset = T)

modNamesProdB <- vector("character", length(cand.modsProdB))
vifProdB <- rep(NA, length(cand.modsProdB))
modFormProdB <- modsProdB <- vector("list", length(cand.modsProdB))
for(i in 1:length(cand.modsProdB)){
  modFormProdB[[i]] <- formula(cand.modsProdB[[i]])
  print(i)
  minProdB <- simpleCap(str_replace_all(as.character(modFormProdB[[i]])[3], "ProdB", "")) #minus "ProdB"
  min1 <- unlist(strsplit(as.character(minProdB), split = ' + 1 ', fixed = T)) #minus "+1"
  modNamesProdB[i] <- paste(as.character(modFormProdB[[i]])[2], as.character(modFormProdB[[i]])[1], min1[1], min1[2])
  modsProdB[[i]] <- lm(formula = modFormProdB[[i]], data =  ewpwAge)
  if(length(coef(modsProdB[[i]])) > 2){
    vifProdB[[i]] <- max(car::vif(modsProdB[[i]]))
  } 
}
max(vifProdB, na.rm = T)
aic.tab.ProdB <- aictab(cand.set = modsProdB, modnames = modNamesProdB, sort = TRUE)


##Winter Wing
cormatProdW <- round(cor( ewpwAge[, c("WiEVI", "WiPrec", "WiTavg")], use = "complete.obs", method = "spearman"),2) 
apply(cormatProdW, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.ProdW <- apply(cormatProdW, 2, function(x){ifelse(x < .5 & x > -.5, T, F)})
TF.ProdW[upper.tri(TF.ProdW, diag = T)] <- NA

globProdW <- lm(Wing.Chord ~ Age + WiEVI + WiPrec + WiTavg, na.action = "na.fail", data =  ewpwAge) #set to na.fail 
drgProdW <- dredge(globProdW, subset = TF.ProdW, m.lim = c(0,3), fixed = "Age")
nrow(drgProdW)
cand.modsProdW <- get.models(drgProdW, subset = T)

modNamesProdW <- vector("character", length(cand.modsProdW))
vifProdW <- rep(NA, length(cand.modsProdW))
modFormProdW <- modsProdW <- vector("list", length(cand.modsProdW))
for(i in 1:length(cand.modsProdW)){
  modFormProdW[[i]] <- formula(cand.modsProdW[[i]])
  print(i)
  minProdW <- simpleCap(str_replace_all(as.character(modFormProdW[[i]])[3], "ProdW", "")) #minus "ProdW"
  min1 <- unlist(strsplit(as.character(minProdW), split = ' + 1 ', fixed = T)) #minus "+1"
  modNamesProdW[i] <- paste(as.character(modFormProdW[[i]])[2], as.character(modFormProdW[[i]])[1], min1[1], min1[2])
  modsProdW[[i]] <- lm(formula = modFormProdW[[i]], data =  ewpwAge)
  if(length(coef(modsProdW[[i]])) > 2){
    vifProdW[[i]] <- max(car::vif(modsProdW[[i]]))
  } 
}
max(vifProdW, na.rm = T)
aic.tab.ProdW <- aictab(cand.set = modsProdW, modnames = modNamesProdW, sort = TRUE) 


##Breeding Mass
cormatProdBM <- round(cor(ewpwAgeM[, c("BrEVI", "BrPrec", "BrTavg")], use = "complete.obs", method = "spearman"),2) 
apply(cormatProdBM, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.ProdBM <- apply(cormatProdBM, 2, function(x){ifelse(x < .5 & x > -.5, T, F)})
TF.ProdBM[upper.tri(TF.ProdBM, diag = T)] <- NA

globProdBM <- lm(Mass ~ Age + BrEVI + BrPrec + BrTavg, na.action = "na.fail", data =  ewpwAgeM) #set to na.fail 
drgProdBM <- dredge(globProdBM, subset = TF.ProdBM, m.lim = c(0,3), fixed = "Age")
nrow(drgProdBM)
cand.modsProdBM <- get.models(drgProdBM, subset = T)

modNamesProdBM <- vector("character", length(cand.modsProdBM))
vifProdBM <- rep(NA, length(cand.modsProdBM))
modFormProdBM <- modsProdBM <- vector("list", length(cand.modsProdBM))
for(i in 1:length(cand.modsProdBM)){
  modFormProdBM[[i]] <- formula(cand.modsProdBM[[i]])
  print(i)
  minProdBM <- simpleCap(str_replace_all(as.character(modFormProdBM[[i]])[3], "ProdBM", "")) #minus "ProdBM"
  min1 <- unlist(strsplit(as.character(minProdBM), split = ' + 1 ', fixed = T)) #minus "+1"
  modNamesProdBM[i] <- paste(as.character(modFormProdBM[[i]])[2], as.character(modFormProdBM[[i]])[1], min1[1], min1[2])
  modsProdBM[[i]] <- lm(formula = modFormProdBM[[i]], data =  ewpwAgeM)
  if(length(coef(modsProdBM[[i]])) > 2){
    vifProdBM[[i]] <- max(car::vif(modsProdBM[[i]]))
  } 
}
max(vifProdBM, na.rm = T)
aic.tab.ProdBM <- aictab(cand.set = modsProdBM, modnames = modNamesProdBM, sort = TRUE)


##Winter Mass
cormatProdWM <- round(cor( ewpwAgeM[, c("WiEVI", "WiPrec", "WiTavg")], use = "complete.obs", method = "spearman"),2) 
apply(cormatProdWM, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.ProdWM <- apply(cormatProdWM, 2, function(x){ifelse(x < .5 & x > -.5, T, F)})
TF.ProdWM[upper.tri(TF.ProdWM, diag = T)] <- NA

globProdWM <- lm(Mass ~ Age + WiEVI + WiPrec + WiTavg, na.action = "na.fail", data =  ewpwAgeM) #set to na.fail 
drgProdWM <- dredge(globProdWM, subset = TF.ProdWM, m.lim = c(0,3), fixed = "Age")
nrow(drgProdWM)
cand.modsProdWM <- get.models(drgProdWM, subset = T)

modNamesProdWM <- vector("character", length(cand.modsProdWM))
vifProdWM <- rep(NA, length(cand.modsProdWM))
modFormProdWM <- modsProdWM <- vector("list", length(cand.modsProdWM))
for(i in 1:length(cand.modsProdWM)){
  modFormProdWM[[i]] <- formula(cand.modsProdWM[[i]])
  print(i)
  minProdWM <- simpleCap(str_replace_all(as.character(modFormProdWM[[i]])[3], "ProdWM", "")) #minus "ProdWM"
  min1 <- unlist(strsplit(as.character(minProdWM), split = ' + 1 ', fixed = T)) #minus "+1"
  modNamesProdWM[i] <- paste(as.character(modFormProdWM[[i]])[2], as.character(modFormProdWM[[i]])[1], min1[1], min1[2])
  modsProdWM[[i]] <- lm(formula = modFormProdWM[[i]], data =  ewpwAgeM)
  if(length(coef(modsProdWM[[i]])) > 2){
    vifProdWM[[i]] <- max(car::vif(modsProdWM[[i]]))
  } 
}
max(vifProdWM, na.rm = T)
aic.tab.ProdWM <- aictab(cand.set = modsProdWM, modnames = modNamesProdWM, sort = TRUE) 


# >>Seasonality (seas) -------------------------------------------------------------
##Breeding Wing 
cormatSeasB <- round(cor( ewpwAge[, c("BrCVevi", "BrPrecCV", "BrTcv")], use = "complete.obs", method = "spearman"),2) 
apply(cormatSeasB, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.SeasB <- apply(cormatSeasB, 2, function(x){ifelse(x < .5 & x > -.5, T, F)})
TF.SeasB[upper.tri(TF.SeasB, diag = T)] <- NA

globSeasB <- lm(Wing.Chord ~ Age + BrCVevi + BrPrecCV + BrTcv, na.action = "na.fail", data =  ewpwAge)
drgSeasB <- dredge(globSeasB, subset = TF.SeasB, m.lim = c(0,3), fixed = "Age")
nrow(drgSeasB)
cand.modsSeasB <- get.models(drgSeasB, subset = T)

modNamesSeasB <- vector("character", length(cand.modsSeasB))
vifSeasB <- rep(NA, length(cand.modsSeasB))
modFormSeasB <- modsSeasB <- vector("list", length(cand.modsSeasB))
for(i in 1:length(cand.modsSeasB)){
  modFormSeasB[[i]] <- formula(cand.modsSeasB[[i]])
  print(i)
  minSeasB <- simpleCap(str_replace_all(as.character(modFormSeasB[[i]])[3], "SeasB", "")) #minus "SeasB"
  min1 <- unlist(strsplit(as.character(minSeasB), split = ' + 1 ', fixed = T)) #minus "+1"
  modNamesSeasB[i] <- paste(as.character(modFormSeasB[[i]])[2], as.character(modFormSeasB[[i]])[1], min1[1], min1[2])
  modsSeasB[[i]] <- lm(formula = modFormSeasB[[i]], data =  ewpwAge)
  if(length(coef(modsSeasB[[i]])) > 2){
    vifSeasB[[i]] <- max(car::vif(modsSeasB[[i]]))
  } 
}
max(vifSeasB, na.rm = T)
aic.tab.SeasB <- aictab(cand.set = modsSeasB, modnames = modNamesSeasB, sort = TRUE)

##Winter Wing
cormatSeasW <- round(cor( ewpwAge[, c("WiCVevi", "WiPrecCV", "WiTcv")], use = "complete.obs", method = "spearman"),2) 
apply(cormatSeasW, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.SeasW <- apply(cormatSeasW, 2, function(x){ifelse(x < .5 & x > -.5, T, F)})
TF.SeasW[upper.tri(TF.SeasW, diag = T)] <- NA

globSeasW <- lm(Wing.Chord ~ Age + WiCVevi + WiPrecCV + WiTcv, na.action = "na.fail", data =  ewpwAge)
drgSeasW <- dredge(globSeasW, subset = TF.SeasW, m.lim = c(0,3), fixed = "Age")
nrow(drgSeasW)
cand.modsSeasW <- get.models(drgSeasW, subset = T)

modNamesSeasW <- vector("character", length(cand.modsSeasW))
vifSeasW <- rep(NA, length(cand.modsSeasW))
modFormSeasW <- modsSeasW <- vector("list", length(cand.modsSeasW))
for(i in 1:length(cand.modsSeasW)){
  modFormSeasW[[i]] <- formula(cand.modsSeasW[[i]])
  print(i)
  minSeasW <- simpleCap(str_replace_all(as.character(modFormSeasW[[i]])[3], "SeasW", "")) #minus "SeasW"
  min1 <- unlist(strsplit(as.character(minSeasW), split = ' + 1 ', fixed = T)) #minus "+1"
  modNamesSeasW[i] <- paste(as.character(modFormSeasW[[i]])[2], as.character(modFormSeasW[[i]])[1], min1[1], min1[2])
  modsSeasW[[i]] <- lm(formula = modFormSeasW[[i]], data =  ewpwAge)
  if(length(coef(modsSeasW[[i]])) > 2){
    vifSeasW[[i]] <- max(car::vif(modsSeasW[[i]]))
  } 
}
max(vifSeasW, na.rm = T)
aic.tab.SeasW <- aictab(cand.set = modsSeasW, modnames = modNamesSeasW, sort = TRUE)


##Breeding Mass
cormatSeasBM <- round(cor( ewpwAgeM[, c("BrCVevi", "BrPrecCV", "BrTcv")], use = "complete.obs", method = "spearman"),2) 
apply(cormatSeasBM, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.SeasBM <- apply(cormatSeasBM, 2, function(x){ifelse(x < .5 & x > -.5, T, F)})
TF.SeasBM[upper.tri(TF.SeasBM, diag = T)] <- NA

globSeasBM <- lm(Mass ~ Age + BrCVevi + BrPrecCV + BrTcv, na.action = "na.fail", data =  ewpwAgeM)
drgSeasBM <- dredge(globSeasBM, subset = TF.SeasBM, m.lim = c(0,3), fixed = "Age")
nrow(drgSeasBM)
cand.modsSeasBM <- get.models(drgSeasBM, subset = T)

modNamesSeasBM <- vector("character", length(cand.modsSeasBM))
vifSeasBM <- rep(NA, length(cand.modsSeasBM))
modFormSeasBM <- modsSeasBM <- vector("list", length(cand.modsSeasBM))
for(i in 1:length(cand.modsSeasBM)){
  modFormSeasBM[[i]] <- formula(cand.modsSeasBM[[i]])
  print(i)
  minSeasBM <- simpleCap(str_replace_all(as.character(modFormSeasBM[[i]])[3], "SeasBM", "")) #minus "SeasBM"
  min1 <- unlist(strsplit(as.character(minSeasBM), split = ' + 1 ', fixed = T)) #minus "+1"
  modNamesSeasBM[i] <- paste(as.character(modFormSeasBM[[i]])[2], as.character(modFormSeasBM[[i]])[1], min1[1], min1[2])
  modsSeasBM[[i]] <- lm(formula = modFormSeasBM[[i]], data =  ewpwAgeM)
  if(length(coef(modsSeasBM[[i]])) > 2){
    vifSeasBM[[i]] <- max(car::vif(modsSeasBM[[i]]))
  } 
}
max(vifSeasBM, na.rm = T)
aic.tab.SeasBM <- aictab(cand.set = modsSeasBM, modnames = modNamesSeasBM, sort = TRUE)

##Winter Mass
cormatSeasWM <- round(cor( ewpwAgeM[, c("WiCVevi", "WiPrecCV", "WiTcv")], use = "complete.obs", method = "spearman"),2) 
apply(cormatSeasWM, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.SeasWM <- apply(cormatSeasWM, 2, function(x){ifelse(x < .5 & x > -.5, T, F)})
TF.SeasWM[upper.tri(TF.SeasWM, diag = T)] <- NA

globSeasWM <- lm(Mass ~ Age + WiCVevi + WiPrecCV + WiTcv, na.action = "na.fail", data =  ewpwAgeM)
drgSeasWM <- dredge(globSeasWM, subset = TF.SeasWM, m.lim = c(0,3), fixed = "Age")
nrow(drgSeasWM)
cand.modsSeasWM <- get.models(drgSeasWM, subset = T)

modNamesSeasWM <- vector("character", length(cand.modsSeasWM))
vifSeasWM <- rep(NA, length(cand.modsSeasWM))
modFormSeasWM <- modsSeasWM <- vector("list", length(cand.modsSeasWM))
for(i in 1:length(cand.modsSeasWM)){
  modFormSeasWM[[i]] <- formula(cand.modsSeasWM[[i]])
  print(i)
  minSeasWM <- simpleCap(str_replace_all(as.character(modFormSeasWM[[i]])[3], "SeasWM", "")) #minus "SeasWM"
  min1 <- unlist(strsplit(as.character(minSeasWM), split = ' + 1 ', fixed = T)) #minus "+1"
  modNamesSeasWM[i] <- paste(as.character(modFormSeasWM[[i]])[2], as.character(modFormSeasWM[[i]])[1], min1[1], min1[2])
  modsSeasWM[[i]] <- lm(formula = modFormSeasWM[[i]], data =  ewpwAgeM)
  if(length(coef(modsSeasWM[[i]])) > 2){
    vifSeasWM[[i]] <- max(car::vif(modsSeasWM[[i]]))
  } 
}
max(vifSeasWM, na.rm = T)
aic.tab.SeasWM <- aictab(cand.set = modsSeasWM, modnames = modNamesSeasWM, sort = TRUE)


# >>Compete Hypotheses ------------------------------------------------------
##Wing
cand.modsComp <- list() #Region 
cand.modsComp[[1]] <- lm(Wing.Chord ~ B.Lat + Age, data =  ewpwAge, na.action = "na.fail")
cand.modsComp[[2]] <- lm(Wing.Chord ~ W.Long + Age, data =  ewpwAge, na.action = "na.fail") #Null model was top model
cand.modsComp[[3]] <- lm(Wing.Chord ~ BrTavg + Age, data =  ewpwAge, na.action = "na.fail")
cand.modsComp[[4]] <- lm(Wing.Chord ~ WiSrad + Age, data =  ewpwAge, na.action = "na.fail") #Null model
cand.modsComp[[5]] <- lm(Wing.Chord ~ BrPrec + Age, data =  ewpwAge, na.action = "na.fail")
cand.modsComp[[6]] <- lm(Wing.Chord ~ WiPrec + Age, data =  ewpwAge, na.action = "na.fail")
cand.modsComp[[7]] <- lm(Wing.Chord ~ BrTcv + Age, data =  ewpwAge, na.action = "na.fail")
cand.modsComp[[8]] <- lm(Wing.Chord ~ WiPrecCV + Age, data =  ewpwAge, na.action = "na.fail") #Null model
cand.modsComp[[9]] <- lm(Wing.Chord ~ str8line + Age, data =  ewpwAge, na.action = "na.fail")

modNamesComp <- vector("character", length(cand.modsComp))
vifComp <- rep(NA, length(cand.modsComp))
modFormComp <- modsComp <- vector("list", length(cand.modsComp))
for(i in 1:length(cand.modsComp)){
  modFormComp[[i]] <- formula(cand.modsComp[[i]])
  print(i)
  minComp <- simpleCap(str_replace_all(as.character(modFormComp[[i]])[3], "Comp", "")) #minus "Comp"
  min1 <- unlist(strsplit(as.character(minComp), split = ' + 1 ', fixed = T)) #minus "+1"
  modNamesComp[i] <- paste(as.character(modFormComp[[i]])[2], as.character(modFormComp[[i]])[1], min1[1])
  modsComp[[i]] <- lm(formula = modFormComp[[i]], data =  ewpwAge)
  if(length(coef(modsComp[[i]])) > 2){
    vifComp[[i]] <- max(car::vif(modsComp[[i]]))
  } 
}
max(vifComp, na.rm = T)
aic.tab.Comp <- aictab(cand.set = modsComp, modnames = modNamesComp, sort = TRUE)

cormatComp <- round(cor( ewpwAge[, c("BrPrec", "BrTavg", "B.Lat")], use = "complete.obs", method = "spearman"),2) 

aic.tab.SeasBM
##Mass
cand.modsCompM <- list() #Region 
cand.modsCompM[[1]] <- lm(Mass ~ B.Lat + B.Long + Age, data =  ewpwAgeM, na.action = "na.fail")
cand.modsCompM[[2]] <- lm(Mass ~ W.Lat + Age, data =  ewpwAgeM, na.action = "na.fail") #Null model was top model
cand.modsCompM[[3]] <- lm(Mass ~ BrTavg + Age, data =  ewpwAgeM, na.action = "na.fail")
cand.modsCompM[[4]] <- lm(Mass ~ WiSrad + Age, data =  ewpwAgeM, na.action = "na.fail") #Null model
cand.modsCompM[[5]] <- lm(Mass ~ BrTavg + Age, data =  ewpwAgeM, na.action = "na.fail") ##SAME AS TR
cand.modsCompM[[6]] <- lm(Mass ~ WiPrec + Age, data =  ewpwAgeM, na.action = "na.fail")
cand.modsCompM[[7]] <- lm(Mass ~ BrPrecCV + BrTcv + Age, data =  ewpwAgeM, na.action = "na.fail")
cand.modsCompM[[8]] <- lm(Mass ~ WiPrecCV + Age, data =  ewpwAgeM, na.action = "na.fail") #Null model
cand.modsCompM[[9]] <- lm(Mass ~ str8line + Age, data =  ewpwAgeM, na.action = "na.fail")

modNamesCompM <- vector("character", length(cand.modsCompM))
vifCompM <- rep(NA, length(cand.modsCompM))
modFormCompM <- modsCompM <- vector("list", length(cand.modsCompM))
for(i in 1:length(cand.modsCompM)){
  modFormCompM[[i]] <- formula(cand.modsCompM[[i]])
  print(i)
  minCompM <- simpleCap(str_replace_all(as.character(modFormCompM[[i]])[3], "CompM", "")) #minus "CompM"
  min1 <- unlist(strsplit(as.character(minCompM), split = ' + 1 ', fixed = T)) #minus "+1"
  modNamesCompM[i] <- paste(as.character(modFormCompM[[i]])[2], as.character(modFormCompM[[i]])[1], min1[1])
  modsCompM[[i]] <- lm(formula = modFormCompM[[i]], data =  ewpwAgeM)
  if(length(coef(modsCompM[[i]])) > 2){
    vifCompM[[i]] <- max(car::vif(modsCompM[[i]]))
  } 
}
max(vifCompM, na.rm = T)
aic.tab.CompM <- aictab(cand.set = modsCompM, modnames = modNamesCompM, sort = TRUE)

cormatCompM <- round(cor( ewpwAgeM[, c("BrPrecCV", "BrTcv", "B.Lat", "B.Long")], use = "complete.obs", method = "spearman"),2) 

# >>Export table ------------------------------------------------------------
# Number of models in each hypothesis
sum(sapply(list(aic.tab.GeoW, aic.tab.TR, aic.tab.Prod, aic.tab.Seas),nrow))
AICbergEWPW <- rbind(aic.tab.GeoW, aic.tab.TR, aic.tab.Prod, aic.tab.Seas)
AICbergEWPW[,c(3:8)] <- lapply(AICbergEWPW[,c(3:8)], round, 2)
#AICbergEWPW$Modnames <- sapply(strsplit(AICcomboAll$Modnames, split = "log"), function(x){x[2]})
for(i in 1:length(AICbergEWPW$Modnames)){
  if(grepl("C13" , AICbergEWPW$Modnames[i], fixed = T)){
    AICbergEWPW$Modnames[i] <- str_replace(AICbergEWPW$Modnames[i], "C13", "δ13C")
  }
}
AICbergEWPW <- subset(AICbergEWPW, select = -ModelLik)
names(AICcomboAll)[1] <- "Model"
names(AICbergEWPW)[6] <- "-2 Log likelihood"

nrow(AICbergEWPW)
write.table(AICbergEWPW, file = "/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Results/AICbergEWPW.txt", sep = ",", quote = FALSE, row.names = F)



# CONI --------------------------------------------------------------------
# >>Geography ---------------------------------------------------------------
##Breeding Wing
cormatGeoBconi <- round(cor(coni.w[, c("B.Lat", "B.Long", "elev.Breed")], use = "complete.obs", method = "spearman"),2) #GeoBconigraphy
#"elev.Winter", "W.Lat"
apply(cormatGeoBconi, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.GeoBconi <- apply(cormatGeoBconi, 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.GeoBconi[upper.tri(TF.GeoBconi, diag = T)] <- NA

globGeoBconi <- lm(Wing.Chord ~  B.Lat + B.Long + elev.Breed, na.action = "na.fail", data =  coni.w) #set to na.fail 
getAllTerms(globGeoBconi) # terms in matrix for subset and global models must be in this order
drgGeoBconi <- dredge(globGeoBconi, subset = TF.GeoBconi, m.lim = c(0,3))
nrow(drgGeoBconi)
cand.modsGeoBconi <- get.models(drgGeoBconi, subset = T)

modNamesGeoBconi <- vector("character", length(cand.modsGeoBconi))
vifGeoBconi <- rep(NA, length(cand.modsGeoBconi))
modFormGeoBconi <- modsGeoBconi <- vector("list", length(cand.modsGeoBconi))
for(i in 1:length(cand.modsGeoBconi)){
  modFormGeoBconi[[i]] <- formula(cand.modsGeoBconi[[i]])
  print(i)
  minGeoBconi <- simpleCap(str_replace_all(as.character(modFormGeoBconi[[i]])[3], "GeoBconi", "")) #minus "GeoBconi"
  min1 <- unlist(strsplit(as.character(minGeoBconi), split = '+ 1', fixed = T)) #minus "+1"
  modNamesGeoBconi[i] <- paste(as.character(modFormGeoBconi[[i]])[2], as.character(modFormGeoBconi[[i]])[1], min1[1])
  modsGeoBconi[[i]] <- lm(formula = modFormGeoBconi[[i]], data =  coni.w)
  if(length(coef(modsGeoBconi[[i]])) > 2){
    vifGeoBconi[[i]] <- max(car::vif(modsGeoBconi[[i]]))
  } 
}
max(vifGeoBconi, na.rm = T)
aic.tab.GeoBconi <- aictab(cand.set = modsGeoBconi, modnames = modNamesGeoBconi, sort = TRUE)
which(duplicated(aic.tab.GeoBconi$AICc))

##Winter Wing
cormatGeoWconi <- round(cor(coni.w[, c("W.Lat", "W.Long", "elev.Winter")], use = "complete.obs", method = "spearman"),2) #GeoWconigraphy
#"elev.Winter", "W.Lat"
apply(cormatGeoWconi, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.GeoWconi <- apply(cormatGeoWconi, 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.GeoWconi[upper.tri(TF.GeoWconi, diag = T)] <- NA

globGeoWconi <- lm(Wing.Chord ~  W.Lat + W.Long + elev.Winter, na.action = "na.fail", data =  coni.w) #set to na.fail 
getAllTerms(globGeoWconi) # terms in matrix for subset and global models must be in this order
drgGeoWconi <- dredge(globGeoWconi, subset = TF.GeoWconi, m.lim = c(0,3))
nrow(drgGeoWconi)
cand.modsGeoWconi <- get.models(drgGeoWconi, subset = T)

modNamesGeoWconi <- vector("character", length(cand.modsGeoWconi))
vifGeoWconi <- rep(NA, length(cand.modsGeoWconi))
modFormGeoWconi <- modsGeoWconi <- vector("list", length(cand.modsGeoWconi))
for(i in 1:length(cand.modsGeoWconi)){
  modFormGeoWconi[[i]] <- formula(cand.modsGeoWconi[[i]])
  print(i)
  minGeoWconi <- simpleCap(str_replace_all(as.character(modFormGeoWconi[[i]])[3], "GeoWconi", "")) #minus "GeoWconi"
  min1 <- unlist(strsplit(as.character(minGeoWconi), split = '+ 1', fixed = T)) #minus "+1"
  modNamesGeoWconi[i] <- paste(as.character(modFormGeoWconi[[i]])[2], as.character(modFormGeoWconi[[i]])[1], min1[1])
  modsGeoWconi[[i]] <- lm(formula = modFormGeoWconi[[i]], data =  coni.w)
  if(length(coef(modsGeoWconi[[i]])) > 2){
    vifGeoWconi[[i]] <- max(car::vif(modsGeoWconi[[i]]))
  } 
}
max(vifGeoWconi, na.rm = T)
aic.tab.GeoWconi <- aictab(cand.set = modsGeoWconi, modnames = modNamesGeoWconi, sort = TRUE)


##Breeding Mass
cormatGeoBMconi <- round(cor(coni.m[, c("B.Lat", "B.Long", "elev.Breed")], use = "complete.obs", method = "spearman"),2) #GeoBMconigraphy
#"elev.Winter", "W.Lat"
apply(cormatGeoBMconi, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.GeoBMconi <- apply(cormatGeoBMconi, 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.GeoBMconi[upper.tri(TF.GeoBMconi, diag = T)] <- NA

globGeoBMconi <- lm(Mass ~  B.Lat + B.Long + elev.Breed, na.action = "na.fail", data =  coni.m) #set to na.fail 
getAllTerms(globGeoBMconi) # terms in matrix for subset and global models must be in this order
drgGeoBMconi <- dredge(globGeoBMconi, subset = TF.GeoBMconi, m.lim = c(0,3))
nrow(drgGeoBMconi)
cand.modsGeoBMconi <- get.models(drgGeoBMconi, subset = T)

modNamesGeoBMconi <- vector("character", length(cand.modsGeoBMconi))
vifGeoBMconi <- rep(NA, length(cand.modsGeoBMconi))
modFormGeoBMconi <- modsGeoBMconi <- vector("list", length(cand.modsGeoBMconi))
for(i in 1:length(cand.modsGeoBMconi)){
  modFormGeoBMconi[[i]] <- formula(cand.modsGeoBMconi[[i]])
  print(i)
  minGeoBMconi <- simpleCap(str_replace_all(as.character(modFormGeoBMconi[[i]])[3], "GeoBMconi", "")) #minus "GeoBMconi"
  min1 <- unlist(strsplit(as.character(minGeoBMconi), split = '+ 1', fixed = T)) #minus "+1"
  modNamesGeoBMconi[i] <- paste(as.character(modFormGeoBMconi[[i]])[2], as.character(modFormGeoBMconi[[i]])[1], min1[1])
  modsGeoBMconi[[i]] <- lm(formula = modFormGeoBMconi[[i]], data =  coni.m)
  if(length(coef(modsGeoBMconi[[i]])) > 2){
    vifGeoBMconi[[i]] <- max(car::vif(modsGeoBMconi[[i]]))
  } 
}
max(vifGeoBMconi, na.rm = T)
aic.tab.GeoBMconi <- aictab(cand.set = modsGeoBMconi, modnames = modNamesGeoBMconi, sort = TRUE)
which(duplicated(aic.tab.GeoBMconi$AICc))

##Winter Mass
cormatGeoWMconi <- round(cor( coni.m[, c("W.Lat", "W.Long", "elev.Winter")], use = "complete.obs", method = "spearman"),2) #GeoWMconigraphy
#"elev.Winter", "W.Lat"
apply(cormatGeoWMconi, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.GeoWMconi <- apply(cormatGeoWMconi, 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.GeoWMconi[upper.tri(TF.GeoWMconi, diag = T)] <- NA

globGeoWMconi <- lm(Mass ~  W.Lat + W.Long + elev.Winter, na.action = "na.fail", data =  coni.m) #set to na.fail 
getAllTerms(globGeoWMconi) # terms in matrix for subset and global models must be in this order
drgGeoWMconi <- dredge(globGeoWMconi, subset = TF.GeoWMconi, m.lim = c(0,3))
nrow(drgGeoWMconi)
cand.modsGeoWMconi <- get.models(drgGeoWMconi, subset = T)

modNamesGeoWMconi <- vector("character", length(cand.modsGeoWMconi))
vifGeoWMconi <- rep(NA, length(cand.modsGeoWMconi))
modFormGeoWMconi <- modsGeoWMconi <- vector("list", length(cand.modsGeoWMconi))
for(i in 1:length(cand.modsGeoWMconi)){
  modFormGeoWMconi[[i]] <- formula(cand.modsGeoWMconi[[i]])
  print(i)
  minGeoWMconi <- simpleCap(str_replace_all(as.character(modFormGeoWMconi[[i]])[3], "GeoWMconi", "")) #minus "GeoWMconi"
  min1 <- unlist(strsplit(as.character(minGeoWMconi), split = '+ 1', fixed = T)) #minus "+1"
  modNamesGeoWMconi[i] <- paste(as.character(modFormGeoWMconi[[i]])[2], as.character(modFormGeoWMconi[[i]])[1], min1[1])
  modsGeoWMconi[[i]] <- lm(formula = modFormGeoWMconi[[i]], data =  coni.m)
  if(length(coef(modsGeoWMconi[[i]])) > 2){
    vifGeoWMconi[[i]] <- max(car::vif(modsGeoWMconi[[i]]))
  } 
}
max(vifGeoWMconi, na.rm = T)
aic.tab.GeoWMconi <- aictab(cand.set = modsGeoWMconi, modnames = modNamesGeoWMconi, sort = TRUE)


# >>Temp regulation (TR) -------------------------------------------------------
###Breeding Wing
cormatTRbConi <- round(cor( coni.w[, c("BrMDR", "BrSrad", "BrTavg")], use = "complete.obs", method = "spearman"),2)
apply(cormatTRbConi, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.TRbConi <- apply(cormatTRbConi, 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.TRbConi[upper.tri(TF.TRbConi, diag = T)] <- NA

globTRbConi <- lm(Wing.Chord ~  BrMDR + BrSrad + BrTavg, na.action = "na.fail", data =  coni.w) #set to na.fail 
drgTRbConi <- dredge(globTRbConi, subset = TF.TRbConi, m.lim = c(0,3))
nrow(drgTRbConi)
cand.modsTRbConi <- get.models(drgTRbConi, subset = T)

modNamesTRbConi <- vector("character", length(cand.modsTRbConi))
vifTRbConi <- rep(NA, length(cand.modsTRbConi))
modFormTRbConi <- modsTRbConi <- vector("list", length(cand.modsTRbConi))
for(i in 1:length(cand.modsTRbConi)){
  modFormTRbConi[[i]] <- formula(cand.modsTRbConi[[i]])
  print(i)
  minTRbConi <- simpleCap(str_replace_all(as.character(modFormTRbConi[[i]])[3], "TRbConi", "")) #minus "TRbConi"
  min1 <- unlist(strsplit(as.character(minTRbConi), split = '+ 1', fixed = T)) #minus "+1"
  modNamesTRbConi[i] <- paste(as.character(modFormTRbConi[[i]])[2], as.character(modFormTRbConi[[i]])[1], min1[1])
  modsTRbConi[[i]] <- lm(formula = modFormTRbConi[[i]], data =  coni.w)
  if(length(coef(modsTRbConi[[i]])) > 2){
    vifTRbConi[[i]] <- max(car::vif(modsTRbConi[[i]]))
  } 
}
max(vifTRbConi, na.rm = T)
aic.tab.TRbConi <- aictab(cand.set = modsTRbConi, modnames = modNamesTRbConi, sort = TRUE)

##Winter Wing
cormatTRwConi <- round(cor( coni.w[, c("WiMDR","WiSrad", "WiTavg")], use = "complete.obs", method = "spearman"),2)
apply(cormatTRwConi, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.TRwConi <- apply(cormatTRwConi, 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.TRwConi[upper.tri(TF.TRwConi, diag = T)] <- NA

globTRwConi <- lm(Wing.Chord ~  WiMDR + WiSrad + WiTavg, na.action = "na.fail", data =  coni.w) #set to na.fail 
drgTRwConi <- dredge(globTRwConi, subset = TF.TRwConi, m.lim = c(0,3))
nrow(drgTRwConi)
cand.modsTRwConi <- get.models(drgTRwConi, subset = T)

modNamesTRwConi <- vector("character", length(cand.modsTRwConi))
vifTRwConi <- rep(NA, length(cand.modsTRwConi))
modFormTRwConi <- modsTRwConi <- vector("list", length(cand.modsTRwConi))
for(i in 1:length(cand.modsTRwConi)){
  modFormTRwConi[[i]] <- formula(cand.modsTRwConi[[i]])
  print(i)
  minTRwConi <- simpleCap(str_replace_all(as.character(modFormTRwConi[[i]])[3], "TR", "")) #minus "TR"
  min1 <- unlist(strsplit(as.character(minTRwConi), split = '+ 1', fixed = T)) #minus "+1"
  modNamesTRwConi[i] <- paste(as.character(modFormTRwConi[[i]])[2], as.character(modFormTRwConi[[i]])[1], min1[1])
  modsTRwConi[[i]] <- lm(formula = modFormTRwConi[[i]], data =  coni.w)
  if(length(coef(modsTRwConi[[i]])) > 2){
    vifTRwConi[[i]] <- max(car::vif(modsTRwConi[[i]]))
  } 
}
max(vifTRwConi, na.rm = T)
aic.tab.TRwConi <- aictab(cand.set = modsTRwConi, modnames = modNamesTRwConi, sort = TRUE)

###Breeding Mass
cormatTRbmConi <- round(cor( coni.m[, c("BrMDR", "BrSrad", "BrTavg")], use = "complete.obs", method = "spearman"),2)
apply(cormatTRbmConi, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.TRbmConi <- apply(cormatTRbmConi, 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.TRbmConi[upper.tri(TF.TRbmConi, diag = T)] <- NA

globTRbmConi <- lm(Mass ~  BrMDR + BrSrad + BrTavg, na.action = "na.fail", data =  coni.m) #set to na.fail 
drgTRbmConi <- dredge(globTRbmConi, subset = TF.TRbmConi, m.lim = c(0,3))
nrow(drgTRbmConi)
cand.modsTRbmConi <- get.models(drgTRbmConi, subset = T)

modNamesTRbmConi <- vector("character", length(cand.modsTRbmConi))
vifTRbmConi <- rep(NA, length(cand.modsTRbmConi))
modFormTRbmConi <- modsTRbmConi <- vector("list", length(cand.modsTRbmConi))
for(i in 1:length(cand.modsTRbmConi)){
  modFormTRbmConi[[i]] <- formula(cand.modsTRbmConi[[i]])
  print(i)
  minTRbmConi <- simpleCap(str_replace_all(as.character(modFormTRbmConi[[i]])[3], "TRbmConi", "")) #minus "TRbmConi"
  min1 <- unlist(strsplit(as.character(minTRbmConi), split = '+ 1', fixed = T)) #minus "+1"
  modNamesTRbmConi[i] <- paste(as.character(modFormTRbmConi[[i]])[2], as.character(modFormTRbmConi[[i]])[1], min1[1])
  modsTRbmConi[[i]] <- lm(formula = modFormTRbmConi[[i]], data =  coni.m)
  if(length(coef(modsTRbmConi[[i]])) > 2){
    vifTRbmConi[[i]] <- max(car::vif(modsTRbmConi[[i]]))
  } 
}
max(vifTRbmConi, na.rm = T)
aic.tab.TRbmConi <- aictab(cand.set = modsTRbmConi, modnames = modNamesTRbmConi, sort = TRUE)

##Winter Mass
cormatTRwmConi <- round(cor( coni.m[, c("WiMDR","WiSrad", "WiTavg")], use = "complete.obs", method = "spearman"),2)
apply(cormatTRwmConi, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.TRwmConi <- apply(cormatTRwmConi, 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.TRwmConi[upper.tri(TF.TRwmConi, diag = T)] <- NA

globTRwmConi <- lm(Mass ~  WiMDR + WiSrad + WiTavg, na.action = "na.fail", data =  coni.m) #set to na.fail 
drgTRwmConi <- dredge(globTRwmConi, subset = TF.TRwmConi, m.lim = c(0,3))
nrow(drgTRwmConi)
cand.modsTRwmConi <- get.models(drgTRwmConi, subset = T)

modNamesTRwmConi <- vector("character", length(cand.modsTRwmConi))
vifTRwmConi <- rep(NA, length(cand.modsTRwmConi))
modFormTRwmConi <- modsTRwmConi <- vector("list", length(cand.modsTRwmConi))
for(i in 1:length(cand.modsTRwmConi)){
  modFormTRwmConi[[i]] <- formula(cand.modsTRwmConi[[i]])
  print(i)
  minTRwmConi <- simpleCap(str_replace_all(as.character(modFormTRwmConi[[i]])[3], "TR", "")) #minus "TR"
  min1 <- unlist(strsplit(as.character(minTRwmConi), split = '+ 1', fixed = T)) #minus "+1"
  modNamesTRwmConi[i] <- paste(as.character(modFormTRwmConi[[i]])[2], as.character(modFormTRwmConi[[i]])[1], min1[1])
  modsTRwmConi[[i]] <- lm(formula = modFormTRwmConi[[i]], data =  coni.m)
  if(length(coef(modsTRwmConi[[i]])) > 2){
    vifTRwmConi[[i]] <- max(car::vif(modsTRwmConi[[i]]))
  } 
}
max(vifTRwmConi, na.rm = T)
aic.tab.TRwmConi <- aictab(cand.set = modsTRwmConi, modnames = modNamesTRwmConi, sort = TRUE)

# >>Productivity ------------------------------------------------------------
##Breeding Wing
cormatProdBconi <- round(cor( coni.w[, c("BrEVI", "BrPrec", "BrTavg")], use = "complete.obs", method = "spearman"),2) 
apply(cormatProdBconi, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.ProdBconi <- apply(cormatProdBconi, 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.ProdBconi[upper.tri(TF.ProdBconi, diag = T)] <- NA

globProdBconi <- lm(Wing.Chord ~  BrEVI + BrPrec + BrTavg, na.action = "na.fail", data =  coni.w) #set to na.fail 
drgProdBconi <- dredge(globProdBconi, subset = TF.ProdBconi, m.lim = c(0,3))
nrow(drgProdBconi)
cand.modsProdBconi <- get.models(drgProdBconi, subset = T)

modNamesProdBconi <- vector("character", length(cand.modsProdBconi))
vifProdBconi <- rep(NA, length(cand.modsProdBconi))
modFormProdBconi <- modsProdBconi <- vector("list", length(cand.modsProdBconi))
for(i in 1:length(cand.modsProdBconi)){
  modFormProdBconi[[i]] <- formula(cand.modsProdBconi[[i]])
  print(i)
  minProdBconi <- simpleCap(str_replace_all(as.character(modFormProdBconi[[i]])[3], "ProdBconi", "")) #minus "ProdBconi"
  min1 <- unlist(strsplit(as.character(minProdBconi), split = '+ 1', fixed = T)) #minus "+1"
  modNamesProdBconi[i] <- paste(as.character(modFormProdBconi[[i]])[2], as.character(modFormProdBconi[[i]])[1], min1[1])
  modsProdBconi[[i]] <- lm(formula = modFormProdBconi[[i]], data =  coni.w)
  if(length(coef(modsProdBconi[[i]])) > 2){
    vifProdBconi[[i]] <- max(car::vif(modsProdBconi[[i]]))
  } 
}
max(vifProdBconi, na.rm = T)
aic.tab.ProdBconi <- aictab(cand.set = modsProdBconi, modnames = modNamesProdBconi, sort = TRUE)

##Winter Wing
cormatProdWconi <- round(cor( coni.w[, c("WiEVI", "WiPrec", "WiTavg")], use = "complete.obs", method = "spearman"),2) 
apply(cormatProdWconi, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.ProdWconi <- apply(cormatProdWconi, 2, function(x){ifelse(x < .5 & x > -.5, T, F)})
TF.ProdWconi[upper.tri(TF.ProdWconi, diag = T)] <- NA

globProdWconi <- lm(Wing.Chord ~  WiEVI + WiPrec + WiTavg, na.action = "na.fail", data =  coni.w) #set to na.fail 
drgProdWconi <- dredge(globProdWconi, subset = TF.ProdWconi, m.lim = c(0,3))
nrow(drgProdWconi)
cand.modsProdWconi <- get.models(drgProdWconi, subset = T)

modNamesProdWconi <- vector("character", length(cand.modsProdWconi))
vifProdWconi <- rep(NA, length(cand.modsProdWconi))
modFormProdWconi <- modsProdWconi <- vector("list", length(cand.modsProdWconi))
for(i in 1:length(cand.modsProdWconi)){
  modFormProdWconi[[i]] <- formula(cand.modsProdWconi[[i]])
  print(i)
  minProdWconi <- simpleCap(str_replace_all(as.character(modFormProdWconi[[i]])[3], "ProdWconi", "")) #minus "ProdWconi"
  min1 <- unlist(strsplit(as.character(minProdWconi), split = '+ 1', fixed = T)) #minus "+1"
  modNamesProdWconi[i] <- paste(as.character(modFormProdWconi[[i]])[2], as.character(modFormProdWconi[[i]])[1], min1[1])
  modsProdWconi[[i]] <- lm(formula = modFormProdWconi[[i]], data =  coni.w)
  if(length(coef(modsProdWconi[[i]])) > 2){
    vifProdWconi[[i]] <- max(car::vif(modsProdWconi[[i]]))
  } 
}
max(vifProdWconi, na.rm = T)
aic.tab.ProdWconi <- aictab(cand.set = modsProdWconi, modnames = modNamesProdWconi, sort = TRUE) 


##Breeding Mass
cormatProdBconi <- round(cor( coni.w[, c("BrEVI", "BrPrec", "BrTavg")], use = "complete.obs", method = "spearman"),2) 
apply(cormatProdBconi, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.ProdBconi <- apply(cormatProdBconi, 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.ProdBconi[upper.tri(TF.ProdBconi, diag = T)] <- NA

globProdBconi <- lm(Wing.Chord ~  BrEVI + BrPrec + BrTavg, na.action = "na.fail", data =  coni.w) #set to na.fail 
drgProdBconi <- dredge(globProdBconi, subset = TF.ProdBconi, m.lim = c(0,3))
nrow(drgProdBconi)
cand.modsProdBconi <- get.models(drgProdBconi, subset = T)

modNamesProdBconi <- vector("character", length(cand.modsProdBconi))
vifProdBconi <- rep(NA, length(cand.modsProdBconi))
modFormProdBconi <- modsProdBconi <- vector("list", length(cand.modsProdBconi))
for(i in 1:length(cand.modsProdBconi)){
  modFormProdBconi[[i]] <- formula(cand.modsProdBconi[[i]])
  print(i)
  minProdBconi <- simpleCap(str_replace_all(as.character(modFormProdBconi[[i]])[3], "ProdBconi", "")) #minus "ProdBconi"
  min1 <- unlist(strsplit(as.character(minProdBconi), split = '+ 1', fixed = T)) #minus "+1"
  modNamesProdBconi[i] <- paste(as.character(modFormProdBconi[[i]])[2], as.character(modFormProdBconi[[i]])[1], min1[1])
  modsProdBconi[[i]] <- lm(formula = modFormProdBconi[[i]], data =  coni.w)
  if(length(coef(modsProdBconi[[i]])) > 2){
    vifProdBconi[[i]] <- max(car::vif(modsProdBconi[[i]]))
  } 
}
max(vifProdBconi, na.rm = T)
aic.tab.ProdBconi <- aictab(cand.set = modsProdBconi, modnames = modNamesProdBconi, sort = TRUE)

##Winter Mass
cormatProdWconi <- round(cor( coni.w[, c("WiEVI", "WiPrec", "WiTavg")], use = "complete.obs", method = "spearman"),2) 
apply(cormatProdWconi, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.ProdWconi <- apply(cormatProdWconi, 2, function(x){ifelse(x < .5 & x > -.5, T, F)})
TF.ProdWconi[upper.tri(TF.ProdWconi, diag = T)] <- NA

globProdWconi <- lm(Wing.Chord ~  WiEVI + WiPrec + WiTavg, na.action = "na.fail", data =  coni.w) #set to na.fail 
drgProdWconi <- dredge(globProdWconi, subset = TF.ProdWconi, m.lim = c(0,3))
nrow(drgProdWconi)
cand.modsProdWconi <- get.models(drgProdWconi, subset = T)

modNamesProdWconi <- vector("character", length(cand.modsProdWconi))
vifProdWconi <- rep(NA, length(cand.modsProdWconi))
modFormProdWconi <- modsProdWconi <- vector("list", length(cand.modsProdWconi))
for(i in 1:length(cand.modsProdWconi)){
  modFormProdWconi[[i]] <- formula(cand.modsProdWconi[[i]])
  print(i)
  minProdWconi <- simpleCap(str_replace_all(as.character(modFormProdWconi[[i]])[3], "ProdWconi", "")) #minus "ProdWconi"
  min1 <- unlist(strsplit(as.character(minProdWconi), split = '+ 1', fixed = T)) #minus "+1"
  modNamesProdWconi[i] <- paste(as.character(modFormProdWconi[[i]])[2], as.character(modFormProdWconi[[i]])[1], min1[1])
  modsProdWconi[[i]] <- lm(formula = modFormProdWconi[[i]], data =  coni.w)
  if(length(coef(modsProdWconi[[i]])) > 2){
    vifProdWconi[[i]] <- max(car::vif(modsProdWconi[[i]]))
  } 
}
max(vifProdWconi, na.rm = T)
aic.tab.ProdWconi <- aictab(cand.set = modsProdWconi, modnames = modNamesProdWconi, sort = TRUE) 

# >>Seasonality (seas) -------------------------------------------------------------
cormatSeasBconi <- round(cor( coni.w[, c("BrCVevi", "BrPrecCV", "BrTcv")], use = "complete.obs", method = "spearman"),2) 
apply(cormatSeasBconi, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.SeasBconi <- apply(cormatSeasBconi, 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.SeasBconi[upper.tri(TF.SeasBconi, diag = T)] <- NA

globSeasBconi <- lm(Wing.Chord ~  BrCVevi + BrPrecCV + BrTcv, na.action = "na.fail", data =  coni.w)
drgSeasBconi <- dredge(globSeasBconi, subset = TF.SeasBconi, m.lim = c(0,3))
nrow(drgSeasBconi)
cand.modsSeasBconi <- get.models(drgSeasBconi, subset = T)

modNamesSeasBconi <- vector("character", length(cand.modsSeasBconi))
vifSeasBconi <- rep(NA, length(cand.modsSeasBconi))
modFormSeasBconi <- modsSeasBconi <- vector("list", length(cand.modsSeasBconi))
for(i in 1:length(cand.modsSeasBconi)){
  modFormSeasBconi[[i]] <- formula(cand.modsSeasBconi[[i]])
  print(i)
  minSeasBconi <- simpleCap(str_replace_all(as.character(modFormSeasBconi[[i]])[3], "SeasBconi", "")) #minus "SeasBconi"
  min1 <- unlist(strsplit(as.character(minSeasBconi), split = '+ 1', fixed = T)) #minus "+1"
  modNamesSeasBconi[i] <- paste(as.character(modFormSeasBconi[[i]])[2], as.character(modFormSeasBconi[[i]])[1], min1[1])
  modsSeasBconi[[i]] <- lm(formula = modFormSeasBconi[[i]], data =  coni.w)
  if(length(coef(modsSeasBconi[[i]])) > 2){
    vifSeasBconi[[i]] <- max(car::vif(modsSeasBconi[[i]]))
  } 
}
max(vifSeasBconi, na.rm = T)
aic.tab.SeasBconi <- aictab(cand.set = modsSeasBconi, modnames = modNamesSeasBconi, sort = TRUE)

##Winter
cormatSeasWconi <- round(cor( coni.w[, c("WiCVevi", "WiPrecCV", "WiTcv")], use = "complete.obs", method = "spearman"),2) 
apply(cormatSeasWconi, 2, function(x){ifelse(x < .7 & x > -.7, NA, x)})
TF.SeasWconi <- apply(cormatSeasWconi, 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.SeasWconi[upper.tri(TF.SeasWconi, diag = T)] <- NA

globSeasWconi <- lm(Wing.Chord ~  WiCVevi + WiPrecCV + WiTcv, na.action = "na.fail", data =  coni.w)
drgSeasWconi <- dredge(globSeasWconi, subset = TF.SeasWconi, m.lim = c(0,3))
nrow(drgSeasWconi)
cand.modsSeasWconi <- get.models(drgSeasWconi, subset = T)

modNamesSeasWconi <- vector("character", length(cand.modsSeasWconi))
vifSeasWconi <- rep(NA, length(cand.modsSeasWconi))
modFormSeasWconi <- modsSeasWconi <- vector("list", length(cand.modsSeasWconi))
for(i in 1:length(cand.modsSeasWconi)){
  modFormSeasWconi[[i]] <- formula(cand.modsSeasWconi[[i]])
  print(i)
  minSeasWconi <- simpleCap(str_replace_all(as.character(modFormSeasWconi[[i]])[3], "SeasWconi", "")) #minus "SeasWconi"
  min1 <- unlist(strsplit(as.character(minSeasWconi), split = '+ 1', fixed = T)) #minus "+1"
  modNamesSeasWconi[i] <- paste(as.character(modFormSeasWconi[[i]])[2], as.character(modFormSeasWconi[[i]])[1], min1[1])
  modsSeasWconi[[i]] <- lm(formula = modFormSeasWconi[[i]], data =  coni.w)
  if(length(coef(modsSeasWconi[[i]])) > 2){
    vifSeasWconi[[i]] <- max(car::vif(modsSeasWconi[[i]]))
  } 
}
max(vifSeasWconi, na.rm = T)
aic.tab.SeasWconi <- aictab(cand.set = modsSeasWconi, modnames = modNamesSeasWconi, sort = TRUE)


# >>Compete Hypotheses ------------------------------------------------------
aic.tab.GeoWconi

summary()


cand.modsCompConi <- list() #Region 
cand.modsCompConi[[1]] <- lm(Wing.Chord ~ B.Lat, data =  coni.w, na.action = "na.fail")
cand.modsCompConi[[2]] <- lm(Wing.Chord ~ elev.Winter, data =  coni.w, na.action = "na.fail") #Null model was top model
cand.modsCompConi[[3]] <- lm(Wing.Chord ~ BrTavg, data =  coni.w, na.action = "na.fail")
cand.modsCompConi[[4]] <- lm(Wing.Chord ~ WiMDR, data =  coni.w, na.action = "na.fail") 
cand.modsCompConi[[5]] <- lm(Wing.Chord ~ BrEVI + BrTavg, data =  coni.w, na.action = "na.fail")
cand.modsCompConi[[6]] <- lm(Wing.Chord ~ WiPrec, data =  coni.w, na.action = "na.fail") #Null model
cand.modsCompConi[[7]] <- lm(Wing.Chord ~ BrPrecCV, data =  coni.w, na.action = "na.fail")
cand.modsCompConi[[8]] <- lm(Wing.Chord ~ WiCVevi, data =  coni.w, na.action = "na.fail")
cand.modsCompConi[[9]] <- lm(Wing.Chord ~ str8line, data =  coni.w, na.action = "na.fail")

modNamesCompConi <- vector("character", length(cand.modsCompConi))
vifCompConi <- rep(NA, length(cand.modsCompConi))
modFormCompConi <- modsCompConi <- vector("list", length(cand.modsCompConi))
for(i in 1:length(cand.modsCompConi)){
  modFormCompConi[[i]] <- formula(cand.modsCompConi[[i]])
  print(i)
  minCompConi <- simpleCap(str_replace_all(as.character(modFormCompConi[[i]])[3], "CompConi", "")) #minus "CompConi"
  min1 <- unlist(strsplit(as.character(minCompConi), split = '+ 1', fixed = T)) #minus "+1"
  modNamesCompConi[i] <- paste(as.character(modFormCompConi[[i]])[2], as.character(modFormCompConi[[i]])[1], min1)
  modsCompConi[[i]] <- lm(formula = modFormCompConi[[i]], data =  coni.w)
  if(length(coef(modsCompConi[[i]])) > 2){
    vifCompConi[[i]] <- max(car::vif(modsCompConi[[i]]))
  } 
}
max(vifCompConi, na.rm = T)
aic.tab.CompConi <- aictab(cand.set = modsCompConi, modnames = modNamesCompConi, sort = TRUE)

cormatCompConi <- round(cor( coni.w[, c("BrPrec", "BrTavg", "B.Lat")], use = "complete.obs", method = "spearman"),2) 

?st_segmentize
?gcIntermediate
?set_units

# Plotting ----------------------------------------------------------------
p <- ggplot(data = ne_land2) + geom_sf() + theme(legend.position = "none") + theme(axis.title = element_blank()) 

BrWiConn <- vector("list", length(njana.w$ID))
for(i in 1:length(njana.w$ID)){
  BrWiConn[[i]] <- st_linestring(matrix(c(cbind(njana.w$B.Long[i], njana.w$W.Long[i]), cbind(njana.w$B.Lat[i], njana.w$W.Lat[i])), nrow = 2, ncol = 2))
}
names(njana.w)

njana.w %>% group_by(Species) %>% summarize(str8 = mean(str8line, na.rm = T), N = n())

BrWiConn <- st_sfc(BrWiConn, crs = 4326) %>% st_segmentize(units::set_units(100, km)) 
BrWiConnDf <- st_sf(ID = njana.w$ID, Band.Number = njana.w$Band.Number, Site.name = njana.w$Site.name, Species = njana.w$Species, geometry = BrWiConn)

#st.Tracks.tr <- st_transform(st.Tracks, crs.Mex)

p + geom_sf(data = BrWiConnDf, alpha = 0.4, aes(color = Species))
ggsave("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Results/Figures/BrWiBergMap.png", bg = "white")
p + geom_path(data = njana.w, aes(x = B.Long, y = B.Lat, group = ID), alpha = 0.4)

