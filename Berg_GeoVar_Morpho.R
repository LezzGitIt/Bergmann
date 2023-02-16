
#Clean up data
setwd("~/Desktop/Grad_School/Thesis/EWPW/Data_Whips/Analysis")
caps1720.comb <- read.csv(paste0(bs, "/Desktop/Grad_School/Thesis/EWPW/Data_Whips/Data_Sheets/2019_and_earlier/whip_data_AAS17-19wIL_all_columns.csv"), header = TRUE)

#How many tags were deployed? 
tags <- caps1720.comb[caps1720.comb$Geo.Trans != "GPS" & caps1720.comb$Geo.Trans != "VHF" & caps1720.comb$Geo.Trans != "9999" & caps1720.comb$Geo.Trans != "", ]
length(caps1720.comb[caps1720.comb$Geo.Trans == "GPS", ])
length(unique(tags$Geo.Trans))

length(unique(tags[tags$State == "MO" & tags$Year == "19", "Geo.Trans"]))
length(unique(tags[tags$State == "WI" & tags$Year == "19", "Geo.Trans"]))
length(unique(tags[tags$State == "OH" & tags$Year == "19", "Geo.Trans"]))
27 + 27 + 14 + 26 #26 deployed in IL. 94 TAGS DEPLOYED. 47 tags recaptured


# Attempt1, not best approach ---------------------------------------------

##Attempt to incorporate USGS data. No luck
USGS <- read.csv("/Users/Aaron/Desktop/Grad_School/Thesis/EWPW/Data_Whips/Public_Or_Borrowed_data/USGS/Morpho_Skinner_EWPW.csv")
names(USGS)[names(USGS) == 'BIRD_WEIGHT'] <- 'Mass'
names(USGS)[names(USGS) == 'TAIL_LENGTH'] <- 'Tail'
names(USGS)[names(USGS) == 'WING_CHORD'] <- 'Wing'
names(USGS)[names(USGS) == 'AGE_CODE'] <- 'Age'
names(USGS)[names(USGS) == 'REC_SOURCE'] <- 'Bander'
names(USGS)[names(USGS) == 'SEX_DETERMINED'] <- 'Sex'
names(USGS)[names(USGS) == 'LON_DECIMAL_DEGREES'] <- 'Long'
names(USGS)[names(USGS) == 'LAT_DECIMAL_DEGREES'] <- 'Lat'
names(USGS)[names(USGS) == 'BANDING_YEAR'] <- 'Year'

##All birds are aged AHY, and are thus unusable! 
USGS.red <- na.omit(USGS[,c("BAND_NUM" ,"Age","Sex", "Year", "Wing", "Tail", "Mass", "Long", "Lat", "Bander")])
USGS.red[ USGS.red$Sex != "F", ]

####
##Prep data. Better to just average data for birds w/ same age (see below)
library(dplyr)
caps1720.comb <- caps1720.comb[caps1720.comb$ID != "853" & caps1720.comb$ID != "858" & caps1720.comb$ID != "1864"  & caps1720.comb$ID != "1880" & caps1720.comb$ID != "1874" & caps1720.comb$ID != "34" & caps1720.comb$ID != "29",] #Remove the first year (no kipps index or wing photos) of individuals 137257443, 135292233, and 3 others that were recaptured in 2019
caps1720.comb <- caps1720.comb[caps1720.comb$Band.Size != "R",] #Changed "Band.Size" to "RK" for "recapture keep"" for the 2019 recaptures of the 9 individuals above, so they are maintained in the analysis

table(caps1720.comb$Sex) #Given that there are so few F birds, I'm choosing to remove them from the analysis. I will control for age as well, thus AHY birds are Unknown and can't be included.
#Remove Unknown age and F birds for this analysis
caps1720.comb <-  caps1720.comb[caps1720.comb$Sex != "F", ] 
caps1720.comb <- caps1720.comb[caps1720.comb$Age != "AHY", ]
nrow(caps1720.comb) #174 to 132 rows

##Remove all 2017 and 2018 birds. Certain wing morphology measurements weren't calculated before I arrived.
caps1720.comb.red <-caps1720.comb[caps1720.comb$Year == 19,] #Just 2019 birds
nrow(caps1720.comb.red)

#Remove individuals that were captured twice (recaps; I imagine this violates independence of samples for the PCA)

caps1720.comb<- filter(caps1720.comb, !is.na(Wing))
caps1720.comb<- filter(caps1720.comb, !is.na(Tail))
caps1720.comb<- filter(caps1720.comb, !is.na(Mass))
caps1720.comb<- caps1720.comb[caps1720.comb$Wing != 9999 & caps1720.comb$Tail != 9999 & caps1720.comb$Mass != 9999,]
nrow(caps1720.comb)

#caps1720.comb.red <- filter(caps1720.comb.red, !is.na(Tail))
#caps1720.comb.red <- filter(caps1720.comb.red, !is.na(Mass))
#caps1720.comb.red <- caps1720.comb.red[caps1720.comb.red$Wing != 9999 & caps1720.comb.red$Tail != 9999 & caps1720.comb.red$Mass != 9999,]
caps1720.comb.red <- filter(caps1720.comb.red, !is.na(Wing))
caps1720.comb.red <- caps1720.comb.red[caps1720.comb.red$Kipps != 9999,] 
caps1720.comb.red <- caps1720.comb.red[caps1720.comb.red$Wing != 9999,] 
caps1720.comb.red <- filter(caps1720.comb.red, !is.na(Kipps))
caps1720.comb.red <-  caps1720.comb.red[caps1720.comb.red$Sex != "F", ] 
caps1720.comb.red <- caps1720.comb.red[caps1720.comb.red$Age != "AHY", ]
nrow(caps1720.comb.red)

table(caps1720.comb.red$Age)
levels(caps1720.comb.red$Age) <- c("SY","SY", "ASY", "ASY", "SY") #Combine age classes

table(caps1720.comb$Age)
levels(caps1720.comb$Age) <- c("SY","SY", "ASY", "ASY", "SY") #Combine age classes
 

# Average morpho vars, Attempt 2 ------------------------------------------
##Prepare data frame
caps1720 <- read.csv("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Data_Whips/Data_Sheets/EWPW_caps_all_years.csv", header = TRUE)
nrow(caps1720)
names(caps1720)

names(caps1720)[names(caps1720) == 'Wing.Chord'] <- 'Wing'
names(caps1720)[names(caps1720) == 'Tail.Length'] <- 'Tail'
names(caps1720)[names(caps1720) == 'Kipp.s.index'] <- 'Kipps'
names(caps1720)[names(caps1720) == "Study..pilot.vs.full."] <- 'Study'
caps1720<- caps1720[, c("Band.Number", "Banding.Date", "Banding.Time", "Geo.Trans", "Study", "Dep.Year", "Age","State", "Location", "Sex", "Mass", "Tail","Wing", "Kipps", "B.Lat", "B.Long", "BrTempC")] #W.Lat doesn't predict pca.size (see mig.dist code)

#Create B.StSit variable
B.StSit <- vector(mode = "character", nrow(caps1720))
for(i in 1:nrow(caps1720)){
  if(caps1720$State[i] == "OH"){
    caps1720$B.StSit[i] <- as.character(caps1720$Location[i])
  }
  else{caps1720$B.StSit[i] <- as.character(caps1720$State[i])
  }
} 

library(naniar)
caps1720 <- replace_with_na(data = caps1720, replace = list(Wing = 9999, Mass = 9999, Tail = 9999, Kipps = 9999))
caps1720 <- caps1720[caps1720$Age != "AHY" & caps1720$Age != "9999", ]
table(caps1720$Age)
caps1720$Age <- as.factor(caps1720$Age)
levels(caps1720$Age) <- c("ASY","ASY", "SY") #Combine age classes

caps1720$Geo.Trans <- as.numeric(as.character(caps1720$Geo.Trans))
caps1720$uniq.ID <- paste(caps1720$Study, caps1720$Geo.Trans, sep = "-")

caps1720$Band.Age <- paste0(caps1720$Band.Number,"_", caps1720$Age)  #This was previously band number, and may work better? 

#Use band.age var to group and mutate to take the mean of morphometric for all individuals that are the same age, irrespective of capture year. 
library(dplyr)
caps1720_ <- group_by(caps1720, Band.Age) %>% mutate(Wing.comb = mean(Wing,na.rm = T), Tail.comb = mean(Tail, na.rm = T), Kipps.comb = mean(Kipps, na.rm = T),Mass.comb = mean(Mass, na.rm = T), .keep = "all")
caps1720_ <- as.data.frame(caps1720_)
caps1720.comb <- caps1720_[!duplicated(caps1720_$Band.Age), ]
caps1720.comb<- filter(caps1720.comb, !is.na(Wing.comb))
caps1720.comb<- filter(caps1720.comb, !is.na(Tail.comb))
caps1720.comb<- filter(caps1720.comb, !is.na(Mass.comb))
duplicated(caps1720.comb$uniq.ID) #Each Study-Band combo is not truly unique... This make sense for birds of different ages captured in the same study e.g. SY in 2019 and ASY 2020.
caps1720.comb[duplicated(caps1720.comb$uniq.ID), ]
nrow(caps1720.comb)
length(unique(caps1720.comb$Band.Number))




# Mass & time -------------------------------------------------------------
caps1720$Banding.Date <- as.POSIXct(as.character(caps1720$Banding.Date), "%m/%d/%y", tz="America/Chicago")
caps1720$month.day <- as.POSIXct(as.character(format(caps1720$Banding.Date, "%m/%d")), "%m/%d", tz = "America/Chicago") #Put everything in the same year (2020)

#This line of code deals with fact that we entered our data differently. You guys entered #### and I entered ##:##
band.time <- format(pmax(
  as.POSIXct(caps1720$Banding.Time, format="%H:%M", tz="America/Chicago"),
  as.POSIXct(caps1720$Banding.Time, format="%H%M", tz="America/Chicago"),
  na.rm=TRUE
), "%H:%M:%S")

#Option 1: try w/ chron. No luck
library(chron)
caps1720$band.time <- chron(times = band.time)
times(format(band.time, "%H:%M:%S"))
#Option2: POSIXct. This works, but graph doesn't look right
caps1720$Banding.Time <- as.POSIXct(band.time, format = "%H:%M")

caps1720$Dep.Year <- as.factor(caps1720$Dep.Year)
#Does mass vary w/ date, and time of day? If so, maybe combining doesn't make sense? Maybe WL doesn't make sense? 
library(nlme)
summary(lme(Mass~month.day, random = ~1|Dep.Year, na.action = na.omit, data = caps1720))
mass.mod <- lm(Mass~month.day * Dep.Year, data = caps1720)
time.mod <- lm(Mass~Banding.Time, data = caps1720)
summary(mass.mod)
anova(mass.mod)
summary(time.mod)

library(ggplot2)
ggplot(data=caps1720, aes(x = month.day, y = Mass, color = Dep.Year)) +
  geom_line(aes(group = Band.Age, alpha=.45)) + xlab("Month") + ylab("Mass (g)")

ggplot(data=caps1720, aes(x = Banding.Time, y = Mass)) + geom_point(aes(color = Dep.Year)) + geom_smooth(alpha = 1, method = "lm", se=FALSE, span=10, fill='gray', linetype=1, size=1) + scale_x_datetime(date_labels = "%H:%M", date_breaks = "6 hours") + xlab("Month") + ylab("Mass (g)") 


# PCA ---------------------------------------------------------------------
###Re-run the PCA from HW5 and add the first column to our dataframe###

#Can think about scaling by age sex combo.. Something like this
#if(caps1720.comb[caps1720.comb$Age == "SY" & caps1720.comb$Sex == "M"] , then caps1720.comb$mass.s <- scale(caps1720.comb$Mass))
# if(caps1720.comb[caps1720.comb$Age == "SY" & caps1720.comb$Sex == "M"] , then caps1720.comb$wing.s <- scale(caps1720.comb$Wing))

caps1720.comb <-  caps1720.comb[caps1720.comb$Sex != "F", ]

prcomp1<- prcomp(~ Tail.comb + Mass.comb + Wing.comb, center=T, scale=T, data=caps1720.comb)
caps1720.comb$pca.size <- prcomp1$x[,1]
biplot(prcomp1)

#round
caps1720.comb[,c(21:25)] <- lapply(caps1720.comb[,c(21:25)], round, 1) 


#WI data
prcomp1<-prcomp(~ Tail.comb + Mass.comb + Wing.comb, center=T, scale=T, data=No.WI)
No.WI$pca.size <- prcomp1$x[,1]
biplot(prcomp1)

#135292241 (SY to ASY), 135292239 (ASY to ASY), 135292233 (3 years data) are good examples to see if code worked. There are no SYs from 2017 or 2018 that are in 2019 and 2020.

subset(caps1720.comb, caps1720.comb$Band.Number == 135292245, select = c(Band.Age,uniq.ID, pca.size))
caps1720.comb[duplicated(caps1720.comb$uniq.ID), c("Band.Age","uniq.ID", "pca.size")]
subset(caps1720.comb, caps1720.comb$Geo.Trans == 1759) #Same tag two diff. individuals
 

# kipps -------------------------------------------------------------------
#Let's also create our Kipps index variable. Kipps is a wing measurement that gets at the pointedness of the wing. I have calculated kipps index as the residuals from a regression between Kipps and wing length (Carvalho et al, 2018), thus isolating wing shape from wing length.
 
caps1720.comb$Band.Number <- as.factor(caps1720.comb$Band.Number)
caps.kipps <- caps1720.comb[caps1720.comb$Kipps.comb != "NaN",]
Kipps.mod <- lme(Kipps.comb ~ Wing.comb, random = ~1|Band.Number, data = caps.kipps, na.action = na.omit)
length(caps.kipps$Kipps.comb)

library(ggplot2)
theme_bw()
ggplot(data=caps.kipps, aes(x = Wing.comb, y = Kipps.comb)) + 
geom_smooth(alpha = 1, method = "lm", se=FALSE, span=10, fill='gray', linetype=1, size=1, aes(color = Age)) +
geom_point(aes(color = Age), size =3, position = "jitter", alpha=.65) + xlab("Wing Length (mm)") + ylab("Kipps Length (mm)")

caps.kipps$Kipps.res <- resid(Kipps.mod)
caps.kipps$Kipps.div <- caps.kipps$Kipps / caps.kipps$Wing #Alternative method
 
# Data exploration -------------------------------------------------------
#Explanatory variables
boxplot(caps1720.comb$B.Lat,  ylab = "Breeding Latitude (degrees)")
boxplot(caps1720.comb$BrTempC,  ylab = "Average Temperature Apr - Oct (C)")
#Response variables
boxplot(caps1720.comb$pca.size, ylab = "PCA axis 1 (size)")
boxplot(caps.kipps$Kipps.res, ylab = "Kipps Index")
boxplot(caps1720.comb$Wing.Loading, ylab = "Wing Loading")
boxplot(caps1720.comb$Aspect.Ratio, ylab = "Aspect Ratio")

caps1720.comb %>% arrange(pca.size)
dotchart(caps1720.comb$Wing, groups = caps1720.comb$Age, 
         xlab = "Wing length (mm)", ylab = "Data ordered by body size (PC Axis 1)")
caps1720.comb %>% arrange(pca.size)
dotchart(caps1720.comb$Tail, xlab = "Tail length (mm)",
         ylab = "Data ordered by body size (PC Axis 1)")

#All the response vars look good (1 potential outlier for aspect ratio, 1 for kipps ratio). The two explanatory vars could be transformed to bring in the two outlier points in WI. Let's log transform temperature and latitude

caps1720.comb$log.temp <- log(caps1720.comb$BrTempC)
caps1720.comb$log.lat <- log(caps1720.comb$B.Lat)
#boxplot(caps1720.comb$log.Lat,  ylab = "Log Breeding Latitude (degrees)") #Doesn't plot
boxplot(caps1720.comb$log.temp,  ylab = "Log average Temperature Apr - Oct (C)")
 
#Given that log transforming temp had very little effect on the distribution of the variable, and there is still a large gap when plotted I decided against transforming my variables as this makes interpretation more difficult.

 
library(lattice)
bwplot(pca.size ~ Age | State, data = caps1720.comb,
       strip = strip.custom(bg = 'white'),  
       cex = .5, layout = c(3, 3),
       xlab = "Age", ylab = "Body size",
       par.settings = list(
         box.rectangle = list(col = 1),
         box.umbrella  = list(col = 1),
         plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))

#I won't plot all the response variables, but let's do the levene's test to see if var is equal
library(car)
with(caps1720.comb,leveneTest(pca.size~Age)) #Cannot reject the null hypothesis that the variance is equal across groups.
with(caps1720.comb,leveneTest(Aspect.Ratio~Age))
with(caps1720.comb,leveneTest(Wing.Loading~Age)) 
with(caps1720.comb,leveneTest(Kipps~Age)) 
 
#Seems that variances are likely OK for all response variables by age groups. 

xyplot(pca.size ~ BrTempC | B.StSit , data = caps1720.comb)
 
#Seems that variances are OK by site as well.


# Analysis ----------------------------------------------------------------
aov.size <- aov(pca.size ~ B.StSit + Age, data = caps1720.comb)
aov.WL <- aov(Wing.Loading ~ B.StSit + Age, data = caps1720.comb)
aov.AR <- aov(Aspect.Ratio ~ B.StSit + Age, data = caps1720.comb)
aov.K <- aov(Kipps.res ~ B.StSit + Age, data = caps.kipps)
summary(aov.size) #WI has bigger birds 
summary(aov.WL) #IL has highest wing loading, MO significantly different
summary(aov.AR) #IL Has lowest Aspect ratio, MO almost significantly different 
summary(aov.K)
cor(caps1720.comb.red$Kipps.res, caps1720.comb.red$Aspect.Ratio, use = "complete.obs")
cor(caps1720.comb.red$Kipps.div, caps1720.comb.red$Aspect.Ratio, use = "complete.obs")
cor(caps1720.comb.red$Wing.Loading, caps1720.comb.red$Aspect.Ratio, use = "complete.obs") 

TukeyHSD(aov.size) #WI has bigger birds 
TukeyHSD(aov.WL)
TukeyHSD(aov.AR)
TukeyHSD(aov.K)

ggplot(data=caps.kipps, aes(x = Wing.comb, y = Kipps.comb)) + 
  geom_smooth(alpha = 1, method = "lm", se=TRUE, span=10, fill='gray', linetype=1, size=1) +
  geom_point(aes(color = State), size =3, position = "jitter", alpha=.65) + xlab("Wing Length (mm)") + ylab("Kipps Length (mm)")
 

1. standardize by age sex class (12 lines of code if then statements), OR JUST F. Scale() function both centers and standardizes
2. Then stick these measurements into PCA 
3. Try w/ age and without age. At this point, you'd hope age wouldn't be significant
4. log transform temperature. Body size scales on a log scale w/ metabolism, and metabolism 
 
library(nlme)
cor(caps1720.comb$BrTempC, caps1720.comb$B.Lat, use = "complete.obs") #Highly negatively correlated

summary(age.mod)
summary(temp.mod)
summary(temp.up)
summary(lat.mod)
summary(lat.up)

library(ggplot2)
library(cowplot)
ggplot2::theme_set(theme_cowplot())

No.WI <- caps1720.comb[caps1720.comb$State != "WI",]
No.WI.SY <- caps1720.comb[!(caps1720.comb$Age == "SY" & caps1720.comb$State == "WI"),]

ggplot(data=caps1720.comb, aes(x = BrTempC, y = pca.size)) + 
  geom_smooth(aes(color = Age), alpha = 1, method = "lm", se=TRUE, span=10, fill='gray', linetype=1, size=1) +
  geom_point(aes(shape = B.StSit, color = Age), size =3, position = "jitter", alpha=.65) + xlab("Breeding Temperature (C)") + ylab("Body Size (PCA axis 1)")
ggsave("/Users/Aaron/Desktop/Grad_School/Thesis/EWPW/Data_Whips/Analysis/2020Analyses/figures/Berg_Rule.png")
 

#Consistent w/ Zuur book ch5, first step is determine random effects structure. An individual bird may be in model twice as a SY and again as ASY , so including individual should be important to avoid pseudoreplication 
 
library(nlme)

temp.mod.noRE <- gls(pca.size~BrTempC * Age, method = "REML", data = caps1720.comb, na.action = na.omit)
temp.interact <- lme(pca.size~BrTempC * Age, random = ~1|Band.Number, data = caps1720.comb, method = "REML", na.action = na.omit) 
AIC(temp.mod.noRE, temp.interact) #Individual RE is important 

#Berg throughout annual cycle. Ran this model already for the 51 birds w/ W.Lat and found no significance. Need temp as well as much more elevational differences on wintering grounds
summary(lme(pca.size ~ W.Lat + Age, random = ~1|Band.Number, data = caps1720.comb, method = "REML", na.action = na.omit))

null <- lme(pca.size ~ 1, random = ~1|Band.Number, data = No.WI, method = "REML", na.action = na.omit) 
age.mod <- lme(pca.size ~ Age, random = ~1|Band.Number, data = No.WI, method = "REML", na.action = na.omit) 
temp.age <- lme(pca.size ~ BrTempC + Age, random = ~1|Band.Number, data = caps1720.comb, method = "REML", na.action = na.omit)
lat.age <- lme(pca.size ~ B.Lat + Age, random = ~1|Band.Number, data = No.WI, method = "REML", na.action = na.omit)
lat.interact <- lme(pca.size ~ B.Lat * Age, random = ~1|Band.Number, data = No.WI, method = "REML", na.action = na.omit)

##Was having lots of crazy patterns in residual vs fitted plot.. Then took out RE of individual and those crazy patterns went away. This can happen if you try to do too much w/ your data (only had about 10-20 individuals in the model twice (different ages))
temp.age.noRE <- lm(pca.size ~ BrTempC + Age, data = caps1720.comb)
plot(temp.age.noRE)
summary(temp.age.noRE)

AIC(null, age.mod, temp.age, lat.age, lat.interact, temp.interact) #Model selection w/ RE's according to Zuur book Ch5
?AIC

summary(temp.age)
 
#Temperature is a better predictor than latitude, and the best model is the model w/ temperature and age but no interaction! 

##Check residuals against explanatory vars
plot(subset(caps1720.comb, caps1720.comb$BrTempC != "NA")$BrTempC, resid(temp.age), xlab = "Temp", ylab = "Residuals")
plot(subset(caps1720.comb, caps1720.comb$BrTempC != "NA")$Age, resid(temp.age), xlab = "Age", ylab = "Residuals")

dev.new()
par(mar = c(0, 0, 0, 0))
E2 <- resid(temp.age, type = "normalized")
F2 <- fitted(temp.age)
MyYlab <- "Residuals"
plot(x = F2, y = E2, xlab = "Fitted values", ylab = MyYlab) 
boxplot(E2 ~ subset(caps1720.comb, caps1720.comb$BrTempC != "NA")$State, data = caps1720.comb, main = "Breeding State", ylab = MyYlab)
boxplot(E2 ~ subset(caps1720.comb, caps1720.comb$BrTempC != "NA")$Age, data = caps1720.comb, main = "Age", ylab = MyYlab)

#Out of curiousity, I wanted to see how temperature and age would perform when explaining the second PC axis, which was mostly composed of mass.
 
mod.t.mass <- lm(prcomp1$x[,2] ~ BrTempC + Age, data = caps1720.comb)
summary(mod.t.mass)
plot(caps1720.comb$BrTempC, prcomp1$x[,2], col = caps1720.comb$Location)
abline(mod.t.mass)
 
#As you can see age is not nearly as important anymore, but breeding temperature remains important.
#I ran the same models as above and maintained the same explanatory variables 
 
temp.mod.K <- lm(Kipps.res~BrTempC * Age, data = caps1720.comb.red)
lat.mod.K <-lm(Kipps.res~B.Lat * Age, data = caps1720.comb.red)

summary(temp.mod.K)

temp.mod.WL <- lm(Wing.Loading~ BrTempC + Age, data = caps1720.comb) 
temp.mod.AR <- lm(Aspect.Ratio~ BrTempC + Age, data = caps1720.comb) 
 

Model validation 
#Let's check that our best models have normal residuals (ideally at each x value)
 
hist(resid(Kipps.mod))
hist(resid(temp.mod))
hist(resid(lat.mod))

boxplot(resid(temp.mod) ~ caps1720.comb$Sex, varwidth = TRUE)
boxplot(resid(temp.mod) ~ caps1720.comb$Age, varwidth = TRUE)
 

Compare Wing Aspect Ratio, and Wing loading between years
#ADD AGE, Tail.Length, Kipps, wing length to 2020 file, need to compare Kipps, etc. 
 
setwd("/Users/Aaron/Desktop/Grad_School/Thesis/EWPW/Data_Whips/Data_Sheets")
Wing19 <- read.csv("whip_data_AAS17-19wIL_all_columns.csv")
all_data <- read.csv("2020/All_birds2020.csv")
#all_data <- all_data[all_data$Year == 19,]
nrow(all_data)
names(all_data)

setwd("/Users/Aaron/Desktop/Grad_School/Thesis/EWPW/Data_Whips/Wing Photos")
Wing20 <- read.csv("Wing_Photos 2020-4.csv")
names(Wing19)

Wing19 <- Wing19[Wing19$Year == "19",]
Wing19<- Wing19[, c("Band.Number", "Age", "Waypoint.ID", "Slant", "Wing.Loading", "Aspect.Ratio","Mass", "Tail.Length", "Kipp.s.index", "Wing.Chord")]
Wing20<- Wing20[, c("Band.Number","Band.Size","Location","Waypoint.ID","X2...WA.Avg", "Slant", "Wing.Loading", "Aspect.Ratio","Mass", "Tail.Length", "Kipp.s.index", "Wing.Chord")] #X2...WA.Avg = 2 * WA

caps1720.comb.merg <- merge(x = Wing19 ,y = Wing20, by= c( "Band.Number" )) #Not sure if the c() is necessary for the by command, but this all works well. all = TRUE includes the other rows in BOTH data frames that don't have a shared entry for the by() command. E.g. let's say a band number is only in caps1720.comb x, but not in y, the all.x or all = TRUE will make this row appear in the final caps1720.comb.

View(caps1720.comb.merg)
#write.csv(caps1720.comb.merg,file = "/Users/Aaron/Desktop/Grad_School/Thesis/EWPW/Data_Whips/Wing Photos/Compare_Wing_1920.csv", row.names = TRUE)

setwd("/Users/Aaron/Desktop/Grad_School/Thesis/EWPW/Data_Whips/Wing Photos")
Wing1920 <- read.csv("Compare_Wing_1920.csv")
Wing1920 <- Wing1920[Wing1920$Band.Number != 137228551 & Wing1920$Band.Number != 137228569 & Wing1920$Band.Number != 265153301,] #Remove SY and AHY birds, and 265153301 was noted as poor photo in 2019


t.test(Wing1920$Wing.Loading.x , Wing1920$Wing.Loading.y , paired = TRUE, alternative = "two.sided") #Two-sided is default
t.test(Wing1920$Aspect.ratio , Wing1920$Aspect.Ratio, paired = TRUE, alternative = "two.sided") 

Wing1920 %>%
summarize(avg19 = mean(Aspect.Ratio), sd = sd(Aspect.Ratio), N = n()) 
Wing1920%>%
summarize(avg20 = mean(Aspect.ratio), sd = sd(Aspect.ratio), N = n())
Wing1920%>%
summarize(avg19 = mean(Wing.Loading.x-Wing.Loading.y), sd = sd(Wing.Loading.x - Wing.Loading.y), N = n())
Wing1920%>%
summarize(avg20 = mean(Wing.Loading.y), sd = sd(Wing.Loading.y), N = n()) 
 