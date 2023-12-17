#load("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/R_Files/MS/BergJIC.Rdata")
load("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/R_Files/MS/BergAnalysis9.25.22.Rdata")

?head

# NEXT STEPS --------------------------------------------------------------

##BT.comb -> Filter out individuals with improbable banding times, calculate tsss by correct timezone, then combine same individuals tsss (instead of by bandtime). COMPLETE :)
#Determine nuisance vars to test, establish rationale for month.day and year COMPLETE :)
#Recalculate environmental variables (in a loop) w/ the correct months for each species COMPLETE :)
#Review emails about framing of paper, read Alicia's version of paper COMPLETE :)

#TAKEAWAY: We're going to stick with our model sets, including the productivity model. Can run a post-hoc model to see support for the 'homeostasis' model (Ex. in methods: 'although this did not align with our a-priori model structure framework, we ran an additional post-hoc model with temp + precip as there is also substantial support for the combinatory effects of temp & fluid regulation'). We can run all models together in code b/c we know that all species will adhere to Bergmann's rule (objective 1), but will likely want to present results differently in the paper (e.g. could separate out geography model), and later compare best environmental model to geography model w/ likelihood ratio test or something). Or insert best geography model into the mix with ALL other environmental models
##Had previously had error in row_number, take another look at that and then link up with Elly's Envi variables. Also rerun correlations and make sure everything makes sense!
#Adjust code to include the nuisance variables we're controlling for including poly(tsss, 2)
#Adjust code to be a single round of model selection instead of 2 rounds 
#SAVE TO GIT - TIDY CODE UP AT THE END (BE LIBERAL DELETING) - RESAVE TO GIT AFTER 
#Restart R and rerun everything to ensure it is self-contained. Will want to reorganize some things 

# Stomach -----------------------------------------------------------------

stomach <- read.csv("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing_Exit_Seminar/Bergs_Rule/Analysis/Bergmann/EvensLathouwers_data_sheet_corrected_stomach.csv")
nrow(stomach)

stomach$Banding.Time <- sapply(str_split(parse_date_time(stomach[,c("Banding.Time")], c("HMS"), truncated = 3), " "), function(x){x[2]})
stomach$Banding.Time <- chron(times = stomach$Banding.Time)
stomach <- stomach %>% mutate(Banding.Time = ifelse(Banding.Time < .5, Banding.Time + 1, Banding.Time))

#This is a pretty cool plot to include in manuscript as a supplementary figure, although would want to remember what the stomach score represents, and would want to convert to tsss 
Sys.setenv(TZ='GMT')
stomach %>% ggplot(aes(x = Banding.Time, y = Stomach)) + geom_jitter(width = .05, alpha = .3) + geom_smooth() + scale_x_chron(format="%H:%M")

summary(lm(Stomach ~ Banding.Time, data = stomach))
with(stomach, cor.test(Banding.Time, Stomach))


# Prep loops --------------------------------------------------------------
loop <- expand.grid(Species = c("CONI", "EWPW", "EUNI"), DV = c("Wing.comb", "Mass.combBT"), Season = c("Breed", "Winter"), Hypothesis = c("Geo", "TR", "Prod", "Seas"))
loop2 <- expand.grid(Species = c("CONI", "EWPW", "EUNI"), DV = c("Wing.comb", "Mass.combBT"), Season = "NA", Hypothesis = "Mig.Dist")
loop <- rbind(loop,loop2)
loop <- arrange(loop, Species, DV, Hypothesis)
loopSppDV <- expand.grid(Species = c("CONI", "EWPW", "EUNI"), DV = c("Wing.comb", "Mass.combBT")) #Model Selection Round 2
loopSppDV <- arrange(loopSppDV, Species, DV)
 
njdf.list <- list(ewpw.w, ewpw.m, coni.w, coni.m, euni.w, euni.m)
names(njdf.list) <- c("EWPW.Wing.comb", "EWPW.Mass.combBT", "CONI.Wing.comb", "CONI.Mass.combBT", "EUNI.Wing.comb", "EUNI.Mass.combBT")
njdf.list.ns <- njdf.list

#Scale numeric variables using select_if()
njdf.list.num <- njdf.list.s <- list()
for(i in 1:length(njdf.list)){
  njdf.list.num[[i]] <- njdf.list[[i]] %>% select_if(function(x){is.numeric(x)}) 
  njdf.list.s[[i]] <- scale(subset(njdf.list.num[[i]], select = -c(Band.Number, rowID, Year)))
  njdf.list[[i]] <- cbind(njdf.list[[i]][,c("Band.Number", "rowID", "Year", "Age", "Sex")] , njdf.list.s[[i]])
}

table(is.na(df$Wing.Chord))


# Nuisance variables by spp -----------------------------------------------
#I think most robust way to test this for EUNI would be to add poly(euni.w$Year, 3) as new columns onto data frame then somehow ensure that if 3 is included, so are 2 and 1, and so on. Alternatively, add poly(2) and poly(1) but ensure that poly(1,2,or3) are never in the same model. Don't think it really matters though since I did post-hoc check and poly(3) is best model for EUNI.

njdf.list.age <- lapply(njdf.list, function(x){x[x$Age != "Unk",]})
lapply(njdf.list.age, nrow) #CONI only has 50 individuals that are aged. Age is not in top model for Wing or Mass (w/ the 50 bird df), so let's leave in all individuals and remove Age from model
lapply(njdf.list, nrow) 
globNuis <- drgNuis <- candNuis <- aictabNuis <- sumTM <- TM <- resid.plots <- list()
for(i in 1:nrow(loopSppDV)){ 
  print(paste("i =", i))
  if(loopSppDV[i,1] == "EUNI" | loopSppDV[i,1] == "EWPW"){
  df <- njdf.list.age[paste0(loopSppDV[i,1], ".", loopSppDV[i,2])][[1]]
  globNuis[[i]] <- lm(as.formula(paste(loopSppDV[i,2],"~", c("B.Lat + Age + Sex"))), na.action = "na.fail", data = df) #Top mod for EUNI Mass includes poly(2), poly(3),
  }
  if(loopSppDV[i,1] == "CONI"){#Overwrite Nuis global; include Unk age birds for CONI (via njdf.list)
    df <- njdf.list[paste0(loopSppDV[i,1], ".", loopSppDV[i,2])][[1]]
    globNuis[[i]] <- lm(as.formula(paste(loopSppDV[i,2],"~", c("B.Lat + Sex"))), na.action = "na.fail", data = df) #Remove age from model
  }
  if(loopSppDV[i,2] == "Mass.combBT"){
    df <- df %>% filter(!is.na(tsss.comb))
    globNuis[[i]] <- update(globNuis[[i]], ~. + poly(tsss.comb,2)) #Add tsss.comb to model
  }
  drgNuis[[i]] <- dredge(globNuis[[i]], m.lim = c(0,6))
  candNuis[[i]] <- get.models(object = drgNuis[[i]], subset = T)
  NamesNuis <- sapply(candNuis[[i]], function(x){paste(x$call)}[2]) #Why +1?
  aictabNuis[[i]] <- aictab(cand.set = candNuis[[i]], modnames = NamesNuis, sort = TRUE)
  TM[[i]] <- lm(as.formula(aictabNuis[[i]]$Modnames[1]), na.action = "na.fail", data = df) #Top model
  sumTM[[i]] <- summary(TM[[i]])  #Summary of the top model
  #resid.plots[[i]] <- plot(TM[[i]], which = 1, main = paste(loopSppDV[i,1], loopSppDV[i,2]))
  ##Residual plots by Age & Sex, they all look resonable
  boxplot(resid(TM[[i]]) ~ Sex, data = df, main = paste(loopSppDV[i,1], "Heterogeneity Sex"), ylab = "Residuals")
  if(loopSppDV[i,1] == "EWPW" | loopSppDV[i,1] == "EUNI"){
    boxplot(resid(TM[[i]]) ~ Age, data = df, main = paste(loopSppDV[i,1], "Heterogeneity Age"), ylab = "Residuals")
  }
}

names(aictabNuis) <- paste0(loopSppDV[,1], ".", loopSppDV[,2])
#Notice all species are the same, Age (not included in CONI models) + Sex for wing chord, and Age + Sex + tsss for mass
lapply(aictabNuis, slice_head, n = 5)
names(sumTM) <- paste0(loopSppDV[,1], ".", loopSppDV[,2])
sumTM


NuisVarsModSelect <- bind_rows(lapply(aictabNuis, slice_head, n = 5))
write.csv(NuisVarsModSelect, "NuisVarsModSelect.csv")

#Age & sex in majority of top models w/ full df, but extremely unbalanced sample sizes for Age & Sex in FAC birds. Had looked at spread of residuals in EWPW age but couldn't even do this for majority of these nuisance variables.
capri.fac %>% group_by(Species) %>% count(Age)
capri.fac %>% group_by(Species) %>% count(Sex)
capri.fac %>% group_by(Species) %>% count(Year) #EUNI 2010 - 2021

euniFAC <- capri.fac[capri.fac$Species == "EUNI",]

HypVars <- vector("list", length = 5)
HypVars[[1]] <- c("Lat", "Long", "Elev") #Geography
HypVars[[2]] <- c("Srad", "Tavg") #Temp Regulation (TR)
HypVars[[3]] <- c("EVI", "Prec") #Productivity
HypVars[[4]] <- c("CVevi", "PrecCV", "Tcv") #Seasonality
HypVars[[5]] <- "str8line"
names(HypVars) <- c("Geo", "TR", "Prod", "Seas", "Mig.Dist")


# Round 1 Model Selection -------------------------------------------------

cormat <- TF.cormat <- globHyp <- drgHyp <- cand.mods <- vif <- aic.tab <- vector("list", length = nrow(loop))
1:nrow(loop)


for(i in 1:nrow(loop)){ #
  print(paste("i =", i))
  df <- njdf.list[paste0(loop[i,1], ".", loop[i,2])][[1]]
  xcols <- df[,c("Mass", "Wing.Chord", "Age", "str8line")] #X for extra
  df <- df[, substr(names(df), 1, 1) == substr(loop[i,3], 1, 1)] #Select columns for the season in question (winter or breeding)
  df <- cbind(df, xcols)
  Vars <- dplyr::select(df, matches(HypVars[[loop[i,4]]], ignore.case = F)) #PrecCV
  VarsVect <- names(Vars)
  
cormat[[i]] <- round(cor(Vars, use = "complete.obs", method = "spearman"), 2)
TF.cormat[[i]] <- apply(cormat[[i]], 2, function(x){ifelse(x < .7 & x > -.7, T, F)})
TF.cormat[[i]][upper.tri(TF.cormat[[i]], diag = T)] <- NA
if(loop[i,4] == "Mig.Dist"){
  TF.cormat[[i]] <- matrix(data = TRUE)
  rownames(TF.cormat[[i]]) <- "str8line"
  colnames(TF.cormat[[i]]) <- "str8line"
}

predictors <- paste(VarsVect, collapse = "+")
if(loop[i,1] == "EWPW"){
 globHyp[[i]] <- lm(as.formula(paste(loop[i,2], "~", predictors, "+ Age")), na.action = "na.fail", data = df)
 drgHyp[[i]] <- dredge(globHyp[[i]], subset = TF.cormat[[i]], m.lim = c(0,3), fixed = "Age")
} else{
globHyp[[i]] <- lm(as.formula(paste(loop[i,2],"~", predictors)), na.action = "na.fail", data = df) 
drgHyp[[i]] <- dredge(globHyp[[i]], subset = TF.cormat[[i]], m.lim = c(0,3)) #  fixed = "Age"
}
cand.mods[[i]] <- get.models(object = drgHyp[[i]], subset = T)

modNames <- vector("character", length(cand.mods[[i]]))
#vif <- rep(NA, length(cand.mods[[i]]))
modForm <- mods <- vector("list", length(cand.mods[[i]]))
for(p in 1:length(cand.mods[[i]])){
  modForm[[p]] <- formula(cand.mods[[i]][[p]])
  print(paste("p =", p))
  min <- simpleCap(as.character(modForm[[p]])[3])
  if(loop[i,1] == "EWPW"){
  min1 <- str_remove(as.character(min), pattern = '\\+ 1 ') #minus "+1"
  } 
  else{
    min1 <- str_remove(as.character(min), pattern = '\\+ 1') #minus "+1"
  }
  modNames[p] <- paste(as.character(modForm[[p]])[2], as.character(modForm[[p]])[1], min1)
  mods[[p]] <- lm(formula = modForm[[p]], data = df)
  if(length(coef(mods[[p]])) > 2 & loop[i,1] != "EWPW"){
  vif[[i]][[p]] <- max(car::vif(mods[[p]]))
  }
  if(loop[i,1] == "EWPW" & length(coef(mods[[p]])) > 3){
    vif[[i]][[p]] <- max(car::vif(mods[[p]])[,1])
  }
}
aic.tab[[i]] <- aictab(cand.set = mods, modnames = modNames, sort = TRUE)
}

max(unlist(vif)) #[[3]][[5]] problematic
sort(unlist(vif))
cand.mods[[3]][[5]]
cand.mods[[12]][[8]]
names(aic.tab) <- apply(loop, 1 , paste , collapse = "-" )
aic.tab[1:9]
aic.tab[9:18]


# Round 2 Model Selection -------------------------------------------------

#CHANGE NAME FROM loopMS2 TO loopSppDV

TM <- aic.tabMS2 <- parm.set <- ImpMods <- num.mods <- bpm <- lrtest <- vector("list", length = nrow(loopMS2)) #TM = Top models, ImpMods = #Important models
p <- -8:0
for(i in 1:6){ # 1:nrow(loopMS2)
  print(paste("i =", i))
 p <- p+9
 #Grab the model w/in DeltaAIC < 2 and the fewest parameters
 num.mods[[i]] <- sum(sapply(aic.tab[p], nrow)-1)
  TM[[i]] <- lapply(aic.tab[p], function(x){filter(x, Delta_AICc < 2) %>% slice_min(K) %>% slice_min(AICc)}[[1]])
 #TM[[i]] <- lapply(aic.tab[p], function(x){x[1,1]}) #This grabs first model

 df <- njdf.list[paste0(loopMS2[i,1], ".", loopMS2[i,2])][[1]]
 df.ns <- njdf.list.ns[paste0(loopMS2[i,1], ".", loopMS2[i,2])][[1]] #not scaled
 colnames(df)[31] <- "Str8line"
 colnames(df.ns)[31] <- "Str8line"
Mnames <- Hyp <- Seas <- vector("character", length(TM[[i]]))
cand.set <- cand.set.ns <- parms <- vector("list", length = length(TM[[i]]))
for(z in 1:length(TM[[i]])){
  print(paste("z =", z))
  cand.set[[z]] <- lm(formula = TM[[i]][[z]], na.action = "na.fail", data = df)
  cand.set.ns[[z]] <- lm(formula = TM[[i]][[z]], na.action = "na.fail", data = df.ns)
}
for(d in 1:length(TM[[i]])){
   Mnames[d] <- TM[[i]][[d]]
   Hyp[d] <- strsplit(names(TM[[i]]), split = "-")[[d]][4]
   Seas[d] <- strsplit(names(TM[[i]]), split = "-")[[d]][3]
   
   if(length(coef(cand.set[[d]])) > 1 & loopMS2[i,1] != "EWPW" | loopMS2[i,1] == "EWPW" & length(coef(cand.set[[d]])) > 3){
   Modavg <- vector("list", length(coef(cand.set[[d]]))-1)
   if(loopMS2[i,1] == "EWPW"){
     Modavg <- vector("list", length(coef(cand.set[[d]]))-3)
   }
   Modavg[[1]] <- modavg(cand.set, parm = names(coef(cand.set[[d]]))[2], modname = Mnames, conf.level = .95)
   parm1 <- names(coef(cand.set[[d]]))[2]
   beta <- Modavg[[1]]$Mod.avg.beta
   LCI <- round(Modavg[[1]]$Lower.CL,2)
   UCI <- round(Modavg[[1]]$Upper.CL, 2)
   if(length(coef(cand.set[[d]])) > 2 & loopMS2[i,1] != "EWPW" | loopMS2[i,1] == "EWPW" & length(coef(cand.set[[d]])) > 4){
  Modavg[[2]] <- modavg(cand.set, parm = names(coef(cand.set[[d]]))[3], modname = Mnames, conf.level = .95)
    parm2 <- names(coef(cand.set[[d]]))[3]
     beta <- sapply(Modavg, function(x){x$Mod.avg.beta})
     LCI <- round(sapply(Modavg, function(x){x$Lower.CL}),2)
     UCI <- round(sapply(Modavg, function(x){x$Upper.CL}),2)
     parm1 <- rbind(parm1,parm2)
   }
   #Create parameter estimates table
   Sig <- ifelse(LCI < 0 & UCI > 0, "N", "Y")
   parms[[d]] <- data.frame(Mnames[d], parm1, Seas[d], Hyp[d], beta, LCI, UCI, Sig)
   }
}
  aic.tabMS2[[i]] <- aictab(cand.set = cand.set, modnames = Mnames, sort = F)
  SeasHypEstSE <- data.frame(Seas, Hyp)
  aic.tabMS2[[i]] <- cbind(aic.tabMS2[[i]], SeasHypEstSE)
  aic.tabMS2[[i]] <- arrange(aic.tabMS2[[i]], AICc)
  aic.tabMS2[[i]][3:7] <- lapply(aic.tabMS2[[i]][3:7], round, 2) 
  bpmn <- filter(aic.tabMS2[[i]], Delta_AICc < 2) %>% slice_min(K) %>% slice_min(AICc) %>% select(Modnames) #best parsimonious model name
  bpm[[i]] <- lm(formula = bpmn$Modnames, na.action = "na.fail", data = df)
  null <- lm(formula = aic.tabMS2[[i]]$Modnames[8], na.action = "na.fail", data = df)
  lrtest[[i]] <- lmtest::lrtest(bpm[[i]], null) #Likelihood ratio test to examine goodness of fit 
  parm.set[[i]] <- bind_rows(parms, .id = NULL)
  ImpMods[[i]] <- cand.set.ns
}

names(lrtest) <- apply(loopMS2, 1 , paste , collapse = "-" )
names(parm.set) <- apply(loopMS2, 1 , paste , collapse = "-" )
names(aic.tabMS2) <- apply(loopMS2, 1 , paste , collapse = "-" )
ImpMods <- unlist(ImpMods, recursive = F) #Important mods
unlist(TM, recursive = F)


# Export tables -----------------------------------------------------------
aic.tabMS2 <- lapply(aic.tabMS2, distinct, Modnames, .keep_all = T) #Rm rep ~1 rows
aicTabs <- list(aic.tab, aic.tabMS2)
names(aicTabs) <- c("aic.tabMS1", "aic.tabMS2")
for(i in 1:length(aicTabs)){
  for(p in 1:length(aicTabs[[i]])){
    print(paste("i =", i))
    print(paste("p =", p))
    aicTabs[[i]][[p]]$Modnames <- str_remove(aicTabs[[i]][[p]]$Modnames, pattern = '\\+ Age')
    aicTabs[[i]][[p]]$Modnames <- sapply(strsplit(aicTabs[[i]][[p]]$Modnames, split = "~ "), function(x){x[2]})
    if(i == 1){
      print(paste("p =", "WEEN"))
    aicTabs[[i]][[p]] <- data.frame(add_row(aicTabs[[i]][[p]], Modnames = apply(loop[p,], 1, paste, collapse = "-" ), .before = 1))
    names(aicTabs[[i]][[p]]) <- c("Modnames", "K", "AICc", "ΔAICc", "ModelLik", "wt", "-2 Log-likelihood", "Cum.Wt")
    aicTabs[[i]][[p]] <- subset(aicTabs[[i]][[p]], select = -Cum.Wt)
    }
  }
  if(i == 2){
    for(z in 1:nrow(loopMS2)){
      print(paste("z =", z))
      aicTabs[[i]][[z]] <- add_row(aicTabs[[i]][[z]], Modnames = apply(loopMS2[z,], 1, paste, collapse = "-" ), .before = 1)
    }
  }
  aicTabs[[i]] <- bind_rows(aicTabs[[i]], .id = NULL)
  aicTabs[[i]]$Modnames <- str_replace_all(as.character(aicTabs[[i]]$Modnames), pattern = c("B.Lat" = "Latitude", "B.Long" = "Longitude", "BrTavg" = "Temperature",  "WiPrec" = "Precipitation",  "BrPrec" = "Precipitation", "BrTcv" = "TemperatureCV", "WiTcv" = "TemperatureCV", "BrCVevi" = "EVI CV", "WiCVevi" = "EVI CV", "BrEVI" = "EVI", "WiEVI" = "EVI", "WiTavg" = "Temperature", "BrSrad" = "Solar Radiation",  "WiMDR" = "Mean Daily Range", "WiSrad" = "Solar Radiation", "W.Long" = "Longitude", "W.Lat" = "Latitude", "BrMDR" = "Mean Daily Range", "BreedElev" = "Elevation", "WinterElev" = "Elevation", "Str8line" = "Migratory Distance"))
  aicTabs[[i]] <- subset(aicTabs[[i]], select = -c(ModelLik))
  aicTabs[[i]][3:6] <- round(aicTabs[[i]][3:6], 2)
  aicTabs[[i]][is.na(aicTabs[[i]])] <- " "
  if(i == 2){
    names(aicTabs[[i]]) <- c("Model", "K", "AICc", "ΔAICc", "wt", "-2 Log-likelihood", "Season", "Hypothesis")
    aicTabs[[i]]$Hypothesis <- str_replace_all(as.character(aicTabs[[i]]$Hypothesis), pattern = c("Geo"= "Geography", "Mig.Dist" = "Migratory Distance", "TR" = "Temperature Regulation", "Seas" = "Seasonality", "Prod" = "Productivity"))
    aicTabs[[i]]$Season <- ifelse(aicTabs[[i]]$Model == 1, "", aicTabs[[i]]$Season)
    aicTabs[[i]]$Hypothesis <- ifelse(aicTabs[[i]]$Model == 1, "", aicTabs[[i]]$Hypothesis)
  }
  #write.table(aicTabs[[i]], file = paste0("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Results/Tables/", names(aicTabs)[i], ".txt"), sep = ",", quote = FALSE, row.names = F)
}

#Remove column AICc, add footer, wt to wi (italicize and subscript), change 1 to Null (model)
write.table(aic.tabMS2, file = "/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Results/Tables/aic.tabMS2.txt", sep = ",", quote = FALSE, row.names = F)
?write.table

  

# Residual plots ----------------------------------------------------------


sum(sapply(ImpMods, function(x){length(coef(x))}) > 1)
pdf(file = "/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Results/Figures/Model_assumption_plots/3leverage_resid_age.pdf", width = 8.5, height = 5, bg = "white")
for(i in 1:length(ImpMods)){
  if(length(coef(ImpMods[[i]])) > 1 & i %in% c(1:18, 37:54) | i %in% c(19:36) & length(coef(ImpMods[[i]])) > 3){
    name <- as.character(formula(ImpMods[[i]]))
    plot(ImpMods[[i]], which = 5, main = paste(loop[i,1], ":", name[2], name[1], name[3])) #which = 1,2,3,5
  }
}
dev.off()

# Parm estimate plots -----------------------------------------------------
dfcoefs <- bind_rows(parm.set, .id = "Species")
dfcoefs$DV <- sapply(strsplit(dfcoefs[,1], split = '-'), function(x){x[2]})
dfcoefs[,1] <- sapply(strsplit(dfcoefs[,1], split = '-'), function(x){x[1]})
rownames(dfcoefs) <- NULL

##Order df by hypothesis, then change IV from class = character to a factor
names(dfcoefs)[3] <- "IV"
dfcoefs <- arrange(dfcoefs, Hyp.d.)

dfcoefs$IV <- factor(dfcoefs$IV, levels = c("B.Lat", "B.Long", "BreedElev",   "BrPrec", "WiPrec",  "BrTcv", "BrCVevi", "WiTcv",  "BrTavg", "WiMDR", "WiSrad", "Str8line")) 
arrange(dfcoefs, abs(beta))
subset(dfcoefs,select= -c(Mnames.d.))

#Add asterisk above models where TopMod == "Y"
imp.modnames <- sapply(aic.tabMS2, function(x){filter(x, Delta_AICc < 2) %>% slice_min(K) %>% slice_min(AICc)}[[1]])[1:4] #Delt AIC < 2 OR 
imp.modnames <- unlist(sapply(aic.tabMS2, function(x){filter(x, Delta_AICc < 4 & K > 2)[,c("Modnames")]})) #Delta AIC < 4, K > 2 to exclude null models
dfcoefs$TopMod <- ifelse(dfcoefs$Mnames.d. %in% imp.modnames, "Y", "N")

#scale_x_discrete(name = "Hypothesis",  labels = c("Geography", "Migratory \nDistance", "Productivity", "Seasonality","Temperature \nRegulation"))
#geom_text(data = filter(dfcoefs, TopMod == "Y" & DV == "Mass"), aes(group = interaction(Species, IV), y = UCI + .05, label = "*"), size = 6, color = "black", position = position_dodge(width = 0.85))
#If you want to group things by hypothesis, can do group = interaction(Hyp.d., IV, Species)
#DV = Wing
ggplot(data = filter(dfcoefs, DV == "Wing.Chord"), aes(x = IV, y = beta, color = Species, group = interaction(IV, Species))) + geom_point(size = 6, position = position_dodge(width = 0.85)) + geom_errorbar(aes(ymin=LCI, ymax=UCI), width=0, size = 1.5, position = position_dodge(width = 0.85)) + scale_y_continuous(name = expression(paste("Parameter estimate (", beta, ")"))) + scale_x_discrete(labels = c("Breeding \nLatitude", "Breeding \nElevation", "Breeding \nPrecipitation", "Winter \nPrecipitation", "Breeding \nTemperature CV", "Breeding \nTemperature", "Winter \nMDR", "Winter \nSolar Radiation", "Migratory \nDistance")) + theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 12), axis.text.x = element_text(size = 12, vjust = .58, angle = 60), legend.title = element_text(size=14), legend.text = element_text(size=12), plot.margin = margin(10,10,8,25)) + geom_hline(yintercept = 0, linetype = "dashed", size = 1) + scale_color_discrete(name = "Species", labels = c("CONI", "EWPW"))

#DV = Mass
ggplot(data = filter(dfcoefs, DV == "Mass"), aes(x = IV, y = beta, color = Species)) + geom_point(aes(group = Species), size = 6, position = position_dodge(width = 0.75)) + geom_errorbar(aes(ymin=LCI, ymax=UCI), width=0, size = 1.5, position = position_dodge(width = 0.75)) + scale_x_discrete(labels = c("Breeding \nLatitude", "Breeding \nLongitude", "Breeding \nPrecipitation", "Winter \nPrecipitation", "Breeding \nTemperature CV", "Breeding \nEVI CV", "Winter \nTemperature CV", "Breeding \nTemperature", "Migratory \nDistance")) + scale_y_continuous(name = expression(paste("Parameter estimate (", beta, ")")))  + theme(axis.title.x = element_blank(), axis.title.y = element_text(size = 12), axis.text.x = element_text(size = 12, vjust = .58, angle = 60), legend.title = element_text(size=14), legend.text = element_text(size=12), plot.margin = margin(10,10,8,25)) + geom_hline(yintercept = 0, linetype = "dashed", size = 1) + scale_color_discrete(name = "Species", labels = c("CONI", "EWPW"))

ggsave("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Results/Figures/ParmEst/ParmEstMass.png", bg = "white")

# Predict function --------------------------------------------------------
summary(lm(formula = aic.tabMS2[[i]]$Modnames[1], data = njdf.list[[1]]))

##Mtcars example
data(mtcars)
mtcars$cyl <- factor(mtcars$cyl)
lm <- lm(mpg ~ disp + hp + cyl, data = mtcars)
newCars <- data.frame(disp = seq(min(mtcars$disp), max(mtcars$disp), length.out = 32), hp = mean(mtcars$hp), cyl = factor(mtcars$cyl, levels = c(4,6,8))) #hold cyl at the mean
# Predicts the future values
ndf <- predict(lm, newdata = newCars, interval = 'confidence')
ndf <- cbind(newCars, ndf) #need to bind to the simulated data, not the raw data

ggplot() +
  geom_line(data=ndf, aes(x=disp, y=fit)) + #show your mean prediction
  geom_ribbon(data=ndf, aes(x=disp, ymin=lwr, ymax=upr), alpha=0.5) + #show your predicted CIs
  geom_point(data=mtcars, aes(x=disp, y=mpg)) #show the raw data


###FULL LOOP TO RUN ALL PLOTS. SEE BELOW FOR FINAL PLOTS 
which(sapply(ImpMods, function(x){length(coef(x))}) == 1) #Check # of covs in each model. Notice the repeat models w/ BrTavg cause # to be different than aic.tabMS2$Modnames
plots <- ndf <-  PredVars <- vector("list", length = length(ImpMods))
plots <- list()

njdf.list.ns$CONI.Wing.Chord$W.Lat

sapply(ImpMods, function(x){length(coef(x))})[19:36] #EWPW models
IVs2 <- which(sapply(ImpMods, function(x){length(coef(x))}) == 2)
pdf(file = "/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Results/Figures/plots3.pdf", width = 8.5, height = 5, bg = "white")
for(i in c(4,14,28,34)){ #IVs2 c(4,14,28,34) 1:length(ImpMods)  
  print(i)
  df <- njdf.list.ns[paste0(loop[i,1], ".", loop[i,2])][[1]]
  ndf[[i]] <- cbind(dplyr::select(df, loop[i, "DV"]), dplyr::select(df, matches(names(ImpMods[[i]]$coefficients))))
if(length(coef(ImpMods[[i]])) == 2){
  names(ndf[[i]])[2] <- str_replace_all(names(ndf[[i]])[2], pattern = c("B.Lat" = "Latitude", "B.Long" = "Longitude", "BrTavg" = "Temperature",  "WiPrec" = "Precipitation",  "BrPrec" = "Precipitation", "BrTcv" = "TemperatureCV", "WiTcv" = "TemperatureCV", "BrCVevi" = "EVI CV",  "WiMDR" = "Mean Daily Range", "WiSrad" = "Solar Radiation", "str8line" = "Migratory Distance", "Wing.Chord" = "Wing Chord"))
  
plots[[i]] <- ggplot(data = ndf[[i]], aes(x = ndf[[i]][,2], y = ndf[[i]][,1])) + geom_point() + geom_smooth(method = "lm", se = TRUE) + ggtitle(paste0(loop[i,1],".", loop[i,3], ".", loop[i,4])) + xlab(names(ndf[[i]])[2]) + ylab(names(ndf[[i]][1]))
print(plots[[i]])

#ggsave(paste0("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Results/Figures/Plots/", apply(loop[i,], 1 , paste , collapse = "_"),".png"), bg = "white")
}

  newdat <- vector("list", length = 2)
if(length(coef(ImpMods[[i]])) > 2){
  PredVars <- dplyr::select(df, matches(names(ImpMods[[i]]$coefficients)))
  for(z in 1:2){
    print(paste("z =", z))
    if(z == 1){
  newdat[[z]] <- data.frame(iv1seq = seq(min(PredVars[,1]), max(PredVars[,1]), length.out = length(PredVars[,1])), iv2mn = mean(PredVars[,2])) #hold an IV at the mean
    }
  else{
    newdat[[z]] <- data.frame(iv1mn = mean(PredVars[,1]), iv2seq = seq(min(PredVars[,2]), max(PredVars[,2]), length.out = length(PredVars[,2])))
  }
  # Predicts the future values
  names(newdat[[z]]) <- names(PredVars)
  ndf[[i]] <- predict(ImpMods[[i]], newdata = newdat[[z]], interval = 'confidence')
  IVsDV <- cbind(dplyr::select(df, loop[i, "DV"]), PredVars, ndf[[i]]) #need to bind to the simulated data, not the raw data
  names(IVsDV) <- str_replace_all(names(IVsDV), pattern = c("B.Lat" = "Latitude", "B.Long" = "Longitude", "BrTavg" = "Temperature",  "WiPrec" = "Precipitation",  "BrPrec" = "Precipitation", "BrTcv" = "TemperatureCV", "WiTcv" = "TemperatureCV", "BrCVevi" = "EVI CV",  "WiMDR" = "Mean Daily Range", "WiSrad" = "Solar Radiation", "str8line" = "Migratory Distance", "Wing.Chord" = "Wing Chord"))
  ndf[[i]] <- cbind(IVsDV, newdat[[z]])
  #names(newdat) <- c(paste0(names(PredVars), c(".seq", ".mn")), paste0(names(PredVars), c(".mn", ".seq")))
  print(i)
  if(z == 1){
plots[[i]] <- ggplot() +
  geom_line(data=ndf[[i]], aes(x= ndf[[i]][,7], y=fit)) + #show your mean prediction
  geom_ribbon(data=ndf[[i]], aes(x= ndf[[i]][,7], ymin=lwr, ymax=upr), alpha=0.5) + #show your predicted CIs
  geom_point(data=ndf[[i]], aes(x = ndf[[i]][,2], y = ndf[[i]][,1])) + xlab(names(ndf[[i]])[2]) + ylab(names(ndf[[i]])[1]) + ggtitle(paste0(loop[i,1],".", loop[i,3], ".", loop[i,4]))  #show the raw data
print(plots[[i]])
}
if(z == 2){
plots[[i]] <- ggplot() +
  geom_line(data=ndf[[i]], aes(x= ndf[[i]][,8], y=fit)) + #show your mean prediction
  geom_ribbon(data=ndf[[i]], aes(x= ndf[[i]][,8], ymin=lwr, ymax=upr), alpha=0.5) + #show your predicted CIs
  geom_point(data=ndf[[i]], aes(x = ndf[[i]][,3], y = ndf[[i]][,1])) + xlab(names(ndf[[i]])[3]) + ylab(names(ndf[[i]])[1]) + ggtitle(paste0(loop[i,1],".", loop[i,3], ".", loop[i,4]))
  print(plots[[i]])
#+ theme(legend.position="none", plot.title = element_text(size = 8, face = "plain", hjust = 0.5))
}
  #ggsave(paste0("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Results/Figures/Plots/2IVs/", apply(loop[i,], 1 , paste , collapse = "_"), paste0("IV",z), ".png"), bg = "white")
}
}
}
dev.off()



bs.mod <- lmerTest::lmer(Wing.Chord ~ BreedElev + (1| Project), data = njdf.list$EWPW.Wing.Chord)
summary(bs.mod)

matrix(c(1,4,7,3), c(1,4,7,3)) 
s<-matrix(1:25,5)
s[lower.tri(s)] = t(s)[lower.tri(s)]
cor(s)

##Attempt 2


top.mods <- plots <- vector("list", length = 6) #6 figures in the end
topModNames <- lapply(aic.tabMS2, function(x){filter(x, Delta_AICc < 2) %>% slice_min(K) %>% slice_min(AICc)}[[1]])[1:4]

####PLOTS THE FINAL PLOTS IN A SINGLE PDF, NEVER FIGURED OUT HOW TO COMBINE INTO A SINGLE FIGURE
pdf(file = "/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Results/Figures/Final_plots.pdf", width = 8.5, height = 5, bg = "white")
p <- 0
par(mfrow = c(2,3))
for(i in 1:4){ #IVs2 c(4,14,28,34) 1:length(ImpMods)  
    print(paste("i = ", i))
  print(paste0("p =", p))
    df <- njdf.list.ns[paste0(loopMS2[i,1], ".", loopMS2[i,2])][[1]]
    top.mods[[i]] <- lm(as.formula(topModNames[[i]]), na.action = "na.fail", data = df)
    ndf[[i]] <- cbind(dplyr::select(df, loopMS2[i, "DV"]), dplyr::select(df, c(matches(names(top.mods[[i]]$coefficients)), "Age")))
    if(length(coef(top.mods[[i]])) == 2 & loopMS2[i,1] != "EWPW" | length(coef(top.mods[[i]])) == 4 & loopMS2[i,1] == "EWPW"){
      names(ndf[[i]])[2] <- str_replace_all(names(ndf[[i]])[2], pattern = c("B.Lat" = "Latitude", "B.Long" = "Longitude", "BrTavg" = "Temperature",  "WiPrec" = "Precipitation",  "BrPrec" = "Precipitation", "BrTcv" = "TemperatureCV", "WiTcv" = "TemperatureCV", "BrCVevi" = "EVI CV",  "WiMDR" = "Mean Daily Range", "WiSrad" = "Solar Radiation", "str8line" = "Migratory Distance", "Wing.Chord" = "Wing Chord"))
      p <- p + 1
      plots[[i]] <- ggplot(data = ndf[[i]], aes(x = ndf[[i]][,2], y = ndf[[i]][,1])) + geom_point() + geom_smooth(method = "lm", se = TRUE) + ggtitle(paste0(loopMS2[i,1])) + xlab(names(ndf[[i]])[2]) + ylab(names(ndf[[i]][1]))
      print(plots[[i]])
    }
newdat <- vector("list", length = 2)
if(length(coef(top.mods[[i]])) > 2 & loopMS2[i,1] != "EWPW" | loopMS2[i,1] == "EWPW" & length(coef(top.mods[[i]])) > 4){
    PredVars <- dplyr::select(df, c(matches(names(top.mods[[i]]$coefficients)), "Age"))
    for(z in 1:2){
      print(paste("z =", z))
      if(z == 1){
        newdat[[z]] <- data.frame(iv1seq = seq(min(PredVars[,1]), max(PredVars[,1]), length.out = length(PredVars[,1])), iv2mn = mean(PredVars[,2]), iv3 = factor(PredVars[,3], levels = c("Adult", "Unk", "Young"))) #hold an IV at the mean
      }
      else{
        newdat[[z]] <- data.frame(iv1mn = mean(PredVars[,1]), iv2seq = seq(min(PredVars[,2]), max(PredVars[,2]), length.out = length(PredVars[,2])), iv3 = factor(PredVars[,3], levels = c("Adult", "Unk", "Young")))
      }
      # Predicts the future values
      names(newdat[[z]]) <- names(PredVars)
      ndf[[i]] <- predict(top.mods[[i]], newdata = newdat[[z]], interval = 'confidence')
      IVsDV <- cbind(dplyr::select(df, loopMS2[i, "DV"]), PredVars, ndf[[i]]) #bind to the simulated data, not the raw data
      names(IVsDV) <- str_replace_all(names(IVsDV), pattern = c("B.Lat" = "Latitude", "B.Long" = "Longitude", "BrTavg" = "Temperature",  "WiPrec" = "Precipitation",  "BrPrec" = "Precipitation", "BrTcv" = "TemperatureCV", "WiTcv" = "TemperatureCV", "BrCVevi" = "EVI CV",  "WiMDR" = "Mean Daily Range", "WiSrad" = "Solar Radiation", "str8line" = "Migratory Distance", "Wing.Chord" = "Wing Chord"))
      ndf[[i]] <- cbind(IVsDV, newdat[[z]])
      ndf[[i]] <- select(ndf[[i]], subset = -c(Age))
      #names(newdat) <- c(paste0(names(PredVars), c(".seq", ".mn")), paste0(names(PredVars), c(".mn", ".seq")))
      print(i)
      if(z == 1){
        p <- p + 1
        print(paste0("p =", p))
        plots[[i]] <- ggplot() +
          geom_line(data=ndf[[i]], aes(x= ndf[[i]][,7], y=fit)) + #show your mean prediction
          geom_ribbon(data=ndf[[i]], aes(x= ndf[[i]][,7], ymin=lwr, ymax=upr), alpha=0.5) + #show your predicted CIs
          geom_point(data=ndf[[i]], aes(x = ndf[[i]][,2], y = ndf[[i]][,1])) + xlab(names(ndf[[i]])[2]) + ylab(names(ndf[[i]])[1]) + ggtitle(paste0(loopMS2[i,1]))  #show the raw data
        print(plots[[i]])
      }
      if(z == 2){
        p <- p + 1
        print(paste0("p =", p))
        plots[[i]] <- ggplot() +
          geom_line(data=ndf[[i]], aes(x= ndf[[i]][,8], y=fit)) + #show your mean prediction
          geom_ribbon(data=ndf[[i]], aes(x= ndf[[i]][,8], ymin=lwr, ymax=upr), alpha=0.5) + #show your predicted CIs
          geom_point(data=ndf[[i]], aes(x = ndf[[i]][,3], y = ndf[[i]][,1])) + xlab(names(ndf[[i]])[3]) + ylab(names(ndf[[i]])[1]) + ggtitle(paste0(loopMS2[i,1]))
        print(plots[[i]])
      }
    }
}
}
dev.off()

#See the following stack overflow: https://stackoverflow.com/questions/12234248/printing-multiple-ggplots-into-a-single-pdf-multiple-plots-per-page

for(i in 1:4){
do.call("grid.arrange", plots[[i]])
}

do.call("grid.arrange", c(plotsArr[W2ind], ncol = 3))

p <- plots[[1]]
q <- plots[[2]]
ggarrange(p, q,
          ncol = 2, nrow = 1,
          align='hv', labels = c("C","D"),
          legend = "none")
# legend = "right")



# Descriptive -------------------------------------------------------------
# >>Allometry scaling -----------------------------------------------------

scaling.plots <- vector("list", 3)
corWM <- rep(NA,3)
xy <- data.frame(x = c(4.3, NA, 3.8, NA, 4.2), y = c(5.4, NA, 5.15, NA, 5.34))
for(i in c(1,3,5)){
  df.ns <- njdf.list.ns[paste0(loopMS2[i,1], ".", loopMS2[i,2])][[1]]
  #Taking the log of the two variables should linearize the relationship
  df.ns[,c("Wing.Chord", "Mass")] <- lapply(df.ns[,c("Wing.Chord", "Mass")], log)
  corWM[i] <- cor(df.ns$Wing.Chord, df.ns$Mass, use = "complete.obs")
  scaling.plots[[i]] <- ggplot(data = df.ns, aes(x = Mass, y = Wing.Chord)) + geom_point() + geom_smooth(method = "lm", se = TRUE) + xlab("Log Mass") + ylab("Log Wing Chord") + ggtitle(paste(loopMS2[i,1])) + annotate("text", x = xy[i,1], y = xy[i,2], label = paste("r =", round(corWM[i],2)), color = "grey22", size = 5)
}
corWM

pdf(file = "/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Results/Figures/Allometry.pdf", width = 8.5, height = 5, bg = "white")
do.call("grid.arrange", c(scaling.plots[c(1,3,5)], ncol = 2))
dev.off()

###SMI -- scaled mass index 
library(smatr)
vignette(package = "smatr") 

#SMA is standard in studies of allometry. This youtube video explains what's going on very well, and the difference between OLS regression. Go to SMA vs OLS section of video, particularly around 9:05 and see subsequent plots. In SMA trying to minimize the distance to the line in both the X and Y axes (instead of just the Y axis). This different method to estimate residuals results in different lines of best fit. https://www.youtube.com/watch?v=dvXEcYYnask&ab_channel=MethodsinExperimentalEcologyI
df <- njdf.list.ns$EWPW.Mass
df <- njdf.list.ns$CONI.Mass
df <- njdf.list.ns$EUNI.Mass

#DELETE?
mod.rm1 <- sma(Mass ~ Wing.Chord, data = df, log = "xy")
mod.rm1 <- sma(Mass ~ Wing.Chord, data = df)
mod.rm1
plot(mod.rm1, log = "xy")

#Notice if you add the argument method = "OLS" (same as standard lm() function), the slope changes to 0.1, but R^2 is the same. 
modWC <- sma(Wing.Chord ~ Mass, data = df, log = "xy") #default method is SMA
modOLS <- sma(Wing.Chord ~ Mass, data = df, log = "xy", method = "OLS") 
summary(lm(log(Wing.Chord) ~ log(Mass), data = df))
#Notice Peig & Green (2009) note that bSMA = bOLS / r, which is essentially (but not exactly) true here, not sure why.
coef(modOLS)[2] / cor(df$Wing.Chord, df$Mass, use = "complete.obs")

 
modWC
coef(modWC)[[2]] #Near .3 in EWPW! 
par(mfrow=c(1,3))
modWC <- sma(Wing.Chord ~ Mass, data = df, log = "xy")
plot(modWC, log = "xy", main = "Whip-poor-will") #scaling coefficient = 0.37
plot(modWC, log = "xy", main = "Nighthawk") #scaling coefficient = -0.46
plot(modWC, log = "xy", main = "European Nightjar") #scaling coefficient = 0.29

ggarrange(pEwpw, pConi)

?sma
?plot.sma #Notice line doesn't extend all the way through points, and "from" & "to" arguments control this (default is min and max of the fitted values from sma)
#Check assumptions
plot(mod.rm1, which = "res")
plot(mod.rm1, which = "qq")

##Additional functionality
#Can use to determine if mass and wing scale isometrically. If log = "xy" then I think slope.test should be equal to 0.33. See Warton 2012 paper for more information
sma(Wing.Chord ~ Mass, data = df, slope.test = 1) #Can add * Age
modWCage <- sma(Wing.Chord ~ Mass * Age, data = df)
modWCage <- sma(Wing.Chord ~ Mass + Age, data = df)

plot(modWCage) #Notice the * or + fixes either the intercept (but slopes vary), or the slope.


###This example is pulled from smatr package documentation I believe 
# Load leaf lifetime dataset:
data(leaflife)

### One sample analyses ###
# Extract only low-nutrient, low-rainfall data:
leaf.low <- subset(leaflife, soilp == 'low' & rain == 'low')

# Fit a MA for log(leaf longevity) vs log(leaf mass per area):
ma(longev ~ lma, log='xy', data=leaflife)

# Test if the MA slope is not significantly different from 1:
ma.test <- ma(longev ~ lma, log='xy', slope.test=1, data=leaflife)
summary(ma.test)

# Construct a residual plot to check assumptions:
plot(ma.test,type="residual")

### Several sample analyses ###

# Now consider low-nutrient sites (high and low rainfall):
leaf.low.soilp <- subset(leaflife, soilp == 'low')

# Fit SMA's separately at each of high and low rainfall sites,
# and test for common slope:
com.test <- sma(longev~lma*rain, log="xy", data=leaf.low.soilp)
com.test

# Plot longevity vs LMA separately for each group:
plot(com.test)

# Fit SMA's separately at each of high and low rainfall sites,
# and test if there is a common slope equal to 1:
sma(longev~lma*rain, log="xy", slope.test=1, data=leaf.low.soilp)

# Fit SMA's with common slope across each of high and low rainfall sites, 
# and test for common elevation:
sma(longev~lma+rain, log="xy", data=leaf.low.soilp)

# Fit SMA's with common slope across each of high and low rainfall sites, 
# and test for no shift along common SMA:
sma(longev~lma+rain, log="xy", type="shift", data=leaf.low.soilp)

# >>General ---------------------------------------------------------------
unlist(num.mods) #Number of mods per spp.DV combination
sapply(njdf.list, nrow) #Sample size per spp.DV combination
sum(sapply(aic.tab, nrow)-1) #230 models total not including nulls, 268 w/ elevation
#Rename & format for final table
range(sapply(aic.tab, nrow)) #2 to 8 models per model set

#How many of final models are breeding vs winter & which hypotheses? 
lapply(aic.tabMS2, filter, Delta_AICc < 4 & K > 2)
table(unlist(lapply(aic.tabMS2, function(x){filter(x, Delta_AICc < 4 & K > 2)[,c("Hyp")]})))
lapply(aic.tabMS2, function(x){table(filter(x, Delta_AICc < 4 & K > 2)[,c("Seas")])})


#Interesting that elevation is important for EWPW wing, and in the direction that's not predicted
data.frame(ewpw.w %>% group_by(Project, Site.name) %>% summarize(mn.elev = mean(BreedElev), lat = mean(B.Lat))) %>% arrange(lat)

#Goal: Understand whether Berg's rule was more important in nighthawks or whip-poor-wills. This is something similar to effect size but clearly different! 
ttest <- dfcoefs %>% mutate(eff.size = abs(beta/(LCI-UCI)))
t.test(filter(ttest, Species == "CONI")$eff.size, filter(ttest, Species == "EWPW")$eff.size)

###Descriptive plotting
#Boxplot by showing latitudinal range in winter & breeding by species 
#Can try to add in the iqr to the right of each graph (think landcover plot in OA manuscript w/ ylabpos, and group argument (including interaction) to get this to work)
njdf %>% dplyr::select(Species, B.Lat, W.Lat) %>% pivot_longer(cols = c(B.Lat, W.Lat), names_to = "Season", values_to = "Lat") %>% group_by(Species, Season) %>% mutate(iqr = IQR(Lat)) %>% ggplot(aes(x = Species, y = Lat, color = Season)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width = .3), alpha=0.3) + scale_x_discrete(labels = c("Common \nnighthawk", "European \nnightjar", "Eastern \nwhip-poor-will")) + scale_color_discrete(labels=c('Breeding', 'Winter')) + xlab(NULL) + ylab("Latitude")  #+ geom_jitter(width=0.1,alpha=0.2) #+ geom_point(position=position_jitterdodge(),alpha=0.3)

ggsave("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/MS/EWPW/Writing:Exit_Seminar/Bergs_Rule/Results/Figures/BrWi_Lat_dist.png", bg = "white")

njdf %>% dplyr::select(Species, B.Lat, W.Lat) %>% tidyr::pivot_longer(cols = c(B.Lat, W.Lat), names_to = "Season", values_to = "Lat") %>% group_by(Species, Season) %>% summarize(iqr = IQR(Lat), range = max(Lat) - min(Lat))

#Understand correlations between predictor variables
cor.test(njdf.list$EWPW.Wing.Chord$B.Long, njdf.list$EWPW.Wing.Chord$BrPrec)
cor.test(njdf.list$EWPW.Wing.Chord$B.Long, njdf.list$EWPW.Wing.Chord$BreedElev)
cor.test(njdf.list$EWPW.Wing.Chord$BreedElev, njdf.list$EWPW.Wing.Chord$BrPrec)
cor.test(njdf.list$EWPW.Wing.Chord$BreedElev, njdf.list$EWPW.Wing.Chord$BrTavg)
range(ewpw.m$WinterElev)

#Correlations between latitude and migratory distance
lapply(njdf.list, nrow) #Mass always larger sample size 
cor.test((njdf$B.Lat - njdf$W.Lat), njdf$str8line)
cor.test(njdf.list$EUNI.Mass$B.Lat, njdf.list$EUNI.Mass$str8line)
cor.test(njdf.list$CONI.Mass$B.Lat, njdf.list$CONI.Mass$str8line)
cor.test(njdf.list$EWPW.Mass$B.Lat, njdf.list$EWPW.Mass$str8line)

filter(dfcoefs, IV == "BreedElev")



# >>post-hoc  --------------------------------------------------------------

#EWPW
modBest <- lm(Wing.Chord ~ B.Lat + BreedElev + Age , data = njdf.list$EWPW.Wing.Chord)
mod1 <- lm(Wing.Chord ~ BrPrec + BrTavg + Age , data = njdf.list$EWPW.Wing.Chord)
modList <- list(modBest, mod1)
aictab(modList)

modBest <- lm(Mass ~ B.Lat + B.Long + Age , data = njdf.list$EWPW.Mass)
mod1 <- lm(Mass ~ BrPrec + BrTavg + Age , data = njdf.list$EWPW.Mass)
modList <- list(modBest, mod1)
aictab(modList)

#CONI
modBest <- lm(Wing.Chord ~ B.Lat , data = njdf.list$CONI.Wing.Chord)
mod1 <- lm(Wing.Chord ~ BrPrec + BrTavg, data = njdf.list$CONI.Wing.Chord)
modList <- list(modBest, mod1)
aictab(modList)

modBest <- lm(Mass ~ BrTcv, data = njdf.list$CONI.Mass)
mod1 <- lm(Mass ~ BrPrec + BrTavg , data = njdf.list$CONI.Mass)
modList <- list(modBest, mod1)
aictab(modList)

summary(mod1)




#EUNI
modBest <- lm(Mass ~ 1, data = njdf.list$EUNI.Mass)
mod1 <- lm(Mass ~ BrPrec + BrTavg , data = njdf.list$EUNI.Mass)
modList <- list(modBest, mod1)
aictab(modList)

modBest <- lm(Mass ~ 1, data = njdf.list$EUNI.Mass)
mod1 <- lm(Mass ~ BrPrec + BrTavg , data = njdf.list$EUNI.Mass)
modList <- list(modBest, mod1)
aictab(modList)

#save.image("/Users/aaronskinner/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Grad_School/R_Files/MS/BergAnalysis9.25.22.Rdata") #Godamit saved w/ all the isotopes / HR analysis as well. NEXT TIME YOU RUN EVERYTHING FROM SCRATCH BE SURE TO SAVE THIS AGAIN W/ JUST BERG'S RULE
