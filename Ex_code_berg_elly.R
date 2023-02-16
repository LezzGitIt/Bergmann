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
cormatGeoW <- round(cor(ewpw.w[, c("W.Lat", "W.Long", "elev.Winter")], use = "complete.obs", method = "spearman"),2) #GeoWgraphy
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
