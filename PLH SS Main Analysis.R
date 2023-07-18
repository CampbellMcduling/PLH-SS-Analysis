#PLH SS - Main Analysis
# Campbell McDuling
rm(list = ls())
library(readxl)
setwd("~/OneDrive - University of Cape Town/2022/UNI/Project/South Sudan/PLH survey data (South Sudan)")

#read in data
dat.join <- read.csv("dat.join.csv")
dat.join <- dat.join[,-1]

#================================ Wrangling for GLME =============================================
### wrangling data into long form
library(dplyr)
#subset data - only need composite scores and demographic data
#pre demographics, pre summed scores, post demo, post summed scores 
dat.model <- cbind(dat.join[,1:17], dat.join[,86:91], dat.join[,45:58], dat.join[,92:97], dat.join[,98:107], dat.join[, 165:168], dat.join[, 132:140], dat.join[, 169:172])

#add time indicator
dat.model$Time <- rep(0, dim(dat.model)[1])
dat.model <- dat.model %>% relocate(`Time`, .before = S.N.x)
Time1 <- dat.model$Time+1
dat.model <- cbind(dat.model, Time1)
dat.model <- dat.model %>% relocate(`Time1`, .before = S.N.y)

Time0 <- dat.model$Time
dat.model <- cbind(dat.model, Time0)
dat.model <- dat.model %>% relocate(`Time0`, .before = S.N)
Time1.1 <- Time1
dat.model <- cbind(dat.model, Time1.1)
dat.model <- dat.model %>% relocate(`Time1.1`, .before = Dem_AgeT.y)
names(dat.model)[names(dat.model) == 'Time0'] <- 'Time'
names(dat.model)[names(dat.model) == 'Time1'] <- 'Time'
names(dat.model)[names(dat.model) == 'Time1.1'] <- 'Time'

#subset into pre and post
dat.modelPRE <- cbind(dat.model[,1:24], dat.model[,47:60])
dat.modelPOST <- cbind(dat.model[,25:45], dat.model[,62:74])

#add missing columns to post
dat.modelPOST$HH.UIC <- dat.modelPRE$HH.UIC
dat.modelPOST <- dat.modelPOST %>% relocate(`HH.UIC`, .after = S.N.y)
dat.modelPOST$Dem_Relationship <- dat.modelPRE$Dem_Relationship
dat.modelPOST <- dat.modelPOST %>% relocate(`Dem_Relationship`, .after = Dem_EducationT.y.x)
dat.modelPOST$S.N <- dat.modelPRE$S.N
dat.modelPOST <- dat.modelPOST %>% relocate(S.N, .before = Dem_AgeT.y)
dat.modelPOST$Dem_GenderCG <- dat.modelPRE$Dem_GenderCG
dat.modelPOST <- dat.modelPOST %>% relocate(Dem_GenderCG, .after = Dem_AgeCG.y)
dat.modelPRE <- dat.modelPRE %>% relocate(S.N.x, .after = HH.UIC)
dat.modelPOST <- dat.modelPOST %>% relocate(S.N.y, .after = HH.UIC)

#make varnames identical
names(dat.modelPRE)[names(dat.modelPRE) == 'S.N.x'] <- 'S.N.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_AgeCG.x'] <- 'Dem_Age.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_GenderCG'] <- 'Dem_Gender.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_ChildAgeCG.x'] <- 'Dem_ChildAge.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_ChildGenderCG.x'] <- 'Dem_ChildGender.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_EducationT.x.x'] <- 'Dem_EducationT.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_Father.x.x'] <- 'Dem_Father.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_Mother.x.x'] <- 'Dem_Mother.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_Poverty.x'] <- 'Dem_Poverty.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_Illness_adult.x'] <- 'Dem_Illness_adult.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_TB_HIV.x.x'] <- 'Dem_TB_HIV.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_Alcohol.x.x'] <- 'Dem_Alcohol.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_Fighting.x.x'] <- 'Dem_Fighting.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_Illness_child.x'] <- 'Dem_Illness_child.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_Disability_child.x'] <- 'Dem_Disability_child.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Summed_PRE_APQ.x'] <- 'APQ.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Summed_PRE_SDQ.x'] <- 'SDQ.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Summed_PRE_ICAST.x'] <- 'ICAST.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Summed_PRE_CESD'] <- 'CESD.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Summed_PRE_FFC'] <- 'FFC.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Summed_PRE_PSE.x'] <- 'PSE.CG'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_AgeT.x'] <- 'Dem_Age.T'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_ReadT.x'] <- 'Dem_Read.T'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_EducationT.x.y'] <- 'Dem_EducationT.T'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_ChildrenT.x'] <- 'Dem_Children.T'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_Father.x.y'] <- 'Dem_Father.T'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_Mother.x.y'] <- 'Dem_Mother.T'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_TB_HIV.x.y'] <- 'Dem_TB_HIV.T'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_Alcohol.x.y'] <- 'Dem_Alcohol.T'
names(dat.modelPRE)[names(dat.modelPRE) == 'Dem_Fighting.x.y'] <- 'Dem_Fighting.T'
names(dat.modelPRE)[names(dat.modelPRE) == 'Summed_PRE_APQ.y'] <- 'APQ.T'
names(dat.modelPRE)[names(dat.modelPRE) == 'Summed_PRE_SDQ.y'] <- 'SDQ.T'
names(dat.modelPRE)[names(dat.modelPRE) == 'Summed_PRE_ICAST.y'] <- 'ICAST.T'
names(dat.modelPRE)[names(dat.modelPRE) == 'Summed_PRE_PSE.y'] <- 'PSE.T'

colnames(dat.modelPOST) <- c("Time", "HH.UIC", "S.N.CG", "Dem_Age.CG", "Dem_Gender.CG", "Dem_ChildAge.CG",
                             "Dem_ChildGender.CG", "Dem_EducationT.CG", "Dem_Relationship", "Dem_Father.CG", 
                             "Dem_Mother.CG", "Dem_Poverty.CG", "Dem_Illness_adult.CG" , "Dem_TB_HIV.CG" , "Dem_Alcohol.CG",
                             "Dem_Fighting.CG", "Dem_Illness_child.CG", "Dem_Disability_child.CG", "APQ.CG","SDQ.CG","ICAST.CG",
                             "CESD.CG","FFC.CG","PSE.CG","S.N","Dem_Age.T","Dem_Read.T","Dem_EducationT.T","Dem_Children.T","Dem_Father.T",
                             "Dem_Mother.T","Dem_TB_HIV.T","Dem_Alcohol.T","Dem_Fighting.T","APQ.T","SDQ.T", "ICAST.T","PSE.T")
names(dat.modelPRE)==names(dat.modelPOST)

#make relevant variables factors
#PRE
dat.modelPRE$Dem_Alcohol.CG <- as.factor(dat.modelPRE$Dem_Alcohol.CG)
dat.modelPRE$Dem_Alcohol.T <- as.factor(dat.modelPRE$Dem_Alcohol.T)
dat.modelPRE$Dem_ChildGender.CG <- as.factor(dat.modelPRE$Dem_ChildGender.CG)
dat.modelPRE$Dem_Children.T <- as.factor(dat.modelPRE$Dem_Children.T)
dat.modelPRE$Dem_Disability_child.CG <- as.factor(dat.modelPRE$Dem_Disability_child.CG)
dat.modelPRE$Dem_Father.CG <- as.factor(dat.modelPRE$Dem_Father.CG)
dat.modelPRE$Dem_Father.T <- as.factor(dat.modelPRE$Dem_Father.T)
dat.modelPRE$Dem_Fighting.CG <- as.factor(dat.modelPRE$Dem_Fighting.CG)
dat.modelPRE$Dem_Fighting.T <- as.factor(dat.modelPRE$Dem_Fighting.T)
dat.modelPRE$Dem_Gender.CG <- as.factor(dat.modelPRE$Dem_Gender.CG)
dat.modelPRE$Dem_Illness_adult.CG <- as.factor(dat.modelPRE$Dem_Illness_adult.CG)
dat.modelPRE$Dem_Illness_child.CG <- as.factor(dat.modelPRE$Dem_Illness_child.CG)
dat.modelPRE$Dem_Mother.CG <- as.factor(dat.modelPRE$Dem_Mother.CG)
dat.modelPRE$Dem_Mother.T <- as.factor(dat.modelPRE$Dem_Mother.T)
dat.modelPRE$Dem_Poverty.CG <- as.factor(dat.modelPRE$Dem_Poverty.CG)
dat.modelPRE$Dem_Read.T <- as.factor(dat.modelPRE$Dem_Read.T)
dat.modelPRE$Dem_Relationship <- as.factor(dat.modelPRE$Dem_Relationship)
dat.modelPRE$Dem_TB_HIV.CG <- as.factor(dat.modelPRE$Dem_TB_HIV.CG)
dat.modelPRE$Dem_TB_HIV.T <- as.factor(dat.modelPRE$Dem_TB_HIV.T)
dat.modelPRE$S.N.CG <- as.factor(dat.modelPRE$S.N.CG)
dat.modelPRE$S.N <- as.factor(dat.modelPRE$S.N)
#POST
dat.modelPOST$Dem_Alcohol.CG <- as.factor(dat.modelPOST$Dem_Alcohol.CG)
dat.modelPOST$Dem_Alcohol.T <- as.factor(dat.modelPOST$Dem_Alcohol.T)
dat.modelPOST$Dem_ChildGender.CG <- as.factor(dat.modelPOST$Dem_ChildGender.CG)
dat.modelPOST$Dem_Children.T <- as.factor(dat.modelPOST$Dem_Children.T)
dat.modelPOST$Dem_Disability_child.CG <- as.factor(dat.modelPOST$Dem_Disability_child.CG)
dat.modelPOST$Dem_Father.CG <- as.factor(dat.modelPOST$Dem_Father.CG)
dat.modelPOST$Dem_Father.T <- as.factor(dat.modelPOST$Dem_Father.T)
dat.modelPOST$Dem_Fighting.CG <- as.factor(dat.modelPOST$Dem_Fighting.CG)
dat.modelPOST$Dem_Fighting.T <- as.factor(dat.modelPOST$Dem_Fighting.T)
dat.modelPOST$Dem_Gender.CG <- as.factor(dat.modelPOST$Dem_Gender.CG)
dat.modelPOST$Dem_Illness_adult.CG <- as.factor(dat.modelPOST$Dem_Illness_adult.CG)
dat.modelPOST$Dem_Illness_child.CG <- as.factor(dat.modelPOST$Dem_Illness_child.CG)
dat.modelPOST$Dem_Mother.CG <- as.factor(dat.modelPOST$Dem_Mother.CG)
dat.modelPOST$Dem_Mother.T <- as.factor(dat.modelPOST$Dem_Mother.T)
dat.modelPOST$Dem_Poverty.CG <- as.factor(dat.modelPOST$Dem_Poverty.CG)
dat.modelPOST$Dem_Read.T <- as.factor(dat.modelPOST$Dem_Read.T)
dat.modelPOST$Dem_Relationship <- as.factor(dat.modelPOST$Dem_Relationship)
dat.modelPOST$Dem_TB_HIV.CG <- as.factor(dat.modelPOST$Dem_TB_HIV.CG)
dat.modelPOST$Dem_TB_HIV.T <- as.factor(dat.modelPOST$Dem_TB_HIV.T)
dat.modelPOST$S.N.CG <- as.factor(dat.modelPOST$S.N.CG)
dat.modelPOST$S.N <- as.factor(dat.modelPOST$S.N)



#merge pre and post
dat.model <- rbind(dat.modelPRE, dat.modelPOST)
dat.model.long <- dat.model[order(factor(dat.model[, "S.N"], levels = unique(dat.model[, "S.N"]))),]
head(dat.model.long) #beautiful

#================================V

#================================ SCORE BARPLOTS ============
##===CG
#PRE
datcg.join <- read.csv("datcg.join.csv")
# str(datcg.join)
# ggplot(datcg.join, aes(x=Summed_PRE_APQ)) + geom_histogram(fill= "#2BB709") + theme_pubclean(base_size=24) + labs(x="APQ score")
# ggplot(datcg.join, aes(x=Summed_PRE_SDQ)) + geom_histogram(fill= "#09ACB7") + theme_pubclean(base_size=24) + labs(x="SDQ score")
# ggplot(datcg.join, aes(x=Summed_PRE_ICAST)) + geom_histogram(fill= "#B73309") + theme_pubclean(base_size=24) + labs(x="ICAST score")
# ggplot(datcg.join, aes(x=Summed_PRE_CESD)) + geom_histogram(fill= "#5C22C1") + theme_pubclean(base_size=24) + labs(x="CESD score")
# ggplot(datcg.join, aes(x=Summed_PRE_FFC)) + geom_histogram(fill= "#C122A6") + theme_pubclean(base_size=24) + labs(x="FFC score")
# ggplot(datcg.join, aes(x=Summed_PRE_PSE)) + geom_histogram(fill= "#81643E") + theme_pubclean(base_size=24) + labs(x="PSE score")
# #POST
# ggplot(datcg.join, aes(x=Summed_POST_APQ)) + geom_histogram(fill= "#2BB709") + theme_pubclean(base_size=24) + labs(x="APQ score") 
# ggplot(datcg.join, aes(x=Summed_POST_SDQ)) + geom_histogram(fill= "#09ACB7") + theme_pubclean(base_size=24) + labs(x="SDQ score")
# ggplot(datcg.join, aes(x=Summed_POST_ICAST)) + geom_histogram(fill= "#B73309") + theme_pubclean(base_size=24) + labs(x="ICAST score")
# ggplot(datcg.join, aes(x=Summed_POST_CESD)) + geom_histogram(fill= "#5C22C1") + theme_pubclean(base_size=24) + labs(x="CESD score")
# ggplot(datcg.join, aes(x=Summed_POST_FFC)) + geom_histogram(fill= "#C122A6") + theme_pubclean(base_size=24) + labs(x="FFC score")
# ggplot(datcg.join, aes(x=Summed_POST_PSE)) + geom_histogram(fill= "#81643E") + theme_pubclean(base_size=24) + labs(x="PSE score")

##===T
datteen.join <- read.csv("datteen.join.csv")
head(datteen.join)
#PRE
# ggplot(datteen.join, aes(x=Summed_PRE_APQ)) + geom_histogram(fill= "#2BB709") + theme_pubclean(base_size=24) + labs(x="APQ score")
# ggplot(datteen.join, aes(x=Summed_PRE_SDQ)) + geom_histogram(fill= "#09ACB7") + theme_pubclean(base_size=24) + labs(x="SDQ score")
# ggplot(datteen.join, aes(x=Summed_PRE_ICAST)) + geom_histogram(fill= "#B73309") + theme_pubclean(base_size=24) + labs(x="ICAST score")
# ggplot(datteen.join, aes(x=Summed_PRE_PSE)) + geom_histogram(fill= "#81643E") + theme_pubclean(base_size=24) + labs(x="PSE score")
# 
# #POST
# ggplot(datteen.join, aes(x=Summed_POST_APQ)) + geom_histogram(fill= "#2BB709") + theme_pubclean(base_size=24) + labs(x="APQ score")
# ggplot(datteen.join, aes(x=Summed_POST_SDQ)) + geom_histogram(fill= "#09ACB7") + theme_pubclean(base_size=24) + labs(x="SDQ score")
# ggplot(datteen.join, aes(x=Summed_POST_ICAST)) + geom_histogram(fill= "#B73309") + theme_pubclean(base_size=24) + labs(x="ICAST score")
# ggplot(datteen.join, aes(x=Summed_POST_PSE)) + geom_histogram(fill= "#81643E") + theme_pubclean(base_size=24) + labs(x="PSE score")
# 
# #============= FULL SAMPLE
# library(ggplot2)
# 
# #============= MATCHED 
# library(ggplot2)
# head(dat.model)
# ##===CG
# #PRE
# ggplot(dat.modelPRE, aes(x=APQ.CG)) + geom_histogram(fill= "#2BB709") + theme_pubclean(base_size=24) + labs(x="APQ score")
# ggplot(dat.modelPRE, aes(x=SDQ.CG)) + geom_histogram(fill= "#09ACB7") + theme_pubclean(base_size=24) + labs(x="SDQ score")
# ggplot(dat.modelPRE, aes(x=ICAST.CG)) + geom_histogram(fill= "#B73309") + theme_pubclean(base_size=24) + labs(x="ICAST score")
# ggplot(dat.modelPRE, aes(x=CESD.CG)) + geom_histogram(fill= "#5C22C1") + theme_pubclean(base_size=24) + labs(x="CESD score")
# ggplot(dat.modelPRE, aes(x=FFC.CG)) + geom_histogram(fill= "#C122A6") + theme_pubclean(base_size=24) + labs(x="FFC score")
# ggplot(dat.modelPRE, aes(x=PSE.CG)) + geom_histogram(fill= "#81643E") + theme_pubclean(base_size=24) + labs(x="PSE score")
# #POST
# ggplot(dat.modelPOST, aes(x=APQ.CG)) + geom_histogram(fill= "#2BB709") + theme_pubclean(base_size=24) + labs(x="APQ score")
# ggplot(dat.modelPOST, aes(x=SDQ.CG)) + geom_histogram(fill= "#09ACB7") + theme_pubclean(base_size=24) + labs(x="SDQ score")
# ggplot(dat.modelPOST, aes(x=ICAST.CG)) + geom_histogram(fill= "#B73309") + theme_pubclean(base_size=24) + labs(x="ICAST score")
# ggplot(dat.modelPOST, aes(x=CESD.CG)) + geom_histogram(fill= "#5C22C1") + theme_pubclean(base_size=24) + labs(x="CESD score")
# ggplot(dat.modelPOST, aes(x=FFC.CG)) + geom_histogram(fill= "#C122A6") + theme_pubclean(base_size=24) + labs(x="FFC score")
# ggplot(dat.modelPOST, aes(x=PSE.CG)) + geom_histogram(fill= "#81643E") + theme_pubclean(base_size=24) + labs(x="PSE score")
# 
# #===T
# ##===CG
# #PRE
# ggplot(dat.modelPRE, aes(x=APQ.T)) + geom_histogram(fill= "#2BB709") + theme_pubclean(base_size=24) + labs(x="APQ score")
# ggplot(dat.modelPRE, aes(x=SDQ.T)) + geom_histogram(fill= "#09ACB7") + theme_pubclean(base_size=24) + labs(x="SDQ score")
# ggplot(dat.modelPRE, aes(x=ICAST.T)) + geom_histogram(fill= "#B73309") + theme_pubclean(base_size=24) + labs(x="ICAST score")
# ggplot(dat.modelPRE, aes(x=PSE.T)) + geom_histogram(fill= "#81643E") + theme_pubclean(base_size=24) + labs(x="PSE score")
# 
# #POST
# ggplot(dat.modelPOST, aes(x=APQ.T)) + geom_histogram(fill= "#2BB709") + theme_pubclean(base_size=24) + labs(x="APQ score")
# ggplot(dat.modelPOST, aes(x=SDQ.T)) + geom_histogram(fill= "#09ACB7") + theme_pubclean(base_size=24) + labs(x="SDQ score")
# ggplot(dat.modelPOST, aes(x=ICAST.T)) + geom_histogram(fill= "#B73309") + theme_pubclean(base_size=24) + labs(x="ICAST score")
# ggplot(dat.modelPOST, aes(x=PSE.T)) + geom_histogram(fill= "#81643E") + theme_pubclean(base_size=24) + labs(x="PSE score")




#===============================Response rates=====
#full sample
#CG
  #demographics
cg.demo.nacount <- 1 - colSums(is.na(datcg.join[,c(4:18, 43)]))/290
cg.demo.nacount
(1-cg.demo.nacount)*290
  #outcome items - PRE
cg.PRE.nacount <- 1 - colSums(is.na(datcg.join[,19:45]))/290
cg.PRE.nacount
  #outcome items - POST
cg.POST.nacount <- 1 - colSums(is.na(datcg.join[,60:86]))/290
cg.POST.nacount
rbind(cg.PRE.nacount, cg.POST.nacount)
#T
  #demo
t.demo.nacount <-  1 - colSums(is.na(datteen.join[,c(4:13,35)]))/295
t.demo.nacount
(1-t.demo.nacount)*295
  #outcomes - PRE
t.PRE.nacount <- 1 - colSums(is.na(datteen.join[,14:37]))/295
t.PRE.nacount
#outcomes - POST
t.POST.nacount <- 1 - colSums(is.na(datteen.join[,47:70]))/295
t.POST.nacount
rbind(t.PRE.nacount, t.POST.nacount)


# C6E0B4; 89E3E6; FB5252; FFCCCC; BC8FF1; FF7CE9; C7AB76

#================================ MIXED EFFECT REGRESSION ============================= 
library(GLMMadaptive)
library(nlme)
library(lme4)


#distribution fit
library(fitdistrplus)
#CAREGIVERS
f.cg.apq.norm <- fitdist(dat.modelPRE$APQ.CG, "norm", discrete = T)
plot(f.cg.apq.norm)
summary(f.cg.apq.norm)
f.cg.apq.lnorm <- fitdist(dat.modelPRE$APQ.CG, "lnorm", discrete = T)
summary(f.cg.apq.lnorm) #lnorm seems better
plot(f.cg.apq.lnorm)

f.cg.sdq.gam <- fitdist(as.vector(na.omit(dat.modelPRE$SDQ.CG+1)), "gamma", discrete = T)
summary(f.cg.sdq.gam) #gamma seems better
plot(f.cg.sdq.gam)
f.cg.sdq.lnorm <- fitdist(as.vector(na.omit(dat.modelPRE$SDQ.CG+1)), "lnorm", discrete = T)
summary(f.cg.sdq.lnorm)
plot(f.cg.sdq.lnorm)

f.cg.ffc.norm <- fitdist(dat.modelPRE$FFC.CG, "norm", discrete = T)
f.cg.ffc.lnorm <- fitdist(dat.modelPRE$FFC.CG, "lnorm", discrete = T)
summary(f.cg.ffc.norm)
summary(f.cg.ffc.lnorm) #lnorm seems better but not much difference
plot(f.cg.ffc.norm)
plot(f.cg.ffc.lnorm)

f.cg.cesd.norm <- fitdist(dat.modelPOST$CESD.CG, "norm", discrete = T)
f.cg.cesd.lnorm <- fitdist(dat.modelPOST$CESD.CG, "lnorm", discrete = T)
summary(f.cg.cesd.norm)
summary(f.cg.cesd.lnorm) #lnorm seems better but not much difference
plot(f.cg.cesd.norm)
plot(f.cg.cesd.lnorm)

#TEENS
f.t.apq.norm <- fitdist(as.vector(na.omit(dat.modelPRE$APQ.T)), "norm", discrete = T)
plot(f.t.apq.norm)
summary(f.t.apq.norm)
f.t.apq.lnorm <- fitdist(as.vector(na.omit(dat.modelPRE$APQ.T)), "lnorm", discrete = T)
summary(f.t.apq.lnorm) 
plot(f.t.apq.lnorm) #normal seems better fit

f.t.sdq.norm <- fitdist(as.vector(na.omit(dat.modelPRE$SDQ.T)+1), "norm", discrete = T)
plot(f.t.sdq.norm)
summary(f.t.sdq.norm)
f.t.sdq.gamma <- fitdist(as.vector(na.omit(dat.modelPRE$SDQ.T)+1), "gamma", discrete = T)
summary(f.t.sdq.gamma) 
plot(f.t.sdq.gamma) #gamma is better fit

### ============= Code for residual plots for mixed_model objects
library(DHARMa)
#code for mixed_model objects (ICAST.CG, ICAST.T, SDQ)
resids_plot <- function (object, y, nsim = 1000,
                         type = c("subject_specific", "mean_subject"),
                         integerResponse = NULL) {
  if (!inherits(object, "MixMod"))
    stop("this function works for 'MixMod' objects.\n")
  type <- match.arg(type)
  if (is.null(integerResponse)) {
    integer_families <- c("binomial", "poisson", "negative binomial",
                          "zero-inflated poisson", "zero-inflated negative binomial", 
                          "hurdle poisson", "hurdle negative binomial")
    numeric_families <- c("hurdle log-normal", "beta", "hurdle beta", "Gamma")
    if (object$family$family %in% integer_families) {
      integerResponse <- TRUE
    } else if (object$family$family %in% numeric_families) {
      integerResponse <- FALSE
    } else {
      stop("non build-in family object; you need to specify the 'integerResponse',\n",
           "\targument indicating whether the outcome variable is integer or not.\n")
    }
  }
  sims <- simulate(object, nsim = nsim, type = type)
  fits <- fitted(object, type = type)
  dharmaRes <- DHARMa::createDHARMa(simulatedResponse = sims, observedResponse = y, 
                                    fittedPredictedResponse = fits, 
                                    integerResponse = integerResponse)
  DHARMa:::plot.DHARMa(dharmaRes, quantreg = FALSE)
}

#define fn to get coeff on natural scale
convert.coeff <- function(model){
  linkinv <- family(model)$linkinv
  estimates <- summary(model)$coefficients[,1]
  s.e <- summary(model)$coefficients[,2]
  natural.coeffs <- round(linkinv(estimates), 2)
  upper <- round(linkinv(estimates + 2*s.e), 2)
  lower <- round(linkinv(estimates - 2*s.e), 2)
  C.I <- cbind(lower, upper)
  return(cbind(natural.coeffs, C.I))
  
}
#alter for mixed_model objects
convert.coeff.MM <- function(model){
  linkinv <- family(model)$linkinv
  estimates <- summary(model)$coef_table[,1]
  s.e <- summary(model)$coef_table[,2]
  natural.coeffs <- round(linkinv(estimates), 2)
  upper <- round(linkinv(estimates + 2*s.e), 2)
  lower <- round(linkinv(estimates - 2*s.e), 2)
  C.I <- cbind(lower, upper)
  return(cbind(natural.coeffs, C.I))
  
}
### =============
# dat.model.long$Dem_Age.CG <- dat.model.long$Dem_Age.CG-25 #for meaningful intercept
# dat.model.long$Dem_ChildAge.CG <- dat.model.long$Dem_ChildAge.CG-10
# ## CAREGIVERS
# #APQ - normal
# hist(dat.modelPRE$APQ.CG)
# qqnorm(dat.modelPRE$APQ.CG); qqline(dat.modelPRE$APQ.CG)
# glmm.cg.apq1 <- lmer(APQ.CG~Time + (1|S.N.CG), data=dat.model.long)
# glmm.cg.apq2 <- glmer(APQ.CG~Time + (1|S.N.CG), family = gaussian("log"),data=dat.model.long)
# 
# plot(simulateResiduals(glmm.cg.apq1), title="APQ Caregivers") #best fit (but still poor)
# 
# glmm.cg.apq <- glmm.cg.apq1
# summary(glmm.cg.apq)
# 
# #SDQ - Gamma
# #transform dist for modelling
# hist(dat.modelPRE$SDQ.CG+1) # gamma
# glmm.cg.sdq1 <- lmer(SDQ.CG ~ Time + (1|S.N.CG), data=dat.model.long)
# glmm.cg.sdq2 <- mixed_model((SDQ.CG+1)~Time, random = ~1|S.N.CG, family = Gamma.fam(), data = dat.model.long)
# plot(simulateResiduals(glmm.cg.sdq1), title="SDQ Caregivers") #poor fit
# resids_plot(glmm.cg.sdq2, na.omit(dat.model.long$SDQ.CG+1)) #poor fit 
# glmm.cg.sdq <- glmm.cg.sdq1
# summary(glmm.cg.sdq)
# 
# 
# #ICAST - poisson
# describe(as.numeric(dat.join$Summed_PRE_ICAST.x)) #mean=variance, poisson
# glmm.cg.icast <- mixed_model(ICAST.CG ~ Time, random = ~1|S.N.CG, family = poisson("log"), max_coef_value=1000, data = dat.model.long,iter_EM=0)
# resids_plot(glmm.cg.icast, dat.model.long$ICAST.CG) #reasonable fit
# summary(glmm.cg.icast) #fit is reasonable, but time coefficient is really really small
# convert.coeff.MM(glmm.cg.icast)
# 
# 
# 
# #FFC - normal
# qqnorm(dat.modelPRE$FFC.CG); qqline(dat.modelPRE$FFC.CG)
# qqnorm(dat.modelPOST$FFC.CG); qqline(dat.modelPOST$FFC.CG)
# glmm.cg.ffc1 <- lmer(FFC.CG~Time + (1|S.N.CG), data=dat.model.long)
# glmm.cg.ffc2 <- glmer(FFC.CG~Time + (1|S.N.CG), family=gaussian("log"),data=dat.model.long)
# plot(simulateResiduals(glmm.cg.ffc1), title = "FFC Caregivers") #best fit (still poor)
# plot(simulateResiduals(glmm.cg.ffc2))
# glmm.cg.ffc <- glmm.cg.ffc1
# summary(glmm.cg.ffc)
# 
# #CESD - normal
# qqnorm(dat.modelPRE$CESD.CG)
# qqnorm(dat.modelPOST$CESD.CG)
# glmm.cg.cesd <- lmer(CESD.CG~Time+(1|S.N.CG), data=dat.model.long)
# plot(simulateResiduals(glmm.cg.cesd), title="CESD Caregivers") #poor fit
# summary(glmm.cg.cesd)
# 
# 
# 
# ## TEENS
# #APQ - normal
# qqnorm(dat.modelPRE$APQ.T); qqline(dat.modelPRE$APQ.T)
# qqnorm(dat.modelPOST$APQ.T)
# #normal
# glmm.t.apq <- lmer(APQ.T~ as.numeric(Time) + (1|S.N), data = dat.model.long)
# plot(simulateResiduals(glmm.t.apq), title="APQ Teens") #reasonable fit
# 
# summary(glmm.t.apq)
# 
# #SDQ
# #normal
# qqnorm(dat.modelPRE$SDQ.T);  qqline(dat.modelPRE$SDQ.T)
# glmm.t.sdq <- lmer(SDQ.T~Time+(1|S.N), data = dat.model.long)
# plot(simulateResiduals(glmm.t.sdq), title="SDQ Teens") #poor fit
# summary(glmm.t.sdq)
# 
# #gamma
# hist(dat.modelPRE$SDQ.T+1)
# glmm.t.sdq2 <- mixed_model((SDQ.T+1)~Time, random=~1|S.N, family = Gamma(), data = dat.model.long)
# summary(glmm.t.sdq2)
# resids_plot(glmm.t.sdq2, na.omit(dat.model.long$SDQ.T+1)) #poor fit
# 
# #ICAST - ZI negative binomial
# hist(dat.join$Summed_PRE_ICAST.y)
# describe(dat.join$Summed_PRE_ICAST.y) #mean != var, neg.bin
# #poisson
# glmm.t.icast1 <- mixed_model(ICAST.T ~ Time, random = ~1|S.N, family = poisson(), max_coef_value=100,data = dat.model.long,iter_EM=0)
# #zi poisson
# glmm.t.icast2 <- mixed_model(ICAST.T ~ Time, random = ~1|S.N, family = zi.poisson(), zi_fixed = ~1, max_coef_value=100,data = dat.model.long,iter_EM=0)
# #zi poisson with random effects
# glmm.t.icast3 <- mixed_model(ICAST.T ~ Time, random = ~1|S.N, family = zi.poisson(), zi_fixed = ~1, zi_random = ~1|S.N, max_coef_value=100,data = dat.model.long,iter_EM=0)
# #zi negbin 
# glmm.t.icast4 <- mixed_model(ICAST.T ~ Time, random = ~1|S.N, family = zi.negative.binomial(), zi_fixed = ~1, max_coef_value=100,data = dat.model.long,iter_EM=0)
# 
# resids_plot(glmm.t.icast1, na.omit(dat.model.long$ICAST.T)) #poor fit 
# resids_plot(glmm.t.icast2, na.omit(dat.model.long$ICAST.T)) #poor fit 
# resids_plot(glmm.t.icast3, na.omit(dat.model.long$ICAST.T)) #poor fit
# resids_plot(glmm.t.icast4, na.omit(dat.model.long$ICAST.T)) #poorfit (best)
# summary(glmm.t.icast1)
# summary(glmm.t.icast4)
# glmm.t.icast <- glmm.t.icast4
# 
# 
# ### ============= Moderator Analysis
# #only CG APQ/ICAST and T APQ provided reasonable fits in ME models
# #CG ICAST gives tiny time coefficient, not worth modeling moderators
# #only those models are carried forward into moderator analysis
# 
# ###CAREGIVERS
# #APQ
# summary(glmm.cg.apq)
# convert.coeff(glmm.cg.apq)
# AIC(glmm.cg.apq)
# #CG demographics
# glmm.cg.apq.mod1 <- lmer(APQ.CG~Time*Dem_Alcohol.CG + (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.apq.mod1) #***
# convert.coeff(glmm.cg.apq.mod1)
# AIC(glmm.cg.apq.mod1)
# glmm.cg.apq.mod2 <- lmer(APQ.CG~Time*Dem_Disability_child.CG + (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.apq.mod2)
# glmm.cg.apq.mod3 <- lmer(APQ.CG~Time*Dem_Father.CG + (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.apq.mod3)
# glmm.cg.apq.mod4 <- lmer(APQ.CG~Time*Dem_Fighting.CG + (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.apq.mod4)#***
# convert.coeff(glmm.cg.apq.mod4)
# AIC(glmm.cg.apq.mod4)
# glmm.cg.apq.mod5 <- lmer(APQ.CG~Time*Dem_Gender.CG + (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.apq.mod5) #***
# convert.coeff(glmm.cg.apq.mod5)
# AIC(glmm.cg.apq.mod5)
# glmm.cg.apq.mod6 <- lmer(APQ.CG~Time*Dem_Illness_adult.CG + (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.apq.mod6) #***
# convert.coeff(glmm.cg.apq.mod6)
# AIC(glmm.cg.apq.mod6)
# glmm.cg.apq.mod7 <- lmer(APQ.CG~Time*Dem_Illness_child.CG + (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.apq.mod7) #***
# convert.coeff(glmm.cg.apq.mod7)
# AIC(glmm.cg.apq.mod7)
# glmm.cg.apq.mod8 <- lmer(APQ.CG~Time*Dem_Mother.CG + (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.apq.mod8)
# glmm.cg.apq.mod10 <- lmer(APQ.CG~Time*Dem_TB_HIV.CG + (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.apq.mod10)#***
# convert.coeff(glmm.cg.apq.mod10)
# AIC(glmm.cg.apq.mod10)
# glmm.cg.apq.mod11 <- lmer(APQ.CG~Time*Dem_Age.CG + (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.apq.mod11)
# #teenager demographics
# glmm.cg.apq.mod12 <- lmer(APQ.CG~Time*Dem_ChildGender.CG+ (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.apq.mod12)
# glmm.cg.apq.mod13 <- lmer(APQ.CG~Time*Dem_Age.T+ (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.apq.mod13)
# 
# ## SDQ
# summary(glmm.cg.sdq)
# convert.coeff(glmm.cg.sdq)
# AIC(glmm.cg.sdq)
# #CG demographics
# glmm.cg.sdq.mod1 <- lmer(SDQ.CG~Time*Dem_Alcohol.CG + (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.sdq.mod1) #***
# convert.coeff(glmm.cg.sdq.mod1)
# AIC(glmm.cg.sdq.mod1)
# glmm.cg.sdq.mod2 <- lmer(SDQ.CG~Time*Dem_Disability_child.CG + (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.sdq.mod2) #**
# convert.coeff(glmm.cg.sdq.mod2)
# AIC(glmm.cg.sdq.mod2)
# glmm.cg.sdq.mod3 <- lmer(SDQ.CG~Time*Dem_Father.CG + (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.sdq.mod3)
# glmm.cg.sdq.mod4 <- lmer(SDQ.CG~Time*Dem_Fighting.CG + (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.sdq.mod4) #****
# convert.coeff(glmm.cg.sdq.mod4)
# AIC(glmm.cg.sdq.mod4)
# glmm.cg.sdq.mod5 <- lmer(SDQ.CG~Time*Dem_Gender.CG + (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.sdq.mod5) 
# glmm.cg.sdq.mod6 <- lmer(SDQ.CG~Time*Dem_Illness_adult.CG + (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.sdq.mod6)  #***
# convert.coeff(glmm.cg.sdq.mod6)
# AIC(glmm.cg.sdq.mod6)
# glmm.cg.sdq.mod7 <- lmer(SDQ.CG~Time*Dem_Illness_child.CG + (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.sdq.mod7)  #***
# convert.coeff(glmm.cg.sdq.mod7)
# AIC(glmm.cg.sdq.mod7)
# glmm.cg.sdq.mod8 <- lmer(SDQ.CG~Time*Dem_Mother.CG + (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.sdq.mod8)
# glmm.cg.sdq.mod10 <- lmer(SDQ.CG~Time*Dem_TB_HIV.CG + (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.sdq.mod10) #***
# convert.coeff(glmm.cg.sdq.mod10)
# AIC(glmm.cg.sdq.mod10)
# glmm.cg.sdq.mod11 <- lmer(SDQ.CG~Time*Dem_Age.CG + (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.sdq.mod11) #***
# convert.coeff(glmm.cg.sdq.mod11)
# AIC(glmm.cg.sdq.mod11)
# #teenager demographics
# glmm.cg.sdq.mod12 <- lmer(SDQ.CG~Time*Dem_ChildGender.CG+ (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.sdq.mod12) #***
# convert.coeff(glmm.cg.sdq.mod12)
# AIC(glmm.cg.sdq.mod12)
# glmm.cg.sdq.mod13 <- lmer(SDQ.CG~Time*Dem_Age.T+ (1|S.N.CG), data=dat.model.long)
# summary(glmm.cg.sdq.mod13)
# 
# 
# 
# ## ICAST
# summary(glmm.cg.icast)
# convert.coeff.MM(glmm.cg.icast)
# 
# glmm.cg.icast.mod1 <- mixed_model(ICAST.CG ~ Time*Dem_ChildGender.CG, random = ~1|S.N.CG, family = poisson("log"), max_coef_value=1000, data = dat.model.long,iter_EM=0)
# summary(glmm.cg.icast.mod1)
# convert.coeff.MM(glmm.cg.icast.mod1)
# glmm.cg.icast.mod2 <- mixed_model(ICAST.CG ~ Time*Dem_Gender.CG, random = ~1|S.N.CG, family = poisson("log"), max_coef_value=1000, data = dat.model.long,iter_EM=0)
# summary(glmm.cg.icast.mod2)
# convert.coeff.MM(glmm.cg.icast.mod2)
# glmm.cg.icast.mod3 <- mixed_model(ICAST.CG ~ Time*Dem_Age.CG, random = ~1|S.N.CG, family = poisson("log"), max_coef_value=1000, data = dat.model.long,iter_EM=0)
# summary(glmm.cg.icast.mod3)
# convert.coeff.MM(glmm.cg.icast.mod3)
# glmm.cg.icast.mod4 <- mixed_model(ICAST.CG ~ Time*Dem_Alcohol.CG, random = ~1|S.N.CG, family = poisson("log"), max_coef_value=1000, data = dat.model.long,iter_EM=0)
# summary(glmm.cg.icast.mod4)
# convert.coeff.MM(glmm.cg.icast.mod4)
# glmm.cg.icast.mod5 <- mixed_model(ICAST.CG ~ Time*Dem_Fighting.CG, random = ~1|S.N.CG, family = poisson("log"), max_coef_value=1000, data = dat.model.long,iter_EM=0)
# summary(glmm.cg.icast.mod5)
# convert.coeff.MM(glmm.cg.icast.mod5)
# glmm.cg.icast.mod6 <- mixed_model(ICAST.CG ~ Time*Dem_Illness_child.CG, random = ~1|S.N.CG, family = poisson("log"), max_coef_value=1000, data = dat.model.long,iter_EM=0)
# summary(glmm.cg.icast.mod6)
# convert.coeff.MM(glmm.cg.icast.mod6)
# glmm.cg.icast.mod7 <- mixed_model(ICAST.CG ~ Time*Dem_Illness_adult.CG, random = ~1|S.N.CG, family = poisson("log"), max_coef_value=1000, data = dat.model.long,iter_EM=0)
# summary(glmm.cg.icast.mod7)
# convert.coeff.MM(glmm.cg.icast.mod7)
# glmm.cg.icast.mod8 <- mixed_model(ICAST.CG ~ Time*Dem_TB_HIV.CG, random = ~1|S.N.CG, family = poisson("log"), max_coef_value=1000, data = dat.model.long,iter_EM=0)
# summary(glmm.cg.icast.mod8)
# convert.coeff.MM(glmm.cg.icast.mod8)
# 
#   #CG RESIDUAL PLOTS
#   #APQ
# plot(simulateResiduals(glmm.cg.apq))
# plot(simulateResiduals(glmm.cg.apq.mod5)) #CG sex
# plot(simulateResiduals(glmm.cg.apq.mod1)) #alcohol
# plot(simulateResiduals(glmm.cg.apq.mod4)) #fighting
# plot(simulateResiduals(glmm.cg.apq.mod7)) #sick child
# plot(simulateResiduals(glmm.cg.apq.mod6)) #sick adult
# plot(simulateResiduals(glmm.cg.apq.mod10)) #hivtb
# 
#   #SDQ
# plot(simulateResiduals(glmm.cg.sdq))
# plot(simulateResiduals(glmm.cg.sdq.mod11)) #cg age
# plot(simulateResiduals(glmm.cg.sdq.mod12)) #tsex
# plot(simulateResiduals(glmm.cg.sdq.mod1)) #alc
# plot(simulateResiduals(glmm.cg.sdq.mod4)) #fight
# plot(simulateResiduals(glmm.cg.sdq.mod2)) #disabled
# plot(simulateResiduals(glmm.cg.sdq.mod7)) #sick child
# plot(simulateResiduals(glmm.cg.sdq.mod6)) #sick adult
# plot(simulateResiduals(glmm.cg.sdq.mod10)) #hivtb
# 
#   #ICAST
# resids_plot(glmm.cg.icast, dat.model.long$ICAST.CG)
# resids_plot(glmm.cg.icast.mod1, dat.model.long$ICAST.CG)
# resids_plot(glmm.cg.icast.mod2, dat.model.long$ICAST.CG)
# #resids_plot(glmm.cg.icast.mod3, dat.model.long$ICAST.CG)
# resids_plot(glmm.cg.icast.mod4, dat.model.long$ICAST.CG)
# resids_plot(glmm.cg.icast.mod5, dat.model.long$ICAST.CG)
# resids_plot(glmm.cg.icast.mod6, dat.model.long$ICAST.CG)
# resids_plot(glmm.cg.icast.mod7, dat.model.long$ICAST.CG)
# resids_plot(glmm.cg.icast.mod8, dat.model.long$ICAST.CG)
# 
# 
# 
# 
# 
# ###TEENS
# #APQ
# summary(glmm.t.apq)
# convert.coeff(glmm.t.apq)
# AIC(glmm.t.apq)
# #t demographics
# glmm.t.apq.mod1 <- lmer(APQ.T~Time*Dem_Alcohol.T + (1|S.N), data=dat.model.long)
# summary(glmm.t.apq.mod1)#**
# convert.coeff(glmm.t.apq.mod1)
# AIC(glmm.t.apq.mod1)
# glmm.t.apq.mod2 <- lmer(APQ.T~Time*Dem_Father.T + (1|S.N), data=dat.model.long)
# summary(glmm.t.apq.mod2)
# glmm.t.apq.mod3 <- lmer(APQ.T~Time*Dem_Fighting.T + (1|S.N), data=dat.model.long)
# summary(glmm.t.apq.mod3) #***
# convert.coeff(glmm.t.apq.mod3)
# AIC(glmm.t.apq.mod3)
# glmm.t.apq.mod4 <- lmer(APQ.T~Time*Dem_ChildGender.CG + (1|S.N), data=dat.model.long)
# summary(glmm.t.apq.mod4) #***
# convert.coeff(glmm.t.apq.mod4)
# AIC(glmm.t.apq.mod4)
# glmm.t.apq.mod5 <- lmer(APQ.T~Time*Dem_Mother.T + (1|S.N), data=dat.model.long)
# summary(glmm.t.apq.mod5)
# glmm.t.apq.mod6 <- lmer(APQ.T~Time*Dem_TB_HIV.T + (1|S.N), data=dat.model.long)
# summary(glmm.t.apq.mod6) #***
# convert.coeff(glmm.t.apq.mod6)
# AIC(glmm.t.apq.mod6)
# glmm.t.apq.mod7 <- lmer(APQ.T~Time*Dem_ChildAge.CG + (1|S.N), data=dat.model.long)
# summary(glmm.t.apq.mod7) #***
# convert.coeff(glmm.t.apq.mod7)
# AIC(glmm.t.apq.mod7)
# glmm.t.apq.mod9 <- lmer(APQ.T~Time*Dem_Read.T + (1|S.N), data=dat.model.long)
# summary(glmm.t.apq.mod9)
# #cg demographics
# glmm.t.apq.mod10 <- lmer(APQ.T~Time*Dem_Gender.CG+ (1|S.N), data=dat.model.long)
# summary(glmm.t.apq.mod10) #**
# convert.coeff(glmm.t.apq.mod10)
# AIC(glmm.t.apq.mod10)
# glmm.t.apq.mod11 <- lmer(APQ.T~Time*Dem_Age.CG+ (1|S.N), data=dat.model.long)
# summary(glmm.t.apq.mod11)
# glmm.t.apq.mod13 <- lmer(APQ.T~Time*Dem_Relationship+ (1|S.N), data=dat.model.long)
# summary(glmm.t.apq.mod13)
# 
# #SDQ
# summary(glmm.t.sdq)
# convert.coeff(glmm.t.sdq)
# AIC(glmm.t.sdq)
# #t demographics
# glmm.t.sdq.mod1 <- lmer(SDQ.T~Time*Dem_Alcohol.T + (1|S.N), data=dat.model.long)
# summary(glmm.t.sdq.mod1)#**
# convert.coeff(glmm.t.sdq.mod1)
# AIC(glmm.t.sdq.mod1)
# glmm.t.sdq.mod2 <- lmer(SDQ.T~Time*Dem_Father.T + (1|S.N), data=dat.model.long)
# summary(glmm.t.sdq.mod2)
# glmm.t.sdq.mod3 <- lmer(SDQ.T~Time*Dem_Fighting.T + (1|S.N), data=dat.model.long)
# summary(glmm.t.sdq.mod3) #***
# convert.coeff(glmm.t.sdq.mod3)
# AIC(glmm.t.sdq.mod3)
# glmm.t.sdq.mod4 <- lmer(SDQ.T~Time*Dem_ChildGender.CG + (1|S.N), data=dat.model.long)
# summary(glmm.t.sdq.mod4) 
# glmm.t.sdq.mod5 <- lmer(SDQ.T~Time*Dem_Mother.T + (1|S.N), data=dat.model.long)
# summary(glmm.t.sdq.mod5)
# glmm.t.sdq.mod6 <- lmer(SDQ.T~Time*Dem_TB_HIV.T + (1|S.N), data=dat.model.long)
# summary(glmm.t.sdq.mod6) #***
# convert.coeff(glmm.t.sdq.mod6)
# AIC(glmm.t.sdq.mod6)
# glmm.t.sdq.mod7 <- lmer(SDQ.T~Time*Dem_ChildAge.CG + (1|S.N), data=dat.model.long)
# summary(glmm.t.sdq.mod7) #***
# convert.coeff(glmm.t.sdq.mod7)
# AIC(glmm.t.sdq.mod7)
# glmm.t.sdq.mod9 <- lmer(SDQ.T~Time*Dem_Read.T + (1|S.N), data=dat.model.long)
# summary(glmm.t.sdq.mod9)
# #cg demographics
# glmm.t.sdq.mod10 <- lmer(SDQ.T~Time*Dem_Gender.CG+ (1|S.N), data=dat.model.long)
# summary(glmm.t.sdq.mod10)
# glmm.t.sdq.mod11 <- lmer(SDQ.T~Time*Dem_Age.CG+ (1|S.N), data=dat.model.long)
# summary(glmm.t.sdq.mod11) #***
# convert.coeff(glmm.t.sdq.mod11)
# AIC(glmm.t.sdq.mod11)
# glmm.t.sdq.mod13 <- lmer(SDQ.T~Time*Dem_Relationship+ (1|S.N), data=dat.model.long)
# summary(glmm.t.sdq.mod13)
# 
# #ICAST
# #define fn to get CI for zero-part coefficient
# convert.coeff.ziMM <- function(model){
#   linkinv <- family(model)$linkinv
#   estimates <- summary(model)$coef_table[,1]
#   zi.est <- summary(model)$coef_table_zi[,1]
#   s.e <- summary(model)$coef_table[,2]
#   zi.se <- summary(model)$coef_table_zi[,2]
#   natural.coeffs <- round(linkinv(estimates), 2)
#   natural.coeff.zi <- round(linkinv(zi.est), 2)
#   upper <- round(linkinv(estimates + 2*s.e), 2)
#   lower <- round(linkinv(estimates - 2*s.e), 2)
#   upper.zi <- round(linkinv(zi.est + 2*zi.se), 2)
#   lower.zi <- round(linkinv(zi.est - 2*zi.se), 2)
#   C.I <- cbind(lower, upper)
#   C.I.zi <- cbind(lower.zi, upper.zi)
#   rnd.var <- round(attr(summary(model)$D, "L")^2, 3)
#   AIC <- round(summary(model)$AIC, 2)
#   return(list(cbind(natural.coeffs, C.I), cbind(natural.coeff.zi, C.I.zi), cbind(rnd.var, AIC)))
#   
# }
# 
# summary(glmm.t.icast)
# convert.coeff.ziMM(glmm.t.icast)
# glmm.t.ic.mod1 <- mixed_model(ICAST.T ~ Time*Dem_ChildGender.CG, random = ~1|S.N, family = zi.negative.binomial(), zi_fixed = ~Dem_ChildGender.CG, max_coef_value=100,data = dat.model.long,iter_EM=0)
# summary(glmm.t.ic.mod1)
# convert.coeff.ziMM(glmm.t.ic.mod1)
# glmm.t.ic.mod2 <- mixed_model(ICAST.T ~ Time*Dem_ChildAge.CG, random = ~1|S.N, family = zi.negative.binomial(), zi_fixed = ~Dem_ChildAge.CG, max_coef_value=100,data = dat.model.long,iter_EM=0)
# summary(glmm.t.ic.mod2)
# convert.coeff.ziMM(glmm.t.ic.mod2)
# glmm.t.ic.mod3 <- mixed_model(ICAST.T ~ Time*Dem_Gender.CG, random = ~1|S.N, family = zi.negative.binomial(), zi_fixed = ~Dem_Gender.CG, max_coef_value=100,data = dat.model.long,iter_EM=0)
# summary(glmm.t.ic.mod3)
# convert.coeff.ziMM(glmm.t.ic.mod3)
# glmm.t.ic.mod4 <- mixed_model(ICAST.T ~ Time*Dem_Alcohol.T, random = ~1|S.N, family = zi.negative.binomial(), zi_fixed = ~Dem_Alcohol.T, max_coef_value=100,data = dat.model.long,iter_EM=0)
# summary(glmm.t.ic.mod4)
# convert.coeff.ziMM(glmm.t.ic.mod4)
# glmm.t.ic.mod5 <- mixed_model(ICAST.T ~ Time*Dem_Fighting.T, random = ~1|S.N, family = zi.negative.binomial(), zi_fixed = ~Dem_Fighting.T, max_coef_value=100,data = dat.model.long,iter_EM=0)
# summary(glmm.t.ic.mod5)
# convert.coeff.ziMM(glmm.t.ic.mod5)
# glmm.t.ic.mod6 <- mixed_model(ICAST.T ~ Time*Dem_TB_HIV.T, random = ~1|S.N, family = zi.negative.binomial(), zi_fixed = ~Dem_TB_HIV.T, max_coef_value=100,data = dat.model.long,iter_EM=0)
# summary(glmm.t.ic.mod6)
# convert.coeff.ziMM(glmm.t.ic.mod6)
# 
# 
# 
#   # T RESID PLOTS
#   #APQ 
# plot(simulateResiduals(glmm.t.apq))
# plot(simulateResiduals(glmm.t.apq.mod4)) #tsex
# plot(simulateResiduals(glmm.t.apq.mod7)) #tage
# plot(simulateResiduals(glmm.t.apq.mod10)) #cgsex
# plot(simulateResiduals(glmm.t.apq.mod1)) #alc
# plot(simulateResiduals(glmm.t.apq.mod3)) #fighting
# plot(simulateResiduals(glmm.t.apq.mod6)) #tbhiv
# 
#   #SDQ
# plot(simulateResiduals(glmm.t.sdq))
# plot(simulateResiduals(glmm.t.sdq.mod7)) #tage
# plot(simulateResiduals(glmm.t.sdq.mod11)) #cg age
# plot(simulateResiduals(glmm.t.sdq.mod1)) #alc
# plot(simulateResiduals(glmm.t.sdq.mod3)) #fight
# plot(simulateResiduals(glmm.t.sdq.mod6)) #tbhiv
# 
#   #ICAST
# resids_plot(glmm.t.icast, na.omit(dat.model.long$ICAST.T))
# resids_plot(glmm.t.ic.mod1, na.omit(dat.model.long$ICAST.T))
# resids_plot(glmm.t.ic.mod2, na.omit(dat.model.long$ICAST.T))
# resids_plot(glmm.t.ic.mod3, na.omit(dat.model.long$ICAST.T))
# #resids_plot(glmm.t.ic.mod4, na.omit(dat.model.long$ICAST.T))
# resids_plot(glmm.t.ic.mod5, na.omit(dat.model.long$ICAST.T))
# resids_plot(glmm.t.ic.mod6, na.omit(dat.model.long$ICAST.T))
# 
# 
# dat.modelPRE$Dem_Age.CG <- dat.modelPRE$Dem_Age.CG+25 #revert ages
# dat.modelPRE$Dem_ChildAge.CG <- dat.modelPRE$Dem_ChildAge.CG+10

#================================== GLM Modeling ==============================
#========== Modeling Selected Pre-intervention Likert scores ===============


###========= Caregivers
# dat.modelPRE$Dem_Age.CG <- dat.modelPRE$Dem_Age.CG-25 #for meaningful intercept
# dat.modelPRE$Dem_ChildAge.CG <- dat.modelPRE$Dem_ChildAge.CG-10
# 
# 
# 
# ##  APQ - forward selection
# glmcg.apq.null <- glm(APQ.CG ~ 1, data=dat.modelPRE)
# summary(glmcg.apq.null)
# convert.coeff(glmcg.apq.null)
# boxplot(glm(APQ.CG ~ 1, data=dat.modelPRE)$residuals) #null model diagnostics
# 
# #moderators
# summary(glm(APQ.CG ~ Dem_ChildGender.CG, data=dat.modelPRE)) 
# summary(glm(APQ.CG ~ Dem_ChildAge.CG, data=dat.modelPRE))
# summary(glm(APQ.CG ~ Dem_Age.CG, data=dat.modelPRE))
# glmcg.apq.1 <- glm(APQ.CG ~ Dem_Alcohol.CG, data=dat.modelPRE)
# summary(glmcg.apq.1) #***
# convert.coeff(glmcg.apq.1)
# plot(glmcg.apq.1$residuals) #diagnostics
# glmcg.apq.2 <- glm(APQ.CG ~ Dem_Father.CG, data=dat.modelPRE)
# summary(glmcg.apq.2) #***
# convert.coeff(glmcg.apq.2)
# plot(glmcg.apq.2$residuals)
# glmcg.apq.3 <- glm(APQ.CG ~ Dem_Fighting.CG, data=dat.modelPRE)
# summary(glmcg.apq.3) #**
# convert.coeff(glmcg.apq.3)
# plot(glmcg.apq.3$residuals) #diagnostics
# glmcg.apq.4 <- glm(APQ.CG ~ Dem_Gender.CG, data=dat.modelPRE)
# summary(glmcg.apq.4) #*
# convert.coeff(glmcg.apq.4)
# plot(glmcg.apq.4$coefficients) #diagnostics
# summary(glm(APQ.CG ~ Dem_Mother.CG, data=dat.modelPRE))
# summary(glm(APQ.CG ~ Dem_Disability_child.CG, data=dat.modelPRE)) #*
# glmcg.apq.5 <- glm(APQ.CG ~ Dem_Illness_adult.CG, data=dat.modelPRE)
# summary(glmcg.apq.5) #*
# convert.coeff(glmcg.apq.5)
# plot(glmcg.apq.5$residuals) #diagnostics
# glmcg.apq6 <- glm(APQ.CG ~ Dem_Illness_child.CG, data=dat.modelPRE)
# summary(glmcg.apq6) #***
# convert.coeff(glmcg.apq6)
# plot(glmcg.apq.6$residuals) #diagnostics
# glmcg.apq7 <- glm(APQ.CG ~ Dem_TB_HIV.CG, data=dat.modelPRE) 
# summary(glmcg.apq7) #***
# convert.coeff(glmcg.apq7)
# plot(glmcg.apq7$residuals) #diagnostics
# 
# #build positive parenting explanatory model
# summary(glm(APQ.CG ~ Dem_Gender.CG + Dem_Alcohol.CG + Dem_Illness_child.CG + Dem_TB_HIV.CG, data = dat.modelPRE ))
# 
# 
# 
# ##  SDQ
# hist(dat.modelPRE$SDQ.CG) #model with gamma
# 
# #null
# glm.cg.sdq.null <- glm((SDQ.CG+1) ~ 1, family = Gamma("log"), data=dat.modelPRE)
# summary(glm.cg.sdq.null)
# convert.coeff(glm.cg.sdq.null)
# boxplot(glm.cg.sdq.null$residuals); hist(glm.cg.sdq.null$residuals)
# 
# 
# 
# #moderators
# summary(glm((SDQ.CG.CG+1) ~ Dem_ChildGender.CG, family=Gamma("log"), data=dat.modelPRE))
# summary(glm((SDQ.CG+1) ~ Dem_ChildAge.CG, family=Gamma("log"), data=dat.modelPRE))
# glmcg.sdq1 <- glm((SDQ.CG+1) ~ Dem_Age.CG, family=Gamma("log"), data=dat.modelPRE)
# summary(glmcg.sdq1) #**
# convert.coeff(glmcg.sdq1)
# plot(glmcg.sdq1$residuals)
# glmcg.sdq2 <- glm((SDQ.CG+1) ~ Dem_Alcohol.CG, family=Gamma("log"), data=dat.modelPRE)
# summary(glmcg.sdq2) #***
# convert.coeff(glmcg.sdq2)
# plot(glmcg.sdq2$residuals)
# summary(glm((SDQ.CG+1) ~ Dem_Father.CG, family=Gamma("log"), data=dat.modelPRE)) 
# glmcg.sdq3 <- glm((SDQ.CG+1) ~ Dem_Fighting.CG, family=Gamma("log"), data=dat.modelPRE)
# summary(glmcg.sdq3) #***
# convert.coeff(glmcg.sdq3)
# plot(glmcg.sdq3$residuals)
# summary(glm((SDQ.CG+1) ~ Dem_Gender.CG, family=Gamma("log"), data=dat.modelPRE)) 
# summary(glm((SDQ.CG+1) ~ Dem_Mother.CG, family=Gamma("log"), data=dat.modelPRE))
# glmcg.sdq4 <- glm((SDQ.CG+1) ~ Dem_Disability_child.CG, family=Gamma("log"), data=dat.modelPRE)
# summary(glmcg.sdq4) #***
# convert.coeff(glmcg.sdq4)
# plot(glmcg.sdq4$residuals)
# glmcg.sdq5 <- glm((SDQ.CG+1) ~ Dem_Illness_adult.CG, family=Gamma("log"), data=dat.modelPRE)
# summary(glmcg.sdq5) #***
# convert.coeff(glmcg.sdq5)
# plot(glmcg.sdq5$residuals)
# glmcg.sdq6 <- glm((SDQ.CG+1) ~ Dem_Illness_child.CG, family=Gamma("log"), data=dat.modelPRE)
# summary(glmcg.sdq6) #***
# convert.coeff(glmcg.sdq6)
# plot(glmcg.sdq6$residuals)
# glmcg.sdq7 <- glm((SDQ.CG+1) ~ Dem_TB_HIV.CG, family=Gamma("log"), data=dat.modelPRE)
# summary(glmcg.sdq7) #***
# convert.coeff(glmcg.sdq7)
# plot(glmcg.sdq7$residuals)
# 
# #build explanatory child behaviour glm
# summary(glm((SDQ.CG+1) ~ + Dem_Age.CG +  Dem_Alcohol.CG + Dem_Disability_child.CG + Dem_TB_HIV.CG, family=Gamma("log"), data=dat.modelPRE))
# plot(glm((SDQ.CG+1) ~ + Dem_Age.CG +  Dem_Alcohol.CG + Dem_Disability_child.CG + Dem_TB_HIV.CG, family=Gamma("log"), data=dat.modelPRE)$residuals)
# plot(glm((SDQ.CG+1) ~ + Dem_Age.CG +  Dem_Alcohol.CG + Dem_Disability_child.CG + Dem_TB_HIV.CG, family=Gamma("log"), data=dat.modelPRE))
# 
# 
# #ICAST - forward selection
# hist(dat.modelPRE$ICAST.CG) #poisson but more like neg bin
# fitdist(dat.modelPRE$ICAST.CG, distr='nbinom') #get theta estimate
# #null
# glmcg.ic.null <- glm(ICAST.CG~1, family=negative.binomial(theta=250), data=dat.modelPRE)
# summary(glmcg.ic.null)
# convert.coeff(glmcg.ic.null)
# plot(glmcg.ic.null$residuals) #go with neg bin 
# 
# #moderators
# glmcg.ic1 <- glm(ICAST.CG ~ Dem_ChildGender.CG, family=negative.binomial(theta=250), data=dat.modelPRE)
# summary(glmcg.ic1) #*
# convert.coeff(glmcg.ic1)
# plot(glmcg.ic1$residuals)
# summary(glm(ICAST.CG ~ Dem_ChildAge.CG, family=negative.binomial(theta=250), data=dat.modelPRE))
# glmcg.ic2 <- glm(ICAST.CG ~ Dem_Age.CG, family=negative.binomial(theta=250), data=dat.modelPRE) 
# summary(glmcg.ic2) #*
# convert.coeff(glmcg.ic2)
# plot(glmcg.ic2$residuals)
# glmcg.ic3 <- glm(ICAST.CG ~ Dem_Alcohol.CG, family=negative.binomial(theta=250), data=dat.modelPRE)
# summary(glmcg.ic3) #*
# convert.coeff(glmcg.ic3)
# plot(glmcg.ic3$residuals)
# summary(glm(ICAST.CG ~ Dem_Father.CG, family=negative.binomial(theta=250), data=dat.modelPRE)) #***
# glmcg.ic4 <- glm(ICAST.CG ~ Dem_Fighting.CG, family=negative.binomial(theta=250), data=dat.modelPRE)
# summary(glmcg.ic4) #*
# convert.coeff(glmcg.ic4)
# plot(glmcg.ic4$residuals)
# glmcg.ic5 <- glm(ICAST.CG ~ Dem_Gender.CG, family=negative.binomial(theta=250), data=dat.modelPRE)
# summary(glmcg.ic5) #*
# convert.coeff(glmcg.ic5)
# plot(glmcg.ic5$residuals)
# summary(glm(ICAST.CG ~ Dem_Mother.CG, family=negative.binomial(theta=250), data=dat.modelPRE))
# summary(glm(ICAST.CG ~ Dem_Disability_child.CG, family=negative.binomial(theta=250), data=dat.modelPRE)) 
# glmcg.ic6 <- glm(ICAST.CG ~ Dem_Illness_adult.CG, family=negative.binomial(theta=250), data=dat.modelPRE)
# summary(glmcg.ic6) #*
# convert.coeff(glmcg.ic6)
# plot(glmcg.ic6$residuals)
# glmcg.ic7 <- glm(ICAST.CG ~ Dem_Illness_child.CG, family=negative.binomial(theta=250), data=dat.modelPRE)
# summary(glmcg.ic7) #*
# convert.coeff(glmcg.ic7)
# plot(glmcg.ic7$residuals)
# glmcg.ic8 <- glm(ICAST.CG ~ Dem_TB_HIV.CG, family=negative.binomial(theta=250), data=dat.modelPRE)
# summary(glmcg.ic8) #*
# convert.coeff(glmcg.ic8)
# plot(glmcg.ic8$residuals)
# 
# #build harsh discipline explanatory model
# summary(glm(ICAST.CG ~ Dem_Age.CG + Dem_Illness_child.CG + Dem_Alcohol.CG + Dem_TB_HIV.CG, family=negative.binomial(theta=250), data=dat.modelPRE))
# plot(glm(ICAST.CG ~ Dem_Age.CG + Dem_Illness_child.CG + Dem_Alcohol.CG + Dem_TB_HIV.CG, family=negative.binomial(theta=250), data=dat.modelPRE))
#   #obs 40 outlier
# 
# ##  CG RESIDUAL PLOTS
# par(mfrow=c(2,2))
# plot(glmcg.apq.null)
# plot(glmcg.apq.4) #CG sex
# plot(glmcg.apq.1) #alcohol
# plot(glmcg.apq.3) #fighting
# plot(glmcg.apq6) #sick child
# plot(glmcg.apq.5) #sick adult
# plot(glmcg.apq7) #HIVTB
# 
# plot(glm.cg.sdq.null)
# plot(glmcg.sdq1) #cg age
# plot(glmcg.sdq2) #alcohol
# plot(glmcg.sdq3) #fighting
# plot(glmcg.sdq4) #disabled child
# plot(glmcg.sdq5) #sick child
# plot(glmcg.sdq6) #sick adult
# plot(glmcg.sdq7) #HIV_TB
# 
# plot(glmcg.ic.null)
# plot(glmcg.ic1) #t sex
# plot(glmcg.ic5) #cg sex
# plot(glmcg.ic2) #cg age
# plot(glmcg.ic3) #alcohol
# plot(glmcg.ic4) #fighting
# plot(glmcg.ic7) #sick child
# plot(glmcg.ic6) #sick adult
# plot(glmcg.ic8) #hiv_tb
# 
# 
# 
# 
# 
# 
# ### TEENS
#   #APQ - forward selection
# hist(dat.modelPRE$APQ.T)
# #null
# glmt.apq.null <- glm(APQ.T ~1, data=dat.modelPRE)
# boxplot(glmt.apq.null$residuals)
# summary(glmt.apq.null)
# convert.coeff(glmt.apq.null)
# #moderators
# summary(glm(APQ.T ~ Dem_Age.CG, data=dat.modelPRE))
# glmt.apq1 <- glm(APQ.T ~ Dem_ChildAge.CG, data=dat.modelPRE)
# summary(glmt.apq1)
# convert.coeff(glmt.apq1)
# plot(glmt.apq1$residuals)
# glmt.apq2 <- glm(APQ.T ~ Dem_ChildGender.CG, data=dat.modelPRE)
# summary(glmt.apq2)
# convert.coeff(glmt.apq2)
# plot(glmt.apq2$residuals)
# summary(glm(APQ.T ~ Dem_Alcohol.T, data=dat.modelPRE))
# summary(glm(APQ.T ~ Dem_Children.T, data=dat.modelPRE))
# summary(glm(APQ.T ~ Dem_Father.T, data=dat.modelPRE)) #*
# glmt.apq3 <- glm(APQ.T ~ Dem_Fighting.T, data=dat.modelPRE)
# summary(glmt.apq3)
# convert.coeff(glmt.apq3)
# plot(glmt.apq3$residuals)
# summary(glm(APQ.T ~ Dem_Gender.CG, data=dat.modelPRE))
# summary(glm(APQ.T ~ Dem_Mother.T, data=dat.modelPRE))
# summary(glm(APQ.T ~ Dem_Poverty.CG, data=dat.modelPRE))
# summary(glm(APQ.T ~ Dem_Read.T, data=dat.modelPRE))
# glmt.apq4 <- glm(APQ.T ~ Dem_TB_HIV.T, data=dat.modelPRE)
# summary(glmt.apq4)
# convert.coeff(glmt.apq4)
# plot(glmt.apq4$residuals)
# 
# #build positive parenting explanatory model
# glm.pre.t.apq <- glm(APQ.T ~ Dem_ChildGender.CG  + Dem_Fighting.T + Dem_TB_HIV.T, data = dat.modelPRE)
# summary(glm.pre.t.apq)
# par(mfrow=c(2,2))
# plot(glm.pre.t.apq)
# 
#   #SDQ - forward selection
# hist(dat.modelPRE$SDQ.T)
# #null
# glmt.sdq.null <- glm(SDQ.T ~ 1, data=dat.modelPRE) 
# summary(glmt.sdq.null)
# convert.coeff(glmt.sdq.null)
# plot(glmt.sdq.null$residuals)
# boxplot(glmt.sdq.null$residuals)
# 
# glmt.sdq.null <- glm((SDQ.T+1) ~ 1, family = Gamma("log"),data=dat.modelPRE)
# summary(glmt.sdq.null)
# convert.coeff(glmt.sdq.null)
# plot(glmt.sdq.null$residuals)
# boxplot(glmt.sdq.null$residuals) #Gamma looks better
# 
# summary(glm((SDQ.T+1) ~ Dem_ChildGender.CG,family = Gamma("log"), data=dat.modelPRE)) 
# glmt.sdq1 <- glm((SDQ.T+1) ~ Dem_ChildAge.CG,family = Gamma("log"), data=dat.modelPRE)
# summary(glmt.sdq1)
# convert.coeff(glmt.sdq1)
# plot(glmt.sdq1$residuals)
# summary(glm((SDQ.T+1) ~ Dem_Age.CG,family = Gamma("log"), data=dat.modelPRE)) 
# glmt.sdq2 <- glm((SDQ.T+1) ~ Dem_Alcohol.T,family = Gamma("log"), data=dat.modelPRE)
# summary(glmt.sdq2)
# convert.coeff(glmt.sdq2)
# plot(glmt.sdq2$residuals)
# summary(glm((SDQ.T+1) ~ Dem_Children.T,family = Gamma("log"), data=dat.modelPRE))
# summary(glm((SDQ.T+1) ~ Dem_Father.T,family = Gamma("log"), data=dat.modelPRE)) 
# glmt.sdq3 <- glm((SDQ.T+1) ~ Dem_Fighting.T,family = Gamma("log"), data=dat.modelPRE)
# summary(glmt.sdq3)
# convert.coeff(glmt.sdq3)
# plot(glmt.sdq3$residuals)
# summary(glm((SDQ.T+1) ~ Dem_Gender.CG,family = Gamma("log"), data=dat.modelPRE))
# summary(glm((SDQ.T+1) ~ Dem_Mother.T,family = Gamma("log"), data=dat.modelPRE))
# summary(glm((SDQ.T+1) ~ Dem_Poverty.CG,family = Gamma("log"), data=dat.modelPRE))
# summary(glm((SDQ.T+1) ~ Dem_Read.T,family = Gamma("log"), data=dat.modelPRE))
# summary(glm((SDQ.T+1) ~ Dem_Relationship,family = Gamma("log"), data=dat.modelPRE))
# glmt.sdq4 <- glm((SDQ.T+1) ~ Dem_TB_HIV.T,family = Gamma("log"), data=dat.modelPRE)
# summary(glmt.sdq4)
# convert.coeff(glmt.sdq4)
# plot(glmt.sdq4$residuals)
# 
# #build child behaviour explanatory model
# glm.pre.t.SDQ <- glm((SDQ.T+1) ~ Dem_ChildAge.CG + Dem_Alcohol.T + Dem_Father.T + Dem_Fighting.T + Dem_TB_HIV.T, family=Gamma("log"), data = dat.modelPRE)
# summary(glm.pre.t.SDQ)
# par(mfrow=c(2,2))
# plot(glm.pre.t.SDQ)
# 
#   #ICAST - forward selection
# hist(dat.modelPRE$ICAST.T)
# 
# glmt.ic.null <- glm(ICAST.T~1, family=poisson, data=dat.modelPRE)
# summary(glmt.ic.null)
# convert.coeff(glmt.ic.null)
# plot(glmt.ic.null$residuals)
# boxplot(glmt.ic.null$residuals)
# 
# glmt.ic1 <- glm(ICAST.T ~ Dem_ChildGender.CG, family=poisson, data=dat.modelPRE)
# summary(glmt.ic1)
# convert.coeff(glmt.ic1)
# plot(glmt.ic1$residuals)
# glmt.ic2 <- glm(ICAST.T ~ Dem_ChildAge.CG,family=poisson, data=dat.modelPRE)
# summary(glmt.ic2)
# convert.coeff(glmt.ic2)
# plot(glmt.ic2$residuals)
# glmt.ic3 <- glm(ICAST.T ~ Dem_Gender.CG,family=poisson, data=dat.modelPRE)
# summary(glmt.ic3)
# convert.coeff(glmt.ic3)
# plot(glmt.ic3$residuals)
# summary(glm(ICAST.T ~ Dem_Age.CG,family=poisson, data=dat.modelPRE))
# glmt.ic4 <- glm(ICAST.T ~ Dem_Alcohol.T,family=poisson, data=dat.modelPRE)
# summary(glmt.ic4)
# convert.coeff(glmt.ic4)
# plot(glmt.ic4$residuals)
# summary(glm(ICAST.T ~ Dem_Father.T,family=poisson, data=dat.modelPRE)) #***
# glmt.ic5 <- glm(ICAST.T ~ Dem_Fighting.T,family=poisson, data=dat.modelPRE)
# summary(glmt.ic5)
# convert.coeff(glmt.ic5)
# plot(glmt.ic5$residuals)
# summary(glm(ICAST.T ~ Dem_Mother.T,family=poisson, data=dat.modelPRE)) #***
# glmt.ic6 <- glm(ICAST.T ~ Dem_TB_HIV.T,family=poisson, data=dat.modelPRE)
# summary(glmt.ic6)
# convert.coeff(glmt.ic6)
# plot(glmt.ic6$residuals)
# 
# 
# #build harsh discipline explanatory model
# glm.pre.t.ICAST <- glm(ICAST.T ~ Dem_ChildGender.CG+Dem_Alcohol.T+Dem_Children.T + Dem_Father.T+Dem_Mother.T + Dem_Fighting.T + Dem_Relationship + Dem_TB_HIV.T,family=poisson, data = dat.modelPRE)
# summary(glm.pre.t.ICAST)
# par(mfrow=c(2,2))
# plot(glm.pre.t.ICAST)
# 
#   ## T RESID PLOTS
# plot(glmt.apq.null)
# plot(glmt.apq2) #childsex
# plot(glmt.apq1) #childage
# plot(glmt.apq3) #fighting
# plot(glmt.apq4) #tbhiv
# 
# plot(glmt.sdq.null)
# plot(glmt.sdq1)
# plot(glmt.sdq2)
# plot(glmt.sdq3)
# plot(glmt.sdq4)
# 
# plot(glmt.ic.null)
# plot(glmt.ic1)
# plot(glmt.ic2)
# plot(glmt.ic3)
# plot(glmt.ic4)
# plot(glmt.ic5)
# plot(glmt.ic6)
# 
# 
# 
# par(mfrow=c(1,1))
# dat.modelPRE$Dem_Age.CG <- dat.modelPRE$Dem_Age.CG+25 #revert ages
# dat.modelPRE$Dem_ChildAge.CG <- dat.modelPRE$Dem_ChildAge.CG+10 #revert ages
# graphics.off()

#=============== Modeling POST-PRE Outcomes ===============
library(car)
dat.model.diff <- dat.modelPRE
dat.model.diff$APQ.CG <- dat.modelPOST$APQ.CG - dat.modelPRE$APQ.CG
dat.model.diff$SDQ.CG <- dat.modelPOST$SDQ.CG - dat.modelPRE$SDQ.CG
dat.model.diff$CESD.CG <- dat.modelPOST$CESD.CG - dat.modelPRE$CESD.CG
dat.model.diff$FFC.CG <- dat.modelPOST$FFC.CG - dat.modelPRE$FFC.CG
dat.model.diff$ICAST.CG <- dat.modelPOST$ICAST.CG - dat.modelPRE$ICAST.CG
dat.model.diff$PSE.CG <- dat.modelPOST$PSE.CG - dat.modelPRE$PSE.CG

dat.model.diff$APQ.T <- dat.modelPOST$APQ.T - dat.modelPRE$APQ.T
dat.model.diff$SDQ.T <- dat.modelPOST$SDQ.T - dat.modelPRE$SDQ.T
dat.model.diff$ICAST.T <- dat.modelPOST$ICAST.T - dat.modelPRE$ICAST.T
dat.model.diff$PSE.T <- dat.modelPOST$PSE.T - dat.modelPRE$PSE.T

#plot distributions
hist(dat.model.diff$APQ.CG, breaks=16, xlim = c(-2,14),xlab="(Post-APQ - Pre-APQ)", main="", col = "#b7d6ab")
hist(dat.model.diff$SDQ.CG, breaks=8, xlab="(Post-SDQ - Pre-SDQ)", main="", col = "#ebe88f")
hist(dat.model.diff$ICAST.CG, breaks=8, xlab="(Post-ICAST - Pre-ICAST)", main="", col = "#db93b8")
hist(dat.model.diff$CESD.CG, breaks=8, xlab="(Post-CESD - Pre-CESD)", main="", col = "#8a73bf")
hist(dat.model.diff$FFC.CG, breaks=8, xlab="(Post-FFC - Pre-FFC)", main="", col = "#7a6346")

hist(dat.model.diff$APQ.T, breaks=10, xlab="(Post-APQ - Pre-APQ)", main="", col = "#b7d6ab")
hist(dat.model.diff$SDQ.T, breaks=8, xlab="(Post-SDQ - Pre-SDQ)", main="", col = "#ebe88f")
hist(dat.model.diff$ICAST.T, breaks=8, xlab="(Post-ICAST - Pre-ICAST)", main="", col = "#db93b8")

#================= (POST-PRE) GLMS ==========================
dat.model.diff$Dem_Age.CG <- dat.model.diff$Dem_Age.CG - 25 #ages for meaningful intercept
dat.model.diff$Dem_Age.T <- dat.model.diff$Dem_Age.T -9
dat.model.diff$Dem_ChildAge.CG<- dat.model.diff$Dem_ChildAge.CG - 10
summary(dat.model.diff$Dem_Age.CG)
summary(dat.model.diff$Dem_Age.T )
summary(dat.model.diff$Dem_ChildAge.CG)

###CAREGIVERS
#APQ
# hist(dat.model.diff$APQ.CG)
# 
# glm.cg.apq1 <- glm(APQ.CG~1, data=dat.model.diff)
# summary(glm.cg.apq1)
# residualPlot(glm.cg.apq1)
# boxplot(glm.cg.apq1$residuals)
# plot(fitdist(residuals(glm.cg.apq1), dist="norm")) #looks like acceptable fit given data
# 
# ###adjust for moderators
# # CG Demographics
# glm.cg.apq1.mod1 <- glm(APQ.CG~1 + Dem_Age.CG  , data=dat.model.diff)
# summary(glm.cg.apq1.mod1)
# glm.cg.apq1.mod2 <- glm(APQ.CG~Dem_Gender.CG  , data=dat.model.diff)
# summary(glm.cg.apq1.mod2) #****
# glm.cg.apq1.mod3 <- glm(APQ.CG~1 + Dem_Relationship  , data=dat.model.diff)
# summary(glm.cg.apq1.mod3) #**
# glm.cg.apq1.mod4 <- glm(APQ.CG~1 + Dem_Father.CG  , data=dat.model.diff)
# summary(glm.cg.apq1.mod4) #**
# glm.cg.apq1.mod5 <- glm(APQ.CG~1 + Dem_Mother.CG , data=dat.model.diff)
# summary(glm.cg.apq1.mod5) #**
# glm.cg.apq1.mod6 <- glm(APQ.CG~1 + Dem_Poverty.CG  , data=dat.model.diff)
# summary(glm.cg.apq1.mod6) #***
# glm.cg.apq1.mod7 <- glm(APQ.CG~1 + Dem_Illness_adult.CG , data=dat.model.diff)
# summary(glm.cg.apq1.mod7) #***
# glm.cg.apq1.mod8 <- glm(APQ.CG~1 + Dem_Illness_child.CG , data=dat.model.diff)
# summary(glm.cg.apq1.mod8) #***
# glm.cg.apq1.mod9 <- glm(APQ.CG~1 + Dem_TB_HIV.CG  , data=dat.model.diff)
# summary(glm.cg.apq1.mod9) #***
# glm.cg.apq1.mod10 <- glm(APQ.CG~1 + Dem_Alcohol.CG , data=dat.model.diff)
# summary(glm.cg.apq1.mod10) #***
# glm.cg.apq1.mod11 <- glm(APQ.CG~1 + Dem_Fighting.CG, data=dat.model.diff)
# summary(glm.cg.apq1.mod11) #***
# glm.cg.apq1.mod12 <- glm(APQ.CG~1 + Dem_Disability_child.CG  , data=dat.model.diff)
# summary(glm.cg.apq1.mod12) #*
# #T demographics
# glm.cg.apq1.mod13 <- glm(APQ.CG~1 + Dem_Age.T  , data=dat.model.diff)
# summary(glm.cg.apq1.mod13)
# glm.cg.apq1.mod14 <- glm(APQ.CG~1 + Dem_ChildGender.CG  , data=dat.model.diff)
# summary(glm.cg.apq1.mod14) 
# glm.cg.apq1.mod15 <- glm(APQ.CG~1 + Dem_Children.T  , data=dat.model.diff)
# summary(glm.cg.apq1.mod15) #*


#SDQ
hist(log(-dat.model.diff$SDQ.CG+2))
ggplot(dat.model.diff, aes(x=SDQ.CG)) + geom_histogram()
plot(fitdist(as.vector(na.omit(-dat.model.diff$SDQ.CG))+2, distr = "lnorm", discrete = T)) 
plot(fitdist(as.vector(na.omit(dat.model.diff$SDQ.CG))+7, distr = "gamma", discrete = T))

glm.cg.sdqnull <- glm((SDQ.CG)~1, family= gaussian(), data=dat.model.diff) #model with normal theory
glm.cg.sdq2 <- glm((SDQ.CG+7)~1, family = Gamma(),data=dat.model.diff)

hist(residuals(glm.cg.sdqnull))
residualPlot(glm.cg.sdqnull) #skewed

###adjust for moderators
# CG Demographics
summary(glm.cg.sdqnull)
convert.coeff(glm.cg.sdqnull)
glm.cg.sdq1 <- glm(SDQ.CG~1 + Dem_Age.CG  , data=dat.model.diff)
summary(glm.cg.sdq1) #**
convert.coeff(glm.cg.sdq1)
glm.cg.sdq7 <- glm(SDQ.CG~1 + Dem_Illness_adult.CG , data=dat.model.diff)
summary(glm.cg.sdq7) #**
convert.coeff(glm.cg.sdq7)
glm.cg.sdq6 <- glm(SDQ.CG~1 + Dem_Illness_child.CG , data=dat.model.diff)
summary(glm.cg.sdq6) #***
convert.coeff(glm.cg.sdq6)
glm.cg.sdq8 <- glm(SDQ.CG~1 + Dem_TB_HIV.CG  , data=dat.model.diff)
summary(glm.cg.sdq8) #**
convert.coeff(glm.cg.sdq8)
glm.cg.sdq3 <- glm(SDQ.CG~1 + Dem_Alcohol.CG , data=dat.model.diff)
summary(glm.cg.sdq3) #***
convert.coeff(glm.cg.sdq3)
glm.cg.sdq4 <- glm(SDQ.CG~1 + Dem_Fighting.CG, data=dat.model.diff)
summary(glm.cg.sdq4) #***
convert.coeff(glm.cg.sdq4)
glm.cg.sdq5 <- glm(SDQ.CG~1 + Dem_Disability_child.CG  , data=dat.model.diff)
summary(glm.cg.sdq5) #***
convert.coeff(glm.cg.sdq5)
#T demographics
glm.cg.sdq2 <- glm(SDQ.CG~1 + Dem_ChildGender.CG  , data=dat.model.diff)
summary(glm.cg.sdq2) #*
convert.coeff(glm.cg.sdq2)




# #CESD
# hist(dat.model.diff$CESD.CG) #try with normal theory
# glm.cg.cesd1 <- glm(CESD.CG ~ 1, data=dat.model.diff)
# summary(glm.cg.cesd1)
# hist(residuals(glm.cg.cesd1))
# residualPlot(glm.cg.cesd1) #good fit
# 
# ###adjust for moderators
# # CG Demographics
# glm.cg.cesd1.mod1 <- glm(CESD.CG~1 + Dem_Age.CG  , data=dat.model.diff)
# summary(glm.cg.cesd1.mod1) 
# glm.cg.cesd1.mod2 <- glm(CESD.CG~Dem_Gender.CG  , data=dat.model.diff)
# summary(glm.cg.cesd1.mod2) #*
# glm.cg.cesd1.mod3 <- glm(CESD.CG~1 + Dem_Relationship  , data=dat.model.diff)
# summary(glm.cg.cesd1.mod3) 
# glm.cg.cesd1.mod4 <- glm(CESD.CG~1 + Dem_Father.CG  , data=dat.model.diff)
# summary(glm.cg.cesd1.mod4) 
# glm.cg.cesd1.mod5 <- glm(CESD.CG~1 + Dem_Mother.CG , data=dat.model.diff)
# summary(glm.cg.cesd1.mod5) 
# glm.cg.cesd1.mod6 <- glm(CESD.CG~1 + Dem_Poverty.CG  , data=dat.model.diff)
# summary(glm.cg.cesd1.mod6) 
# glm.cg.cesd1.mod7 <- glm(CESD.CG~1 + Dem_Illness_adult.CG , data=dat.model.diff)
# summary(glm.cg.cesd1.mod7) 
# glm.cg.cesd1.mod8 <- glm(CESD.CG~1 + Dem_Illness_child.CG , data=dat.model.diff)
# summary(glm.cg.cesd1.mod8) 
# glm.cg.cesd1.mod9 <- glm(CESD.CG~1 + Dem_TB_HIV.CG  , data=dat.model.diff)
# summary(glm.cg.cesd1.mod9) 
# glm.cg.cesd1.mod10 <- glm(CESD.CG~1 + Dem_Alcohol.CG , data=dat.model.diff)
# summary(glm.cg.cesd1.mod10) 
# glm.cg.cesd1.mod11 <- glm(CESD.CG~1 + Dem_Fighting.CG, data=dat.model.diff)
# summary(glm.cg.cesd1.mod11) 
# glm.cg.cesd1.mod12 <- glm(CESD.CG~1 + Dem_Disability_child.CG  , data=dat.model.diff)
# summary(glm.cg.cesd1.mod12) 
# #T demographics
# glm.cg.cesd1.mod13 <- glm(CESD.CG~1 + Dem_Age.T  , data=dat.model.diff)
# summary(glm.cg.cesd1.mod13) #*
# glm.cg.cesd1.mod14 <- glm(CESD.CG~1 + Dem_ChildGender.CG  , data=dat.model.diff)
# summary(glm.cg.cesd1.mod14) 
# glm.cg.cesd1.mod15 <- glm(CESD.CG~1 + Dem_Children.T  , data=dat.model.diff)
# summary(glm.cg.cesd1.mod15) 
#define fn to get coeff on natural scale
convert.coeff.ffc <- function(model){
  estimates <- summary(model)$coefficients[,1]
  s.e <- summary(model)$coefficients[,2]
  natural.coeffs <- rbind(round(-exp(estimates[1]), 2), round(exp(estimates[2]), 2))
  upper <- c(round(-exp(estimates[1] - 2*s.e[1]), 2), round(exp(estimates[2] + 2*s.e[2]), 2))
  lower <- c(round(-exp(estimates[1] + 2*s.e[1]), 2), round(exp(estimates[2] - 2*s.e[2]), 2))
  C.I <- cbind(lower, upper)
  return(cbind(natural.coeffs, C.I))
  
}
#FFC
hist(log(-dat.model.diff$FFC.CG))
glm.cg.ffcnull <- glm(log(-FFC.CG)~1, data=dat.model.diff)
summary(glm.cg.ffcnull)
convert.coeff.ffc(glm.cg.ffcnull)
hist(residuals(glm.cg.ffcnull))
residualPlot(glm.cg.ffcnull) #good fit

###adjust for moderators
# CG Demographics
summary(glm.cg.ffcnull)
convert.coeff.ffc(glm.cg.ffcnull)
glm.cg.ffc1 <- glm(log(-FFC.CG)~1 + Dem_Age.CG  , data=dat.model.diff)
summary(glm.cg.ffc1)
convert.coeff.ffc(glm.cg.ffc1)
glm.cg.ffc5 <- glm(log(-FFC.CG)~1 + Dem_Illness_adult.CG , data=dat.model.diff)
summary(glm.cg.ffc5) #*
convert.coeff.ffc(glm.cg.ffc5)
glm.cg.ffc6 <- glm(log(-FFC.CG)~1 + Dem_TB_HIV.CG  , data=dat.model.diff)
summary(glm.cg.ffc6) #***
convert.coeff.ffc(glm.cg.ffc6)
glm.cg.ffc3 <- glm(log(-FFC.CG)~1 + Dem_Alcohol.CG , data=dat.model.diff)
summary(glm.cg.ffc3) #**
convert.coeff.ffc(glm.cg.ffc3)
glm.cg.ffc4 <- glm(log(-FFC.CG)~1 + Dem_Fighting.CG, data=dat.model.diff)
summary(glm.cg.ffc4) #*
convert.coeff.ffc(glm.cg.ffc4)
#T demographics
glm.cg.ffc2 <- glm(log(-FFC.CG)~1 + Dem_ChildGender.CG  , data=dat.model.diff)
summary(glm.cg.ffc2) #*
convert.coeff.ffc(glm.cg.ffc2)


# #ICAST
# hist(dat.model.diff$ICAST.CG) #need to apply transformation to model with poisson
# summary(dat.model.diff$ICAST.CG)
# summary(-1*dat.model.diff$ICAST.CG)
# hist(-1*dat.model.diff$ICAST.CG)
# dat.model.diff$ICAST.CG.trans <- -1*dat.model.diff$ICAST.CG
# #fit model assuming poisson
# glm.cg.icast <- glm(ICAST.CG.trans ~1, family = poisson, data=dat.model.diff)
# summary(glm.cg.icast)
# residualPlot(glm.cg.icast) # skewed
# ###adjust for moderators
# # CG Demographics
# glm.cg.icast.mod1 <- glm(ICAST.CG~1 + Dem_Age.CG  , data=dat.model.diff)
# summary(glm.cg.icast.mod1) #**
# glm.cg.icast.mod2 <- glm(ICAST.CG~Dem_Gender.CG  , data=dat.model.diff)
# summary(glm.cg.icast.mod2) #*
# glm.cg.icast.mod3 <- glm(ICAST.CG~1 + Dem_Relationship  , data=dat.model.diff)
# summary(glm.cg.icast.mod3) 
# glm.cg.icast.mod4 <- glm(ICAST.CG~1 + Dem_Father.CG  , data=dat.model.diff)
# summary(glm.cg.icast.mod4) #***
# glm.cg.icast.mod5 <- glm(ICAST.CG~1 + Dem_Mother.CG , data=dat.model.diff)
# summary(glm.cg.icast.mod5) 
# glm.cg.icast.mod6 <- glm(ICAST.CG~1 + Dem_Poverty.CG  , data=dat.model.diff)
# summary(glm.cg.icast.mod6) #***
# glm.cg.icast.mod7 <- glm(ICAST.CG~1 + Dem_Illness_adult.CG , data=dat.model.diff)
# summary(glm.cg.icast.mod7) #***
# glm.cg.icast.mod8 <- glm(ICAST.CG~1 + Dem_Illness_child.CG , data=dat.model.diff)
# summary(glm.cg.icast.mod8) #***
# glm.cg.icast.mod9 <- glm(ICAST.CG~1 + Dem_TB_HIV.CG  , data=dat.model.diff)
# summary(glm.cg.icast.mod9) #***
# glm.cg.icast.mod10 <- glm(ICAST.CG~1 + Dem_Alcohol.CG , data=dat.model.diff)
# summary(glm.cg.icast.mod10) #***
# glm.cg.icast.mod11 <- glm(ICAST.CG~1 + Dem_Fighting.CG, data=dat.model.diff)
# summary(glm.cg.icast.mod11) #***
# glm.cg.icast.mod12 <- glm(ICAST.CG~1 + Dem_Disability_child.CG  , data=dat.model.diff)
# summary(glm.cg.icast.mod12) 
# #T demographics
# glm.cg.icast.mod13 <- glm(ICAST.CG~1 + Dem_Age.T  , data=dat.model.diff)
# summary(glm.cg.icast.mod13) 
# glm.cg.icast.mod14 <- glm(ICAST.CG~1 + Dem_ChildGender.CG  , data=dat.model.diff)
# summary(glm.cg.icast.mod14) #*
# glm.cg.icast.mod15 <- glm(ICAST.CG~1 + Dem_Children.T  , data=dat.model.diff)
# summary(glm.cg.icast.mod15) 
# 




# #TEENS
# #APQ
# hist(dat.model.diff$APQ.T) #model with normal theory
# 
# glm.t.apq1 <- glm(APQ.T~1, data=dat.model.diff)
# summary(glm.t.apq1)
# hist(residuals(glm.t.apq1))
# residualPlot(glm.t.apq1) #reasonable fit
# #adjust for moderators 
# #teen demographics
# summary(glm(APQ.T~Dem_Alcohol.T, dat=dat.model.diff))
# summary(glm(APQ.T~Dem_ChildGender.CG, dat=dat.model.diff)) #**
# summary(glm(APQ.T~Dem_Children.T, dat=dat.model.diff))
# summary(glm(APQ.T~Dem_Father.T, dat=dat.model.diff)) #**
# summary(glm(APQ.T~Dem_Fighting.T, dat=dat.model.diff)) #***
# summary(glm(APQ.T~Dem_Mother.T, dat=dat.model.diff))
# summary(glm(APQ.T~Dem_Read.T, dat=dat.model.diff))
# summary(glm(APQ.T~Dem_Relationship, dat=dat.model.diff))
# summary(glm(APQ.T~Dem_TB_HIV.T, dat=dat.model.diff)) #*
# #caregiver demographics
# summary(glm(APQ.T~Dem_Age.CG, dat=dat.model.diff))
# summary(glm(APQ.T~Dem_Gender.CG, dat=dat.model.diff))
# summary(glm(APQ.T~Dem_Poverty.CG, dat=dat.model.diff))


#SDQ
hist(dat.model.diff$SDQ.T) #model with normal theory
hist(log(dat.model.diff$SDQ.T+13)) #model with normal theory

glm.t.sdqnull <- glm(SDQ.T~1,  data=dat.model.diff)
summary(glm.t.sdqnull)
convert.coeff(glm.t.sdqnull)
#adjust for moderators 
#teen demographics
glmt.sdq3 <- glm(SDQ.T~Dem_Alcohol.T,  dat=dat.model.diff) #***
summary(glmt.sdq3)
convert.coeff(glmt.sdq3)
glmt.sdq4 <- glm(SDQ.T~Dem_Fighting.T, dat=dat.model.diff) #**
summary(glmt.sdq4)
convert.coeff(glmt.sdq4)
glmt.sdq5 <- glm(SDQ.T~Dem_TB_HIV.T, dat=dat.model.diff) #.
summary(glmt.sdq5)
convert.coeff(glmt.sdq5)
#caregiver demographics
glmt.sdq1 <- glm(SDQ.T~Dem_Age.CG, dat=dat.model.diff)
summary(glmt.sdq1)
convert.coeff(glmt.sdq1)
glmt.sdq2 <- glm(SDQ.T~Dem_ChildAge.CG, dat=dat.model.diff)
summary(glmt.sdq2)
convert.coeff(glmt.sdq2)





# #ICAST
# hist(dat.model.diff$ICAST.T, breaks=20) #transformation needed
# summary((dat.model.diff$ICAST.T*-1)) #try model with poisson
# dat.model.diff$ICAST.T.trans <- dat.model.diff$ICAST.T*-1
# library(pscl)
# glm.t.icast1 <- glm(ICAST.T.trans ~ 1 , family = poisson(), data=dat.model.diff)
# summary(glm.t.icast1)
# #adjust for moderators 
# #teen demographics
# summary(glm(ICAST.T~Dem_Alcohol.T, dat=dat.model.diff)) 
# summary(glm(ICAST.T~Dem_ChildGender.CG, dat=dat.model.diff)) 
# summary(glm(ICAST.T~Dem_Children.T, dat=dat.model.diff))
# summary(glm(ICAST.T~Dem_Father.T, dat=dat.model.diff)) #**
# summary(glm(ICAST.T~Dem_Fighting.T, dat=dat.model.diff)) #**
# summary(glm(ICAST.T~Dem_Mother.T, dat=dat.model.diff))
# summary(glm(ICAST.T~Dem_Read.T, dat=dat.model.diff))
# summary(glm(ICAST.T~Dem_Relationship, dat=dat.model.diff)) #*
# summary(glm(ICAST.T~Dem_TB_HIV.T, dat=dat.model.diff)) #*
# #caregiver demographics
# summary(glm(ICAST.T~Dem_Age.CG, dat=dat.model.diff))
# summary(glm(ICAST.T~Dem_Gender.CG, dat=dat.model.diff))
# summary(glm(ICAST.T~Dem_Poverty.CG, dat=dat.model.diff))
# 

#======resid plots
par(mfrow=c(2,2))
#==CG
#SDQ
plot(glm.cg.sdqnull)
plot(glm.cg.sdq1)
plot(glm.cg.sdq2)
plot(glm.cg.sdq3)
plot(glm.cg.sdq4)
plot(glm.cg.sdq5)
plot(glm.cg.sdq6)
plot(glm.cg.sdq7)
plot(glm.cg.sdq8)
#FFC
plot(glm.cg.ffcnull)
plot(glm.cg.ffc1)
plot(glm.cg.ffc2)
plot(glm.cg.ffc3)
plot(glm.cg.ffc4)
plot(glm.cg.ffc5)
plot(glm.cg.ffc6)

#==T
#SDQ
plot(glm.t.sdqnull)
plot(glmt.sdq1)
plot(glmt.sdq2)
plot(glmt.sdq3)
plot(glmt.sdq4)
plot(glmt.sdq5)



dat.model.diff$Dem_Age.CG <- dat.model.diff$Dem_Age.CG + 25 #ages for meaningful intercept
dat.model.diff$Dem_Age.T <- dat.model.diff$Dem_Age.T + 9

#================================ CUMULATIVE LINK REGRESSION ============================= 
#============== DATA WRANGLING =======
#create long data frame with items
dat.model.item <- cbind(dat.join[,1:44], dat.join[,59:85], dat.join[,98:131], dat.join[,141:164])


#subset into pre and post
dat.model.item.PRE <- cbind(dat.model.item[,1:44], dat.model.item[,72:105])
dat.model.item.POST <- cbind(dat.model.item[,1:17], dat.model.item[,45:71], dat.model.item[,72:81], dat.model.item[,106:129])

#add time indicator
dat.model.item.PRE$Time <- rep(0, dim(dat.model.item.PRE)[1])
dat.model.item.PRE <- dat.model.item.PRE %>% relocate(`Time`, .before = S.N.x)
dat.model.item.POST$Time <- rep(1, dim(dat.model.item.POST)[1])
dat.model.item.POST <- dat.model.item.POST %>% relocate(`Time`, .before = S.N.x)

#make varnames identical
colnames(dat.model.item.PRE) <- c("Time", "S.N.CG","HH.UIC", "Dem_Age.CG", "Dem_Gender.CG", "Dem_ChildAge.CG",
                                  "Dem_ChildGender.CG", "Dem_EducationT.CG", "Dem_Relationship", "Dem_Father.CG", 
                                  "Dem_Mother.CG", "Dem_Poverty.CG", "Dem_Illness_adult.CG" , "Dem_TB_HIV.CG" , "Dem_Alcohol.CG",
                                  "Dem_Fighting.CG", "Dem_Illness_child.CG", "Dem_Disability_child.CG", "APQ_CG_talk","APQ_CG_involved", "APQ_CG_talk_friends", "APQ_CG_evening", "APQ_CG_time","APQ_CG_dark",
                                  "SDQ_CG_tantrums", "SDQ_CG_obedient", "SDQ_CG_fights", "SDQ_CG_lies", "SDQ_CG_steals", 
                                  "ICAST_CG_spank", "ICAST_CG_object", "ICAST_CG_scream", "ICAST_CG_upset", "MICS_CG",
                                  "CESD_CG_depressed", "CESD_CG_effort", "CESD_CG_lonely", 
                                  "FFC_CG_meat", "FFC_CG_electricity", "FFC_CG_transport", "FFC_CG_airtime", "FFC_CG_worried", "PSE_CG_enrolled", "PSE_CG_praise", "PSE_CG_others",
                                  "S.N.T","Dem_Age.T","Dem_Read.T","Dem_EducationT.T","Dem_Children.T","Dem_Father.T","Dem_Mother.T","Dem_TB_HIV.T","Dem_Alcohol.T","Dem_Fighting.T", 
                                  "APQ_T_talk","APQ_T_involved", "APQ_T_talk_friends", "APQ_T_evening", "APQ_T_time","APQ_T_dark",
                                  "SDQ_T_headaches", "SDQ_T_tantrums", "SDQ_T_obedient", "SDQ_T_worried", "SDQ_T_fights", "SDQ_T_unhappy", "SDQ_T_new", "SDQ_T_lies", "SDQ_T_steals", "SDQ_T_fears",
                                  "ICAST_T_spank", "ICAST_T_object", "ICAST_T_scream", "ICAST_T_upset", "MICS_T",
                                  "PSE_T_enrolled", "PSE_T_praise", "PSE_T_others")

colnames(dat.model.item.POST) <- c("Time", "S.N.CG","HH.UIC", "Dem_Age.CG", "Dem_Gender.CG", "Dem_ChildAge.CG",
                                   "Dem_ChildGender.CG", "Dem_EducationT.CG", "Dem_Relationship", "Dem_Father.CG", 
                                   "Dem_Mother.CG", "Dem_Poverty.CG", "Dem_Illness_adult.CG" , "Dem_TB_HIV.CG" , "Dem_Alcohol.CG",
                                   "Dem_Fighting.CG", "Dem_Illness_child.CG", "Dem_Disability_child.CG", "APQ_CG_talk","APQ_CG_involved", "APQ_CG_talk_friends", "APQ_CG_evening", "APQ_CG_time","APQ_CG_dark",
                                   "SDQ_CG_tantrums", "SDQ_CG_obedient", "SDQ_CG_fights", "SDQ_CG_lies", "SDQ_CG_steals", 
                                   "ICAST_CG_spank", "ICAST_CG_object", "ICAST_CG_scream", "ICAST_CG_upset", "MICS_CG",
                                   "CESD_CG_depressed", "CESD_CG_effort", "CESD_CG_lonely", 
                                   "FFC_CG_meat", "FFC_CG_electricity", "FFC_CG_transport", "FFC_CG_airtime", "FFC_CG_worried", "PSE_CG_enrolled", "PSE_CG_praise", "PSE_CG_others",
                                   "S.N.T","Dem_Age.T","Dem_Read.T","Dem_EducationT.T","Dem_Children.T","Dem_Father.T","Dem_Mother.T","Dem_TB_HIV.T","Dem_Alcohol.T","Dem_Fighting.T", 
                                   "APQ_T_talk","APQ_T_involved", "APQ_T_talk_friends", "APQ_T_evening", "APQ_T_time","APQ_T_dark",
                                   "SDQ_T_headaches", "SDQ_T_tantrums", "SDQ_T_obedient", "SDQ_T_worried", "SDQ_T_fights", "SDQ_T_unhappy", "SDQ_T_new", "SDQ_T_lies", "SDQ_T_steals", "SDQ_T_fears",
                                   "ICAST_T_spank", "ICAST_T_object", "ICAST_T_scream", "ICAST_T_upset", "MICS_T",
                                   "PSE_T_enrolled", "PSE_T_praise", "PSE_T_others")
names(dat.model.item.PRE)==names(dat.model.item.POST)

#merge pre and post
dat.model.item <- rbind(dat.model.item.PRE, dat.model.item.POST)
dat.model.item.long <- dat.model.item[order(factor(dat.model.item[, "S.N.T"], levels = unique(dat.model.item[, "S.N.T"]))),]
head(dat.model.item.long) #beautiful

#make all items into ordered factors
#caregivers
dat.model.item.long$APQ_CG_dark <- as.ordered(as.factor(dat.model.item.long$APQ_CG_dark))
dat.model.item.long$APQ_CG_talk <- as.ordered(as.factor(dat.model.item.long$APQ_CG_talk))
dat.model.item.long$APQ_CG_involved <- as.ordered(as.factor(dat.model.item.long$APQ_CG_involved))
dat.model.item.long$APQ_CG_talk_friends <- as.ordered(as.factor(dat.model.item.long$APQ_CG_talk_friends))
dat.model.item.long$APQ_CG_evening <- as.ordered(as.factor(dat.model.item.long$APQ_CG_evening))
dat.model.item.long$APQ_CG_time <- as.ordered(as.factor(dat.model.item.long$APQ_CG_time))
dat.model.item.long$SDQ_CG_fights <- as.ordered(as.factor(dat.model.item.long$SDQ_CG_fights))
dat.model.item.long$SDQ_CG_tantrums <- as.ordered(as.factor(dat.model.item.long$SDQ_CG_tantrums))
dat.model.item.long$SDQ_CG_obedient <- as.ordered(as.factor(dat.model.item.long$SDQ_CG_obedient))
dat.model.item.long$SDQ_CG_lies <- as.ordered(as.factor(dat.model.item.long$SDQ_CG_lies))
dat.model.item.long$SDQ_CG_steals <- as.ordered(as.factor(dat.model.item.long$SDQ_CG_steals))
dat.model.item.long$ICAST_CG_object <- as.ordered(as.factor(dat.model.item.long$ICAST_CG_object))
dat.model.item.long$ICAST_CG_spank <- as.ordered(as.factor(dat.model.item.long$ICAST_CG_spank))
dat.model.item.long$ICAST_CG_scream <- as.ordered(as.factor(dat.model.item.long$ICAST_CG_scream))
dat.model.item.long$ICAST_CG_upset <- as.ordered(as.factor(dat.model.item.long$ICAST_CG_upset))
dat.model.item.long$FFC_CG_airtime <- as.ordered(as.factor(dat.model.item.long$FFC_CG_airtime))
dat.model.item.long$FFC_CG_meat <- as.ordered(as.factor(dat.model.item.long$FFC_CG_meat))
dat.model.item.long$FFC_CG_electricity <- as.ordered(as.factor(dat.model.item.long$FFC_CG_electricity))
dat.model.item.long$FFC_CG_transport <- as.ordered(as.factor(dat.model.item.long$FFC_CG_transport))
dat.model.item.long$FFC_CG_worried <- as.ordered(as.factor(dat.model.item.long$FFC_CG_worried))
dat.model.item.long$CESD_CG_depressed <- as.ordered(as.factor(dat.model.item.long$CESD_CG_depressed))
dat.model.item.long$CESD_CG_effort <- as.ordered(as.factor(dat.model.item.long$CESD_CG_effort))
dat.model.item.long$CESD_CG_lonely <- as.ordered(as.factor(dat.model.item.long$CESD_CG_lonely))
dat.model.item.long$PSE_CG_others <- as.ordered(as.factor(dat.model.item.long$PSE_CG_others))
dat.model.item.long$PSE_CG_praise <- as.ordered(as.factor(dat.model.item.long$PSE_CG_praise))
dat.model.item.long$MICS_CG <- as.ordered(as.factor(dat.model.item.long$MICS_CG))
#teens
dat.model.item.long$APQ_T_dark <- as.ordered(as.factor(dat.model.item.long$APQ_T_dark))
dat.model.item.long$APQ_T_talk <- as.ordered(as.factor(dat.model.item.long$APQ_T_talk))
dat.model.item.long$APQ_T_involved <- as.ordered(as.factor(dat.model.item.long$APQ_T_involved))
dat.model.item.long$APQ_T_talk_friends <- as.ordered(as.factor(dat.model.item.long$APQ_T_talk_friends))
dat.model.item.long$APQ_T_evening <- as.ordered(as.factor(dat.model.item.long$APQ_T_evening))
dat.model.item.long$APQ_T_time <- as.ordered(as.factor(dat.model.item.long$APQ_T_time))
dat.model.item.long$SDQ_T_fights <- as.ordered(as.factor(dat.model.item.long$SDQ_T_fights))
dat.model.item.long$SDQ_T_tantrums <- as.ordered(as.factor(dat.model.item.long$SDQ_T_tantrums))
dat.model.item.long$SDQ_T_obedient <- as.ordered(as.factor(dat.model.item.long$SDQ_T_obedient))
dat.model.item.long$SDQ_T_lies <- as.ordered(as.factor(dat.model.item.long$SDQ_T_lies))
dat.model.item.long$SDQ_T_steals <- as.ordered(as.factor(dat.model.item.long$SDQ_T_steals))
dat.model.item.long$SDQ_T_headaches <- as.ordered(as.factor(dat.model.item.long$SDQ_T_headaches))
dat.model.item.long$SDQ_T_worried <- as.ordered(as.factor(dat.model.item.long$SDQ_T_worried))
dat.model.item.long$SDQ_T_unhappy <- as.ordered(as.factor(dat.model.item.long$SDQ_T_unhappy))
dat.model.item.long$SDQ_T_new <- as.ordered(as.factor(dat.model.item.long$SDQ_T_new))
dat.model.item.long$SDQ_T_fears <- as.ordered(as.factor(dat.model.item.long$SDQ_T_fears))
dat.model.item.long$ICAST_T_object <- as.ordered(as.factor(dat.model.item.long$ICAST_T_object))
dat.model.item.long$ICAST_T_spank <- as.ordered(as.factor(dat.model.item.long$ICAST_T_spank))
dat.model.item.long$ICAST_T_scream <- as.ordered(as.factor(dat.model.item.long$ICAST_T_scream))
dat.model.item.long$ICAST_T_upset <- as.ordered(as.factor(dat.model.item.long$ICAST_T_upset))
dat.model.item.long$MICS_T <- as.ordered(as.factor(dat.model.item.long$MICS_T))
dat.model.item.long$PSE_T_others <- as.ordered(as.factor(dat.model.item.long$PSE_T_others))
dat.model.item.long$PSE_T_praise <- as.ordered(as.factor(dat.model.item.long$PSE_T_praise))

dat.model.item.long$S.N.CG <- as.factor(dat.model.item.long$S.N.CG)
dat.model.item.long$S.N.T <- as.factor(dat.model.item.long$S.N.T)

#========================== ITEMS ========================
library(ordinal)
### ======================= CAREGIVERS
##   CESD1
hist(dat.model.item.PRE$CESD_CG_depressed) #check dist (all 3's)
hist(dat.model.item.POST$CESD_CG_depressed) #mostly 1's
ord.cg.cesd1.1 <- clmm2(CESD_CG_depressed ~ Time, random=S.N.CG, 
                      data = dat.model.item.long, Hess = T, threshold = "equidistant") #not converged
ord.cg.cesd1.2 <- clmm2(CESD_CG_depressed ~ Time, random=S.N.CG, 
                        data = dat.model.item.long, Hess = T, threshold="flexible") #not converged
anova(ord.cg.cesd1.1, ord.cg.cesd1.2, test="LRT")

#null model
ord.cg.cesd1.null <- clmm2(CESD_CG_depressed ~ 1, random = S.N.CG, data=dat.model.item.long)
anova(ord.cg.cesd1.2, ord.cg.cesd1.null, test = "LRT") #sig diff between two models (treatment effect)
summary(ord.cg.cesd1.2) 
exp(-ord.cg.cesd1.2$coefficients[4]) #infinite relative odds of being in lower category at post compared to pre
cbind(exp(-(ord.cg.cesd1.2$coefficients[4] + 2*14.07)), exp(-(ord.cg.cesd1.2$coefficients[4] - 2*14.07)))

##   CESD2
hist(dat.model.item.PRE$CESD_CG_lonely) #check dist (all 4's)
hist(dat.model.item.POST$CESD_CG_lonely)
ord.cg.cesd2.1 <- clmm2(CESD_CG_lonely ~ Time, random=S.N.CG, 
                      data = dat.model.item.long, Hess = T, threshold = "equidistant") 
ord.cg.cesd2.2 <- clmm2(CESD_CG_lonely ~ Time, random=S.N.CG, 
                      data = dat.model.item.long, Hess = T, threshold = "flexible") 
anova(ord.cg.cesd2.1, ord.cg.cesd2.2, test="LRT")
#null model
ord.cg.cesd2.null <- clmm2(CESD_CG_lonely ~ 1, data=dat.model.item.long)
anova(ord.cg.cesd2.2, ord.cg.cesd2.null) #sig diff between two models (treatment effect)
summary(ord.cg.cesd2.2) 
exp(-ord.cg.cesd2.2$coefficients[4]) #infinite relative odds of being in lower category at post compared to pre


##   CESD3
hist(dat.model.item.PRE$CESD_CG_effort) #check dist (all 2's)
hist(dat.model.item.POST$CESD_CG_effort) #mostly 3's
#ord.cg.cesd3.1 <- clmm2(CESD_CG_effort ~ Time, random=S.N.CG, 
#                      data = dat.model.item.long, Hess = T, threshold = "equidistant") #not converged -CANNOT COMPUTE EQUIDISTANT
ord.cg.cesd3.2 <- clmm2(CESD_CG_effort ~ Time, random=S.N.CG, 
                        data = dat.model.item.long, Hess = T, threshold="flexible") #not converged
anova(ord.cg.cesd3.2, ord.cg.cesd3.2, test="LRT")
#null model
ord.cg.cesd3.null <- clmm2(CESD_CG_effort ~ 1, data=dat.model.item.long)
anova(ord.cg.cesd3.2, ord.cg.cesd3.null) #sig diff between two models (treatment effect)
summary(ord.cg.cesd3.2) 
exp(-ord.cg.cesd3.2$coefficients[2]) #negligible relative odds of being in lower category at post compared to pre
cbind(exp(-(ord.cg.cesd3.2$coefficients[2] + 2*419.43)), exp(-(ord.cg.cesd3.2$coefficients[2] - 2*419.43)))

##  PSE1
hist(dat.model.item.PRE$PSE_CG_praise) #check dist (all 4's)
hist(dat.model.item.POST$PSE_CG_praise) #mostly 5's
ord.cg.pse1.1 <- clmm2(PSE_CG_praise ~ Time, random = S.N.CG, data = dat.model.item.long, Hess=T, threshold = "equidistant")
ord.cg.pse1.2 <- clmm2(PSE_CG_praise ~ Time, random = S.N.CG, data = dat.model.item.long, Hess=T, threshold = "flexible")
anova(ord.cg.pse1.1, ord.cg.pse1.2, test="LRT") 
ord.cg.pse1.null <- clmm2(PSE_CG_praise ~ 1, random = S.N.CG, data = dat.model.item.long, Hess=T) #no variation
anova(ord.cg.pse1.2, ord.cg.pse1.null) #sig treatment effect
summary(ord.cg.pse1.2)
exp(-ord.cg.pse1.2$coefficients[4]) #very low relative odds of moving from lower to higher category
cbind(exp(-(ord.cg.pse1.2$coefficients[4]-2*1.4206)), exp(-(ord.cg.pse1.2$coefficients[4]+
                                                        2*1.4206))) #CI

##  PSE2
hist(dat.model.item.PRE$PSE_CG_others) #check dist (all 3's)
hist(dat.model.item.POST$PSE_CG_others) #mostly 5's
#ord.cg.pse2.1 <- clmm2(PSE_CG_others ~ Time, random = S.N.CG, data = dat.model.item.long, Hess=T, threshold = "equidistant") #CANNOT COMPUTE EQUIDIST
ord.cg.pse2.2 <- clmm2(PSE_CG_others ~ Time, random = S.N.CG, data = dat.model.item.long, Hess=T) #not converged
ord.cg.pse2.null <- clmm2(PSE_CG_others ~ 1, random = S.N.CG, data = dat.model.item.long, Hess=T)
anova(ord.cg.pse2.2, ord.cg.pse2.null) #sig treatment effect
summary(ord.cg.pse2.2)
exp(-ord.cg.pse2.2$coefficients[2]) #very small relative odds of moving from category 3 to 5
cbind(exp(-(ord.cg.pse2.2$coefficients[2] + 2*0.006)), exp(-(ord.cg.pse2.2$coefficients[2] - 2*0.006)))

##  MICS
hist(dat.model.item.PRE$MICS_CG) #bi modal
hist(dat.model.item.POST$MICS_CG) #almost all zeros
ord.cg.mics1 <- clmm2(MICS_CG ~Time, random = S.N.CG, data=dat.model.item.long, Hess = T, threshold = "equidistant")
ord.cg.mics2 <- clmm2(MICS_CG ~Time, random = S.N.CG, data=dat.model.item.long, Hess = T, threshold="flexible")
ord.cg.mics.null <- clmm2(MICS_CG ~1, random = S.N.CG, data=dat.model.item.long, Hess = T)
anova(ord.cg.mics1, ord.cg.mics2, test="LRT")
anova(ord.cg.mics2, ord.cg.mics.null) #sig treatment effect
summary(ord.cg.mics2)
exp(-ord.cg.mics2$coefficients[5]) #very high relative odds of moving to lower category from pre to post
cbind(exp(-(ord.cg.mics2$coefficients[5]+ 2*0.06311)), exp(-(ord.cg.mics2$coefficients[5]- 2*0.06311)))


### ======================= TEENS
##  PSE1
hist(dat.model.item.PRE$PSE_T_praise) 
hist(dat.model.item.POST$PSE_T_praise) #mostly 5's
ord.T.pse1.1 <- clmm2(PSE_T_praise ~ Time, random = S.N.T, data = dat.model.item.long, Hess=T, threshold = "equidistant")
ord.T.pse1.2 <- clmm2(PSE_T_praise ~ Time, random = S.N.T, data = dat.model.item.long, Hess=T, threshold="flexible")
anova(ord.T.pse1.1, ord.T.pse1.2)
ord.T.pse1.null <- clmm2(PSE_T_praise ~ 1, random = S.N.T, data = dat.model.item.long, Hess=T)
anova(ord.T.pse1.2, ord.T.pse1.null) #sig treatment effect
summary(ord.T.pse1.2)
exp(-ord.T.pse1.2$coefficients[6]) #very low relative odds of moving to lower category
cbind(exp(-(ord.T.pse1.2$coefficients[6]+ 2*0.53)), exp(-(ord.T.pse1.2$coefficients[6]- 2*0.53)))

##  PSE2
hist(dat.model.item.PRE$PSE_T_others) #pretty symmetrical
hist(dat.model.item.POST$PSE_T_others) #mostly 5's
ord.T.pse2.1 <- clmm2(PSE_T_others ~ Time, random = S.N.T, data = dat.model.item.long, Hess=T, threshold = "equidistant") 
ord.T.pse2.2 <- clmm2(PSE_T_others ~ Time, random = S.N.T, data = dat.model.item.long, Hess=T, threshold="flexible")
anova(ord.T.pse2.1, ord.T.pse2.2)
ord.T.pse2.null <- clmm2(PSE_T_others ~ 1, random = S.N.T, data = dat.model.item.long, Hess=T)
anova(ord.T.pse2.2, ord.T.pse2.null) #sig treatment effect
summary(ord.T.pse2.2)
exp(-ord.T.pse2.2$coefficients[6]) #very low relative odds of moving to lower
cbind(exp(-(ord.T.pse2.2$coefficients[6]+ 2*0.4108)), exp(-(ord.T.pse1.2$coefficients[6]- 2*0.4108)))


##  MICS
hist(dat.model.item.PRE$MICS_T) #left skewed
hist(dat.model.item.POST$MICS_T) #all zeros
ord.T.mics1 <- clmm2(MICS_T ~Time, random = S.N.T, data=dat.model.item.long, Hess = T, threshold="equidistant")
ord.T.mics2 <- clmm2(MICS_T ~Time, random = S.N.T, data=dat.model.item.long, Hess = T, threshold="flexible")
anova(ord.T.mics1, ord.T.mics2)
ord.T.mics.null <- clmm2(MICS_T ~1, random = S.N.T, data=dat.model.item.long, Hess = T)
anova(ord.T.mics2, ord.T.mics.null) #sig treatment effect
summary(ord.T.mics2)
exp(-ord.T.mics2$coefficients[5]) #infinitely high relative odds of moving to lower category from pre to post


#========================== SCORES =====
#====CAREGIVERS
dat.model.long$APQ.CG <- as.factor(dat.model.long$APQ.CG)
dat.model.long$SDQ.CG <- as.factor(dat.model.long$SDQ.CG)
dat.model.long$ICAST.CG <- as.factor(dat.model.long$ICAST.CG)
dat.model.long$FFC.CG <- as.factor(dat.model.long$FFC.CG)
dat.model.long$APQ.T <- as.factor(dat.model.long$APQ.T)
dat.model.long$SDQ.T <- as.factor(dat.model.long$SDQ.T)
dat.model.long$ICAST.T <- as.factor(dat.model.long$ICAST.T)
str(dat.model.long)

#APQ
ord.cg.apq <- clmm2(APQ.CG ~Time, random = S.N.CG, data=dat.model.long, Hess = T)
ord.cg.apqnull <- clmm2(APQ.CG ~ 1, random = S.N.CG, data=dat.model.long, Hess = T)
anova(ord.cg.apq, ord.cg.apqnull) #sig treatment effect
summary(ord.cg.apq)
exp(-ord.cg.apq$beta)
cbind(exp(-(ord.cg.apq$beta + 2*0.6329)), exp(-(ord.cg.apq$beta - 2*0.6329)))


#SDQ
ord.cg.sdq <- clmm2(SDQ.CG ~Time, random = S.N.CG, data=dat.model.long, Hess = T)
ord.cg.sdqnull <- clmm2(SDQ.CG ~ 1, random = S.N.CG, data=dat.model.long, Hess = T)
anova(ord.cg.sdq, ord.cg.sdqnull) #sig treatment effect
summary(ord.cg.sdq)
exp(-(ord.cg.sdq$beta))
cbind(exp(-(ord.cg.sdq$beta + 2*1.0424)), exp(-(ord.cg.sdq$beta - 2*1.0424)))


#ICAST
ord.cg.ic <- clmm2(ICAST.CG ~Time, random = S.N.CG, data=dat.model.long, Hess = T)
ord.cg.icnull <- clmm2(ICAST.CG ~ 1, random = S.N.CG, data=dat.model.long, Hess = T)
anova(ord.cg.ic, ord.cg.icnull) #sig treatment effect
summary(ord.cg.ic)
exp(-ord.cg.ic$beta)


#FFC
ord.cg.ffc <- clmm2(FFC.CG ~Time, random = S.N.CG, data=dat.model.long, Hess = T)
ord.cg.ffcnull <- clmm2(FFC.CG ~ 1, random = S.N.CG, data=dat.model.long, Hess = T)
anova(ord.cg.ffc, ord.cg.ffcnull) #sig treatment effect
summary(ord.cg.ffc)
exp(-ord.cg.ffc$beta)



#====TEENS
#APQ
ord.t.apq <- clmm2(APQ.T ~Time, random = S.N, data=dat.model.long, Hess = T)
ord.t.apqnull <- clmm2(APQ.T ~ 1, random = S.N, data=dat.model.long, Hess = T)
anova(ord.t.apq, ord.t.apqnull) #sig treatment effect
summary(ord.t.apq)
exp(-ord.t.apq$beta)
cbind(exp(-(ord.t.apq$beta + 2*1.1888)), exp(-(ord.t.apq$beta - 2*1.1888)))


#SDQ
ord.t.sdq <- clmm2(SDQ.T ~Time, random = S.N, data=dat.model.long, Hess = T)
ord.t.sdqnull <- clmm2(SDQ.T ~ 1, random = S.N, data=dat.model.long, Hess = T)
anova(ord.t.sdq, ord.t.sdqnull) #sig treatment effect
summary(ord.t.sdq)
exp(-ord.t.sdq$beta)
cbind(exp(-(ord.t.sdq$beta + 2*1.0447)), exp(-(ord.t.sdq$beta - 2*1.0447)))



#ICAST
ord.t.ic <- clmm2(ICAST.T ~Time, random = S.N, data=dat.model.long, Hess = T)
ord.t.icnull <- clmm2(ICAST.T ~ 1, random = S.N, data=dat.model.long, Hess = T)
anova(ord.t.ic, ord.t.icnull) #sig treatment effect
summary(ord.t.ic)
exp(-ord.t.ic$beta)

