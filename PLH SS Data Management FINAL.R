# PLH - South Sudan - Data Management
rm(list=ls())
setwd("~/OneDrive - University of Cape Town/2022/UNI/Project/South Sudan/PLH survey data (South Sudan)")
library(readxl)
library(tidyverse)
library(dplyr)
library(psych)
library(ggplot2)
library(lavaan)

#import data
datteen.pre <- data.frame(read_xlsx("TEEN_PRE.xlsx")); str(datteen.pre)
datteen.post <- data.frame(read_xlsx("TEEN_POST.xlsx")); str(datteen.post)
datcg.pre <- data.frame(read_xlsx("CG_PRE.xlsx")); str(datcg.pre)
datcg.post <- data.frame(read_xlsx("CG_POST.xlsx")); str(datcg.post)


#=================================DUPLICATES===============================================
#check for duplicated rows
which(duplicated(datcg.pre[,2:8])==TRUE)
which(duplicated(datcg.post[,2:8])==TRUE)
which(duplicated(datteen.pre[,2:8])==TRUE) #no completely duplicated rows
which(duplicated(datteen.post[,2:8])==TRUE) # these are missing data, will be deleted later

#look for rows with duplicate ID + age + gender 
  #caregivers
which(duplicated(datcg.pre[,2])==TRUE) #just ID
datcg.pre[which(duplicated(datcg.pre[,2])==TRUE),2]
which(duplicated(datcg.pre[,2:4])==TRUE) #ID, age, gender
datcg.pre[which(duplicated(datcg.pre[,2:4])==TRUE),2]
#View(datcg.pre[which(datcg.pre$HH.UIC == "NI/ME/HH288"),]) #doesnt match with teen, will be dropped anyway
# View(datcg.pre[which(datcg.pre$HH.UIC == "NI/AB/HH308"),]) #doesnt match with teen, will be dropped anyway
# View(datcg.pre[which(datcg.pre$HH.UIC == "NI/ME/HH021"),]) #seems like two caregivers for the same teen but both say they are mother
 #View(datcg.pre[which(datcg.pre$HH.UIC == "NI/MW/HH037"),])
 
#look in post cg
which(duplicated(datcg.post[,2])==TRUE)  #just ID
datcg.post[which(duplicated(datcg.post[,2])==TRUE),2] #same as pre
which(duplicated(datcg.post[,2:4])==TRUE)  #ID, age, gender
datcg.post[which(duplicated(datcg.post[,2:4])==TRUE),2] #same IDs as PRE

#teens
which(duplicated(datteen.pre$UIC)==TRUE)
sum(duplicated(datteen.pre$UIC)==TRUE) #123 duplicated IDs
datteen.pre[which(duplicated(datteen.pre$UIC)==TRUE),2] #lots of duplicate ID's. probably children from same household
#narrow down by including age and gender
datteen.pre[which(duplicated(datteen.pre[, 2:4])==TRUE),2]
# View(datteen.pre[which(datteen.pre$UIC == "NI/ME/HH136"),]) 
# View(datteen.pre[which(datteen.pre$UIC == "NI/MEHH066"),])
# View(datteen.pre[which(datteen.pre$UIC == "NI/KWHH088"),])
# View(datteen.pre[which(datteen.pre$UIC == "NI/AB/HH260"),])
# View(datteen.pre[which(datteen.pre$UIC == "NI/RC/HH186"),]) #these seem like different children form same HH

#look in post teen
which(duplicated(datteen.post$UIC)==TRUE) #just ID
sum(duplicated(datteen.post$UIC)==TRUE) #138 duplicated IDs, difference from PRE comes from missing data
which(duplicated(datteen.post[,2:4])==TRUE)  #duplicated on ID, age, gender
datteen.post[which(duplicated(datteen.post[, 2:4])==TRUE),2] 
# View(datteen.post[which(datteen.post$UIC == "NI/MW/HH169"),])
# View(datteen.post[which(datteen.post$UIC == "NI/MW/HH332"),])
# View(datteen.post[which(datteen.post$UIC == "NI/AB/HH252"),])
# View(datteen.post[which(datteen.post$UIC == "NI/AB/HH057"),])
# View(datteen.post[which(datteen.post$UIC == "NI/ME/HH285"),])
# View(datteen.post[which(datteen.post$UIC == "NI/ME/HH271"),]) #seem like different children from same HH

#one problematic duplicate has been identified: caregivers - HH.UIC=="NI/ME/HH021"
datcg.pre[which(datcg.pre$HH.UIC == "NI/ME/HH021"),]
datcg.post[which(datcg.post$HH.UIC == "NI/ME/HH021"),]
#need to drop the above duplicate from pre and post - DROP
# datcg.pre <- datcg.pre[-214,]
# datcg.post <- datcg.post[-19,]

#=================================VARIABLE RANGE============================================
#verify variable ranges - scrub those that dont comply
describe(datteen.pre, skew = F)
datteen.pre <- scrub(datteen.pre, where = c("Dem_ChildrenT"), max = 1)
datteen.pre <- scrub(datteen.pre, where = c("Dem_Alcohol", "Dem_Fighting"), max = c(1,1))
datteen.pre <- scrub(datteen.pre, where = c("PRE_SDQ_worried", "PRE_SDQ_fights", "PRE_SDQ_fears"), max = c(2,2, 2))
datteen.pre <- scrub(datteen.pre, where="PRE_MICS_physical_punishment", max = 4)
describe(datteen.pre) #is good

describe(datteen.post, skew = F)
datteen.post <- scrub(datteen.post, where = "POST_SDQ_fears", max=2)
describe(datteen.post, skew = F) #is good

describe(datcg.pre, skew=F)
which(datcg.pre$Dem_ChildAgeCG > 20)
datcg.pre[272, ] #capture error
which(datcg.pre$Dem_ChildAgeCG < 8)
datcg.pre[c(145, 175), ]
datcg.pre <- scrub(datcg.pre, where = "Dem_ChildAgeCG", min=8, max = 20)
describe(datcg.pre, skew=F) #is good

describe(datcg.post, skew = F) #is good

#=================================MISSING DATA=============================================
#check for empty rows
datteen.post[296:311,]#these rows are completely empty - DROP
datteen.post <- datteen.post[-c(296:311),]

#check for empty rows
cg.nacount <- rowSums(is.na(datcg.pre))
cg.nacount #is good
cg.nacount <- rowSums(is.na(datcg.post))
cg.nacount #is good

t.nacount <- rowSums(is.na(datteen.pre))
t.nacount; which(t.nacount>8)
datteen.pre[c(246),] #this may be problematic, half of pre survey blank
t.nacount <- rowSums(is.na(datteen.post))
t.nacount #is good


#investigate response patterns
#=================================MATCHING PRE-POST=============================================
#match pre to post
datteen.join <- inner_join(datteen.pre, datteen.post, by=c("UIC", "Dem_GenderT", "S.N"))
datcg.join <- inner_join(datcg.pre, datcg.post, by=c("HH.UIC", "Dem_GenderCG", "Dem_Relationship"))


#replace post demo with pre demo
datteen.join$Dem_AgeT.y <- datteen.join$Dem_AgeT.x
datteen.join$Dem_ReadT.y <-datteen.join$Dem_ReadT.x
datteen.join$Dem_EducationT.y <- datteen.join$Dem_EducationT.x
datteen.join$Dem_ChildrenT.y <- datteen.join$Dem_ChildrenT.x
datteen.join$Dem_Father.y <- datteen.join$Dem_Father.x
datteen.join$Dem_Mother.y <- datteen.join$Dem_Mother.x
datteen.join$Dem_TB_HIV.y <- datteen.join$Dem_TB_HIV.x
datteen.join$Dem_Alcohol.y <- datteen.join$Dem_Alcohol.x
datteen.join$Dem_Fighting.y <- datteen.join$Dem_Fighting.x

datcg.join$Dem_AgeCG.y <- datcg.join$Dem_AgeCG.x
datcg.join$Dem_ChildAgeCG.y <- datcg.join$Dem_ChildAgeCG.x
datcg.join$Dem_ChildGenderCG.y <- datcg.join$Dem_ChildGenderCG.x
datcg.join$Dem_EducationT.y <- datcg.join$Dem_EducationT.x
datcg.join$Dem_Father.y <- datcg.join$Dem_Father.x
datcg.join$Dem_Mother.y <- datcg.join$Dem_Mother.x
datcg.join$Dem_Poverty.y <- datcg.join$Dem_Poverty.x
datcg.join$Dem_Illness_adult.y <- datcg.join$Dem_Illness_adult.x
datcg.join$Dem_TB_HIV.y <- datcg.join$Dem_TB_HIV.x
datcg.join$Dem_Alcohol.y <- datcg.join$Dem_Alcohol.x
datcg.join$Dem_Fighting.y <- datcg.join$Dem_Fighting.x
datcg.join$Dem_Illness_child.y <- datcg.join$Dem_Illness_child.x
datcg.join$Dem_Disability_child.y <- datcg.join$Dem_Disability_child.x


### check for missing data in prepost data
t.nacount <- rowSums(is.na(datteen.join))
t.nacount
which(t.nacount>10) #this row was identified in line 100
cg.nacount <- rowSums(is.na(datcg.join))
cg.nacount #is good, seems like matching pre-post worked

#save datasets
#write_csv(datcg.join, file = "~/OneDrive - University of Cape Town/2022/UNI/Project/South Sudan/PLH survey data (South Sudan)/datcg.join.csv")
#write_csv(datteen.join, file = "~/OneDrive - University of Cape Town/2022/UNI/Project/South Sudan/PLH survey data (South Sudan)/datteen.join.csv")

#==========================DESCRIPTIVE STATS 1=============================================
#isolate baseline demographic data
datcg.demo <- datcg.join[, 1:17]
datteen.demo <- datteen.join[, 1:12]

#age distribution
hist(datcg.demo$Dem_AgeCG.x, main = "", xlab="Caregiver Age")
hist(datteen.demo$Dem_AgeT.x, main = "", xlab="Adolescent Age")

describe(datcg.demo, skew = F)
summary(as.factor(datcg.demo$Dem_Relationship))
(173+72)/290 #biological parent
1- (173+72)/290 #other
summary(as.factor(datcg.demo$Dem_Father.x))
148/289 #F lives in HH
79/289 # F deceased
summary(as.factor(datcg.demo$Dem_Mother.x))
206/288 # M lives in HH
44/288 # M is deceased

describe(datteen.demo, skew = F)
summary(as.factor(datteen.demo$Dem_ReadT.x))
117/(295-6) #cannot read
145/289 #can read with much difficulty
(18+9)/289 #can read easily or with little difficulty
summary(datteen.pre$PRE_PSE_enrolled)
summary(as.factor(datteen.demo$Dem_Father.x))
149/294 #father lives in HH
70/294 #father deceased
summary(as.factor(datteen.demo$Dem_Mother.x))
207/295 #M in HH
47/295 #M deceased

#child literacy
datteen.demo$Read_labels <- factor(datteen.demo$Dem_ReadT.x, levels = 0:3, labels = c("Cannot read", "Can read with much difficulty", "Can read with little difficulty", "Can read easily"))
ggplot(na.omit(datteen.demo), aes(x=Read_labels)) + geom_bar(stat = "count") + labs(x="Adolescent Literacy Response") + guides(fill="legend")

#child education
ggplot(datteen.demo, aes(x=Dem_EducationT.x)) + geom_bar() + labs(x="Adolescent Education Level")

#==========================RESPONSE DISTRIBUTIONS 1 ================================
# STACKED BARPLOTS
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
#function to find hexdex cols to make categories same cols
gg_color_hue <- function(n) {
     hues = seq(15, 375, length = n + 1)
     hcl(h = hues, l = 65, c = 100)[1:n]
}


######CAREGIVERS
# PRE.APQ.caregivers.wide <- datcg.join[,18:23] #isolate PRE_APQ
# caregiver_ID <- c(1:290) #append with ID column
# PRE.APQ.caregivers.wide <- cbind(caregiver_ID, PRE.APQ.caregivers.wide)
# PRE.APQ.caregivers.long <- PRE.APQ.caregivers.wide %>% gather(key='Question_num', value='Answer', -caregiver_ID)
# PRE.APQ.caregivers.long$Answer <- factor(PRE.APQ.caregivers.long$Answer, levels=0:4,
#                                          labels=c("Never", "Almost Never", "Sometimes", "Often", "Always"))
# p.cg.1a <- ggplot(na.omit(PRE.APQ.caregivers.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip()
# 
# POST.APQ.caregivers.wide <- datcg.join[,59:64] #same for POST
# POST.APQ.caregivers.wide <- cbind(caregiver_ID, POST.APQ.caregivers.wide)
# POST.APQ.caregivers.long <- POST.APQ.caregivers.wide %>% gather(key='Question_num', value='Answer', -caregiver_ID)
# POST.APQ.caregivers.long$Answer <- factor(POST.APQ.caregivers.long$Answer, levels=0:4,
#                                           labels=c("Never", "Almost Never", "Sometimes", "Often", "Always"))
# p.cg.1b <- ggplot(na.omit(POST.APQ.caregivers.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip()
# 
# #SDQ
# PRE.SDQ.caregivers.wide <- datcg.join[,24:28]
# PRE.SDQ.caregivers.wide <- cbind(caregiver_ID, PRE.SDQ.caregivers.wide)
# PRE.SDQ.caregivers.long <- PRE.SDQ.caregivers.wide %>% gather(key='Question_num', value='Answer', -caregiver_ID)
# PRE.SDQ.caregivers.long$Answer <- factor(PRE.SDQ.caregivers.long$Answer, levels=0:2,
#                                          labels=c("Not True", "Somewhat True", "Very True"))
# 
# p.cg.2a <- ggplot(na.omit(PRE.SDQ.caregivers.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip()
# 
# POST.SDQ.caregivers.wide <- datcg.join[,65:69]
# POST.SDQ.caregivers.wide <- cbind(caregiver_ID, POST.SDQ.caregivers.wide)
# POST.SDQ.caregivers.long <- POST.SDQ.caregivers.wide %>% gather(key='Question_num', value='Answer', -caregiver_ID)
# POST.SDQ.caregivers.long$Answer <- factor(POST.SDQ.caregivers.long$Answer, levels=0:2,
#                                           labels=c("Not True", "Somewhat True", "Very True"))
# 
# p.cg.2b <- ggplot(na.omit(POST.SDQ.caregivers.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip() +
#   scale_fill_manual(values = c("#F8766D", "#00BA38"))
# 
# #ICAST
# PRE.ICAST.caregivers.wide <- datcg.join[,29:32]
# PRE.ICAST.caregivers.wide <- cbind(caregiver_ID, PRE.ICAST.caregivers.wide)
# PRE.ICAST.caregivers.long <- PRE.ICAST.caregivers.wide %>% gather(key='Question_num', value='Answer', -caregiver_ID)
# PRE.ICAST.caregivers.long$Answer <- factor(PRE.ICAST.caregivers.long$Answer, levels=0:8,
#                                            labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8+"))
# p.cg.3a <- ggplot(na.omit(PRE.ICAST.caregivers.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip()
# 
# POST.ICAST.caregivers.wide <- datcg.join[,70:73]
# POST.ICAST.caregivers.wide <- cbind(caregiver_ID, POST.ICAST.caregivers.wide)
# POST.ICAST.caregivers.long <- POST.ICAST.caregivers.wide %>% gather(key='Question_num', value='Answer', -caregiver_ID)
# POST.ICAST.caregivers.long$Answer <- factor(POST.ICAST.caregivers.long$Answer, levels=0:8,
#                                             labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8+"))
# p.cg.3b <- ggplot(na.omit(POST.ICAST.caregivers.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip()
# 
# #CESD
# PRE.CESD.caregivers.wide <- datcg.join[,34:36]
# PRE.CESD.caregivers.wide <- cbind(caregiver_ID, PRE.CESD.caregivers.wide)
# PRE.CESD.caregivers.long <- PRE.CESD.caregivers.wide %>% gather(key='Question_num', value='Answer', -caregiver_ID)
# PRE.CESD.caregivers.long$Answer <- factor(PRE.CESD.caregivers.long$Answer, levels=0:3,
#                                           labels=c("< 1 day", "1-2 days", "3-4 days", "5-7 days"))
# p.cg.4a <- ggplot(na.omit(PRE.CESD.caregivers.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip() +
#   scale_fill_manual(values = c("#00BFC4", "#C77CFF"))
# 
# POST.CESD.caregivers.wide <- datcg.join[,75:77]
# POST.CESD.caregivers.wide <- cbind(caregiver_ID, POST.CESD.caregivers.wide)
# POST.CESD.caregivers.long <- POST.CESD.caregivers.wide %>% gather(key='Question_num', value='Answer', -caregiver_ID)
# POST.CESD.caregivers.long$Answer <- factor(POST.CESD.caregivers.long$Answer, levels=0:3,
#                                            labels=c("< 1 day", "1-2 days", "3-4 days", "5-7 days"))
# p.cg.4b <- ggplot(na.omit(POST.CESD.caregivers.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip()
# 
# #FFC
# PRE.FFC.caregivers.wide <- datcg.join[,37:41]
# PRE.FFC.caregivers.wide <- cbind(caregiver_ID, PRE.FFC.caregivers.wide)
# PRE.FFC.caregivers.long <- PRE.FFC.caregivers.wide %>% gather(key='Question_num', value='Answer', -caregiver_ID)
# PRE.FFC.caregivers.long$Answer <- factor(PRE.FFC.caregivers.long$Answer, levels=0:3,
#                                          labels=c("Never", "Rarely", "Sometimes", "Often"))
# p.cg.5a <- ggplot(na.omit(PRE.FFC.caregivers.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip()
# 
# POST.FFC.caregivers.wide <- datcg.join[,78:82]
# POST.FFC.caregivers.wide <- cbind(caregiver_ID, POST.FFC.caregivers.wide)
# POST.FFC.caregivers.long <- POST.FFC.caregivers.wide %>% gather(key='Question_num', value='Answer', -caregiver_ID)
# POST.FFC.caregivers.long$Answer <- factor(POST.FFC.caregivers.long$Answer, levels=0:3,
#                                           labels=c("Never", "Rarely", "Sometimes", "Often"))
# p.cg.5b <- ggplot(na.omit(POST.FFC.caregivers.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip()
# 
# #PSE
# PRE.PSE.caregivers.wide <- datcg.join[,43:44]
# PRE.PSE.caregivers.wide <- cbind(caregiver_ID, PRE.PSE.caregivers.wide)
# PRE.PSE.caregivers.long <- PRE.PSE.caregivers.wide %>% gather(key='Question_num', value='Answer', -caregiver_ID)
# PRE.PSE.caregivers.long$Answer <- factor(PRE.PSE.caregivers.long$Answer, levels=1:5,
#                                          labels=c("Never", "Hardly ever", "Sometimes", "Most of the time", "Almost every day"))
# p.cg.6a <- ggplot(na.omit(PRE.PSE.caregivers.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip() +
#   scale_fill_manual(values = "#00BF7D")
# 
# POST.PSE.caregivers.wide <- datcg.join[,84:85]
# POST.PSE.caregivers.wide <- cbind(caregiver_ID, POST.PSE.caregivers.wide)
# POST.PSE.caregivers.long <- POST.PSE.caregivers.wide %>% gather(key='Question_num', value='Answer', -caregiver_ID)
# POST.PSE.caregivers.long$Answer <- factor(POST.PSE.caregivers.long$Answer, levels=1:5,
#                                           labels=c("Never", "Hardly ever", "Sometimes", "Most of the time", "Almost every day"))
# p.cg.6b <- ggplot(na.omit(POST.PSE.caregivers.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip() 
# 
# 
# ggarrange(p.cg.1a, p.cg.1b, p.cg.2a, p.cg.2b, p.cg.3a, p.cg.3b, ncol = 2, nrow = 3)
# ggarrange(p.cg.4a, p.cg.4b, p.cg.5a, p.cg.5b, p.cg.6a, p.cg.6b, ncol=2, nrow = 3)
# 
# # PSE enrolled and MICS physical punishment
# PREPOST.enrolled.caregivers.wide <- datcg.join[,c(42,83)]
# PREPOST.enrolled.caregivers.wide <- cbind(caregiver_ID, PREPOST.enrolled.caregivers.wide)
# PREPOST.enrolled.caregivers.long <- PREPOST.enrolled.caregivers.wide %>% gather(key='Question_num', value='Answer', -caregiver_ID)
# PREPOST.enrolled.caregivers.long$Answer <- factor(PREPOST.enrolled.caregivers.long$Answer, levels=0:1,
#                                                   labels=c("No", "Yes"))
# p.cg.7 <- ggplot(na.omit(PREPOST.enrolled.caregivers.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip()
# 
# PREPOST.MICS.caregivers.wide <- datcg.join[,c(33,74)]
# PREPOST.MICS.caregivers.wide <- cbind(caregiver_ID, PREPOST.MICS.caregivers.wide)
# PREPOST.MICS.caregivers.long <- PREPOST.MICS.caregivers.wide %>% gather(key='Question_num', value='Answer', -caregiver_ID)
# PREPOST.MICS.caregivers.long$Answer <- factor(PREPOST.MICS.caregivers.long$Answer, levels=0:4,
#                                               labels=c("Strongly disagree", "Disagree", "Not sure", "Agree", "Strongly agree"))
# p.cg.8 <- ggplot(na.omit(PREPOST.MICS.caregivers.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip()
# 
# ######TEENS
# PRE.APQ.teens.wide <- datteen.join[,13:18]
# teen_ID <- c(1:295)
# PRE.APQ.teens.wide <- cbind(teen_ID, PRE.APQ.teens.wide)
# PRE.APQ.teens.long <- PRE.APQ.teens.wide %>% gather(key='Question_num', value='Answer', -teen_ID)
# PRE.APQ.teens.long$Answer <- factor(PRE.APQ.teens.long$Answer, levels=0:4,
#                                     labels=c("Never", "Almost Never", "Sometimes", "Often", "Always"))
# p.t.1a <- ggplot(na.omit(PRE.APQ.teens.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip()
# 
# POST.APQ.teens.wide <- datteen.join[,46:51]
# POST.APQ.teens.wide <- cbind(teen_ID, POST.APQ.teens.wide)
# POST.APQ.teens.long <- POST.APQ.teens.wide %>% gather(key='Question_num', value='Answer', -teen_ID)
# POST.APQ.teens.long$Answer <- factor(POST.APQ.teens.long$Answer, levels=0:4,
#                                      labels=c("Never", "Almost Never", "Sometimes", "Often", "Always"))
# p.t.1b <- ggplot(na.omit(POST.APQ.teens.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip()
# 
# #SDQ
# PRE.SDQ.teens.wide <- datteen.join[,19:28]
# PRE.SDQ.teens.wide <- cbind(teen_ID, PRE.SDQ.teens.wide)
# PRE.SDQ.teens.long <- PRE.SDQ.teens.wide %>% gather(key='Question_num', value='Answer', -teen_ID)
# PRE.SDQ.teens.long$Answer <- factor(PRE.SDQ.teens.long$Answer, levels=0:2,
#                                     labels=c("Not True", "Somewhat True", "Very True"))
# p.t.2a <- ggplot(na.omit(PRE.SDQ.teens.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip()
# 
# POST.SDQ.teens.wide <- datteen.join[,52:61]
# POST.SDQ.teens.wide <- cbind(teen_ID, POST.SDQ.teens.wide)
# POST.SDQ.teens.long <- POST.SDQ.teens.wide %>% gather(key='Question_num', value='Answer', -teen_ID)
# POST.SDQ.teens.long$Answer <- factor(POST.SDQ.teens.long$Answer, levels=0:2,
#                                      labels=c("Not True", "Somewhat True", "Very True"))
# p.t.2b <- ggplot(na.omit(POST.SDQ.teens.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip()
# 
# #ICAST
# PRE.ICAST.teens.wide <- datteen.join[,29:32]
# PRE.ICAST.teens.wide <- cbind(teen_ID, PRE.ICAST.teens.wide)
# PRE.ICAST.teens.long <- PRE.ICAST.teens.wide %>% gather(key='Question_num', value='Answer', -teen_ID)
# PRE.ICAST.teens.long$Answer <- factor(PRE.ICAST.teens.long$Answer, levels=0:8,
#                                       labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8+"))
# p.t.3a <- ggplot(na.omit(PRE.ICAST.teens.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip()
# 
# POST.ICAST.teens.wide <- datteen.join[,62:65]
# POST.ICAST.teens.wide <- cbind(teen_ID, POST.ICAST.teens.wide)
# POST.ICAST.teens.long <- POST.ICAST.teens.wide %>% gather(key='Question_num', value='Answer', -teen_ID)
# POST.ICAST.teens.long$Answer <- factor(POST.ICAST.teens.long$Answer, levels=0:8,
#                                        labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8+"))
# p.t.3b <- ggplot(na.omit(POST.ICAST.teens.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip()
# 
# #PSE
# PRE.PSE.teens.wide <- datteen.join[,35:36]
# PRE.PSE.teens.wide <- cbind(teen_ID, PRE.PSE.teens.wide)
# PRE.PSE.teens.long <- PRE.PSE.teens.wide %>% gather(key='Question_num', value='Answer', -teen_ID)
# PRE.PSE.teens.long$Answer <- factor(PRE.PSE.teens.long$Answer, levels=1:5,
#                                     labels=c("Never", "Hardly ever", "Sometimes", "Most of the time", "Almost every day"))
# p.t.4a <- ggplot(na.omit(PRE.PSE.teens.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip()
# 
# POST.PSE.teens.wide <- datteen.join[,68:69]
# POST.PSE.teens.wide <- cbind(teen_ID, POST.PSE.teens.wide)
# POST.PSE.teens.long <- POST.PSE.teens.wide %>% gather(key='Question_num', value='Answer', -teen_ID)
# POST.PSE.teens.long$Answer <- factor(POST.PSE.teens.long$Answer, levels=1:5,
#                                      labels=c("Never", "Hardly ever", "Sometimes", "Most of the time", "Almost every day"))
# p.t.4b <- ggplot(na.omit(POST.PSE.teens.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip()
# 
# ggarrange(p.t.1a, p.t.1b, p.t.2a, p.t.2b, p.t.3a, p.t.3b, p.t.4a, p.t.4b, ncol=2, nrow=4)
# 
# 
# # PSE enrolled and MICS physical punishment
# PREPOST.enrolled.teens.wide <- datteen.join[,c(34,67)]
# PREPOST.enrolled.teens.wide <- cbind(teen_ID, PREPOST.enrolled.teens.wide)
# PREPOST.enrolled.teens.long <- PREPOST.enrolled.teens.wide %>% gather(key='Question_num', value='Answer', -teen_ID)
# PREPOST.enrolled.teens.long$Answer <- factor(PREPOST.enrolled.teens.long$Answer, levels=0:1,
#                                              labels=c("No", "Yes"))
# p.t.5 <- ggplot(na.omit(PREPOST.enrolled.teens.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip()
# 
# PREPOST.MICS.teens.wide <- datteen.join[,c(33,66)]
# PREPOST.MICS.teens.wide <- cbind(teen_ID, PREPOST.MICS.teens.wide)
# PREPOST.MICS.teens.long <- PREPOST.MICS.teens.wide %>% gather(key='Question_num', value='Answer', -teen_ID)
# PREPOST.MICS.teens.long$Answer <- factor(PREPOST.MICS.teens.long$Answer, levels=0:4,
#                                          labels=c("Strongly disagree", "Disagree", "Not sure", "Agree", "Strongly agree"))
# p.t.6 <- ggplot(na.omit(PREPOST.MICS.teens.long), aes(x=Question_num)) +
#   geom_bar(aes(fill=Answer), position = "fill") + ylab("Proportion") + xlab("Item") + coord_flip()
# 
# 
# 
# 
# 
# #Individual item barplots
# #####caregivers pre&post
# ##APQ
# #Talk
# p.cg.apq1a <- ggplot(PRE.APQ.caregivers.long[which(PRE.APQ.caregivers.long$Question_num=="PRE_APQ_talk"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_APQ_talk",y="Frequency", x="Response")
# p.cg.apq1b <- ggplot(POST.APQ.caregivers.long[which(POST.APQ.caregivers.long$Question_num=="POST_APQ_talk"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_APQ_talk",y="Frequency", x="Response")
# #Involved
# p.cg.apq2a <- ggplot(PRE.APQ.caregivers.long[which(PRE.APQ.caregivers.long$Question_num=="PRE_APQ_involved"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_APQ_involved",y="Frequency", x="Response")
# p.cg.apq2b <- ggplot(POST.APQ.caregivers.long[which(POST.APQ.caregivers.long$Question_num=="POST_APQ_involved"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_APQ_involved",y="Frequency", x="Response")
# #Talk friends
# p.cg.apq3a <- ggplot(PRE.APQ.caregivers.long[which(PRE.APQ.caregivers.long$Question_num=="PRE_APQ_talk_friends"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_APQ_talk_friends",y="Frequency", x="Response")
# p.cg.apq3b <- ggplot(POST.APQ.caregivers.long[which(POST.APQ.caregivers.long$Question_num=="POST_APQ_talk_friends"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_APQ_talk_friends",y="Frequency", x="Response")
# #Evening
# p.cg.apq4a <- ggplot(PRE.APQ.caregivers.long[which(PRE.APQ.caregivers.long$Question_num=="PRE_APQ_evening"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_APQ_evening",y="Frequency", x="Response")
# p.cg.apq4b <- ggplot(POST.APQ.caregivers.long[which(POST.APQ.caregivers.long$Question_num=="POST_APQ_evening"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_APQ_evening",y="Frequency", x="Response")
# #Time
# p.cg.apq5a <- ggplot(PRE.APQ.caregivers.long[which(PRE.APQ.caregivers.long$Question_num=="PRE_APQ_time"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_APQ_time",y="Frequency", x="Response")
# p.cg.apq5b <- ggplot(POST.APQ.caregivers.long[which(POST.APQ.caregivers.long$Question_num=="POST_APQ_time"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_APQ_time",y="Frequency", x="Response")
# #Dark
# p.cg.apq6a <- ggplot(PRE.APQ.caregivers.long[which(PRE.APQ.caregivers.long$Question_num=="PRE_APQ_dark"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_APQ_dark",y="Frequency", x="Response")
# p.cg.apq6b <- ggplot(POST.APQ.caregivers.long[which(POST.APQ.caregivers.long$Question_num=="POST_APQ_dark"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_APQ_dark",y="Frequency", x="Response")
# ggarrange(p.cg.apq1a, p.cg.apq1b, p.cg.apq2a, p.cg.apq2b, p.cg.apq3a, p.cg.apq3b, p.cg.apq4a, p.cg.apq4b, p.cg.apq5a, p.cg.apq5b, p.cg.apq6a,p.cg.apq6b, ncol=2, nrow=6)
# ##SDQ
# #tantrums
# p.cg.sdq1a <- ggplot(PRE.SDQ.caregivers.long[which(PRE.SDQ.caregivers.long$Question_num=="PRE_SDQ_tantrums"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_SDQ_tantrums",y="Frequency", x="Response")
# p.cg.sdq1b <- ggplot(POST.SDQ.caregivers.long[which(POST.SDQ.caregivers.long$Question_num=="POST_SDQ_tantrums"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_SDQ_tantrums",y="Frequency", x="Response")
# #obedient
# p.cg.sdq2a <- ggplot(PRE.SDQ.caregivers.long[which(PRE.SDQ.caregivers.long$Question_num=="PRE_SDQ_obedient"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_SDQ_obedient",y="Frequency", x="Response")
# p.cg.sdq2b <- ggplot(POST.SDQ.caregivers.long[which(POST.SDQ.caregivers.long$Question_num=="POST_SDQ_obedient"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_SDQ_obedient",y="Frequency", x="Response")
# #Fights
# p.cg.sdq3a <- ggplot(PRE.SDQ.caregivers.long[which(PRE.SDQ.caregivers.long$Question_num=="PRE_SDQ_fights"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_SDQ_fights",y="Frequency", x="Response")
# p.cg.sdq3b <- ggplot(POST.SDQ.caregivers.long[which(POST.SDQ.caregivers.long$Question_num=="POST_SDQ_fights"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_SDQ_fights",y="Frequency", x="Response")
# #lies
# p.cg.sdq4a <- ggplot(PRE.SDQ.caregivers.long[which(PRE.SDQ.caregivers.long$Question_num=="PRE_SDQ_lies"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_SDQ_lies",y="Frequency", x="Response")
# p.cg.sdq4b <- ggplot(POST.SDQ.caregivers.long[which(POST.SDQ.caregivers.long$Question_num=="POST_SDQ_lies"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_SDQ_lies",y="Frequency", x="Response")
# #steals
# p.cg.sdq5a <- ggplot(PRE.SDQ.caregivers.long[which(PRE.SDQ.caregivers.long$Question_num=="PRE_SDQ_steals"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_SDQ_steals",y="Frequency", x="Response")
# p.cg.sdq5b <- ggplot(POST.SDQ.caregivers.long[which(POST.SDQ.caregivers.long$Question_num=="POST_SDQ_steals"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_SDQ_steals",y="Frequency", x="Response")
# ggarrange(p.cg.sdq1a, p.cg.sdq1b, p.cg.sdq2a, p.cg.sdq2b, p.cg.sdq3a, p.cg.sdq3b, p.cg.sdq4a, p.cg.sdq4b, p.cg.sdq5a, p.cg.sdq5b, ncol=2, nrow=5)
# 
# ##ICAST
# #spank
# p.cg.icast1a <- ggplot(PRE.ICAST.caregivers.long[which(PRE.ICAST.caregivers.long$Question_num=="PRE_ICAST_spank"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_ICAST_spank",y="Frequency", x="Response")
# p.cg.icast1b <- ggplot(POST.ICAST.caregivers.long[which(POST.ICAST.caregivers.long$Question_num=="POST_ICAST_spank"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_ICAST_spank",y="Frequency", x="Response")
# #object
# p.cg.icast2a <- ggplot(PRE.ICAST.caregivers.long[which(PRE.ICAST.caregivers.long$Question_num=="PRE_ICAST_object"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_ICAST_object",y="Frequency", x="Response")
# p.cg.icast2b <- ggplot(POST.ICAST.caregivers.long[which(POST.ICAST.caregivers.long$Question_num=="POST_ICAST_object"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_ICAST_object",y="Frequency", x="Response")
# #scream
# p.cg.icast3a <- ggplot(PRE.ICAST.caregivers.long[which(PRE.ICAST.caregivers.long$Question_num=="PRE_ICAST_scream"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_ICAST_scream",y="Frequency", x="Response")
# p.cg.icast3b <- ggplot(POST.ICAST.caregivers.long[which(POST.ICAST.caregivers.long$Question_num=="POST_ICAST_scream"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_ICAST_scream",y="Frequency", x="Response")
# #upset
# p.cg.icast4a <- ggplot(PRE.ICAST.caregivers.long[which(PRE.ICAST.caregivers.long$Question_num=="PRE_ICAST_upset"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_ICAST_upset",y="Frequency", x="Response")
# p.cg.icast4b <- ggplot(POST.ICAST.caregivers.long[which(POST.ICAST.caregivers.long$Question_num=="POST_ICAST_upset"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_ICAST_upset",y="Frequency", x="Response")
# ggarrange(p.cg.icast1a, p.cg.icast1b, p.cg.icast2a, p.cg.icast2b, p.cg.icast3a, p.cg.icast3b, p.cg.icast4a, p.cg.icast4b, ncol=2, nrow = 4)
# 
# ##CESD
# #depressed
# p.cg.cesd1a <- ggplot(PRE.CESD.caregivers.long[which(PRE.CESD.caregivers.long$Question_num=="PRE_CESD_depressed"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_CESD_depressed",y="Frequency", x="Response")
# p.cg.cesd1b <- ggplot(POST.CESD.caregivers.long[which(POST.CESD.caregivers.long$Question_num=="POST_CESD_depressed"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_CESD_depressed",y="Frequency", x="Response")
# #effort
# p.cg.cesd2a <- ggplot(PRE.CESD.caregivers.long[which(PRE.CESD.caregivers.long$Question_num=="PRE_CESD_effort"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_CESD_effort",y="Frequency", x="Response")
# p.cg.cesd2b <- ggplot(POST.CESD.caregivers.long[which(POST.CESD.caregivers.long$Question_num=="POST_CESD_effort"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_CESD_effort",y="Frequency", x="Response")
# #lonely
# p.cg.cesd3a <- ggplot(PRE.CESD.caregivers.long[which(PRE.CESD.caregivers.long$Question_num=="PRE_CESD_lonely"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_CESD_lonely",y="Frequency", x="Response")
# p.cg.cesd3b <- ggplot(POST.CESD.caregivers.long[which(POST.CESD.caregivers.long$Question_num=="POST_CESD_lonely"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_CESD_lonely",y="Frequency", x="Response")
# ggarrange(p.cg.cesd1a, p.cg.cesd1b, p.cg.cesd2a, p.cg.cesd2b, p.cg.cesd3a, p.cg.cesd3b, ncol = 2, nrow=3)
# ##FFC
# #meat
# p.cg.ffc1a <- ggplot(PRE.FFC.caregivers.long[which(PRE.FFC.caregivers.long$Question_num=="PRE_FFC_meat"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_FFC_meat",y="Frequency", x="Response")
# p.cg.ffc1b <- ggplot(POST.FFC.caregivers.long[which(POST.FFC.caregivers.long$Question_num=="POST_FFC_meat"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_FFC_meat",y="Frequency", x="Response")
# #electricity
# p.cg.ffc2a <- ggplot(PRE.FFC.caregivers.long[which(PRE.FFC.caregivers.long$Question_num=="PRE_FFC_electricity"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_FFC_electricity",y="Frequency", x="Response")
# p.cg.ffc2b <- ggplot(POST.FFC.caregivers.long[which(POST.FFC.caregivers.long$Question_num=="POST_FFC_electricity"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_FFC_electricity",y="Frequency", x="Response")
# #transport
# p.cg.ffc3a <- ggplot(PRE.FFC.caregivers.long[which(PRE.FFC.caregivers.long$Question_num=="PRE_FFC_transport"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_FFC_transport",y="Frequency", x="Response")
# p.cg.ffc3b <- ggplot(POST.FFC.caregivers.long[which(POST.FFC.caregivers.long$Question_num=="POST_FFC_transport"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_FFC_transport",y="Frequency", x="Response")
# #airtime
# p.cg.ffc4a <- ggplot(PRE.FFC.caregivers.long[which(PRE.FFC.caregivers.long$Question_num=="PRE_FFC_airtime"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_FFC_airtime",y="Frequency", x="Response")
# p.cg.ffc4b <- ggplot(POST.FFC.caregivers.long[which(POST.FFC.caregivers.long$Question_num=="POST_FFC_airtime"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_FFC_airtime",y="Frequency", x="Response")
# #worried
# p.cg.ffc5a <- ggplot(PRE.FFC.caregivers.long[which(PRE.FFC.caregivers.long$Question_num=="PRE_FFC_worried"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_FFC_worried",y="Frequency", x="Response")
# p.cg.ffc5b <- ggplot(POST.FFC.caregivers.long[which(POST.FFC.caregivers.long$Question_num=="POST_FFC_worried"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_FFC_worried",y="Frequency", x="Response")
# ggarrange(p.cg.ffc1a,p.cg.ffc1b, p.cg.ffc2a, p.cg.ffc2b, p.cg.ffc3a, p.cg.ffc3b, p.cg.ffc4a, p.cg.ffc4b, p.cg.ffc5a, p.cg.ffc5b, ncol=2, nrow = 5)
# ##PSE
# #praise
# p.cg.pse1a <- ggplot(PRE.PSE.caregivers.long[which(PRE.PSE.caregivers.long$Question_num=="PRE_PSE_praise"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_PSE_praise",y="Frequency", x="Response")
# p.cg.pse1b <- ggplot(POST.PSE.caregivers.long[which(POST.PSE.caregivers.long$Question_num=="POST_PSE_praise"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_PSE_praise",y="Frequency", x="Response")
# #other
# p.cg.pse2a <- ggplot(PRE.PSE.caregivers.long[which(PRE.PSE.caregivers.long$Question_num=="PRE_PSE_others"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_PSE_others",y="Frequency", x="Response")
# p.cg.pse2b <- ggplot(POST.PSE.caregivers.long[which(POST.PSE.caregivers.long$Question_num=="POST_PSE_others"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_PSE_others",y="Frequency", x="Response")
# ggarrange(p.cg.pse1a, p.cg.pse1b, p.cg.pse2a, p.cg.pse2b, ncol = 2, nrow = 2)
# 
# #####teens pre&post
# ##APQ
# #Talk
# p.t.apq1a <- ggplot(PRE.APQ.teens.long[which(PRE.APQ.teens.long$Question_num=="PRE_APQ_talk"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_APQ_talk",y="Frequency", x="Response")
# p.t.apq1b <- ggplot(POST.APQ.teens.long[which(POST.APQ.teens.long$Question_num=="POST_APQ_talk"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_APQ_talk",y="Frequency", x="Response")
# #Involved
# p.t.apq2a <- ggplot(PRE.APQ.teens.long[which(PRE.APQ.teens.long$Question_num=="PRE_APQ_involved"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_APQ_involved",y="Frequency", x="Response")
# p.t.apq2b <- ggplot(POST.APQ.teens.long[which(POST.APQ.teens.long$Question_num=="POST_APQ_involved"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_APQ_involved",y="Frequency", x="Response")
# #Talk friends
# p.t.apq3a <- ggplot(PRE.APQ.teens.long[which(PRE.APQ.teens.long$Question_num=="PRE_APQ_talk_friends"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_APQ_talk_friends",y="Frequency", x="Response")
# p.t.apq3b <- ggplot(POST.APQ.teens.long[which(POST.APQ.teens.long$Question_num=="POST_APQ_talk_friends"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_APQ_talk_friends",y="Frequency", x="Response")
# #Evening
# p.t.apq4a <- ggplot(PRE.APQ.teens.long[which(PRE.APQ.teens.long$Question_num=="PRE_APQ_evening"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_APQ_evening",y="Frequency", x="Response")
# p.t.apq4b <- ggplot(POST.APQ.teens.long[which(POST.APQ.teens.long$Question_num=="POST_APQ_evening"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_APQ_evening",y="Frequency", x="Response")
# #Time
# p.t.apq5a <- ggplot(PRE.APQ.teens.long[which(PRE.APQ.teens.long$Question_num=="PRE_APQ_time"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_APQ_time",y="Frequency", x="Response")
# p.t.apq5b <- ggplot(POST.APQ.teens.long[which(POST.APQ.teens.long$Question_num=="POST_APQ_time"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_APQ_time",y="Frequency", x="Response")
# #Dark
# p.t.apq6a <- ggplot(PRE.APQ.teens.long[which(PRE.APQ.teens.long$Question_num=="PRE_APQ_dark"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_APQ_dark",y="Frequency", x="Response")
# p.t.apq6b <- ggplot(POST.APQ.teens.long[which(POST.APQ.teens.long$Question_num=="POST_APQ_dark"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_APQ_dark",y="Frequency", x="Response")
# ggarrange(p.t.apq1a, p.t.apq1b, p.t.apq2a, p.t.apq2b, p.t.apq3a, p.t.apq3b, p.t.apq4a, p.t.apq4b, p.t.apq5a, p.t.apq5b, p.t.apq6a, p.t.apq6b, ncol=2, nrow=6)
# 
# ##SDQ
# #headaches
# p.t.sdq1a <- ggplot(PRE.SDQ.teens.long[which(PRE.SDQ.teens.long$Question_num=="PRE_SDQ_headaches"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_SDQ_headaches",y="Frequency", x="Response")
# p.t.sdq1b <- ggplot(POST.SDQ.teens.long[which(POST.SDQ.teens.long$Question_num=="POST_SDQ_headaches"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_SDQ_headaches",y="Frequency", x="Response")
# #tantrums
# p.t.sdq2a <- ggplot(PRE.SDQ.teens.long[which(PRE.SDQ.teens.long$Question_num=="PRE_SDQ_tantrums"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_SDQ_tantrums",y="Frequency", x="Response")
# p.t.sdq2b <- ggplot(POST.SDQ.teens.long[which(POST.SDQ.teens.long$Question_num=="POST_SDQ_tantrums"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_SDQ_tantrums",y="Frequency", x="Response")
# #obedient
# p.t.sdq3a <- ggplot(PRE.SDQ.teens.long[which(PRE.SDQ.teens.long$Question_num=="PRE_SDQ_obedient"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_SDQ_obedient",y="Frequency", x="Response")
# p.t.sdq3b <- ggplot(POST.SDQ.teens.long[which(POST.SDQ.teens.long$Question_num=="POST_SDQ_obedient"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_SDQ_obedient",y="Frequency", x="Response")
# #worried
# p.t.sdq4a <- ggplot(PRE.SDQ.teens.long[which(PRE.SDQ.teens.long$Question_num=="PRE_SDQ_worried"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_SDQ_worried",y="Frequency", x="Response")
# p.t.sdq4b <- ggplot(POST.SDQ.teens.long[which(POST.SDQ.teens.long$Question_num=="POST_SDQ_worried"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_SDQ_worried",y="Frequency", x="Response")
# #Fights
# p.t.sdq5a <- ggplot(PRE.SDQ.teens.long[which(PRE.SDQ.teens.long$Question_num=="PRE_SDQ_fights"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_SDQ_fights",y="Frequency", x="Response")
# p.t.sdq5b <- ggplot(POST.SDQ.teens.long[which(POST.SDQ.teens.long$Question_num=="POST_SDQ_fights"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_SDQ_fights",y="Frequency", x="Response")
# #unhappy
# p.t.sdq6a <- ggplot(PRE.SDQ.teens.long[which(PRE.SDQ.teens.long$Question_num=="PRE_SDQ_unhappy"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_SDQ_unhappy",y="Frequency", x="Response")
# p.t.sdq6b <- ggplot(POST.SDQ.teens.long[which(POST.SDQ.teens.long$Question_num=="POST_SDQ_unhappy"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_SDQ_unhappy",y="Frequency", x="Response")
# #new
# p.t.sdq7a <- ggplot(PRE.SDQ.teens.long[which(PRE.SDQ.teens.long$Question_num=="PRE_SDQ_new"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_SDQ_new",y="Frequency", x="Response")
# p.t.sdq7b <- ggplot(POST.SDQ.teens.long[which(POST.SDQ.teens.long$Question_num=="POST_SDQ_new"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_SDQ_new",y="Frequency", x="Response")
# #lies
# p.t.sdq8a <- ggplot(PRE.SDQ.teens.long[which(PRE.SDQ.teens.long$Question_num=="PRE_SDQ_lies"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_SDQ_lies",y="Frequency", x="Response")
# p.t.sdq8b <- ggplot(POST.SDQ.teens.long[which(POST.SDQ.teens.long$Question_num=="POST_SDQ_lies"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_SDQ_lies",y="Frequency", x="Response")
# #steals
# p.t.sdq9a <- ggplot(PRE.SDQ.teens.long[which(PRE.SDQ.teens.long$Question_num=="PRE_SDQ_steals"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_SDQ_steals",y="Frequency", x="Response")
# p.t.sdq9b <- ggplot(POST.SDQ.teens.long[which(POST.SDQ.teens.long$Question_num=="POST_SDQ_steals"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_SDQ_steals",y="Frequency", x="Response")
# #fears
# p.t.sdq10a <- ggplot(PRE.SDQ.teens.long[which(PRE.SDQ.teens.long$Question_num=="PRE_SDQ_fears"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_SDQ_fears",y="Frequency", x="Response")
# p.t.sdq10b <- ggplot(POST.SDQ.teens.long[which(POST.SDQ.teens.long$Question_num=="POST_SDQ_fears"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_SDQ_fears",y="Frequency", x="Response")
# ggarrange(p.t.sdq1a, p.t.sdq1b, p.t.sdq2a, p.t.sdq2b, p.t.sdq3a, p.t.sdq3b, p.t.sdq4a, p.t.sdq4b, p.t.sdq5a, p.t.sdq5b, p.t.sdq6a, p.t.sdq6b, p.t.sdq7a, p.t.sdq7b, p.t.sdq8a, p.t.sdq8b, p.t.sdq9a, p.t.sdq9b, p.t.sdq10a, p.t.sdq10b, ncol=2, nrow=10)
# 
# ##ICAST
# #spank
# p.t.icast1a <- ggplot(PRE.ICAST.teens.long[which(PRE.ICAST.teens.long$Question_num=="PRE_ICAST_spank"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_ICAST_spank",y="Frequency", x="Response")
# p.t.icast1b <- ggplot(POST.ICAST.teens.long[which(POST.ICAST.teens.long$Question_num=="POST_ICAST_spank"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_ICAST_spank",y="Frequency", x="Response")
# #object
# p.t.icast2a <- ggplot(PRE.ICAST.teens.long[which(PRE.ICAST.teens.long$Question_num=="PRE_ICAST_object"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_ICAST_object",y="Frequency", x="Response")
# p.t.icast2b <- ggplot(POST.ICAST.teens.long[which(POST.ICAST.teens.long$Question_num=="POST_ICAST_object"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_ICAST_object",y="Frequency", x="Response")
# #scream
# p.t.icast3a <- ggplot(PRE.ICAST.teens.long[which(PRE.ICAST.teens.long$Question_num=="PRE_ICAST_scream"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_ICAST_scream",y="Frequency", x="Response")
# p.t.icast3b <- ggplot(POST.ICAST.teens.long[which(POST.ICAST.teens.long$Question_num=="POST_ICAST_scream"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_ICAST_scream",y="Frequency", x="Response")
# #upset
# p.t.icast4a <- ggplot(PRE.ICAST.teens.long[which(PRE.ICAST.teens.long$Question_num=="PRE_ICAST_upset"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_ICAST_upset",y="Frequency", x="Response")
# p.t.icast4b <- ggplot(POST.ICAST.teens.long[which(POST.ICAST.teens.long$Question_num=="POST_ICAST_upset"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_ICAST_upset",y="Frequency", x="Response")
# ggarrange(p.t.icast1a, p.t.icast1b, p.t.icast2a, p.t.icast2b, p.t.icast3a, p.t.icast3b, p.t.icast4a, p.t.icast4b, ncol = 2, nrow=4)
# 
# ##PSE
# #praise
# p.t.pse1a <- ggplot(PRE.PSE.teens.long[which(PRE.PSE.teens.long$Question_num=="PRE_PSE_praise"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_PSE_praise",y="Frequency", x="Response")
# p.t.pse1b <- ggplot(POST.PSE.teens.long[which(POST.PSE.teens.long$Question_num=="POST_PSE_praise"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_PSE_praise",y="Frequency", x="Response")
# #other
# p.t.pse2a <- ggplot(PRE.PSE.teens.long[which(PRE.PSE.teens.long$Question_num=="PRE_PSE_others"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "PRE_PSE_others",y="Frequency", x="Response")
# p.t.pse2b <- ggplot(POST.PSE.teens.long[which(POST.PSE.teens.long$Question_num=="POST_PSE_others"),], aes(x=factor(Answer))) +
#   geom_bar(stat="count", fill="steelblue") + labs(title = "POST_PSE_others",y="Frequency", x="Response")
# ggarrange(p.t.pse1a, p.t.pse1b, p.t.pse2a, p.t.pse2b, ncol = 2, nrow = 2)
#drop post demographic variables
# datteen.join <- datteen.join[,-c(37:45)]
# datcg.join <- datcg.join[,-c(45:58)]

#=================================REVERSE CODING ================================
##caregivers
head(datcg.join$PRE_APQ_dark)
datcg.join$PRE_APQ_dark <- reverse.code(-1, datcg.join$PRE_APQ_dark, mini=0, maxi=4)
head(datcg.join$PRE_APQ_dark)
datcg.join$PRE_APQ_evening <- reverse.code(-1, datcg.join$PRE_APQ_evening, mini=0, maxi=4)
head(datcg.join$PRE_APQ_time)
datcg.join$PRE_APQ_time <- reverse.code(-1, datcg.join$PRE_APQ_time, mini=0, maxi=4)
head(datcg.join$PRE_APQ_time)
datcg.join$POST_APQ_dark <- reverse.code(-1, datcg.join$POST_APQ_dark, mini=0, maxi=4)
datcg.join$POST_APQ_evening <- reverse.code(-1, datcg.join$POST_APQ_evening, mini=0, maxi=4)
datcg.join$POST_APQ_time <- reverse.code(-1, datcg.join$POST_APQ_time, mini=0, maxi=4)

datcg.join$PRE_SDQ_obedient <- reverse.code(-1, datcg.join$PRE_SDQ_obedient, mini=0, maxi=2)
datcg.join$POST_SDQ_obedient <- reverse.code(-1, datcg.join$POST_SDQ_obedient, mini=0, maxi=2)

##teens
datteen.join$PRE_APQ_dark <- reverse.code(-1, datteen.join$PRE_APQ_dark, mini=0, maxi=4)
datteen.join$PRE_APQ_evening <- reverse.code(-1, datteen.join$PRE_APQ_evening, mini=0, maxi=4)
datteen.join$PRE_APQ_time <- reverse.code(-1, datteen.join$PRE_APQ_time, mini=0, maxi=4)
datteen.join$POST_APQ_dark <- reverse.code(-1, datteen.join$POST_APQ_dark, mini=0, maxi=4)
datteen.join$POST_APQ_evening <- reverse.code(-1, datteen.join$POST_APQ_evening, mini=0, maxi=4)
datteen.join$POST_APQ_time <- reverse.code(-1, datteen.join$POST_APQ_time, mini=0, maxi=4)

datteen.join$PRE_SDQ_obedient <- reverse.code(-1, datteen.join$PRE_SDQ_obedient, mini=0, maxi=2)
datteen.join$POST_SDQ_obedient <- reverse.code(-1, datteen.join$POST_SDQ_obedient, mini=0, maxi=2)


#=======================
#=================================CREATE COMPOSITE SCORES
#rowSum for each section: pre and post
##teens
#PRE
datteen.join <- datteen.join %>% mutate(Summed_PRE_APQ = rowSums(.[13:18]))
datteen.join <- datteen.join %>% mutate(Summed_PRE_SDQ = rowSums(.[19:28]))
datteen.join <- datteen.join %>% mutate(Summed_PRE_ICAST = rowSums(.[29:32]))
datteen.join <- datteen.join %>% mutate(Summed_PRE_PSE = rowSums(.[35:36]))
#POST
datteen.join <- datteen.join %>% mutate(Summed_POST_APQ = rowSums(.[46:51]))
datteen.join <- datteen.join %>% mutate(Summed_POST_SDQ = rowSums(.[52:61]))
datteen.join <- datteen.join %>% mutate(Summed_POST_ICAST = rowSums(.[62:65]))
datteen.join <- datteen.join %>% mutate(Summed_POST_PSE = rowSums(.[68:69]))

##caregivers
#PRE
datcg.join <- datcg.join %>% mutate(Summed_PRE_APQ = rowSums(.[18:23]))
datcg.join <- datcg.join %>% mutate(Summed_PRE_SDQ = rowSums(.[24:28]))
datcg.join <- datcg.join %>% mutate(Summed_PRE_ICAST = rowSums(.[29:32]))
datcg.join <- datcg.join %>% mutate(Summed_PRE_CESD = rowSums(.[34:36]))
datcg.join <- datcg.join %>% mutate(Summed_PRE_FFC = rowSums(.[37:41]))
datcg.join <- datcg.join %>% mutate(Summed_PRE_PSE = rowSums(.[43:44]))
#POST
datcg.join <- datcg.join %>% mutate(Summed_POST_APQ = rowSums(.[59:64]))
datcg.join <- datcg.join %>% mutate(Summed_POST_SDQ = rowSums(.[65:69]))
datcg.join <- datcg.join %>% mutate(Summed_POST_ICAST = rowSums(.[70:73]))
datcg.join <- datcg.join %>% mutate(Summed_POST_CESD = rowSums(.[75:77]))
datcg.join <- datcg.join %>% mutate(Summed_POST_FFC = rowSums(.[78:82]))
datcg.join <- datcg.join %>% mutate(Summed_POST_PSE = rowSums(.[84:85]))

# plot distribution of summed scores (for CG & T, PRE & POST)
#datteen.join
hist(datteen.join$Summed_PRE_APQ, breaks=10, xlab="Pre-Intervention APQ Score", main="", col = "#b7d6ab")
hist(datteen.join$Summed_PRE_SDQ, breaks=10, xlab="Pre-Intervention SDQ Score", main="", col="#ebe88f")
hist(datteen.join$Summed_PRE_ICAST, breaks=10, xlab="Pre-Intervention ICAST Score", main="", col="#db93b8")
hist(datteen.join$Summed_PRE_PSE, breaks=10, xlab="Pre-Intervention PSE Score", main="")
hist(datteen.join$Summed_POST_APQ, breaks=5, xlab="Post-Intervention APQ Score", main="", col = "#b7d6ab")
hist(datteen.join$Summed_POST_SDQ, breaks=4, xlab="Post-Intervention SDQ Score", main="", col="#ebe88f")
hist(datteen.join$Summed_POST_ICAST, breaks=1, xlab="Post-Intervention ICAST Score", main="", col="#db93b8")
hist(datteen.join$Summed_POST_PSE, breaks=4, xlab="Post-Intervention PSE  Score", main="")

#caregivers
hist(datcg.join$Summed_PRE_APQ, breaks=10, xlab="Summed Pre-Intervention APQ Score", main="", col = "#b7d6ab")
hist(datcg.join$Summed_PRE_SDQ, breaks=8, xlab="Summed Pre-Intervention SDQ Score", main="", col="#ebe88f")
hist(datcg.join$Summed_PRE_ICAST, breaks=6, xlab="Summed Pre-Intervention ICAST Score", main="", col="#db93b8")
hist(datcg.join$Summed_PRE_CESD, breaks=1, xlab="Summed Pre-Intervention CESD Score", main="", xlim = c(8,9), col="#8a73bf")
hist(datcg.join$Summed_PRE_FFC, breaks=4, xlab="Summed Pre-Intervention FFC Score", main="", col = "#7a6346")
hist(datcg.join$Summed_PRE_PSE, breaks=2, xlab="Summed Pre-Intervention PSE Score", main="", xlim=c(6,7))
hist(datcg.join$Summed_POST_APQ, breaks=8, xlab="Summed Post-Intervention APQ Score", main="", col = "#b7d6ab")
hist(datcg.join$Summed_POST_SDQ, breaks=2, xlab="Summed Post-Intervention SDQ Score", main="", col="#ebe88f")
hist(datcg.join$Summed_POST_ICAST, breaks=1, xlab="Summed Post-Intervention ICAST Score", main="", col="#db93b8")
hist(datcg.join$Summed_POST_CESD, breaks=6, xlab="Summed Post-Intervention CESD Score", main="", col="#8a73bf")
hist(datcg.join$Summed_POST_FFC, breaks=5, xlab="Summed Post-Intervention FFC Score", main="", col = "#7a6346")
hist(datcg.join$Summed_POST_PSE, breaks=5, xlab="Summed Post-Intervention PSE Score", main="")
write.csv(datcg.join, file = "~/OneDrive - University of Cape Town/2022/UNI/Project/South Sudan/PLH survey data (South Sudan)/datcg.join.csv")
write.csv(datteen.join, file = "~/OneDrive - University of Cape Town/2022/UNI/Project/South Sudan/PLH survey data (South Sudan)/datteen.join.csv")

#=================================ITEM RELIABILITY ANALYSIS=============================================
library(ltm)
#TEENS
cronbach.alpha(datteen.join[,13:18], na.rm = T) #APQ (PRE)
cronbach.alpha(datteen.join[,19:28], na.rm = T) #SDQ (PRE)
cronbach.alpha(datteen.join[,29:32], na.rm = T) #ICAST (PRE)
cronbach.alpha(datteen.join[,46:51], na.rm = T) #APQ (POST)
cronbach.alpha(datteen.join[,52:61], na.rm = T) #SDQ (POST)
cronbach.alpha(datteen.join[,62:65], na.rm = T) #ICAST (POST) ?????????????

#CAREGIVERS
cronbach.alpha(datcg.join[,18:23], na.rm=T, CI=T) #APQ (PRE)
cronbach.alpha(datcg.join[,24:28], na.rm=T) #SDQ (PRE)
cronbach.alpha(datcg.join[,29:32], na.rm=T) #ICAST (PRE)
cronbach.alpha(datcg.join[,34:36], na.rm=T) #CESD (PRE)
cronbach.alpha(datcg.join[,37:41], na.rm=T) #FFC (PRE)
cronbach.alpha(datcg.join[,59:64], na.rm=T) #APQ (POST)
cronbach.alpha(datcg.join[,65:69], na.rm=T) #SDQ (POST)
cronbach.alpha(datcg.join[,70:73], na.rm=T) #ICAST (POST)
cronbach.alpha(datcg.join[,75:77], na.rm=T) #CESD (POST)
cronbach.alpha(datcg.join[,78:82], na.rm=T) #FFC (POST)

#splitting APQ domain
  #teens
cronbach.alpha(datteen.join[,13:15], na.rm = T) #APQ (PRE) (Involved parenting)
cronbach.alpha(datteen.join[,16:18], na.rm = T) #APQ (PRE) (Parental monitoring)

  #caregivers
cronbach.alpha(datcg.join[,18:20], na.rm=T) #APQ (PRE) (Involved parenting)
cronbach.alpha(datcg.join[,21:23], na.rm=T) #APQ (PRE) (Parental monitoring)
#results are no better splitting up APQ
library(lavaan)
#EFA
  #CAREGIVERS

  
  
  #TEENS
efa.t.apq.pre <- factanal(~PRE_APQ_talk + PRE_APQ_involved + PRE_APQ_talk_friends + PRE_APQ_evening + PRE_APQ_time + PRE_APQ_dark,factors=1,data=data.frame(datteen.join))
efa.t.apq.pre

#CFA
  #CAREGIVERS - note CESD_PRE, SDQ_POST, ICAST_POST have no variation
#note ICAST_object, and all of FFC, CESD are removed because of no variation
CFA.PRECG.models <- ' APQ_PRE =~ PRE_APQ_talk + PRE_APQ_involved + PRE_APQ_talk_friends + PRE_APQ_evening + PRE_APQ_time + PRE_APQ_dark
                SDQ_PRE =~ PRE_SDQ_tantrums + PRE_SDQ_obedient + PRE_SDQ_fights + PRE_SDQ_lies + PRE_SDQ_steals 
                ICAST_PRE =~ PRE_ICAST_spank + PRE_ICAST_scream + PRE_ICAST_upset 
                '
cfa.cg.pre <- cfa(CFA.PRECG.models, data = datcg.join)
summary(cfa.cg.pre, fit.measures=T)

CFA.POSTCG.models <- ' APQ_POST =~ POST_APQ_talk + POST_APQ_involved + POST_APQ_talk_friends + POST_APQ_evening + POST_APQ_time
                      CESD_POST =~ POST_CESD_depressed + POST_CESD_effort + POST_CESD_lonely
                      '
cfa.cg.post <- cfa(CFA.POSTCG.models, data = datcg.join)
summary(cfa.cg.post, fit.measures=T) #no convergence

  #TEENS
CFA.PRET.models <- ' APQ_PRE =~ PRE_APQ_talk + PRE_APQ_involved + PRE_APQ_talk_friends + PRE_APQ_evening + PRE_APQ_time + PRE_APQ_dark
                SDQ_PRE =~ PRE_SDQ_tantrums + PRE_SDQ_obedient + PRE_SDQ_worried + PRE_SDQ_fights + PRE_SDQ_unhappy + PRE_SDQ_new + PRE_SDQ_lies + PRE_SDQ_steals + PRE_SDQ_fears
                ICAST_PRE =~ PRE_ICAST_spank + PRE_ICAST_object + PRE_ICAST_scream + PRE_ICAST_upset '
cfa.t.pre <- cfa(CFA.PRET.models, data = datteen.join)
summary(cfa.t.pre, fit.measures=T) #SDQ and ICAST have good internal consistency

#ICAST and SDQ have too little variation
CFA.POSTT.models <- ' APQ_POST =~ POST_APQ_talk + POST_APQ_involved + POST_APQ_talk_friends + POST_APQ_evening + POST_APQ_time 
                '
cfa.t.post <- cfa(CFA.POSTT.models, data = datteen.join)
summary(cfa.t.post, fit.measures=T)

#=================================MATCHING Caregiver-Teen=========
#match teen to caregiver
dat.join <- inner_join(datcg.join, datteen.join, by=c("HH.UIC" = "UIC", "Dem_ChildGenderCG.x"="Dem_GenderT"))
dat.nacount <- rowSums(is.na(dat.join))
dat.nacount
duplicated(dat.join$HH.UIC) #which common household ID's remain after matching

dat.join2 <- inner_join(datcg.join, datteen.join, by=c("S.N.x" = "S.N"))
#View(cbind(dat.join2$HH.UIC, dat.join2$UIC)) #matching by S.N doesnt work
rm(dat.join2)

#save dataset
write.csv(dat.join, file = "~/OneDrive - University of Cape Town/2022/UNI/Project/South Sudan/PLH survey data (South Sudan)/dat.join.csv")
