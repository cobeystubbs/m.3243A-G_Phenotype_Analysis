#Plotting graphs of phenotypes x m.3243A>G heteroplasmy

#read in data from the table and assign a name 
binarydata <- read.csv("/Users/cobeystubbs/Desktop/Binary_data.csv")
  head(binary)
  
#load in extra packages
library(ggplot2)
library(dplyr)
  
#Add CNSeffects column as a combination of encephalopathy, SLE and seizures - if patient has any of the three = score of 1, if they have none of the three score = 0

binarydata$CNSeffects <- ifelse(binarydata$Seizures_MaxScore==1|binarydata$SLE_MaxScore==1|binarydata$Encephalopathy_MaxScore==1, 1, 0)

#Add CNSeffects_MinAge column - take lowest age that patient presented with phenotype 

for (i in 1:nrow(binarydata)){
  
  binarydata$CNSeffects_MinAge[i] <- min(binarydata$Seizures_MinAge[i], binarydata$SLE_MinAge[i], binarydata$Encephalopathy_MinAge[i])
  
  binarydata$CNSeffects_MinAge[i] <- ifelse(((binarydata$Seizures_MaxScore[i] == 1) & (binarydata$SLE_MaxScore[i] == 0) & (binarydata$Encephalopathy_MaxScore[i] == 0)), 
                                            binarydata$Seizures_MinAge[i], binarydata$CNSeffects_MinAge[i])
  binarydata$CNSeffects_MinAge[i] <- ifelse(((binarydata$Seizures_MaxScore[i] == 0) & (binarydata$SLE_MaxScore[i] == 1) & (binarydata$Encephalopathy_MaxScore[i] == 0)), binarydata$SLE_MinAge[i], binarydata$CNSeffects_MinAge[i])
  binarydata$CNSeffects_MinAge[i] <- ifelse(((binarydata$Seizures_MaxScore[i] == 0) & (binarydata$SLE_MaxScore[i] == 0) & (binarydata$Encephalopathy_MaxScore[i] == 1)), binarydata$Encephalopathy_MinAge[i], binarydata$CNSeffects_MinAge[i])
  
  binarydata$CNSeffects_MinAge[i] <- ifelse(((binarydata$Seizures_MaxScore[i] == 1) & (binarydata$SLE_MaxScore[i] == 1) & (binarydata$Encephalopathy_MaxScore[i] == 0)), min(binarydata$Seizures_MinAge[i], binarydata$SLE_MinAge[i]), binarydata$CNSeffects_MinAge[i])
  binarydata$CNSeffects_MinAge[i] <- ifelse(((binarydata$Seizures_MaxScore[i] == 0) & (binarydata$SLE_MaxScore[i] == 1) & (binarydata$Encephalopathy_MaxScore[i] == 1)), min(binarydata$Encephalopathy_MinAge[i], binarydata$SLE_MinAge[i]), binarydata$CNSeffects_MinAge[i])
  binarydata$CNSeffects_MinAge[i] <- ifelse(((binarydata$Seizures_MaxScore[i] == 1) & (binarydata$SLE_MaxScore[i] == 0) & (binarydata$Encephalopathy_MaxScore[i] == 1)), min(binarydata$Seizures_MinAge[i], binarydata$Encephalopathy_MinAge[i]), binarydata$CNSeffects_MinAge[i])
  
}
  
#Encephalopathy x m.3243A>G heteroplasmy - stats test 
  
binaryYes <- binarydata %>% filter(Encephalopathy_MaxScore==1)
binaryNo <- binarydata %>% filter(Encephalopathy_MaxScore==0)
  
EncephAns <- wilcox.test(binaryNo$HetAdj, binaryYes$HetAdj)
EncephAns1 <- EncephAns$p.value
EncephAns1
  
#Plot graph of encephalopathy x m.3243A>G heteroplasmy with stats test 
ggplot(binarydata, aes(x=Encephalopathy_MaxScore, y=HetAdj))+
  geom_boxplot(aes(group = as.factor(Encephalopathy_MaxScore)), colour = "#182cdb", width = 0.6)+
  labs(x = "Encephalopathy Binary Score", y = "Adjusted Heteroplasmy Score")+
  geom_jitter(alpha = 0.3, width = 0.05, size = 1.7, colour = "#85CADF")+
  theme_linedraw()+
  theme(text=element_text(family='Times New Roman', size=12))+
  geom_text(x=0.4,y=0.6, colour='blue',hjust=0,label=paste( '\np-value:', round(EncephAns1, 5)))

#Seizures x m.3243A>G heteroplasmy - stats test 

binaryYes <- binarydata %>% filter(Seizures_MaxScore==1)
binaryNo <- binarydata %>% filter(Seizures_MaxScore==0)

SeizAns <- wilcox.test(binaryNo$HetAdj, binaryYes$HetAdj)
SeizAns1 <- SeizAns$p.value
SeizAns1

#Plot graph of seizures x m.3243A>G heteroplasmy with stats test 
ggplot(binarydata, aes(x=Seizures_MaxScore, y=HetAdj))+
  geom_boxplot(aes(group = as.factor(Seizures_MaxScore)), colour = "#182cdb", width = 0.6)+
  labs(x = "Seizures Binary Score", y = "Adjusted Heteroplasmy Score")+
  geom_jitter(alpha = 0.3, width = 0.05, size = 1.7, colour = "#85CADF")+
  theme_linedraw()+
  theme(text=element_text(family='Times New Roman', size=12))+
  geom_text(x=0.4,y=0.55, colour='blue',hjust=0,label=paste( '\np-value:', round(SeizAns1, 5)))

#SLE x m.3243A>G heteroplasmy - stats test 

binaryYes <- binarydata %>% filter(SLE_MaxScore==1)
binaryNo <- binarydata %>% filter(SLE_MaxScore==0)

SLEAns <- wilcox.test(binaryNo$HetAdj, binaryYes$HetAdj)
SLEAns1 <- SLEAns$p.value
SLEAns1

#Plot graph of SLE x m.3243A>G heteroplasmy with stats test 
ggplot(binarydata, aes(x=SLE_MaxScore, y=HetAdj))+
  geom_boxplot(aes(group = as.factor(SLE_MaxScore)), colour = "#182cdb", width = 0.6)+
  labs(x = "SLE Binary Score", y = "Adjusted Heteroplasmy Score")+
  geom_jitter(alpha = 0.3, width = 0.05, size = 1.7, colour = "#85CADF")+
  theme_linedraw()+
  theme(text=element_text(family='Times New Roman', size=12))+
  geom_text(x=0.4,y=0.55, colour='blue',hjust=0,label=paste( '\np-value:', round(SLEAns1, 5)))

#CNS effects x m.3243A>G heteroplasmy - stats test 

binaryYes <- binarydata %>% filter(CNSeffects==1)
binaryNo <- binarydata %>% filter(CNSeffects==0)

CNSAns <- wilcox.test(binaryNo$HetAdj, binaryYes$HetAdj)
CNSAns1 <- CNSAns$p.value
CNSAns1

#Plot graph of CNS effects x m.3243A>G heteroplasmy with stats test 
ggplot(binarydata, aes(x=CNSeffects, y=HetAdj))+
  geom_boxplot(aes(group = as.factor(CNSeffects)), colour = "#182cdb", width = 0.6)+
  labs(x = "CNS effects Binary Score", y = "Adjusted Heteroplasmy Score")+
  geom_jitter(alpha = 0.3, width = 0.05, size = 1.7, colour = "#85CADF")+
  theme_linedraw()+
  theme(text=element_text(family='Times New Roman', size=12))+
  geom_text(x=0.4,y=0.55, colour='blue',hjust=0,label=paste( '\np-value:', round(CNSAns1, 5)))

#Diabetes x m.3243A>G heteroplasmy - stats test 

binaryYes <- binarydata %>% filter(Diabetes_MaxScore==1)
binaryNo <- binarydata %>% filter(Diabetes_MaxScore==0)

DiabAns <- wilcox.test(binaryNo$HetAdj, binaryYes$HetAdj)
DiabAns1 <- DiabAns$p.value
DiabAns1

#Plot graph of diabetes x m.3243A>G heteroplasmy with stats test 
ggplot(binarydata, aes(x=Diabetes_MaxScore, y=HetAdj))+
  geom_boxplot(aes(group = as.factor(Diabetes_MaxScore)), colour = "#182cdb", width = 0.6)+
  labs(x = "Diabetes Binary Score", y = "Adjusted Heteroplasmy Score")+
  geom_jitter(alpha = 0.3, width = 0.05, size = 1.7, colour = "#85CADF")+
  theme_linedraw()+
  theme(text=element_text(family='Times New Roman', size=12))+
  geom_text(x=0.4,y=0.55, colour='blue',hjust=0,label=paste("p-value: <0.00001"))

#Hearing impairemnt x m.3243A>G heteroplasmy - stats test 

binaryYes <- binarydata %>% filter(Hearing_MaxScore==1)
binaryNo <- binarydata %>% filter(Hearing_MaxScore==0)

HearAns <- wilcox.test(binaryNo$HetAdj, binaryYes$HetAdj)
HearAns1 <- HearAns$p.value
HearAns1

#Plot graph of hearing impairment x m.3243A>G heteroplasmy with stats test 
ggplot(binarydata, aes(x=Hearing_MaxScore, y=HetAdj))+
  geom_boxplot(aes(group = as.factor(Hearing_MaxScore)), colour = "#182cdb", width = 0.6)+
  labs(x = "Hearing Binary Score", y = "Adjusted Heteroplasmy Score")+
  geom_jitter(alpha = 0.3, width = 0.05, size = 1.7, colour = "#85CADF")+
  theme_linedraw()+
  theme(text=element_text(family='Times New Roman', size=12))+
  geom_text(x=0.4,y=0.45, colour='blue',hjust=0,label=paste( '\np-value:', round(HearAns1, 5)))

