#Plotting graphs of phenotypes x m.3243A>G heteroplasmy

#read in data from the table and assign a name 
binarydata <- read.csv("/Users/cobeystubbs/Desktop/Binary_data.csv")
  head(binary)
  
#Load in extra packages
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
  geom_text(x=0.4,y=0.6, size=5, colour='blue',hjust=0,label=paste( '\np-value:', round(EncephAns1, 5)))

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
  geom_text(x=0.4,y=0.55, size=5, colour='blue',hjust=0,label=paste( '\np-value:', round(SeizAns1, 5)))

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
  geom_text(x=0.4,y=0.55, size=5, colour='blue',hjust=0,label=paste( '\np-value:', round(SLEAns1, 5)))

#CNS effects x m.3243A>G heteroplasmy - stats test 

binaryYes <- binarydata %>% filter(CNSeffects==1)
binaryNo <- binarydata %>% filter(CNSeffects==0)

CNSAns <- wilcox.test(binaryNo$HetAdj, binaryYes$HetAdj)c
CNSAns1 <- CNSAns$p.value
CNSAns1

#Plot graph of CNS effects x m.3243A>G heteroplasmy with stats test 
ggplot(binarydata, aes(x=CNSeffects, y=HetAdj))+
  geom_boxplot(aes(group = as.factor(CNSeffects)), colour = "#182cdb", width = 0.6)+
  labs(x = "CNS effects Binary Score", y = "Adjusted Heteroplasmy Score")+
  geom_jitter(alpha = 0.3, width = 0.05, size = 1.7, colour = "#85CADF")+
  theme_linedraw()+
  theme(text=element_text(family='Times New Roman', size=12))+
  geom_text(x=0.4,y=0.55, size=5, colour='blue',hjust=0,label=paste( '\np-value:', round(CNSAns1, 5)))

#Plotting graphs of phenotypes x patient age

#Encephalopathy x patient age - stats test 

binaryYes <- binarydata %>% filter(Encephalopathy_MaxScore==1)
binaryNo <- binarydata %>% filter(Encephalopathy_MaxScore==0)

EncephA <- wilcox.test(binaryNo$Encephalopathy_MinAge, binaryYes$Encephalopathy_MinAge)
EncephA1 <- EncephA$p.value
EncephA1

#Plot graph of encephalopathy x patient age with stats test 
ggplot(binarydata, aes(x=Encephalopathy_MaxScore, y=Encephalopathy_MinAge))+
  geom_boxplot(aes(group = as.factor(Encephalopathy_MaxScore)), colour = "#182cdb", width = 0.6)+
  labs(x = "Encephalopathy Binary Score", y = "Patient Age")+
  geom_jitter(alpha = 0.3, width = 0.05, size = 1.7, colour = "#85CADF")+
  theme_linedraw()+
  theme(text=element_text(family='Times New Roman', size=12))+
  geom_text(x=0.5,y=60, size=5, colour='blue',hjust=0,label=paste( '\np-value:', round(EncephA1, 5)))

#Seizures x patient age - stats test 

binaryYes <- binarydata %>% filter(Seizures_MaxScore==1)
binaryNo <- binarydata %>% filter(Seizures_MaxScore==0)

SeizA <- wilcox.test(binaryNo$Seizures_MinAge, binaryYes$Seizures_MinAge)
SeizA1 <- SeizA$p.value
SeizA1

#Plot graph of seizures x patient age with stats test 
ggplot(binarydata, aes(x=Seizures_MaxScore, y=Seizures_MinAge))+
  geom_boxplot(aes(group = as.factor(Seizures_MaxScore)), colour = "#182cdb", width = 0.6)+
  labs(x = "Seizures Binary Score", y = "Patient Age")+
  geom_jitter(alpha = 0.3, width = 0.05, size = 1.7, colour = "#85CADF")+
  theme_linedraw()+
  theme(text=element_text(family='Times New Roman', size=12))+
  geom_text(x=0.5,y=60, size=5, colour='blue',hjust=0,label=paste( '\np-value:', round(SeizA1, 5)))

#SLE x patient age - stats test 

binaryYes <- binarydata %>% filter(SLE_MaxScore==1)
binaryNo <- binarydata %>% filter(SLE_MaxScore==0)

SLEA <- wilcox.test(binaryNo$SLE_MinAge, binaryYes$SLE_MinAge)
SLEA1 <- SLEA$p.value
SLEA1

#Plot graph of SLE x patient age with stats test 
ggplot(binarydata, aes(x=SLE_MaxScore, y=SLE_MinAge))+
  geom_boxplot(aes(group = as.factor(SLE_MaxScore)), colour = "#182cdb", width = 0.6)+
  labs(x = "SLE Binary Score", y = "Patient Age")+
  geom_jitter(alpha = 0.3, width = 0.05, size = 1.7, colour = "#85CADF")+
  theme_linedraw()+
  theme(text=element_text(family='Times New Roman', size=12))+
  geom_text(x=0.5,y=60, size=5, colour='blue',hjust=0,label=paste( '\np-value:', round(SLEA1, 5)))

#CNS effects x patient age - stats test 

binaryYes <- binarydata %>% filter(CNSeffects==1)
binaryNo <- binarydata %>% filter(CNSeffects==0)

CNSA <- wilcox.test(binaryNo$CNSeffects_MinAge, binaryYes$CNSeffects_MinAge)
CNSA1 <- CNSA$p.value
CNSA1

#Plot graph of CNS effects x patient age with stats test 
ggplot(binarydata, aes(x=CNSeffects, y=CNSeffects_MinAge))+
  geom_boxplot(aes(group = as.factor(CNSeffects)), colour = "#182cdb", width = 0.6)+
  labs(x = "CNS effects Binary Score", y = "Patient Age")+
  geom_jitter(alpha = 0.3, width = 0.05, size = 1.7, colour = "#85CADF")+
  theme_linedraw()+
  theme(text=element_text(family='Times New Roman', size=12))+
  geom_text(x=0.5,y=60, size=5, colour='blue',hjust=0,label=paste( '\np-value:', round(CNSA1, 5)))

#Plot graphs of phenotype x phenotype to look at their correlation

#Encephalopathy x seizures - stats test 

binaryYes <- binarydata %>% filter(Encephalopathy_MaxScore==1)
binaryNo <- binarydata %>% filter(Encephalopathy_MaxScore==0)

encephXseizures <- wilcox.test(binaryNo$Seizures_MaxScore, binaryYes$Seizures_MaxScore)
encephXseizures1 <- encephXseizures$p.value
encephXseizures1

#Plot graph of encephalopathy x seizures with stats test 
ggplot(binarydata, aes(x=Encephalopathy_MaxScore, y=Seizures_MaxScore))+
  labs(x = "Encephalopathy Binary Score", y = "Seizures Binary Score")+
  geom_jitter(alpha = 0.3, width = 0.05, height = 0.25, size = 1.7, colour = "#182cdb")+
  theme_linedraw()+
  theme(text=element_text(family='Times New Roman', size=12))+
  geom_text(x=0.5,y=0.5, colour='blue',hjust=0,label=paste("p-value: <0.00001"))

#Encephalopathy x SLE - stats test 

binaryYes <- binarydata %>% filter(Encephalopathy_MaxScore==1)
binaryNo <- binarydata %>% filter(Encephalopathy_MaxScore==0)

encephXSLE <- wilcox.test(binaryNo$SLE_MaxScore, binaryYes$SLE_MaxScore)
encephxSLE1 <- encephXSLE$p.value
encephxSLE1

#Plot graph of encephalopathy x SLE with stats test 
ggplot(binarydata, aes(x=Encephalopathy_MaxScore, y=SLE_MaxScore))+
  labs(x = "Encephalopathy Binary Score", y = "SLE Binary Score")+
  geom_jitter(alpha = 0.3, width = 0.05, height = 0.25, size = 1.7, colour = "#182cdb")+
  theme_linedraw()+
  theme(text=element_text(family='Times New Roman', size=12))+
  geom_text(x=0.5,y=0.5, colour='blue',hjust=0,label=paste("p-value: <0.00001"))

#SLE x seizures - stats test 

binaryYes <- binarydata %>% filter(SLE_MaxScore==1)
binaryNo <- binarydata %>% filter(SLE_MaxScore==0)

SLExseizures <- wilcox.test(binaryNo$Seizures_MaxScore, binaryYes$Seizures_MaxScore)
SLExseizures1 <- SLExseizures$p.value
SLExseizures1

#Plot graph of SLE x seizures with stats test 
ggplot(binarydata, aes(x=SLE_MaxScore, y=Seizures_MaxScore))+
  labs(x = "SLE Binary Score", y = "Seizures Binary Score")+
  geom_jitter(alpha = 0.3, width = 0.05, height = 0.25, size = 1.7, colour = "#182cdb")+
  theme_linedraw()+
  theme(text=element_text(family='Times New Roman', size=12))+
  geom_text(x=0.5,y=0.5, colour='blue',hjust=0,label=paste("p-value: <0.00001"))



