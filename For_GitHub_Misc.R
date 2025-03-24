#Count the number of SNPs examined in each GWAS 

dim(enceph)
dim(seiz)
dim(SLE)
dim(CNS)

#Find out how many patients have data for each phenotype 

table(binary$Encephalopathy_MaxScore)[TRUE]
table(binary$Seizures_MaxScore)[TRUE]
table(binary$SLE_MaxScore)[TRUE]
table(binary$CNSeffects)[TRUE]

#Find out how many people passed the quality control steps
DFR <- data.table::fread("/Users/cobeystubbs/Desktop/DFR.genome")
sort(unique(DFR$IID1))

#Find out how many people passed quality control for each phenotype 
merged <- merge(DFR, binary, by.x="IID1", by.y="UniqueID")

#People in DFR not in binary phenotype dataset
unique(DFR$IID1)[!sort(unique(DFR$IID1)) %in% sort(unique(binary$UniqueID))]

#Getting unique people from merged dataset
merged <- merged %>%
  distinct(IID1, .keep_all = TRUE)
table(merged$Encephalopathy_MaxScore)[TRUE]
table(merged$Seizures_MaxScore)[TRUE]
table(merged$SLE_MaxScore)[TRUE]
table(merged$CNSeffects)[TRUE]

#Separate patients by cohort and sex 

ncl <- binary%>%
  filter(Cohort == 'NCL')
table(ncl$Sex)[TRUE]

exeter <- binary%>%
  filter(Cohort == 'Exeter')
table(exeter$Sex)[TRUE]

germany <- binary%>%
  filter(Cohort == 'Germany')
table(germany$Sex)[TRUE]

italy <- binary%>%
  filter(Cohort == 'Italy')
table(italy$Sex)[TRUE]

ucl <- binary%>%
  filter(Cohort == 'UCL')
table(ucl$Sex)[TRUE]

oxford <- binary%>%
  filter(Cohort == 'Oxford')
table(oxford$Sex)[TRUE]

#Confirm correlation between phenotypes 

correlation1 <- cor.test(binarydata$Encephalopathy_MaxScore, binarydata$Seizures_MaxScore, method = 'pearson')
correlation1

correlation2 <- cor.test(binarydata$Encephalopathy_MaxScore, binarydata$SLE_MaxScore, method = 'pearson')
correlation2

correlation3 <- cor.test(binarydata$Seizures_MaxScore, binarydata$SLE_MaxScore, method = 'pearson')
correlation3

#Calculation of lambda inflation factors 
install.packages("QCEWAS")
library(QCEWAS)
P_lambda(enceph$p.value)
P_lambda(seiz$p.value)
P_lambda(SLE$p.value)
P_lambda(CNSeffects$p.value)

dim(enceph)
dim(seiz)
dim(SLE)
dim(CNSeffects)

#Calculation of the average age of patients in each cohort 
ncl <- binary%>%
  filter(Cohort == 'NCL')
mean(ncl$CNSeffects_MinAge, na.rm = TRUE)

exeter <- binary%>%
  filter(Cohort == 'Exeter')
mean(exeter$Diabetes_MinAge, na.rm = TRUE)

germ <- binary%>%
  filter(Cohort == 'Germany')
mean(germ$CNSeffects_MinAge, na.rm = TRUE)

italy <- binary%>%
  filter(Cohort == 'Italy')
mean(italy$CNSeffects_MinAge, na.rm = TRUE)

ucl <- binary%>%
  filter(Cohort == 'UCL')
mean(ucl$CNSeffects_MinAge, na.rm = TRUE)

mean(binary$CNSeffects_MinAge, na.rm = TRUE)

#Calculation of standard deviation of age 

sd(ncl$CNSeffects_MinAge, na.rm = TRUE)

sd(exeter$Diabetes_MinAge, na.rm = TRUE)

sd(germ$CNSeffects_MinAge, na.rm = TRUE)

sd(italy$CNSeffects_MinAge, na.rm = TRUE)

sd(ucl$CNSeffects_MinAge, na.rm = TRUE)

sd(binary$CNSeffects_MinAge, na.rm = TRUE)

#Calculation of the average m.3243A>G heteroplasmy levels of patients in each cohort

mean(ncl$HetAdj, na.rm = TRUE)

mean(exeter$HetAdj, na.rm = TRUE)

mean(germ$HetAdj, na.rm = TRUE)

mean(italy$HetAdj, na.rm = TRUE)

mean(ucl$HetAdj, na.rm = TRUE)

mean(binary$HetAdj, na.rm = TRUE)

#Calculation of standard deviation of m.3243A>G heteroplasmy

sd(ncl$HetAdj, na.rm = TRUE)

sd(exeter$HetAdj, na.rm = TRUE)

sd(germ$HetAdj, na.rm = TRUE)

sd(italy$HetAdj, na.rm = TRUE)

sd(ucl$HetAdj, na.rm = TRUE)

sd(binary$HetAdj, na.rm = TRUE)


#Find median values of age and het 

het0 <- binary%>%
  filter(Encephalopathy_MaxScore == 0)
median(het0$HetAdj)
het1 <- binary%>%
  filter(Encephalopathy_MaxScore == 1)
median(het1$HetAdj)

het2 <- binary%>%
  filter(Seizures_MaxScore == 0)
median(het2$HetAdj)
het3 <- binary%>%
  filter(Seizures_MaxScore == 1)
median(het3$HetAdj)

het4 <- binary%>%
  filter(SLE_MaxScore == 0)
median(het4$HetAdj)
het5 <- binary%>%
  filter(SLE_MaxScore == 1)
median(het5$HetAdj)

het6 <- binary%>%
  filter(CNSeffects == 0)
median(het6$HetAdj)
het7 <- binary%>%
  filter(CNSeffects == 1)
median(het7$HetAdj)

