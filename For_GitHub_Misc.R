#Count the number of SNPs examined in each GWAS 

dim(enceph)
dim(seiz)
dim(SLE)
dim(CNS)
dim(diabetes)
dim(hear)

#Find out how many patients have data for each phenotype 

table(binary$Encephalopathy_MaxScore)[TRUE]
table(binary$Seizures_MaxScore)[TRUE]
table(binary$SLE_MaxScore)[TRUE]
table(binary$CNSeffects)[TRUE]
table(binary$Diabetes_MaxScore)[TRUE]
table(binary$Hearing_MaxScore)[TRUE]

#Seperate patients by cohort and sex 

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

#Find out how many people passed the quality control steps

DFR <- data.table::fread("/Users/cobeystubbs/Desktop/DFR.genome")
sort(unique(DFR$IID1))

