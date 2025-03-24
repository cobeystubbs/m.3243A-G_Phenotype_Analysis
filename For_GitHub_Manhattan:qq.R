#Plotting Manhattan plots and qqplots 

#Load in extra packages 
library(ggplot2)
library(qqman)

#Manhattan plot for encephalopathy 

enceph <- data.table::fread("/Users/cobeystubbs/Desktop/Encephalopathy_No_Cohort_Cov.txt.gz")%>%
  as.data.frame()
head(enceph)

enceph$p_sci = scientific(enceph$p.value, digits = 3)

manhattan(subset(enceph, p.value<0.05), chr="CHR", bp="POS", snp="SNPID", p="p.value", ylim=c(2, 8), col=c("#182cdb", "#85CADF"))

#qqplot for encephalopathy

qq(enceph$p.value)

#prepare a file to input to FUMA/LocusZoom to examine results

write.table(enceph[ , c("CHR", "POS", "Allele1", "Allele2", "N", "BETA", "SE", "p_sci", "AF_Allele2")], file="/Users/cobeystubbs/Desktop/Encephalopathy_FUMA.txt", sep="\t", row.names = F, col.names = T, quote=F)

#Manhattan plot for seizures 

seiz <- data.table::fread("/Users/cobeystubbs/Desktop/Seizures_No_Cohort_Cov.txt.gz")%>%
  as.data.frame()
head(seiz)

seiz$p_sci = scientific(seiz$p.value, digits = 3)

manhattan(subset(seiz, p.value<0.05), chr="CHR", bp="POS", snp="SNPID", p="p.value", ylim=c(2, 8), col=c("#182cdb", "#85CADF"))

#qqplot for seizures

qq(seiz$p.value)

#prepare a file to input to FUMA/LocusZoom to examine results

write.table(seizures[ , c("CHR", "POS", "Allele1", "Allele2", "N", "BETA", "SE", "p_sci", "AF_Allele2")], file="/Users/cobeystubbs/Desktop/Seizures_FUMA.txt", sep="\t", row.names = F, col.names = T, quote=F)
CNSeffects
#Manhattan plot for SLE 

SLE <- data.table::fread("/Users/cobeystubbs/Desktop/SLE_No_Cohort_Cov.txt.gz")%>%
  as.data.frame()
head(SLE)

SLE$p_sci = scientific(SLE$p.value, digits = 3)

manhattan(subset(SLE, p.value<0.05), chr="CHR", bp="POS", snp="SNPID", p="p.value", ylim=c(2, 8), col=c("#182cdb", "#85CADF"))

#qqplot for SLE

qq(SLE$p.value)

#prepare a file to input to FUMA/LocusZoom to examine results

write.table(SLE[ , c("CHR", "POS", "Allele1", "Allele2", "N", "BETA", "SE", "p_sci", "AF_Allele2")], file="/Users/cobeystubbs/Desktop/SLE_FUMA.txt", sep="\t", row.names = F, col.names = T, quote=F)

#Manhattan plot for CNS effects 

CNS <- data.table::fread("/Users/cobeystubbs/Desktop/CNS_No_Cohort_Cov.txt.gz")%>%
  as.data.frame()
head(CNS)

seizures$p_sci = scientific(CNS$p.value, digits = 3)

manhattan(subset(CNS, p.value<0.05), chr="CHR", bp="POS", snp="SNPID", p="p.value", ylim=c(2, 8), col=c("#182cdb", "#85CADF"))

#qqplot for CNS effects

qq(CNS$p.value)

#prepare a file to input to FUMA/LocusZoom to examine results

write.table(CNS[ , c("CHR", "POS", "Allele1", "Allele2", "N", "BETA", "SE", "p_sci", "AF_Allele2")], file="/Users/cobeystubbs/Desktop/CNSeffects_FUMA.txt", sep="\t", row.names = F, col.names = T, quote=F)









