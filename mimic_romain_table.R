originaldb <- read_csv("~/Desktop/TEAM METHODS/Romain Bey/tabular mimic data/predictors.csv")
originaldb <-data.frame(originaldb)
cleaned_admissions<-read_csv("~/Desktop/TEAM METHODS/Romain Bey/tabular mimic data/cleaned_admissions.csv")
cleaned_ages<-read_csv("~/Desktop/TEAM METHODS/Romain Bey/tabular mimic data/cleaned_ages.csv")
cleaned_lab<-read_csv("~/Desktop/TEAM METHODS/Romain Bey/tabular mimic data/cleaned_labsfirstday.csv")
cleaned_transfers<-read_csv("~/Desktop/TEAM METHODS/Romain Bey/tabular mimic data/cleaned_transfers.csv")

head(originaldb)
head(admissions)
head(cleaned_ages)
head(cleaned_lab)
head(cleaned_transfers)

colnames(cleaned_admissions)[3]<-"hadm_id"

duplicate_hadm_id_admission<-table(cleaned_admissions$hadm_id)
table(duplicate_hadm_id_admission)

duplicate_hadm_id_originaldb<-table(originaldb$hadm_id)
table(duplicate_hadm_id_originaldb)

colnames(originaldb)
colnames(cleaned_admissions)

table(table(originaldb$icustay_id))
predb1<-merge(originaldb, cleaned_admissions, by="hadm_id")
table(table(predb1$icustay_id))

predb1$hospital_mortality<-ifelse(is.na(predb1$DEATHTIME_admissions)==TRUE, 0, 1)
head(predb1)

predb2<-merge(predb1, cleaned_ages, by="icustay_id")
head(predb2)
table(table(predb2$icustay_id))

predb3<-merge(predb2, cleaned_lab, by="icustay_id")
head(predb3)
table(table(predb3$icustay_id))

cleaned_transfers$MICUorSICU<-cleaned_transfers$CURR_CAREUNIT_transfers==("MICU") | cleaned_transfers$CURR_CAREUNIT_transfers==("SICU")
cleaned_transfers<-subset(cleaned_transfers, cleaned_transfers$MICUorSICU==TRUE)
colnames(cleaned_transfers)[4]<-"icustay_id"
head(cleaned_transfers)
table(table(cleaned_transfers$icustay_id))
n_occur <- data.frame(table(cleaned_transfers$icustay_id))
n_occur[n_occur$Freq > 1,]
cleaned_transfers<-subset(cleaned_transfers, !duplicated(icustay_id))
cleaned_transfers[which(cleaned_transfers$icustay_id=="200463"),]

nrow(cleaned_transfers)
nrow(predb3)

database<-merge(predb3, cleaned_transfers, by="icustay_id")
nrow(database)
summary(database)

write.csv(database, file="~/Desktop/TEAM METHODS/Romain Bey/tabular mimic data/cleaned_mimic.csv")

table(database$subject_id==database$SUBJECT_ID_admissions)
table(database$subject_id==database$SUBJECT_ID_transfers)
table(database$hadm_id==database$HADM_ID_transfers)

table(database$CURR_CAREUNIT_transfers)
prop.table(table(database$CURR_CAREUNIT_transfers))*100
median(database$age[database$CURR_CAREUNIT_transfers=="MICU"])
median(database$age[database$CURR_CAREUNIT_transfers=="SICU"])

prop.table(table(database$hospital_mortality[database$CURR_CAREUNIT_transfers=="SICU"]))*100
prop.table(table(database$hospital_mortality[database$CURR_CAREUNIT_transfers=="MICU"]))*100

plot(database$age_score[database$age<90]~database$age[database$age<90])
plot(database$bun_score~database$bun_max)

plot(database$hospital_mortality~database$sapsii_prob)
