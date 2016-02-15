############################################################
##### this part is  added for ICULOS regression analysis####
#################### Joint modeling #######################
############################################################

rm(list=ls())
setwd("/Users/askming/Dropbox/Research Works/Self/6. Real Data/Claudia TBI Data")

############################################################
################  data processing   ########################
############################################################

## load data
demog <- read.csv("demographic.csv",header=T)
vital_signs <- read.csv("vital_signs.csv",header=T)
cbf_cmro2 <- read.csv("cbf_cmro2.csv",header=T)
gluc_lact <- read.csv("gluc_lact.csv", header=T)
cma_md <- read.csv("cma_md.csv",header=T)

summary(demog$ICULOS, na.rm=T)

### 1. create variable event for death indicator
### here event is the ICU discharge and people who died in ICU is censored
LOS = subset(demog, select=c("IDNo", "ICULOS", "HospLOS"), !is.na(HospLOS) & !is.na(ICULOS))
subLOS = subset(LOS, HospLOS <= round(ICULOS, 1)) # patients died in ICU

for (i in 1: dim(demog)[1]){
	if (demog$IDNo[i] %in% subLOS$IDNo) demog$event[i] = 0
	else if (is.na(demog$HospLOS[i]) | is.na(demog$ICULOS[i])) demog$event[i] = NA
	else demog$event[i] = 1
}



#### 2. merge CBF15 and AvCBF into cbf_mew ###
cbf_cmro2$cbf_new <- cbf_cmro2$CBF15
for (i in 1:dim(cbf_cmro2)[1]){
	if (is.na(cbf_cmro2$cbf_new[i])) cbf_cmro2$cbf_new[i]<-cbf_cmro2$AvCBF[i]
}

#### 3.
gluc_lact_join <- join(cma_md,gluc_lact,by="IDNo",type="full")
for (i in 1:dim(gluc_lact_join)[1]){
	if (!is.na(gluc_lact_join$Pyruvate[i]) & !is.na(gluc_lact_join$Lactate[i]) & is.na(gluc_lact_join$L.P.Ratio[i])){
		gluc_lact_join$L.P.Ratio[i] <- gluc_lact_join$Lactate[i]/gluc_lact_join$Pyruvate[i]
	}
}


#### 4. merge all data
merge1 <- join(demog, vital_signs, by="IDNo", type="right")
merge2 <- join(merge1, cbf_cmro2, by=c("IDNo","HAI"), type="full")
overall <- join(merge2, gluc_lact_join, by=c("IDNo","HAI"), type="full")
sub_overall <- subset(overall, select=c("IDNo", "ICULOS","HospLOS", "event", "ERLpupilreactivity", "ERRpupilreactivity", "CT.Code", "AIS", "HAI", "ICP", "Age", "Gender", "GCS.sum", "MAP", "SjvO2", "PCO2", "PbtO2", "cbf_new", "CMRO2", "L.P.Ratio"), subset=!is.na(Gender) & ICP>0 & Age>0 & !is.na(event))

sub_overall$Gender <- factor(sub_overall$Gender)

# K-M plot of ICULOS
# surv_data = subset(sub_overall, select=c('ICULOS', 'event'), !is.na(event))
# library(survival)
# surv_data_type = with(surv_data, Surv(ICULOS, event==1))
# fit = survfit(surv_data_type~1)
# summary(fit)
# plot(fit, main="Kaplan-Meier estimate of ICULOS with 95% confidence bounds", xlab="time/day", ylab="survival function", col="blue")




############################################################
######  data for longitudinal model, data_longi   ###########
############################################################
# ICULOS is of unit day
# iculen = floor(tapply(sub_overall$ICULOS, sub_overall$IDNo, unique)*24)
# icutime = numeric(0)
# ID = integer(0)
# for (i in 1:length(iculen)){
		# icutime = c(icutime, seq(1:iculen[i]))
		# ID = c(ID, rep(unique(sub_overall$IDNo)[i], iculen[i]))
# }
# icudata = data.frame(IDNo = ID, HAI = icutime) # maximum observation time is the time of ICU discharge

# data_longi = join(icudata, sub_overall, by = c("IDNo", "HAI"), type="right")
data_longi = subset(sub_overall, select=c("IDNo", "HAI", "Age", "Gender", "ERLpupilreactivity", "ERRpupilreactivity", "CT.Code", "AIS", "ICP", "GCS.sum", "MAP", "SjvO2", "PCO2", "PbtO2", "cbf_new", "CMRO2", "L.P.Ratio"), !is.na(ICP)&!is.na(GCS.sum)&!is.na(MAP)&!is.na(SjvO2)&!is.na(PCO2)& ERLpupilreactivity %in% c('-', '+') & ERRpupilreactivity %in% c('-', '+') & CT.Code %in% c("D1","D2", "D3", "D4", "M1", "M2")& !is.na(AIS))

for (i in 1:dim(data_longi)[1]){
	if (data_longi$ERLpupilreactivity[i]=='-' & data_longi$ERRpupilreactivity[i]=='-') data_longi$eyereactivity[i]= 0 #worst
	else if (data_longi$ERLpupilreactivity[i]=='+' & data_longi$ERRpupilreactivity[i]=='+') data_longi$eyereactivity[i]= 2 #best
	else data_longi$eyereactivity[i]= 1 #medium

	if (data_longi$CT.Code[i] %in% c("D1","D2")) data_longi$newCT[i] = 'D1'
	else if (data_longi$CT.Code[i] %in% c("D3","D4")) data_longi$newCT[i] = 'D2'
	else if (data_longi$CT.Code[i] %in% c("M1","M2")) data_longi$newCT[i] = 'M'

}

data_longi$eyereactivity = as.factor(data_longi$eyereactivity)

# # max.hai = tapply(sub_overall$HAI, sub_overall$IDNo, max)
# # sum(max.hai > iculen)
# [1] 0


############################################################
#########  data for survival model, data_id   ##############
############################################################

data_id = subset(demog, select=c("IDNo", "ICULOS", "event", "Age", "Gender", "ERLpupilreactivity", "ERRpupilreactivity", "CT.Code", "AIS"), !is.na(Gender) & Age > 0 & Gender != 'other' & !is.na(event) & ERLpupilreactivity %in% c('-', '+') & ERRpupilreactivity %in% c('-', '+') & CT.Code %in% c("D1","D2", "D3", "D4", "M1", "M2") & !is.na(AIS))
data_id$iculos_hour = data_id$ICULOS*24
data_id$Gender = factor(data_id$Gender)

for (i in 1:dim(data_id)[1]){
	if (data_id$ERLpupilreactivity[i]=='-' & data_id$ERRpupilreactivity[i]=='-') data_id$eyereactivity[i]= 0 #worst
	else if (data_id$ERLpupilreactivity[i]=='+' & data_id$ERRpupilreactivity[i]=='+') data_id$eyereactivity[i]= 2 #best
	else data_id$eyereactivity[i]= 1 #medium

	if (data_id$CT.Code[i] %in% c("D1","D2")) data_id$newCT[i] = 'D12'
	else if (data_id$CT.Code[i] %in% c("D3","D4")) data_id$newCT[i] = 'D34'
	else if (data_id$CT.Code[i] %in% c("M1","M2")) data_id$newCT[i] = 'M12'

}
data_id$eyereactivity = as.factor(data_id$eyereactivity)

# two dataset should have the same patient group
data_id = subset(data_id, subset=unique(data_id$IDNo)%in%unique(data_longi$IDNo))


############################################################
#######################  fit joint model   #################
############################################################
library(JM)
surv_fit = coxph(Surv(iculos_hour, event) ~ Age + Gender + eyereactivity + newCT + AIS, data=data_id, x=TRUE)
lme_fit = lme(ICP ~   Age + Gender + eyereactivity + newCT + HAI + GCS.sum, random= ~ 1|IDNo, data=data_longi, na.action="na.omit")
JM_fit = jointModel(lme_fit, surv_fit, timeVar="HAI", method='piecewise-PH-GH')

summary(JM_fit)
xtable(JM_fit)


#### plotting
xtabs = xtabs(~event+newCT, data=data_id)

barplot(xtabs,  ylim=c(0,100), ylab='count')
legend("topleft", legend=c('deaths in ICU (censored)', 'ICU discharge (events)'), fill=c('black','gray'))

# fitlm = lm(ICULOS ~ Age + Gender + newCT + eyereactivity + AIS, data=data_id)
# summary(fitlm)