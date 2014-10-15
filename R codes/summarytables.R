## data cleaning and understanding
## TBI data

rm(list=ls())
setwd("/Users/askming/Dropbox/RA Works/Self/6. Real Data/Claudia TBI Data")

demographic<-read.csv("Demographic.csv",header=T)
# > dim(demographic)
# [1] 1239   33

# > names(demographic)
# [1] "IDNo"               "AdmNo"              "HINo"               "Gender"            
# [5] "Age"                "Race.text"          "InitialHospital"    "PH.Hypoxia"        
# [9] "PH.hypotension"     "Mechanism"          "ERGCSsum"           "ERGCSmotor"        
# [13] "ERLpupilsize"       "ERLpupilreactivity" "ERRpupilsize"       "ERRpupilreactivity"
# [17] "AIS"                "CT.Code"            "ICULOS"             "ICUDCHAI"          
# [21] "HospLOS"            "DischargeGCS"       "DischGOS"           "DischargeDRS"      
# [25] "Month1GCS"          "Mon1GOS"            "Month1DRS"          "Month3GCS"         
# [29] "Mon3GOS"            "Month3DRS"          "Month6GCS"          "Mon6GOS"           
# [33] "Month6DRS" 

#### 1. for Age
sum(is.na(demographic$Age))
# [1] 0
sum(demographic$Age < 0)
# [1] 1
Age = subset(demographic, Age > 0)$Age
round(c(mean(Age), sd(Age)), 1)
# [1] 34.3 14.4

### 2. sex
table(demographic$Gender)
# female   male  other 
#   188   1050      1 


### 3. race
table(demographic$Race.text)
# Amer Indian/Eskimo              Asian              black           Hispanic              other 
                 1                 34                307                537                  3 
#             white 
#                357 

### 4. Mechanism
levels(demographic$Mechanism)
                                    # assault            automobile               bicycle 
                   # 16                   215                   548                    10 
                  # bus             explosion             fall/jump         gunshot wound 
                    # 1                     1                   150                    83 
# hit by falling object   industrial accident                 moped            motorcycle 
                    # 8                     6                     1                    92 
      # offroad vehicle                 other  recreational vehicle                sports 
                    # 7                     3                     1                     4 
                # train                 truck               unknown 
                    # 6                    27                    60 


### 5. Pupils
pupils = numeric(dim(demographic)[1])
for (i in 1:length(pupils)){
	if (demographic$ERLpupilreactivity[i] == '+' & demographic$ERRpupilreactivity[i] == '+') pupils[i] = 'both reactive'
	else if (demographic$ERLpupilreactivity[i] == '-' & demographic$ERRpupilreactivity[i] == '-') pupils[i] = 'both unreactive'
	else if (demographic$ERLpupilreactivity[i] == '-' | demographic$ERRpupilreactivity[i] == '-') pupils[i] = '1 unreactive'
	else pupils[i] = 'untestable'
}
table(pupils)
#   1 unreactive   both reactive both unreactive      untestable 
#            130             445             343             321 


### 6. CT code
table(demographic$CT.Code)
#      D1  D2  D3  D4  M1  M2 PBI UNK 
#  9  19 373 227  13 531  56   1  10 

### 7. GOS
table(demographic$DischGOS)
#      D  GR LTF  MD PVS  SD W/D 
# 204 274   7  11  42 149 544   8

table(demographic$Mon1GOS)
#   1   2   3   4   5   8   9 
# 256 236 462 115  41   7  43

table(demographic$Mon3GOS)
#  1   2   3   4   5   8   9 
# 279  99 361 183 106   8 113 

table(demographic$Mon6GOS)
#   1   2   3   4   5   8   9 
# 303  49 258 170 167   8 173 


#################################################
#################################################
vital_signs<-read.csv("vital_signs.csv",header=T)
# > dim(vital_signs)
# [1] 65533    38

# > names(vital_signs)
# [1] "IDNo"               "HAI"                "GCS.eye"            "GCS.motor"         
# [5] "GCS.verbal"         "GCS.sum"            "L.pupil.size"       "L.pupil.reactivity"
# [9] "R.pupil.size"       "R.pupil.reactivity" "ICP"                "SBP"               
# [13] "DBP"                "MAP"                "CPP"                "SaO2"              
# [17] "ETCO2"              "SjvO2"              "SjvO2.artifact"     "PbtO2"             
# [21] "Pbto2.artifact"     "CBF"                "JVP"                "PWP"               
# [25] "CO"                 "CVP"                "Temperature"        "Brain.temperature" 
# [29] "Jugular.temperture" "PO2"                "PCO2"               "Hemoglobin"        
# [33] "Sodium"             "Glucose"            "Osmolality"         "PT"                
# [37] "PTT"                "Platelet.count"  

c(mean(vital_signs$ICP, na.rm=T), sd(vital_signs$ICP, na.rm=T))

c(mean(vital_signs$MAP, na.rm=T), sd(vital_signs$MAP, na.rm=T))

c(mean(vital_signs$CPP, na.rm=T), sd(vital_signs$CPP, na.rm=T))

c(mean(vital_signs$SjvO2, na.rm=T), sd(vital_signs$SjvO2, na.rm=T))

c(mean(vital_signs$SaO2, na.rm=T), sd(vital_signs$SaO2, na.rm=T))

c(mean(vital_signs$PbtO2, na.rm=T), sd(vital_signs$PbtO2, na.rm=T))


#################################################
#################################################
cbf<-read.csv("cbf_cmro2.csv",header=T)
# > dim(cbf)
# [1] 2589   59

# > names(cbf)
# [1] "IDNo"                   "CBFNo"                  "HAI"                   
# [4] "TD.CBF"                 "ICP"                    "MAP"                   
# [7] "Hgb"                    "PaO2"                   "SaO2"                  
# [10] "CaO2"                   "PaCO2"                  "CaCO2"                 
# [13] "PaH"                    "PjvO2"                  "SjvO2"                 
# [16] "CjvO2"                  "PjvCO2"                 "CjvCO2"                
# [19] "CjvH"                   "CBF10"                  "CBF15"                 
# [22] "CBFinf"                 "CBFcorr"                "CBFlow.limit"          
# [25] "CBFhigh.limit"          "CVR"                    "Alact"                 
# [28] "JVlact"                 "Agluc"                  "JVgluc"                
# [31] "IITS"                   "PVI"                    "CSF.pH"                
# [34] "CSF.lactate"            "CSF.glucose"            "CSF.hematocrit"        
# [37] "PWP"                    "CO"                     "VO2"                   
# [40] "SVR"                    "PVR"                    "Comments"              
# [43] "CMRO2"                  "AvCBF"                  "Left.mcaFV"            
# [46] "Left.acaFV"             "Left.pcaFV"             "Left.icaFV"            
# [49] "Left.Flow.Volume"       "Left.Hemispheric.CBF"   "Right.mcaFV"           
# [52] "Right.acaFV"            "Right.pcaFV"            "Right.icaFV"           
# [55] "Right.Flow.Volume"      "Right.Hemispheric.CBF"  "PbtO2"                 
# [58] "rCBF.at.TCD.probe.site" "rCBF.at.PbtO2.MD.site" 

### table the time points of measurement for each patient in vital_signs data with icp==na removed
no_measures<-tapply(vital_signs$HAI[!is.na(vital_signs$ICP)],vital_signs$IDNo[!is.na(vital_signs$ICP)],length)

### those patients have vital signs also have domographic info
indicator<-unique(vital_signs$IDNo)%in%unique(demographic$IDNo)
subvitalID<-unique(vital_signs$IDNo)[indicator]




   
