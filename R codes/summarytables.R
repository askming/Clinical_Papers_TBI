## data cleaning and understanding
## TBI data

rm(list=ls())
setwd("/Users/askming/Dropbox/RA Works/Self/6. Real Data/Claudia TBI Data")


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

sub_vital = subset(vital_signs, !is.na(ICP))
ID1 = unique(sub_vital$IDNo)


demographic = read.csv("Demographic.csv",header=T)
demographic = subset(demographic, Age >0 & Gender != 'other' & IDNo %in% ID1)
ID2 = unique(demographic$IDNo)

sub_vital = subset(sub_vital, IDNo %in% ID2)
length(unique(sub_vital$IDNo))

## 438 subjects in analysis

#################################################
#################################################

c(mean(vital_signs$ICP, na.rm=T), sd(vital_signs$ICP, na.rm=T))

c(mean(vital_signs$MAP, na.rm=T), sd(vital_signs$MAP, na.rm=T))

c(mean(vital_signs$CPP, na.rm=T), sd(vital_signs$CPP, na.rm=T))

c(mean(vital_signs$SjvO2, na.rm=T), sd(vital_signs$SjvO2, na.rm=T))

c(mean(vital_signs$SaO2, na.rm=T), sd(vital_signs$SaO2, na.rm=T))

c(mean(vital_signs$PCO2, na.rm=T), sd(vital_signs$PCO2, na.rm=T))

c(mean(vital_signs$PbtO2, na.rm=T), sd(vital_signs$PbtO2, na.rm=T))


#################################################
#################################################


#### 1. for Age
Age = subset(demographic)$Age
round(c(mean(Age), sd(Age)), 1)
# [1] 34.3 14.4

### 2. sex
table(demographic$Gender)
# female   male  other 
#     59    379      0 


### 3. race
table(demographic$Race.text)
# Amer Indian/Eskimo              Asian              black           Hispanic              other 
                 1                 34                307                537                  3 
#             white 
#                357 

### 4. Mechanism
levels(demographic$Mechanism)
table(demographic$Mechanism)
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






   
