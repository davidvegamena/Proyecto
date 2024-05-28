library(haven)
studysample_allmerged <- read_dta("D:/David/Documents/Universidad/Proyecto II/Duflo 2015/studysample_allmerged.dta")

Duflo <- studysample_allmerged[, c("schoolid", "pupilid", "sex", "Utreat",
        "HIVtreat", "Q_a3_8_date_of_birth", "Q_b1_10", "Q_b1_12_edu_level",
        "Q_b2_48_agreement", "Q_b2_49_agreement","Q_b2_50_agreement",
        "Q_b2_51_agreement","Q_b2_52_agreement", "Q_b2_53_agreement",
        "Q_b2_61_decision_kids", "Q_b2_68_edu_comparison", "Q_b3_78",
        "Q_b3_79_prevent_hiv", "Q_b3_80_how_prevent_hiv", "Q_b3_81",
        "Q_b3_83", "Q_b3_84","Q_b3_88", "Q_b3_94","Q_b3_102_get_condoms",
        "Q_b3_104_agree_with_statement", "Q_b3_105_agree_with_statement",
        "Q_b3_106_agree_with_statement", "Q_b3_107_agree_with_statement",
        "Q_b3_108_agree_with_statement", "Q_b4_114_marital_status", 
        "Q_b4_117_had_sex", "Q_b4_118_sex_one_person", "Q_b4_119_age_1st_sex",
        "Q_b4_120_6months", "Q_b4_121_sex_patner", "Q_b4_123_use_condom",
        "Q_b4_126_used_condom", "Q_b4_127_last_sex_use_condom", "Q_b4_130",
        "Q_b4_132_who_decides", "Q_b5_135_change_behavior", "Q_b5_136",
        "Q_b6_139_given_birth","Q_b6_161_pregnant","Q_b6_163_wanted_pregnancy",
        "Q_b6_164", "Q_b6_165","Q_b6_202_sti","Q_b6_203_discharge", "Q_b6_204",
        "Q_b6_206_seek_treatment", "Q_b6_208_inform_patner", "Q_b6_209_stop_sex",
        "Q_b6_210_use_condom", "hiv_positive", "hsv2_positive")]

head(Duflo)

# Age

for (i in 1:nrow(Duflo)) {
  Duflo$Q_a3_8_date_of_birth[i] = Duflo$Q_a3_8_date_of_birth[i] - 1992 + 17
  }

for (i in 1:nrow(Duflo)){
  if (is.na(Duflo$Q_a3_8_date_of_birth[i])) {
    Duflo$Q_a3_8_date_of_birth[i] <- 18
  }
  else if (Duflo$Q_a3_8_date_of_birth[i] <= 18){
    Duflo$Q_a3_8_date_of_birth[i] <- "18"
  }
  else if (19 <= Duflo$Q_a3_8_date_of_birth[i] & Duflo$Q_a3_8_date_of_birth[i] <= 20){
    Duflo$Q_a3_8_date_of_birth[i] <- "19-20"
  }
  else{
    Duflo$Q_a3_8_date_of_birth[i] <- "21-22"
  }
}

View(Duflo)

names(Duflo)[names(Duflo) == "Q_a3_8_date_of_birth"] <- "age"

# Level of education

for (i in 1:nrow(Duflo)){
  if(is.na(Duflo$Q_b1_12_edu_level[i])){
    Duflo$Q_b1_12_edu_level <- "None or primary"
  }
  else if(Duflo$Q_b1_12_edu_level[i] == ""){
    Duflo$Q_b1_12_edu_level[i] <- "None or primary"
  }
  else if(Duflo$Q_b1_12_edu_level[i] == "F1" || Duflo$Q_b1_12_edu_level[i] == "F2" || Duflo$Q_b1_12_edu_level[i] == 6){
    Duflo$Q_b1_12_edu_level[i] <- "F1-F2"
  }
  else if(Duflo$Q_b1_12_edu_level[i] == 7 || Duflo$Q_b1_12_edu_level[i] == "F3" || Duflo$Q_b1_12_edu_level[i] == "F4" || Duflo$Q_b1_12_edu_level[i] == 8){
    Duflo$Q_b1_12_edu_level[i] <- "F3-F4"
  }
  else{
    Duflo$Q_b1_12_edu_level[i] <- "F5 or higher"
  }
}

names(Duflo)[names(Duflo) == "Q_b1_12_edu_level"] <- "level of education"

CowanDuflo <- Duflo[, c("sex", "age", "level of education")]
View(CowanDuflo)

# Ever married 

for(i in 1:nrow(Duflo)){
  if(is.na(Duflo$Q_b4_114_marital_status[i])){
    Duflo$Q_b4_114_marital_status[i] <- 0
  }
  else if(Duflo$Q_b4_114_marital_status[i] != 4){
    Duflo$Q_b4_114_marital_status[i] <- 1
  }
  else{
    Duflo$Q_b4_114_marital_status[i] <- 0
  }
}

names(Duflo)[names(Duflo) == "Q_b4_114_marital_status"] <- "ever married"

CowanDuflo <- Duflo[, c("Utreat", "HIVtreat","sex", "age", "level of education", "ever married")]

# HIV acquisition /5

valores_separados <- strsplit(as.character(Duflo$Q_b3_78), ", ")

Duflo$"HIV acquisition" <- 1

for (i in 1:length(valores_separados)) {
  if (any(as.numeric(valores_separados[[i]]) %in% c(12, 14, 15, 16, 17, 18))) {
    Duflo$"HIV acquisition"[i] <- 0
  }
}

for (i in 1:nrow(Duflo)){
  if(!is.na(Duflo$Q_b3_81[i]) && Duflo$Q_b3_81[i] == 1){
    Duflo$"HIV acquisition"[i] <- Duflo$"HIV acquisition"[i] +1
  }
}

for (i in 1:nrow(Duflo)){
  if(!is.na(Duflo$Q_b3_83[i]) && Duflo$Q_b3_83[i] == 1){
    Duflo$"HIV acquisition"[i] <- Duflo$"HIV acquisition"[i] +1
  }
}

for (i in 1:nrow(Duflo)){
  if(!is.na(Duflo$Q_b3_84[i]) && Duflo$Q_b3_84[i] == 1){
    Duflo$"HIV acquisition"[i] <- Duflo$"HIV acquisition"[i] +1
  }
}

for (i in 1:nrow(Duflo)){
  if(!is.na(Duflo$Q_b3_88[i]) && Duflo$Q_b3_88[i] == 2){
    Duflo$"HIV acquisition"[i] <- Duflo$"HIV acquisition"[i] +1
  }
}

# HIV-testing self-efficacy

Duflo$"HIV-testing self-efficacy" <- 0

for (i in 1:nrow(Duflo)){
  if(!is.na(Duflo$Q_b3_94[i]) && Duflo$Q_b3_94[i] == 1){
    Duflo$"HIV-testing self-efficacy"[i] <- 1
  }
}

# Condom self-efficacy

Duflo$"Condom self-efficacy" <- 0

for (i in 1:nrow(Duflo)){
  if(!is.na(Duflo$Q_b3_102_get_condoms[i]) && Duflo$Q_b3_102_get_condoms[i] == 1){
    Duflo$"Condom self-efficacy"[i] <- Duflo$"Condom self-efficacy"[i]+1
  }
}

# Control around sexual partners /4

Duflo$"Control around sexual partners" <- 0

for (i in 1:nrow(Duflo)){
  if(!is.na(Duflo$Q_b3_106_agree_with_statement[i]) && Duflo$Q_b3_106_agree_with_statement[i] %in% c(4, 5)){
    Duflo$"Control around sexual partners"[i] <- Duflo$"Control around sexual partners"[i]+1
  }
}

for (i in 1:nrow(Duflo)){
  if(!is.na(Duflo$Q_b3_107_agree_with_statement[i]) && Duflo$Q_b3_107_agree_with_statement[i] %in% c(4, 5)){
    Duflo$"Control around sexual partners"[i] <- Duflo$"Control around sexual partners"[i]+1
  }
}

for (i in 1:nrow(Duflo)){
  if(!is.na(Duflo$Q_b3_108_agree_with_statement[i]) && Duflo$Q_b3_108_agree_with_statement[i] %in% c(4, 5)){
    Duflo$"Control around sexual partners"[i] <- Duflo$"Control around sexual partners"[i]+1
  }
}

for (i in 1:nrow(Duflo)){
  if(!is.na(Duflo$Q_b4_132_who_decides[i]) && Duflo$Q_b4_132_who_decides[i] %in% c(1, 3)){
    Duflo$"Control around sexual partners"[i] <- Duflo$"Control around sexual partners"[i]+1
  }
}

# Safe sex and condoms /5

Duflo$"Safe sex and condoms" <- 0

for (i in 1:nrow(Duflo)){
  if(!is.na(Duflo$Q_b5_135_change_behavior[i]) && Duflo$Q_b5_135_change_behavior[i] %in% c(1)){
    Duflo$"Safe sex and condoms"[i] <- Duflo$"Safe sex and condoms"[i]+1
  }
}

for (i in 1:nrow(Duflo)){
  if(!is.na(Duflo$Q_b5_136[i])){
    Duflo$"Safe sex and condoms"[i] <- Duflo$"Safe sex and condoms"[i]+1
  }
}

for (i in 1:nrow(Duflo)){
  if(!is.na(Duflo$Q_b6_208_inform_patner[i]) && Duflo$Q_b6_208_inform_patner[i]==1){
    Duflo$"Safe sex and condoms"[i] <- Duflo$"Safe sex and condoms"[i]+1
  }
}

for (i in 1:nrow(Duflo)){
  if(!is.na(Duflo$Q_b6_209_stop_sex[i]) && Duflo$Q_b6_209_stop_sex[i]==1){
    Duflo$"Safe sex and condoms"[i] <- Duflo$"Safe sex and condoms"[i]+1
  }
}

for (i in 1:nrow(Duflo)){
  if(!is.na(Duflo$Q_b6_210_use_condom[i]) && Duflo$Q_b6_210_use_condom[i]==1){
    Duflo$"Safe sex and condoms"[i] <- Duflo$"Safe sex and condoms"[i]+1
  }
}


Duflo_sin_NA_HIV <- Duflo[!is.na(Duflo$hiv_positive), ]
View(Duflo_sin_NA_HIV)

CowanDuflo <- Duflo[, c("HIVtreat", "sex", "age", "level of education", "ever married", 
                        "HIV acquisition", "HIV-testing self-efficacy", 
                        "Condom self-efficacy", "Control around sexual partners",
                        "Safe sex and condoms")]
# Ever had sex

CowanDuflo$"Ever had sex" <- Duflo$Q_b4_117_had_sex

for (i in 1:nrow(CowanDuflo)){
  if(!is.na(CowanDuflo$"Ever had sex"[i]) && CowanDuflo$"Ever had sex"[i] == 1){
    CowanDuflo$"Ever had sex"[i] <- 1
  }
  else{
    CowanDuflo$"Ever had sex"[i] <- 0
  }
}

# Two or more lifetime partners

CowanDuflo$"Two or more lifetime partners" <- Duflo$Q_b4_118_sex_one_person

for (i in 1:nrow(CowanDuflo)){
  if(!is.na(CowanDuflo$"Two or more lifetime partners"[i]) && CowanDuflo$"Two or more lifetime partners"[i] == 2){
    CowanDuflo$"Two or more lifetime partners"[i] <- 1
  }
  else{
    CowanDuflo$"Two or more lifetime partners"[i] <- 0
  }
}

# Sexual debut 17 or younger

CowanDuflo$"Sexual debut 17 or younger" <- Duflo$Q_b4_119_age_1st_sex

for (i in 1:nrow(CowanDuflo)){
  if(!is.na(CowanDuflo$"Sexual debut 17 or younger"[i]) && CowanDuflo$"Sexual debut 17 or younger"[i] <= 17){
    CowanDuflo$"Sexual debut 17 or younger"[i] <- 1
  }
  else{
    CowanDuflo$"Sexual debut 17 or younger"[i] <- 0
  }
}

# Two or more partners in last 12m

CowanDuflo$"Two or more partners in last 12m" <- Duflo$Q_b4_120_6months

for (i in 1:nrow(CowanDuflo)){
  if(!is.na(CowanDuflo$"Two or more partners in last 12m"[i]) && CowanDuflo$"Two or more partners in last 12m"[i] >= 2){
    CowanDuflo$"Two or more partners in last 12m"[i] <- 1
  }
  else{
    CowanDuflo$"Two or more partners in last 12m"[i] <- 0
  }
}

# Did not use condom at last sex

CowanDuflo$"Did not use condom at last sex" <- Duflo$Q_b4_127_last_sex_use_condom

for (i in 1:nrow(CowanDuflo)){
  if(!is.na(CowanDuflo$"Did not use condom at last sex"[i]) && CowanDuflo$"Did not use condom at last sex"[i] %in% c(2,3)){
    CowanDuflo$"Did not use condom at last sex"[i] <- 1
  }
  else{
    CowanDuflo$"Did not use condom at last sex"[i] <- 0
  }
}

# Sought treatment for STD symptoms

CowanDuflo$"Sought treatment for STD symptoms" <- Duflo$Q_b6_206_seek_treatment

for (i in 1:nrow(CowanDuflo)){
  if(!is.na(CowanDuflo$"Sought treatment for STD symptoms"[i]) && CowanDuflo$"Sought treatment for STD symptoms"[i] == 1){
    CowanDuflo$"Sought treatment for STD symptoms"[i] <- 1
  }
  else{
    CowanDuflo$"Sought treatment for STD symptoms"[i] <- 0
  }
}

# Genital discharge prevalence

CowanDuflo$"Genital discharge prevalence" <- Duflo$Q_b6_203_discharge

for (i in 1:nrow(CowanDuflo)){
  if(!is.na(CowanDuflo$"Genital discharge prevalence"[i]) && CowanDuflo$"Genital discharge prevalence"[i] == 1){
    CowanDuflo$"Genital discharge prevalence"[i] <- 1
  }
  else{
    CowanDuflo$"Genital discharge prevalence"[i] <- 0
  }
}

# Genital warts or sores prevalence

CowanDuflo$"Genital warts or sores prevalence" <- Duflo$Q_b6_204

for (i in 1:nrow(CowanDuflo)){
  if(!is.na(CowanDuflo$"Genital warts or sores prevalence"[i]) && CowanDuflo$"Genital warts or sores prevalence"[i] == 1){
    CowanDuflo$"Genital warts or sores prevalence"[i] <- 1
  }
  else{
    CowanDuflo$"Genital warts or sores prevalence"[i] <- 0
  }
}

# Rights within marriage

CowanDuflo$"Rights within marriage" <- Duflo$Q_b2_61_decision_kids

for (i in 1:nrow(CowanDuflo)){
  if(!is.na(CowanDuflo$"Rights within marriage"[i]) && CowanDuflo$"Rights within marriage"[i] == 3){
    CowanDuflo$"Rights within marriage"[i] <- 1
  }
  else{
    CowanDuflo$"Rights within marriage"[i] <- 0
  }
}

# Reported past or current pregnancy

CowanDuflo$"Reported past or current pregnancy" <- Duflo$Q_b6_163_wanted_pregnancy

for (i in 1:nrow(CowanDuflo)){
  if(!is.na(CowanDuflo$"Reported past or current pregnancy"[i]) && CowanDuflo$"Reported past or current pregnancy"[i] %in% c(1,2,3)){
    CowanDuflo$"Reported past or current pregnancy"[i] <- 1
  }
  else{
    CowanDuflo$"Reported past or current pregnancy"[i] <- 0
  }
}

# Prevalence of any symptom of STD

CowanDuflo$"Prevalence of any symptom of STD" <- Duflo$Q_b6_202_sti

for (i in 1:nrow(CowanDuflo)){
  if(!is.na(CowanDuflo$"Prevalence of any symptom of STD"[i]) && CowanDuflo$"Prevalence of any symptom of STD"[i] == 1){
    CowanDuflo$"Prevalence of any symptom of STD"[i] <- 1
  }
  else{
    CowanDuflo$"Prevalence of any symptom of STD"[i] <- 0
  }
}

# HIV infection

CowanDuflo$"HIV infection" <- Duflo$hiv_positive

# HSV-2 infection

CowanDuflo$"HSV-2 infection" <- Duflo$hsv2_positive

for (i in 1:nrow(CowanDuflo)){
  if (CowanDuflo$`HIV acquisition`[i] >= 4){
    CowanDuflo$`HIV acquisition`[i] <- 1
  }
  else{
    CowanDuflo$`HIV acquisition`[i] <- 0 
  }
}

for (i in 1:nrow(CowanDuflo)){
  if (CowanDuflo$`Control around sexual partners`[i] >= 3){
    CowanDuflo$`Control around sexual partners`[i] <- 1
  }
  else{
    CowanDuflo$`Control around sexual partners`[i] <- 0 
  }
}

for (i in 1:nrow(CowanDuflo)){
  if (CowanDuflo$`Safe sex and condoms`[i] >= 4){
    CowanDuflo$`Safe sex and condoms`[i] <- 1
  }
  else{
    CowanDuflo$`Safe sex and condoms`[i] <- 0 
  }
}

CowanDuflo_sin_NA_HIV <- CowanDuflo[!is.na(CowanDuflo$"HIV infection"), ]
CowanDuflo_sin_NA_HIV_sin_NA_HSV2 <- CowanDuflo_sin_NA_HIV[!is.na(CowanDuflo_sin_NA_HIV$"HSV-2 infection"), ]

# CowanDuflo sin datos faltantes en diagnóstico 

View(CowanDuflo_sin_NA_HIV_sin_NA_HSV2)
library(xlsx)

# Escribir el data frame como un archivo CSV con punto y coma como separador
write.csv(CowanDuflo_sin_NA_HIV_sin_NA_HSV2, "dfCowanDuflo.csv", row.names = FALSE)
getwd()
