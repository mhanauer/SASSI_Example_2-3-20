---
title: "SASSI 2-10-20 Contract"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Look for errors in each data set
```{r}
library(prettyR)
library(MBESS)
library(descr)
library(coefficientalpha)
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/SASSI/Data/2-10-20_contract")
clinical_sample = read.csv("clinical_sample.csv", header = TRUE, na.strings = c(98,255))
cross_validation_sample = read.csv("cross_validation_sample.csv", header = TRUE, na.strings = c(98,255))
development_sample = read.csv("development_sample.csv", header = TRUE, na.strings = c(98,255))
normative_sample = read.csv("normative_sample.csv", header= TRUE, na.strings = c(98,255))
stability_sample = read.csv("stability_sample.csv", header = TRUE, na.strings = c(98,255))
```
Review for errors and descriptives
```{r}
dim(clinical_sample)
dim(cross_validation_sample)
dim(development_sample)
dim(normative_sample)
dim(stability_sample)

#describe(clinical_sample)
## Do this to get counts if you are having trouble


#describe(development_sample)
#describe(cross_validation_sample)
#describe(normative_sample)


### NODIAG change to 1 = NODIAG; 0 = DIAG; 
### Change SASSDR  2 low prob = 0, 1 high prob = 1 
clinical_sample$NODIAG = ifelse(clinical_sample$NODIAG == 1,0,1)
clinical_sample$SASSDR = ifelse(clinical_sample$SASSDR == 2,0,1)

cross_validation_sample$NODIAG = ifelse(cross_validation_sample$NODIAG == 1,0,1)
cross_validation_sample$SASSDR = ifelse(cross_validation_sample$SASSDR == 2,0,1)

development_sample$NODIAG = ifelse(development_sample$NODIAG == 1,0,1)
development_sample$SASSDR = ifelse(development_sample$SASSDR == 2,0,1)

normative_sample$SASSDR = ifelse(normative_sample$SASSDR == 2,0,1) 
stability_sample$SASSDR = ifelse(stability_sample$SASSDR == 2,0,1)

```


Recode the vars (see AR3 scoring)
```{r}
#### Get rid of the "R" at the end of the S vars in stability sample
test_sample =  stability_sample
test_sample = stability_sample[,8:94]

names(test_sample) = gsub("R", "", names(test_sample))
stability_sample[,8:94] =  NULL

stability_sample = data.frame(stability_sample, test_sample)


#### ### Recode the variables
#frisk =   s53 (2).
clinical_sample$S53 = ifelse(clinical_sample$S53 == 2,1,2)
cross_validation_sample$S53 = ifelse(cross_validation_sample$S53 == 2,1,2)
development_sample$S53 = ifelse(development_sample$S53 == 2,1,2)
normative_sample$S53 = ifelse(normative_sample$S53 == 2,1,2)
stability_sample$S53 = ifelse(stability_sample$S53 == 2,1,2)

#oat =  s10,s14,s27,s30 (2).
clinical_sample$S10 = ifelse(clinical_sample$S10 == 2,1,2)
cross_validation_sample$S10 = ifelse(cross_validation_sample$S10 == 2,1,2)
development_sample$S10 = ifelse(development_sample$S10 == 2,1,2)
normative_sample$S10 = ifelse(normative_sample$S10 == 2,1,2)
stability_sample$S10 = ifelse(stability_sample$S10 == 2,1,2)

clinical_sample$S14 = ifelse(clinical_sample$S14 == 2,1,2)
cross_validation_sample$S14 = ifelse(cross_validation_sample$S14 == 2,1,2)
development_sample$S14 = ifelse(development_sample$S14 == 2,1,2)
normative_sample$S14 = ifelse(normative_sample$S14 == 2,1,2)
stability_sample$S14 = ifelse(stability_sample$S14 == 2,1,2)

clinical_sample$S27 = ifelse(clinical_sample$S27 == 2,1,2)
cross_validation_sample$S27 = ifelse(cross_validation_sample$S27 == 2,1,2)
development_sample$S27 = ifelse(development_sample$S27 == 2,1,2)
normative_sample$S27 = ifelse(normative_sample$S27 == 2,1,2)
stability_sample$S27 = ifelse(stability_sample$S27 == 2,1,2)

clinical_sample$S30 = ifelse(clinical_sample$S30 == 2,1,2)
cross_validation_sample$S30 = ifelse(cross_validation_sample$S30 == 2,1,2)
development_sample$S30 = ifelse(development_sample$S30 == 2,1,2)
normative_sample$S30 = ifelse(normative_sample$S30 == 2,1,2)
stability_sample$S30 = ifelse(stability_sample$S30 == 2,1,2)


#### sat = S11,s15,s20,s53,s61,s67 (2).
clinical_sample$S11 = ifelse(clinical_sample$S11 == 2,1,2)
cross_validation_sample$S11 = ifelse(cross_validation_sample$S11 == 2,1,2)
development_sample$S11 = ifelse(development_sample$S11 == 2,1,2)
normative_sample$S11 = ifelse(normative_sample$S11 == 2,1,2)
stability_sample$S11 = ifelse(stability_sample$S11 == 2,1,2)

clinical_sample$S15 = ifelse(clinical_sample$S15 == 2,1,2)
cross_validation_sample$S15 = ifelse(cross_validation_sample$S15 == 2,1,2)
development_sample$S15 = ifelse(development_sample$S15 == 2,1,2)
normative_sample$S15 = ifelse(normative_sample$S15 == 2,1,2)
stability_sample$S15 = ifelse(stability_sample$S15 == 2,1,2)

clinical_sample$S20 = ifelse(clinical_sample$S20 == 2,1,2)
cross_validation_sample$S20 = ifelse(cross_validation_sample$S20 == 2,1,2)
development_sample$S20 = ifelse(development_sample$S20 == 2,1,2)
normative_sample$S20 = ifelse(normative_sample$S20 == 2,1,2)
stability_sample$S20 = ifelse(stability_sample$S20 == 2,1,2)


clinical_sample$S61 = ifelse(clinical_sample$S61 == 2,1,2)
cross_validation_sample$S61 = ifelse(cross_validation_sample$S61 == 2,1,2)
development_sample$S61 = ifelse(development_sample$S61 == 2,1,2)
normative_sample$S61 = ifelse(normative_sample$S61 == 2,1,2)
stability_sample$S61 = ifelse(stability_sample$S61 == 2,1,2)

clinical_sample$S67 = ifelse(clinical_sample$S67 == 2,1,2)
cross_validation_sample$S67 = ifelse(cross_validation_sample$S67 == 2,1,2)
development_sample$S67 = ifelse(development_sample$S67 == 2,1,2)
normative_sample$S67 = ifelse(normative_sample$S67 == 2,1,2)
stability_sample$S67 = ifelse(stability_sample$S67 == 2,1,2)

#DEF  = S5,S14,S22,S24,S30  (1)   AND   S2,S4,S9,S17,S23,S68,S69 (2).
## Recode S14, S30
clinical_sample$S14_DEF = ifelse(clinical_sample$S14 == 1,2,1)
cross_validation_sample$S14_DEF = ifelse(cross_validation_sample$S14 == 1,2,1)
development_sample$S14_DEF = ifelse(development_sample$S14 == 1,2,1)
normative_sample$S14_DEF = ifelse(normative_sample$S14 == 1,2,1)
stability_sample$S14_DEF = ifelse(stability_sample$S14 == 1,2,1)

clinical_sample$S30_DEF = ifelse(clinical_sample$S30 == 1,2,1)
cross_validation_sample$S30_DEF = ifelse(cross_validation_sample$S30 == 1,2,1)
development_sample$S30_DEF = ifelse(development_sample$S30 == 1,2,1)
normative_sample$S30_DEF = ifelse(normative_sample$S30 == 1,2,1)
stability_sample$S30_DEF = ifelse(stability_sample$S30 == 1,2,1)

clinical_sample$S2_DEF = ifelse(clinical_sample$S2 == 2,1,2)
cross_validation_sample$S2_DEF = ifelse(cross_validation_sample$S2 == 2,1,2)
development_sample$S2_DEF = ifelse(development_sample$S2 == 2,1,2)
normative_sample$S2_DEF = ifelse(normative_sample$S2 == 2,1,2)
stability_sample$S2_DEF = ifelse(stability_sample$S2 == 2,1,2)

##########################

clinical_sample$S4 = ifelse(clinical_sample$S4 == 2,1,2)
cross_validation_sample$S4 = ifelse(cross_validation_sample$S4 == 2,1,2)
development_sample$S4 = ifelse(development_sample$S4 == 2,1,2)
normative_sample$S4 = ifelse(normative_sample$S4 == 2,1,2)
stability_sample$S4 = ifelse(stability_sample$S4 == 2,1,2)

clinical_sample$S9 = ifelse(clinical_sample$S9 == 2,1,2)
cross_validation_sample$S9 = ifelse(cross_validation_sample$S9 == 2,1,2)
development_sample$S9 = ifelse(development_sample$S9 == 2,1,2)
normative_sample$S9 = ifelse(normative_sample$S9 == 2,1,2)
stability_sample$S9 = ifelse(stability_sample$S9 == 2,1,2)

clinical_sample$S17 = ifelse(clinical_sample$S17 == 2,1,2)
cross_validation_sample$S17 = ifelse(cross_validation_sample$S17 == 2,1,2)
development_sample$S17 = ifelse(development_sample$S17 == 2,1,2)
normative_sample$S17 = ifelse(normative_sample$S17 == 2,1,2)
stability_sample$S17 = ifelse(stability_sample$S17 == 2,1,2)

clinical_sample$S23 = ifelse(clinical_sample$S23 == 2,1,2)
cross_validation_sample$S23 = ifelse(cross_validation_sample$S23 == 2,1,2)
development_sample$S23 = ifelse(development_sample$S23 == 2,1,2)
normative_sample$S23 = ifelse(normative_sample$S23 == 2,1,2)
stability_sample$S23 = ifelse(stability_sample$S23 == 2,1,2)

clinical_sample$S68 = ifelse(clinical_sample$S68 == 2,1,2)
cross_validation_sample$S68 = ifelse(cross_validation_sample$S68 == 2,1,2)
development_sample$S68 = ifelse(development_sample$S68 == 2,1,2)
normative_sample$S68 = ifelse(normative_sample$S68 == 2,1,2)
stability_sample$S68 = ifelse(stability_sample$S68 == 2,1,2)
### Change for DEF
clinical_sample$S69_DEF = ifelse(clinical_sample$S69 == 2,1,2)
cross_validation_sample$S69_DEF = ifelse(cross_validation_sample$S69 == 2,1,2)
development_sample$S69_DEF = ifelse(development_sample$S69 == 2,1,2)
normative_sample$S69_DEF = ifelse(normative_sample$S69 == 2,1,2)
stability_sample$S69_DEF = ifelse(stability_sample$S69 == 2,1,2)

# sam s11 
clinical_sample$S11 = ifelse(clinical_sample$S11 == 2,1,2)
cross_validation_sample$S11 = ifelse(cross_validation_sample$S11 == 2,1,2)
development_sample$S11 = ifelse(development_sample$S11 == 2,1,2)
normative_sample$S11 = ifelse(normative_sample$S11 == 2,1,2)
stability_sample$S11 = ifelse(stability_sample$S11 == 2,1,2)

# COR= s3,s15,s27 (2).
clinical_sample$S3 = ifelse(clinical_sample$S3 == 2,1,2)
cross_validation_sample$S3 = ifelse(cross_validation_sample$S3 == 2,1,2)
development_sample$S3 = ifelse(development_sample$S3 == 2,1,2)
normative_sample$S3 = ifelse(normative_sample$S3 == 2,1,2)
stability_sample$S3 = ifelse(stability_sample$S3 == 2,1,2)

#VAL    = S15,S27,S30,S36,S47    (1)  AND    S1,S23,S25,S32,S51,S54 (2).

### Reverse code for VAL S15, S27, 30
clinical_sample$S15_VAL = ifelse(clinical_sample$S15 == 1,2,1)
cross_validation_sample$S15_VAL = ifelse(cross_validation_sample$S15 == 1,2,1)
development_sample$S15_VAL = ifelse(development_sample$S15 == 1,2,1)
normative_sample$S15_VAL = ifelse(normative_sample$S15 == 1,2,1)
stability_sample$S15_VAL = ifelse(stability_sample$S15 == 1,2,1)

clinical_sample$S27_VAL = ifelse(clinical_sample$S27 == 1,2,1)
cross_validation_sample$S27_VAL = ifelse(cross_validation_sample$S27 == 1,2,1)
development_sample$S27_VAL = ifelse(development_sample$S27 == 1,2,1)
normative_sample$S27_VAL = ifelse(normative_sample$S27 == 1,2,1)
stability_sample$S27_VAL = ifelse(stability_sample$S27 == 1,2,1)

clinical_sample$S30_VAL = ifelse(clinical_sample$S30 == 1,2,1)
cross_validation_sample$S30_VAL = ifelse(cross_validation_sample$S30 == 1,2,1)
development_sample$S30_VAL = ifelse(development_sample$S30 == 1,2,1)
normative_sample$S30_VAL = ifelse(normative_sample$S30 == 1,2,1)
stability_sample$S30_VAL = ifelse(stability_sample$S30 == 1,2,1)

### This is a 2 for VAL and 1 for other vars
clinical_sample$S25_VAL = ifelse(clinical_sample$S25 == 2,1,2)
cross_validation_sample$S25_VAL = ifelse(cross_validation_sample$S25 == 2,1,2)
development_sample$S25_VAL = ifelse(development_sample$S25 == 2,1,2)
normative_sample$S25_VAL = ifelse(normative_sample$S25 == 2,1,2)
stability_sample$S25_VAL = ifelse(stability_sample$S25 == 2,1,2)

clinical_sample$S32_VAL = ifelse(clinical_sample$S32 == 2,1,2)
cross_validation_sample$S32_VAL = ifelse(cross_validation_sample$S32 == 2,1,2)
development_sample$S32_VAL = ifelse(development_sample$S32 == 2,1,2)
normative_sample$S32_VAL = ifelse(normative_sample$S32 == 2,1,2)
stability_sample$S32_VAL = ifelse(stability_sample$S32 == 2,1,2)



### These are just two for VAL
clinical_sample$S51_VAL = ifelse(clinical_sample$S51 == 2,1,2)
cross_validation_sample$S51_VAL = ifelse(cross_validation_sample$S51 == 2,1,2)
development_sample$S51_VAL = ifelse(development_sample$S51 == 2,1,2)
normative_sample$S51_VAL = ifelse(normative_sample$S51 == 2,1,2)
stability_sample$S51_VAL = ifelse(stability_sample$S51 == 2,1,2)

clinical_sample$S54 = ifelse(clinical_sample$S54 == 2,1,2)
cross_validation_sample$S54 = ifelse(cross_validation_sample$S54 == 2,1,2)
development_sample$S54 = ifelse(development_sample$S54 == 2,1,2)
normative_sample$S54 = ifelse(normative_sample$S54 == 2,1,2)
stability_sample$S54 = ifelse(stability_sample$S54 == 2,1,2)

clinical_sample$S1 = ifelse(clinical_sample$S1 == 2,1,2)
cross_validation_sample$S1 = ifelse(cross_validation_sample$S1 == 2,1,2)
development_sample$S1 = ifelse(development_sample$S1 == 2,1,2)
normative_sample$S1 = ifelse(normative_sample$S1 == 2,1,2)
stability_sample$S1 = ifelse(stability_sample$S1 == 2,1,2)

```
Fisher test's for differences in development and cross validation sample
```{r}
cross_validation_sample_demos = cross_validation_sample[,c(2:13)]
cross_validation_sample_demos$ID = rep(0, dim(cross_validation_sample_demos)[1])


development_sample_demos = development_sample[,c(2:13)]
development_sample_demos$ID = rep(1, dim(development_sample_demos)[1])

cross_development_demos = rbind(cross_validation_sample_demos, development_sample_demos)
ID = cross_development_demos$ID
cross_development_demos$ID = NULL
fisher.test(cross_development_demos$AGE, ID)
### Need to collapse  ETHN, LIVING_SIT, MANDATED, REFERRAL
describe.factor(cross_development_demos$ETHN)
cross_development_demos$ETHN = ifelse(cross_development_demos$ETHN == 7, 1,0)
### Causing problems drop
describe.factor(cross_development_demos$LIVING_SIT)
#cross_development_demos$LIVING_SIT = ifelse(cross_development_demos$LIVING_SIT == 9,1,0)
cross_development_demos$LIVING_SIT = NULL
describe.factor(cross_development_demos$MANDATED)
describe.factor(cross_development_demos$REFERRAL)
cross_development_demos$REFERRAL = ifelse(cross_development_demos$REFERRAL == 7,1,0)
head(cross_development_demos)
describe.factor(cross_development_demos$DWI_ARREST)
### Not enough to include
cross_development_demos$DWI_ARREST = NULL
### Employment status too few
describe.factor(cross_development_demos$EMP_STATUS)
cross_development_demos$EMP_STATUS = NULL
describe.factor(cross_development_demos$TOT_ARREST)
cross_development_demos$TOT_ARREST = ifelse(cross_development_demos$TOT_ARREST == 0,1,0)
describe.factor(cross_development_demos$PRIOR_TREAT)
cross_development_demos$PRIOR_TREAT = ifelse(cross_development_demos$PRIOR_TREAT == 1, 1,0)
head(cross_development_demos)
#cross_development_demos = as.list(cross_development_demos)
######### Try testing each one
head(cross_development_demos)
fisher.test(cross_development_demos$AGE, ID)
fisher.test(cross_development_demos$SEX, ID)
fisher.test(cross_development_demos$ETH, ID)
fisher.test(cross_development_demos$TOT_ARREST, ID)
fisher.test(cross_development_demos$PRIOR_TREAT, ID)
fisher.test(cross_development_demos$CLIENTSETTING, ID)
### Significant at .01 alpha
fisher.test(cross_development_demos$MANDATED, ID)
compmeans(cross_development_demos$MANDATED, ID)
fisher.test(cross_development_demos$REFERRAL, ID)
compmeans(cross_development_demos$REFERRAL, ID)


```
Ok set up for stability samples
```{r}
head(stability_sample)
dim(stability_sample)
dim(clinical_sample)
stability_sample
```
Table 2 Omegas
```{r}
library(psych)
head(clinical_sample)
head(clinical_sample[,15:101])
#sassi_omega =  ci.reliability(clinical_sample[,15:101])

head(clinical_sample[,102:115])

#FVA  = SUM (A1 TO A14).
FVA_omega = ci.reliability(clinical_sample[,102:115])
FVA_omega
#FVOD = SUM (D1 TO D20).
FVOD_omega = ci.reliability(clinical_sample[,116:135])
FVOD_omega
#summary(omega(clinical_sample[,116:135]))
#####################
# frisk = s2,s21,s26,s55,s66 (1)  AND  s53 (2).
FRISK_omega = ci.reliability(clinical_sample[,c(16, 35, 40, 69, 80, 67)])
FRISK_omega
################################
#S18,s29,s35,s42,s51,s60,s65,s70 (1).
att = data.frame(S18 = clinical_sample$S18, S29 = clinical_sample$S29, S35 = clinical_sample$S35, S42 = clinical_sample$S42, S51 = clinical_sample$S51, S60 = clinical_sample$S60, S65 = clinical_sample$S65, S70 = clinical_sample$S70)
att_omega = ci.reliability(att)
att_omega
########################
# Symptoms sym = S16,S28,s31,s37,s38,s39,S49,S58,S63,s74,s75,s77,s80,s81,s82,s83,s84,s85,s87 (1).
sym = data.frame(S16 = clinical_sample$S16, S28 = clinical_sample$S28, S31 = clinical_sample$S31, S37 = clinical_sample$S37, S38 = clinical_sample$S38, S39 = clinical_sample$S39, S49 = clinical_sample$S49, S58 = clinical_sample$S58, S63 = clinical_sample$S63, S74 = clinical_sample$S74, S75 = clinical_sample$S75, S77 = clinical_sample$S77, S80 = clinical_sample$S80, S81 = clinical_sample$S81, S82 = clinical_sample$S82, S83 = clinical_sample$S83, S84 = clinical_sample$S84, S85 = clinical_sample$S85, S87 = clinical_sample$S87)
sym_omega = ci.reliability(sym)
sym_omega
#######################
# Obvious Attributes s8,S25,s41,s45,s69,s79,s86 (1)  AND  s10,s14,s27,s30 (2).
oat = data.frame(S8 = clinical_sample$S8, S25 = clinical_sample$S25, S41 = clinical_sample$S41, S45 = clinical_sample$S45, S69 = clinical_sample$S69, S79 = clinical_sample$S79, S86 = clinical_sample$S86, S10 = clinical_sample$S10, S14 = clinical_sample$S14, S27 = clinical_sample$S27, S30 = clinical_sample$S30)
oat_omega = ci.reliability(oat)
oat_omega
###########################################
# Subtle Attributes s6,s13, s19,S33,s44,s46,s50,s64, s66,s78 (1)  AND   S11,s15,s20,s53,s61,s67 (2).
sat = data.frame(S6 = clinical_sample$S6, S13 = clinical_sample$S13, S19 = clinical_sample$S19, S33 = clinical_sample$S33, S44 = clinical_sample$S44, S46 = clinical_sample$S46, S50 = clinical_sample$S50, S64 = clinical_sample$S64, S66 = clinical_sample$S66, S78 = clinical_sample$S78, S11 = clinical_sample$S11, S15 = clinical_sample$S15, S20 = clinical_sample$S20, S53 = clinical_sample$S53, S61 = clinical_sample$S61, S67 = clinical_sample$S67)
sat_omega = ci.reliability(sat)
sat_omega
summary(omega(sat))
sat_omega_complete = na.omit(sat)
cronbach.alpha(sat_omega_complete, CI = TRUE)
#####################
## Defensiveness S5,S14,S22,S24,S30  (1)   AND   S2,S4,S9,S17,S23,S68,S69 (2).
def = data.frame(S5 = clinical_sample$S5, S14 = clinical_sample$S14_DEF, S22 = clinical_sample$S22, S24 = clinical_sample$S24, S30 = clinical_sample$S30_DEF, S2 = clinical_sample$S2_DEF, S4 = clinical_sample$S4, S9 = clinical_sample$S9, S17 = clinical_sample$S17, S23 = clinical_sample$S23, S68 = clinical_sample$S68, S69 = clinical_sample$S69_DEF)
def_omega = ci.reliability(def)
def_omega
########
### sam s16,S21,s26,s38,s42,s46,s51,s60,s63,s70,s84 (1)   AND  s11 (2).
sam = data.frame(S16 =  clinical_sample$S16, S21 = clinical_sample$S21, S26 = clinical_sample$S26, S38 = clinical_sample$S38, S42 = clinical_sample$S42, S46 = clinical_sample$S46, S51 = clinical_sample$S51, S60 = clinical_sample$S60, S63 = clinical_sample$S63, S70 = clinical_sample$S70, S84 = clinical_sample$S84, S11 = clinical_sample$S11)
sam_omega = ci.reliability(sam)
sam_omega
######################
## Correctional s16,s19,s25,s26,s31,s32,s33,s49,s50,s56,s58,s65,s81,s83 (1)  AND  s3,s15,s27 (2).
correct = data.frame(S16 = clinical_sample$S16, S19 = clinical_sample$S19, S25 = clinical_sample$S25, S26 = clinical_sample$S26, S31 = clinical_sample$S31, S32 = clinical_sample$S32, S33 = clinical_sample$S33, S49 = clinical_sample$S49, S50= clinical_sample$S50, S56 = clinical_sample$S56, S58 = clinical_sample$S58, S65 = clinical_sample$S65, S81 = clinical_sample$S81, S83 = clinical_sample$S83, S3 = clinical_sample$S3, S15 = clinical_sample$S15, S27 = clinical_sample$S27)
correct_omega = ci.reliability(correct)
correct_omega

## VAL    = S15,S27,S30,S36,S47    (1)  AND    S1,S23,S25,S32,S51,S54 (2).
#val = data.frame(S15 = clinical_sample$S15_VAL, S27 = clinical_sample$S27_VAL, S30 = clinical_sample$S30_VAL, S36 = clinical_sample$S36, S1 = clinical_sample$S1, S23 = clinical_sample$S23, S32 = clinical_sample$S32_VAL, S51 = clinical_sample$S51_VAL, S54 = clinical_sample$S54)
#val_omega = ci.reliability(val)
#val_omega

# Rx Rx= s74, s75 (1). + SUM (D9+D17+D18).
rx = data.frame(S74 = clinical_sample$S74, S75 = clinical_sample$S75, D9 = clinical_sample$D9, D17 = clinical_sample$D17, D18 = clinical_sample$D18)
rx_omega = ci.reliability(rx)
rx_omega
```
Stability coefficents
```{r}
### Need merge those with client in stability with normative

dim(stability_sample)
names(stability_sample)[1] = "ID"
names(normative_sample)[1] = "ID"
dim(stability_sample)
stable_norm = merge(stability_sample, normative_sample, by = "ID", all.x = TRUE)
dim(stable_norm)
stable_norm

SASDR_stable = cor.test(stable_norm$SASSDR.x, stable_norm$SASSDR.y)
SASDR_stable

FVA_stable =  cor.test(stable_norm$FVA.x, stable_norm$FVA.y)
FVA_stable

FVOD_stable = cor.test(stable_norm$FVOD.x, stable_norm$FVOD.y)
FVOD_stable

FRISK_stable = cor.test(stable_norm$frisk.x, stable_norm$frisk.y)
FRISK_stable

att_stable = cor.test(stable_norm$att.x, stable_norm$att.y)
att_stable

sym_stable = cor.test(stable_norm$sym.x, stable_norm$sym.y)
sym_stable

oat_stable = cor.test(stable_norm$oat.x, stable_norm$oat.y)
oat_stable

sat_stable = cor.test(stable_norm$sat.x, stable_norm$sat.y)
sat_stable

def_stable = cor.test(stable_norm$DEF.x, stable_norm$DEF.y)
def_stable

sam_stable = cor.test(stable_norm$sam.x, stable_norm$sam.y)
sam_stable

cor_stable = cor.test(stable_norm$COR.x, stable_norm$COR.y)
cor_stable

rx_stable = cor.test(stable_norm$Rx.x, stable_norm$Rx.y)
rx_stable

```


Table 2 results
```{r}
sassi_omega
FVA_omega
FVOD_omega
FRISK_omega
att_omega
sym_omega
oat_omega
sat_omega
def_omega
sam_omega
correct_omega
val_omega
### Stabilities
SASDR_stable
FVA_stable
FVOD_stable
FRISK_stable
att_stable
sym_stable
oat_stable
sat_stable
def_stable
sam_stable
cor_stable
val_stable
```


Tables 3 through 5
```{r}
head(development_sample)
library(caret)
library(DescTools)


development_sample$SASSDR = as.factor(development_sample$SASSDR)
development_sample$NODIAG = as.factor(development_sample$NODIAG)
SASSDR_development =  confusionMatrix(development_sample$SASSDR, development_sample$NODIAG, positive = "1")
n_correct_SASSDR_development =  sum(SASSDR_development$table[1,1], SASSDR_development$table[2,2])
n_correct_SASSDR_development

SASSDR_development_totals = data.frame(test_p = sum(SASSDR_development$table[2,]), test_n = sum(SASSDR_development$table[1,]), criteria_p = sum(SASSDR_development$table[,2]), criteria_n = sum(SASSDR_development$table[,1]))


cramer_v_SASSDR_development = CramerV(SASSDR_development$table, conf.level = .99)
cramer_v_SASSDR_development



cross_validation_sample$SASSDR = as.factor(cross_validation_sample$SASSDR)
cross_validation_sample$NODIAG = as.factor(cross_validation_sample$NODIAG)
SASSDR_cross_validation =  confusionMatrix(cross_validation_sample$SASSDR, cross_validation_sample$NODIAG, positive = "1")
n_correct_SASSDR_cross_validation =  sum(SASSDR_cross_validation$table[1,1], SASSDR_cross_validation$table[2,2])
n_correct_SASSDR_cross_validation

cross_validation_sample_totals = data.frame(test_p = sum(SASSDR_cross_validation$table[2,]), test_n = sum(SASSDR_cross_validation$table[1,]), criteria_p = sum(SASSDR_cross_validation$table[,2]), criteria_n = sum(SASSDR_cross_validation$table[,1]))
cross_validation_sample_totals

cramer_v_SASSDR_cross_validation = CramerV(SASSDR_cross_validation$table, conf.level = .99)
cramer_v_SASSDR_cross_validation

clinical_sample$SASSDR = as.factor(clinical_sample$SASSDR)
clinical_sample$NODIAG = as.factor(clinical_sample$NODIAG)
SASSDR_clinical=  confusionMatrix(clinical_sample$SASSDR, clinical_sample$NODIAG, positive = "1")

n_correct_SASSDR_clinical=  sum(SASSDR_clinical$table[1,1], SASSDR_clinical$table[2,2])
n_correct_SASSDR_clinical

clinical_sample_totals = data.frame(test_p = sum(SASSDR_clinical$table[2,]), test_n = sum(SASSDR_clinical$table[1,]), criteria_p = sum(SASSDR_clinical$table[,2]), criteria_n = sum(SASSDR_clinical$table[,1]))
clinical_sample_totals

cramer_v_SASSDR_clinical= CramerV(SASSDR_clinical$table, conf.level = .99)
cramer_v_SASSDR_clinical


```
Table 3 Discrimint analysis
```{r}
head(development_sample)
development_sample[,15:101]
dis_dat = data.frame(NODIAG =  development_sample$NODIAG, development_sample[,15:101]) 
dis_dat_complete = na.omit(dis_dat)
dim(dis_dat_complete)
fit <- lda(NODIAG ~ ., data=dis_dat_complete, CV=TRUE)
fit$class
length(dis_dat$NODIAG)
length(fit$class)
correct = table(dis_dat_complete$NODIAG, fit$class)
correct
sum(diag(prop.table(correct)))

```


Tables 3 through 5 results
```{r}
## 3
SASSDR_development
n_correct_SASSDR_development
SASSDR_development_totals
cramer_v_SASSDR_development
## 4
SASSDR_cross_validation
n_correct_SASSDR_cross_validation
cross_validation_sample_totals
cramer_v_SASSDR_cross_validation
## 5
SASSDR_clinical
n_correct_SASSDR_clinical
clinical_sample_totals
cramer_v_SASSDR_clinical

0.8266-0.9767
0.8571-0.8488
0.8366-0.9341

```
Table 6

As shown in Table 6, overall SASSI-A3 screening accuracy was XX.X% for cases where participants’ DEF scores were one standard deviation above the normative sample DEF scale mean score or lower (i.e., DEF scale scores of 7 or less).
```{r}
def_criteria =  round(mean(normative_sample$DEF)+sd(normative_sample$DEF))
def_criteria
### DEF 9
clinical_sample_def_9 = subset(clinical_sample, DEF <= 9)
dim(clinical_sample_def_9)
SASSDR_clinical_def_9=  confusionMatrix(clinical_sample_def_9$SASSDR, clinical_sample_def_9$NODIAG, positive = "1")
SASSDR_clinical_def_9

n_correct_SASSDR_clinical_def_9=  sum(SASSDR_clinical_def_9$table[1,1], SASSDR_clinical_def_9$table[2,2])
n_correct_SASSDR_clinical_def_9

SASSDR_clinical_def_9_totals = data.frame(test_p = sum(SASSDR_clinical_def_9$table[2,]), test_n = sum(SASSDR_clinical_def_9$table[1,]), criteria_p = sum(SASSDR_clinical_def_9$table[,2]), criteria_n = sum(SASSDR_clinical_def_9$table[,1]))


cramer_v_SASSDR_clinical_def_9= CramerV(SASSDR_clinical_def_9$table, conf.level = .99)
cramer_v_SASSDR_clinical_def_9


#### DEF 10
clinical_sample_def_10 = subset(clinical_sample, DEF <= 10)
dim(clinical_sample_def_10)
SASSDR_clinical_def_10=  confusionMatrix(clinical_sample_def_10$SASSDR, clinical_sample_def_10$NODIAG, positive = "1")
SASSDR_clinical_def_10

n_correct_SASSDR_clinical_def_10=  sum(SASSDR_clinical_def_10$table[1,1], SASSDR_clinical_def_10$table[2,2])
n_correct_SASSDR_clinical_def_10

SASSDR_clinical_def_10_totals = data.frame(test_p = sum(SASSDR_clinical_def_10$table[2,]), test_n = sum(SASSDR_clinical_def_10$table[,1]), criteria_p = sum(SASSDR_clinical_def_10$table[,2]), criteria_n = sum(SASSDR_clinical_def_10$table[,1]))
SASSDR_clinical_def_10_totals

cramer_v_SASSDR_clinical_def_10= CramerV(SASSDR_clinical_def_10$table, conf.level = .99)
cramer_v_SASSDR_clinical_def_10


```
Table 6 and 7 results
```{r}

### Table 6
SASSDR_clinical_def_9
n_correct_SASSDR_clinical_def_9
SASSDR_clinical_def_9_totals
cramer_v_SASSDR_clinical_def_9

### Table 7
SASSDR_clinical_def_10
n_correct_SASSDR_clinical_def_10
cramer_v_SASSDR_clinical_def_10
SASSDR_clinical_def_10_totals


```
Table 8 data prep and analysis
```{r}
clinical_sample_def_0_4 = subset(clinical_sample, DEF = 4)
dim(clinical_sample_def_0_4)
SASSDR_clinical_def_0_4=  confusionMatrix(clinical_sample_def_0_4$SASSDR, clinical_sample_def_0_4$NODIAG, positive = "1")
SASSDR_clinical_def_0_4

n_correct_SASSDR_clinical_def_0_4=  sum(SASSDR_clinical_def_0_4$table[1,1], SASSDR_clinical_def_0_4$table[2,2])
n_correct_SASSDR_clinical_def_0_4

clinical_sample_def_5 = subset(clinical_sample, DEF = 5)
dim(clinical_sample_def_5)
SASSDR_clinical_def_5=  confusionMatrix(clinical_sample_def_5$SASSDR, clinical_sample_def_5$NODIAG, positive = "1")
SASSDR_clinical_def_5

n_correct_SASSDR_clinical_def_5=  sum(SASSDR_clinical_def_5$table[1,1], SASSDR_clinical_def_5$table[2,2])
n_correct_SASSDR_clinical_def_5

clinical_sample_def_6 = subset(clinical_sample, DEF = 6)
dim(clinical_sample_def_6)
SASSDR_clinical_def_6=  confusionMatrix(clinical_sample_def_6$SASSDR, clinical_sample_def_6$NODIAG, positive = "1")
SASSDR_clinical_def_6

n_correct_SASSDR_clinical_def_6=  sum(SASSDR_clinical_def_6$table[1,1], SASSDR_clinical_def_6$table[2,2])
n_correct_SASSDR_clinical_def_6


clinical_sample_def_7 = subset(clinical_sample, DEF = 7)
dim(clinical_sample_def_7)
SASSDR_clinical_def_7=  confusionMatrix(clinical_sample_def_7$SASSDR, clinical_sample_def_7$NODIAG, positive = "1")
SASSDR_clinical_def_7

n_correct_SASSDR_clinical_def_7=  sum(SASSDR_clinical_def_7$table[1,1], SASSDR_clinical_def_7$table[2,2])
n_correct_SASSDR_clinical_def_7

clinical_sample_def_8 = subset(clinical_sample, DEF = 8)
dim(clinical_sample_def_8)
SASSDR_clinical_def_8=  confusionMatrix(clinical_sample_def_8$SASSDR, clinical_sample_def_8$NODIAG, positive = "1")
SASSDR_clinical_def_8

n_correct_SASSDR_clinical_def_8=  sum(SASSDR_clinical_def_8$table[1,1], SASSDR_clinical_def_8$table[2,2])
n_correct_SASSDR_clinical_def_8

clinical_sample_def_9 = subset(clinical_sample, DEF = 9)
dim(clinical_sample_def_9)
SASSDR_clinical_def_9=  confusionMatrix(clinical_sample_def_9$SASSDR, clinical_sample_def_9$NODIAG, positive = "1")
SASSDR_clinical_def_9

n_correct_SASSDR_clinical_def_9=  sum(SASSDR_clinical_def_9$table[1,1], SASSDR_clinical_def_9$table[2,2])
n_correct_SASSDR_clinical_def_9

clinical_sample_def_10_11 = subset(clinical_sample, DEF == 10 | DEF == 11)
dim(clinical_sample_def_10_11)

SASSDR_clinical_def_10_11=  confusionMatrix(clinical_sample_def_10_11$SASSDR, clinical_sample_def_10_11$NODIAG, positive = "1")
SASSDR_clinical_def_10_11

n_correct_SASSDR_clinical_def_10_11=  sum(SASSDR_clinical_def_10_11$table[1,1], SASSDR_clinical_def_10_11$table[2,2])
n_correct_SASSDR_clinical_def_10_11

```
Table 8 results 
```{r}
SASSDR_clinical_def_0_4
n_correct_SASSDR_clinical_def_0_4
SASSDR_clinical_def_5
n_correct_SASSDR_clinical_def_5
SASSDR_clinical_def_6
n_correct_SASSDR_clinical_def_6
SASSDR_clinical_def_7
n_correct_SASSDR_clinical_def_7
SASSDR_clinical_def_8
n_correct_SASSDR_clinical_def_8
SASSDR_clinical_def_9
n_correct_SASSDR_clinical_def_9
SASSDR_clinical_def_10_11
n_correct_SASSDR_clinical_def_10_11
```
Table 9 data analysis
```{r}
table_9_fva = ifelse(clinical_sample$FVA >= 7, 1, ifelse(clinical_sample$FVOD >= 12, 1,ifelse(clinical_sample$sym >= 6, 1,0)))
table_9_fva = as.factor(table_9_fva)
clinical_sample$NODIAG = as.factor(clinical_sample$NODIAG)

table_9_fva_results =  confusionMatrix(table_9_fva, clinical_sample$NODIAG, positive = "1")
table_9_fva_results

table_9_fva_n_correct=  sum(table_9_fva_results$table[1,1], table_9_fva_results$table[2,2])
table_9_fva_n_correct

table_9_fva_totals = data.frame(test_p = sum(table_9_fva_results$table[2,]), test_n = sum(table_9_fva_results$table[1,]), criteria_p = sum(table_9_fva_results$table[,2]), criteria_n = sum(table_9_fva_results$table[,1]))
table_9_fva_totals

table_9_fva_cramer_v= CramerV(table_9_fva_results$table, conf.level = .99)
table_9_fva_cramer_v
```
Table 9 Results
```{r}
table_9_fva_results
table_9_fva_n_correct
table_9_fva_totals
table_9_fva_cramer_v
```
Table 10 data analysis
Rule 1: FVA 12 or FVOD 12 or more

Rule 2: FRISK 5 or more

Rule 3: SYM 5 or more

Face Valid Classification: Rules 1 or 2 or 3

Rule 4: SAT 9 or more

Rule 5: OAT 4 and DEF 10 or more

Rule 6: OAT 7 or more and SAT 6 or more and DEF 2 or more and SAM 4 or more

Rule 7: FVA or FVOD 7 or more and FRISK or ATT or SAM 3 or more and OAT 5 or more

Rule 7: FVA or FVOD 5 or more and OAT 4 or more and DEF 7 or more

Rule 8: FVA or FVOD 5 or more and SAT 3 or more and DEF 4 or more and SAM 3 or more

 
```{r}
rule1 = as.factor(ifelse(clinical_sample$FVA >= 7, 1,ifelse(clinical_sample$FVOD >= 12,1,0)))
clinical_sample$NODIAG = as.factor(clinical_sample$NODIAG)
rule1_results =  confusionMatrix(rule1, clinical_sample$NODIAG, positive = "1")
rule1_results

rule1_test_p = sum(rule1_results$table[2,])
rule1_diag_p = sum(rule1_results$table[,2])
rule1_accurate = rule1_test_p /rule1_diag_p 
rule1_accurate

rule1_totals = data.frame(test_p = sum(rule1_results$table[2,]), test_n = sum(rule1_results$table[1,]), criteria_p = sum(rule1_results$table[,2]), criteria_n = sum(rule1_results$table[,1]))
rule1_totals


rule2 = as.factor(ifelse(clinical_sample$frisk >=5,1,0))

rule2_results =  confusionMatrix(rule2, clinical_sample$NODIAG, positive = "1")
rule2_results

rule2_test_p = sum(rule2_results$table[2,])
rule2_diag_p = sum(rule2_results$table[,2])
rule2_accurate = rule2_test_p /rule2_diag_p 
rule2_accurate

rule2_totals = data.frame(test_p = sum(rule2_results$table[2,]), test_n = sum(rule2_results$table[1,]), criteria_p = sum(rule2_results$table[,2]), criteria_n = sum(rule2_results$table[,1]))
rule2_totals


rule3 = as.factor(ifelse(clinical_sample$sym >=6,1,0))

rule3_results =  confusionMatrix(rule3, clinical_sample$NODIAG, positive = "1")
rule3_results

rule3_test_p = sum(rule3_results$table[2,])
rule3_diag_p = sum(rule3_results$table[,2])
rule3_accurate = rule3_test_p /rule3_diag_p 
rule3_accurate

rule3_totals = data.frame(test_p = sum(rule3_results$table[2,]), test_n = sum(rule3_results$table[1,]), criteria_p = sum(rule3_results$table[,2]), criteria_n = sum(rule3_results$table[,1]))
rule3_totals


rule4 = as.factor(ifelse(clinical_sample$sat >=11,1,0))

rule4_results =  confusionMatrix(rule4, clinical_sample$NODIAG, positive = "1")
rule4_results

rule4_test_p = sum(rule4_results$table[2,])
rule4_diag_p = sum(rule4_results$table[,2])
rule4_accurate = rule4_test_p /rule4_diag_p 
rule4_accurate

rule4_totals = data.frame(test_p = sum(rule4_results$table[2,]), test_n = sum(rule4_results$table[1,]), criteria_p = sum(rule4_results$table[,2]), criteria_n = sum(rule4_results$table[,1]))
rule4_totals



rule5 = as.factor(ifelse(clinical_sample$oat >= 8 &  clinical_sample$sat >=6 & clinical_sample$DEF >= 2 & clinical_sample$sam >= 4, 1,0))
rule5_results =  confusionMatrix(rule5, clinical_sample$NODIAG, positive = "1")
rule5_results

rule5_test_p = sum(rule5_results$table[2,])
rule5_diag_p = sum(rule5_results$table[,2])
rule5_accurate = rule5_test_p /rule5_diag_p 
rule5_accurate

rule5_totals = data.frame(test_p = sum(rule5_results$table[2,]), test_n = sum(rule5_results$table[1,]), criteria_p = sum(rule5_results$table[,2]), criteria_n = sum(rule5_results$table[,1]))
rule5_totals



rule6 = as.factor(ifelse(clinical_sample$FVOD >= 7 & clinical_sample$frisk >= 3 | clinical_sample$att >= 3 | clinical_sample$sam >= 3 & clinical_sample$oat >= 5,1,0))
rule6
rule6_results =  confusionMatrix(rule6, clinical_sample$NODIAG, positive = "1")
rule6_results

rule6_test_p = sum(rule6_results$table[2,])
rule6_diag_p = sum(rule6_results$table[,2])
rule6_accurate = rule6_test_p /rule6_diag_p 
rule6_accurate

rule6_totals = data.frame(test_p = sum(rule6_results$table[2,]), test_n = sum(rule6_results$table[1,]), criteria_p = sum(rule6_results$table[,2]), criteria_n = sum(rule6_results$table[,1]))
rule6_totals



rule7 = as.factor(ifelse(clinical_sample$FVA >= 5 | clinical_sample$FVOD >= 5 & clinical_sample$oat >= 4 & clinical_sample$DEF >= 7,1,0))
rule7
rule7_results =  confusionMatrix(rule7, clinical_sample$NODIAG, positive = "1")
rule7_results

rule7_test_p = sum(rule7_results$table[2,])
rule7_diag_p = sum(rule7_results$table[,2])
rule7_accurate = rule7_test_p /rule7_diag_p 
rule7_accurate

rule7_totals = data.frame(test_p = sum(rule7_results$table[2,]), test_n = sum(rule7_results$table[1,]), criteria_p = sum(rule7_results$table[,2]), criteria_n = sum(rule7_results$table[,1]))
rule7_totals



rule8 = as.factor(ifelse(clinical_sample$FVA >= 5 | clinical_sample$FVOD >= 5 & clinical_sample$oat >= 4 & clinical_sample$DEF >= 4 & clinical_sample$sam >=3,1,0))
rule8
rule8_results =  confusionMatrix(rule8, clinical_sample$NODIAG, positive = "1")
rule8_results


rule8_test_p = sum(rule8_results$table[2,])
rule8_diag_p = sum(rule8_results$table[,2])
rule8_accurate = rule8_test_p /rule8_diag_p 
rule8_accurate

rule8_totals = data.frame(test_p = sum(rule8_results$table[2,]), test_n = sum(rule8_results$table[1,]), criteria_p = sum(rule8_results$table[,2]), criteria_n = sum(rule8_results$table[,1]))
rule8_totals



```
Table 10 results
```{r}
rule1_test_p
rule1_diag_p
rule1_accurate

rule2_test_p
rule2_diag_p
rule2_accurate

rule3_test_p
rule3_diag_p
rule3_accurate

rule4_test_p
rule4_diag_p
rule4_accurate

rule5_test_p
rule5_diag_p
rule5_accurate

rule6_test_p
rule6_diag_p
rule6_accurate

rule7_test_p
rule7_diag_p
rule7_accurate

rule8_test_p
rule8_diag_p
rule8_accurate


```
Try second rules
Proposed alt Set Decision Rule
Rule 1: FVA 7 or FVOD 12 or more
Rule 2: FRISK 6 or more
Rule 3: SYM 5 or more
Face Valid Classification: Rules 1 or 2 or 3
Rule 4: SAT 7 or more
Rule 5: OAT 7 or more and SAT 6 or more and DEF 2 or more and SAM 4 or more
Rule 6: FVOD 7 or more and FRISK or ATT or SAM 3 or more and OAT 5 or more
Rule 7: FVA or FVOD 5 or more and OAT 4 or more and DEF 7 or more
Rule 8: FVA or FVOD 5 or more and SAT 3 or more and DEF 4 or more and SAM 3 or more
```{r}
rule1 = as.factor(ifelse(clinical_sample$FVA >= 7, 1,ifelse(clinical_sample$FVOD >= 12,1,0)))
clinical_sample$NODIAG = as.factor(clinical_sample$NODIAG)
rule1_results =  confusionMatrix(rule1, clinical_sample$NODIAG, positive = "1")
rule1_results

rule1_test_p = sum(rule1_results$table[2,])
rule1_diag_p = sum(rule1_results$table[,2])
rule1_accurate = rule1_test_p /rule1_diag_p 
rule1_accurate

rule1_totals = data.frame(test_p = sum(rule1_results$table[2,]), test_n = sum(rule1_results$table[1,]), criteria_p = sum(rule1_results$table[,2]), criteria_n = sum(rule1_results$table[,1]))
rule1_totals


rule2 = as.factor(ifelse(clinical_sample$frisk >=6,1,0))

rule2_results =  confusionMatrix(rule2, clinical_sample$NODIAG, positive = "1")
rule2_results

rule2_test_p = sum(rule2_results$table[2,])
rule2_diag_p = sum(rule2_results$table[,2])
rule2_accurate = rule2_test_p /rule2_diag_p 
rule2_accurate

rule2_totals = data.frame(test_p = sum(rule2_results$table[2,]), test_n = sum(rule2_results$table[1,]), criteria_p = sum(rule2_results$table[,2]), criteria_n = sum(rule2_results$table[,1]))
rule2_totals


rule3 = as.factor(ifelse(clinical_sample$sym >=5,1,0))

rule3_results =  confusionMatrix(rule3, clinical_sample$NODIAG, positive = "1")
rule3_results

rule3_test_p = sum(rule3_results$table[2,])
rule3_diag_p = sum(rule3_results$table[,2])
rule3_accurate = rule3_test_p /rule3_diag_p 
rule3_accurate

rule3_totals = data.frame(test_p = sum(rule3_results$table[2,]), test_n = sum(rule3_results$table[1,]), criteria_p = sum(rule3_results$table[,2]), criteria_n = sum(rule3_results$table[,1]))
rule3_totals


rule4 = as.factor(ifelse(clinical_sample$sat >=7,1,0))

rule4_results =  confusionMatrix(rule4, clinical_sample$NODIAG, positive = "1")
rule4_results

rule4_test_p = sum(rule4_results$table[2,])
rule4_diag_p = sum(rule4_results$table[,2])
rule4_accurate = rule4_test_p /rule4_diag_p 
rule4_accurate

rule4_totals = data.frame(test_p = sum(rule4_results$table[2,]), test_n = sum(rule4_results$table[1,]), criteria_p = sum(rule4_results$table[,2]), criteria_n = sum(rule4_results$table[,1]))
rule4_totals


#Rule 5: OAT 7 or more and SAT 6 or more and DEF 2 or more and SAM 4 or more
rule5 = as.factor(ifelse(clinical_sample$oat >= 7 &  clinical_sample$sat >=6 & clinical_sample$DEF >= 2 & clinical_sample$sam >= 4, 1,0))
rule5_results =  confusionMatrix(rule5, clinical_sample$NODIAG, positive = "1")
rule5_results

rule5_test_p = sum(rule5_results$table[2,])
rule5_diag_p = sum(rule5_results$table[,2])
rule5_accurate = rule5_test_p /rule5_diag_p 
rule5_accurate

rule5_totals = data.frame(test_p = sum(rule5_results$table[2,]), test_n = sum(rule5_results$table[1,]), criteria_p = sum(rule5_results$table[,2]), criteria_n = sum(rule5_results$table[,1]))
rule5_totals

#Rule 6: FVOD 7 or more and FRISK or ATT or SAM 3 or more and OAT 5 or more

rule6 = as.factor(ifelse(clinical_sample$FVOD >= 7 & clinical_sample$frisk >= 3 | clinical_sample$att >= 3 | clinical_sample$sam >= 3 & clinical_sample$oat >= 5,1,0))
rule6
rule6_results =  confusionMatrix(rule6, clinical_sample$NODIAG, positive = "1")
rule6_results

rule6_test_p = sum(rule6_results$table[2,])
rule6_diag_p = sum(rule6_results$table[,2])
rule6_accurate = rule6_test_p /rule6_diag_p 
rule6_accurate

rule6_totals = data.frame(test_p = sum(rule6_results$table[2,]), test_n = sum(rule6_results$table[1,]), criteria_p = sum(rule6_results$table[,2]), criteria_n = sum(rule6_results$table[,1]))
rule6_totals

#Rule 7: FVA or FVOD 5 or more and OAT 4 or more and DEF 7 or more


rule7 = as.factor(ifelse(clinical_sample$FVA >= 5 | clinical_sample$FVOD >= 5 & clinical_sample$oat >= 4 & clinical_sample$DEF >= 7,1,0))
rule7
rule7_results =  confusionMatrix(rule7, clinical_sample$NODIAG, positive = "1")
rule7_results

rule7_test_p = sum(rule7_results$table[2,])
rule7_diag_p = sum(rule7_results$table[,2])
rule7_accurate = rule7_test_p /rule7_diag_p 
rule7_accurate

rule7_totals = data.frame(test_p = sum(rule7_results$table[2,]), test_n = sum(rule7_results$table[1,]), criteria_p = sum(rule7_results$table[,2]), criteria_n = sum(rule7_results$table[,1]))
rule7_totals

#Rule 8: FVA or FVOD 5 or more and SAT 3 or more and DEF 4 or more and SAM 3 or more


rule8 = as.factor(ifelse(clinical_sample$FVA >= 5 | clinical_sample$FVOD >= 5 & clinical_sample$sat >= 3 & clinical_sample$DEF >= 4 & clinical_sample$sam >=3,1,0))
rule8_results =  confusionMatrix(rule8, clinical_sample$NODIAG, positive = "1")
rule8_results


rule8_test_p = sum(rule8_results$table[2,])
rule8_diag_p = sum(rule8_results$table[,2])
rule8_accurate = rule8_test_p /rule8_diag_p 
rule8_accurate

rule8_totals = data.frame(test_p = sum(rule8_results$table[2,]), test_n = sum(rule8_results$table[1,]), criteria_p = sum(rule8_results$table[,2]), criteria_n = sum(rule8_results$table[,1]))
rule8_totals
```
Table 10 other rules results make another table for these results
```{r}
rule1_test_p
rule1_diag_p
rule1_accurate

rule2_test_p
rule2_diag_p
rule2_accurate

rule3_test_p
rule3_diag_p
rule3_accurate

rule4_test_p
rule4_diag_p
rule4_accurate

rule5_test_p
rule5_diag_p
rule5_accurate

rule6_test_p
rule6_diag_p
rule6_accurate

rule7_test_p
rule7_diag_p
rule7_accurate

rule8_test_p
rule8_diag_p
rule8_accurate


```


Table 11
```{r}
accurate_var = ifelse(clinical_sample$SASSDR == clinical_sample$NODIAG,1,0)
accurate_var = as.factor(accurate_var)
cramer_v_table11 = CramerV(clinical_sample$CLIENTSETTING, accurate_var,  conf.level = .99)
cramer_v_table11

describe.factor(clinical_sample$CLIENTSETTING)
describe.factor(development_sample$CLIENTSETTING)
describe.factor(cross_validation_sample$CLIENTSETTING)

CLIENTSETTING_sample_table_11 = clinical_sample
CLIENTSETTING_sample_table_11$CLIENTSETTING = ifelse(CLIENTSETTING_sample_table_11$CLIENTSETTING ==5,7,CLIENTSETTING_sample_table_11$CLIENTSETTING)
describe.factor(CLIENTSETTING_sample_table_11$CLIENTSETTING)

percent_table11 = tapply(accurate_var,CLIENTSETTING_sample_table_11$CLIENTSETTING,function(x){prop.table(table(x))})
percent_table11

n_table11 =  tapply(accurate_var,CLIENTSETTING_sample_table_11$CLIENTSETTING,function(x){table(x)})
n_table11 =  unlist(n_table11)
n_table11 = matrix(n_table11, ncol = 2, byrow = TRUE)
inaccurate_n_table11 = sum(n_table11[,1])
accurate_n_table11 =  sum(n_table11[,2])
```
Table 11 results
```{r}
percent_table11
inaccurate_n_table11
accurate_n_table11
describe.factor(CLIENTSETTING_sample_table_11$CLIENTSETTING)

```
Table 12 through 17 data cleaning
Criminal justice program = 1
Social services = 2
Medical pan clinic = 3
Medical facility other = 4
6 = Substance use treatment
7 = other
```{r}

#### Criminal justice programs
clinical_sample_crime = subset(clinical_sample, CLIENTSETTING == 1)
clinical_sample_crime_total_n = dim(clinical_sample_crime)[1]
SASSDR_clinical_crime=  confusionMatrix(as.factor(clinical_sample_crime$SASSDR), as.factor(clinical_sample_crime$NODIAG), positive = "1")
SASSDR_clinical_crime

n_correct_SASSDR_clinical_crime=  sum(SASSDR_clinical_crime$table[1,1], SASSDR_clinical_crime$table[2,2])
n_correct_SASSDR_clinical_crime

SASSDR_clinical_crime_totals = data.frame(test_p = sum(SASSDR_clinical_crime$table[2,]), test_n = sum(SASSDR_clinical_crime$table[1,]), criteria_p = sum(SASSDR_clinical_crime$table[,2]), criteria_n = sum(SASSDR_clinical_crime$table[,1]))
SASSDR_clinical_crime_totals



#### Social services programs
clinical_sample_social_services = subset(clinical_sample, CLIENTSETTING == 2)

clinical_sample_social_services_total_n = dim(clinical_sample_social_services)[1]

SASSDR_clinical_social_services=  confusionMatrix(as.factor(clinical_sample_social_services$SASSDR), as.factor(clinical_sample_social_services$NODIAG), positive = "1")
SASSDR_clinical_social_services

n_correct_SASSDR_clinical_social_services=  sum(SASSDR_clinical_social_services$table[1,1], SASSDR_clinical_social_services$table[2,2])
n_correct_SASSDR_clinical_social_services

SASSDR_clinical_social_services_totals = data.frame(test_p = sum(SASSDR_clinical_social_services$table[2,]), test_n = sum(SASSDR_clinical_social_services$table[1,]), criteria_p = sum(SASSDR_clinical_social_services$table[,2]), criteria_n = sum(SASSDR_clinical_social_services$table[,1]))
SASSDR_clinical_social_services_totals


###
#### medical pain clinics
clinical_sample_pain_clinic = subset(clinical_sample, CLIENTSETTING == 3)
dim(clinical_sample_pain_clinic)[1]
### None

## other pain clinic
clinical_sample_other_pain_clinic = subset(clinical_sample, CLIENTSETTING == 4)
dim(clinical_sample_other_pain_clinic)


## substance use facilities
#### substance use clinics
clinical_sample_substance_use = subset(clinical_sample, CLIENTSETTING == 6)
clinical_sample_substance_use_total_n =  dim(clinical_sample_substance_use)[1]
SASSDR_clinical_substance_use=  confusionMatrix(as.factor(clinical_sample_substance_use$SASSDR), as.factor(clinical_sample_substance_use$NODIAG), positive = "1")


n_correct_SASSDR_clinical_substance_use=  sum(SASSDR_clinical_substance_use$table[1,1], SASSDR_clinical_substance_use$table[2,2])
n_correct_SASSDR_clinical_substance_use

SASSDR_clinical_substance_use_totals = data.frame(test_p = sum(SASSDR_clinical_substance_use$table[2,]), test_n = sum(SASSDR_clinical_substance_use$table[1,]), criteria_p = sum(SASSDR_clinical_substance_use$table[,2]), criteria_n = sum(SASSDR_clinical_substance_use$table[,1]))
SASSDR_clinical_substance_use_totals

#### other
clinical_sample_other = subset(clinical_sample, CLIENTSETTING == 7 | CLIENTSETTING == 5)
clinical_sample_other_total_n =  dim(clinical_sample_other)[1]
SASSDR_clinical_other=  confusionMatrix(as.factor(clinical_sample_other$SASSDR), as.factor(clinical_sample_other$NODIAG), positive = "1")
SASSDR_clinical_other

n_correct_SASSDR_clinical_other=  sum(SASSDR_clinical_other$table[1,1], SASSDR_clinical_other$table[2,2])
n_correct_SASSDR_clinical_other

SASSDR_clinical_other_totals = data.frame(test_p = sum(SASSDR_clinical_other$table[2,]), test_n = sum(SASSDR_clinical_other$table[1,]), criteria_p = sum(SASSDR_clinical_other$table[,2]), criteria_n = sum(SASSDR_clinical_other$table[,1]))
SASSDR_clinical_other_totals


```
Tables 12 through 17 results
```{r}
SASSDR_clinical_crime
clinical_sample_crime_total_n
n_correct_SASSDR_clinical_crime
SASSDR_clinical_crime_totals

SASSDR_clinical_social_services
clinical_sample_social_services_total_n
n_correct_SASSDR_clinical_social_services
SASSDR_clinical_social_services_totals

SASSDR_clinical_substance_use
clinical_sample_substance_use_total_n
n_correct_SASSDR_clinical_substance_use
SASSDR_clinical_substance_use_totals

SASSDR_clinical_other
clinical_sample_other_total_n
n_correct_SASSDR_clinical_other
SASSDR_clinical_other_totals
```
Table 18 data cleaning
```{r}
clinical_sample$

```

Table 18 results

Table 19 data cleaning
28 = 5th
29 = 6th
30 = 7th
31 = 8th
32 = 9th
33 = 10th
34 = 11th
35 = 12th
36 = Other
```{r}

five_six_dat = subset(clinical_sample, YEARS_ED == 28 | YEARS_ED == 29)
describe.factor(five_six_dat$YEARS_ED)

five_six_dat_total_n =  dim(five_six_dat)[1]

five_six_dat_results=  confusionMatrix(as.factor(five_six_dat$SASSDR), as.factor(five_six_dat$NODIAG), positive = "1")

five_six_dat_accurate =  sum(five_six_dat_results$table[1,1], five_six_dat_results$table[2,2])
five_six_dat_inaccurate =  sum(five_six_dat_results$table[1,2], five_six_dat_results$table[2,1])

seven_eight_nine_dat = subset(clinical_sample, YEARS_ED == 30 | YEARS_ED == 31 | YEARS_ED == 32)
describe.factor(seven_eight_nine_dat$YEARS_ED)

seven_eight_nine_dat_total_n =  dim(seven_eight_nine_dat)[1]

seven_eight_nine_dat_results=  confusionMatrix(as.factor(seven_eight_nine_dat$SASSDR), as.factor(seven_eight_nine_dat$NODIAG), positive = "1")

seven_eight_nine_dat_accurate =  sum(seven_eight_nine_dat_results$table[1,1], seven_eight_nine_dat_results$table[2,2])
seven_eight_nine_dat_inaccurate =  sum(seven_eight_nine_dat_results$table[1,2], seven_eight_nine_dat_results$table[2,1])

ten_eleven_twelve_dat = subset(clinical_sample, YEARS_ED == 33 | YEARS_ED == 34 | YEARS_ED == 35)
describe.factor(ten_eleven_twelve_dat$YEARS_ED)

ten_eleven_twelve_dat_total_n =  dim(ten_eleven_twelve_dat)[1]

ten_eleven_twelve_dat_results=  confusionMatrix(as.factor(ten_eleven_twelve_dat$SASSDR), as.factor(ten_eleven_twelve_dat$NODIAG), positive = "1")

ten_eleven_twelve_dat_accurate =  sum(ten_eleven_twelve_dat_results$table[1,1], ten_eleven_twelve_dat_results$table[2,2])
ten_eleven_twelve_dat_inaccurate =  sum(ten_eleven_twelve_dat_results$table[1,2], ten_eleven_twelve_dat_results$table[2,1])

total_n_five_twelve = sum(five_six_dat_total_n, seven_eight_nine_dat_total_n, ten_eleven_twelve_dat_total_n)

edu_dat_cramer = subset(clinical_sample, YEARS_ED < 36)
edu_dat_cramer$accurate = ifelse(edu_dat_cramer$SASSDR == edu_dat_cramer$NODIAG,1,0)

edu_cramersV = CramerV(edu_dat_cramer$YEARS_ED, edu_dat_cramer$accurate, conf.level = .99)
edu_cramersV
```
Table 19 results
```{r}
five_six_dat_total_n
five_six_dat_results
five_six_dat_accurate
five_six_dat_inaccurate

seven_eight_nine_dat_total_n
seven_eight_nine_dat_results
seven_eight_nine_dat_accurate
seven_eight_nine_dat_inaccurate

ten_eleven_twelve_dat_total_n
ten_eleven_twelve_dat_results
ten_eleven_twelve_dat_accurate
ten_eleven_twelve_dat_inaccurate

edu_cramersV
```
Table 20 - 23

22 = Full time
23 = part time
24 = not employed
25= volunteer

Just doing part time and not employed
```{r}
describe.factor(clinical_sample$EMP_STATUS)

not_employed_dat = subset(clinical_sample,  EMP_STATUS == 24)
describe.factor(not_employed_dat$YEARS_ED)

not_employed_dat_total_n =  dim(not_employed_dat)[1]

not_employed_dat_results=  confusionMatrix(as.factor(not_employed_dat$SASSDR), as.factor(not_employed_dat$NODIAG), positive = "1")

not_employed_dat_accurate =  sum(not_employed_dat_results$table[1,1], not_employed_dat_results$table[2,2])
not_employed_dat_inaccurate =  sum(not_employed_dat_results$table[1,2], not_employed_dat_results$table[2,1])


part_employed_dat = subset(clinical_sample,  EMP_STATUS == 23)
describe.factor(part_employed_dat$YEARS_ED)

part_employed_dat_total_n =  dim(part_employed_dat)[1]

part_employed_dat_results=  confusionMatrix(as.factor(part_employed_dat$SASSDR), as.factor(part_employed_dat$NODIAG), positive = "1")

part_employed_dat_accurate =  sum(part_employed_dat_results$table[1,1], part_employed_dat_results$table[2,2])
part_employed_dat_inaccurate =  sum(part_employed_dat_results$table[1,2], part_employed_dat_results$table[2,1])

employed_dat_cramer = subset(clinical_sample, EMP_STATUS == 23 | EMP_STATUS == 24)
employed_dat_cramer$accurate = ifelse(employed_dat_cramer$SASSDR == employed_dat_cramer$NODIAG,1,0)

employed_cramersV = CramerV(employed_dat_cramer$YEARS_ED, employed_dat_cramer$accurate, conf.level = .99)
employed_cramersV

employed_n = sum(not_employed_dat_total_n, part_employed_dat_total_n)

```
Tables 20 - 23 results
```{r}
not_employed_dat_total_n
not_employed_dat_accurate
not_employed_dat_inaccurate

part_employed_dat_total_n
part_employed_dat_accurate
part_employed_dat_inaccurate
employed_cramersV

employed_n

```
Table 24 - 26

Male = 1
Female = 2
```{r}
male_dat = subset(clinical_sample,  SEX == 1)

male_dat_total_n =  dim(male_dat)[1]

male_dat_results=  confusionMatrix(as.factor(male_dat$SASSDR), as.factor(male_dat$NODIAG), positive = "1")

male_dat_accurate =  sum(male_dat_results$table[1,1], male_dat_results$table[2,2])
male_dat_inaccurate =  sum(male_dat_results$table[1,2], male_dat_results$table[2,1])


female_dat = subset(clinical_sample,  SEX == 2)

female_dat_total_n =  dim(female_dat)[1]

female_dat_results=  confusionMatrix(as.factor(female_dat$SASSDR), as.factor(female_dat$NODIAG), positive = "1")

female_dat_accurate =  sum(female_dat_results$table[1,1], female_dat_results$table[2,2])
female_dat_inaccurate =  sum(female_dat_results$table[1,2], female_dat_results$table[2,1])

male_dat = subset(clinical_sample, SEX == 1)

male_dat_total_n =  dim(male_dat)[1]

male_dat_results=  confusionMatrix(as.factor(male_dat$SASSDR), as.factor(male_dat$NODIAG), positive = "1")

male_totals = data.frame(test_p = sum(male_dat_results$table[2,]), test_n = sum(male_dat_results$table[,2]), criteria_p = sum(male_dat_results$table[,2]), criteria_n = sum(male_dat_results$table[2,]))
male_totals

female_dat = subset(clinical_sample, SEX == 2)

female_dat_total_n =  dim(female_dat)[1]

female_dat_results=  confusionMatrix(as.factor(female_dat$SASSDR), as.factor(female_dat$NODIAG), positive = "1")

female_totals = data.frame(test_p = sum(female_dat_results$table[2,]), test_n = sum(female_dat_results$table[1,]), criteria_p = sum(female_dat_results$table[,2]), criteria_n = sum(female_dat_results$table[,1]))
female_totals


total_gender_n = sum(male_dat_total_n, female_dat_total_n)
total_gender_accurate = sum(male_dat_accurate, female_dat_accurate)
total_gender_inaccurate = sum(male_dat_inaccurate, female_dat_inaccurate)

```
Tables 24 through 26
```{r}

### Table 24
male_dat_total_n
male_dat_accurate
male_dat_inaccurate

female_dat_total_n
female_dat_accurate
female_dat_inaccurate

total_gender_n
total_gender_accurate
total_gender_inaccurate

## Table 25
male_dat_results
male_totals


### Table 26
female_dat_results
female_totals





```
Table 27 
Ages 13,14,15,16,17,18
```{r}
thirteen_dat = subset(clinical_sample,  AGE == 13)

thirteen_dat_total_n =  dim(thirteen_dat)[1]

thirteen_dat_results=  confusionMatrix(as.factor(thirteen_dat$SASSDR), as.factor(thirteen_dat$NODIAG), positive = "1")

thirteen_dat_accurate =  sum(thirteen_dat_results$table[1,1], thirteen_dat_results$table[2,2])
thirteen_dat_inaccurate =  sum(thirteen_dat_results$table[1,2], thirteen_dat_results$table[2,1])


fourteen_dat = subset(clinical_sample,  AGE == 14)

fourteen_dat_total_n =  dim(fourteen_dat)[1]

fourteen_dat_results=  confusionMatrix(as.factor(fourteen_dat$SASSDR), as.factor(fourteen_dat$NODIAG), positive = "1")

fourteen_dat_accurate =  sum(fourteen_dat_results$table[1,1], fourteen_dat_results$table[2,2])
fourteen_dat_inaccurate =  sum(fourteen_dat_results$table[1,2], fourteen_dat_results$table[2,1])


fifteen_dat = subset(clinical_sample,  AGE == 15)

fifteen_dat_total_n =  dim(fifteen_dat)[1]

fifteen_dat_results=  confusionMatrix(as.factor(fifteen_dat$SASSDR), as.factor(fifteen_dat$NODIAG), positive = "1")

fifteen_dat_accurate =  sum(fifteen_dat_results$table[1,1], fifteen_dat_results$table[2,2])
fifteen_dat_inaccurate =  sum(fifteen_dat_results$table[1,2], fifteen_dat_results$table[2,1])

sixteen_dat = subset(clinical_sample,  AGE == 16)

sixteen_dat_total_n =  dim(sixteen_dat)[1]

sixteen_dat_results=  confusionMatrix(as.factor(sixteen_dat$SASSDR), as.factor(sixteen_dat$NODIAG), positive = "1")

sixteen_dat_accurate =  sum(sixteen_dat_results$table[1,1], sixteen_dat_results$table[2,2])
sixteen_dat_inaccurate =  sum(sixteen_dat_results$table[1,2], sixteen_dat_results$table[2,1])

seventeen_dat = subset(clinical_sample,  AGE == 17)

seventeen_dat_total_n =  dim(seventeen_dat)[1]

seventeen_dat_results=  confusionMatrix(as.factor(seventeen_dat$SASSDR), as.factor(seventeen_dat$NODIAG), positive = "1")

seventeen_dat_accurate =  sum(seventeen_dat_results$table[1,1], seventeen_dat_results$table[2,2])
seventeen_dat_inaccurate =  sum(seventeen_dat_results$table[1,2], seventeen_dat_results$table[2,1])

eighteen_dat = subset(clinical_sample,  AGE == 18)

eighteen_dat_total_n =  dim(eighteen_dat)[1]

eighteen_dat_results=  confusionMatrix(as.factor(eighteen_dat$SASSDR), as.factor(eighteen_dat$NODIAG), positive = "1")

eighteen_dat_accurate =  sum(eighteen_dat_results$table[1,1], eighteen_dat_results$table[2,2])
eighteen_dat_inaccurate =  sum(eighteen_dat_results$table[1,2], eighteen_dat_results$table[2,1])


total_age_accurate = sum(thirteen_dat_accurate, fourteen_dat_accurate, fifteen_dat_accurate, sixteen_dat_accurate, seventeen_dat_accurate, eighteen_dat_accurate)

total_age_inaccurate = sum(thirteen_dat_inaccurate, fourteen_dat_inaccurate, fifteen_dat_inaccurate, sixteen_dat_inaccurate, seventeen_dat_inaccurate, eighteen_dat_inaccurate)

age_dat_cramer = clinical_sample
age_dat_cramer$accurate = ifelse(clinical_sample$SASSDR == age_dat_cramer$NODIAG,1,0)

age_cramersV = CramerV(age_dat_cramer$AGE, age_dat_cramer$accurate, conf.level = .99)
age_cramersV

```
Table 27 results
```{r}
thirteen_dat_total_n
thirteen_dat_results
thirteen_dat_accurate
thirteen_dat_inaccurate

fourteen_dat_total_n
fourteen_dat_results
fourteen_dat_accurate
fourteen_dat_inaccurate

fifteen_dat_total_n
fifteen_dat_results
fifteen_dat_accurate
fifteen_dat_inaccurate

sixteen_dat_total_n
sixteen_dat_results
sixteen_dat_accurate
sixteen_dat_inaccurate

eighteen_dat_total_n
eighteen_dat_results
eighteen_dat_accurate
eighteen_dat_inaccurate

age_cramersV


```
Table 28

Only include the following ethnicities rest are below 10: 
White = 7         
Hispanic = 5  (per email)      
Black = 3         
Mixed race =  8
```{r}
describe.factor(clinical_sample$ETHN)

white_dat = subset(clinical_sample,  ETHN == 7)

white_dat_total_n =  dim(white_dat)[1]

white_dat_results=  confusionMatrix(as.factor(white_dat$SASSDR), as.factor(white_dat$NODIAG), positive = "1")

white_dat_accurate =  sum(white_dat_results$table[1,1], white_dat_results$table[2,2])

white_totals = data.frame(test_p = sum(white_dat_results$table[2,]), test_n = sum(white_dat_results$table[1,]), criteria_p = sum(white_dat_results$table[,2]), criteria_n = sum(white_dat_results$table[,1]))
white_totals


hispanic_dat = subset(clinical_sample,  ETHN == 5)

hispanic_dat_total_n =  dim(hispanic_dat)[1]

hispanic_dat_results=  confusionMatrix(as.factor(hispanic_dat$SASSDR), as.factor(hispanic_dat$NODIAG), positive = "1")

hispanic_dat_accurate =  sum(hispanic_dat_results$table[1,1], hispanic_dat_results$table[2,2])


hispanic_totals = data.frame(test_p = sum(hispanic_dat_results$table[2,]), test_n = sum(hispanic_dat_results$table[1,]), criteria_p = sum(hispanic_dat_results$table[,2]), criteria_n = sum(hispanic_dat_results$table[,1]))
hispanic_totals


black_dat = subset(clinical_sample,  ETHN == 3)

black_dat_total_n =  dim(black_dat)[1]

black_dat_results=  confusionMatrix(as.factor(black_dat$SASSDR), as.factor(black_dat$NODIAG), positive = "1")

black_dat_accurate =  sum(black_dat_results$table[1,1], black_dat_results$table[2,2])

black_totals = data.frame(test_p = sum(black_dat_results$table[2,]), test_n = sum(black_dat_results$table[1,]), criteria_p = sum(black_dat_results$table[,2]), criteria_n = sum(black_dat_results$table[,1]))
black_totals

mixed_dat = subset(clinical_sample,  ETHN == 8)

mixed_dat_total_n =  dim(mixed_dat)[1]

mixed_dat_results =  confusionMatrix(as.factor(mixed_dat$SASSDR), as.factor(mixed_dat$NODIAG), positive = "1")

mixed_dat_accurate =  sum(mixed_dat_results$table[1,1], mixed_dat_results$table[2,2])

mixed_totals = data.frame(test_p = sum(mixed_dat_results$table[2,]), test_n = sum(mixed_dat_results$table[1,]), criteria_p = sum(mixed_dat_results$table[,2]), criteria_n = sum(mixed_dat_results$table[,1]))
mixed_totals

```
Tables 29 - 35 results
```{r}
white_dat_total_n
white_dat_results
white_dat_accurate
white_totals

hispanic_dat_total_n
hispanic_dat_results
hispanic_dat_accurate
hispanic_totals


black_dat_total_n
black_dat_results
black_dat_accurate
black_totals

mixed_dat_total_n
mixed_dat_results
mixed_dat_accurate
mixed_totals



```
Table 36 data cleaning
```{r}
legal_dat = subset(clinical_sample, TOT_ARREST > 0)

legal_dat_total_n =  dim(legal_dat)[1]

legal_dat_results=  confusionMatrix(as.factor(legal_dat$SASSDR), as.factor(legal_dat$NODIAG), positive = "1")

legal_dat_accurate =  sum(legal_dat_results$table[1,1], legal_dat_results$table[2,2])

legal_totals = data.frame(test_p = sum(legal_dat_results$table[2,]), test_n = sum(legal_dat_results$table[1,]), criteria_p = sum(legal_dat_results$table[,2]), criteria_n = sum(legal_dat_results$table[,1]))
legal_totals


```
Table 36 results
```{r}
legal_dat_total_n
legal_dat_results
legal_dat_accurate
legal_totals
```
Table 37 data cleaning
9 = Parents
13 = residential
10 = other relatives
```{r}
describe.factor(clinical_sample$LIVING_SIT)

living_sit_dat = subset(clinical_sample, LIVING_SIT != 9)

living_sit_dat_total_n =  dim(living_sit_dat)[1]

living_sit_dat_results=  confusionMatrix(as.factor(living_sit_dat$SASSDR), as.factor(living_sit_dat$NODIAG), positive = "1")

living_sit_dat_accurate =  sum(living_sit_dat_results$table[1,1], living_sit_dat_results$table[2,2])

living_sit_totals = data.frame(test_p = sum(living_sit_dat_results$table[2,]), test_n = sum(living_sit_dat_results$table[1,]), criteria_p = sum(living_sit_dat_results$table[,2]), criteria_n = sum(living_sit_dat_results$table[,1]))
living_sit_totals

```
Table 37 results
```{r}
living_sit_dat_total_n
living_sit_dat_results
living_sit_dat_accurate
living_sit_totals



```
Table 38 data cleaning
The SASSI test is represented by the Rx score or 2 or more which is considered “High prob of Prescription Drug Abuse” and an Rx score of less than 2 is considered “Low probability of prescription Drug Abuse.” The “truth” is represented by the clinician’s diagnosis of an Opioid or Sedative SUD which is indicated by a 1 in either the OPIOIDDIAG and/or SEDDIAG field. No SUD is of course a 1 in the NODIAG field.
```{r}

table_38_dat = clinical_sample
table_38_dat$Rx_test = ifelse(table_38_dat$Rx >=2, 1, 0)
table_38_dat$Rx_truth = ifelse(table_38_dat$OPIOIDDIAG == 1 | table_38_dat$SEDDIAG == 1,1,0)


table_38_dat_total_n =  dim(table_38_dat)[1]

table_38_dat_results=  confusionMatrix(as.factor(table_38_dat$Rx_test), as.factor(table_38_dat$Rx_truth), positive = "1")
table_38_dat_results$table
table_38_dat_results_row1 = data.frame(one_one= table_38_dat_results$table[2,2], one_zero = table_38_dat_results$table[1,2], row1_total = sum(table_38_dat_results$table[,2]))
table_38_dat_results_row1

table_38_dat$NODIAG_real = ifelse(table_38_dat$NODIAG == 0,1,0)
table_38_dat_nodiag_results=  confusionMatrix(as.factor(table_38_dat$Rx_test), as.factor(table_38_dat$NODIAG_real), positive = "1")
table_38_dat_nodiag_results$table
table_38_dat_nodiag_results_row1 = data.frame(one_one= table_38_dat_nodiag_results$table[2,2], one_zero = table_38_dat_nodiag_results$table[1,2], row1_total = sum(table_38_dat_nodiag_results$table[,2]))
table_38_dat_nodiag_results_row1


table_38_dat_accurate =  sum(table_38_dat_results$table[1,1], table_38_dat_results$table[2,2])
table_38_dat_accurate

```
Table 38 text data
```{r}
clinical_sample$SASSDR = as.factor(clinical_sample$SASSDR)
clinical_sample$NODIAG = as.factor(clinical_sample$NODIAG)
SASSDR_clinical =  confusionMatrix(clinical_sample$SASSDR, clinical_sample$NODIAG, positive = "1")
n_correct_SASSDR_clinical =  sum(SASSDR_clinical$table[1,1], SASSDR_clinical$table[2,2])
n_correct_SASSDR_clinical

SASSDR_clinical_totals = data.frame(test_p = sum(SASSDR_clinical$table[2,]), test_n = sum(SASSDR_clinical$table[1,]), criteria_p = sum(SASSDR_clinical$table[,2]), criteria_n = sum(SASSDR_clinical$table[,1]))


cramer_v_SASSDR_clinical = CramerV(SASSDR_clinical$table, conf.level = .99)
cramer_v_SASSDR_clinical

table_38_text_dat = clinical_sample
table_38_text_dat$accurate = ifelse(table_38_text_dat$SASSDR == table_38_text_dat$NODIAG,1,0)
describe.factor(table_38_text_dat$accurate)

#education, employment status, ethnic group membership, gender, and age
table_38_text_dat$ETHN = ifelse(table_38_text_dat$ETHN == 7, 1,0)
### Causing problems drop
describe.factor(table_38_text_dat$LIVING_SIT)
#table_38_text_dat$LIVING_SIT = ifelse(table_38_text_dat$LIVING_SIT == 9,1,0)
table_38_text_dat$LIVING_SIT = NULL
describe.factor(table_38_text_dat$MANDATED)
describe.factor(table_38_text_dat$REFERRAL)
table_38_text_dat$REFERRAL = ifelse(table_38_text_dat$REFERRAL == 7,1,0)
head(table_38_text_dat)
describe.factor(table_38_text_dat$DWI_ARREST)
### Not enough to include
table_38_text_dat$DWI_ARREST = NULL
### Employment status too few
describe.factor(table_38_text_dat$EMP_STATUS)
table_38_text_dat$EMP_STATUS = NULL
describe.factor(table_38_text_dat$TOT_ARREST)
table_38_text_dat$TOT_ARREST = ifelse(table_38_text_dat$TOT_ARREST == 0,1,0)
describe.factor(table_38_text_dat$PRIOR_TREAT)
table_38_text_dat$PRIOR_TREAT = ifelse(table_38_text_dat$PRIOR_TREAT == 1, 1,0)

table_38_text_logit = glm(accurate ~ ETHN + MANDATED + REFERRAL + TOT_ARREST + PRIOR_TREAT, data = table_38_text_dat, family = "binomial")
summary(table_38_text_logit)
```


Table 38 results
```{r}
table_38_dat_total_n
table_38_dat_results
table_38_dat_results_row1
table_38_dat_accurate

table_38_dat_nodiag_results
table_38_dat_nodiag_results_row1

## text results
n_correct_SASSDR_clinical
SASSDR_clinical
SASSDR_clinical_totals
cramer_v_SASSDR_clinical



```


Table 38 new table data cleaning
That is the field of NORXDIAG. If that field is a 1 then it indicated NoRXDIAG. If it’s zero and one of the following categories has a one in it (indicating that’s the Rx of abuse) then they are diagnosed with a  prescription drug abuse diag in that category: RXPOTDIAG; RXOPIOIDDIAG; RXSEDDIAG; RXSTIMDIAG; RXOTHERDIAG. If it’s 0 in NoRXDIAG and no categories are marked, it means they left the field blank and No Rx diag present (sorry, I didn’t do this coding).

```{r}
table_38_2_dat = clinical_sample

table_38_2_dat$rx_2_test = ifelse(table_38_2_dat$NORXDIAG == 0 & table_38_2_dat$RXPOTDIAG == 1 | table_38_2_dat$RXOTHRDRUGDIAG == 1 |table_38_2_dat$RXSEDDIAG == 1 | table_38_2_dat$RXSTIMDIAG == 1 | table_38_2_dat$RXOTHRDRUGDIAG == 1,1,0)
table_38_2_dat$Rx_truth = ifelse(table_38_dat$OPIOIDDIAG == 1 | table_38_dat$SEDDIAG == 1,1,0)


table_38_2_dat_total_n =  dim(table_38_2_dat)[1]

table_38_2_dat_results=  confusionMatrix(as.factor(table_38_2_dat$rx_2_test), as.factor(table_38_2_dat$Rx_truth), positive = "1")

table_38_2_dat_accurate =  sum(table_38_2_dat_results$table[1,1], table_38_2_dat_results$table[2,2])

table_38_2_totals = data.frame(test_p = sum(table_38_2_dat_results$table[2,]), test_n = sum(table_38_2_dat_results$table[1,]), criteria_p = sum(table_38_2_dat_results$table[,2]), criteria_n = sum(table_38_2_dat_results$table[,1]))
table_38_2_totals

```
Table 38 new table results
```{r}
table_38_2_dat_total_n
table_38_2_dat_results
table_38_2_dat_accurate
table_38_2_totals
```
