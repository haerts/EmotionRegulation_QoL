###################################################################################################################
###                                                                                                             ###
###                 The effect of emotion regulation on quality of life in brain tumor patients                 ###
###               ==============================================================================                ###
###                                       PART 1: DESCRIPTIVE ANALYES                                           ###
###                                                                                                             ###
### Created by Hannelore Aerts & Tineke Van Vrekhem                                                             ###
###################################################################################################################


### Read in data and prepare for analyses ------------------------------------------------------------------------#

# Wide format
results <- read.csv(file="RESULTS_ALLpatients.csv", header=TRUE, sep=",")
str(results)

results=within(results, {
  subID=as.character(subID)
  date_t1=as.Date(date_t1, "%d/%m/%y")
  date_t2=as.Date(date_t2, "%d/%m/%y")
  group=factor(group, ordered=TRUE, levels=c('MEN', 'GLI'))
  motivation_t1=(1/MOT_latency_mean_t1)*1000
  motivation_t2=(1/MOT_latency_mean_t2)*1000
})

attach(results)

# Long format
resultslong <- read.table(file="RESULTS_ALLpatients_long.csv", header=TRUE, sep=",")
str(resultslong)

resultslong=within(resultslong, {
  subID=as.character(subID)
  time=factor(time, ordered=TRUE, levels=c('pre','post'))
  date=as.Date(date, "%d/%m/%y")
  group=factor(group, ordered=TRUE, levels=c('MEN', 'GLI'))
  motivation=(1/MOT_latency_mean)*1000
})

library(foreign)
library(corrplot)
library(car)
library(Hmisc)


# Missing data:

# PAT12T1 has missing value for STAI_t1, 
# --> impute with predicted score from regression on PSWQ!
cor(STAI_t1,PSWQ_t1,use='pairwise.complete.obs')
lm_STAI=lm(STAI_t1~PSWQ_t1); summary(lm_STAI)
preds=predict(lm_STAI,data.frame(PSWQ_t1))
results$STAI_t1[11]=round(preds[[11]])
detach(results);attach(results)
resultslong$STAI[21]=round(preds[[11]])
rm(lm_STAI); rm(preds)

# PAT02T1 has missing value for RVP_A_t1 
# --> impute with predicted score from regression on post-op RVP scores 
cor(RVP_A_t1,RVP_A_t2,use='pairwise.complete.obs')
lm_RVP=lm(RVP_A_t1~RVP_A_t2); summary(lm_RVP)
preds=predict(lm_RVP,data.frame(RVP_A_t1))
results$RVP_A_t1[2]=round(preds[[2]],2)
detach(results);attach(results)
resultslong$RVP_A[3]=round(preds[[2]],2)
rm(lm_RVP);rm(preds)

# PAT01T1, PAT02T1 & PAT03T1 have missing value for ERQ_PRO_t1 and ERQ_EX_t1
# --> impute with predicted score from regression on post-op ER scores!
cor(ERQ_PRO_t1, ERQ_PRO_t2,use='pairwise.complete.obs')
lm_PRO=lm(ERQ_PRO_t1~ERQ_PRO_t2); summary(lm_PRO)
preds=predict(lm_PRO,data.frame(ERQ_PRO_t1))
results$ERQ_PRO_t1[c(1:3)]=unname(round(preds[c(1:3)]))
detach(results);attach(results)
resultslong$ERQ_PRO[c(1,3,5)]=unname(round(preds[c(1:3)]))
rm(lm_PRO); rm(preds)

cor(ERQ_EX_t1, ERQ_EX_t2,use='pairwise.complete.obs')
lm_EX=lm(ERQ_EX_t1~ERQ_EX_t2); summary(lm_EX)
preds=predict(lm_EX,data.frame(ERQ_EX_t1))
results$ERQ_EX_t1[c(1:3)]=unname(round(preds[c(1:3)]))
detach(results);attach(results)
resultslong$ERQ_EX[c(1,3,5)]=unname(round(preds[c(1:3)]))
rm(lm_EX); rm(preds)

# Post-op data
results_postop=results[!is.na(date_t2),]


###################################################################################################################
###                                       PART 1: DEMOGRAPHICS DESCRIPTIVES                                     ###
###################################################################################################################

# Participants per group

# Pre
table(group)
#MEN GLI 
#17  11

# Post
table(results_postop$group)
#MEN GLI 
#15   7 


# Age per group

# Pre
aggregate(age, list(group), mean)
#  Group.1        x
#1     MEN 58.47059
#2     GLI 45.63636
vars=aggregate(age, list(group), var); sqrt(vars$x)
#[1] 12.78729 11.95218
rm(vars)

# Post
aggregate(results_postop$age, list(results_postop$group), mean)
#  Group.1        x
#1     MEN 60.86667
#2     GLI 50.71429
vars=aggregate(results_postop$age, list(results_postop$group), var); sqrt(vars$x)
#[1] 11.52554 11.65782
rm(vars)


# Sex per group 

# Pre
table(sex,group)
#group
#sex MEN GLI
#F  14   4
#M   3   7

# Post
with(results_postop,{
  table(sex,group)
})
#   group
#sex MEN GLI
#F  12   3
#M   3   4


# Follow-up time 

results$date_diff = difftime(date_t2, date_t1, units="days") / 30
detach(results); attach(results)
summary(as.numeric(date_diff))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#5.200   6.492   8.183   7.847   8.692  10.267       6

rm(results_postop)


###################################################################################################################
###                                           PART 2: QoL DESCRIPTIVES                                          ###
###################################################################################################################

# 1: Cognition ---------------------------------------------------------------------------------------------------#

# Motivation

results$motivation_diff=motivation_t2-motivation_t1; detach(results); attach(results)
t.test(motivation_diff) #t(21)=1.18, p=0.25


# Correlations with confounders - continuous

colnames(results)
correlates_t1=results[,c(13:16,37,4,10,22)]
head(correlates_t1)
colnames(correlates_t1)=c('RTI','RVP','SOC','SSP','motivation','age','lesion size','STAI')
correlates_t2=results[,c(18:21,36,4,11,23)]
head(correlates_t2)
colnames(correlates_t2)=c('RTI','RVP','SOC','SSP','motivation','age','lesion size','STAI')
cor_t1=rcorr(as.matrix(correlates_t1))
cor_t2=rcorr(as.matrix(correlates_t2))

#png('cognition_covariates.png', width=1000)
par(mfrow=c(1,2))
corrplot(cor(correlates_t1, use="pairwise"), type="lower", method="color", tl.col="black",
         diag=F, addCoef.col="black", p.mat=cor_t1$P, sig.level=.2); title('Before surgery')
corrplot(cor(correlates_t2, use="pairwise"), type="lower", method="color", tl.col="black",
         diag=F, addCoef.col="black", p.mat=cor_t2$P, sig.level=.2); title('After surgery')
#dev.off()
# --> important covariates: motivation, age, STAI
# --> not so important: lesion volume
rm(list=c('cor_t1', 'cor_t2', 'correlates_t1', 'correlates_t2'))


# Correlations with confounders - binary

t.test(RTI_fiveRT_t1~sex) #t(17.469)=0.33, p=0.75, 95% CI=[-46.91 64.25]
t.test(RVP_A_t1~sex) #t(18.625)=-1.00, p=0.33, 95% CI=[-0.08 0.03]
t.test(SOC_prob_minmoves_t1~sex) #t(15.118)=0.19, p=0.85, 95% CI=[-1.58 1.89]
t.test(SSP_spanlength_t1~sex) #t(25.606)=-0.79, p=0.44, 95% CI=[-1.24 0.55]

t.test(RTI_fiveRT_t2~sex) #t(16.446)=-0.54, p=0.59, 95% CI=[-135.35 80.08]
t.test(RVP_A_t2~sex) #t(19.017)=-1.34, p=0.20, 95% CI=[-0.10 0.02]
t.test(SOC_prob_minmoves_t2~sex) #t(14.648)=-1.28, p=0.22, 95% CI=[-2.75 0.69]
t.test(SSP_spanlength_t2~sex) #t(19.406)=-1.35, p=0.19, 95% CI=[-1.90 0.41]

#png('cognition_covariates_sex_t1')
par(mfrow=c(2,2))
plot(RTI_fiveRT_t1~sex)
plot(RVP_A_t1~sex)
plot(SOC_prob_minmoves_t1~sex)
plot(SSP_spanlength_t1~sex)
#dev.off()
#png('cognition_covariates_sex_t2')
par(mfrow=c(2,2))
plot(RTI_fiveRT_t2~sex)
plot(RVP_A_t2~sex)
plot(SOC_prob_minmoves_t2~sex)
plot(SSP_spanlength_t2~sex)
#dev.off()
# --> not so important: sex


# Regress out important confounding effects of CANTAB scores 

#-pre

lm_RTI_t1 = lm(RTI_fiveRT_t1 ~ motivation_t1 + STAI_t1 + age)
#summary(lm_RTI_t1) 
RTI_res_t1=lm_RTI_t1$residuals
shapiro.test(RTI_res_t1) 
results$RTI_res_t1 = RTI_res_t1
rm(lm_RTI_t1); rm(RTI_res_t1)

lm_RVP_t1 = lm(RVP_A_t1 ~ motivation_t1 + STAI_t1 + age)
#summary(lm_RVP_t1) 
RVP_res_t1=lm_RVP_t1$residuals
shapiro.test(RVP_res_t1) 
results$RVP_res_t1 = RVP_res_t1
rm(lm_RVP_t1); rm(RVP_res_t1)

lm_SOC_t1 = lm(SOC_prob_minmoves_t1 ~ motivation_t1 + STAI_t1 + age)
#summary(lm_SOC_t1) 
SOC_res_t1=lm_SOC_t1$residuals
shapiro.test(SOC_res_t1) 
results$SOC_res_t1 = SOC_res_t1
rm(lm_SOC_t1); rm(SOC_res_t1)

lm_SSP_t1 = lm(SSP_spanlength_t1 ~ motivation_t1 + STAI_t1 + age)
#summary(lm_SSP_t1) 
SSP_res_t1=lm_SSP_t1$residuals
shapiro.test(SSP_res_t1) 
results$SSP_res_t1 = SSP_res_t1
rm(lm_SSP_t1); rm(SSP_res_t1)


#-post

lm_RTI_t2 = lm(RTI_fiveRT_t2 ~ motivation_t2 + STAI_t2 + age)
#summary(lm_RTI_t2) 
RTI_res_t2=lm_RTI_t2$residuals
shapiro.test(RTI_res_t2) 
results$RTI_res_t2 = c(RTI_res_t2[1:10], NA , RTI_res_t2[11:15], NA, RTI_res_t2[16:21], 
                       NA, RTI_res_t2[22], NA, NA, NA)
cbind(results$subID, results$RTI_fiveRT_t2, results$RTI_res_t2)
rm(lm_RTI_t2); rm(RTI_res_t2)

lm_RVP_t2 = lm(RVP_A_t2 ~ motivation_t2 + STAI_t2 + age)
#summary(lm_RVP_t2) 
RVP_res_t2=lm_RVP_t2$residuals
shapiro.test(RVP_res_t2) 
results$RVP_res_t2 = c(RVP_res_t2[1:10], NA , RVP_res_t2[11:15], NA, RVP_res_t2[16:21], 
                       NA, RVP_res_t2[22], NA, NA, NA)
cbind(results$RVP_A_t2, results$RVP_res_t2)
rm(lm_RVP_t2); rm(RVP_res_t2)

lm_SOC_t2 = lm(SOC_prob_minmoves_t2 ~ motivation_t2 + STAI_t2 + age)
#summary(lm_SOC_t2) 
SOC_res_t2=lm_SOC_t2$residuals
shapiro.test(SOC_res_t2) 
results$SOC_res_t2 = c(SOC_res_t2[1:10], NA , SOC_res_t2[11:15], NA, SOC_res_t2[16:21], 
                       NA, SOC_res_t2[22], NA, NA, NA)
cbind(SOC_prob_minmoves_t2, results$SOC_res_t2)
rm(lm_SOC_t2); rm(SOC_res_t2)

lm_SSP_t2 = lm(SSP_spanlength_t2 ~ motivation_t2 + STAI_t2 + age)
#summary(lm_SSP_t2) 
SSP_res_t2=lm_SSP_t2$residuals
shapiro.test(SSP_res_t2) 
results$SSP_res_t2 = c(SSP_res_t2[1:10], NA , SSP_res_t2[11:15], NA, SSP_res_t2[16:21],  
                       NA, SSP_res_t2[22], NA, NA, NA)
cbind(SSP_spanlength_t2, results$SSP_res_t2)
rm(lm_SSP_t2); rm(SSP_res_t2)

detach(results); attach(results)


# Normalization procedure to improve interpretability
# --> use mean and sd of pre-op measurements across all patients

results$RTI_norm_t1 = (RTI_fiveRT_t1 - mean(RTI_fiveRT_t1)) / sqrt(var(RTI_fiveRT_t1))
results$RVP_norm_t1 = (RVP_A_t1 - mean(RVP_A_t1)) / sqrt(var(RVP_A_t1))
results$SSP_norm_t1 = (SSP_spanlength_t1 - mean(SSP_spanlength_t1)) / sqrt(var(SSP_spanlength_t1))
results$SOC_norm_t1 = (SOC_prob_minmoves_t1 - mean(SOC_prob_minmoves_t1)) / sqrt(var(SOC_prob_minmoves_t1))

results$RTI_norm_t2 = (RTI_fiveRT_t2 - mean(RTI_fiveRT_t1)) / sqrt(var(RTI_fiveRT_t1))
results$RVP_norm_t2 = (RVP_A_t2 - mean(RVP_A_t1)) / sqrt(var(RVP_A_t1))
results$SSP_norm_t2 = (SSP_spanlength_t2 - mean(SSP_spanlength_t1)) / sqrt(var(SSP_spanlength_t1))
results$SOC_norm_t2 = (SOC_prob_minmoves_t2 - mean(SOC_prob_minmoves_t1)) / sqrt(var(SOC_prob_minmoves_t1))

results$RTI_resnorm_t1 = (RTI_res_t1 - mean(RTI_res_t1)) / sqrt(var(RTI_res_t1))
results$RVP_resnorm_t1 = (RVP_res_t1 - mean(RVP_res_t1)) / sqrt(var(RVP_res_t1))
results$SSP_resnorm_t1 = (SSP_res_t1 - mean(SSP_res_t1)) / sqrt(var(SSP_res_t1))
results$SOC_resnorm_t1 = (SOC_res_t1 - mean(SOC_res_t1)) / sqrt(var(SOC_res_t1))

results$RTI_resnorm_t2 = (RTI_res_t2 - mean(RTI_res_t1)) / sqrt(var(RTI_res_t1))
results$RVP_resnorm_t2 = (RVP_res_t2 - mean(RVP_res_t1)) / sqrt(var(RVP_res_t1))
results$SSP_resnorm_t2 = (SSP_res_t2 - mean(SSP_res_t1)) / sqrt(var(SSP_res_t1))
results$SOC_resnorm_t2 = (SOC_res_t2 - mean(SOC_res_t1)) / sqrt(var(SOC_res_t1))

detach(results); attach(results)


## Difference scores between normalised variables
results$RTI_norm_dif = RTI_norm_t2 - RTI_norm_t1
results$RVP_norm_dif = RVP_norm_t2 - RVP_norm_t1
results$SOC_norm_dif = SOC_norm_t2 - SOC_norm_t1
results$SSP_norm_dif = SSP_norm_t2 - SSP_norm_t1

results$RTI_resnorm_dif = RTI_resnorm_t2 - RTI_resnorm_t1
results$RVP_resnorm_dif = RVP_resnorm_t2 - RVP_resnorm_t1
results$SOC_resnorm_dif = SOC_resnorm_t2 - SOC_resnorm_t1
results$SSP_resnorm_dif = SSP_resnorm_t2 - SSP_resnorm_t1
detach(results); attach(results)


## Tests (after taking into account confounders)

# Diff =/= 0?
t.test(RTI_resnorm_dif) #t(21)=0.34, p=0.74, 95% CI=[-0.45 0.63] 
t.test(RVP_resnorm_dif) #t(21)=0.25, p=0.81, 95% CI=[-0.44 0.56] 
t.test(SOC_resnorm_dif) #t(21)=0.25, p=0.80, 95% CI=[-0.52 0.67] 
t.test(SSP_resnorm_dif) #t(21)=0.43, p=0.67, 95% CI=[-0.41 0.63] 

# --> differences over time not significantly different from 0 (across subjects)



## 2: Affective functioning --------------------------------------------------------------------------------------#

# Normalization procedure to improve interpretability
# --> use mean and sd of pre-op measurements across all patients
results$PSWQ_norm_t1 = (PSWQ_t1 - mean(PSWQ_t1)) / sqrt(var(PSWQ_t1))
results$STAI_norm_t1 = (STAI_t1 - mean(STAI_t1)) / sqrt(var(STAI_t1))
results$PSWQ_norm_t2 = (PSWQ_t2 - mean(PSWQ_t1)) / sqrt(var(PSWQ_t1))
results$STAI_norm_t2 = (STAI_t2 - mean(STAI_t1)) / sqrt(var(STAI_t1))
detach(results);attach(results)

# Tests

results$STAI_norm_diff=STAI_norm_t2-STAI_norm_t1
results$PSWQ_norm_diff=PSWQ_norm_t2-PSWQ_norm_t1
detach(results);attach(results)
t.test(STAI_norm_diff) #t(21)=-5.00, p<0.0001, 95% CI=[-1.17 -0.48]
t.test(PSWQ_norm_diff) #t(21)=0.19, p=0.85, 95% CI=[-0.21 0.25]


# 3: Social functioning ------------------------------------------------------------------------------------------#

# Normalization procedure to improve interpretability
# --> use mean and sd of pre-op measurements across all patients
results$LO_norm_t1 = (LO_t1 - mean(LO_t1)) / sqrt(var(LO_t1))
results$LO_norm_t2 = (LO_t2 - mean(LO_t1)) / sqrt(var(LO_t1))
detach(results);attach(results)

## Tests
results$LO_norm_dif=LO_norm_t2-LO_norm_t1
detach(results);attach(results)
t.test(LO_norm_dif) #t(21)=2.55, p=0.0185, 95% CI=[0.09 0.91]



###################################################################################################################
###                                  PART 3: EMOTION REGULATION DESCRIPTIVES                                    ###
###################################################################################################################

# Normalization procedure to improve interpretability
# --> use mean and sd of pre-op measurements across all patients
results$ERQ_RE_norm_t1 = (ERQ_RE_t1 - mean(ERQ_RE_t1)) / sqrt(var(ERQ_RE_t1))
results$ERQ_SU_norm_t1 = (ERQ_SU_t1 - mean(ERQ_SU_t1)) / sqrt(var(ERQ_SU_t1))
results$ERQ_PRO_norm_t1 = (ERQ_PRO_t1 - mean(ERQ_PRO_t1,na.rm=T)) / sqrt(var(ERQ_PRO_t1,na.rm=T))
results$ERQ_EX_norm_t1 = (ERQ_EX_t1 - mean(ERQ_EX_t1,na.rm=T)) / sqrt(var(ERQ_EX_t1,na.rm=T))

results$ERQ_RE_norm_t2 = (ERQ_RE_t2 - mean(ERQ_RE_t1)) / sqrt(var(ERQ_RE_t1))
results$ERQ_SU_norm_t2 = (ERQ_SU_t2 - mean(ERQ_SU_t1)) / sqrt(var(ERQ_SU_t1))
results$ERQ_PRO_norm_t2 = (ERQ_PRO_t2 - mean(ERQ_PRO_t1,na.rm=T)) / sqrt(var(ERQ_PRO_t1,na.rm=T))
results$ERQ_EX_norm_t2 = (ERQ_EX_t2 - mean(ERQ_EX_t1,na.rm=T)) / sqrt(var(ERQ_EX_t1,na.rm=T))
detach(results); attach(results)

## Tests

results$ERQ_RE_norm_dif=ERQ_RE_norm_t2-ERQ_RE_norm_t1
results$ERQ_PRO_norm_dif=ERQ_PRO_norm_t2-ERQ_PRO_norm_t1
results$ERQ_EX_norm_dif=ERQ_EX_norm_t2-ERQ_EX_norm_t1
results$ERQ_SU_norm_dif=ERQ_SU_norm_t2-ERQ_SU_norm_t1
detach(results);attach(results)

t.test(ERQ_RE_norm_dif) #t(21)=-0.52, p=0.61, 95% CI=[-0.45 0.27]
t.test(ERQ_PRO_norm_dif) #t(21)=-1.52, p=0.14, 95% CI=[-0.58 0.09]
t.test(ERQ_EX_norm_dif) #t(21)=0.12, p=0.91, 95% CI=[-0.23 0.26]
t.test(ERQ_SU_norm_dif) #t(21)=1.20, p=0.24, 95% CI=[-0.16 0.59]


## Association between ER and QoL indices 

colnames(results)
correlates_t1=results[,c(80:83,34,22,24,55:58)]
head(correlates_t1)
colnames(correlates_t1)=c('ER_RE','ER_SU','ER_PRO','ER_EX','PSWQ','STAI','LO','RTI','RVP','SSP','SOC')
correlates_t2=results[,c(84:87,35,23,25,59:62)]
head(correlates_t2)
colnames(correlates_t2)=c('ER_RE','ER_SU','ER_PRO','ER_EX','PSWQ','STAI','LO','RTI','RVP','SSP','SOC')
cor_t1=rcorr(as.matrix(correlates_t1))
cor_t2=rcorr(as.matrix(correlates_t2))

#png('ER_covariates.png', width=1200)
par(mfrow=c(1,2))
corrplot(cor(correlates_t1, use="pairwise"), type="lower", method="color", tl.col="black",
         diag=F, addCoef.col="black", p.mat=cor_t1$P, sig.level=.2); title('Before surgery')
corrplot(cor(correlates_t2, use="pairwise"), type="lower", method="color", tl.col="black",
         diag=F, addCoef.col="black", p.mat=cor_t2$P, sig.level=.2); title('After surgery')
#dev.off()
# --> important covariates: affective & social functioning (cognitive not so much)
rm(list=c('cor_t1', 'cor_t2', 'correlates_t1', 'correlates_t2', 'resultslong_ER'))


#------------------------------------------------------------------------------------------------------------------#

# Save dataset with ALL variables

write.table(x=results, file="RESULTS_ALLpatients_afterprep.csv", quote=TRUE, sep=';', dec='.', row.names=FALSE)





