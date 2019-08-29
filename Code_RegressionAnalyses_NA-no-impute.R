###################################################################################################################
###                                                                                                             ###
###                 The effect of emotion regulation on quality of life in brain tumor patients                 ###
###               ==============================================================================                ###
###                                                                                                             ###
### Created by Hannelore Aerts & Tineke Van Vrekhem                                                             ###
### Date last update: 26/03/2019                                                                                ###
###   - include loneliness in MANOVA model for affective functioning                                            ###
### Psycho-Oncology Revision 1, version 1:                                                                      ###
###   - include caregivers data (CON)                                                                           ###
###   - drop motivation as confounder since this may capture relevant information (and as a result induce       ###
###     spurious associations                                                                                   ###
###   - for normalization: use mean and sd across caregivers before surgery                                     ###
###   - redo regression analyses without motivation as confounder, and including group?                         ###
###   - include correlation analyses                                                                            ###
###   - no imputation of missing data                                                                           ###
### --> This version (without imputation of missing values) was used for regression analyses, but this doesn,'t ###
###     make a large difference, as most interesting associations were found post-operatively and then no       ###
###     data were imputed.                                                                                      ###
###################################################################################################################

setwd('/home/hannelore/Documents/ANALYSES/ER_QoL/')

### Read in data and prepare for analyses ------------------------------------------------------------------------#

# Wide format
results <- read.csv(file="RESULTS_ALL_noMRI.csv", header=TRUE, sep=",")
str(results)

results=within(results, {
  subID=as.character(subID)
  date_t1=as.Date(date_t1, "%d/%m/%y")
  date_t2=as.Date(date_t2, "%d/%m/%y")
  group=factor(group, ordered=TRUE, levels=c('CON', 'MEN', 'GLI'))
})

attach(results)

library(foreign)
library(corrplot)
library(car)
library(Hmisc)


# Post-op data
results_postop=results[!is.na(date_t2),]


###################################################################################################################
###                                       PART 1: DEMOGRAPHICS DESCRIPTIVES                                     ###
###################################################################################################################

#-- Participants per group

# Pre
table(group)
#CON MEN GLI 
#11  17  11 

# Post
table(results_postop$group)
#CON MEN GLI 
#10  15   7 


#-- Age per group

# Pre
aggregate(age, list(group), mean)
#  Group.1        x
#1     CON 58.63636
#2     MEN 58.47059
#3     GLI 45.63636
vars=aggregate(age, list(group), var); sqrt(vars$x)
#[1] 10.29828 12.78729 11.95218
rm(vars)
summary(aov(age~group)) 
#            Df Sum Sq Mean Sq F value Pr(>F)  
#group        2   1314   657.1   4.634 0.0162 *
#Residuals   36   5105   141.8

# Post
aggregate(results_postop$age, list(results_postop$group), mean)
#  Group.1        x
#1     CON 59.60000
#2     MEN 60.86667
#3     GLI 50.71429
vars=aggregate(results_postop$age, list(results_postop$group), var); sqrt(vars$x)
#[1] 10.31935 11.52554 11.65782
rm(vars)
summary(aov(results_postop$age~results_postop$group)) 
#                     Df Sum Sq Mean Sq F value Pr(>F)
#results_postop$group  2    518   259.2   2.069  0.145
#Residuals            29   3634   125.3


#-- Sex per group 

# Pre
table(sex,group)
#group
#sex CON MEN GLI
#F   4  14   4
#M   7   3   7

# Post
with(results_postop,{
  table(sex,group)
})
#   group
#sex CON MEN GLI
#F   4  12   3
#M   6   3   4


#-- Follow-up time 

results$date_diff = difftime(date_t2, date_t1, units="days") / 30
detach(results); attach(results)
summary(as.numeric(date_diff))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#5.200   6.400   8.183   7.873   8.867  10.733       7
summary(aov(as.numeric(date_diff)~group)) 
#            Df Sum Sq Mean Sq F value Pr(>F)
#group        2   0.52   0.258    0.11  0.897
#Residuals   29  68.31   2.356             
#7 observations deleted due to missingness#

rm(results_postop)


###################################################################################################################
###                                           PART 2: QoL DESCRIPTIVES                                          ###
###################################################################################################################

# 1: Cognition ---------------------------------------------------------------------------------------------------#

# Correlations with confounders - continuous

colnames(results)
correlates_t1=results[,c(13:16,4,10,22)]
head(correlates_t1)
colnames(correlates_t1)=c('RTI','RVP','SOC','SSP','age','lesion size','STAI')
correlates_t2=results[,c(18:21,4,11,23)]
head(correlates_t2)
colnames(correlates_t2)=c('RTI','RVP','SOC','SSP','age','lesion size','STAI')

cor_t1=rcorr(as.matrix(correlates_t1))
cor_t2=rcorr(as.matrix(correlates_t2))

png('cognition_covariates_inclCON_noMotivation_p05.png', width=1000)
par(mfrow=c(1,2))
corrplot(cor(correlates_t1, use="pairwise"), type="lower", method="color", tl.col="black",
         diag=F, addCoef.col="black", p.mat=cor_t1$P, sig.level=.05); title('Before surgery')
corrplot(cor(correlates_t2, use="pairwise"), type="lower", method="color", tl.col="black",
         diag=F, addCoef.col="black", p.mat=cor_t2$P, sig.level=.05); title('After surgery')
dev.off()
# --> important covariates: age, STAI
# --> not so important: lesion volume
rm(list=c('cor_t1', 'cor_t2', 'correlates_t1', 'correlates_t2'))


# Correlations with confounders - binary

t.test(RTI_fiveRT_t1~sex) #ns
t.test(RVP_A_t1~sex) #ns
t.test(SOC_prob_minmoves_t1~sex) #ns
t.test(SSP_spanlength_t1~sex) #ns

t.test(RTI_fiveRT_t2~sex) #ns
t.test(RVP_A_t2~sex) #ns
t.test(SOC_prob_minmoves_t2~sex) #ns
t.test(SSP_spanlength_t2~sex) #ns

png('cognition_covariates_sex_t1_inclCON.png')
par(mfrow=c(2,2))
plot(RTI_fiveRT_t1~sex)
plot(RVP_A_t1~sex)
plot(SOC_prob_minmoves_t1~sex)
plot(SSP_spanlength_t1~sex)
dev.off()
png('cognition_covariates_sex_t2_inclCON.png')
par(mfrow=c(2,2))
plot(RTI_fiveRT_t2~sex)
plot(RVP_A_t2~sex)
plot(SOC_prob_minmoves_t2~sex)
plot(SSP_spanlength_t2~sex)
dev.off()
# --> not so important: sex


# Regress out important confounding effects of CANTAB scores 

#-pre

lm_RTI_t1 = lm(RTI_fiveRT_t1 ~ STAI_t1 + age)
#summary(lm_RTI_t1) 
RTI_res_t1=lm_RTI_t1$residuals
shapiro.test(RTI_res_t1) #p=0.015 but hist okay
results$RTI_res_t1 = c(RTI_res_t1[1:21],NA,RTI_res_t1[22:38])
rm(lm_RTI_t1); rm(RTI_res_t1)

lm_RVP_t1 = lm(RVP_A_t1 ~ STAI_t1 + age)
#summary(lm_RVP_t1) 
RVP_res_t1=lm_RVP_t1$residuals
shapiro.test(RVP_res_t1) #p=0.31
results$RVP_res_t1 = c(RVP_res_t1[1:12],NA,RVP_res_t1[13:20],NA,RVP_res_t1[21:37])
rm(lm_RVP_t1); rm(RVP_res_t1)

lm_SOC_t1 = lm(SOC_prob_minmoves_t1 ~ STAI_t1 + age)
#summary(lm_SOC_t1) 
SOC_res_t1=lm_SOC_t1$residuals
shapiro.test(SOC_res_t1) #p=0.42
results$SOC_res_t1 = c(SOC_res_t1[1:21],NA,SOC_res_t1[22:38])
rm(lm_SOC_t1); rm(SOC_res_t1)

lm_SSP_t1 = lm(SSP_spanlength_t1 ~ STAI_t1 + age)
#summary(lm_SSP_t1) 
SSP_res_t1=lm_SSP_t1$residuals
shapiro.test(SSP_res_t1) #p=0.20
results$SSP_res_t1 = c(SSP_res_t1[1:21],NA,SSP_res_t1[22:38])
rm(lm_SSP_t1); rm(SSP_res_t1)


#-post

lm_RTI_t2 = lm(RTI_fiveRT_t2 ~ + STAI_t2 + age)
#summary(lm_RTI_t2) 
RTI_res_t2=lm_RTI_t2$residuals
shapiro.test(RTI_res_t2) #p=0.0006 but hist okay
results$RTI_res_t2 = c(NA,RTI_res_t2[1:20], NA , RTI_res_t2[21:25], NA, RTI_res_t2[26:31], 
                       NA, RTI_res_t2[32], NA, NA, NA)
cbind(results$subID, results$RTI_fiveRT_t2, results$RTI_res_t2)
rm(lm_RTI_t2); rm(RTI_res_t2)

lm_RVP_t2 = lm(RVP_A_t2 ~ STAI_t2 + age)
#summary(lm_RVP_t2) 
RVP_res_t2=lm_RVP_t2$residuals
shapiro.test(RVP_res_t2) #p=0.05
results$RVP_res_t2 = c(NA,RVP_res_t2[1:20], NA , RVP_res_t2[21:25], NA, RVP_res_t2[26:31], 
                       NA, RVP_res_t2[32], NA, NA, NA)
cbind(results$RVP_A_t2, results$RVP_res_t2)
rm(lm_RVP_t2); rm(RVP_res_t2)

lm_SOC_t2 = lm(SOC_prob_minmoves_t2 ~ STAI_t2 + age)
#summary(lm_SOC_t2) 
SOC_res_t2=lm_SOC_t2$residuals
shapiro.test(SOC_res_t2) #p=0.01
results$SOC_res_t2 = c(NA,SOC_res_t2[1:20], NA , SOC_res_t2[21:25], NA, SOC_res_t2[26:31], 
                       NA, SOC_res_t2[32], NA, NA, NA)
cbind(SOC_prob_minmoves_t2, results$SOC_res_t2)
rm(lm_SOC_t2); rm(SOC_res_t2)

lm_SSP_t2 = lm(SSP_spanlength_t2 ~ STAI_t2 + age)
#summary(lm_SSP_t2) 
SSP_res_t2=lm_SSP_t2$residuals
shapiro.test(SSP_res_t2) #p=0.91
results$SSP_res_t2 = c(NA,SSP_res_t2[1:20], NA , SSP_res_t2[21:25], NA, SSP_res_t2[26:31],  
                       NA, SSP_res_t2[32], NA, NA, NA)
cbind(SSP_spanlength_t2, results$SSP_res_t2)
rm(lm_SSP_t2); rm(SSP_res_t2)

detach(results); attach(results)


# Normalization procedure to improve interpretability
# --> use mean and sd of pre-op measurements across control subjects

results$RTI_norm_t1 = (RTI_fiveRT_t1 - mean(RTI_fiveRT_t1[1:11])) / sqrt(var(RTI_fiveRT_t1[1:11]))
results$RVP_norm_t1 = (RVP_A_t1 - mean(RVP_A_t1[1:11])) / sqrt(var(RVP_A_t1[1:11]))
results$SSP_norm_t1 = (SSP_spanlength_t1 - mean(SSP_spanlength_t1[1:11])) / sqrt(var(SSP_spanlength_t1[1:11]))
results$SOC_norm_t1 = (SOC_prob_minmoves_t1 - mean(SOC_prob_minmoves_t1[1:11])) / sqrt(var(SOC_prob_minmoves_t1[1:11]))

results$RTI_norm_t2 = (RTI_fiveRT_t2 - mean(RTI_fiveRT_t1[1:11])) / sqrt(var(RTI_fiveRT_t1[1:11]))
results$RVP_norm_t2 = (RVP_A_t2 - mean(RVP_A_t1[1:11])) / sqrt(var(RVP_A_t1[1:11]))
results$SSP_norm_t2 = (SSP_spanlength_t2 - mean(SSP_spanlength_t1[1:11])) / sqrt(var(SSP_spanlength_t1[1:11]))
results$SOC_norm_t2 = (SOC_prob_minmoves_t2 - mean(SOC_prob_minmoves_t1[1:11])) / sqrt(var(SOC_prob_minmoves_t1[1:11]))

results$RTI_resnorm_t1 = (RTI_res_t1 - mean(RTI_res_t1[1:11])) / sqrt(var(RTI_res_t1[1:11]))
results$RVP_resnorm_t1 = (RVP_res_t1 - mean(RVP_res_t1[1:11])) / sqrt(var(RVP_res_t1[1:11]))
results$SSP_resnorm_t1 = (SSP_res_t1 - mean(SSP_res_t1[1:11])) / sqrt(var(SSP_res_t1[1:11]))
results$SOC_resnorm_t1 = (SOC_res_t1 - mean(SOC_res_t1[1:11])) / sqrt(var(SOC_res_t1[1:11]))

results$RTI_resnorm_t2 = (RTI_res_t2 - mean(RTI_res_t1[1:11])) / sqrt(var(RTI_res_t1[1:11]))
results$RVP_resnorm_t2 = (RVP_res_t2 - mean(RVP_res_t1[1:11])) / sqrt(var(RVP_res_t1[1:11]))
results$SSP_resnorm_t2 = (SSP_res_t2 - mean(SSP_res_t1[1:11])) / sqrt(var(SSP_res_t1[1:11]))
results$SOC_resnorm_t2 = (SOC_res_t2 - mean(SOC_res_t1[1:11])) / sqrt(var(SOC_res_t1[1:11]))

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


## Group differences? (after taking into account confounders)

# Pre
summary(aov(RTI_resnorm_t1~group)) #F(2,35)=0.33,p=0.72
summary(aov(RVP_resnorm_t1~group)) #F(2,34)=0.88,p=0.42
summary(aov(SOC_resnorm_t1~group)) #F(2,35)=0.20,p=0.82
summary(aov(SSP_resnorm_t1~group)) #F(2,35)=0.08,p=0.92

# Post
summary(aov(RTI_resnorm_t2~group)) #F(2,29)=0.01,p=0.99
summary(aov(RVP_resnorm_t2~group)) #F(2,29)=0.33,p=0.72
summary(aov(SOC_resnorm_t2~group)) #F(2,29)=1.99,p=0.16
summary(aov(SSP_resnorm_t2~group)) #F(2,29)=0.08,p=0.92

# Diff
summary(aov(RTI_resnorm_dif~group)) #F(2,29)=0.27,p=0.77
summary(aov(RVP_resnorm_dif~group)) #F(2,28)=0.29,p=0.75
summary(aov(SOC_resnorm_dif~group)) #F(2,29)=1.46,p=0.25
summary(aov(SSP_resnorm_dif~group)) #F(2,29)=0.60,p=0.55


# --> no group differences whatsoever 

# Diff =/= 0?
t.test(RTI_resnorm_dif) #t(31)=0.45, p=0.65, 95% CI=[-0.41 0.65] 
t.test(RVP_resnorm_dif) #t(30)=0.43, p=0.67, 95% CI=[-0.30 0.46] 
t.test(SOC_resnorm_dif) #t(31)=-0.04, p=0.97, 95% CI=[-0.40 0.39] 
t.test(SSP_resnorm_dif) #t(31)=0.15, p=0.88, 95% CI=[-0.37 0.43] 

# --> differences over time not significantly different from 0 (across subjects)


## 2: Affective functioning --------------------------------------------------------------------------------------#

# Normalization procedure to improve interpretability
# --> use mean and sd of pre-op measurements across controls
results$PSWQ_norm_t1 = (PSWQ_t1 - mean(PSWQ_t1[1:11])) / sqrt(var(PSWQ_t1[1:11]))
results$STAI_norm_t1 = (STAI_t1 - mean(STAI_t1[1:11])) / sqrt(var(STAI_t1[1:11]))
results$LO_norm_t1 = (LO_t1 - mean(LO_t1[1:11])) / sqrt(var(LO_t1[1:11]))
results$PSWQ_norm_t2 = (PSWQ_t2 - mean(PSWQ_t1[1:11])) / sqrt(var(PSWQ_t1[1:11]))
results$STAI_norm_t2 = (STAI_t2 - mean(STAI_t1[1:11])) / sqrt(var(STAI_t1[1:11]))
results$LO_norm_t2 = (LO_t2 - mean(LO_t1[1:11])) / sqrt(var(LO_t1[1:11]))
detach(results);attach(results)

## Tests

# Pre
summary(aov(STAI_norm_t1~group)) #F(2,35)=0.29, p=0.75
summary(aov(PSWQ_norm_t1~group)) #F(2,36)=2.43, p=0.10
plot(PSWQ_norm_t1~group)
summary(aov(LO_norm_t1~group)) #F(2,36)=0.15, p=0.86

# Post
summary(aov(STAI_norm_t2~group)) #F(2,29)=0.26, p=0.77
summary(aov(PSWQ_norm_t2~group)) #F(2,29)=0.82, p=0.45
summary(aov(LO_norm_t2~group)) #F(2,29)=1.09, p=0.35

# Diff
results$STAI_norm_diff=STAI_norm_t2-STAI_norm_t1
results$PSWQ_norm_diff=PSWQ_norm_t2-PSWQ_norm_t1
results$LO_norm_dif=LO_norm_t2-LO_norm_t1
detach(results);attach(results)
t.test(STAI_norm_diff) #t(31)=-5.95, p<0.0001, 95% CI=[-1.60 -0.78]
t.test(PSWQ_norm_diff) #t(31)=-1.39, p=0.17, 95% CI=[-0.40 0.07]
t.test(LO_norm_dif) #t(31)=1.36, p=0.1845, 95% CI=[-0.13 0.62]
summary(aov(STAI_norm_diff~group)) #F(2,29)=0.23, p=0.79
summary(aov(PSWQ_norm_diff~group)) #F(2,29)=3.26, p=0.053
summary(aov(LO_norm_dif~group)) #F(2,29)=4.28, p=0.0235



###################################################################################################################
###                                  PART 3: EMOTION REGULATION DESCRIPTIVES                                    ###
###################################################################################################################

# Normalization procedure to improve interpretability
# --> use mean and sd of pre-op measurements across all patients
results$ERQ_RE_norm_t1 = (ERQ_RE_t1 - mean(ERQ_RE_t1[1:11])) / sqrt(var(ERQ_RE_t1[1:11]))
results$ERQ_SU_norm_t1 = (ERQ_SU_t1 - mean(ERQ_SU_t1[1:11])) / sqrt(var(ERQ_SU_t1[1:11]))
results$ERQ_PRO_norm_t1 = (ERQ_PRO_t1 - mean(ERQ_PRO_t1[1:11],na.rm=T)) / sqrt(var(ERQ_PRO_t1[1:11],na.rm=T))
results$ERQ_EX_norm_t1 = (ERQ_EX_t1 - mean(ERQ_EX_t1[1:11],na.rm=T)) / sqrt(var(ERQ_EX_t1[1:11],na.rm=T))

results$ERQ_RE_norm_t2 = (ERQ_RE_t2 - mean(ERQ_RE_t1[1:11])) / sqrt(var(ERQ_RE_t1[1:11]))
results$ERQ_SU_norm_t2 = (ERQ_SU_t2 - mean(ERQ_SU_t1[1:11])) / sqrt(var(ERQ_SU_t1[1:11]))
results$ERQ_PRO_norm_t2 = (ERQ_PRO_t2 - mean(ERQ_PRO_t1[1:11],na.rm=T)) / sqrt(var(ERQ_PRO_t1[1:11],na.rm=T))
results$ERQ_EX_norm_t2 = (ERQ_EX_t2 - mean(ERQ_EX_t1[1:11],na.rm=T)) / sqrt(var(ERQ_EX_t1[1:11],na.rm=T))
detach(results); attach(results)

## Tests

# Pre
summary(aov(ERQ_RE_norm_t1~group)) #F(2,36)=0.81, p=0.45
summary(aov(ERQ_PRO_norm_t1~group)) #F(2,31)=0.53, p=0.59
summary(aov(ERQ_EX_norm_t1~group)) #F(2,31)=0.30, p=0.74
summary(aov(ERQ_SU_norm_t1~group)) #F(2,36)=0.63, p=0.54

# Post
summary(aov(ERQ_RE_norm_t2~group)) #F(2,29)=1.32, p=0.28
summary(aov(ERQ_PRO_norm_t2~group)) #F(2,29)=0.18, p=0.84
summary(aov(ERQ_EX_norm_t2~group)) #F(2,29)=1.63, p=0.21
summary(aov(ERQ_SU_norm_t2~group)) #F(2,29)=3.43, p=0.0459

# Differences
results$ERQ_RE_norm_dif=ERQ_RE_norm_t2-ERQ_RE_norm_t1
results$ERQ_PRO_norm_dif=ERQ_PRO_norm_t2-ERQ_PRO_norm_t1
results$ERQ_EX_norm_dif=ERQ_EX_norm_t2-ERQ_EX_norm_t1
results$ERQ_SU_norm_dif=ERQ_SU_norm_t2-ERQ_SU_norm_t1
detach(results);attach(results)

t.test(ERQ_RE_norm_dif) #t(31)=-0.45, p=0.65, 95% CI=[-0.41 0.27]
t.test(ERQ_PRO_norm_dif) #t(27)=-1.72, p=0.0966, 95% CI=[-1.29 0.11]
t.test(ERQ_EX_norm_dif) #t(27)=-0.95, p=0.35, 95% CI=[-0.33 0.12]
t.test(ERQ_SU_norm_dif) #t(31)=0.27, p=0.79, 95% CI=[-0.21 0.27]

summary(aov(ERQ_RE_norm_dif~group)) #F(2,29)=0.80, p=0.46
summary(aov(ERQ_PRO_norm_dif~group)) #F(2,25)=2.50, p=0.10
summary(aov(ERQ_EX_norm_dif~group)) #F(2,25)=1.34, p=0.28
summary(aov(ERQ_SU_norm_dif~group)) #F(2,29)=3.38, p=0.048


## Plots

# ER-RE
png('ER_reappraisal_norm_inclCON.png')
par(mfrow=c(1,3))
plot(ERQ_RE_norm_t1~group, col="gray", xlab="", ylab="Reappraisal", ylim=c(-2.7,2.7)); title('Before surgery')
plot(ERQ_RE_norm_t2~group, col="gray", xlab="", ylab="", ylim=c(-2.7,2.7)); title('After surgery')
plot(ERQ_RE_norm_dif~group, col="gray", xlab="", ylab=""); title('Difference')
dev.off()

# ER-PRO
png('ER_processing_norm_inclCON.png')
par(mfrow=c(1,3))
plot(ERQ_PRO_norm_t1~group, col="gray", xlab="", ylab="Processing", ylim=c(-4.2,4.2)); title('Before surgery')
plot(ERQ_PRO_norm_t2~group, col="gray", xlab="", ylab="", ylim=c(-4.2,4.2)); title('After surgery')
plot(ERQ_PRO_norm_dif~group, col="gray", xlab="", ylab=""); title('Difference')
dev.off()

# ER-EX
png('ER_expression_norm_inclCON.png')
par(mfrow=c(1,3))
plot(ERQ_EX_norm_t1~group, col="gray", xlab="", ylab="Expression", ylim=c(-2.5,2)); title('Before surgery')
plot(ERQ_EX_norm_t2~group, col="gray", xlab="", ylab="", ylim=c(-2.5,2)); title('After surgery')
plot(ERQ_EX_norm_dif~group, col="gray", xlab="", ylab=""); title('Difference')
dev.off()

# ER-SU
png('ER_suppression_norm_inclCON.png')
par(mfrow=c(1,3))
plot(ERQ_SU_norm_t1~group, col="gray", xlab="", ylab="Suppression", ylim=c(-2,2.2)); title('Before surgery')
plot(ERQ_SU_norm_t2~group, col="gray", xlab="", ylab="", ylim=c(-2,2.2)); title('After surgery')
plot(ERQ_SU_norm_dif~group, col="gray", xlab="", ylab=""); title('Difference')
dev.off()



###################################################################################################################
###                             PART 4: Associations between ER and QoL indices                                 ###
###################################################################################################################

# All subjects
colnames(results)
correlates_t1=results[,c(78:81,34,22,24,53:56)]
head(correlates_t1)
colnames(correlates_t1)=c('ER_RE','ER_SU','ER_PRO','ER_EX','PSWQ','STAI','LO','RTI res','RVP res','SSP res','SOC res')
correlates_t2=results[,c(82:85,35,23,25,57:60)]
head(correlates_t2)
colnames(correlates_t2)=c('ER_RE','ER_SU','ER_PRO','ER_EX','PSWQ','STAI','LO','RTI res','RVP res','SSP res','SOC res')
cor_t1=rcorr(as.matrix(correlates_t1))
cor_t2=rcorr(as.matrix(correlates_t2))

png('ER_covariates_inclCON_noMotivation_NAnoImpute_p10.png', width=1200)
par(mfrow=c(1,2))
corrplot(cor(correlates_t1, use="pairwise"), type="lower", method="color", tl.col="black",
         diag=F, addCoef.col="black", p.mat=cor_t1$P, sig.level=.10); title('Before surgery')
corrplot(cor(correlates_t2, use="pairwise"), type="lower", method="color", tl.col="black",
         diag=F, addCoef.col="black", p.mat=cor_t2$P, sig.level=.10); title('After surgery')
dev.off()
rm(list=c('cor_t1', 'cor_t2', 'correlates_t1', 'correlates_t2'))

# Patients only
correlates_t1=results[c(12:39),c(78:81,34,22,24,53:56)]
head(correlates_t1)
colnames(correlates_t1)=c('ER_RE','ER_SU','ER_PRO','ER_EX','PSWQ','STAI','LO','RTI res','RVP res','SSP res','SOC res')
correlates_t2=results[c(12:39),c(82:85,35,23,25,57:60)]
head(correlates_t2)
colnames(correlates_t2)=c('ER_RE','ER_SU','ER_PRO','ER_EX','PSWQ','STAI','LO','RTI res','RVP res','SSP res','SOC res')
cor_t1=rcorr(as.matrix(correlates_t1))
cor_t2=rcorr(as.matrix(correlates_t2))

png('ER_covariates_PATonly_noMotivation_NAnoImpute_p10.png', width=1200)
par(mfrow=c(1,2))
corrplot(cor(correlates_t1, use="pairwise"), type="lower", method="color", tl.col="black",
         diag=F, addCoef.col="black", p.mat=cor_t1$P, sig.level=.10); title('Before surgery')
corrplot(cor(correlates_t2, use="pairwise"), type="lower", method="color", tl.col="black",
         diag=F, addCoef.col="black", p.mat=cor_t2$P, sig.level=.10); title('After surgery')
dev.off()
rm(list=c('cor_t1', 'cor_t2', 'correlates_t1', 'correlates_t2'))

# Caregivers only
correlates_t1=results[c(1:11),c(78:81,34,22,24,53:56)]
head(correlates_t1)
colnames(correlates_t1)=c('ER_RE','ER_SU','ER_PRO','ER_EX','PSWQ','STAI','LO','RTI res','RVP res','SSP res','SOC res')
correlates_t2=results[c(1:11),c(82:85,35,23,25,57:60)]
head(correlates_t2)
colnames(correlates_t2)=c('ER_RE','ER_SU','ER_PRO','ER_EX','PSWQ','STAI','LO','RTI res','RVP res','SSP res','SOC res')
cor_t1=rcorr(as.matrix(correlates_t1))
cor_t2=rcorr(as.matrix(correlates_t2))

png('ER_covariates_CONonly_noMotivation_NAnoImpute_p10.png', width=1200)
par(mfrow=c(1,2))
corrplot(cor(correlates_t1, use="pairwise"), type="lower", method="color", tl.col="black",
         diag=F, addCoef.col="black", p.mat=cor_t1$P, sig.level=.10); title('Before surgery')
corrplot(cor(correlates_t2, use="pairwise"), type="lower", method="color", tl.col="black",
         diag=F, addCoef.col="black", p.mat=cor_t2$P, sig.level=.10); title('After surgery')
dev.off()
rm(list=c('cor_t1', 'cor_t2', 'correlates_t1', 'correlates_t2'))



###################################################################################################################
###                                           PART 5: Regression models                                         ###
###################################################################################################################

## Pre-operative associations ------------------------------------------------------------------------------------#

# Cognitive functioning t1 ~ Emotion regulation t1 

man_cog_pre=manova(cbind(RTI_norm_t1,RVP_norm_t1,SOC_norm_t1,SSP_norm_t1) ~ 
                     age + STAI_norm_t1 + 
                     ERQ_RE_norm_t1 + ERQ_SU_norm_t1 + ERQ_EX_norm_t1 + ERQ_PRO_norm_t1)
summary(man_cog_pre)
#                Df  Pillai approx F num Df den Df Pr(>F)
#age              1 0.45042   4.7126      4     23 0.006329 **
#STAI_norm_t1     1 0.36367   3.2861      4     23 0.028634 * 
#ERQ_RE_norm_t1   1 0.05833   0.3561      4     23 0.837084   
#ERQ_SU_norm_t1   1 0.14587   0.9820      4     23 0.436756   
#ERQ_EX_norm_t1   1 0.07762   0.4838      4     23 0.747379   
#ERQ_PRO_norm_t1  1 0.05390   0.3276      4     23 0.856554   
#Residuals       26  



# Affective functioning t1 ~ Emotion regulation t1 

man_aff_pre=manova(cbind(PSWQ_norm_t1, STAI_norm_t1, LO_norm_t1) ~ 
                     ERQ_RE_norm_t1 + ERQ_SU_norm_t1 + ERQ_EX_norm_t1 + ERQ_PRO_norm_t1)
summary(man_aff_pre)
#                Df  Pillai approx F num Df den Df   Pr(>F)   
#ERQ_RE_norm_t1   1 0.21805   2.4168      3     26 0.089122 . 
#ERQ_SU_norm_t1   1 0.14398   1.4577      3     26 0.249066   
#ERQ_EX_norm_t1   1 0.38756   5.4844      3     26 0.004682 **
#ERQ_PRO_norm_t1  1 0.20280   2.2047      3     26 0.111505   
#Residuals       28   

lm_PSWQ_pre=lm(PSWQ_norm_t1~ERQ_RE_norm_t1+ERQ_EX_norm_t1)
summary(lm_PSWQ_pre)
#                Estimate Std. Error t value Pr(>|t|)  
#(Intercept)     0.001522   0.166951   0.009   0.9928  
#ERQ_RE_norm_t1 -0.357487   0.137651  -2.597   0.0143 *
#ERQ_EX_norm_t1  0.350527   0.162333   2.159   0.0387 *
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.9652 on 31 degrees of freedom
#(5 observations deleted due to missingness)
#Multiple R-squared:  0.2573,	Adjusted R-squared:  0.2094 
#F-statistic: 5.369 on 2 and 31 DF,  p-value: 0.00995

lm_STAI_pre=lm(STAI_norm_t1~ERQ_RE_norm_t1+ERQ_EX_norm_t1)
summary(lm_STAI_pre)
#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
#(Intercept)      0.1874     0.2500   0.750    0.459
#ERQ_RE_norm_t1  -0.2105     0.2068  -1.018    0.317
#ERQ_EX_norm_t1   0.1539     0.2579   0.597    0.555
#
#Residual standard error: 1.409 on 30 degrees of freedom
#(6 observations deleted due to missingness)
#Multiple R-squared:  0.04579,	Adjusted R-squared:  -0.01782 
#F-statistic: 0.7198 on 2 and 30 DF,  p-value: 0.4951

lm_soc_pre=lm(LO_norm_t1 ~ ERQ_RE_norm_t1 + ERQ_EX_norm_t1)
summary(lm_soc_pre)
#Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)
#(Intercept)    -0.18601    0.19055  -0.976   0.3366  
#ERQ_RE_norm_t1  0.06067    0.15711   0.386   0.7020  
#ERQ_EX_norm_t1 -0.31423    0.18528  -1.696   0.0999 .
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 1.102 on 31 degrees of freedom
#(5 observations deleted due to missingness)
#Multiple R-squared:  0.087,	Adjusted R-squared:  0.0281 
#F-statistic: 1.477 on 2 and 31 DF,  p-value: 0.2439



## Difference in ER strategy scores associated with differences in QoL metrics? ----------------------------------#

# Cognitive functioning diff ~ Emotion regulation diff 

man_cog_dif=manova(cbind(RTI_norm_dif,RVP_norm_dif,SOC_norm_dif,SSP_norm_dif) ~ 
                     age + STAI_norm_diff +
                     ERQ_RE_norm_dif + ERQ_SU_norm_dif + ERQ_EX_norm_dif + ERQ_PRO_norm_dif)
summary(man_cog_dif)
#                 Df  Pillai approx F num Df den Df Pr(>F)
#age               1 0.24628  1.47036      4     18 0.2525
#STAI_norm_diff    1 0.11658  0.59381      4     18 0.6716
#ERQ_RE_norm_dif   1 0.12044  0.61621      4     18 0.6566
#ERQ_SU_norm_dif   1 0.33303  2.24692      4     18 0.1044
#ERQ_EX_norm_dif   1 0.09053  0.44791      4     18 0.7726
#ERQ_PRO_norm_dif  1 0.01244  0.05669      4     18 0.9935
#Residuals        21  


# Affective functioning dif ~ Emotion regulation dif 

man_aff_dif=manova(cbind(PSWQ_norm_diff, STAI_norm_diff, LO_norm_dif) ~ 
                     ERQ_RE_norm_dif + ERQ_SU_norm_dif + ERQ_EX_norm_dif + ERQ_PRO_norm_dif)
summary(man_aff_dif)
#                 Df   Pillai approx F num Df den Df Pr(>F)
#ERQ_RE_norm_dif   1 0.079688  0.60612      3     21 0.6184
#ERQ_SU_norm_dif   1 0.141890  1.15746      3     21 0.3494
#ERQ_EX_norm_dif   1 0.073868  0.55832      3     21 0.6484
#ERQ_PRO_norm_dif  1 0.038177  0.27784      3     21 0.8407
#Residuals        23



## Post-operative associations -------------------------------------------------------------------#

# Cognitive functioning ~ Emotion regulation  

man_cog_post=manova(cbind(RTI_norm_t2,RVP_norm_t2,SOC_norm_t2,SSP_norm_t2) ~ 
                      age + STAI_norm_t2 +
                      ERQ_RE_norm_t2 + ERQ_SU_norm_t2 + ERQ_EX_norm_t2 + ERQ_PRO_norm_t2)
summary(man_cog_post)
#                Df  Pillai approx F num Df den Df   Pr(>F)   
#age              1 0.44915   4.4846      4     22 0.008418 **
#STAI_norm_t2     1 0.33544   2.7761      4     22 0.052365 . 
#ERQ_RE_norm_t2   1 0.11621   0.7232      4     22 0.585420   
#ERQ_SU_norm_t2   1 0.29131   2.2608      4     22 0.095223 . 
#ERQ_EX_norm_t2   1 0.29063   2.2533      4     22 0.096061 . 
#ERQ_PRO_norm_t2  1 0.12114   0.7581      4     22 0.563576   
#Residuals       25                                           


lm_RTI_post=lm(RTI_norm_t2 ~ age + STAI_norm_t2 + ERQ_SU_norm_t2 + ERQ_EX_norm_t2)
summary(lm_RTI_post)
#Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)   
#(Intercept)    -1.20575    1.69267  -0.712  0.48237   
#age             0.05099    0.02825   1.805  0.08225 . 
#STAI_norm_t2    0.87876    0.28346   3.100  0.00449 **
#ERQ_SU_norm_t2  0.38240    0.37951   1.008  0.32258   
#ERQ_EX_norm_t2  0.31805    0.38527   0.826  0.41632   
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1.789 on 27 degrees of freedom
#(7 observations deleted due to missingness)
#Multiple R-squared:  0.3217,	Adjusted R-squared:  0.2212 
#F-statistic: 3.201 on 4 and 27 DF,  p-value: 0.02834

lm_RVP_post=lm(RVP_norm_t2 ~ age + STAI_norm_t2 + ERQ_SU_norm_t2 + ERQ_EX_norm_t2)
summary(lm_RVP_post)
#Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)  
#(Intercept)    -0.19242    1.35522  -0.142   0.8881  
#age            -0.01482    0.02262  -0.655   0.5179  
#STAI_norm_t2   -0.59017    0.22695  -2.600   0.0149 *
#ERQ_SU_norm_t2 -0.49875    0.30385  -1.641   0.1123  
#ERQ_EX_norm_t2 -0.33918    0.30847  -1.100   0.2812  
#---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1.433 on 27 degrees of freedom
#(7 observations deleted due to missingness)
#Multiple R-squared:  0.2484,	Adjusted R-squared:  0.1371 
#F-statistic: 2.231 on 4 and 27 DF,  p-value: 0.09213

lm_SOC_post=lm(SOC_norm_t2 ~ age + STAI_norm_t2 + ERQ_SU_norm_t2 + ERQ_EX_norm_t2)
summary(lm_SOC_post)
#Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)   
#(Intercept)     0.89098    0.74781   1.191  0.24385   
#age            -0.01749    0.01248  -1.401  0.17247   
#STAI_norm_t2   -0.20928    0.12523  -1.671  0.10625   
#ERQ_SU_norm_t2 -0.52084    0.16766  -3.106  0.00442 **
#ERQ_EX_norm_t2 -0.35136    0.17021  -2.064  0.04873 * 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.7905 on 27 degrees of freedom
#(7 observations deleted due to missingness)
#Multiple R-squared:  0.3564,	Adjusted R-squared:  0.2611 
#F-statistic: 3.738 on 4 and 27 DF,  p-value: 0.01515
confint(lm_SOC_post)
#ERQ_SU_norm_t2 -0.86485646 -0.176825119
#ERQ_EX_norm_t2 -0.70060246 -0.002111316

lm_SSP_post=lm(SSP_norm_t2 ~ age + STAI_norm_t2 + ERQ_SU_norm_t2 + ERQ_EX_norm_t2)
summary(lm_SSP_post)
#Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)   
#(Intercept)     2.54333    0.96353   2.640  0.01362 * 
#age            -0.05212    0.01608  -3.241  0.00316 **
#STAI_norm_t2   -0.41698    0.16136  -2.584  0.01549 * 
#ERQ_SU_norm_t2 -0.15671    0.21603  -0.725  0.47443   
#ERQ_EX_norm_t2 -0.07184    0.21931  -0.328  0.74576   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1.018 on 27 degrees of freedom
#(7 observations deleted due to missingness)
#Multiple R-squared:  0.3801,	Adjusted R-squared:  0.2883 
#F-statistic: 4.139 on 4 and 27 DF,  p-value: 0.009632



# Affective functioning ~ Emotion regulation  

man_aff_post=manova(cbind(PSWQ_norm_t2, STAI_norm_t2, LO_norm_t2) ~ 
                      ERQ_RE_norm_t2 + ERQ_SU_norm_t2 + ERQ_EX_norm_t2 + ERQ_PRO_norm_t2)
summary(man_aff_post)
#                Df   Pillai approx F num Df den Df  Pr(>F)  
#ERQ_RE_norm_t2   1 0.253845  2.83504      3     25 0.05857 .
#ERQ_SU_norm_t2   1 0.226516  2.44043      3     25 0.08791 .
#ERQ_EX_norm_t2   1 0.238509  2.61011      3     25 0.07374 .
#ERQ_PRO_norm_t2  1 0.028478  0.24428      3     25 0.86459  
#Residuals       27    

lm_PSWQ_post=lm(PSWQ_norm_t2 ~ ERQ_RE_norm_t2 + ERQ_SU_norm_t2 + ERQ_EX_norm_t2)
summary(lm_PSWQ_post)
#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)   
#(Intercept)     0.02393    0.18826   0.127  0.89976   
#ERQ_RE_norm_t2 -0.52061    0.16077  -3.238  0.00309 **
#ERQ_SU_norm_t2  0.38362    0.23207   1.653  0.10949   
#ERQ_EX_norm_t2  0.61769    0.22714   2.719  0.01110 * 
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.9724 on 28 degrees of freedom
#(7 observations deleted due to missingness)
#Multiple R-squared:  0.3213,	Adjusted R-squared:  0.2486 
#F-statistic: 4.418 on 3 and 28 DF,  p-value: 0.01153
confint(lm_PSWQ_post)
#                     2.5 %     97.5 %
#(Intercept)    -0.36170770  0.4095712
#ERQ_RE_norm_t2 -0.84992769 -0.1912888
#ERQ_EX_norm_t2  0.15241419  1.0829566

lm_STAI_post=lm(STAI_norm_t2 ~ ERQ_RE_norm_t2 + ERQ_SU_norm_t2 + ERQ_EX_norm_t2)
summary(lm_STAI_post)
#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    -0.91605    0.21302  -4.300 0.000187 ***
#ERQ_RE_norm_t2 -0.40979    0.18191  -2.253 0.032299 *  
#ERQ_SU_norm_t2  0.02247    0.26259   0.086 0.932408    
#ERQ_EX_norm_t2  0.18153    0.25700   0.706 0.485816    
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 1.1 on 28 degrees of freedom
#(7 observations deleted due to missingness)
#Multiple R-squared:  0.1915,	Adjusted R-squared:  0.1048 
#F-statistic:  2.21 on 3 and 28 DF,  p-value: 0.109
confint(lm_STAI_post)


lm_soc_post=lm(LO_norm_t2 ~ ERQ_RE_norm_t2 + ERQ_SU_norm_t2 + ERQ_EX_norm_t2)
summary(lm_soc_post)
#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)  
#(Intercept)     0.17215    0.22633   0.761   0.4532  
#ERQ_RE_norm_t2 -0.24586    0.19327  -1.272   0.2138  
#ERQ_SU_norm_t2  0.56308    0.27899   2.018   0.0532 .
#ERQ_EX_norm_t2 -0.02043    0.27306  -0.075   0.9409  
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 1.169 on 28 degrees of freedom
#(7 observations deleted due to missingness)
#Multiple R-squared:  0.1654,	Adjusted R-squared:  0.07593 
#F-statistic: 1.849 on 3 and 28 DF,  p-value: 0.1612


# Save dataset with ALL variables

write.table(x=results, file="output_Rev1_v1.csv", quote=TRUE, sep=';', dec='.', row.names=FALSE)



## Investigate associations SOC-SU & SOC-EX without outlier

results <- read.csv(file="output_Rev1_v1.csv", header=TRUE, sep=";")
str(results)

results=within(results, {
  subID=as.character(subID)
  date_t1=as.Date(date_t1, "%d/%m/%y")
  date_t2=as.Date(date_t2, "%d/%m/%y")
  group=factor(group, ordered=TRUE, levels=c('CON', 'MEN', 'GLI'))
})
attach(results)
results$SOC_resnorm_t2[subID=='PAT01']<-NA
results$SOC_norm_t2[subID=='PAT01']<-NA
results$SOC_res_t2[subID=='PAT01']<-NA
results$SOC_prob_minmoves_t2[subID=='PAT01']<-NA
detach(results); attach(results)

# Redo regression analyses
lm_SOC_post=lm(SOC_norm_t2 ~ age + STAI_norm_t2 + ERQ_SU_norm_t2 + ERQ_EX_norm_t2)
summary(lm_SOC_post)
#Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)   
#(Intercept)     0.86746    0.69055   1.256   0.2202  
#age            -0.01495    0.01157  -1.292   0.2078  
#STAI_norm_t2   -0.23386    0.11609  -2.014   0.0544 .
#ERQ_SU_norm_t2 -0.25595    0.19064  -1.343   0.1910  
#ERQ_EX_norm_t2 -0.12342    0.18402  -0.671   0.5083

#--> association only in patients, removing outlier
lm_SOC_postPAT=lm(SOC_norm_t2[12:39] ~ age[12:39] + STAI_norm_t2[12:39] + 
                    ERQ_SU_norm_t2[12:39] + ERQ_EX_norm_t2[12:39])
summary(lm_SOC_postPAT)
#                      Estimate Std. Error t value Pr(>|t|)  
#(Intercept)            0.68260    0.65260   1.046   0.3111  
#age[12:39]            -0.01249    0.01117  -1.119   0.2799  
#STAI_norm_t2[12:39]   -0.14283    0.11074  -1.290   0.2154  
#ERQ_SU_norm_t2[12:39] -0.55209    0.27286  -2.023   0.0601 .
#ERQ_EX_norm_t2[12:39] -0.08477    0.25447  -0.333   0.7434

#--> association only in patients, with outlier
lm_SOC_postPAT=lm(SOC_norm_t2[12:39] ~ age[12:39] + STAI_norm_t2[12:39] + 
                    ERQ_SU_norm_t2[12:39] + ERQ_EX_norm_t2[12:39])
summary(lm_SOC_postPAT)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)            0.75910    0.66331   1.144   0.2683    
#age[12:39]            -0.01481    0.01125  -1.316   0.2057    
#STAI_norm_t2[12:39]   -0.09248    0.10594  -0.873   0.3948    
#ERQ_SU_norm_t2[12:39] -0.83969    0.16439  -5.108 8.75e-05 ***
#ERQ_EX_norm_t2[12:39] -0.33612    0.16988  -1.979   0.0643 . 

# Post-hoc correlation analyses
## SOC - Suppression:

cor.test(SOC_resnorm_t2,ERQ_SU_norm_t2)
#	Pearson's product-moment correlation
#
#data:  results_outlierRM$SOC_resnorm_t2 and results_outlierRM$ERQ_SU_norm_t2
#t = -1.008, df = 29, p-value = 0.3218
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.5053760  0.1822381
#sample estimates:
#  cor 
#-0.1839825
# --> association no longer significant when outlier is removed


results_outlierIncl <- read.csv(file="output_Rev1_v1.csv", header=TRUE, sep=";")
cor.test(results_outlierIncl$SOC_resnorm_t2,results_outlierIncl$ERQ_SU_norm_t2)
#	Pearson's product-moment correlation
#
#data:  results$SOC_resnorm_t2 and results$ERQ_SU_norm_t2
#t = -2.4433, df = 30, p-value = 0.02065
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.66204111 -0.06841931
#sample estimates:
#  cor 
#-0.4073939
# --> significant association with outlier


cor.test(SOC_resnorm_t2[12:39],ERQ_SU_norm_t2[12:39])
#	Pearson's product-moment correlation
#
#data:  results_outlierRM$SOC_resnorm_t2[12:39] and results_outlierRM$ERQ_SU_norm_t2[12:39]
#t = -2.8773, df = 19, p-value = 0.009647
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.7938091 -0.1564166
#sample estimates:
#  cor 
#-0.5509046 
# --> association is different in patients vs. caregivers! 
# --> for patients, it is negative (significant)


cor.test(SOC_resnorm_t2[1:11],ERQ_SU_norm_t2[1:11])
#Pearson's product-moment correlation
#
#data:  results_outlierRM$SOC_resnorm_t2[1:11] and results_outlierRM$ERQ_SU_norm_t2[1:11]
#t = 1.1998, df = 8, p-value = 0.2645
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.3170739  0.8188124
#sample estimates:
#      cor 
#0.3905138 
# --> for caregivers, it is positive (not significant)



## SOC - Expression:

cor.test(SOC_resnorm_t2,ERQ_EX_norm_t2)
#	Pearson's product-moment correlation
#
#data:  results_outlierRM$SOC_resnorm_t2 and results_outlierRM$ERQ_EX_norm_t2
#t = -0.10459, df = 29, p-value = 0.9174
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.3712037  0.3372428
#sample estimates:
#  cor 
#-0.01941766
# --> association not significant when outlier is removed

cor.test(results_outlierIncl$SOC_resnorm_t2,results_outlierIncl$ERQ_EX_norm_t2)
#	Pearson's product-moment correlation
#
#data:  results$SOC_resnorm_t2 and results$ERQ_EX_norm_t2
#t = -1.0526, df = 30, p-value = 0.3009
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.5042411  0.1712317
#sample estimates:
#  cor 
#-0.1887309 
# --> non-significant association with outlier


cor.test(SOC_resnorm_t2[12:39],ERQ_EX_norm_t2[12:39])
#	Pearson's product-moment correlation
#
#data:  results_outlierRM$SOC_resnorm_t2[12:39] and results_outlierRM$ERQ_EX_norm_t2[12:39]
#t = 1.6049, df = 19, p-value = 0.125
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.1012866  0.6763202
#sample estimates:
#  cor 
#0.3455072 
# --> association is different in patients vs. caregivers! 
# --> for patients, no association

cor.test(SOC_resnorm_t2[1:11],ERQ_EX_norm_t2[1:11])
#	Pearson's product-moment correlation
#
#data:  results_outlierRM$SOC_resnorm_t2[1:11] and results_outlierRM$ERQ_EX_norm_t2[1:11]
#t = -2.1385, df = 8, p-value = 0.06493
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.89345484  0.04277265
#sample estimates:
#  cor 
#-0.6030955 
# --> for caregivers, it is negative (not significant)
