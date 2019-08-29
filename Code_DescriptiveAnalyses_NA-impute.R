###################################################################################################################
###                                                                                                             ###
###                 The effect of emotion regulation on quality of life in brain tumor patients                 ###
###               ==============================================================================                ###
###                                                                                                             ###
### Created by Hannelore Aerts & Tineke Van Vrekhem                                                             ###
### Date last update: 20/03/2019                                                                                ###
### Psycho-Oncology Revision 1, version 2:                                                                      ###
###   - repeat v1, but now also imputing missing variables                                                      ###
### --> This version (with imputation of missing values) was used for descriptive analyses                      ###
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
#library(car) 
#library(Hmisc)


# Missing data:

# PAT02T1 has missing value for RVP_A_t1 
# --> impute with predicted score from regression on post-op RVP scores (r=0.72)
cor(RVP_A_t1,RVP_A_t2,use='pairwise.complete.obs')
lm_RVP=lm(RVP_A_t1~RVP_A_t2); summary(lm_RVP)
preds=predict(lm_RVP,data.frame(RVP_A_t1))
results$RVP_A_t1[13]=round(preds[[13]],2)
detach(results);attach(results)
rm(lm_RVP);rm()

# CON01T1, CON02T1, PAT01T1, PAT02T1 & PAT03T1 have missing value for ERQ_PRO_t1 and ERQ_EX_t1
# --> impute with predicted score from regression on post-op ER scores!
cor(ERQ_PRO_t1, ERQ_PRO_t2,use='pairwise.complete.obs')
lm_PRO=lm(ERQ_PRO_t1~ERQ_PRO_t2); summary(lm_PRO)
preds=predict(lm_PRO,data.frame(ERQ_PRO_t1))
results$ERQ_PRO_t1[c(1:2,12:14)]=unname(round(preds[c(1:2,12:14)]))
detach(results);attach(results)
rm(lm_PRO);rm(preds)

cor(ERQ_EX_t1, ERQ_EX_t2,use='pairwise.complete.obs')
lm_EX=lm(ERQ_EX_t1~ERQ_EX_t2); summary(lm_EX)
preds=predict(lm_EX,data.frame(ERQ_EX_t1))
results$ERQ_EX_t1[c(1:2,12:14)]=unname(round(preds[c(1:2,12:14)]))
detach(results);attach(results)
rm(lm_EX); rm(preds)

# Post-op data
results_postop=results[!is.na(date_t2),]


# Group (PAT vs. Caregiver) identifyer:
results$group2[results$group=="CON"]="caregiver"
results$group2[results$group=="MEN"]="patient"
results$group2[results$group=="GLI"]="patient"
results$group2=factor(results$group2)
detach(results); attach(results)


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


# Post
aggregate(results_postop$age, list(results_postop$group), mean)
#  Group.1        x
#1     CON 59.60000
#2     MEN 60.86667
#3     GLI 50.71429
vars=aggregate(results_postop$age, list(results_postop$group), var); sqrt(vars$x)
#[1] 10.31935 11.52554 11.65782
rm(vars)
summary(aov(age~group, data=results_postop))



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

rm(results_postop)



###################################################################################################################
###                                           PART 2: QoL DESCRIPTIVES                                          ###
###################################################################################################################

# 1: Cognition ---------------------------------------------------------------------------------------------------#

# Regress out important confounding effects of CANTAB scores 

#-pre

lm_RTI_t1 = lm(RTI_fiveRT_t1 ~ STAI_t1 + age)
#summary(lm_RTI_t1) 
RTI_res_t1=lm_RTI_t1$residuals
shapiro.test(RTI_res_t1) 
results$RTI_res_t1 = c(RTI_res_t1[1:21],NA,RTI_res_t1[22:38])
rm(lm_RTI_t1); rm(RTI_res_t1)

lm_RVP_t1 = lm(RVP_A_t1 ~ STAI_t1 + age)
#summary(lm_RVP_t1) 
RVP_res_t1=lm_RVP_t1$residuals
shapiro.test(RVP_res_t1) 
results$RVP_res_t1 = c(RVP_res_t1[1:21],NA,RVP_res_t1[22:38])
rm(lm_RVP_t1); rm(RVP_res_t1)

lm_SOC_t1 = lm(SOC_prob_minmoves_t1 ~ STAI_t1 + age)
#summary(lm_SOC_t1) 
SOC_res_t1=lm_SOC_t1$residuals
shapiro.test(SOC_res_t1) 
results$SOC_res_t1 = c(SOC_res_t1[1:21],NA,SOC_res_t1[22:38])
rm(lm_SOC_t1); rm(SOC_res_t1)

lm_SSP_t1 = lm(SSP_spanlength_t1 ~ STAI_t1 + age)
#summary(lm_SSP_t1) 
SSP_res_t1=lm_SSP_t1$residuals
shapiro.test(SSP_res_t1) 
results$SSP_res_t1 = c(SSP_res_t1[1:21],NA,SSP_res_t1[22:38])
rm(lm_SSP_t1); rm(SSP_res_t1)


#-post

lm_RTI_t2 = lm(RTI_fiveRT_t2 ~ + STAI_t2 + age)
#summary(lm_RTI_t2) 
RTI_res_t2=lm_RTI_t2$residuals
shapiro.test(RTI_res_t2)
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
summary(aov(RVP_resnorm_t1~group)) #F(2,35)=1.48,p=0.24
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
t.test(RVP_resnorm_dif) #t(31)=-0.12, p=0.90, 95% CI=[-0.38 0.34] 
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
TukeyHSD(aov(PSWQ_norm_t1~group))
#diff        lwr       upr     p adj
#MEN-CON -0.3369880 -1.3623902 0.6884141 0.7034183
#GLI-CON  0.5870249 -0.5429133 1.7169630 0.4210763
#GLI-MEN  0.9240129 -0.1013892 1.9494150 0.0843238
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
summary(aov(ERQ_SU_norm_t1~group)) #F(2,36)=0.63, p=0.54
summary(aov(ERQ_EX_norm_t1~group)) #F(2,35)=0.25, p=0.78
summary(aov(ERQ_PRO_norm_t1~group)) #F(2,35)=0.52, p=0.60

# Post
summary(aov(ERQ_RE_norm_t2~group)) #F(2,29)=1.32, p=0.28
summary(aov(ERQ_SU_norm_t2~group)) #F(2,29)=3.43, p=0.0459
boxplot(ERQ_SU_norm_t2~group)
TukeyHSD(aov(ERQ_SU_norm_t2~group))
#              diff        lwr         upr     p adj
#MEN-CON  0.4972452 -0.3654538  1.35994408 0.3423597
#GLI-CON -0.5067165 -1.5480991  0.53466612 0.4617350
#GLI-MEN -1.0039617 -1.9712402 -0.03668307 0.0406932
summary(aov(ERQ_EX_norm_t2~group)) #F(2,29)=1.63, p=0.21
summary(aov(ERQ_PRO_norm_t2~group)) #F(2,29)=0.18, p=0.84

# Diff
results$ERQ_RE_norm_diff=ERQ_RE_norm_t2-ERQ_RE_norm_t1
results$ERQ_SU_norm_diff=ERQ_SU_norm_t2-ERQ_SU_norm_t1
results$ERQ_EX_norm_diff=ERQ_EX_norm_t2-ERQ_EX_norm_t1
results$ERQ_PRO_norm_diff=ERQ_PRO_norm_t2-ERQ_PRO_norm_t1
detach(results);attach(results)
t.test(ERQ_RE_norm_diff) #t(31)=-0.45, p=0.65
t.test(ERQ_SU_norm_diff) #t(31)=0.27, p=0.79
t.test(ERQ_EX_norm_diff) #t(31)=-1.06, p=0.30
t.test(ERQ_PRO_norm_diff) #t(31)=-1.82, p=0.0778
par(mfrow=c(1,3))
plot(ERQ_PRO_norm_t1~group);plot(ERQ_PRO_norm_t2~group); plot(ERQ_PRO_norm_diff~group)
summary(aov(ERQ_PRO_norm_diff~group)) #F(2,29)=2.77, p=0.0794
TukeyHSD(aov(ERQ_PRO_norm_diff~group))
#diff       lwr        upr     p adj
#MEN-CON  0.5656854 -1.136475 2.26784601 0.6933776
#GLI-CON -1.2525892 -3.307304 0.80212587 0.3031706
#GLI-MEN -1.8182746 -3.726778 0.09022842 0.0642125


###################################################################################################################
###                             PART 4: Associations between ER and QoL indices                                 ###
###################################################################################################################

## Association between ER and QoL indices 
colnames(results)
correlates_t1=results[,c(50:53,44:46,,)]
head(correlates_t1)
colnames(correlates_t1)=c('ER_RE','ER_SU','ER_PRO','ER_EX','PSWQ','STAI','LO','RTI res','RVP res',
                          'SSP res','SOC res')
correlates_t2=results[,c(82:85,35,23,25,57:60)]
head(correlates_t2)
colnames(correlates_t2)=c('ER_RE','ER_SU','ER_PRO','ER_EX','PSWQ','STAI','LO','RTI res','RVP res',
                          'SSP res','SOC res')
cor_t1=rcorr(as.matrix(correlates_t1))
cor_t2=rcorr(as.matrix(correlates_t2))

png('ER_covariates_PATonly_noMotivation_p05.png', width=1200)
par(mfrow=c(1,2))
corrplot(cor(correlates_t1, use="pairwise"), type="lower", method="color", tl.col="black",
         diag=F, addCoef.col="black", p.mat=cor_t1$P, sig.level=.05); title('Before surgery')
corrplot(cor(correlates_t2, use="pairwise"), type="lower", method="color", tl.col="black",
         diag=F, addCoef.col="black", p.mat=cor_t2$P, sig.level=.05); title('After surgery')
dev.off()
# --> important covariates: affective & social functioning (cognitive not so much)
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
#                Df  Pillai approx F num Df den Df  Pr(>F)  
#age              1 0.34396   3.5391      4     27 0.01906 *
#STAI_norm_t1     1 0.25843   2.3523      4     27 0.07929 .
#ERQ_RE_norm_t1   1 0.02571   0.1781      4     27 0.94775  
#ERQ_SU_norm_t1   1 0.08677   0.6413      4     27 0.63764  
#ERQ_EX_norm_t1   1 0.05663   0.4052      4     27 0.80319  
#ERQ_PRO_norm_t1  1 0.04234   0.2985      4     27 0.87636  
#Residuals       30  
#--> qualitatively the same as without using imputation



# Affective functioning t1 ~ Emotion regulation t1 

man_aff_pre=manova(cbind(PSWQ_norm_t1, STAI_norm_t1, LO_norm_t1) ~ 
                     ERQ_RE_norm_t1 + ERQ_SU_norm_t1 + ERQ_EX_norm_t1 + ERQ_PRO_norm_t1)
summary(man_aff_pre)
#                Df  Pillai approx F num Df den Df   Pr(>F)   
#ERQ_RE_norm_t1   1 0.15700   1.8624      3     30 0.157277   
#ERQ_SU_norm_t1   1 0.09009   0.9901      3     30 0.410760   
#ERQ_EX_norm_t1   1 0.34620   5.2952      3     30 0.004744 **
#ERQ_PRO_norm_t1  1 0.14791   1.7358      3     30 0.180818   
#Residuals       32 
#--> RE no longer significant


lm_PSWQ_pre=lm(PSWQ_norm_t1~ERQ_EX_norm_t1)
summary(lm_PSWQ_pre)
#               Estimate Std. Error t value Pr(>|t|)  
#(Intercept)     0.02622    0.17555   0.149   0.8821  
#ERQ_EX_norm_t1  0.32502    0.16992   1.913   0.0638 .
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 1.078 on 36 degrees of freedom
#(1 observation deleted due to missingness)
#Multiple R-squared:  0.09226,	Adjusted R-squared:  0.06704 
#F-statistic: 3.659 on 1 and 36 DF,  p-value: 0.06375


lm_STAI_pre=lm(STAI_norm_t1~ERQ_EX_norm_t1)
summary(lm_STAI_pre)
#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
#(Intercept)     0.19111    0.22919   0.834    0.410
#ERQ_EX_norm_t1  0.08441    0.23521   0.359    0.722
#
#Residual standard error: 1.377 on 35 degrees of freedom
#(2 observations deleted due to missingness)
#Multiple R-squared:  0.003666,	Adjusted R-squared:  -0.0248 
#F-statistic: 0.1288 on 1 and 35 DF,  p-value: 0.7218


lm_soc_pre=lm(LO_norm_t1 ~ ERQ_EX_norm_t1)
summary(lm_soc_pre)

#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)  
#(Intercept)     -0.1353     0.1695  -0.798   0.4300  
#ERQ_EX_norm_t1  -0.2825     0.1640  -1.722   0.0936 .
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 1.041 on 36 degrees of freedom
#(1 observation deleted due to missingness)
#Multiple R-squared:  0.07614,	Adjusted R-squared:  0.05048 
#F-statistic: 2.967 on 1 and 36 DF,  p-value: 0.09356

rm(lm_PSWQ_pre); rm(lm_soc_pre); rm(lm_STAI_pre); rm(man_aff_pre); rm(man_cog_pre); rm(results_postop)


## Post-operative associations -------------------------------------------------------------------#
# --> exactly the same, since no post-operative missing data were imputed!

write.table(x=results, file="output_Rev1_v2.csv", quote=TRUE, sep=';', dec='.', row.names=FALSE)
