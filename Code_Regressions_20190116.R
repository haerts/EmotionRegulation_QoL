###################################################################################################################
###                                                                                                             ###
###                 The effect of emotion regulation on quality of life in brain tumor patients                 ###
###               ==============================================================================                ###
###                                             PART 2: Regression models                                       ###
###                                                                                                             ###
### Created by Hannelore Aerts & Tineke Van Vrekhem                                                             ###
###################################################################################################################


### Read in data and prepare for analyses ------------------------------------------------------------------------#

results <- read.csv(file="RESULTS_ALLpatients_afterprep.csv", header=TRUE, sep=";")
str(results)

results=within(results, {
  subID=as.character(subID)
  date_t1=as.Date(date_t1)
  date_t2=as.Date(date_t2)
  group=factor(group, ordered=TRUE, levels=c('MEN', 'GLI'))
})

attach(results)


## Pre-operative associations ------------------------------------------------------------------------------------#

# Cognitive functioning t1 ~ Emotion regulation t1 

man_cog_pre=manova(cbind(RTI_norm_t1,RVP_norm_t1,SOC_norm_t1,SSP_norm_t1) ~ 
                    motivation_t1 + age + STAI_norm_t1 + 
                      ERQ_RE_norm_t1 + ERQ_SU_norm_t1 + ERQ_EX_norm_t1 + ERQ_PRO_norm_t1)
summary(man_cog_pre)
#                Df  Pillai approx F num Df den Df Pr(>F)
#motivation_t1    1 0.50740   4.3777      4     17 0.01293 *
#age              1 0.22377   1.2252      4     17 0.33698  
#STAI_norm_t1     1 0.19774   1.0475      4     17 0.41204  
#ERQ_RE_norm_t1   1 0.03626   0.1599      4     17 0.95573  
#ERQ_SU_norm_t1   1 0.02081   0.0903      4     17 0.98424  
#ERQ_EX_norm_t1   1 0.08999   0.4203      4     17 0.79181  
#ERQ_PRO_norm_t1  1 0.08294   0.3844      4     17 0.81678  
#Residuals       20  



# Affective functioning t1 ~ Emotion regulation t1 

man_aff_pre=manova(cbind(PSWQ_norm_t1, STAI_norm_t1) ~ 
                    ERQ_RE_norm_t1 + ERQ_SU_norm_t1 + ERQ_EX_norm_t1 + ERQ_PRO_norm_t1)
summary(man_aff_pre)
#                Df   Pillai approx F num Df den Df Pr(>F)
#ERQ_RE_norm_t1   1 0.157011   2.0488      2     22 0.1528
#ERQ_SU_norm_t1   1 0.098396   1.2005      2     22 0.3200
#ERQ_EX_norm_t1   1 0.157785   2.0608      2     22 0.1512
#ERQ_PRO_norm_t1  1 0.180591   2.4243      2     22 0.1118
#Residuals       23 



# Social functioning t1 ~ Emotion regulation t1 

lm_soc_pre=lm(LO_norm_t1 ~ ERQ_RE_norm_t1 + ERQ_SU_norm_t1 + ERQ_EX_norm_t1 + ERQ_PRO_norm_t1)
summary(lm_soc_pre)
#                  Estimate Std. Error t value Pr(>|t|)
#(Intercept)      9.646e-16  1.954e-01   0.000    1.000
#ERQ_RE_norm_t1  -1.760e-02  2.114e-01  -0.083    0.934
#ERQ_SU_norm_t1  -3.929e-02  2.211e-01  -0.178    0.860
#ERQ_EX_norm_t1  -3.171e-01  2.555e-01  -1.241    0.227
#ERQ_PRO_norm_t1  1.265e-02  2.547e-01   0.050    0.961



## Difference in ER strategy scores associated with differences in QoL metrics? ----------------------------------#

# Cognitive functioning diff ~ Emotion regulation diff 

man_cog_dif=manova(cbind(RTI_norm_dif,RVP_norm_dif,SOC_norm_dif,SSP_norm_dif) ~ 
                     motivation_diff + age + STAI_norm_diff +
                    ERQ_RE_norm_dif + ERQ_SU_norm_dif + ERQ_EX_norm_dif + ERQ_PRO_norm_dif)
summary(man_cog_dif)
#                 Df  Pillai approx F num Df den Df   Pr(>F)   
#motivation_diff   1 0.25335   0.9331      4     11 0.479904   
#age               1 0.08609   0.2590      4     11 0.898085   
#STAI_norm_diff    1 0.25091   0.9211      4     11 0.485872   
#ERQ_RE_norm_dif   1 0.13540   0.4307      4     11 0.783793   
#ERQ_SU_norm_dif   1 0.75639   8.5387      4     11 0.002185 **
#ERQ_EX_norm_dif   1 0.54275   3.2642      4     11 0.053862 . 
#ERQ_PRO_norm_dif  1 0.27774   1.0575      4     11 0.422195   
#Residuals        14    

lm_RTI_dif=lm(RTI_norm_dif ~ motivation_diff + age + STAI_norm_diff +
                ERQ_SU_norm_dif + ERQ_EX_norm_dif)
summary(lm_RTI_dif) 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)      1.62407    2.03388   0.799    0.436
#motivation_diff -0.10069    1.53352  -0.066    0.948
#age             -0.01062    0.03755  -0.283    0.781
#STAI_norm_diff   0.57151    0.49401   1.157    0.264
#ERQ_SU_norm_dif  0.17501    0.49170   0.356    0.727
#ERQ_EX_norm_dif -0.20373    0.74010  -0.275    0.787
#
#Residual standard error: 1.582 on 16 degrees of freedom
#(6 observations deleted due to missingness)
#Multiple R-squared:  0.1376,	Adjusted R-squared:  -0.1319 
#F-statistic: 0.5105 on 5 and 16 DF,  p-value: 0.7644

lm_RVP_dif=lm(RVP_norm_dif ~ motivation_diff + age + STAI_norm_diff +
                ERQ_SU_norm_dif + ERQ_EX_norm_dif)
summary(lm_RVP_dif) 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)     -0.576984   0.944400  -0.611    0.550
#motivation_diff  0.388387   0.712066   0.545    0.593
#age              0.009091   0.017435   0.521    0.609
#STAI_norm_diff  -0.032450   0.229386  -0.141    0.889
#ERQ_SU_norm_dif  0.020597   0.228311   0.090    0.929
#ERQ_EX_norm_dif  0.119689   0.343654   0.348    0.732
#
#Residual standard error: 0.7346 on 16 degrees of freedom
#(6 observations deleted due to missingness)
#Multiple R-squared:  0.09567,	Adjusted R-squared:  -0.1869 
#F-statistic: 0.3385 on 5 and 16 DF,  p-value: 0.8821

lm_SOC_dif=lm(SOC_norm_dif ~ motivation_diff + age + STAI_norm_diff +
                ERQ_SU_norm_dif + ERQ_EX_norm_dif)
summary(lm_SOC_dif) 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     -1.54040    0.80949  -1.903 0.075201 .  
#motivation_diff -1.42864    0.61034  -2.341 0.032527 *  
#age              0.04754    0.01494   3.181 0.005803 ** 
#STAI_norm_diff   0.89172    0.19662   4.535 0.000338 ***
#ERQ_SU_norm_dif -1.47354    0.19570  -7.530 1.21e-06 ***
#ERQ_EX_norm_dif -1.22975    0.29456  -4.175 0.000715 ***
#  ---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.6297 on 16 degrees of freedom
#(6 observations deleted due to missingness)
#Multiple R-squared:  0.793,	Adjusted R-squared:  0.7283 
#F-statistic: 12.26 on 5 and 16 DF,  p-value: 5.257e-05
confint(lm_SOC_dif)
#                   2.5 %      97.5 %
#(Intercept)     -3.25643052  0.17563609
#motivation_diff -2.72250394 -0.13477067
#age              0.01585989  0.07922027
#STAI_norm_diff   0.47490902  1.30852436
#ERQ_SU_norm_dif -1.88839823 -1.05868847
#ERQ_EX_norm_dif -1.85418940 -0.60530814

lm_SSP_dif=lm(SSP_norm_dif ~ motivation_diff + age + STAI_norm_diff +
                ERQ_SU_norm_dif + ERQ_EX_norm_dif)
summary(lm_SSP_dif) 
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)
#(Intercept)      0.49592    1.05280   0.471    0.644
#motivation_diff -0.98289    0.79380  -1.238    0.233
#age             -0.00377    0.01944  -0.194    0.849
#STAI_norm_diff   0.04885    0.25572   0.191    0.851
#ERQ_SU_norm_dif  0.07022    0.25452   0.276    0.786
#ERQ_EX_norm_dif -0.17008    0.38310  -0.444    0.663
#
#Residual standard error: 0.8189 on 16 degrees of freedom
#(6 observations deleted due to missingness)
#Multiple R-squared:  0.1576,	Adjusted R-squared:  -0.1057 
#F-statistic: 0.5985 on 5 and 16 DF,  p-value: 0.7018



# Affective functioning dif ~ Emotion regulation dif 

man_aff_dif=manova(cbind(PSWQ_norm_diff, STAI_norm_diff) ~ 
                    ERQ_RE_norm_dif + ERQ_SU_norm_dif + ERQ_EX_norm_dif + ERQ_PRO_norm_dif)
summary(man_aff_dif)
#                 Df   Pillai approx F num Df den Df Pr(>F)
#ERQ_RE_norm_dif   1 0.047630  0.40010      2     16 0.6768
#ERQ_SU_norm_dif   1 0.117167  1.06174      2     16 0.3690
#ERQ_EX_norm_dif   1 0.021445  0.17532      2     16 0.8408
#ERQ_PRO_norm_dif  1 0.028788  0.23713      2     16 0.7916
#Residuals        17   


# Social functioning dif ~ Emotion regulation dif 

lm_soc_dif=lm(LO_norm_dif ~ ERQ_RE_norm_dif + ERQ_SU_norm_dif + ERQ_EX_norm_dif + ERQ_PRO_norm_dif)
summary(lm_soc_dif)
#                 Estimate Std. Error t value Pr(>|t|)  
#(Intercept)       0.53515    0.24940   2.146   0.0466 *
#ERQ_RE_norm_dif  -0.03006    0.34747  -0.086   0.9321  
#ERQ_SU_norm_dif  -0.07054    0.29213  -0.241   0.8121  
#ERQ_EX_norm_dif  -0.02067    0.44820  -0.046   0.9637  
#ERQ_PRO_norm_dif  0.08796    0.40305   0.218   0.8298



## Post-operative associations -------------------------------------------------------------------#

# Cognitive functioning ~ Emotion regulation  

man_cog_post=manova(cbind(RTI_norm_t2,RVP_norm_t2,SOC_norm_t2,SSP_norm_t2) ~ 
                     motivation_t2 + age + STAI_norm_t2 +
                     ERQ_RE_norm_t2 + ERQ_SU_norm_t2 + ERQ_EX_norm_t2 + ERQ_PRO_norm_t2)
summary(man_cog_post)
#                 Df  Pillai approx F num Df den Df   Pr(>F)   
#motivation_t2    1 0.71774   6.9927      4     11 0.004710 **
#age              1 0.48931   2.6348      4     11 0.091632 . 
#STAI_norm_t2     1 0.45192   2.2675      4     11 0.127620   
#ERQ_RE_norm_t2   1 0.07321   0.2172      4     11 0.923315   
#ERQ_SU_norm_t2   1 0.67498   5.7109      4     11 0.009745 **
#ERQ_EX_norm_t2   1 0.54196   3.2538      4     11 0.054319 . 
#ERQ_PRO_norm_t2  1 0.50626   2.8198      4     11 0.078023 . 
#Residuals       14                                           


lm_RTI_post=lm(RTI_norm_t2 ~ motivation_t2 + age + STAI_norm_t2 +
                 ERQ_SU_norm_t2 + ERQ_EX_norm_t2 + ERQ_PRO_norm_t2)
summary(lm_RTI_post)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      9.33009    2.30131   4.054  0.00104 ** 
#motivation_t2   -6.66464    1.43137  -4.656  0.00031 ***
#age              0.01473    0.02016   0.731  0.47620    
#STAI_norm_t2     0.69675    0.30753   2.266  0.03871 *  
#ERQ_SU_norm_t2  -0.60528    0.25329  -2.390  0.03043 *  
#ERQ_EX_norm_t2  -1.99864    0.57570  -3.472  0.00342 ** 
#ERQ_PRO_norm_t2  1.40318    0.46154   3.040  0.00827 ** 
#  ---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 1.075 on 15 degrees of freedom
#(6 observations deleted due to missingness)
#Multiple R-squared:  0.7795,	Adjusted R-squared:  0.6914 
#F-statistic:  8.84 on 6 and 15 DF,  p-value: 0.0003116
confint(lm_RTI_post)
#                      2.5 %      97.5 %
#(Intercept)      4.42497253 14.23521074
#motivation_t2   -9.71553737 -3.61373741
#age             -0.02823521  0.05769547
#STAI_norm_t2     0.04126549  1.35223928
#ERQ_SU_norm_t2  -1.14514148 -0.06540925
#ERQ_EX_norm_t2  -3.22572142 -0.77155082
#ERQ_PRO_norm_t2  0.41943166  2.38692628

lm_RVP_post=lm(RVP_norm_t2 ~ motivation_t2 + age + STAI_norm_t2 +
                 ERQ_SU_norm_t2 + ERQ_EX_norm_t2 + ERQ_PRO_norm_t2)
summary(lm_RVP_post)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)     -5.883527   2.142993  -2.745  0.01502 * 
#motivation_t2    3.964089   1.332905   2.974  0.00946 **
#age              0.007695   0.018771   0.410  0.68765   
#STAI_norm_t2    -0.144655   0.286375  -0.505  0.62081   
#ERQ_SU_norm_t2   0.039505   0.235862   0.167  0.86922   
#ERQ_EX_norm_t2   0.453448   0.536100   0.846  0.41094   
#ERQ_PRO_norm_t2 -0.298999   0.429788  -0.696  0.49726   
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 1.001 on 15 degrees of freedom
#(6 observations deleted due to missingness)
#Multiple R-squared:  0.5498,	Adjusted R-squared:  0.3698 
#F-statistic: 3.053 on 6 and 15 DF,  p-value: 0.03711


lm_SOC_post=lm(SOC_norm_t2 ~ motivation_t2 + age + STAI_norm_t2 +
                      ERQ_SU_norm_t2 + ERQ_EX_norm_t2 + ERQ_PRO_norm_t2)
summary(lm_SOC_post)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)     -0.333572   1.488109  -0.224  0.82566   
#motivation_t2    0.556098   0.925578   0.601  0.55694   
#age             -0.011868   0.013035  -0.911  0.37695   
#STAI_norm_t2    -0.100678   0.198861  -0.506  0.62002   
#ERQ_SU_norm_t2  -0.543819   0.163784  -3.320  0.00466 **
#ERQ_EX_norm_t2  -0.009837   0.372272  -0.026  0.97927   
#ERQ_PRO_norm_t2 -0.362627   0.298448  -1.215  0.24313   
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.6949 on 15 degrees of freedom
#(6 observations deleted due to missingness)
#Multiple R-squared:  0.6713,	Adjusted R-squared:  0.5399 
#F-statistic: 5.107 on 6 and 15 DF,  p-value: 0.004844
confint(lm_SOC_post)
#                      2.5 %      97.5 %
#(Intercept)     -3.50540060  2.83825630
#motivation_t2   -1.41672509  2.52892074
#age             -0.03965137  0.01591453
#STAI_norm_t2    -0.52453937  0.32318397
#ERQ_SU_norm_t2  -0.89291616 -0.19472202
#ERQ_EX_norm_t2  -0.80331546  0.78364053
#ERQ_PRO_norm_t2 -0.99875350  0.27350010

lm_SSP_post=lm(SSP_norm_t2 ~ motivation_t2 + age + STAI_norm_t2 +
                 ERQ_SU_norm_t2 + ERQ_EX_norm_t2 + ERQ_PRO_norm_t2)
summary(lm_SSP_post)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)     -1.18022    2.06843  -0.571   0.5767  
#motivation_t2    2.49309    1.28653   1.938   0.0717 .
#age             -0.03857    0.01812  -2.129   0.0502 .
#STAI_norm_t2    -0.21476    0.27641  -0.777   0.4493  
#ERQ_SU_norm_t2  -0.08853    0.22765  -0.389   0.7028  
#ERQ_EX_norm_t2  -0.02971    0.51745  -0.057   0.9550  
#ERQ_PRO_norm_t2  0.13247    0.41483   0.319   0.7539  
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.9659 on 15 degrees of freedom
#(6 observations deleted due to missingness)
#Multiple R-squared:  0.5365,	Adjusted R-squared:  0.3511 
#F-statistic: 2.894 on 6 and 15 DF,  p-value: 0.0444



# Affective functioning ~ Emotion regulation  

man_aff_post=manova(cbind(PSWQ_norm_t2, STAI_norm_t2) ~ 
                     ERQ_RE_norm_t2 + ERQ_SU_norm_t2 + ERQ_EX_norm_t2 + ERQ_PRO_norm_t2)
summary(man_aff_post)
#                 Df   Pillai approx F num Df den Df Pr(>F)
#ERQ_RE_norm_t2   1 0.31312   3.6469      2     16 0.04955 *
#ERQ_SU_norm_t2   1 0.00175   0.0141      2     16 0.98605  
#ERQ_EX_norm_t2   1 0.38810   5.0741      2     16 0.01965 *
#ERQ_PRO_norm_t2  1 0.02938   0.2422      2     16 0.78775  
#Residuals       17 

lm_PSWQ_post=lm(PSWQ_norm_t2 ~ ERQ_RE_norm_t2 + ERQ_EX_norm_t2)
summary(lm_PSWQ_post)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)    -0.08993    0.16689  -0.539  0.59624   
#ERQ_RE_norm_t2 -0.53111    0.17484  -3.038  0.00677 **
#ERQ_EX_norm_t2  0.52173    0.20264   2.575  0.01856 * 
#  ---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.7821 on 19 degrees of freedom
#(6 observations deleted due to missingness)
#Multiple R-squared:  0.4296,	Adjusted R-squared:  0.3695 
#F-statistic: 7.155 on 2 and 19 DF,  p-value: 0.004828
confint(lm_PSWQ_post)
#                     2.5 %     97.5 %
#(Intercept)    -0.43923786  0.2593766
#ERQ_RE_norm_t2 -0.89705581 -0.1651740
#ERQ_EX_norm_t2  0.09760627  0.9458463

lm_STAI_post=lm(STAI_norm_t2 ~ ERQ_RE_norm_t2 + ERQ_EX_norm_t2)
summary(lm_STAI_post)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     -0.8440     0.1895  -4.455 0.000271 ***
#ERQ_RE_norm_t2  -0.3035     0.1985  -1.529 0.142704    
#ERQ_EX_norm_t2   0.1300     0.2300   0.565 0.578468    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.8878 on 19 degrees of freedom
#(6 observations deleted due to missingness)
#Multiple R-squared:  0.1162,	Adjusted R-squared:  0.02316 
#F-statistic: 1.249 on 2 and 19 DF,  p-value: 0.3093



# Social functioning ~ Emotion regulation  

lm_soc_post=lm(LO_norm_t2 ~ ERQ_RE_norm_t2 + ERQ_SU_norm_t2 + ERQ_EX_norm_t2 + ERQ_PRO_norm_t2)
summary(lm_soc_post)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)      0.59310    0.26516   2.237   0.0390 *
#ERQ_RE_norm_t2  -0.65319    0.29125  -2.243   0.0385 *
#ERQ_SU_norm_t2   0.09667    0.21086   0.458   0.6524  
#ERQ_EX_norm_t2  -0.73716    0.48254  -1.528   0.1450  
#ERQ_PRO_norm_t2  0.70974    0.45717   1.552   0.1390  
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 1.097 on 17 degrees of freedom
#(6 observations deleted due to missingness)
#Multiple R-squared:  0.287,	Adjusted R-squared:  0.1193 
#F-statistic: 1.711 on 4 and 17 DF,  p-value: 0.1939
confint(lm_soc_post)
#                      2.5 %     97.5 %
#(Intercept)      0.03365304  1.1525493
#ERQ_RE_norm_t2  -1.26766805 -0.0387101
#ERQ_SU_norm_t2  -0.34820150  0.5415504
#ERQ_EX_norm_t2  -1.75524052  0.2809149
#ERQ_PRO_norm_t2 -0.25481354  1.6742891



### Plot significant associations --------------------------------------------------------------------------------#


# ER - Cognition difference

#png('ER_Cog_diff.png', width=1200, height=600)
par(mar=c(5,6,4,1)+.1, mfrow=c(1,2), oma=c(4,1,1,1))
idxvis=!is.na(ERQ_SU_norm_dif)
SOC_vis=SOC_resnorm_dif[idxvis]
SU_vis=ERQ_SU_norm_dif[idxvis]
plot(SOC_vis ~ SU_vis, pch=21, bg="gray", cex=2.2, cex.lab=2.4, cex.axis=1.8,
     xlab="Suppression difference", ylab="Planning accuracy difference")
legend(1,2.5, legend=paste('r =',round(cor(SOC_resnorm_dif, ERQ_SU_norm_dif, use='pairwise.complete.obs'),2)), 
       box.lty=0, cex=2.2)
model=(lm(SOC_vis ~ SU_vis))
myPredict=predict(model, data.frame(SU_vis), interval="confidence")
ix <- sort(SU_vis,index.return=T)$ix
lines(SU_vis[ix], myPredict[ix , 1], col="darkgray", lwd=2 )  
polygon(c(rev(SU_vis[ix]), SU_vis[ix]), c(rev(myPredict[ix,3]), myPredict[ix,2]), 
        col = rgb(0.7,0.7,0.7,0.4) , border = NA)
rm(list=c("model", "myPredict", "ix", 'idxvis','SU_vis','SOC_vis'))

idxvis=!is.na(ERQ_EX_norm_dif)
SOC_vis=SOC_resnorm_dif[idxvis]
EX_vis=ERQ_EX_norm_dif[idxvis]
plot(SOC_vis ~ EX_vis, pch=21, bg="gray", cex=2.2, cex.lab=2.4, cex.axis=1.8,
     xlab="Expression difference", ylab="Planning accuracy difference")
legend(-1,2.5, legend=paste('r =',round(cor(SOC_resnorm_dif, ERQ_EX_norm_dif, use='pairwise.complete.obs'),2)), 
       box.lty=0, cex=2.2)
model=(lm(SOC_vis ~ EX_vis))
myPredict=predict(model, data.frame(EX_vis), interval="confidence")
ix <- sort(EX_vis,index.return=T)$ix
lines(EX_vis[ix], myPredict[ix , 1], col="darkgray", lwd=2 )  
polygon(c(rev(EX_vis[ix]), EX_vis[ix]), c(rev(myPredict[ix,3]), myPredict[ix,2]), 
        col = rgb(0.7,0.7,0.7,0.4) , border = NA)
rm(list=c("model", "myPredict", "ix", 'idxvis','EX_vis','SOC_vis'))

#dev.off()


# ER - Cognition postop

#png('ER_Cog_postop.png', width=1000, height=1000)
par(mar=c(5,6,4,1)+.1, mfrow=c(2,2), oma=c(4,1,1,1))

idxvis=!is.na(ERQ_EX_norm_t2)
RTI_vis=RTI_resnorm_t2[idxvis]
EX_vis=ERQ_EX_norm_t2[idxvis]
plot(RTI_vis ~ EX_vis, pch=21, bg="gray", cex=2.2, cex.lab=2.4, cex.axis=1.8,
     xlab="Expression after surgery", ylab="Reaction time after surgery")
legend(0.2,3.2, legend=paste('r =',round(cor(RTI_resnorm_t2, ERQ_EX_norm_t2, use='pairwise.complete.obs'),2)), 
       box.lty=0, cex=2.2)
model=(lm(RTI_vis ~ EX_vis))
myPredict=predict(model, data.frame(EX_vis), interval="confidence")
ix <- sort(EX_vis,index.return=T)$ix
lines(EX_vis[ix], myPredict[ix , 1], col="darkgray", lwd=2 )  
polygon(c(rev(EX_vis[ix]), EX_vis[ix]), c(rev(myPredict[ix,3]), myPredict[ix,2]), 
        col = rgb(0.7,0.7,0.7,0.4) , border = NA)
rm(list=c("model", "myPredict", "ix", 'idxvis','EX_vis','RTI_vis'))

idxvis=!is.na(ERQ_PRO_norm_t2)
RTI_vis=RTI_resnorm_t2[idxvis]
PRO_vis=ERQ_PRO_norm_t2[idxvis]
plot(RTI_vis ~ PRO_vis, pch=21, bg="gray", cex=2.2, cex.lab=2.4, cex.axis=1.8,
     xlab="Processing after surgery", ylab="Reaction time after surgery")
legend(0.3,3.2, legend=paste('r =',round(cor(RTI_resnorm_t2, ERQ_PRO_norm_t2, use='pairwise.complete.obs'),2)), 
       box.lty=0, cex=2.2)
model=(lm(RTI_vis ~ PRO_vis))
myPredict=predict(model, data.frame(PRO_vis), interval="confidence")
ix <- sort(PRO_vis,index.return=T)$ix
lines(PRO_vis[ix], myPredict[ix , 1], col="darkgray", lwd=2 )  
polygon(c(rev(PRO_vis[ix]), PRO_vis[ix]), c(rev(myPredict[ix,3]), myPredict[ix,2]), 
        col = rgb(0.7,0.7,0.7,0.4) , border = NA)
rm(list=c("model", "myPredict", "ix", 'idxvis','PRO_vis','RTI_vis'))

idxvis=!is.na(ERQ_SU_norm_t2)
RTI_vis=RTI_resnorm_t2[idxvis]
SU_vis=ERQ_SU_norm_t2[idxvis]
plot(RTI_vis ~ SU_vis, pch=21, bg="gray", cex=2.2, cex.lab=2.4, cex.axis=1.8,
     xlab="Suppression after surgery", ylab="Reaction time after surgery")
legend(1.4,3.2, legend=paste('r =',round(cor(RTI_resnorm_t2, ERQ_SU_norm_t2, use='pairwise.complete.obs'),2)), 
       box.lty=0, cex=2.2)
model=(lm(RTI_vis ~ SU_vis))
myPredict=predict(model, data.frame(SU_vis), interval="confidence")
ix <- sort(SU_vis,index.return=T)$ix
lines(SU_vis[ix], myPredict[ix , 1], col="darkgray", lwd=2 )  
polygon(c(rev(SU_vis[ix]), SU_vis[ix]), c(rev(myPredict[ix,3]), myPredict[ix,2]), 
        col = rgb(0.7,0.7,0.7,0.4) , border = NA)
rm(list=c("model", "myPredict", "ix", 'idxvis','SU_vis','RTI_vis'))

idxvis=!is.na(ERQ_SU_norm_t2)
SOC_vis=SOC_resnorm_t2[idxvis]
SU_vis=ERQ_SU_norm_t2[idxvis]
plot(SOC_vis ~ SU_vis, pch=21, bg="gray", cex=2.2, cex.lab=2.4, cex.axis=1.8,
     xlab="Suppression after surgery", ylab="Planning accuracy after surgery")
legend(1.3,1.6, legend=paste('r =',round(cor(SOC_resnorm_t2, ERQ_SU_norm_t2, use='pairwise.complete.obs'),2)), 
       box.lty=0, cex=2.2)
model=(lm(SOC_vis ~ SU_vis))
myPredict=predict(model, data.frame(SU_vis), interval="confidence")
ix <- sort(SU_vis,index.return=T)$ix
lines(SU_vis[ix], myPredict[ix , 1], col="darkgray", lwd=2 )  
polygon(c(rev(SU_vis[ix]), SU_vis[ix]), c(rev(myPredict[ix,3]), myPredict[ix,2]), 
        col = rgb(0.7,0.7,0.7,0.4) , border = NA)
rm(list=c("model", "myPredict", "ix", 'idxvis','SU_vis','SOC_vis'))

#dev.off()



# ER - Affect postop

#png('ER_Aff_postop.png', width=1200, height=600)
par(mar=c(5,6,4,1)+.1, mfrow=c(1,2), oma=c(4,1,1,1))
idxvis=!is.na(ERQ_RE_norm_t2)
PSWQ_vis=PSWQ_norm_t2[idxvis]
ER_vis=ERQ_RE_norm_t2[idxvis]
plot(PSWQ_vis ~ ER_vis, pch=21, bg="gray", cex=2.2, cex.lab=2.4, cex.axis=1.8,
     xlab="Reappraisal after surgery", ylab="Worry after surgery")
legend(0.6,1.8, legend=paste('r =',round(cor(PSWQ_norm_t2, ERQ_RE_norm_t2, use='pairwise.complete.obs'),2)), 
       box.lty=0, cex=2.2)
model=(lm(PSWQ_vis ~ ER_vis))
myPredict=predict(model, data.frame(ER_vis), interval="confidence")
ix <- sort(ER_vis,index.return=T)$ix
lines(ER_vis[ix], myPredict[ix , 1], col="darkgray", lwd=2 )  
polygon(c(rev(ER_vis[ix]), ER_vis[ix]), c(rev(myPredict[ix,3]), myPredict[ix,2]), 
        col = rgb(0.7,0.7,0.7,0.4) , border = NA)
rm(list=c("model", "myPredict", "ix", "idxvis","PSWQ_vis","ER_vis"))

idxvis=!is.na(ERQ_EX_norm_t2)
PSWQ_vis=PSWQ_norm_t2[idxvis]
ER_vis=ERQ_EX_norm_t2[idxvis]
plot(PSWQ_vis ~ ER_vis, pch=21, bg="gray", cex=2.2, cex.lab=2.4, cex.axis=1.8,
     xlab="Expression after surgery", ylab="Worry after surgery")
legend(-2.1,1.8, legend=paste('r =',round(cor(PSWQ_norm_t2, ERQ_EX_norm_t2, use='pairwise.complete.obs'),2)), 
       box.lty=0, cex=2.2)
model=(lm(PSWQ_vis ~ ER_vis))
myPredict=predict(model, data.frame(ER_vis), interval="confidence")
ix <- sort(ER_vis,index.return=T)$ix
lines(ER_vis[ix], myPredict[ix , 1], col="darkgray", lwd=2 )  
polygon(c(rev(ER_vis[ix]), ER_vis[ix]), c(rev(myPredict[ix,3]), myPredict[ix,2]), 
        col = rgb(0.7,0.7,0.7,0.4) , border = NA)
rm(list=c("model", "myPredict", "ix", "idxvis","PSWQ_vis","ER_vis"))

#dev.off()