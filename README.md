# EmotionRegulation_QoL
Code used for analyses in manuscript "The interplay between emotion regulation, emotional well‐being, and cognitive functioning in brain tumor patients and their caregivers: An exploratory study" (Aerts et al. 2019, Psycho-Oncology,  https://doi.org/10.1002/pon.5195).

(1) Preparation & descriptive analyses: For this part we imputed the missing data points that were present before surgery (see Methods section of paper for more information).
* Code_DescriptiveAnalyses_NA-impute.R
  
(2) Regression analyses: For this part we did not impute the pre-operative missing data points, but this did not change the main conclusions from the paper as the interesting associations (regardless of imputation) were found after surgery, and no missing values were present post-operatively.
* Code_RegressionAnalyses_NA-no-impute.R
* Code_RegressionAnalyses_NA-no-impute_plots.Rmd
* plotcorr.R (function used in Rmd file above)
