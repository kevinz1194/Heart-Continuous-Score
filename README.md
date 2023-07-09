# Heart-Continuous-Score

Code files corresponding to the manuscript *Predicting Death without Transplantation in Adult Heart Transplant Candidates: Developing and Validating the US Candidate Risk Score (US-CRS)* by by Zhang, Narang, et al.

All files are written and tested using R 4.2.3. 

Scientific Registry of Transplant Recipient (SRTR) data files are required to run these files, which are restricted under a data usage agreement. 
A premade dataset generated from SRTR data is also used; details provided at https://github.com/kevinlazenby/heart_data_pipeline.

Files should be run in this order:

### 1. 6week_death_data_prep.Rmd

Creates 14-day interval (blocked) dataset from SRTR data, including 6-week mortality outcome for patients not transplanted. Checks against original source dataset, as well as pre- and post- blocking, are performed.

Updated 7/8/2023: Added.


### 2. elastic_net_model.R

Sample iteration of elastic net models with parameter tuning on alpha and lambda. Requires large amount of computational resources. Optional.
Estimated runtime of several hours on a high-memory node.

Updated 7/8/2023: Added.   


### 3. xgboost_mem_vars_model.R

Sample iteration of gradient-boosted models with parameter tuning. Requires large amount of computational resources. Optional.
Estimated runtime of several days on a high-memory node.

Updated 7/8/2023: Added.   


### 4. model_results.Rmd

Runs logistic regression models, creates predicted results, and AUC curves. 

Updated 7/8/2023: Added.   
