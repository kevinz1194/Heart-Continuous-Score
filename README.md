Code files corresponding to the manuscript *Development and Validation of a Risk Score Predicting Death Without Transplant in Adult Heart Transplant Candidates* by Zhang, Narang, et al, available at: https://jamanetwork.com/journals/jama/article-abstract/2814884. <br />

All files are written and tested using R 4.3.1.

Kevin Lazenby (coauthor and collaborator) performed a reproducibility check on 10/14/2023.

Scientific Registry of Transplant Recipient (SRTR) data files are required to run these files, which are restricted under a data usage agreement. A premade dataset generated from SRTR data (**status_changes_pubsaf2303.csv**) is also used; details provided at https://github.com/kevinlazenby/heart_data_pipeline.

An app for calculating US-CRS scores is available at https://kevinz1194.shinyapps.io/US-CRS_Score. The corresponding code file is app.R. 

Files should be run in this order:

### 1. 6week_death_data_prep.Rmd
Creates 14-day interval (blocked) dataset from SRTR data, including 6-week mortality outcome for patients not transplanted. Checks against original source dataset, as well as pre- and post- blocking, are performed.

Datasets required: 

**status_changes_pubsaf2303.csv** (helper dataset) <br />
**cand_thor.sas7bdat** (SRTR dataset) <br />
**JustFormHRStat1.sas7bdat** (SRTR dataset) <br />
**JustFormHRStat2.sas7bdat** (SRTR dataset) <br />
**JustFormHRStat3.sas7bdat** (SRTR dataset) <br />
**JustFormHRStat4.sas7bdat** (SRTR dataset) <br />

Updated 7/9/2023: Added.

Updated 7/11/2023: Imputation procedure changed, code cleaned to fully account for edge cases.

Updated 8/6/2023: BNP type (NT-proBNP) accounted for.

Updated 8/14/2023: Added lower bound for sodium, and censoring date now uses the maximum of removal date, last active date, or last inactive date.

Updated 10/15/2023: Typo with calculation of short-term MCS is fixed, error with IV dosage changed, new loop for maximum time.

Updated 11/24/2023: Imputation of bilirubin and albumin added.

Updated 1/19/2024: One-line change to directory so that it is clear that it should be adjusted based on user's file location. 

Updated 4/16/2024: Reworked definition of durable LVAD.

### 2. elastic_net_model.R
Sample iteration of elastic net models with parameter tuning on alpha and lambda. Requires large amount of computational resources. Optional. Estimated runtime of several hours on a high-memory node.

Updated 7/4/2023: Added.

Updated 8/11/2023: Models add NT-proBNP.

Updated 10/15/2023: Dataset version number corrected.


### 3. xgboost_mem_vars_model.R
Sample iteration of gradient-boosted models with parameter tuning. Requires large amount of computational resources. Optional. Estimated runtime of several days on a high-memory node.

Updated 7/4/2023: Added.

Updated 8/11/2023: Models add NT-proBNP.

Updated 10/15/2023: Dataset version number corrected.


### 4. model_results.Rmd
Runs logistic regression models, creates predicted results and the main figures for the blocked (14-day interval) dataset.

Updated 7/9/2023: Added.

Updated 7/10/2023: Figure colors changed.

Updated 7/11/2023: Main figure structure revised, now includes calibration and facet plots of score.

Updated 7/13/2023: Colors changed, confidence intervals added.

Updated 8/11/2023: Models add NT-proBNP, new French-CRS score added.

Updated 8/18/2023: Sensitivity analysis using MCS without balloon pump added. Model changed to drop hemodynamics.

Updated 8/22/2023: Make additional predictions on model with hemodynamics.

Updated 10/8/2023: AUC, in addition to AIC, is used for model selection. An additional set of models without BNP is added. Gender and race disparities are examined as a sensitivity analysis. Non-informative censoring is examined. An additional mixed-effects model is added. A sensitivity analysis with a different temporal split is added.

Updated 10/14/2023: Code for specific thresholds of sensitivity and specificity are added.

Updated 10/15/2023: Minor change to legend of AUC curves.

Updated 10/22/2023: Sensitivity and specificity code revised.

Updated 11/28/2023: Robust standard errors clustered by patient are now used. Time-dependent AUC curves added. Some calculations use only the first interval. 

Updated 11/29/2023: Time-dependent AUC curves fixed, specific intervals added. Sensitivity analyses for 2-week is added.

Updated 1/19/2024: One-line change to directory so that it is clear that it should be adjusted based on user's file location. 


### 5. results_continuous.Rmd
Figures and discrimination, calibration from the full, continuous (no 14-day intervals) dataset. 

Updated 7/9/2023: Added.

Updated 7/11/2023: Unblocking method changed to every justification form change.

Updated 7/13/2023: Colors changed.

Updated 8/11/2023: Accounts for NT-proBNP, new French-CRS score.

Updated 8/18/2023: Name changed, updated to work with new data. Cox PH model added.

Updated 8/22/2023: Added MCS components to Table 1, added c-index calculations for US-CRS model with hemodynamics.

Updated 11/25/2023: Section with Harrell's c removed. Integrated AUC added.

Updated 8/25/2023: IABP, durable and perc LVAD variables added.

Updated 10/14/2023: Updated notes on Harrell's c, also draws an AUC curve. Generalized c-index added.

Updated 10/27/2023: Name changed to results_continuous, bootstrapping and time-dependent AUC's added. 

Updated 1/19/2024: One-line change to directory so that it is clear that it should be adjusted based on user's file location. 


