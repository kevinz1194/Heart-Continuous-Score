Code files corresponding to the manuscript *Predicting Death without Transplantation in Adult Heart Transplant Candidates: Developing and Validating the US Candidate Risk Score (US-CRS)* by by Zhang, Narang, et al. <br />

All files are written and tested using R 4.3.1.

Scientific Registry of Transplant Recipient (SRTR) data files are required to run these files, which are restricted under a data usage agreement. A premade dataset generated from SRTR data (**status_changes_pubsaf2303.csv**) is also used; details provided at https://github.com/kevinlazenby/heart_data_pipeline.

Files should be run in this order:

### 1. 6week_death_data_prep.Rmd
Creates 14-day interval (blocked) dataset from SRTR data, including 6-week mortality outcome for patients not transplanted. Checks against original source dataset, as well as pre- and post- blocking, are performed.

Datasets required: 

**status_changes_pubsaf2303.csv** <br />
**cand_thor.sas7bdat** <br />
**JustFormHRStat1.sas7bdat**  <br />
**JustFormHRStat2.sas7bdat** <br />
**JustFormHRStat3.sas7bdat**  <br />
**JustFormHRStat4.sas7bdat** <br />

Updated 7/9/2023: Added.

Updated 7/11/2023: Imputation procedure changed, code cleaned to fully account for edge cases.

Updated 8/6/2023: BNP type added.


### 2. elastic_net_model.R
Sample iteration of elastic net models with parameter tuning on alpha and lambda. Requires large amount of computational resources. Optional. Estimated runtime of several hours on a high-memory node.

Updated 7/4/2023: Added.


### 3. xgboost_mem_vars_model.R
Sample iteration of gradient-boosted models with parameter tuning. Requires large amount of computational resources. Optional. Estimated runtime of several days on a high-memory node.

Updated 7/4/2023: Added.


### 4. model_results.Rmd
Runs logistic regression models, creates predicted results and the main figures for the blocked (14-day interval) dataset.

Updated 7/9/2023: Added.

Updated 7/10/2023: Figure colors changed.

Updated 7/11/2023: Main figure structure revised, now includes calibration and facet plots of score.

Updated 7/13/2023: Colors changed, confidence intervals added.

Updated 8/9/2023: Models add NT-proBNP, new French-CRS score.


### 5. figures_unblocked.Rmd
Figures from the full, unblocked (no 14-day intervals) dataset. 

Updated 7/9/2023: Added.

Updated 7/11/2023: Unblocking method changed to every justification form change.

Updated 7/13/2023: Colors changed.

Updated 8/10/2023: Accounts for NT-proBNP, new French-CRS score.
