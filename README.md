---
title: "Model Results"
output:
  html_document:
    toc: true
    df_print: paged
authors: Kevin Zhang
---

<!-- 1. Set up dataset with generated cardiac variables (Line 32) -->
<!-- 2. Set up matrices for predictions (Line 132) -->
<!-- 3. Run selected logistic regression models and create coefficient plots (Line 583) -->
<!-- 4. Obtain machine learning model results (Line 970) -->
<!-- 5. Make predictions on test dataset (Line 1028) -->
<!-- 6. Create AUC curves (Line 1069) -->
<!-- 7. Variable importance plots from gradient-boosted models/Supplement 7 (Line 1139) -->



```{r, include=F, warning=F, echo=F, comment=NA}
rm(list=ls()); invisible(gc())
if (!require('pacman')) {install.packages('pacman')}
library(pacman)
pacman::p_load(tidyverse, splines, knitr, kableExtra, glmnet, caret, gtsummary, pROC, xgboost, survival, DescTools)
knitr::opts_knit$set(root.dir = 'C:/Users/kevinz94/Desktop/DiscreteData')
```


---------------------------------


# 1. Set up dataset with generated cardiac variables
Starting dataset requires the interval dataset from **6week_death_data_prep.Rmd**.


```{r, warning=F, comment=NA}
load('./intervals_v16_impute_6wkdeath.RData')


### Convert variable types to correct types
factor_vars <- c('diagnosis', 'diabetes', 'dialysis', 'IV_inotropes', 'dobutamine', 
                 'dopamine', 'epinephrine', 'milrinone', 'IABP', 'ECMO', 'BiVAD', 'LVAD', 'RVAD',
                 'MCSD_complication', 'life_arrhythmia', 'BiVAD_no_discharge', 
                 'temp_surg_LVAD', 'other_durable_MCSD', 'MCSD_malfunction', 'v_tach_fib', 
                 'perc_LVAD', 'MCSD_hemolysis', 'MCSD_rhf', 'MCSD_device_infx', 
                 'MCSD_bleed', 'MCSD_aortic_insf', 'angina', 
                 
                 'temp_surg_MCS_ever', 'BiVAD_no_discharge_ever', 'IV_inotropes_ever', 
                 'IABP_ever', 'ECMO_ever', 'BiVAD_ever', 'LVAD_ever', 'RVAD_ever')

numeric_vars <- c('interval', 'interval_start', 'interval_stop', 'age', 
                  'systolicBP', 'diastolicBP', 'PASP', 'PADP', 'heart_rate', 'cardiac_output', 
                  'central_venous_pressure', 'arterial_lactate', 
                  'PCWP', 'hemoglobin', 'albumin', 'bilirubin', 'creatinine', 'sodium', 
                  'AST', 'BNP', 'BUN', 'INR', 'LDH', 'kidney_deficit', 'eGFR',
                  
                  'sodium_max', 'creatinine_max', 
                  'bilirubin_max', 'albumin_max', 'arterial_lactate_max', 'BUN_max', 'AST_max', 
                  'INR_max', 'BNP_max', 'LDH_max', 'sodium_min', 'creatinine_min', 
                  'bilirubin_min', 'albumin_min', 'arterial_lactate_min', 'BUN_min', 'AST_min', 
                  'INR_min', 'BNP_min', 'LDH_min', 'sodium_slope', 'creatinine_slope', 'bilirubin_slope', 
                  'albumin_slope', 'arterial_lactate_slope', 'BUN_slope', 'AST_slope', 
                  'INR_slope', 'BNP_slope', 'LDH_slope')


df_intervals[ ,factor_vars] <- lapply(df_intervals[ ,factor_vars], as.factor)
df_intervals[ ,numeric_vars] <- lapply(df_intervals[ ,numeric_vars], as.numeric)
df_intervals$death6week <- as.factor(df_intervals$death6week)


### Change specific baseline factor levels 
df_intervals$diagnosis <- relevel(factor(df_intervals$diagnosis), ref = 'Dilated_CM')
df_intervals$dobutamine <- relevel(factor(df_intervals$dobutamine), ref = 'Not High')
df_intervals$dopamine <- relevel(factor(df_intervals$dopamine), ref = 'Not High')
df_intervals$epinephrine <- relevel(factor(df_intervals$epinephrine), ref = 'Not High')
df_intervals$milrinone <- relevel(factor(df_intervals$milrinone), ref = 'Not High')



### Generated cardiac variables
df_intervals <- df_intervals %>%
  mutate(
    cpo = (1/541) * cardiac_output * (((2/3)*systolicBP) + ((1/3)*diastolicBP)),
    api = (systolicBP - diastolicBP) / PCWP,
    papi = (PASP - PADP) / central_venous_pressure) %>%
  
  mutate(cpo = as.numeric(cpo),
         api = as.numeric(api),
         papi = as.numeric(papi))


# Fix cases where PCWP or central venous pressure is 0 (division by 0)
# Lower bound of PAPi set to 1
df_intervals$api[df_intervals$PCWP == 0] <- NA

df_intervals$papi[df_intervals$central_venous_pressure == 0] <- 1
df_intervals$papi[df_intervals$papi < 1 & !is.na(df_intervals$papi)] <- 1


### MCS variables
df_intervals <- df_intervals %>%
  mutate(short_MCS_French = 
           case_when(
             (ECMO == '1' & status == '1') | IABP == '1' ~ 1,
             TRUE ~ 0)) %>% 
  mutate(short_MCS_ever = 
           case_when(
             ECMO_ever == '1' | temp_surg_ever == '1' | BiVAD_no_discharge_ever == '1' ~ 1,
             TRUE ~ 0))



### Split into training and testing
set.seed(200)
center_ids_train <- sample(x = unique(df_intervals$CenterId), 
                           size = ceiling(0.7*length(unique(df_intervals$CenterId))), 
                           replace = F)
df_intervals$train_test <-  'Test'
df_intervals$train_test[df_intervals$CenterId %in% center_ids_train] <- 'Train'

save(center_ids_train, file = './train_ids.RData')
df_intervals$CenterId <- NULL


save(df_intervals, file = './df_intervals_setup.RData') ### Save a cleaner copy
```


---------------------------------


# 2. Set up matrices for predictions

### Training matrices
```{r, warning=F, comment=NA}
interval_dummies_train <- predict(
  caret::dummyVars(~ diagnosis + dobutamine + dopamine + epinephrine + milrinone, 
                   data = df_intervals[df_intervals$train_test == 'Train', ],
                   sep = NULL,
                   fullrank = T), 
  newdata = df_intervals[df_intervals$train_test == 'Train', ])
interval_dummies_train <- as.data.frame(interval_dummies_train)
interval_dummies_train <- interval_dummies_train %>% 
  select(-c(diagnosisDilated_CM, `dobutamineNot High`, `dopamineNot High`, 
            `epinephrineNot High`, `milrinoneNot High`))



mat_train <- data.frame(cbind(
  df_intervals[df_intervals$train_test == 'Train', ]$death6week,
  bs(df_intervals[df_intervals$train_test == 'Train', ]$interval, knots=3), 
  bs(df_intervals[df_intervals$train_test == 'Train', ]$age, knots=3),
  df_intervals[df_intervals$train_test == 'Train', ]$diabetes, 
  df_intervals[df_intervals$train_test == 'Train', ]$dialysis, 
  interval_dummies_train, 
  df_intervals[df_intervals$train_test == 'Train', ]$IABP,
  df_intervals[df_intervals$train_test == 'Train', ]$ECMO, 
  df_intervals[df_intervals$train_test == 'Train', ]$LVAD, 
  df_intervals[df_intervals$train_test == 'Train', ]$MCSD_complication, 
  df_intervals[df_intervals$train_test == 'Train', ]$systolicBP,
  df_intervals[df_intervals$train_test == 'Train', ]$diastolicBP, 
  df_intervals[df_intervals$train_test == 'Train', ]$PASP, 
  df_intervals[df_intervals$train_test == 'Train', ]$PADP, 
  df_intervals[df_intervals$train_test == 'Train', ]$heart_rate,
  df_intervals[df_intervals$train_test == 'Train', ]$cardiac_output, 
  df_intervals[df_intervals$train_test == 'Train', ]$central_venous_pressure, 
  df_intervals[df_intervals$train_test == 'Train', ]$arterial_lactate,
  df_intervals[df_intervals$train_test == 'Train', ]$cpo, 
  df_intervals[df_intervals$train_test == 'Train', ]$api, 
  df_intervals[df_intervals$train_test == 'Train', ]$papi, 
  df_intervals[df_intervals$train_test == 'Train', ]$hemoglobin, 
  df_intervals[df_intervals$train_test == 'Train', ]$albumin, 
  df_intervals[df_intervals$train_test == 'Train', ]$bilirubin,
  df_intervals[df_intervals$train_test == 'Train', ]$eGFR, 
  df_intervals[df_intervals$train_test == 'Train', ]$sodium, 
  df_intervals[df_intervals$train_test == 'Train', ]$AST, 
  df_intervals[df_intervals$train_test == 'Train', ]$BNP,
  df_intervals[df_intervals$train_test == 'Train', ]$BUN, 
  df_intervals[df_intervals$train_test == 'Train', ]$INR, 
  df_intervals[df_intervals$train_test == 'Train', ]$life_arrhythmia,
  df_intervals[df_intervals$train_test == 'Train', ]$BiVAD_no_discharge,
  df_intervals[df_intervals$train_test == 'Train', ]$temp_surg_LVAD,
  df_intervals[df_intervals$train_test == 'Train', ]$other_durable_MCSD,
  df_intervals[df_intervals$train_test == 'Train', ]$MCSD_malfunction,
  df_intervals[df_intervals$train_test == 'Train', ]$v_tach_fib,
  df_intervals[df_intervals$train_test == 'Train', ]$perc_LVAD,
  df_intervals[df_intervals$train_test == 'Train', ]$MCSD_hemolysis,
  df_intervals[df_intervals$train_test == 'Train', ]$MCSD_rhf,
  df_intervals[df_intervals$train_test == 'Train', ]$MCSD_device_infx,
  df_intervals[df_intervals$train_test == 'Train', ]$MCSD_bleed,
  df_intervals[df_intervals$train_test == 'Train', ]$MCSD_aortic_insf,
  df_intervals[df_intervals$train_test == 'Train', ]$angina))


mat_train_all <- cbind(mat_train,
                       df_intervals[df_intervals$train_test == 'Train', ]$temp_surg_MCS_ever,
                       df_intervals[df_intervals$train_test == 'Train', ]$BiVAD_no_discharge_ever,
                       df_intervals[df_intervals$train_test == 'Train', ]$IV_inotropes_ever,
                       df_intervals[df_intervals$train_test == 'Train', ]$IABP_ever,
                       df_intervals[df_intervals$train_test == 'Train', ]$ECMO_ever,
                       df_intervals[df_intervals$train_test == 'Train', ]$BiVAD_ever,
                       df_intervals[df_intervals$train_test == 'Train', ]$LVAD_ever,
                       df_intervals[df_intervals$train_test == 'Train', ]$RVAD_ever,
                       df_intervals[df_intervals$train_test == 'Train', ]$short_MCS_ever,
                       
                       df_intervals[df_intervals$train_test == 'Train', ]$sodium_min,
                       df_intervals[df_intervals$train_test == 'Train', ]$creatinine_min,
                       df_intervals[df_intervals$train_test == 'Train', ]$bilirubin_min,
                       df_intervals[df_intervals$train_test == 'Train', ]$albumin_min,
                       df_intervals[df_intervals$train_test == 'Train', ]$arterial_lactate_min,
                       df_intervals[df_intervals$train_test == 'Train', ]$BUN_min,
                       df_intervals[df_intervals$train_test == 'Train', ]$AST_min,
                       df_intervals[df_intervals$train_test == 'Train', ]$INR_min,
                       df_intervals[df_intervals$train_test == 'Train', ]$BNP_min,
                       
                       df_intervals[df_intervals$train_test == 'Train', ]$sodium_max,
                       df_intervals[df_intervals$train_test == 'Train', ]$creatinine_max,
                       df_intervals[df_intervals$train_test == 'Train', ]$bilirubin_max,
                       df_intervals[df_intervals$train_test == 'Train', ]$albumin_max,
                       df_intervals[df_intervals$train_test == 'Train', ]$arterial_lactate_max,
                       df_intervals[df_intervals$train_test == 'Train', ]$BUN_max,
                       df_intervals[df_intervals$train_test == 'Train', ]$AST_max,
                       df_intervals[df_intervals$train_test == 'Train', ]$INR_max,
                       df_intervals[df_intervals$train_test == 'Train', ]$BNP_max,
                       df_intervals[df_intervals$train_test == 'Train', ]$sodium_slope,
                       df_intervals[df_intervals$train_test == 'Train', ]$creatinine_slope,
                       df_intervals[df_intervals$train_test == 'Train', ]$bilirubin_slope,
                       df_intervals[df_intervals$train_test == 'Train', ]$albumin_slope,
                       df_intervals[df_intervals$train_test == 'Train', ]$arterial_lactate_slope,
                       df_intervals[df_intervals$train_test == 'Train', ]$BUN_slope,
                       df_intervals[df_intervals$train_test == 'Train', ]$AST_slope,
                       df_intervals[df_intervals$train_test == 'Train', ]$INR_slope,
                       df_intervals[df_intervals$train_test == 'Train', ]$BNP_slope)


names(mat_train) <- c('Death', 'Interval_1', 'Interval_2', 'Interval_3', 'Interval_4',
                      'Age10_1', 'Age10_2', 'Age10_3', 'Age10_4', 'Diabetes', 'Dialysis', 
                      'Diag_Amyloid', 'Diag_Congenital', 'Diag_Hypertrophic', 'Diag_Ischemic',
                      'Diag_Other', 'Diag_Retransplant', 'Diag_Restricted', 'Diag_Valvular',
                      'Dobutamine_High', 'Dopamine_High', 'Epinephrine_High', 'Milrinone_High', 
                      'IABP', 'ECMO', 'LVAD', 'MCSD_Complication', 'SystolicBP', 'DiastolicBP', 
                      'PASP', 'PADP', 'Heart_Rate10', 'Cardiac_Output', 'Central_Venous_Pressure',
                      'Arterial_Lactate', 'CPO', 'API', 'PAPI', 'Hemoglobin', 'Albumin', 'Bilirubin', 
                      'eGFR', 'Sodium', 'AST100', 'BNP100', 'BUN10', 'INR', 'Life_Arrhythmia',
                      'BiVAD_NoDischarge', 'TempSurgery_LVAD', 'Other_DurableMCSD', 'MCSD_Malfunction',
                      'VTach_Fib', 'Perc_LVAD', 'MCSD_Hemolysis', 'MCSD_Rhf', 'MCSD_DeviceInfection',
                      'MCSD_Bleed', 'MCSD_Aortic_Insufficent', 'Angina')
                          
                          
names(mat_train_all) <-  c('Death', 'Interval_1', 'Interval_2', 'Interval_3', 'Interval_4',
                           'Age10_1', 'Age10_2', 'Age10_3', 'Age10_4', 'Diabetes', 'Dialysis', 
                           'Diag_Amyloid', 'Diag_Congenital', 'Diag_Hypertrophic', 'Diag_Ischemic',
                           'Diag_Other', 'Diag_Retransplant', 'Diag_Restricted', 'Diag_Valvular',
                           'Dobutamine_High', 'Dopamine_High', 'Epinephrine_High', 'Milrinone_High', 
                           'IABP', 'ECMO', 'LVAD', 'MCSD_Complication', 'SystolicBP', 'DiastolicBP', 
                           'PASP', 'PADP', 'Heart_Rate10', 'Cardiac_Output', 'Central_Venous_Pressure',
                           'Arterial_Lactate', 'CPO', 'API', 'PAPI', 'Hemoglobin', 'Albumin', 'Bilirubin', 
                           'eGFR', 'Sodium', 'AST100', 'BNP100', 'BUN10', 'INR', 'Life_Arrhythmia',
                           'BiVAD_NoDischarge', 'TempSurgery_LVAD', 'Other_DurableMCSD', 'MCSD_Malfunction',
                           'VTach_Fib', 'Perc_LVAD', 'MCSD_Hemolysis', 'MCSD_Rhf', 'MCSD_DeviceInfection',
                           'MCSD_Bleed', 'MCSD_Aortic_Insufficent', 'Angina',
                          
                           'Temp_Surg_Ever', 'BiVAD_No_Discharge_Ever', 'IV_Inotropes_Ever', 'IABP_Ever', 
                           'ECMO_Ever', 'BiVAD_Ever', 'LVAD_Ever', 'RVAD_Ever', 'Short_MCS_Ever',
                           
                           'Sodium_Min', 'Creatinine_Min', 'Bilirubin_Min', 'Albumin_Min', 'Arterial_Lactate_Min',
                           'BUN10_Min', 'AST100_Min', 'INR_Min', 'BNP100_Min',
                           
                           'Sodium_Max', 'Creatinine_Max', 'Bilirubin_Max', 'Albumin_Max', 'Arterial_Lactate_Max',
                           'BUN10_Max', 'AST100_Max', 'INR_Max', 'BNP100_Max',
                           
                           'Sodium_Slope', 'Creatinine_Slope', 'Bilirubin_Slope', 'Albumin_Slope',
                           'Arterial_Lactate_Slope', 'BUN10_Slope', 'AST100_Slope', 'INR_Slope', 'BNP100_Slope')
```


### Testing matrices
```{r, warning=F, comment=NA}
## Testing matrix
interval_dummies_test <- predict(
  caret::dummyVars(~ diagnosis + dobutamine + dopamine + epinephrine + milrinone, 
                   data = df_intervals[df_intervals$train_test == 'Test', ],
                   sep = NULL,
                   fullrank = T), 
  newdata = df_intervals[df_intervals$train_test == 'Test', ])
interval_dummies_test <- as.data.frame(interval_dummies_test)
interval_dummies_test <- interval_dummies_test %>% 
  select(-c(diagnosisDilated_CM, `dobutamineNot High`, `dopamineNot High`, 
            `epinephrineNot High`, `milrinoneNot High`))



mat_test <- data.frame(cbind(
  df_intervals[df_intervals$train_test == 'Test', ]$death6week,
  bs(df_intervals[df_intervals$train_test == 'Test', ]$interval, knots=3), 
  bs(df_intervals[df_intervals$train_test == 'Test', ]$age, knots=3),
  df_intervals[df_intervals$train_test == 'Test', ]$diabetes, 
  df_intervals[df_intervals$train_test == 'Test', ]$dialysis, 
  interval_dummies_test, 
  df_intervals[df_intervals$train_test == 'Test', ]$IABP,
  df_intervals[df_intervals$train_test == 'Test', ]$ECMO, 
  df_intervals[df_intervals$train_test == 'Test', ]$LVAD, 
  df_intervals[df_intervals$train_test == 'Test', ]$MCSD_complication, 
  df_intervals[df_intervals$train_test == 'Test', ]$systolicBP,
  df_intervals[df_intervals$train_test == 'Test', ]$diastolicBP, 
  df_intervals[df_intervals$train_test == 'Test', ]$PASP, 
  df_intervals[df_intervals$train_test == 'Test', ]$PADP, 
  df_intervals[df_intervals$train_test == 'Test', ]$heart_rate,
  df_intervals[df_intervals$train_test == 'Test', ]$cardiac_output, 
  df_intervals[df_intervals$train_test == 'Test', ]$central_venous_pressure, 
  df_intervals[df_intervals$train_test == 'Test', ]$arterial_lactate,
  df_intervals[df_intervals$train_test == 'Test', ]$cpo, 
  df_intervals[df_intervals$train_test == 'Test', ]$api, 
  df_intervals[df_intervals$train_test == 'Test', ]$papi,
  df_intervals[df_intervals$train_test == 'Test', ]$hemoglobin, 
  df_intervals[df_intervals$train_test == 'Test', ]$albumin, 
  df_intervals[df_intervals$train_test == 'Test', ]$bilirubin,
  df_intervals[df_intervals$train_test == 'Test', ]$eGFR, 
  df_intervals[df_intervals$train_test == 'Test', ]$sodium, 
  df_intervals[df_intervals$train_test == 'Test', ]$AST, 
  df_intervals[df_intervals$train_test == 'Test', ]$BNP,
  df_intervals[df_intervals$train_test == 'Test', ]$BUN, 
  df_intervals[df_intervals$train_test == 'Test', ]$INR, 
  df_intervals[df_intervals$train_test == 'Test', ]$life_arrhythmia,
  df_intervals[df_intervals$train_test == 'Test', ]$BiVAD_no_discharge,
  df_intervals[df_intervals$train_test == 'Test', ]$temp_surg_LVAD,
  df_intervals[df_intervals$train_test == 'Test', ]$other_durable_MCSD,
  df_intervals[df_intervals$train_test == 'Test', ]$MCSD_malfunction,
  df_intervals[df_intervals$train_test == 'Test', ]$v_tach_fib,
  df_intervals[df_intervals$train_test == 'Test', ]$perc_LVAD,
  df_intervals[df_intervals$train_test == 'Test', ]$MCSD_hemolysis,
  df_intervals[df_intervals$train_test == 'Test', ]$MCSD_rhf,
  df_intervals[df_intervals$train_test == 'Test', ]$MCSD_device_infx,
  df_intervals[df_intervals$train_test == 'Test', ]$MCSD_bleed,
  df_intervals[df_intervals$train_test == 'Test', ]$MCSD_aortic_insf,
  df_intervals[df_intervals$train_test == 'Test', ]$angina))


mat_test_all <- cbind(mat_test,
                      df_intervals[df_intervals$train_test == 'Test', ]$temp_surg_MCS_ever,
                      df_intervals[df_intervals$train_test == 'Test', ]$BiVAD_no_discharge_ever,
                      df_intervals[df_intervals$train_test == 'Test', ]$IV_inotropes_ever,
                      df_intervals[df_intervals$train_test == 'Test', ]$IABP_ever,
                      df_intervals[df_intervals$train_test == 'Test', ]$ECMO_ever,
                      df_intervals[df_intervals$train_test == 'Test', ]$BiVAD_ever,
                      df_intervals[df_intervals$train_test == 'Test', ]$LVAD_ever,
                      df_intervals[df_intervals$train_test == 'Test', ]$RVAD_ever,
                      df_intervals[df_intervals$train_test == 'Test', ]$short_MCS_ever,
                      
                      df_intervals[df_intervals$train_test == 'Test', ]$sodium_min,
                      df_intervals[df_intervals$train_test == 'Test', ]$creatinine_min,
                      df_intervals[df_intervals$train_test == 'Test', ]$bilirubin_min,
                      df_intervals[df_intervals$train_test == 'Test', ]$albumin_min,
                      df_intervals[df_intervals$train_test == 'Test', ]$arterial_lactate_min,
                      df_intervals[df_intervals$train_test == 'Test', ]$BUN_min,
                      df_intervals[df_intervals$train_test == 'Test', ]$AST_min,
                      df_intervals[df_intervals$train_test == 'Test', ]$INR_min,
                      df_intervals[df_intervals$train_test == 'Test', ]$BNP_min,
                      
                      df_intervals[df_intervals$train_test == 'Test', ]$sodium_max,
                      df_intervals[df_intervals$train_test == 'Test', ]$creatinine_max,
                      df_intervals[df_intervals$train_test == 'Test', ]$bilirubin_max,
                      df_intervals[df_intervals$train_test == 'Test', ]$albumin_max,
                      df_intervals[df_intervals$train_test == 'Test', ]$arterial_lactate_max,
                      df_intervals[df_intervals$train_test == 'Test', ]$BUN_max,
                      df_intervals[df_intervals$train_test == 'Test', ]$AST_max,
                      df_intervals[df_intervals$train_test == 'Test', ]$INR_max,
                      df_intervals[df_intervals$train_test == 'Test', ]$BNP_max,
                      
                      df_intervals[df_intervals$train_test == 'Test', ]$sodium_slope,
                      df_intervals[df_intervals$train_test == 'Test', ]$creatinine_slope,
                      df_intervals[df_intervals$train_test == 'Test', ]$bilirubin_slope,
                      df_intervals[df_intervals$train_test == 'Test', ]$albumin_slope,
                      df_intervals[df_intervals$train_test == 'Test', ]$arterial_lactate_slope,
                      df_intervals[df_intervals$train_test == 'Test', ]$BUN_slope,
                      df_intervals[df_intervals$train_test == 'Test', ]$AST_slope,
                      df_intervals[df_intervals$train_test == 'Test', ]$INR_slope,
                      df_intervals[df_intervals$train_test == 'Test', ]$BNP_slope)


names(mat_test) <- c('Death', 'Interval_1', 'Interval_2', 'Interval_3', 'Interval_4',
                      'Age10_1', 'Age10_2', 'Age10_3', 'Age10_4', 'Diabetes', 'Dialysis', 
                      'Diag_Amyloid', 'Diag_Congenital', 'Diag_Hypertrophic', 'Diag_Ischemic',
                      'Diag_Other', 'Diag_Retransplant', 'Diag_Restricted', 'Diag_Valvular',
                      'Dobutamine_High', 'Dopamine_High', 'Epinephrine_High', 'Milrinone_High', 
                      'IABP', 'ECMO', 'LVAD', 'MCSD_Complication', 'SystolicBP', 'DiastolicBP', 
                      'PASP', 'PADP', 'Heart_Rate10', 'Cardiac_Output', 'Central_Venous_Pressure',
                      'Arterial_Lactate', 'CPO', 'API', 'PAPI', 'Hemoglobin', 'Albumin', 'Bilirubin', 
                      'eGFR', 'Sodium', 'AST100', 'BNP100', 'BUN10', 'INR', 'Life_Arrhythmia',
                      'BiVAD_NoDischarge', 'TempSurgery_LVAD', 'Other_DurableMCSD', 'MCSD_Malfunction',
                      'VTach_Fib', 'Perc_LVAD', 'MCSD_Hemolysis', 'MCSD_Rhf', 'MCSD_DeviceInfection',
                      'MCSD_Bleed', 'MCSD_Aortic_Insufficent', 'Angina')
                          
                          
names(mat_test_all) <- c('Death', 'Interval_1', 'Interval_2', 'Interval_3', 'Interval_4',
                          'Age10_1', 'Age10_2', 'Age10_3', 'Age10_4', 'Diabetes', 'Dialysis', 
                          'Diag_Amyloid', 'Diag_Congenital', 'Diag_Hypertrophic', 'Diag_Ischemic',
                          'Diag_Other', 'Diag_Retransplant', 'Diag_Restricted', 'Diag_Valvular',
                          'Dobutamine_High', 'Dopamine_High', 'Epinephrine_High', 'Milrinone_High', 
                          'IABP', 'ECMO', 'LVAD', 'MCSD_Complication', 'SystolicBP', 'DiastolicBP', 
                          'PASP', 'PADP', 'Heart_Rate10', 'Cardiac_Output', 'Central_Venous_Pressure',
                          'Arterial_Lactate', 'CPO', 'API', 'PAPI', 'Hemoglobin', 'Albumin', 'Bilirubin', 
                          'eGFR', 'Sodium', 'AST100', 'BNP100', 'BUN10', 'INR', 'Life_Arrhythmia',
                          'BiVAD_NoDischarge', 'TempSurgery_LVAD', 'Other_DurableMCSD', 'MCSD_Malfunction',
                          'VTach_Fib', 'Perc_LVAD', 'MCSD_Hemolysis', 'MCSD_Rhf', 'MCSD_DeviceInfection',
                          'MCSD_Bleed', 'MCSD_Aortic_Insufficent', 'Angina',
                          
                          'Temp_Surg_Ever', 'BiVAD_No_Discharge_Ever', 'IV_Inotropes_Ever', 'IABP_Ever', 
                          'ECMO_Ever', 'BiVAD_Ever', 'LVAD_Ever', 'RVAD_Ever', 'Short_MCS_Ever',
                          
                          'Sodium_Min', 'Creatinine_Min', 'Bilirubin_Min', 'Albumin_Min', 'Arterial_Lactate_Min',
                          'BUN10_Min', 'AST100_Min', 'INR_Min', 'BNP100_Min',
                          
                          'Sodium_Max', 'Creatinine_Max', 'Bilirubin_Max', 'Albumin_Max', 'Arterial_Lactate_Max',
                          'BUN10_Max', 'AST100_Max', 'INR_Max', 'BNP100_Max',
                          
                          'Sodium_Slope', 'Creatinine_Slope', 'Bilirubin_Slope', 'Albumin_Slope',
                          'Arterial_Lactate_Slope', 'BUN10_Slope', 'AST100_Slope', 'INR_Slope', 'BNP100_Slope')



mat_test_numeric <- mat_test  %>% select(-Death) %>% mutate_all(as.numeric)
mat_test_numeric$`(Intercept)` <- 1
mat_test_numeric <- mat_test_numeric[ , c('(Intercept)', 'Interval_1', 'Interval_2', 'Interval_3', 'Interval_4',
                                          'Age10_1', 'Age10_2', 'Age10_3', 'Age10_4', 'Diabetes', 'Dialysis',
                                          'Diag_Amyloid', 'Diag_Congenital', 'Diag_Hypertrophic', 'Diag_Ischemic',
                                          'Diag_Other', 'Diag_Retransplant', 'Diag_Restricted', 'Diag_Valvular',
                                          'Dobutamine_High', 'Dopamine_High', 'Epinephrine_High', 'Milrinone_High', 
                                          'IABP', 'ECMO', 'LVAD', 'MCSD_Complication', 'SystolicBP',
                                          'DiastolicBP', 'PASP', 'PADP', 'Heart_Rate10', 'Cardiac_Output',
                                          'Central_Venous_Pressure', 'Arterial_Lactate', 'CPO', 'API', 'PAPI',
                                          'Hemoglobin', 'Albumin', 'Bilirubin', 'eGFR', 'Sodium', 
                                          'AST100', 'BNP100', 'BUN10', 'INR',
                                          'Life_Arrhythmia', 'BiVAD_NoDischarge', 'TempSurgery_LVAD',
                                          'Other_DurableMCSD', 'MCSD_Malfunction', 'VTach_Fib', 'Perc_LVAD',
                                          'MCSD_Hemolysis', 'MCSD_Rhf', 'MCSD_DeviceInfection', 'MCSD_Bleed',
                                          'MCSD_Aortic_Insufficent', 'Angina')]
mat_test_numeric <- mat_test_numeric %>% mutate_all(as.numeric)


mat_test_all_numeric <- mat_test_all %>% select(-Death) %>% mutate_all(as.numeric)
names(mat_test_all_numeric) <- c('Interval_1', 'Interval_2', 'Interval_3', 'Interval_4',
                                 'Age10_1', 'Age10_2', 'Age10_3', 'Age10_4', 'Diabetes', 'Dialysis',
                                 'Diag_Amyloid', 'Diag_Congenital', 'Diag_Hypertrophic', 'Diag_Ischemic',
                                 'Diag_Other', 'Diag_Retransplant', 'Diag_Restricted', 'Diag_Valvular',
                                 'Dobutamine_High', 'Dopamine_High', 'Epinephrine_High', 'Milrinone_High', 
                                 'IABP', 'ECMO', 'LVAD', 'MCSD_Complication', 'SystolicBP',
                                 'DiastolicBP', 'PASP', 'PADP', 'Heart_Rate10', 'Cardiac_Output',
                                 'Central_Venous_Pressure', 'Arterial_Lactate', 'CPO', 'API', 'PAPI',
                                 'Hemoglobin', 'Albumin',
                                 'Bilirubin', 'eGFR', 'Sodium', 'AST100', 'BNP100', 'BUN10', 'INR',
                                 'Life_Arrhythmia', 'BiVAD_NoDischarge', 'TempSurgery_LVAD', 'Other_DurableMCSD',
                                 'MCSD_Malfunction', 'VTach_Fib', 'Perc_LVAD', 'MCSD_Hemolysis', 'MCSD_Rhf',
                                 'MCSD_DeviceInfection', 'MCSD_Bleed', 'MCSD_Aortic_Insufficent', 'Angina',
                                 
                                 'Temp_Surg_Ever', 'BiVAD_No_Discharge_Ever', 'IV_Inotropes_Ever', 'IABP_Ever', 
                                 'ECMO_Ever', 'BiVAD_Ever', 'LVAD_Ever', 'RVAD_Ever', 'Short_MCS_Ever',
                                 
                                 'Sodium_Min', 'Creatinine_Min', 'Bilirubin_Min', 'Albumin_Min', 
                                 'Arterial_Lactate_Min', 'BUN10_Min', 'AST100_Min', 'INR_Min', 'BNP100_Min',
                                 
                                 'Sodium_Max', 'Creatinine_Max', 'Bilirubin_Max', 'Albumin_Max',
                                 'Arterial_Lactate_Max', 'BUN10_Max', 'AST100_Max', 'INR_Max', 'BNP100_Max',
                                   
                                 'Sodium_Slope', 'Creatinine_Slope', 'Bilirubin_Slope',
                                 'Albumin_Slope', 'Arterial_Lactate_Slope', 'BUN10_Slope', 'AST100_Slope', 
                                 'INR_Slope', 'BNP100_Slope')

mat_test_all_numeric$`(Intercept)` <- 1
mat_test_all_numeric <- mat_test_all_numeric[ , c('(Intercept)', 
                                                  'Interval_1', 'Interval_2', 'Interval_3', 'Interval_4',
                                                  'Age10_1', 'Age10_2', 'Age10_3', 'Age10_4', 'Diabetes', 'Dialysis',
                                                  'Diag_Congenital', 'Diag_Hypertrophic', 'Diag_Ischemic',
                                                  'Diag_Other', 'Diag_Retransplant', 'Diag_Restricted', 'Diag_Valvular',
                                                  'Dobutamine_High', 'Dopamine_High', 
                                                  'Epinephrine_High', 'Milrinone_High',
                                                  'IABP', 'ECMO', 'LVAD', 'MCSD_Complication', 'SystolicBP',
                                                  'DiastolicBP', 'PASP', 'PADP', 'Heart_Rate10', 'Cardiac_Output',
                                                  'Central_Venous_Pressure', 'Arterial_Lactate', 'Hemoglobin',
                                                  'Albumin', 'Bilirubin', 'eGFR', 'Sodium', 'AST100', 'BNP100',
                                                  'BUN10', 'INR', 'Life_Arrhythmia', 'BiVAD_NoDischarge',
                                                  'TempSurgery_LVAD', 'Other_DurableMCSD', 
                                                  'MCSD_Malfunction', 'VTach_Fib',
                                                  'Perc_LVAD', 'MCSD_Hemolysis', 'MCSD_Rhf',
                                                  'MCSD_DeviceInfection', 'MCSD_Bleed', 
                                                  'MCSD_Aortic_Insufficent', 'Angina',
                                 
                                                  'Temp_Surg_Ever', 'BiVAD_No_Discharge_Ever', 
                                                  'IV_Inotropes_Ever', 'IABP_Ever', 'ECMO_Ever', 
                                                  'BiVAD_Ever', 'LVAD_Ever', 'RVAD_Ever', 'Short_MCS_Ever',
                                 
                                                  'Sodium_Min', 'Creatinine_Min', 'Bilirubin_Min', 'Albumin_Min',
                                                  'Arterial_Lactate_Min', 'BUN10_Min', 'AST100_Min', 
                                                  'INR_Min', 'BNP100_Min',
                                                  
                                                  'Sodium_Max', 'Creatinine_Max', 'Bilirubin_Max', 'Albumin_Max',
                                                  'Arterial_Lactate_Max', 'BUN10_Max', 
                                                  'AST100_Max', 'INR_Max', 'BNP100_Max',
                                   
                                                  'Sodium_Slope', 'Creatinine_Slope', 'Bilirubin_Slope',
                                                  'Albumin_Slope', 'Arterial_Lactate_Slope', 'BUN10_Slope', 
                                                  'AST100_Slope', 'INR_Slope', 'BNP100_Slope')]



df_intervals_test <- subset(df_intervals, df_intervals$train_test == 'Test')
df_intervals_test$status_initial <- factor(df_intervals_test$status_initial, 
                                           levels=c('1', '2', '3', '4', '5', '6'), ordered=T)
df_intervals_test$train_test <- NULL
```


### Matrices for gradient-boosted models
```{r, warning=F, comment=NA}
DMat_test <- xgb.DMatrix(as.matrix(mat_test_numeric[,c('(Intercept)', 
                                                       'Interval_1', 'Interval_2', 'Interval_3', 'Interval_4',
                                                       'Age10_1', 'Age10_2', 'Age10_3', 'Age10_4', 
                                                       'Diabetes', 'Dialysis',
                                                       'Diag_Congenital', 'Diag_Hypertrophic',
                                                       'Diag_Ischemic', 'Diag_Other', 'Diag_Retransplant',
                                                       'Diag_Restricted', 'Diag_Valvular', 'Dobutamine_High',
                                                       'Dopamine_High', 'Epinephrine_High', 'Milrinone_High',
                                                       'IABP', 'ECMO', 'LVAD', 'MCSD_Complication', 'SystolicBP',
                                                       'DiastolicBP', 'PASP', 'PADP', 'Heart_Rate10', 
                                                       'Cardiac_Output', 'Central_Venous_Pressure', 
                                                       'Arterial_Lactate', 'Hemoglobin',
                                                       'Albumin', 'Bilirubin', 'eGFR', 'Sodium', 'AST100', 'BNP100',
                                                       'BUN10', 'INR', 'Life_Arrhythmia', 'BiVAD_NoDischarge',
                                                       'TempSurgery_LVAD', 'Other_DurableMCSD', 'MCSD_Malfunction',
                                                       'VTach_Fib', 'Perc_LVAD', 'MCSD_Hemolysis', 'MCSD_Rhf',
                                                       'MCSD_DeviceInfection', 'MCSD_Bleed', 'MCSD_Aortic_Insufficent',
                                                       'Angina')]))
DMat_test_all <- xgb.DMatrix(as.matrix(mat_test_all_numeric))




colnames(DMat_test) <- c('(Intercept)', 'Interval_1', 'Interval_2', 'Interval_3', 'Interval_4',
                         'Age10_1', 'Age10_2', 'Age10_3', 'Age10_4', 'Diabetes1', 'Dialysis1',
                         'Diag_Congenital', 'Diag_Hypertrophic', 'Diag_Ischemic',
                         'Diag_Other', 'Diag_Retransplant', 'Diag_Restricted', 'Diag_Valvular',
                         'Dobutamine_High', 'Dopamine_High', 'Epinephrine_High', 'Milrinone_High', 'IABP1', 'ECMO1',
                         'LVAD1', 'MCSD_Complication1', 'SystolicBP', 'DiastolicBP', 'PASP', 'PADP',
                         'Heart_Rate10', 'Cardiac_Output', 'Central_Venous_Pressure',
                         'Arterial_Lactate', 'Hemoglobin', 'Albumin',
                         'Bilirubin', 'eGFR', 'Sodium', 'AST100', 'BNP100', 'BUN10', 'INR',
                         'Life_Arrhythmia1', 'BiVAD_NoDischarge1', 'TempSurgery_LVAD1', 'Other_DurableMCSD1',
                         'MCSD_Malfunction1', 'VTach_Fib1', 'Perc_LVAD1', 'MCSD_Hemolysis1', 'MCSD_Rhf1',
                         'MCSD_DeviceInfection1', 'MCSD_Bleed1', 'MCSD_Aortic_Insufficent1', 'Angina1')


colnames(DMat_test_all) <- c('(Intercept)', 'Interval_1', 'Interval_2', 'Interval_3', 'Interval_4',
                             'Age10_1', 'Age10_2', 'Age10_3', 'Age10_4', 'Diabetes1', 'Dialysis1',
                             'Diag_Congenital', 'Diag_Hypertrophic', 'Diag_Ischemic',
                             'Diag_Other', 'Diag_Retransplant', 'Diag_Restricted', 'Diag_Valvular',
                             'Dobutamine_High', 'Dopamine_High', 'Epinephrine_High', 'Milrinone_High', 'IABP1', 'ECMO1',
                             'LVAD1', 'MCSD_Complication1', 'SystolicBP', 'DiastolicBP', 'PASP', 'PADP',
                             'Heart_Rate10', 'Cardiac_Output', 'Central_Venous_Pressure',
                             'Arterial_Lactate', 'Hemoglobin', 'Albumin',
                             'Bilirubin', 'eGFR', 'Sodium', 'AST100', 'BNP100', 'BUN10', 'INR',
                             'Life_Arrhythmia1', 'BiVAD_NoDischarge1', 'TempSurgery_LVAD1', 'Other_DurableMCSD1',
                             'MCSD_Malfunction1', 'VTach_Fib1', 'Perc_LVAD1', 'MCSD_Hemolysis1', 'MCSD_Rhf1',
                             'MCSD_DeviceInfection1', 'MCSD_Bleed1', 'MCSD_Aortic_Insufficent1', 'Angina1',
                             
                             'Temp_Surg_Ever1', 'BiVAD_No_Discharge_Ever1', 'IV_Inotropes_Ever1', 
                             'IABP_Ever1', 'ECMO_Ever1','BiVAD_Ever1', 'LVAD_Ever1', 'RVAD_Ever1', 'Short_MCS_Ever',
                             
                             'Sodium_Min', 'Creatinine_Min', 'Bilirubin_Min', 'Albumin_Min', 
                             'Arterial_Lactate_Min', 'BUN10_Min', 'AST100_Min', 'INR_Min', 'BNP100_Min',
                             
                             'Sodium_Max', 'Creatinine_Max', 'Bilirubin_Max', 'Albumin_Max', 
                             'Arterial_Lactate_Max', 'BUN10_Max', 'AST100_Max', 'INR_Max', 'BNP100_Max',
                             
                             'Sodium_Slope', 'Creatinine_Slope', 'Bilirubin_Slope', 'Albumin_Slope', 
                             'Arterial_Lactate_Slope', 'BUN10_Slope', 'AST100_Slope', 'INR_Slope', 'BNP100_Slope')

```


-------------


# 3. Run selected logistic regression models
All models are run in the training datasets.


### Selected Model #1: French candidate risk score

* Short term MCS (ECMO or Balloon Pump)
* Natural log of (bilirubin + 1)
* eGFR
* Natural log of (BNP + 1)


```{r, echo=F, warning=F, comment=NA, fig.align='center'}
model1 <- glm(formula = as.formula(
  death6week ~ 
    short_MCS_French + log(bilirubin + 1) + eGFR + log(BNP + 1)), 
  data = df_intervals[df_intervals$train_test == 'Train',], 
  family = binomial)

model1_results <- as.data.frame(summary(model1)$coefficients)
model1_results$name <- rownames(model1_results)

model1_results <- model1_results[ ,c('name', 'Estimate', 'Pr(>|z|)')]
colnames(model1_results) <- c('Variable', 'Estimate', 'PValue')
rownames(model1_results) <- seq(1:nrow(model1_results))
model1_results <- model1_results %>% 
  mutate(Estimate = round(Estimate, 3),
           PValue = round(PValue, 3))

model1_results$Variable <- c('Intercept', 'Short Term MCS', 'Log Bilirubin+1', 'eGFR', 'Log BNP+1')

model1_results %>%
  kable(align=c('c', 'c', 'c')) %>%
  kable_styling(bootstrap_options='striped', full_width=F, position='center') %>%
  column_spec(1, border_right=T)

save(model1, file = './model_frenchCRS.RData')
```


### Selected Model #2: US candidate risk score with PCWP

* Pulmonary capillary wedge pressure (PCWP)
* Albumin
* Natural log of (bilirubin + 1)
* eGFR
* Sodium
* Natural log of (BNP + 1)
* LVAD
* Short MCS ever (ECMO ever, temporary surgery LVAD ever, BiVAD no discharge ever)


```{r, echo=F, warning=F, comment=NA, fig.align='center'}
model2 <- glm(formula = as.formula(
  death6week ~ 
    PCWP + albumin + log(bilirubin + 1) + eGFR + sodium + log(BNP + 1) + LVAD + short_MCS_ever), 
  data = df_intervals[df_intervals$train_test == 'Train',], 
  family = binomial)

model2_results <- as.data.frame(summary(model2)$coefficients)
model2_results$name <- rownames(model2_results)

model2_results <- model2_results[ ,c('name', 'Estimate', 'Pr(>|z|)')]
colnames(model2_results) <- c('Variable', 'Estimate', 'PValue')
rownames(model2_results) <- seq(1:nrow(model2_results))
model2_results <- model2_results %>% 
  mutate(Estimate = round(Estimate, 3),
           PValue = round(PValue, 3))

model2_results$Variable <- c('Intercept', 'PCWP', 'Albumin', 'Log Bilirubin+1', 'eGFR', 'Sodium', 'Log BNP+1',
                             'LVAD', 'Short MCS Ever')

model2_results %>%
  kable(align=c('c', 'c', 'c')) %>%
  kable_styling(bootstrap_options='striped', full_width=F, position='center') %>%
  column_spec(1, border_right=T)
```


### Selected Model #3: US candidate risk score replacing PCWP

* Replaces PCWP with Cardiac Pressure Output (CPO) and Aortic Pulsatility Index (API)
* CPO = (1/541) x (Cardiac Output) x (2/3 x Systolic BP  +  1/3 x Diastolic BP)
* API = (Systolic BP - Diastolic BP) / PCWP


#### Check that CPO and API are not heavily correlated
```{r, echo=F, warning=F, comment=NA, fig.align='center'}
paste0('Pearson correlation ', cor(df_intervals$cpo, df_intervals$api, use = 'pairwise.complete.obs', method = 'pearson'))
paste0('Spearman correlation ', cor(df_intervals$cpo, df_intervals$api, use = 'pairwise.complete.obs', method = 'spearman'))
```


#### Model results
```{r, echo=F, warning=F, comment=NA, fig.align='center'}
model3 <- glm(formula = as.formula(
  death6week ~ 
    cpo + api + albumin + log(bilirubin + 1) + eGFR + sodium + log(BNP + 1) + LVAD + short_MCS_ever), 
  data = df_intervals[df_intervals$train_test == 'Train',], 
  family = binomial)

model3_results <- as.data.frame(summary(model3)$coefficients)
model3_results$name <- rownames(model3_results)

model3_results <- model3_results[ ,c('name', 'Estimate', 'Pr(>|z|)')]
colnames(model3_results) <- c('Variable', 'Estimate', 'PValue')
rownames(model3_results) <- seq(1:nrow(model3_results))
model3_results <- model3_results %>% 
  mutate(Estimate = round(Estimate, 3),
           PValue = round(PValue, 3))

model3_results$Variable <- c('Intercept', 'CPO', 'API', 'Albumin', 'Log Bilirubin+1', 'eGFR', 'Sodium', 'Log BNP+1',
                             'LVAD', 'Short MCS Ever')

model3_results %>%
  kable(align=c('c', 'c', 'c')) %>%
  kable_styling(bootstrap_options='striped', full_width=F, position='center') %>%
  column_spec(1, border_right=T)
```


### Selected Model #4: US candidate risk score with only CPO


```{r, echo=F, warning=F, comment=NA, fig.align='center'}
model4 <- glm(formula = as.formula(
  death6week ~ 
    cpo + albumin + log(bilirubin + 1) + eGFR + sodium + log(BNP + 1) + LVAD + short_MCS_ever), 
  data = df_intervals[df_intervals$train_test == 'Train',], 
  family = binomial)

model4_results <- as.data.frame(summary(model4)$coefficients)
model4_results$name <- rownames(model4_results)

model4_results <- model4_results[ ,c('name', 'Estimate', 'Pr(>|z|)')]
colnames(model4_results) <- c('Variable', 'Estimate', 'PValue')
rownames(model4_results) <- seq(1:nrow(model4_results))
model4_results <- model4_results %>% 
  mutate(Estimate = round(Estimate, 3),
           PValue = round(PValue, 3))

model4_results$Variable <- c('Intercept', 'CPO', 'Albumin', 'Log Bilirubin+1', 'eGFR', 'Sodium', 'Log BNP+1',
                             'LVAD', 'Short MCS Ever')

model4_results %>%
  kable(align=c('c', 'c', 'c')) %>%
  kable_styling(bootstrap_options='striped', full_width=F, position='center') %>%
  column_spec(1, border_right=T)
```


### Selected Model #5: US candidate risk score with only API


```{r, echo=F, warning=F, comment=NA, fig.align='center'}
model5 <- glm(formula = as.formula(
  death6week ~ 
    api + albumin + log(bilirubin + 1) + eGFR + sodium + log(BNP + 1) + LVAD + short_MCS_ever), 
  data = df_intervals[df_intervals$train_test == 'Train',], 
  family = binomial)

model5_results <- as.data.frame(summary(model5)$coefficients)
model5_results$name <- rownames(model5_results)

model5_results <- model5_results[ ,c('name', 'Estimate', 'Pr(>|z|)')]
colnames(model5_results) <- c('Variable', 'Estimate', 'PValue')
rownames(model5_results) <- seq(1:nrow(model5_results))
model5_results <- model5_results %>% 
  mutate(Estimate = round(Estimate, 3),
           PValue = round(PValue, 3))

model5_results$Variable <- c('Intercept', 'API', 'Albumin', 'Log Bilirubin+1', 'eGFR', 'Sodium', 'Log BNP+1',
                             'LVAD', 'Short MCS Ever')

model5_results %>%
  kable(align=c('c', 'c', 'c')) %>%
  kable_styling(bootstrap_options='striped', full_width=F, position='center') %>%
  column_spec(1, border_right=T)
```


### Selected Model #6: US candidate risk score replacing API with PAPI

* PAPI = (Systolic pulmonary artery pressure - diastolic pulmonary artery pressure) / right atrial pressure 
* Right atrial pressure is central venous pressure


#### Check that CPO and PAPi are not heavily correlated
```{r, echo=F, warning=F, comment=NA, fig.align='center'}
paste0('Pearson correlation ', cor(df_intervals$cpo, df_intervals$papi, use = 'pairwise.complete.obs', method = 'pearson'))
paste0('Spearman correlation ', cor(df_intervals$cpo, df_intervals$papi, use = 'pairwise.complete.obs', method = 'spearman'))
```


#### Model results
```{r, echo=F, warning=F, comment=NA, fig.align='center'}
model6 <- glm(formula = as.formula(
  death6week ~ 
    cpo + papi + albumin + log(bilirubin + 1) + eGFR + sodium + log(BNP + 1) + LVAD + short_MCS_ever), 
  data = df_intervals[df_intervals$train_test == 'Train',], 
  family = binomial)

model6_results <- as.data.frame(summary(model6)$coefficients)
model6_results$name <- rownames(model6_results)

model6_results <- model6_results[ ,c('name', 'Estimate', 'Pr(>|z|)')]
colnames(model6_results) <- c('Variable', 'Estimate', 'PValue')
rownames(model6_results) <- seq(1:nrow(model6_results))
model6_results <- model6_results %>% 
  mutate(Estimate = round(Estimate, 3),
           PValue = round(PValue, 3))

model6_results$Variable <- c('Intercept', 'CPO', 'PAPI', 'Albumin', 'Log Bilirubin+1', 'eGFR', 'Sodium', 'Log BNP+1',
                             'LVAD', 'Short MCS Ever')

model6_results %>%
  kable(align=c('c', 'c', 'c')) %>%
  kable_styling(bootstrap_options='striped', full_width=F, position='center') %>%
  column_spec(1, border_right=T)

save(model6, file = './model_USCRS.RData')
```


### Selected Model #7: US candidate risk score with only PAPI

```{r, echo=F, warning=F, comment=NA, fig.align='center'}
model7 <- glm(formula = as.formula(
  death6week ~ 
    papi + albumin + log(bilirubin + 1) + eGFR + sodium + log(BNP + 1) + LVAD + short_MCS_ever), 
  data = df_intervals[df_intervals$train_test == 'Train',], 
  family = binomial)

model7_results <- as.data.frame(summary(model7)$coefficients)
model7_results$name <- rownames(model7_results)

model7_results <- model7_results[ ,c('name', 'Estimate', 'Pr(>|z|)')]
colnames(model7_results) <- c('Variable', 'Estimate', 'PValue')
rownames(model7_results) <- seq(1:nrow(model7_results))
model7_results <- model7_results %>% 
  mutate(Estimate = round(Estimate, 3),
           PValue = round(PValue, 3))

model7_results$Variable <- c('Intercept', 'PAPI', 'Albumin', 'Log Bilirubin+1', 'eGFR', 'Sodium', 'Log BNP+1',
                             'LVAD', 'Short MCS Ever')

model7_results %>%
  kable(align=c('c', 'c', 'c')) %>%
  kable_styling(bootstrap_options='striped', full_width=F, position='center') %>%
  column_spec(1, border_right=T)
```


### Compare model AICs and BICs

```{r, echo=F, warning=F, comment=NA, fig.align='center'}
model_comparison <- data.frame(
  'Model' = c('1 (French)', '2 (US-CRS with PCWP)', 
              '3 (US-CRS with CPO and API)', 
              '4 (US-CRS with CPO Only)',
              '5 (US-CRS with API Only)',
              '6 (US-CRS with CPO and PAPI)',
              '7 (US-CRS with PAPI Only)'),
  'AIC' = c(AIC(model1), AIC(model2), AIC(model3), AIC(model4), AIC(model5), AIC(model6), AIC(model7)),
  'BIC' = c(BIC(model1), BIC(model2), BIC(model3), BIC(model4), BIC(model5), BIC(model6), BIC(model7)))

model_comparison$AIC <- round(model_comparison$AIC, 3)
model_comparison$BIC <- round(model_comparison$BIC, 3)


model_comparison %>%
  kable(align=c('c', 'c', 'c')) %>%
  kable_styling(bootstrap_options='striped', full_width=F, position='center') %>%
  column_spec(1, border_right=T)
```


### Coefficient plot of results (Figure 1)
```{r, warning=F, echo=F, message=F, comment=NA, fig.align='center', fig.height=8, fig.width=8}
df_coefplot <- as.data.frame(summary(model6)$coefficients)[ ,c('Estimate', 'Std. Error')]
df_coefplot$var <- rownames(df_coefplot)
rownames(df_coefplot) <- NULL
colnames(df_coefplot) <- c('us_est', 'us_stderr', 'var')

df_coefplot$french_est <- NA
df_coefplot$french_stderr <- NA

df_coefplot$french_est[df_coefplot$var == 'short_MCS_ever'] <- summary(model1)$coefficients['short_MCS_French', 'Estimate']
df_coefplot$french_est[df_coefplot$var == 'log(bilirubin + 1)'] <- 
  summary(model1)$coefficients['log(bilirubin + 1)', 'Estimate']
df_coefplot$french_est[df_coefplot$var == 'eGFR'] <- summary(model1)$coefficients['eGFR', 'Estimate']
df_coefplot$french_est[df_coefplot$var == 'log(BNP + 1)'] <- summary(model1)$coefficients['log(BNP + 1)', 'Estimate']


df_coefplot$french_stderr[df_coefplot$var == 'short_MCS_ever'] <- summary(model1)$coefficients['short_MCS_French', 'Std. Error']
df_coefplot$french_stderr[df_coefplot$var == 'log(bilirubin + 1)'] <- 
  summary(model1)$coefficients['log(bilirubin + 1)', 'Std. Error']
df_coefplot$french_stderr[df_coefplot$var == 'eGFR'] <- summary(model1)$coefficients['eGFR', 'Std. Error']
df_coefplot$french_stderr[df_coefplot$var == 'log(BNP + 1)'] <- summary(model1)$coefficients['log(BNP + 1)', 'Std. Error']


df_coefplot <- df_coefplot[ ,c('var', 'french_est', 'french_stderr', 'us_est', 'us_stderr')]
df_coefplot$var <- c('(Intercept)', 'CPO', 'PAPi', 'Albumin', 'Log Bilirubin', 
'eGFR', 'Sodium', 'Log BNP', 'Durable LVAD', 'Short-Term MCS')
df_coefplot <- subset(df_coefplot, df_coefplot$var != '(Intercept)')


df_coefplot <- df_coefplot %>%
  arrange(sapply(var, 
                 function(x) 
                   which(x == c('Short-Term MCS', 'Log Bilirubin', 'eGFR', 'Log BNP', 
                                'CPO', 'PAPi', 'Albumin', 'Sodium', 'Durable LVAD'))))


### Scale certain estimates
df_coefplot$french_est[df_coefplot$var == 'eGFR'] <- df_coefplot$french_est[df_coefplot$var == 'eGFR'] * 10
df_coefplot$french_est[df_coefplot$var == 'PAPi'] <- df_coefplot$french_est[df_coefplot$var == 'PAPi'] * 2
df_coefplot$french_est[df_coefplot$var == 'Sodium'] <- df_coefplot$french_est[df_coefplot$var == 'Sodium'] * 10


df_coefplot <- df_coefplot %>%
  mutate(
    french_lower = french_est - (1.96*french_stderr),
    french_upper = french_est + (1.96*french_stderr),
    us_lower = us_est - (1.96*us_stderr),
    us_upper = us_est + (1.96*us_stderr)) %>%
  select(-c(french_stderr, us_stderr))


df_coefplot <- df_coefplot %>%
  pivot_longer(!var, names_to = 'stat', values_to = 'count')
df_coefplot$model <- ifelse(grepl('^french', df_coefplot$stat), 'French', 'US')
df_coefplot$stat <- sub('.*_', '', df_coefplot$stat)

df_coefplot$est <- ifelse(df_coefplot$stat == 'est', df_coefplot$count, NA)
df_coefplot$lower <- ifelse(df_coefplot$stat == 'lower', df_coefplot$count, NA)
df_coefplot$upper <- ifelse(df_coefplot$stat == 'upper', df_coefplot$count, NA)

df_coefplot$stat <- NULL; df_coefplot$count <- NULL
df_coefplot <- df_coefplot %>%
  group_by(var, model) %>%
  summarize_all(~ max(., na.rm=T), .groups = 'drop')

df_coefplot$est[df_coefplot$est == -Inf] <- NA
df_coefplot$lower[df_coefplot$lower == -Inf] <- NA
df_coefplot$upper[df_coefplot$upper == -Inf] <- NA

df_coefplot <- df_coefplot %>%
  arrange(sapply(var, 
                 function(x) 
                   which(x == c('Short-Term MCS', 'Log Bilirubin', 'eGFR', 'Log BNP', 'CPO', 'PAPi', 'Albumin', 
                                'Sodium', 'Durable LVAD'))))


df_coefplot$var[df_coefplot$var == 'Short-Term MCS'] <- 'Short-Term MCS*'
df_coefplot$var[df_coefplot$var == 'eGFR'] <- 'eGFR**'
df_coefplot$var[df_coefplot$var == 'PAPi'] <- 'PAPi***'
df_coefplot$var[df_coefplot$var == 'Sodium'] <- 'Sodium**'




df_coefplot %>%
  ggplot(aes(x=est, 
             y=factor(var, levels = c('Durable LVAD', 'Sodium**', 'Albumin', 'PAPi***', 'CPO', 
                                      'Log BNP', 'eGFR**', 'Log Bilirubin', 'Short-Term MCS*')), 
             color=model)) +
  geom_point(size = 3.25, position = position_dodge(width = 0.7)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), width = 0.05, position = position_dodge(width = 0.7)) +
  geom_vline(xintercept = 0, lty = 2, color = 'red') +
  xlab('Effect Size') + xlim(-1.25, 1.25) +
  scale_colour_manual(name='Model', values=c('#fd8d3c', '#bd0026'), labels = c('French-CRS', 'US-CRS')) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(face='bold', size = 14, margin = margin(t=5)), 
        axis.text.x = element_text(face='bold', size = 13, margin = margin(t=5)), 
        axis.title.y = element_blank(),
        axis.text.y = element_text(face='bold', size = 13, margin = margin(r=5)),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16))  
```


-------------


# 4. Obtain machine learning model results 

### Elastic Net Regression, No Memory Variables
Results are obtained from **elastic_net_model.R**.


```{r, echo=F, warning=F, comment=NA, fig.align='center'}
load('./Newest Models/model_elastic_nomem_6wk.RData')

model_elastic_nomem_results <- data.frame(
  'Variable' = rownames(
    coef(model_elastic_nomem$finalModel, model_elastic_nomem$bestTune$lambda, model_elastic_nomem$bestTune$alpha)),
  
  'Estimate' = as.numeric(
    coef(model_elastic_nomem$finalModel, model_elastic_nomem$bestTune$lambda, model_elastic_nomem$bestTune$alpha)))


model_elastic_nomem_results[ ,'Estimate'] <- signif(model_elastic_nomem_results[ ,'Estimate'], 2)
model_elastic_nomem_results %>%
  kable(align=c('c', 'c')) %>%
  kable_styling(bootstrap_options='striped', full_width=F, position='center') %>%
  column_spec(1, border_right=T)
```


### Elastic Net Regression, All Memory Variables

```{r, echo=F, warning=F, comment=NA, fig.align='center'}
load('./Newest Models/model_elastic_mem_6wk.RData')

model_elastic_all_results <- data.frame(
  'Variable' = rownames(
    coef(model_elastic_all$finalModel, model_elastic_all$bestTune$lambda, model_elastic_all$bestTune$alpha)),
  
  'Estimate' = as.numeric(coef(model_elastic_all$finalModel, model_elastic_all$bestTune$lambda,
                               model_elastic_all$bestTune$alpha)))


model_elastic_all_results[ ,'Estimate'] <- signif(model_elastic_all_results[ ,'Estimate'], 2)
model_elastic_all_results %>%
  kable(align=c('c', 'c')) %>%
  kable_styling(bootstrap_options='striped', full_width=F, position='center') %>%
  column_spec(1, border_right=T)
```


### Gradient-Boosted Model Results
Results are obtained from the final iteration of **xgboost_mem_vars_model.R**.

```{r, echo=F, warning=F, message=F, comment=NA}
load('./Newest Models/model_xgboost_nomem_final.RData')
load('./Newest Models/model_xgboost_mem_final.RData')
```


-------------


# 5. Make predictions on test dataset

```{r, echo=F, warning=F, message=F, comment=NA, fig.align='center', fig.height=8, fig.width=8, results='hide'}
model1_predict <- as.numeric(
  predict(model1, newdata = df_intervals_test, type = 'response'))
model1_indices <- which(!is.na(model1_predict))
model1_predict <- na.omit(model1_predict)

model6_predict <- as.numeric(
  predict(model6, newdata = df_intervals_test, type = 'response'))
model6_indices <- which(!is.na(model6_predict))
model6_predict <- na.omit(model6_predict)



elastic_nomem_predict <- predict(model_elastic_nomem, newdata = mat_test, type = 'prob', na.action = na.pass)[,'1']
nomem_indices <- which(!is.na(elastic_nomem_predict))
elastic_nomem_predict<- na.omit(elastic_nomem_predict)

elastic_all_predict <- predict(model_elastic_all, newdata = mat_test_all, type = 'prob', na.action = na.pass)[,'1']
mem_indices <- which(!is.na(elastic_all_predict))
elastic_all_predict<- na.omit(elastic_all_predict)


xg_nomem_predict <- predict(model_xgboost_nomem, newdata = DMat_test)
xg_all_predict <- predict(model_xgboost_mem, newdata = DMat_test_all)


rownames(df_intervals_test) <- seq(1:nrow(df_intervals_test))

df_intervals_test_nomem <- df_intervals_test[(as.numeric(rownames(df_intervals_test)) %in% nomem_indices), ]
df_intervals_test_mem <- df_intervals_test[(as.numeric(rownames(df_intervals_test)) %in% mem_indices), ]

df_intervals_test_model1 <- df_intervals_test[(as.numeric(rownames(df_intervals_test)) %in% model1_indices), ]
df_intervals_test_model6 <- df_intervals_test[(as.numeric(rownames(df_intervals_test)) %in% model6_indices), ]
```


-------------


# 6. Create AUC curves

### ROC curve for models with no memory variables (Supplemental Appendix 5)
```{r, warning=F, echo=F, message=F, comment=NA, fig.align='center', fig.height=8, fig.width=8, results='hide'}
pROC::roc(data=df_intervals_test, response=death6week, predictor=status_initial, direction='>', 
          plot=T, col='#d7191c', print.auc=T, lwd=2.5, quiet=T)
pROC::roc(df_intervals_test_nomem$death6week ~ elastic_nomem_predict, direction='<', 
          plot=T, print.auc=T, print.auc.y=0.45, smooth=T, lwd=2.5, col='#fdae61', add=T, quiet=T)
pROC::roc(df_intervals_test$death6week ~ xg_nomem_predict, direction='<', 
          plot=T, print.auc=T, print.auc.y=0.40, smooth=T, lwd=2.5, col='#ffffbf', add=T, quiet=T)
pROC::roc(df_intervals_test_model1$death6week ~ model1_predict, direction='<', 
          plot=T, print.auc=T, print.auc.y=0.35, smooth=T, lwd=2.5, col='#abdda4', add=T, quiet=T)
pROC::roc(df_intervals_test_model6$death6week ~ model6_predict, direction='<', 
          plot=T, print.auc=T, print.auc.y=0.30, smooth=T, lwd=2.5, col='#2b83ba', add=T, quiet=T)
title(main = 'Heart Allocation Score vs. 6 Status, 14-Day Intervals, 6-Week Death, No Memory', line = 2.5)
legend('bottomright', cex = 0.9, xpd = T,
      legend = c('6-Status', 
                 'Elastic Net',
                 'Gradient-Boosted Model', 
                 'French CRS',
                 'US CRS with PAPI and CPO'),
       col = c('#d7191c', '#fdae61', '#ffffbf', '#abdda4', '#2b83ba'), lwd = 2.5)
```


### ROC curve for models with all memory variables (Supplemental Appendix 5)
```{r, warning=F, echo=F, message=F, comment=NA, fig.align='center', fig.height=8, fig.width=8, results='hide'}
pROC::roc(data=df_intervals_test, response=death6week, predictor=status_initial, direction='>', 
          plot=T, col='#d7191c', print.auc=T, lwd=2.5, quiet=T)
pROC::roc(df_intervals_test_mem$death6week ~ elastic_all_predict, direction='<', 
          plot=T, print.auc=T, print.auc.y=0.45, smooth=T, lwd=2.5, col='#fdae61', add=T, quiet=T)
pROC::roc(df_intervals_test$death6week ~ xg_all_predict, direction='<',
          plot=T, print.auc=T, print.auc.y=0.40, smooth=T, lwd=2.5, col='#ffffbf', add=T, quiet=T)
pROC::roc(df_intervals_test_model1$death6week ~ model1_predict, direction='<',
          plot=T, print.auc=T, print.auc.y=0.35, smooth=T, lwd=2.5, col='#abdda4', add=T, quiet=T)
pROC::roc(df_intervals_test_model6$death6week ~ model6_predict, direction='<',
          plot=T, print.auc=T, print.auc.y=0.30, smooth=T, lwd=2.5, col='#2b83ba', add=T, quiet=T)
title(main = 'Heart Allocation Score vs. 6 Status, 14-Day Intervals, 6-Week Death, All Memory', line = 2.5)
legend('bottomright', cex = 0.9, xpd = T,
      legend = c('6-Status', 
                 'Elastic Net',
                 'Gradient-Boosted Model', 
                 'French CRS',
                 'US CRS with PAPI and CPO'),
       col = c('#d7191c', '#fdae61', '#ffffbf', '#abdda4', '#2b83ba'), lwd = 2.5)
```


### ROC curve for subset of models (Figure 2)
```{r, warning=F, echo=F, message=F, comment=NA, fig.align='center', fig.height=10, fig.width=10, results='hide'}
pROC::roc(data=df_intervals_test, response=death6week, predictor=status_initial, direction='>',
          plot=T, col='#FCAE91', print.auc=T,
          lwd=3, quiet=T, print.thres = c(1,2,3,4,5,6), 
          print.thres.cex = 1.2, 
          print.thres.pattern= "Status%2.f",
          print.auc.cex = 1.2,
          legacy.axes=T)
pROC::roc(df_intervals_test_model1$death6week ~ model1_predict, direction='<',
          plot=T, print.auc=T, print.auc.y=0.45, smooth=T, lwd=2.5, col='#FB6A4A', add=T, quiet=T, print.auc.cex = 1.2)
pROC::roc(df_intervals_test_model6$death6week ~ model6_predict, direction='<',
          plot=T, print.auc=T, print.auc.y=0.40, smooth=T, lwd=2.5, col='#A50F15', add=T, quiet=T, print.auc.cex = 1.2)
legend('bottomright', cex = 1.2, xpd = T,
      legend = c('6-Status', 'French CRS', 'US CRS'),
       col = c('#FCAE91', '#FB6A4A', '#A50F15'), lwd=3)
```


-------------


# 7. Variable importance plots from gradient-boosted models/Supplement 7

## Variable importance for XGBoost, No Memory
```{r, warning=F, echo=F, message=F, comment=NA, fig.align='center', fig.height=8, fig.width=8, results='hide'}
importance1 <- xgb.importance(feature_names = colnames(DMat_test), model=model_xgboost_nomem)
importance1 <- importance1[importance1$Gain >= 0.03, ]


p1 <- xgb.ggplot.importance(importance1, n_clusters = 1)

p1 +  
  theme_bw() + 
  ggtitle('Feature Importance, 0.03 or Higher') +
  scale_fill_manual(values = 'firebrick') +
  theme(
    legend.position = 'none',
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 14, margin = margin(t=5)), 
    axis.text.x = element_text(size = 13, margin = margin(t=5)), 
    axis.title.y = element_text(size = 14, margin = margin(r=5)),
    axis.text.y = element_text(size = 13, margin = margin(r=5)),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    plot.title = element_text(face = 'bold', size = 16, hjust=0.5)) 
```


## Variable importance for XGBoost, All Memory
```{r, warning=F, echo=F, message=F, comment=NA, fig.align='center', fig.height=8, fig.width=8, results='hide'}
importance2 <- xgb.importance(feature_names = colnames(DMat_test_all), model=model_xgboost_mem)
importance2 <- importance2[importance2$Gain >= 0.03, ]


p2 <- xgb.ggplot.importance(importance1, n_clusters = 1)

p2 +  
  theme_bw() + 
  ggtitle('Feature Importance, 0.03 or Higher') +
  scale_fill_manual(values = 'firebrick') +
  theme(
    legend.position = 'none',
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 14, margin = margin(t=5)), 
    axis.text.x = element_text(size = 13, margin = margin(t=5)), 
    axis.title.y = element_text(size = 14, margin = margin(r=5)),
    axis.text.y = element_text(size = 13, margin = margin(r=5)),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    plot.title = element_text(face = 'bold', size = 16, hjust=0.5)) 
```
