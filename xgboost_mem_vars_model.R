rm(list=ls()); gc()
set.seed(200)
setwd('~/Desktop')
library(tidyverse)
library(splines)
library(glmnet)
library(xgboost)
library(parallel)
library(caret)


load('./intervals_v16_impute_6wkdeath.RData')

factor_vars <- c('diagnosis', 'diabetes', 'dialysis', 'IV_inotropes', 'dobutamine', 
                 'dopamine', 'epinephrine', 'milrinone', 'IABP', 'ECMO', 'BiVAD', 'LVAD', 'RVAD',
                 'MCSD_complication', 'life_arrhythmia', 'BiVAD_no_discharge', 
                 'temp_surg_LVAD', 'other_durable_MCSD', 'MCSD_malfunction', 'v_tach_fib', 
                 'perc_LVAD', 'MCSD_hemolysis', 'MCSD_rhf', 'MCSD_device_infx', 
                 'MCSD_bleed', 'MCSD_aortic_insf', 'angina', 
                 
                 'temp_surg_ever', 'BiVAD_no_discharge_ever','IV_inotropes_ever', 
                 'IABP_ever', 'ECMO_ever', 'BiVAD_ever', 'LVAD_ever', 'RVAD_ever')

numeric_vars <- c('interval', 'interval_start', 'interval_stop', 'age', 
                  'systolicBP', 'diastolicBP', 'PASP', 'PADP', 'heart_rate', 'cardiac_output', 
                  'central_venous_pressure', 'arterial_lactate', 
                  'PCWP', 'hemoglobin', 'albumin', 'bilirubin', 'creatinine', 'eGFR', 'sodium', 

                  'AST', 'BNP', 'BUN', 'INR', 'LDH', 'sodium_max', 'creatinine_max', 
                  'bilirubin_max', 'albumin_max', 'arterial_lactate_max', 'BUN_max', 'AST_max', 
                  'INR_max', 'BNP_max', 'LDH_max', 'sodium_min', 'creatinine_min', 
                  'bilirubin_min', 'albumin_min', 'arterial_lactate_min', 'BUN_min', 'AST_min', 
                  'INR_min', 'BNP_min', 'LDH_min', 'sodium_slope', 'creatinine_slope', 'bilirubin_slope', 
                  'albumin_slope', 'arterial_lactate_slope', 'BUN_slope', 'AST_slope', 
                  'INR_slope', 'BNP_slope', 'LDH_slope')


df_intervals[ ,factor_vars] <- lapply(df_intervals[ ,factor_vars], as.factor)
df_intervals[ ,numeric_vars] <- lapply(df_intervals[ ,numeric_vars], as.numeric)
rm(factor_vars, numeric_vars)


df_intervals$diagnosis <- relevel(factor(df_intervals$diagnosis), ref = 'Dilated_CM')
df_intervals$dobutamine <- relevel(factor(df_intervals$dobutamine), ref = 'Not High')
df_intervals$dopamine <- relevel(factor(df_intervals$dopamine), ref = 'Not High')
df_intervals$epinephrine <- relevel(factor(df_intervals$epinephrine), ref = 'Not High')
df_intervals$milrinone <- relevel(factor(df_intervals$milrinone), ref = 'Not High')


df_intervals <- df_intervals %>% 
  mutate(
    cpo = (1/541) * cardiac_output * (((2/3)*systolicBP) + ((1/3)*diastolicBP)), 
    api = (systolicBP - diastolicBP) / PCWP)
df_intervals$api[df_intervals$PCWP == 0] <- NA

df_intervals <- df_intervals %>%
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
df_intervals$CenterId <- NULL



## Training matrix
interval_dummies_train <- predict(
  caret::dummyVars(~ diagnosis + dobutamine + dopamine + epinephrine + milrinone, 
                   data = df_intervals[df_intervals$train_test == 'Train', ],
                   sep = NULL,
                   fullrank = T), 
  newdata = df_intervals[df_intervals$train_test == 'Train', ])
interval_dummies_train <- as.data.frame(interval_dummies_train)
interval_dummies_train <- interval_dummies_train %>% select(-c(diagnosisDilated_CM, `dobutamineNot High`, 
                                                               `dopamineNot High`, `epinephrineNot High`, `milrinoneNot High`))

mat_train_all <- data.frame(cbind(df_intervals[df_intervals$train_test == 'Train', ]$death6week,
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
                              df_intervals[df_intervals$train_test == 'Train', ]$PCWP, 
                              df_intervals[df_intervals$train_test == 'Train', ]$hemoglobin, 
                              df_intervals[df_intervals$train_test == 'Train', ]$albumin, 
                              df_intervals[df_intervals$train_test == 'Train', ]$bilirubin,
                              df_intervals[df_intervals$train_test == 'Train', ]$eGFR, 
                              df_intervals[df_intervals$train_test == 'Train', ]$sodium, 
                              df_intervals[df_intervals$train_test == 'Train', ]$AST, 
                              df_intervals[df_intervals$train_test == 'Train', ]$BNP,
                              df_intervals[df_intervals$train_test == 'Train', ]$BNP_NT_Pro,
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
                              df_intervals[df_intervals$train_test == 'Train', ]$angina,
                              
                              df_intervals[df_intervals$train_test == 'Train', ]$temp_surg_ever,
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
                              df_intervals[df_intervals$train_test == 'Train', ]$BNP_slope))



names(mat_train_all) <- c('Death', 'Interval_1', 'Interval_2', 'Interval_3', 'Interval_4',
                          'Age10_1', 'Age10_2', 'Age10_3', 'Age10_4', 
                          'Diabetes', 'Dialysis',
                          'Diag_Amyloid', 'Diag_Congenital', 'Diag_Hypertrophic', 
                          'Diag_Ischemic', 'Diag_Other',
                          'Diag_Retransplant', 'Diag_Restricted', 'Diag_Valvular', 
                          'Dobutamine_High', 'Dopamine_High', 'Epinephrine_High', 'Milrinone_High',
                          'IABP', 'ECMO', 'LVAD', 'MCSD_Complication', 'SystolicBP',
                          'DiastolicBP', 'PASP', 'PADP', 'Heart_Rate10', 'Cardiac_Output',
                          'Central_Venous_Pressure', 'Arterial_Lactate', 'PCWP', 
                          'Hemoglobin', 'Albumin',
                          'Bilirubin', 'eGFR', 'Sodium', 'AST100', 'BNP100', 'BNP_NT_Pro', 'BUN10', 
                          'INR', 'Life_Arrhythmia', 'BiVAD_NoDischarge', 
                          'TempSurgery_LVAD', 'Other_DurableMCSD',
                          'MCSD_Malfunction', 'VTach_Fib', 'Perc_LVAD', 
                          'MCSD_Hemolysis', 'MCSD_Rhf', 'MCSD_DeviceInfection', 
                          'MCSD_Bleed', 'MCSD_Aortic_Insufficent', 'Angina',
                      
                          'Temp_Surg_Ever', 'BiVAD_No_Discharge_Ever', 
                          'IV_Inotropes_Ever', 'IABP_Ever', 'ECMO_Ever', 'BiVAD_Ever', 
                          'LVAD_Ever', 'RVAD_Ever', 'Short_MCS_Ever',
                          
                          'Sodium_Min', 'Creatinine_Min', 'Bilirubin_Min', 
                          'Albumin_Min', 'Arterial_Lactate_Min',
                          'BUN10_Min', 'AST100_Min', 'INR_Min', 'BNP100_Min',
                          
                          'Sodium_Max', 'Creatinine_Max', 'Bilirubin_Max', 
                          'Albumin_Max', 'Arterial_Lactate_Max',
                          'BUN10_Max', 'AST100_Max', 'INR_Max', 'BNP100_Max',
                          
                          'Sodium_Slope', 'Creatinine_Slope', 'Bilirubin_Slope', 
                          'Albumin_Slope', 'Arterial_Lactate_Slope', 
                          'BUN10_Slope', 'AST100_Slope', 'INR_Slope', 'BNP100_Slope')



df_intervals$death6week <- factor(df_intervals$death6week, levels=c(0,1))


###########
DMat_train_all <- xgb.DMatrix(data = 
                                model.matrix.lm(~ Interval_1 + Interval_2 + Interval_3 + Interval_4 + 
                                                  Age10_1 + Age10_2 + Age10_3 + Age10_4 + 
                                                  Diabetes + Dialysis + Diag_Congenital + Diag_Hypertrophic + 
                                                  Diag_Ischemic + Diag_Other + Diag_Retransplant + 
                                                  Diag_Restricted + Diag_Valvular + Dobutamine_High + 
                                                  Dopamine_High + Epinephrine_High + Milrinone_High + 
                                                  IABP + ECMO + LVAD + MCSD_Complication + SystolicBP +
                                                  DiastolicBP + PASP + PADP + Heart_Rate10 + 
                                                  Cardiac_Output + Central_Venous_Pressure + Arterial_Lactate +
                                                  Hemoglobin + Albumin + Bilirubin + eGFR + 
                                                  Sodium + AST100 + BNP100 + BNP_NT_Pro + BUN10 + INR + 
                                                  Life_Arrhythmia + BiVAD_NoDischarge + 
                                                  TempSurgery_LVAD + Other_DurableMCSD +
                                                  MCSD_Malfunction + VTach_Fib + Perc_LVAD + 
                                                  MCSD_Hemolysis + MCSD_Rhf + MCSD_DeviceInfection + 
                                                  MCSD_Bleed + MCSD_Aortic_Insufficent + Angina +
                                                  
                                                  Temp_Surg_Ever + BiVAD_No_Discharge_Ever + IV_Inotropes_Ever + 
                                                  IABP_Ever + ECMO_Ever + BiVAD_Ever + 
                                                  LVAD_Ever + RVAD_Ever + Short_MCS_Ever + 
                                                  
                                                  Sodium_Min + Creatinine_Min + Bilirubin_Min + Albumin_Min + 
                                                  Arterial_Lactate_Min + BUN10_Min + AST100_Min + 
                                                  INR_Min + BNP100_Min + 
                                                  
                                                  Sodium_Max + Creatinine_Max + Bilirubin_Max + Albumin_Max + 
                                                  Arterial_Lactate_Max + BUN10_Max + AST100_Max + 
                                                  INR_Max + BNP100_Max + 
                                                  
                                                  Sodium_Slope + Creatinine_Slope + Bilirubin_Slope + 
                                                  Albumin_Slope + Arterial_Lactate_Slope +
                                                  BUN10_Slope + AST100_Slope + INR_Slope + BNP100_Slope,
                                                
                                                data = mat_train_all, na.action = na.pass),
                              label = as.vector(mat_train_all$Death))



params_xgb <- expand.grid(
  eta = c(0.01, 0.03, 0.05, 0.1),
  max_depth = c(2, 4, 6, 10),
  gamma = c(0, 0.5, 1),
  colsample_bytree = c(0, 0.5, 1),
  subsample = c(0, 0.5, 1),
  scale_pos_weight = c(1, 4.5, 20),
  auc = 0)



for(i in seq_len(nrow(params_xgb))) {
  set.seed(200)
  model_xgboost_all <- xgb.cv(
    data = DMat_train_all,
    nrounds = 200,
    objective = 'binary:logistic',
    metrics = 'auc',
    nfold = 5,
    early_stopping_rounds = 100,
    params = list(
      eta = params_xgb$eta[i],
      max_depth = params_xgb$max_depth[i],
      gamma = params_xgb$gamma[i],
      colsample_bytree = params_xgb$colsample_bytree[i],
      subsample = params_xgb$subsample[i],
      scale_pos_weight = params_xgb$scale_pos_weight[i],
      nthread = 48
    )
  ) 
  
  params_xgb$auc[i] <- min(model_xgboost_all$evaluation_log$test_auc_mean)
}

save(params_xgb, file = './model_xgboost_mem_6wk.RData')



