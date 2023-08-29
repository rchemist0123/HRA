names(hra_dt)
source('working/HRA/custom_functions.R',local = T)

hra_ml <- hra_dt2[,.(ill_yn,  ill_duration,
                    SEX, AGE_CAT, REGION,
                    HDL,LDL, TG, TCHOLE,
                    FBG, OBE, LIVER, BP,
                    # DRK_CON, MET_CON,
                    WC, DRK, PROTEURIA, HB, eGFR,
                    SMK, PA, CCI, T2DM_MED, HPTN_MED, DLPM_MED,
                    WC_burden_total, HDL_burden_total, OBE_burden_total,
                    PA_burden_total, SMK_burden_total, DRK_burden_total,
                    TG_burden_total, BP_burden_total,METS_burden_total,
                    FMLY_HPRTS_PATIEN_YN,FMLY_APOP_PATIEN_YN,FMLY_HDISE_PATIEN_YN,
                    FMLY_DIABML_PATIEN_YN,FMLY_CANCER_PATIEN_YN)] %>% na.omit();dim(hra_ml)
target <- hra_ml[,REGION:FMLY_CANCER_PATIEN_YN] %>% names()
hra_ml[,(target):=lapply(.SD, as.character),.SDcols=target]
hra_ml[,CCI := as.numeric(CCI)]


hra_ml[,(target):=lapply(.SD, as.numeric),.SDcols=target]
set.seed(2022)
split_id <- createDataPartition(hra_ml$ill_yn, p=0.7)
split_id2 <- createDataPartition(hra_ml2$ill_yn, p=0.7)

train <- hra_ml[split_id$Resample1]
test <- hra_ml[-split_id$Resample1]

train <- hra_ml2[split_id2$Resample1]
test <- hra_ml2[-split_id2$Resample1]


# modeling ----------------------------------------------------------------
require(caret)
require(pROC)
train %>% str()
fit1_1 <- glm(ill_yn ~  SEX + AGE_CAT + REGION +
                HDL + LDL + TG + TCHOLE +
                FBG + OBE + LIVER + BP +
                WC + PROTEURIA + HB + eGFR +
                SMK + DRK +  PA + CCI +
                T2DM_MED + HPTN_MED + DLPM_MED,
              family=binomial,
              train)
test[,pred_prob := predict(fit1_1, test, type='response')]
test[,pred_bin := ifelse(pred_prob>=0.5,1,0) %>% as.factor]
perfScore(data=test, pred_prob = 'pred_prob',
          pred_bin = 'pred_bin',
          fit = fit1_1, ref = 'ill_yn')

fit1_2 <- glm(ill_yn ~  SEX + AGE_CAT + REGION +
                HDL + LDL + TG + TCHOLE +
                FBG + OBE + LIVER + BP +
                WC + DRK + PROTEURIA + HB + eGFR +
                SMK + PA + CCI +
                T2DM_MED + HPTN_MED + DLPM_MED +
                WC_burden_total + HDL_burden_total + OBE_burden_total +
                PA_burden_total + SMK_burden_total + DRK_burden_total +
                TG_burden_total + BP_burden_total +
                METS_burden_total, family=binomial,train)

test[,pred_prob2 := predict(fit1_2, test, type='response')]
test[,pred_bin2 := ifelse(pred_prob2>=0.5,1,0) %>% as.factor]
perfScore(data=test, pred_prob = 'pred_prob2',
          pred_bin = 'pred_bin2',
          fit = fit1_2, ref = 'ill_yn')


getNRI(data=test,
       ref = 'ill_yn',
       pred_bin = 'pred_bin',
       pred_bin2 = 'pred_bin1_3')

fit1_3 <- glm(ill_yn ~  SEX + AGE_CAT + REGION +
                HDL + LDL + TG + TCHOLE +
                FBG + OBE + LIVER + BP +
                WC + DRK + PROTEURIA + HB + eGFR +
                SMK + PA + CCI +
                T2DM_MED + HPTN_MED + DLPM_MED +
                WC_burden_total + HDL_burden_total + OBE_burden_total +
                PA_burden_total + SMK_burden_total + DRK_burden_total +
                TG_burden_total + BP_burden_total +
                METS_burden_total + FMLY_HPRTS_PATIEN_YN +
                FMLY_APOP_PATIEN_YN + FMLY_HDISE_PATIEN_YN +
                FMLY_DIABML_PATIEN_YN + FMLY_CANCER_PATIEN_YN, family=binomial,
              train)


test[,pred_prob3 := predict(fit1_3, test, type='response')]
test[,pred_bin3 := ifelse(pred_prob3>=0.5,1,0) %>% as.factor]

perfScore(data=test, pred_prob = 'pred_prob3',
          pred_bin = 'pred_bin3',
          fit = fit1_3, ref = 'ill_yn')
getNRI(data = test, ref = 'ill_yn',
       pred_bin = 'pred_bin',
       pred_bin2 = 'pred_bin3')

train_dummy %>% names()


# with caret --------------------------------------------------------------
hra_ml <- na.omit(hra_dt2)
set.seed(2022)
split_id <- createDataPartition(hra_ml$ill_yn, p=0.7)

feature1 <-  c('SEX' , 'AGE_CAT' , 'REGION' ,'HDL', 'LDL', 'TG' , 'TCHOLE' ,
             'FBG', 'OBE', 'LIVER' , 'BP' ,'WC' , 'DRK', 'PROTEURIA' , 'HB' , 'eGFR',
             'SMK' , 'PA', 'CCI','T2DM_MED', 'HPTN_MED', 'DLPM_MED')
feature2 <- c(feature1,'WC_burden_total', 'HDL_burden_total', 'OBE_burden_total',
                'PA_burden_total', 'SMK_burden_total', 
              'DRK_burden_total', 'METS_burden_total',
                'TG_burden_total', 'BP_burden_total')
feature3 <- c(feature2, 'FMLY_HPRTS_PATIEN_YN','FMLY_APOP_PATIEN_YN', 'FMLY_HDISE_PATIEN_YN','FMLY_DIABML_PATIEN_YN')

mod1 <- train_model(features = feature1,
                    model = 'logistic',
                    target = 'ill_yn',
                    data = hra_dt2,
                    na.method = 'omit',
                    preprocess = 'range')
test_performance(fit = mod1, target='ill_yn')

mod2 <- train_model(features = feature2,
                    model = 'logistic',
                    target = 'ill_yn',
                    data = hra_dt2,
                    na.method = 'omit',
                    preprocess = 'range')
test_performance(fit = mod2, target='ill_yn')

mod3 <- train_model(features = feature3,
                    model = 'logistic',
                    target = 'ill_yn',
                    data = hra_dt2,
                    na.method = 'omit',
                    preprocess = 'range')
test_performance(fit = mod3, target='ill_yn')
mod3$finalModel
plot_performance(fit=mod3, type='roc')
plot_performance(fit=mod3, type='prc')
plot_performance(fit=mod3, type='hist')


## xgboost ----------------------
hra_ml <- na.omit(hra_dt)
str(hra_ml)

xgb_fit <- train_model(data = hra_dt3, model = 'xgboost',
                       features = feature1, target = 'ill_yn',na.method = 'omit',preprocess = 'range')
test_performance(fit=xgb_fit, target = 'ill_yn')

xgb_fit <- train_model(data = hra_dt3, model = 'xgboost',
                       features = feature2, target = 'ill_yn',na.method = 'omit',preprocess = 'range')
test_performance(fit=xgb_fit, target = 'ill_yn')

xgb_fit <- train_model(data = hra_dt3, model = 'xgboost',
                       features = feature3,target = 'ill_yn',na.method = 'omit',preprocess = 'range')
test_performance(fit=xgb_fit, target = 'ill_yn')

## cox ---------------------------

coxfit <- train_model(data = hra_dt3, model = 'cox',features = feature1,
                      target = 'ill_yn',na.method = 'omit',preprocess = 'range')
c(AIC(coxfit),summary(coxfit)[['concordance']])

coxfit <- train_model(data = hra_dt3, model = 'cox',features = feature2,
                      target = 'ill_yn',na.method = 'omit',preprocess = 'range')
c(AIC(coxfit),summary(coxfit)[['concordance']])

coxfit <- train_model(data = hra_dt3, model = 'cox',features = feature3,
            target = 'ill_yn',na.method = 'omit',preprocess = 'range')
c(AIC(coxfit),summary(coxfit)[['concordance']])

confusionMatrix(test$pred_bin3, test$ill_yn,positive='1',
                mode = 'everything')

# histogram ---------------------------------------------------------------


test[,ha := (1-pred_prob3)*1000]
ggplot(test,aes(x=ha))+
  geom_histogram(bins=10000,
                 color='black',
                 fill='lightblue3',
                 binwidth = 20)+
  # scale_x_continuous(limits = c(0,1000))+
  theme_bw()

summary(test$ha)

# ROC ---------------------------------------------------------------------

glm_roc <- pROC::roc(test$ill_yn~test$pred_prob3);glm_roc
plot.roc(glm_roc,
         asp=NA,
         legacy.axes = T,
         print.auc.y=.4,
         print.auc = T,
         col='orange')
prauc <- PRAUC(y_true = test$ha, y_pred=test$pred);prauc

varImp(fit1)
# PRC
test$pred_bin3
pred_2 <- prediction(test$pred_prob3,test$ill_yn)
perf <- performance(pred_2, 'prec','rec')
plot(perf,
     avg='threshold',
     main=paste0('Precison/Recall Curve: ',round(prauc,4)),
     col='red')

# validation -------------------------------------------------------------------
require(survival)

require(survminer)
test[,pred_group := ifelse(pred<250,'group1',
                       ifelse(pred<500,'group2',
                        ifelse(pred<750,'group3','group4')))]

coxfit <- coxph(Surv(death_duration, death==1)~pred, test)
coxfit %>% summary()

survfit <- survfit(Surv(death_duration, death==1)~pred_group, data=test[death_duration>=365])
ggsurvplot(survfit, 
           data= test[death_duration>=365],
           censor=F)
test[death_duration>=365,.N,by=death]

coxfit <- coxph(Surv(mace_duration, mace_yn==1)~pred, test)
coxfit %>% summary()

survfit <- survfit(Surv(mace_duration, mace_yn==1)~pred_group, data=test[mace_duration>=365])
ggsurvplot(survfit, 
           data=test[mace_duration>=365],
           censor=F)
test[mace_duration>=365,.N,by=mace_yn]
test[,summary(mace_duration)]

coxfit <- coxph(Surv(cancer_male_duration, cancer_male==1)~pred, test)
coxfit %>% summary()

survfit <- survfit(Surv(mace_duration, mace_yn==1)~pred_group, data=test[mace_duration>=365])
ggsurvplot(survfit, 
           data=test[mace_duration>=365],
           censor=F)
test[mace_duration>=365,.N,by=mace_yn]
test[,summary(mace_duration)]


test[,summary(cancer_male_duration)]
test[,.N,by=cancer_male]
test[,.N,by=cancer_female]
