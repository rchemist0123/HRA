require(caret)
set.seed(2022)

# 1) omit na -------------------------------
dt_ha_final <- na.omit(dt_ha)
dt_ha_final <- na.omit(dt_ha2)

# 2) impute na -----------------------------
dt_ha_impute <- copy(dt_ha)
dt_ha_impute <- copy(dt_ha2)

target <- c('MET_CON')
dt_ha_impute[,(target):=lapply(.SD, function(x) ifelse(is.na(x),mean(x,na.rm=T),x)),.SDcols=target]
names(dt_ha_impute)
target <- names(dt_ha_impute)[c(11:28,30:33)];target
dt_ha_impute[,(target):=lapply(.SD, function(x) ifelse(is.na(x), median(x,na.rm=T),x)),.SDcols=target]

train_id <- createDataPartition(dt_ha_final$ha, p=.7)
train <- dt_ha_final[train_id$Resample1]
test <- dt_ha_final[-train_id$Resample1]

train_id2 <- createDataPartition(dt_ha_impute$ha, p=.7)
train <- dt_ha_impute[train_id2$Resample1]
test <- dt_ha_impute[-train_id2$Resample1]


# modeling ----------------------------------------------------------------

basic_features <-　c('AGE', 'SEX','REGION', 'TOT_CHOLE','LDL_CHOLE', 'HDL_CHOLE',
                       'TRIGLYCERIDE','BLDS','HMG','SGOT_AST','SGPT_ALT','GAMMA_GTP',
                       'BMI','WAIST','BP_LWST','BP_HIGH','eGFR',
                    'SMK_STATUS', 'DRK',
                       'MET_CON' , 'I(MET_CON^2)')
advanced_features <- c(basic_features, 'FMLY_HPRTS_PATIEN_YN' , 'FMLY_APOP_PATIEN_YN' ,'FMLY_HDISE_PATIEN_YN' , 'FMLY_DIABML_PATIEN_YN')

tt <- preprocessing(dt_ha2, features = basic_features, na = 'impute', target='ha')

fit <- train_model(train_data = tt$tr, 
                   features = basic_features, 
                   target = 'ha', 
                   model = 'glm',
                   preprocess = 'range')
fit_male <- train_model(train_data=dt_ha2[SEX==1], 
                        features = basic_features, 
                        strata='SEX',
                        target = 'ha',
                        model = 'glm',
                        preprocess = 'range')
fit_female <- train_model(train_data=dt_ha2[SEX==0], 
                        features = basic_features, 
                        strata='SEX',
                        target = 'ha',
                        model = 'glm',
                        preprocess = 'range')
test_performance(fit, test=tt$te,target='ha')
test_performance(fit_male, test=test_dt,target='ha')
test_performance(fit_female, test=test_dt,target='ha')
# ROC & PRC curve-----------------------------

plot_performance(fit, test=tt$te,target='ha',type='auc')
plot_performance(fit, test=tt$te,target='ha',type='prc')
plot_performance(fit, test=tt$te,type='hist')
plot_performance(fit, test=tt$te,type='poly')

# validation-------------------------------------
test_dt[,pred_group := ifelse(ha_score<250,'group1',
                           ifelse(ha_score<500,'group2',
                                  ifelse(ha_score<750,'group3','group4')))]

test_dt[,.N,by=pred_group]
coxfit <- coxph(Surv(death_duration, death==1)~ha_score, test_dt)
coxfit %>% summary()
test_dt[,.N,by=death]

survfit <- survfit(Surv(death_duration, death==1)~pred_group, data=test_dt)
g <- ggsurvplot(survfit, 
           censor=F)
g$plot + 
  scale_color_brewer(palette = 'Set1',
                     name="",
                     label = c(
                       'score 0-250',
                               'score 250-500',
                               'score 500-750',
                               'score 750-1000')) +
  theme(legend.position = c(0.2, 0.15))


# mace --------------------------------------------------------------------

coxfit <- coxph(Surv(mace_duration, mace==1)~ha_score, test_dt)
coxfit %>% summary()
test_dt[,.N,by=mace]

survfit <- survfit(Surv(mace_duration, mace==1)~pred_group, data=test_dt)
g <- ggsurvplot(survfit, 
                fun = 'event',
                censor=F)
g$plot + 
  scale_y_continuous(limits = c(0,0.2))+
  scale_color_brewer(palette = 'Set1',
                     name="",
                     label = c(
                       'score 0-250',
                               'score 250-500',
                               'score 500-750',
                               'score 750-1000')) +
  theme(legend.position = c(0.2, 0.8))
test[mace_duration>=365,.N,by=mace_yn]
test[,summary(mace_duration)]


# cancer ------------------------------------------------------------------


coxfit <- coxph(Surv(cancer_male_duration, cancer==2)~ha_score, test_dt)
coxfit %>% summary()
test_dt[,.N,by=cancer_female]
survfit <- survfit(Surv(cancer_duration, cancer==2)~pred_group, data=test_dt)
g <- ggsurvplot(survfit,censor=F, fun='event')
g$plot + 
  # scale_y_continuous(limits = c(0,0.2))+
  scale_color_brewer(palette = 'Set1',
                     name="",
                     label = c(
                       'score 0-250',
                               'score 250-500',
                               'score 500-750',
                               'score 750-1000')) +
  theme(legend.position = c(0.2, 0.8))

step(fit$finalModel)


# calibration -------------------------------------------------------------
set.seed(2022)
tv <- sample(nrow(test_dt),0.5*nrow(test_dt))
test_t <- test_dt[tv]
test_v <- test_dt[-tv]
pred
platt.recal_pred <- lr_platt_recal_func(fit, test_v, pred, test_t, y='ha')
iso.recal_pred <- lr_iso_recal_func(fit, test_v, pred, test_t, y='ha')
summary((1-iso.recal_pred)*1000)
summary((1-platt.recal_pred)*1000)


ggplot(data=data.table(x=platt.recal_pred),
       aes(x=(1-x)*1000))+
  geom_histogram(bins=10000,
                 color='black',
                 fill='lightblue3',
                 binwidth = 20)+
  labs(x='Score', title='Platt') +
  # scale_x_continuous(limits = c(0,1000))+
  theme_bw()

ggplot(data=data.table(x=iso.recal_pred),
       aes(x=(1-x)*1000))+
  labs(x='Score', title='Isotonic recalibration') +
  geom_histogram(bins=10000,
                 color='black',
                 fill='lightblue3',
                 binwidth = 20)+
  # scale_x_continuous(limits = c(0,1000))+
  theme_bw()
