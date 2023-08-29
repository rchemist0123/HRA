getwd()


# import data -------------------------------------------------------------

gj <- fread('../pcadata/nsc2_g1e_0915_db.csv')
bnc <- fread('working/pcadata/nsc2_bnc_db.csv')
bnd <- fread('working/pcadata/nsc2_bnd_db.csv')
getwd()

dt <- createCohort()


data <- preprocessing(dt, 'death', adv, 'impute')
# train(as.formula(form),data=data$train,method='glm')
fit_sex <- train_model(train = data$train, 
                   target = 'death',
                   features = adv,
                   strata='SEX')
summary(fit_sex$male$finalModel)
summary(fit_sex$female$finalModel)
summary(pred)

form <- paste('death','~',paste0(c(adv,'I(METs^2)'), collapse = '+'))
lr_male <- glm(as.formula(form),family=binomial, data=data$train[SEX==1])
lr_female <- glm(as.formula(form),family=binomial, data=data$train[SEX==2])

pred_male <- predict(lr_male, data$test[SEX==1], type='response')
pred_female<- predict(lr_male, data$test[SEX==2], type='response')
pred_male_bin <- ifelse(pred_male>=0.5,1,0) %>% as.factor()
pred_female_bin <- ifelse(pred_female>=0.5,1,0) %>% as.factor()


confusionMatrix(pred_male_bin, data$test[SEX==1]$death, positive = '1')
pROC::auc( data$test[SEX==1]$death ~ pred_male)
pROC::auc( data$test[SEX==2]$death ~ pred_female)

pred2_male <- prediction(pred_male,data$test[SEX==1]$death)
pred2_female <- prediction(pred_female,data$test[SEX==2]$death)

summary((pred_male)*1000)
summary((pred_female)*1000)

performance(pred2_male, 'auc')
performance(pred2_female, 'auc')

data$train[,..base] %>% summary()

require(survival)
coxph(Surv(DTH_DUR_M, death==1) ~ AGE + G1E_TOT_CHOL+G1E_HDL + G1E_LDL  + G1E_TG + G1E_FBS + G1E_HGB +
        G1E_SGOT + G1E_SGPT + G1E_GGT + G1E_BMI + G1E_WSTC + G1E_BP_SYS + G1E_BP_DIA +
        eGFR + SMK_COMBINED + DRK_COMBINED + METs +
        # Q_FHX_STK + Q_FHX_HTN + Q_FHX_HTDZ +Q_FHX_DM +
        I(METs ^ 2), data$train[SEX==1]) %>% 
  summary()

data$train[,.(death, DTH_DUR_M)] %>% summary()

coxph(Surv(DTH_DUR_M, death==1) ~ AGE ++ G1E_TOT_CHOL + G1E_HDL + G1E_LDL + G1E_TG + G1E_FBS + G1E_HGB +
        G1E_SGOT + G1E_SGPT + G1E_GGT + G1E_BMI + G1E_WSTC + G1E_BP_SYS + G1E_BP_DIA +
        eGFR + SMK_COMBINED + DRK_COMBINED + METs +
        # Q_FHX_STK + Q_FHX_HTN + Q_FHX_HTDZ +Q_FHX_DM +
        I(METs ^ 2), data$train[SEX==2]) %>% 
  summary()



require(R6)
R6Class(
  classname = 'lrModel',
  public = list(
    
  )
)
lrModeling <- R6class('lrModel',
                      
                      )

dt_death <- createCohort(target='death',washout=NULL)
dt_cancer <- createCohort(target='cancer', washout=6)
dt_mace <- createCohort(target='mace', washout= 6)
cvdDeath
bnd[substr(COD1,1,3) %in% c('I21','I22','I23','I63','G45','I62','I65','I67','I68','I20','I24','I25','I26','I27','I28','I30','I31','I32','I33','I34','I35','I36','I37','I38','I39','I40','I41','I42','I43','I44','I45','I46','I47','I48','I49','I51','I52','I64') |
      substr(COD2,1,3) %in% c('I21','I22','I23','I63','G45','I62','I65','I67','I68','I20','I24','I25','I26','I27','I28','I30','I31','I32','I33','I34','I35','I36','I37','I38','I39','I40','I41','I42','I43','I44','I45','I46','I47','I48','I49','I51','I52','I64')]
# cancer
# 'C16','C22','C18','C19','C20','C21', 'C33','C34','C61'
# 'C16','C22','C18','C19','C20','C21', 'C33','C34','C50'


require(data.table)
getwd()
names(m20_1619)
cancer_male <- fread('cancer_male.csv')
cancer_female <- fread('cancer_female.csv')
cancer_female[SICK_SYM2 %like% 'C50']
cancer_male[SICK_SYM1 %like% 'C61']

cancer_male[,.N,by=substr(SICK_SYM1,1,3)][order(-N)] %>% head(10)

rbindlist(list(m20[SICK_SYM1 %in% c('C16','C22','C18','C19','C20','C21', 'C33','C34','C61')|
                SICK_SYM2 %in% c('C16','C22','C18','C19','C20','C21', 'C33','C34','C61')],
          m20_1619[SICK_SYM1 %in% c('C16','C22','C18','C19','C20','C21', 'C33','C34','C61')|
                SICK_SYM2 %in% c('C16','C22','C18','C19','C20','C21', 'C33','C34','C61')])) %>% fwrite('cancer_male.csv')
rbindlist(list(m20[SICK_SYM1 %in% c('C16','C22','C18','C19','C20','C21', 'C33','C34','C50')|
                     SICK_SYM2 %in% c('C16','C22','C18','C19','C20','C21', 'C33','C34','C50')],
               m20_1619[SICK_SYM1 %in% c('C16','C22','C18','C19','C20','C21', 'C33','C34','C50')|
                          SICK_SYM2 %in% c('C16','C22','C18','C19','C20','C21', 'C33','C34','C50')])) %>% fwrite('cancer_female.csv')

getwd()
require(data.table)
require(dplyr)
m20 <- fread('nsc2_m20.csv')
m20_1619 <- fread('nsc2_m20_1619.csv')
# MACE



m20_mace<- m20[FORM_CD %in% c(2,7) & (substr(SICK_SYM1,1,3) %in% c('I63','G45', 'I62','I63','I20','I23','I24','I25','I26','I27','I28','I30','I31','I32','I33',
                                 'I34','I35','I36','I37','I38','I39','I40','I41','I42','I43','I44','I45','I46','I47','I48','I49','I51','I52','I64') |
      substr(SICK_SYM1,1,4) %in% c( 'I210','I211','I212','I213','I214','I219','I220','I221','I228','I229','I509',
                                          'I469', #suden cardiac arrest
                                          'R570',#cardiogenishock
                                          'I630','I631','I632') |
      substr(SICK_SYM2,1,3) %in% c('I63','G45', 'I62','I63','I20','I23','I24','I25','I26','I27','I28','I30','I31','I32','I33',
                                   'I34','I35','I36','I37','I38','I39','I40','I41','I42','I43','I44','I45','I46','I47','I48','I49','I51','I52','I64') |
      substr(SICK_SYM2,1,4) %in% c( 'I210','I211','I212','I213','I214','I219','I220','I221','I228','I229','I509',
                                    'I469', #suden cardiac arrest
                                    'R570',#cardiogenishock
                                    'I630','I631','I632'))]
m20_1619_mace<- m20_1619[FORM_CD %in% c(2,7) & (substr(SICK_SYM1,1,3) %in% c('I63','G45', 'I62','I63','I20','I23','I24','I25','I26','I27','I28','I30','I31','I32','I33',
                                            'I34','I35','I36','I37','I38','I39','I40','I41','I42','I43','I44','I45','I46','I47','I48','I49','I51','I52','I64') |
                 substr(SICK_SYM1,1,4) %in% c( 'I210','I211','I212','I213','I214','I219','I220','I221','I228','I229','I509',
                                               'I469', #suden cardiac arrest
                                               'R570',#cardiogenishock
                                               'I630','I631','I632') |
                 substr(SICK_SYM2,1,3) %in% c('I63','G45', 'I62','I63','I20','I23','I24','I25','I26','I27','I28','I30','I31','I32','I33',
                                              'I34','I35','I36','I37','I38','I39','I40','I41','I42','I43','I44','I45','I46','I47','I48','I49','I51','I52','I64') |
                 substr(SICK_SYM2,1,4) %in% c( 'I210','I211','I212','I213','I214','I219','I220','I221','I228','I229','I509',
                                               'I469', #suden cardiac arrest
                                               'R570',#cardiogenishock
                                               'I630','I631','I632'))]

m20_revas <- m20[substr(SICK_SYM1,1,5) %in% c ('HA670','HA680','HA681','HA682','M6551','M6552','M6561','M6562','M6563','M6465','M6571','M6572','M6634',
                                               'O1641','O1642','O1647','M6620','M6633','O2053','O2057','OA631','OA634','OA635','OB631','OB634','OB635')|
                   substr(SICK_SYM2,1,5) %in% c ('HA670','HA680','HA681','HA682','M6551','M6552','M6561','M6562','M6563','M6465','M6571','M6572','M6634',
                                                 'O1641','O1642','O1647','M6620','M6633','O2053','O2057','OA631','OA634','OA635','OB631','OB634','OB635')]
m20_1619_revas <- m20_1619[substr(SICK_SYM1,1,5) %in% c ('HA670','HA680','HA681','HA682','M6551','M6552','M6561','M6562','M6563','M6465','M6571','M6572','M6634','O1641','O1642','O1647','M6620','M6633','O2053','O2057','OA631','OA634','OA635','OB631','OB634','OB635')|
                             substr(SICK_SYM2,1,5) %in% c ('HA670','HA680','HA681','HA682','M6551','M6552','M6561','M6562','M6563','M6465','M6571','M6572','M6634',
                                                           'O1641','O1642','O1647','M6620','M6633','O2053','O2057','OA631','OA634','OA635','OB631','OB634','OB635')]

mace <- rbindlist(list(m20_revas, m20_1619_revas, m20_mace, m20_1619_mace))
mace_id <- mace[mace[order(MDCARE_STRT_DT),.I[1L],by=RN_INDI]$V1,.(RN_INDI,MDCARE_STRT_DT)]

fwrite(mace,'mace_raw.csv')
fwrite(mace_id, 'mace_id.csv')
rm(m20,m)
