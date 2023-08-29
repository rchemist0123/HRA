
getwd()
folder_path<-'/userdata06/room058'
#folder_path<-'C:\\Users\\kakao\\Desktop\\project\\HRA\\OnGoing\\nhis'
setwd(folder_path)

# library  
library(haven)
library(dplyr)
library(readxl)
library(survival)
library(tidyverse)
library(rwa)
library(car)
library(neuralnet)
library(e1071)
library(stringr)
library(randomForest)
library(xgboost)
library(caret)
library(pROC)
library(Epi)
# function 

count_id<-function(data){
  data_temp<-data%>%select(RN_INDI)
  data_temp<-unique(data_temp)
  return(dim(data_temp)[1])
}

DB_parser <- function(raw_g1e0915,birth_data,folderpath, parameter){
  #
  'ORIGINAL '
  #
  ## 출생 및 사망테이블(출생년도) 추가
  # read csv
  
  #raw_year<-read.csv('working/pcadata/nsc2_bnd_db.csv')
  raw_year<-birth_data
  raw_year_save<-raw_year
  
  # 이상 결측치 확인
  # summary(!is.na(raw_year$BTH_YYYY))
  # table(raw_year$BTH_YYYY) # 이상치 발견
  temp1a<-dim(raw_year)[1]
  raw_year<-raw_year %>% filter(BTH_YYYY!='1921LE')
  temp1b<-dim(raw_year)[1]
  
  print(paste('all_data:',temp1a,'->before 1921  removed :',temp1a-temp1b))
  
  raw_year$BTH_YYYY <- as.character(raw_year$BTH_YYYY) # chr -> num
  raw_year$BTH_YYYY <- as.numeric(raw_year$BTH_YYYY)
  
  ## 건강검진 테이블(검진데이터) 추가
  #raw_g1e0915<-read.csv('working/pcadata/nsc2_g1e_0915_db.csv')
  raw_g1e0915_save<-raw_g1e0915
  # 이상 결측치 확인
  # table(raw_g1e0915$EXMD_BZ_YYYY) 
  # summary(!is.na(raw_g1e0915$EXMD_BZ_YYYY)) 
  raw_g1e0915$EXMD_BZ_YYYY <- as.numeric(raw_g1e0915$EXMD_BZ_YYYY)
  # 검진데이터 결합
  temp_for_cal_age<-left_join(raw_year, raw_g1e0915,by="RN_INDI")
  
  # age 계산
  temp_for_cal_age$age <- temp_for_cal_age$EXMD_BZ_YYYY-temp_for_cal_age$BTH_YYYY
  temp_for_cal_time<- temp_for_cal_age%>%select(RN_INDI ,age) %>% group_by(RN_INDI) %>% summarize(min= min(age))
  temp_for_cal_age<-left_join(temp_for_cal_age, temp_for_cal_time, by='RN_INDI')
  temp_for_cal_age$time<-temp_for_cal_age$age - temp_for_cal_age$min
  
  
  ## sex항 추가
  raw_bnc <- raw_bnc%>%select(-X)
  ##List of people who had medical check at least once##
  # sex항 결합 
  temp_dataset <- left_join(temp_for_cal_age, raw_bnc,  by ="RN_INDI")
  dataset_for_cox<-temp_dataset %>% select(RN_INDI, BTH_YYYY,EXMD_BZ_YYYY,age, time)
  
  
  '####### all dataset #########'
  all_dataset <-temp_dataset
  all_dataset$G1E_URN_PROT <- all_dataset$G1E_URN_PROT -1
  all_dataset$SEX <- as.numeric(all_dataset$SEX)
  #eGFR 계산공식(MDRD GFR equation)
  ifelse((all_dataset$SEX == 2),
         all_dataset$eGFR <- 0.742*186.3*((all_dataset$G1E_CRTN)^(-1.154))*((all_dataset$age)^(-0.203)),
         all_dataset$eGFR <- 186.3*((all_dataset$G1E_CRTN)^(-1.154))*((all_dataset$age)^(-0.203)))
  
  '## 과거력 있었으면 history =1 ##'
  
  all_dataset$history <- ifelse((all_dataset$Q_PHX_DX_STK==1|all_dataset$Q_PHX_DX_HTDZ==1|all_dataset$Q_PHX_DX_HTN==1|all_dataset$Q_PHX_DX_DM==1|
                                   all_dataset$Q_PHX_DX_DLD==1|all_dataset$	Q_PHX_TX_STK==1|all_dataset$Q_PHX_TX_HTDZ==1|
                                   all_dataset$Q_PHX_TX_HTN==1|all_dataset$Q_PHX_TX_DM==1|all_dataset$	Q_PHX_TX_DLD==1),1,0)
  
  all_dataset$history[is.na(all_dataset$history)] = 0
  
  # family_parameter_AND_exercise_parameter <-c('Q_FHX_STK',	'Q_FHX_HTDZ',	'Q_FHX_HTN',	'Q_FHX_DM',	'Q_FHX_ETC','Q_PA_VD',	'Q_PA_MD',	'Q_PA_WALK' 	)
  # other_parameter<-   c( 'Q_SMK_YN'	,'Q_SMK_PRE_DRT',	'Q_SMK_PRE_AMT',	'Q_SMK_NOW_DRT'	,'Q_SMK_NOW_AMT_V09N'	,                       'Q_DRK_FRQ_V09N'	,'Q_DRK_AMT_V09N'	 )
  
  # exercise
  all_dataset$Q_PA_VD[is.na(all_dataset$Q_PA_VD)] = 0
  all_dataset$Q_PA_MD[is.na(all_dataset$Q_PA_MD)] = 0
  all_dataset$Q_PA_WALK[is.na(all_dataset$Q_PA_WALK)] = 0
  all_dataset$PA<-all_dataset$Q_PA_WALK*2.9*30+all_dataset$Q_PA_MD*4*30+all_dataset$Q_PA_VD*7*20
  all_dataset$PA2 <- all_dataset$PA* all_dataset$PA
  
  #smoke
  all_dataset$Q_SMK_PRE_DRT[is.na(all_dataset$Q_SMK_PRE_DRT)] = 0
  all_dataset$Q_SMK_PRE_AMT[is.na(all_dataset$Q_SMK_PRE_AMT)] = 0
  all_dataset$Q_SMK_NOW_DRT[is.na(all_dataset$Q_SMK_NOW_DRT)] = 0
  all_dataset$Q_SMK_NOW_AMT_V09N[is.na(all_dataset$Q_SMK_NOW_AMT_V09N)] = 0
  all_dataset$Q_SMK_YN[is.na(all_dataset$Q_SMK_YN)] = 0
  all_dataset$PY_now<-all_dataset$Q_SMK_NOW_DRT * all_dataset$Q_SMK_NOW_AMT_V09N /20
  all_dataset$PY_pre<-all_dataset$Q_SMK_PRE_DRT * all_dataset$Q_SMK_PRE_AMT /20
  all_dataset$SMK <- ifelse(all_dataset$PY_now>0|all_dataset$Q_SMK_YN==3,2,ifelse(all_dataset$PY_pre>0|all_dataset$Q_SMK_YN==2,1,0))
  all_dataset$SMK <- ifelse(all_dataset$PY_now>0, all_dataset$PY_now,  ifelse(all_dataset$PY_pre>0, all_dataset$PY_pre,0))
  
  # drink
  all_dataset$Q_DRK_FRQ_V09N[is.na(all_dataset$Q_DRK_FRQ_V09N)] = 0
  all_dataset$Q_DRK_AMT_V09N[is.na(all_dataset$Q_DRK_AMT_V09N)] = 0
  
  all_dataset$DRKDANGER<-ifelse(all_dataset$Q_DRK_AMT_V09N>=7|all_dataset$Q_DRK_FRQ_V09N>=3,2,ifelse(all_dataset$Q_DRK_AMT_V09N>=1|all_dataset$Q_DRK_FRQ_V09N>=1,1,0))
  #if (all_dataset$SEX==1){
  #  all_dataset$DRKDANGER<-ifelse(all_dataset$Q_DRK_AMT_V09N>=7|all_dataset$Q_DRK_FRQ_V09N>=3,2,ifelse(all_dataset$Q_DRK_AMT_V09N>=1|all_dataset$Q_DRK_FRQ_V09N>=1,1,0))
  #} 
  #if (all_dataset$SEX==2){
  #  all_dataset$DRKDANGER<-ifelse(all_dataset$Q_DRK_AMT_V09N>=5|all_dataset$Q_DRK_FRQ_V09N>=3,2,ifelse(all_dataset$Q_DRK_AMT_V09N>=1|all_dataset$Q_DRK_FRQ_V09N>=1,1,0))
  # }
  all_dataset$BW<- all_dataset$Q_DRK_FRQ_V09N*all_dataset$Q_DRK_AMT_V09N/7
  all_dataset$DRK<-all_dataset$BW
  all_dataset$DRK2<-all_dataset$DRK*all_dataset$DRK
  
  all_dataset$familyhistory<- ifelse((all_dataset$Q_FHX_STK==1|all_dataset$Q_FHX_HTDZ==1| all_dataset$Q_FHX_ETC==1|
                                        all_dataset$Q_FHX_HTN==1|all_dataset$Q_FHX_DM==1),1,0)
  all_dataset$familyhistory[is.na(all_dataset$familyhistory)] = 0
  
  all_dataset_save<-all_dataset
  all_dataset<-all_dataset_save
  
  all_dataset0912 <-all_dataset %>%filter(EXMD_BZ_YYYY %in% c(2009, 2010, 2011, 2012))
  temp_0912<- count_id(all_dataset0912)
  print(paste('0912 data:',temp_0912))
  print("0912data should be added people who born at 1921 before")
  
  # NA
  tempa<- count_id(all_dataset)
  all_dataset<- all_dataset %>% filter(0<=age&age<90) %>% filter(0<=G1E_BMI & G1E_BMI<=40000) %>% 
    filter(0<=G1E_WSTC&G1E_WSTC<=1200000) %>% filter(0<=G1E_BP_SYS&G1E_BP_SYS<=180000000) %>% 
    filter(0<=G1E_BP_DIA & G1E_BP_DIA<=120000000) %>%
    filter(0<=G1E_FBS & G1E_FBS<=250000000) %>% 
    filter(0<=G1E_TOT_CHOL & G1E_TOT_CHOL<=400000000) %>% 
    filter(0<=G1E_TG & G1E_TG<=5500000) %>% 
    filter(0<=G1E_HDL & G1E_HDL<=2500000) %>%
    filter(G1E_LDL<=22000000) %>% filter(0<=G1E_HGB&G1E_HGB<=10000) %>% 
    filter(0<=G1E_CRTN&G1E_CRTN<=200000000) %>%
    filter(G1E_SGOT<=800000000) %>% filter(G1E_SGPT<=1200000000) %>% filter(G1E_GGT<=25000000000)  %>% 
    filter(0<=G1E_URN_PROT) %>% filter(0<=eGFR & eGFR<=2200000000)
     tempb<- count_id(all_dataset)
  print(paste('all_data:',tempa,'->who has NA :',tempa-tempb))
  
  # age>30
  tempa<- count_id(all_dataset)
  all_dataset<- all_dataset %>% filter(30<=age)
  tempb<- count_id(all_dataset)
  print(paste('all_data:',tempa,'->who UNDER 30 :',tempa-tempb))
  
  
  # outlier no
   tempa<- count_id(all_dataset)
   all_dataset<- all_dataset %>% filter(30<=age) %>% filter(10<=G1E_BMI & G1E_BMI<=40) %>% 
     filter(60<=G1E_WSTC&G1E_WSTC<=150) %>% filter(50<=G1E_BP_SYS&G1E_BP_SYS<=300) %>% 
     filter(40<=G1E_BP_DIA & G1E_BP_DIA<=150) %>%
     filter(50<=G1E_FBS & G1E_FBS<=400) %>% 
     filter(50<=G1E_TOT_CHOL & G1E_TOT_CHOL<=500) %>% 
     filter(30<=G1E_TG & G1E_TG<=500) %>% 
     filter(20<=G1E_HDL & G1E_HDL<=250) %>%
     filter(G1E_LDL<=250) %>% filter(5<=G1E_HGB&G1E_HGB<=20) %>% 
     filter(0.3<=G1E_CRTN&G1E_CRTN<=2.5) %>%
     filter(G1E_SGOT<=200) %>% filter(G1E_SGPT<=200) %>% filter(G1E_GGT<=250)  %>% 
     filter(G1E_URN_PROT %in% c(0,1,2,3,4)) %>% filter(0<eGFR & eGFR<=250)%>% 
     filter(SMK < 100)%>%filter(PA < 2500)%>% filter(PA2 < 6250000)%>% filter(DRK < 20) %>% filter(DRK2 < 400)
  tempb<- count_id(all_dataset)
  print(paste('all_data:',tempa,'->who has outlier :',tempa-tempb))
  
  #medication
  all_dataset$Q_PHX_DX_STK[is.na(all_dataset$Q_PHX_DX_STK)] = 0
  all_dataset$Q_PHX_TX_DLD[is.na(all_dataset$Q_PHX_TX_DLD)] = 0
  all_dataset$Q_PHX_DX_HTDZ[is.na(all_dataset$Q_PHX_DX_HTDZ)] = 0
  all_dataset$Q_PHX_DX_DM[is.na(all_dataset$Q_PHX_DX_DM)] = 0
  all_dataset$Q_PHX_DX_HTN[is.na(all_dataset$Q_PHX_DX_HTN)] = 0
  all_dataset$medication<- ifelse((all_dataset$Q_PHX_DX_STK==1|all_dataset$Q_PHX_TX_DLD==1| all_dataset$Q_PHX_DX_HTDZ==1|
                                     all_dataset$Q_PHX_DX_DM==1|all_dataset$Q_PHX_DX_HTN==1),1,0)
  
  #family history
  all_dataset$Q_FHX_STK[is.na(all_dataset$Q_FHX_STK)] = 0
  all_dataset$Q_FHX_HTDZ[is.na(all_dataset$Q_FHX_HTDZ)] = 0
  all_dataset$Q_FHX_DM[is.na(all_dataset$Q_FHX_DM)] = 0
  all_dataset$Q_FHX_HTN[is.na(all_dataset$Q_FHX_HTN)] = 0
  all_dataset$Q_FHX_STK<-as.factor(all_dataset$Q_FHX_STK)
  all_dataset$Q_FHX_HTDZ<-as.factor(all_dataset$Q_FHX_HTDZ)
  all_dataset$Q_FHX_DM<-as.factor(all_dataset$Q_FHX_DM)
  all_dataset$Q_FHX_HTN<-as.factor(all_dataset$Q_FHX_HTN)
  all_dataset_save_temp<-all_dataset
  all_dataset<-all_dataset_save_temp
  
  # all
  
  all_dataset <- all_dataset %>% select((c(parameter,'history','PA','PA2','SMK','DRK','DRK2','familyhistory','medication',
                                           'Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_DM','Q_FHX_HTN',
                                           'Q_PHX_DX_STK','Q_PHX_TX_DLD','Q_PHX_DX_HTDZ','Q_PHX_DX_DM','Q_PHX_DX_HTN','DRKDANGER')))

  
  
  print(paste('no outlier, age over 30 : ', tempb))
  
  print('all age, no outlier, all year')
  print(paste('man :',count_id(filter(all_dataset, SEX==1))))
  print(paste('woman :',count_id(filter(all_dataset, SEX==2))))
  
  
  
  # age에 따라 수식 다르게 저장
  all_dataset20<- all_dataset %>% filter(20<=age & age<30)
  print(paste('20-30 :',count_id(all_dataset20)))
  print(paste('man :',count_id(filter(all_dataset20, SEX==1))))
  print(paste('woman :',count_id(filter(all_dataset20, SEX==2))))
  
  all_dataset30<- all_dataset %>% filter(30<=age & age<40)
  print(paste('30-40 :',count_id(all_dataset30)))
  print(paste('man :',count_id(filter(all_dataset30, SEX==1))))
  print(paste('woman :',count_id(filter(all_dataset30, SEX==2))))
  
  all_dataset40<- all_dataset %>% filter(40<=age & age<50)
  print(paste('40-50 :',count_id(all_dataset40)))
  print(paste('man :',count_id(filter(all_dataset40, SEX==1))))
  print(paste('woman :',count_id(filter(all_dataset40, SEX==2))))
  
  all_dataset50<- all_dataset %>% filter(50<=age & age<60)
  print(paste('50-60 :',count_id(all_dataset50)))
  print(paste('man :',count_id(filter(all_dataset50, SEX==1))))
  print(paste('woman :',count_id(filter(all_dataset50, SEX==2))))
  
  all_dataset60<- all_dataset %>% filter(60<=age & age<70)
  print(paste('60-89 :',count_id(all_dataset60)))
  print(paste('man :',count_id(filter(all_dataset60, SEX==1))))
  print(paste('woman :',count_id(filter(all_dataset60, SEX==2))))
  
  
  
  # 0915_dataset 저장
  write.csv(all_dataset, 'working/result_pca_r/dataset_0915_.csv')
  write.csv(all_dataset20, 'working/result_pca_r/dataset_0915_20.csv')
  write.csv(all_dataset30, 'working/result_pca_r/dataset_0915_30.csv')
  write.csv(all_dataset40, 'working/result_pca_r/dataset_0915_40.csv')
  write.csv(all_dataset50, 'working/result_pca_r/dataset_0915_50.csv')
  write.csv(all_dataset60, 'working/result_pca_r/dataset_0915_60.csv')
  write.csv(dataset_for_cox,'working/result_pca_r/dataset_for_cox.csv' )
  print('')
  print('')
  
  
  
  #0912 with burden
  print(' #### ## #### #### ##### ## ## ')
  print("0912 dataset number")
  print(' #### ## #### #### ##### ## ## ')
  
  print('')
  print('')
  print(paste('man :',count_id(filter(all_dataset0912, SEX==1))))
  print(paste('woman :',count_id(filter(all_dataset0912, SEX==2))))
  
  # NA
  tempa<- count_id(all_dataset0912)
  
  all_dataset0912<- all_dataset0912 %>% filter(0<=age&age<90) %>% filter(0<=G1E_BMI & G1E_BMI<=40000) %>% 
    filter(0<=G1E_WSTC&G1E_WSTC<=1200000) %>% filter(0<=G1E_BP_SYS&G1E_BP_SYS<=180000000) %>% 
    filter(0<=G1E_BP_DIA & G1E_BP_DIA<=120000000) %>%
    filter(0<=G1E_FBS & G1E_FBS<=250000000) %>% 
    filter(0<=G1E_TOT_CHOL & G1E_TOT_CHOL<=400000000) %>% 
    filter(0<=G1E_TG & G1E_TG<=5500000) %>% 
    filter(0<=G1E_HDL & G1E_HDL<=2500000) %>%
    filter(G1E_LDL<=22000000) %>% filter(0<=G1E_HGB&G1E_HGB<=10000) %>% 
    filter(0<=G1E_CRTN&G1E_CRTN<=200000000) %>%
    filter(G1E_SGOT<=800000000) %>% filter(G1E_SGPT<=1200000000) %>% filter(G1E_GGT<=25000000000)  %>% 
    filter(0<=G1E_URN_PROT) %>% filter(0<=eGFR & eGFR<=2200000000)
  tempb<- count_id(all_dataset0912)
  print(paste('0912 data:',tempa,'->who has NA :',tempa-tempb))
  
  # age>30
  tempa<- count_id(all_dataset0912)
  all_dataset0912<- all_dataset0912 %>% filter(30<=age)
  tempb<- count_id(all_dataset0912)
  print(paste('0912 DATA WITH NO NA:',tempa,'->who UNDER 30 :',tempa-tempb))
  
  
  # outlier no
  tempa<- count_id(all_dataset0912)
  all_dataset0912<- all_dataset0912 %>% filter(30<=age) %>% filter(10<=G1E_BMI & G1E_BMI<=40) %>% 
    filter(60<=G1E_WSTC&G1E_WSTC<=150) %>% filter(50<=G1E_BP_SYS&G1E_BP_SYS<=300) %>% 
    filter(40<=G1E_BP_DIA & G1E_BP_DIA<=200) %>%
    filter(50<=G1E_FBS & G1E_FBS<=400) %>% 
    filter(50<=G1E_TOT_CHOL & G1E_TOT_CHOL<=500) %>% 
    filter(30<=G1E_TG & G1E_TG<=500) %>% 
    filter(20<=G1E_HDL & G1E_HDL<=250) %>%
    filter(G1E_LDL<=250) %>% filter(5<=G1E_HGB&G1E_HGB<=20) %>% 
    filter(0.3<=G1E_CRTN&G1E_CRTN<=2.5) %>%
    filter(G1E_SGOT<=200) %>% filter(G1E_SGPT<=200) %>% filter(G1E_GGT<=250)  %>% 
    filter(G1E_URN_PROT %in% c(0,1,2,3,4)) %>% filter(0<eGFR & eGFR<=250)%>% 
    filter(SMK < 100)%>%filter(PA < 2500)%>% filter(PA2 < 6250000)%>% filter(DRK < 20) %>% filter(DRK2 < 400)
  
  tempb<- count_id(all_dataset0912)
  print(paste('0912 data no NA:',tempa,'->who has outlier :',tempa-tempb))
  
  #medication
  all_dataset0912$Q_PHX_DX_STK[is.na(all_dataset0912$Q_PHX_DX_STK)] = 0
  all_dataset0912$Q_PHX_TX_DLD[is.na(all_dataset0912$Q_PHX_TX_DLD)] = 0
  all_dataset0912$Q_PHX_DX_HTDZ[is.na(all_dataset0912$Q_PHX_DX_HTDZ)] = 0
  all_dataset0912$Q_PHX_DX_DM[is.na(all_dataset0912$Q_PHX_DX_DM)] = 0
  all_dataset0912$Q_PHX_DX_HTN[is.na(all_dataset0912$Q_PHX_DX_HTN)] = 0
  all_dataset0912$medication<- ifelse((all_dataset0912$Q_PHX_DX_STK==1|all_dataset0912$Q_PHX_TX_DLD==1| all_dataset0912$Q_PHX_DX_HTDZ==1|
                                     all_dataset0912$Q_PHX_DX_DM==1|all_dataset0912$Q_PHX_DX_HTN==1),1,0)
  
  #family history
  all_dataset0912$Q_FHX_STK[is.na(all_dataset0912$Q_FHX_STK)] = 0
  all_dataset0912$Q_FHX_HTDZ[is.na(all_dataset0912$Q_FHX_HTDZ)] = 0
  all_dataset0912$Q_FHX_DM[is.na(all_dataset0912$Q_FHX_DM)] = 0
  all_dataset0912$Q_FHX_HTN[is.na(all_dataset0912$Q_FHX_HTN)] = 0
  all_dataset$Q_FHX_STK<-as.factor(all_dataset$Q_FHX_STK)
  all_dataset$Q_FHX_HTDZ<-as.factor(all_dataset$Q_FHX_HTDZ)
  all_dataset$Q_FHX_DM<-as.factor(all_dataset$Q_FHX_DM)
  all_dataset$Q_FHX_HTN<-as.factor(all_dataset$Q_FHX_HTN)
  
  print("0912 dataset over 30 without NA")
  print(paste('total: ', count_id(all_dataset0912)))

  print(paste('man :',count_id(filter(all_dataset0912, SEX==1))))
  print(paste('woman :',count_id(filter(all_dataset0912, SEX==2))))
  
  all_dataset0912 <- all_dataset0912 %>% select((c(parameter,'history','PA','PA2','SMK','DRK','DRK2','familyhistory','medication','DRKDANGER',
                                                   'Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_DM','Q_FHX_HTN',
                                                   'Q_PHX_TX_STK','Q_PHX_TX_DLD','Q_PHX_TX_HTDZ','Q_PHX_TX_DM','Q_PHX_TX_HTN')))
  print(' ')
  
  print(' burden calculation ')
  print(' ')
  
  'obesity_burden'
  length(which(all_dataset0912$G1E_BMI>=25))
  burden_0912<-all_dataset0912[which(all_dataset0912$G1E_BMI>=25),'RN_INDI']
  n_burden_0912<-data.frame(table(burden_0912))
  
  RN_INDI_burden_0912<- n_burden_0912%>%filter(Freq>=3)%>%select(burden_0912)
  temp_burden <- all_dataset0912%>%filter(RN_INDI%in%RN_INDI_burden_0912[,1])
  temp_burden$obesity_burden <-1
  temp_burden<- temp_burden%>%select(RN_INDI, obesity_burden)
  temp_burden<-unique(temp_burden)
  all_dataset0912  <- left_join(all_dataset0912,temp_burden, by="RN_INDI")
  all_dataset0912$obesity_burden[is.na(all_dataset0912$obesity_burden)] = 0
  cat('obesity_burden : ', table(all_dataset0912$obesity_burden), '\n')
  
  'bp_burden'
  burden_0912<-all_dataset0912[which(all_dataset0912$G1E_BP_SYS>=130|all_dataset0912$G1E_BP_DIA>=85|all_dataset0912$Q_PHX_TX_HTN==1),'RN_INDI']
  n_burden_0912<-data.frame(table(burden_0912))
  
  RN_INDI_burden_0912<- n_burden_0912%>%filter(Freq>=3)%>%select(burden_0912)
  temp_burden <- all_dataset0912%>%filter(RN_INDI%in%RN_INDI_burden_0912[,1])
  temp_burden$bp_burden <-1
  temp_burden<- temp_burden%>%select(RN_INDI, bp_burden)
  temp_burden<-unique(temp_burden)
  
  all_dataset0912  <- left_join(all_dataset0912,temp_burden, by="RN_INDI")
  all_dataset0912$bp_burden[is.na(all_dataset0912$bp_burden)] = 0
  cat('bp_burden : ', table(all_dataset0912$bp_burden), '\n')
  
  
  
  'fbs_burden'
  burden_0912<-all_dataset0912[which(all_dataset0912$G1E_FBS>=100|all_dataset0912$Q_PHX_TX_DM==1),'RN_INDI']
  n_burden_0912<-data.frame(table(burden_0912))
  
  RN_INDI_burden_0912<- n_burden_0912%>%filter(Freq>=3)%>%select(burden_0912)
  temp_burden <- all_dataset0912%>%filter(RN_INDI%in%RN_INDI_burden_0912[,1])
  temp_burden$fbs_burden <-1
  temp_burden<- temp_burden%>%select(RN_INDI, fbs_burden)
  temp_burden<-unique(temp_burden)
  
  all_dataset0912  <- left_join(all_dataset0912,temp_burden, by="RN_INDI")
  all_dataset0912$fbs_burden[is.na(all_dataset0912$fbs_burden)] = 0
  
  cat('fbs_burden : ', table(all_dataset0912$fbs_burden), '\n')
  
  
  'wc_burden'
  all_dataset0912_1<-all_dataset0912%>%filter(SEX==1)
  burden_0912_1<-all_dataset0912_1[which(all_dataset0912_1$G1E_WSTC>=90),'RN_INDI']
  all_dataset0912_2<-all_dataset0912%>%filter(SEX==2)
  burden_0912_2<-all_dataset0912_2[which(all_dataset0912_2$G1E_WSTC>=85),'RN_INDI']
  n_burden_0912_1<-data.frame(table(burden_0912_1))
  n_burden_0912_2<-data.frame(table(burden_0912_2))
  colnames(n_burden_0912_1) <-c("burden_0912", "Freq")
  colnames(n_burden_0912_2) <-c("burden_0912", "Freq")
  n_burden_0912<-rbind(n_burden_0912_1,n_burden_0912_2)
  
  RN_INDI_burden_0912<- n_burden_0912%>%filter(Freq>=3)%>%select(burden_0912)
  temp_burden <- all_dataset0912%>%filter(RN_INDI%in%RN_INDI_burden_0912[,1])
  temp_burden$wc_burden <-1
  temp_burden<- temp_burden%>%select(RN_INDI, wc_burden)
  temp_burden<-unique(temp_burden)
  
  all_dataset0912  <- left_join(all_dataset0912,temp_burden, by="RN_INDI")
  all_dataset0912$wc_burden[is.na(all_dataset0912$wc_burden)] = 0
  
  cat('wc_burden : ', table(all_dataset0912$wc_burden), '\n')
  
  
  
  'hdl_burden'
  all_dataset0912_1<-all_dataset0912%>%filter(SEX==1)
  burden_0912_1<-all_dataset0912_1[which(all_dataset0912_1$G1E_HDL<40|all_dataset0912_1$Q_PHX_TX_DLD==1),'RN_INDI']
  all_dataset0912_2<-all_dataset0912%>%filter(SEX==2)
  burden_0912_2<-all_dataset0912_2[which(all_dataset0912_2$G1E_HDL<50|all_dataset0912_2$Q_PHX_TX_DLD==1),'RN_INDI']
  n_burden_0912_1<-data.frame(table(burden_0912_1))
  n_burden_0912_2<-data.frame(table(burden_0912_2))
  colnames(n_burden_0912_1) <-c("burden_0912", "Freq")
  colnames(n_burden_0912_2) <-c("burden_0912", "Freq")
  n_burden_0912<-rbind(n_burden_0912_1,n_burden_0912_2)
  
  RN_INDI_burden_0912<- n_burden_0912%>%filter(Freq>=3)%>%select(burden_0912)
  temp_burden <- all_dataset0912%>%filter(RN_INDI%in%RN_INDI_burden_0912[,1])
  temp_burden$hdl_burden <-1
  temp_burden<- temp_burden%>%select(RN_INDI, hdl_burden)
  temp_burden<-unique(temp_burden)
  
  all_dataset0912  <- left_join(all_dataset0912,temp_burden, by="RN_INDI")
  all_dataset0912$hdl_burden[is.na(all_dataset0912$hdl_burden)] = 0
  
  cat('hdl_burden : ', table(all_dataset0912$hdl_burden), '\n')
  
  
  'tg_burden'
  burden_0912<-all_dataset0912[which(all_dataset0912$G1E_TG>=150|all_dataset0912$Q_PHX_TX_DLD==1),'RN_INDI']
  n_burden_0912<-data.frame(table(burden_0912))
  
  RN_INDI_burden_0912<- n_burden_0912%>%filter(Freq>=3)%>%select(burden_0912)
  temp_burden <- all_dataset0912%>%filter(RN_INDI%in%RN_INDI_burden_0912[,1])
  temp_burden$tg_burden <-1
  temp_burden<- temp_burden%>%select(RN_INDI, tg_burden)
  temp_burden<-unique(temp_burden)
  
  all_dataset0912  <- left_join(all_dataset0912,temp_burden, by="RN_INDI")
  all_dataset0912$tg_burden[is.na(all_dataset0912$tg_burden)] = 0
  
  cat('tg_burden : ', table(all_dataset0912$tg_burden), '\n')
  
  'pa_burden'
  q25<-quantile(all_dataset0912$PA,0.25)
  burden_0912<-all_dataset0912[which(all_dataset0912$PA<=q25),'RN_INDI']
  n_burden_0912<-data.frame(table(burden_0912))
  
  RN_INDI_burden_0912<- n_burden_0912%>%filter(Freq>=3)%>%select(burden_0912)
  temp_burden <- all_dataset0912%>%filter(RN_INDI%in%RN_INDI_burden_0912[,1])
  temp_burden$pa_burden <-1
  temp_burden<- temp_burden%>%select(RN_INDI, pa_burden)
  temp_burden<-unique(temp_burden)
  
  all_dataset0912  <- left_join(all_dataset0912,temp_burden, by="RN_INDI")
  all_dataset0912$pa_burden[is.na(all_dataset0912$pa_burden)] = 0
  
  cat('pa_burden : ', table(all_dataset0912$pa_burden), '\n')
  
  'smoking_burden'
  burden_0912<-all_dataset0912[which(all_dataset0912$SMK!=0),'RN_INDI']
  n_burden_0912<-data.frame(table(burden_0912))
  
  RN_INDI_burden_0912<- n_burden_0912%>%filter(Freq>=3)%>%select(burden_0912)
  temp_burden <- all_dataset0912%>%filter(RN_INDI%in%RN_INDI_burden_0912[,1])
  temp_burden$smoking_burden <-1
  temp_burden<- temp_burden%>%select(RN_INDI, smoking_burden)
  temp_burden<-unique(temp_burden)
  
  all_dataset0912  <- left_join(all_dataset0912,temp_burden, by="RN_INDI")
  all_dataset0912$smoking_burden[is.na(all_dataset0912$smoking_burden)] = 0
  
  cat('smoking_burden : ', table(all_dataset0912$smoking_burden), '\n')
  
  'drinking_burden'
  table(all_dataset0912$DRKDANGER)
  burden_0912<-all_dataset0912[which(all_dataset0912$DRKDANGER==2),'RN_INDI']
  n_burden_0912<-data.frame(table(burden_0912))
  
  RN_INDI_burden_0912<- n_burden_0912%>%filter(Freq>=3)%>%select(burden_0912)
  temp_burden <- all_dataset0912%>%filter(RN_INDI%in%RN_INDI_burden_0912[,1])
  temp_burden$drinking_burden <-1
  temp_burden<- temp_burden%>%select(RN_INDI, drinking_burden)
  temp_burden<-unique(temp_burden)
  
  all_dataset0912  <- left_join(all_dataset0912,temp_burden, by="RN_INDI")
  all_dataset0912$drinking_burden[is.na(all_dataset0912$drinking_burden)] = 0
  
  cat('drinking_burden : ', table(all_dataset0912$drinking_burden), '\n')
  all_dataset0912<-all_dataset0912%>%select(-c(Q_PHX_TX_STK,Q_PHX_TX_DLD,Q_PHX_TX_HTDZ,Q_PHX_TX_DM,Q_PHX_TX_HTN))
  
  print(' ')
  print(' ')
  
    # age에 따라 수식 다르게 저장
  all_dataset091220<- all_dataset0912 %>% filter(20<=age & age<30)
  print(paste('20-30 :',count_id(all_dataset091220)))
  print(paste('man :',count_id(filter(all_dataset091220, SEX==1))))
  print(paste('woman :',count_id(filter(all_dataset091220, SEX==2))))
  
  all_dataset091230<- all_dataset0912 %>% filter(30<=age & age<40)
  print(paste('30-40 :',count_id(all_dataset091230)))
  print(paste('man :',count_id(filter(all_dataset091230, SEX==1))))
  print(paste('woman :',count_id(filter(all_dataset091230, SEX==2))))
  
  all_dataset091240<- all_dataset0912 %>% filter(40<=age & age<50)
  print(paste('40-50 :',count_id(all_dataset091240)))
  print(paste('man :',count_id(filter(all_dataset091240, SEX==1))))
  print(paste('woman :',count_id(filter(all_dataset091240, SEX==2))))
  
  all_dataset091250<- all_dataset0912 %>% filter(50<=age & age<60)
  print(paste('50-60 :',count_id(all_dataset091250)))
  print(paste('man :',count_id(filter(all_dataset091250, SEX==1))))
  print(paste('woman :',count_id(filter(all_dataset091250, SEX==2))))
  
  all_dataset091260<- all_dataset0912 %>% filter(60<=age & age<70)
  print(paste('60-89 :',count_id(all_dataset091260)))
  print(paste('man :',count_id(filter(all_dataset091260, SEX==1))))
  print(paste('woman :',count_id(filter(all_dataset091260, SEX==2))))
  
  
  
  # all_dataset0912 저장
  write.csv(all_dataset0912, 'working/result_pca_r/dataset_0912_.csv')
  write.csv(all_dataset091220, 'working/result_pca_r/dataset_0912_20.csv')
  write.csv(all_dataset091230, 'working/result_pca_r/dataset_0912_30.csv')
  write.csv(all_dataset091240, 'working/result_pca_r/dataset_0912_40.csv')
  write.csv(all_dataset091250, 'working/result_pca_r/dataset_0912_50.csv')
  write.csv(all_dataset091260, 'working/result_pca_r/dataset_0912_60.csv')
  write.csv(dataset_for_cox,'working/result_pca_r/dataset_for_cox.csv' )
  print('')
  print('')
  
  '## 건강한 dataset 구축 ##'
  dataset_healthy<-all_dataset0912
  temp3a<-count_id(dataset_healthy)
  # normal range를 정해 이상치 제외 # strict하게 잡아야한다. 건강나이는 건강한 사람이 기준이되어서 하는것.
  
  dataset_healthy<- dataset_healthy %>% filter(age>=30) %>% filter(15<=G1E_BMI & G1E_BMI<=30) %>% 
    filter(105>=G1E_WSTC&G1E_WSTC>=60) %>% filter(80<=G1E_BP_SYS&G1E_BP_SYS<=160) %>% 
    filter(G1E_BP_DIA>=50 & G1E_BP_DIA<=100) %>% 
    filter(G1E_FBS<=140 & G1E_FBS>=50) %>% 
    filter(G1E_TOT_CHOL<=260 & G1E_TOT_CHOL>=50) %>% 
    filter(G1E_TG>=50 & G1E_TG<=400) %>% 
    filter(G1E_HDL<=90 & G1E_HDL>=20) %>% 
    filter(G1E_LDL <=190) %>% filter(G1E_HGB<=18&G1E_HGB>=10) %>% 
    filter(G1E_CRTN<=2&G1E_CRTN>=0.4) %>%
    filter(G1E_SGOT<=60) %>% filter(G1E_SGPT<=80) %>% filter(G1E_GGT<=150)  %>% filter(G1E_URN_PROT %in% c(0,1,2,3,4))
  
  temp3b<-count_id(dataset_healthy)
  print(paste('0912 no outlier no NA over 30 :',temp3a,'->who are not in healthy range :',temp3a-temp3b))
  
  
  temp4a<-count_id(dataset_healthy)
  dataset_healthy<-dataset_healthy%>% filter(history!=1)%>%select(-history)
  temp4b<-count_id(dataset_healthy)
  
  print(paste('0912 healthy range:',temp4a,'->who has already chronic disease :',temp4a-temp4b))
  print('')
  
  print('0912 dataset, healthy range, no past disease')
  print(paste('total: ', count_id(dataset_healthy)))
  print(paste('man :',count_id(filter(dataset_healthy, SEX==1))))
  print(paste('woman :',count_id(filter(dataset_healthy, SEX==2))))
  
  
  # age에 따라 수식 다르게 저장
  
  dataset_healthy20<-  dataset_healthy %>% filter(20<=age & age<30)
  print(paste('20-30 :',count_id(dataset_healthy20)))
  print(paste('man :',count_id(filter(dataset_healthy20, SEX==1))))
  print(paste('woman :',count_id(filter(dataset_healthy20, SEX==2))))
  
  dataset_healthy30<-  dataset_healthy %>% filter(30<=age & age<40)
  print(paste('30-40 :',count_id(dataset_healthy30)))
  print(paste('man :',count_id(filter(dataset_healthy30, SEX==1))))
  print(paste('woman :',count_id(filter(dataset_healthy30, SEX==2))))
  
  dataset_healthy40<-  dataset_healthy %>% filter(40<=age & age<50)
  print(paste('40-50 :',count_id(dataset_healthy40)))
  print(paste('man :',count_id(filter(dataset_healthy40, SEX==1))))
  print(paste('woman :',count_id(filter(dataset_healthy40, SEX==2))))
  
  
  dataset_healthy50<-  dataset_healthy %>% filter(50<=age & age<60)
  print(paste('50-60 :',count_id(dataset_healthy50)))
  print(paste('man :',count_id(filter(dataset_healthy50, SEX==1))))
  print(paste('woman :',count_id(filter(dataset_healthy50, SEX==2))))
  
  dataset_healthy60<-  dataset_healthy %>% filter(60<=age)
  print(paste('60-89 :',count_id(dataset_healthy60)))
  print(paste('man :',count_id(filter(dataset_healthy60, SEX==1))))
  print(paste('woman :',count_id(filter(dataset_healthy60, SEX==2))))
  
  
  #  dataset_healthy 저장
  write.csv( dataset_healthy, 'working/result_pca_r/dataset_0912_healthy_.csv')
  write.csv( dataset_healthy20, 'working/result_pca_r/dataset_0912_healthy_20.csv')
  write.csv( dataset_healthy30, 'working/result_pca_r/dataset_0912_healthy_30.csv')
  write.csv( dataset_healthy40, 'working/result_pca_r/dataset_0912_healthy_40.csv')
  write.csv( dataset_healthy50, 'working/result_pca_r/dataset_0912_healthy_50.csv')
  write.csv( dataset_healthy60, 'working/result_pca_r/dataset_0912_healthy_60.csv')
  print('')
  print('### healthy condition group saved ###')
  print('')
  
}

select_norm_data_and_cov_matrix <- function(data_specific_gender_all_components, gender, components) {
  data_specific_gender_all_components
  data_specific_gender <- data_specific_gender_all_components[,components]
  data_specific_gender_scale<-data_specific_gender
  data_specific_gender_scale[,-1] <- scale(data_specific_gender_scale[,-1])
  df_use = data_specific_gender_scale[,-1]
  cov_matrix= cov(df_use)
  
  return(cov_matrix)
}

cov_matrix_to_PCA_table <- function(cov_matrix, components){
  component2 <- components[-c(1,2)] # age, sex 제거
  eigen_val <- eigen(cov_matrix[-1,-1])$values
  eigen_vec <- eigen(cov_matrix[-1,-1])$vectors
  eigen_val_sum <- sum(eigen_val)  
  pc_count <- sum(eigen_val>=1)
  eigen_val_t <- t(data.frame(eigen_val))
  eigen_table <- rbind(eigen_vec, eigen_val_t)
  X_name <- seq(data.frame(dim(eigen_table))[2,])
  Y_name <- append(component2, 'eigenvalue')
  rownames(eigen_table) <- Y_name
  colnames(eigen_table) <-X_name
  return(eigen_table)
}

PC_to_BAS <- function(data_specific_gender,age_label, gender, pc_number_is_1 = TRUE, components=selected_components){
  filename_pc <- paste0('working/result_pca_r/PC_variable_table_selected_',gender,'_',age_label,'.csv')
  data_pc <- read.csv(filename_pc)
  eigenval_sum <-sum(tail(data_pc, n=1)[-1])
  pc_count <- sum(tail(data_pc, n=1)[-1]>=1)
  pc1 <- data_pc[,c(1,2)]
  colnames(pc1)<-c('parameter','PC1')
  
  # pc1의 부호 결정은 HDL이 음수가 되도록 한다. 단 eigenvalue 값은 양수
  pc1_pm <- pc1 %>% filter(parameter=='G1E_HDL')
  if (pc1_pm[1,2]>0){
    pc1$PC1<- pc1$PC1*(-1)
    pc1[pc1$parameter=='eigenvalue',2]<-pc1[pc1$parameter=='eigenvalue',2]*-1
  }
  
  
  data_specific_gender <- data_specific_gender%>% filter(SEX==gender)
  
  BAS <- data.frame(matrix(0, nrow=dim(pc1)[1], ncol=2))
  colnames(BAS) <- c('parameter', 'BAS')  
  sc <- append(selected_components[-c(1,2)], 'Constant')
  BAS[,'parameter'] <-sc
  
  eigenvalue<-pc1[pc1$parameter=='eigenvalue',2]
  
  if (pc_number_is_1 ==T) {
    Constant=0
    
    for (i in 1:dim(pc1)[1]){
      if (i !=dim(pc1)[1] ){
        
        sdvalue <- apply(data_specific_gender[as.character(pc1[i,1])],2,sd)
        meanvalue <-apply(data_specific_gender[as.character(pc1[i,1])],2,mean)
        BAS[i,2]=(eigenvalue/eigenval_sum)*pc1[i,2]/sdvalue
        Constant <-  -1 * meanvalue*  BAS[i,2]+Constant
        
      }
      if (i ==dim(pc1)[1]  ){
        BAS[i,2] = Constant
      }
    }
  }
  return(BAS)
}

BA <- function(data_specific_gender, BAS){
  mean_age <- apply(data_specific_gender['age'],2,mean)
  std_age<-apply(data_specific_gender['age'],2,sd)
  
  BA<-BAS
  BA['BAS'] <-0
  
  for (i in 1:dim(BA)[1]){
    if (i !=dim(BA)[1] ){
      BA[i,2] <- BAS[i,2]*std_age
    }
    if (i ==dim(BA)[1] ){
      BA[i,2] <- BAS[i,2]*std_age+mean_age
    }
  }
  return(BA)
}

BA_CA_linear_fit<-function(data_with_BA){
  model <- lm(data_with_BA$biologic_age~data_with_BA$age)
  return (model)
}

corrected_BA<-function(data_specific_gender, BA_parameters, model){
  mean_age <- apply(data_specific_gender['age'],2,mean)
  std_age<-apply(data_specific_gender['age'],2,sd)
  corrected_BA_parameters<-BA_parameters
  
  corrected_BA_parameters[dim(BA_parameters)[1]+1,1]='age'
  corrected_BA_parameters[dim(BA_parameters)[1]+1,2]=(1- as.numeric(coef(model)[2]))
  corrected_BA_parameters[dim(BA_parameters)[1],2] = BA_parameters[dim(BA_parameters)[1],2] - (1- as.numeric(coef(model)[2]))*mean_age
  # constant 제일 뒤로
  Constant_BA<-subset(corrected_BA_parameters, parameter=='Constant')
  corrected_BA_parameters<-rbind(subset(corrected_BA_parameters, parameter!='Constant'), Constant_BA)
  # age 가장 앞으로
  age_BA<-subset(corrected_BA_parameters, parameter=='age')
  corrected_BA_parameters<-rbind(age_BA, subset(corrected_BA_parameters, parameter!='age'))
  return (corrected_BA_parameters)
  
}

BA_add_to_dataset <-function(all_data,components, dictionary, gender, corrected){
  if (corrected==F){
    component2 <- components[-c(1,2)]
    data_specific_gender_temp<-all_data[component2]
    data_specific_gender_temp['Constant_column']<-1
    data_specific_gender_matrix <-as.matrix(data_specific_gender_temp)
    data_specific_gender_BA <- data_specific_gender_matrix %*% as.matrix(dictionary[,2])
    final_component <- append('RN_INDI', component2)
    data_with_BA<-all_data[final_component]
    data_with_BA['biologic_age']<-data_specific_gender_BA
  }else {
    component2 <- components[-1]
    data_specific_gender_temp<-all_data[component2]
    data_specific_gender_temp['Constant_column']<-1
    data_specific_gender_matrix <-as.matrix(data_specific_gender_temp)
    data_specific_gender_BA <- data_specific_gender_matrix %*% as.matrix(dictionary[,2])
    final_component <- append('RN_INDI', component2)
    data_with_BA<-all_data[final_component]
    data_with_BA['biologic_age']<-data_specific_gender_BA
    
  }
  return (data_with_BA)
}

BA_add_to_dataset_ML <-function(ML_model, all_data,components, dictionary, gender, corrected){
  if (corrected==F){
    component2 <- components[-c(1,2)]
    data_specific_gender_temp<-all_data[component2]
    data_specific_gender_temp['Constant_column']<-1
    data_specific_gender_matrix <-as.matrix(data_specific_gender_temp)
    data_specific_gender_BA <- data_specific_gender_matrix %*% as.matrix(dictionary[,2])
    final_component <- append('RN_INDI', component2)
    data_with_BA<-all_data[final_component]
    data_with_BA['biologic_age']<-data_specific_gender_BA
  }else {
    component2 <- components[-1]
    data_specific_gender_temp<-all_data[component2]
    
    data_specific_gender_temp_scaled<-scale(data_specific_gender_temp)
    sc_center<-attr(data_specific_gender_temp_scaled, 'scaled:center')
    sc_std<-attr(data_specific_gender_temp_scaled, 'scaled:scale')
    
    test_result<-predict(ML_model,subset(data_specific_gender_temp_scaled, select=-age))
    sc_age_ML <- (data.frame(test_result))$test_result
    age_ML<-sc_age_ML*sc_std['age']+sc_center['age']
    data_specific_gender_temp[,'age']<-age_ML
    data_specific_gender_temp['Constant_column']<-1
    
    #### ## 
    data_specific_gender_matrix <-as.matrix(data_specific_gender_temp)
    
    data_specific_gender_BA <- data_specific_gender_matrix %*% as.matrix(dictionary[,2])
    final_component <- append('RN_INDI', component2)
    data_with_BA<-all_data[final_component]
    data_with_BA['biologic_age']<-data_specific_gender_BA
    
  }
  return (data_with_BA)
}

PCA_model_R<-function(age_label, gender,selected_components, ML_TF=FALSE){
  #### 1. sex, age를 기준으로 데이터 로딩 
  
  components_all= c('SEX', 'age', 'G1E_HGHT', 'G1E_WGHT','G1E_BMI', 'G1E_WSTC', 'G1E_BP_SYS', 'G1E_BP_DIA', 'G1E_FBS','G1E_TOT_CHOL', 'G1E_TG', 'G1E_HDL', 'G1E_LDL', 'G1E_HGB','G1E_URN_PROT',
                    'G1E_CRTN', 'G1E_SGOT', 'G1E_SGPT', 'G1E_GGT', 'eGFR', 'DRK', 'DRK2', 'SMK', 'PA', 'PA2','familyhistory','Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_DM','Q_FHX_HTN',
                    'obesity_burden','bp_burden','fbs_burden','wc_burden','hdl_burden','tg_burden','pa_burden','drinking_burden','smoking_burden')
  file_name <- paste0('working/result_pca_r/dataset_0912_healthy_',age_label,'.csv')
  data_12 = read.csv(file_name)
  all_data<-data_12%>%filter(SEX==gender)
  
  df_raw_12<-data_12 
  data_specific_gender_all_components <- df_raw_12 %>% filter(df_raw_12$SEX==gender)
  # left one data of an one person at youngest age
  data_temp<- data_specific_gender_all_components %>% group_by(RN_INDI) %>% summarise(mage=min(age))
  data_temp$age<-data_temp$mage
  data_specific_gender_all_components <- left_join(data_temp, data_specific_gender_all_components, by=c('RN_INDI','age'))
  
  data_specific_gender <- data_specific_gender_all_components[,components_all]
  data_specific_gender_scale<-data_specific_gender
  data_specific_gender_scale[,-1] <- scale(data_specific_gender_scale[,-1]) # normalize 필요한 변수 age~eGFR 에 대해 진행
  
  
  #### 2. select component and normalize data and get covariance matrix
  cov_norm  <- select_norm_data_and_cov_matrix(data_specific_gender_all_components=data_specific_gender_all_components, gender=gender, components=components_all)
  write.csv(cov_norm, paste0('/userdata06/room058/working/result_pca_r/covariance_matrix_all_',gender,'_',age_label,'.csv'))
  
  
  #### 3. get PC vs varaible_all_include_age table
  pca_table <- cov_matrix_to_PCA_table(cov_matrix=cov_norm, components=components_all)
  write.csv(pca_table, paste0('/userdata06/room058/working/result_pca_r/PC_variable_Table_all_',gender,'_',age_label,'.csv'))
  
  #### 4. selected components PCA
  cov_norm_selected <- select_norm_data_and_cov_matrix(data_specific_gender_all_components=data_specific_gender_all_components, gender=gender, components=selected_components)
  write.csv(cov_norm_selected, paste0('/userdata06/room058/working/result_pca_r/covariance_matrix_selected_',gender,'_',age_label,'.csv'))
  result <- cov_matrix_to_PCA_table(cov_matrix=cov_norm_selected, components=selected_components)
  write.csv(result, paste0('/userdata06/room058/working/result_pca_r/PC_variable_table_selected_',gender,'_',age_label,'.csv'))
  
  
  #### 5. PC_variable_table to Biological Age Score 
  BAS_parameters <- PC_to_BAS(data_specific_gender=data_specific_gender,age_label=age_label, gender=gender, pc_number_is_1=T, components=selected_components)
  BA_parameters <- BA(data_specific_gender=data_specific_gender, BAS_parameters)
  
  #### 6. BA-CA fit
  all_data<-data_12%>%filter(SEX==gender)
  data_with_BA <- BA_add_to_dataset( all_data=all_data, dictionary =BA_parameters, 
                                     components=selected_components, corrected=F)
  data_with_BA_linear_fit<- cbind(data_with_BA, age=all_data$age) 
  model <- BA_CA_linear_fit(data_with_BA_linear_fit)
  
  #### 7. Correction
  corrected_BA_parameters <- corrected_BA(data_specific_gender=data_specific_gender, BA_parameters, model=model)

  if (ML_TF==FALSE){
    
    #### 8. Calculate BA to all 0912 dataset
    file_name <- paste0('working/result_pca_r/dataset_0912_',age_label,'.csv')
    data_0912_12 = read.csv(file_name)
    data_0912_12<-data_0912_12%>%filter(SEX==gender)
    
    data_with_BA <- BA_add_to_dataset( all_data=data_0912_12,dictionary =corrected_BA_parameters, components=selected_components, corrected=T)
    write.csv(data_with_BA, paste0('/userdata06/room058/working/result_pca_r/data_with_BA_',gender,'_',age_label,'.csv'))
    colnames(corrected_BA_parameters)<-c('parameter', 'coef')
    write.csv(corrected_BA_parameters, paste0('/userdata06/room058/working/result_pca_r/corrected_biological_age_coef_',gender,'_',age_label,'.csv'))
  } else if(ML_TF==TRUE){
    ML_model<-RF_DM(data_12=data_12 , SEED=12345, save=FALSE)
    # ML_model<-readRDS('working/pcadata/ML_model.rda')
   
     #### 8. Calculate BA to all 0912 dataset
    file_name <- paste0('working/result_pca_r/dataset_0912_',age_label,'.csv')
    data_0912_12 = read.csv(file_name)
    data_0912_12<-data_0912_12%>%filter(SEX==gender)
    data_with_BA_ML <- BA_add_to_dataset_ML(ML_model=ML_model, all_data=data_0912_12, dictionary =corrected_BA_parameters, components=selected_components, corrected=T)
    write.csv(data_with_BA_ML, paste0('/userdata06/room058/working/result_pca_r/data_with_BA_',gender,'_',age_label,'_ML.csv'))
    colnames(corrected_BA_parameters)<-c('parameter', 'coef')
    write.csv(corrected_BA_parameters, paste0('/userdata06/room058/working/result_pca_r/corrected_biological_age_coef_',gender,'_',age_label,'_ML.csv'))
  }
  return(corrected_BA_parameters)
}


disease_and_BA <- function(BA_data,dataset_for_cox,birth_data,disease_target, strings){
  # 국민기록
  MK_data<- MK_data %>% select(RN_INDI, SICK_SYM1,SICK_SYM2, SYMcode, STD_YYYY)
  
  # data_with_BA_path <- paste0('/userdata06/room058/working/result_pca_r/data_with_BA_',gender,'_',age_label,'.csv')
  # BA_data<-read.csv(data_with_BA_path)
  
  
  #dataset_for_cox_path<- 'working/result_pca_r/dataset_for_cox_bigger_cohort_.csv'
  #dataset_for_cox<-read.csv('working/result_pca_r/dataset_for_cox_bigger_cohort_.csv')
  
  BA_data <- left_join(BA_data, dataset_for_cox, by=c('RN_INDI','age'))
  
  # str_detect용 library
  # install.packages('tidyverse')
  MK_data$temp_disease = str_detect(MK_data$SYMcode, paste(strings, collapse = "|"))*1
  MK_data$dz<-MK_data$temp_disease
  
  # 연 두차례 이상 방문 확인 # time consume too much
  # MK_data$ID_SICK_YEAR <-paste0(MK_data$RN_INDI,MK_data$SICK_SYM1, MK_data$STD_YYYY) 
  # MK_data$go_to_hospital_more_than_twice <- duplicated(MK_data$ID_SICK_YEAR)*1
  # MK_data$dz <- ifelse(MK_data$go_to_hospital_more_than_twice*MK_data$temp_disease==1, 1, 0)
  
  # MK_data_save2<-MK_data
  
  # MK_data_with_disease=1
  MK_data_with_disease <- MK_data %>% filter(dz==1) %>%select(RN_INDI, SICK_SYM1, SICK_SYM2, dz, STD_YYYY)
  
  
  # 나이
  # birth_data<-read.csv('working/pcadata/nsc2_bnd_db.csv')
  birth_data<-birth_data %>% select(RN_INDI, BTH_YYYY)
  birth_data<-birth_data %>% filter(BTH_YYYY!='1921LE')
  birth_data$BTH_YYYY <- as.character(birth_data$BTH_YYYY) # chr -> num
  birth_data$BTH_YYYY <- as.numeric(birth_data$BTH_YYYY)
  MK_data_with_disease <- left_join(MK_data_with_disease,birth_data, by='RN_INDI')
  MK_data_with_disease$age <- MK_data_with_disease$STD_YYYY-MK_data_with_disease$BTH_YYYY
  
  ## youngest age
  MK_data_with_disease_temp<-MK_data_with_disease%>%select(RN_INDI, age)
  BA_data_temp<-BA_data%>% select(RN_INDI, age,time)
  BA_data_temp$medicalcheckage<-BA_data_temp$age
  BA_data_temp <- BA_data_temp %>% select(RN_INDI, medicalcheckage,time)
  ba_mk_temp<-left_join(BA_data_temp,MK_data_with_disease_temp, by='RN_INDI')
  ba_mk_temp_tm <- ba_mk_temp %>%group_by(RN_INDI) %>% summarise(minmcage=min(medicalcheckage), minage=min(age))
  
  
  #get disease at most 5year before from first medical checkup
  ba_mk_temp_tmtm <-ba_mk_temp_tm %>% filter(minmcage<=minage& minage<minmcage+5)
  ba_mk_temp_tmtm$whohavedisease<-1
  ba_mk_temp_tmtm$futtime <- ba_mk_temp_tmtm$minage - ba_mk_temp_tmtm$minmcage
  
  # seperate and paste
  BA_data<-BA_data %>% select(RN_INDI,age,biologic_age, time)
  BA_data$BA_CA <- BA_data$biologic_age-BA_data$age
  
  BA_data_with_event<-left_join(BA_data, ba_mk_temp_tmtm, by=c('RN_INDI'))
  
  
  #ba_ca -> BA_data_with_event_ba_ca at minimal age
  BA_data_with_event_temp1<- BA_data_with_event %>% group_by(RN_INDI) %>% summarise(mage=min(age))
  BA_data_with_event<- left_join(BA_data_with_event, BA_data_with_event_temp1, by='RN_INDI')
  BA_data_with_event_ba_ca<- BA_data_with_event %>% filter(age==mage) %>% select(RN_INDI, BA_CA)
  
  
  
  ####
  BA_data_with_event$whohavedisease[is.na(BA_data_with_event$whohavedisease)] <- 0
  BA_data_with_event_time_0<-BA_data_with_event%>% filter((whohavedisease)==0) %>% group_by(RN_INDI) %>% summarise(event_time=max(time))
  
  BA_data_with_event_time_1<-BA_data_with_event%>% filter((whohavedisease)==1) %>% group_by(RN_INDI) %>% summarise(event_time=max(futtime))
  BA_data_with_event_time_01<-rbind(BA_data_with_event_time_1,BA_data_with_event_time_0)
  BA_data_with_whohavedisease<-BA_data_with_event%>%select(RN_INDI, whohavedisease)
  BA_data_with_event_ba_ca<-unique(BA_data_with_event_ba_ca)
  BA_data_with_whohavedisease<-unique(BA_data_with_whohavedisease)
  
  BA_dz_selected<-left_join(BA_data_with_event_ba_ca, BA_data_with_whohavedisease, by='RN_INDI')
  BA_dz_selected<-merge(BA_dz_selected, BA_data_with_event_time_01, by='RN_INDI')
  BA_dz_selected$time<-BA_dz_selected$event_time
  BA_dz_selected$event<-BA_dz_selected$whohavedisease
  
  write.csv(as.data.frame(BA_dz_selected), 
            paste0('/userdata06/room058/working/result_pca_r/BA_CA_difference_and_disease_',disease_target, '_',gender,'.csv'))
  
  return(BA_dz_selected)
}

death_and_BA <-function(BA_data,dataset_for_cox,birth_data,death_code_excluded){
  BA_data <- left_join(BA_data, dataset_for_cox, by=c('RN_INDI','age'))
  death_data<-birth_data
  death_data_save<-death_data
  death_data<-mutate_at(death_data, "DTH_YYYYMM", ~replace(.,is.na(.),0))
  # 이상 결측치 확인
  # summary(!is.na(death_data$DTH_YYYYMM))
  # table(death_data$DTH_YYYYMM) # 이상치 발견
  death_data$DTH_YYYYMM <- as.numeric(death_data$DTH_YYYYMM)
  
  strings<-death_code_excluded
  death_data<-mutate_at(death_data, "DTH_YYYYMM", ~replace(.,str_detect(death_data$COD1, paste(strings, collapse = "|")),0))
  
  death_data$event<-ifelse(death_data$DTH_YYYYMM==0,0,1)
  
  
  # MK_data_with_disease=1
  death_data_1 <- death_data %>% filter(event==1)
  
  # 나이
  
  death_data_1<-death_data_1 %>% filter(BTH_YYYY!='1921LE')
  death_data_1$BTH_YYYY <- as.character(death_data_1$BTH_YYYY) # chr -> num
  death_data_1$BTH_YYYY <- as.numeric(death_data_1$BTH_YYYY)
  
  death_data_1$DTH_YYYYMM <- floor(death_data_1$DTH_YYYYMM/100)
  death_data_1$deadage <- death_data_1$DTH_YYYYMM-death_data_1$BTH_YYYY
  
  ##
  BA_data_temp<-BA_data%>% select(RN_INDI, age,biologic_age,time)
  ba_mk_temp<-left_join(BA_data_temp,death_data_1, by='RN_INDI')
  
  ba_mk_temp_tm <- ba_mk_temp %>%group_by(RN_INDI) %>% summarise(minmcage=min(age), mindage=min(deadage))
  
  
  #get dead at most 5year before from first medical checkup
  ba_mk_temp_tmtm <-ba_mk_temp_tm %>% filter(minmcage<=mindage& mindage<minmcage+5)
  ba_mk_temp_tmtm$whodead<-1
  ba_mk_temp_tmtm$futtime <- ba_mk_temp_tmtm$mindage - ba_mk_temp_tmtm$minmcage
  
  # seperate and paste
  BA_data<-BA_data %>% select(RN_INDI,age,biologic_age, time)
  BA_data$BA_CA <- BA_data$biologic_age-BA_data$age
  
  BA_data_with_event<-left_join(BA_data, ba_mk_temp_tmtm, by=c('RN_INDI'))
  
  #ba_ca -> BA_data_with_event_ba_ca at minimal age
  BA_data_with_event_temp1<- BA_data_with_event %>% group_by(RN_INDI) %>% summarise(mage=min(age))
  BA_data_with_event<- left_join(BA_data_with_event, BA_data_with_event_temp1, by='RN_INDI')
  BA_data_with_event_ba_ca<- BA_data_with_event %>% filter(age==mage) %>% select(RN_INDI, BA_CA)
  
  
  
  
  ####
  BA_data_with_event$whodead[is.na(BA_data_with_event$whodead)] <- 0
  BA_data_with_event_time_0<-BA_data_with_event%>% filter((whodead)==0) %>% group_by(RN_INDI) %>% summarise(event_time=max(time))
  
  BA_data_with_event_time_1<-BA_data_with_event%>% filter((whodead)==1) %>% group_by(RN_INDI) %>% summarise(event_time=max(futtime))
  BA_data_with_event_time_01<-rbind(BA_data_with_event_time_1,BA_data_with_event_time_0)
  BA_data_with_whodead<-BA_data_with_event%>%select(RN_INDI, whodead)
  BA_data_with_event_ba_ca<-unique(BA_data_with_event_ba_ca)
  BA_data_with_whodead<-unique(BA_data_with_whodead)
  BA_dz_selected<-left_join(BA_data_with_event_ba_ca, BA_data_with_whodead, by='RN_INDI')
  BA_dz_selected<-merge(BA_dz_selected, BA_data_with_event_time_01, by='RN_INDI')
  BA_dz_selected$time<-BA_dz_selected$event_time
  BA_dz_selected$event<-BA_dz_selected$whodead
  
  write.csv(as.data.frame(BA_dz_selected), paste0('/userdata06/room058/working/result_pca_r/BA_CA_difference_and_death_',gender,'.csv'))
  deathab<-as.data.frame(BA_dz_selected)
  return(deathab)
}

score = function(test,predict, call=FALSE){
  confusion_matrix = table(test$event,predict)
  if(call==TRUE){
    accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix)
    Precision = confusion_matrix[2,2]/sum(confusion_matrix[2,])
    Recall = confusion_matrix[2,2]/sum(confusion_matrix[,2])
    f1score = 2*(Precision*Recall)/(Precision+Recall)
    cat("accuracy: ",accuracy,"\tPrecision: ",Precision,"\tRecall: ",Recall,"\n")
    cat("F1-score: ",f1score,"\t")
    error_rate = 1 - accuracy
    cat("error_rate:",error_rate,"\t")

    roc<- roc(test$event,test$BA_CA)

    ROC(form=event~BA_CA, data=test, plot='ROC')
    #auc = auc(test$event,predict)
    #cat("AUC: ", auc)
    
    print("### calculate ROC ###")
    print(roc$auc)
    #ci.thresholds(roc, thresholds='best')
    }
  return(confusion_matrix)
}

calculateNRI<-function(compare,exp){
  dat1<-compare
  dat2<-exp
  if ((dat1[1,1]+dat1[1,2]) == (dat2[1,1]+dat2[1,2])& (dat1[2,1]+dat1[2,2])==(dat2[2,1]+dat2[2,2])){ 
    (dat2[1,1]-dat1[1,1])/(dat1[1,1]+dat1[1,2]) + (dat2[2,2]-dat1[2,2])/(dat1[2,1]+dat1[2,2])
  }else{
    print('error')
  }
}


disease_cox_fn <-function(BA_data,disease_target, strings){
  
  
  dab <-disease_and_BA(dataset_for_cox=dataset_for_cox,BA_data=BA_data,birth_data=birth_data,disease_target, strings)
  dab<- as.data.frame(dab)
  
  if (disease_target =='HTN'){
    BA_data_temp<- BA_data%>% select(RN_INDI, G1E_BP_SYS)
    BA_data_temp<-BA_data_temp%>%group_by(RN_INDI) %>% summarise(maxsbp=max(G1E_BP_SYS))
    BA_data_temp$sbp_event<-ifelse(BA_data_temp$maxsbp>=140,1,0)
    dab_temp<-left_join(dab, BA_data_temp, by='RN_INDI')
    dab_temp$event_temp<-dab_temp$event
    dab_temp$event<-ifelse(dab_temp$event_temp==1 | dab_temp$sbp_event==1 , 1, 0)
    dab<-dab_temp%>%select(RN_INDI, BA_CA, time, event, whohavedisease, event_time)
  }else if (disease_target =='DM'){
    BA_data_temp<- BA_data%>% select(RN_INDI, G1E_FBS)
    BA_data_temp<-BA_data_temp%>%group_by(RN_INDI) %>% summarise(maxfbs=max(G1E_FBS))
    BA_data_temp$fbs_event<-ifelse(BA_data_temp$maxfbs>=126,1,0)
    dab_temp<-left_join(dab, BA_data_temp, by='RN_INDI')
    dab_temp$event_temp<-dab_temp$event
    dab_temp$event<-ifelse(dab_temp$event_temp==1 | dab_temp$fbs_event==1 , 1, 0)
    dab<-dab_temp%>%select(RN_INDI, BA_CA, time, event, whohavedisease, event_time)
  }else{
    dab<-dab
  }
  dab_temp<- left_join(dab, BA_data, by='RN_INDI')
  scs<-c(selected_components, 'BA_CA','time','event')
  scs<-scs[-1]
  dab<-dab_temp%>%select(scs)
  
  set.seed(12345)
  split_dummy <- sample( nrow(dab), size= 0.7*nrow(dab))
  dab_train1 <- dab[split_dummy, ] 
  dab_test1 <- dab[-split_dummy, ]
  #View(dab_train1)
  #dab_train1$highBA<-round(dab_train1$BA_CA)
  #
  #dab_train1$highBA<-ifelse(dab_train1$highBA<(0), -1, dab_train1$highBA)
  #dab_train1$highBA<-ifelse(dab_train1$highBA>0, 1, dab_train1$highBA)
  #km1 <- survfit(Surv(time, event)~highBA, data=dab_train1)
  #library(survminer)
  #ggsurvplot(km1, palette='jco', risk.table=TRUE)
  #
  disease_cox1 <- coxph(Surv(time, event)~., data=dab_train1)
  disease_cox_step<-step(disease_cox1, direction = 'backward',trace = FALSE)
  try( final_c <- c(rownames(summary(disease_cox_step)$coefficients), 'BA_CA') , silent = TRUE )
  try( final_c <- c(rownames(extractHR(disease_cox_step)), 'BA_CA'), silent = TRUE)

  final_coef<-paste(final_c, collapse='+')
  formula_cox<- as.formula(paste('Surv(time, event)', '~', final_coef))
  disease_cox_final_result <- coxph(formula_cox, data=dab_train1)
  disease_cox1<-disease_cox_final_result
  if (disease_target =='HTN'){
    predict_data1<- ifelse(exp(-predict(disease_cox1, type='expected', newdata=dab_test1))<0.9,1,0)
  }else if (disease_target =='DM'){
    predict_data1<- ifelse(exp(-predict(disease_cox1, type='expected', newdata=dab_test1))<0.992,1,0)
  }else{
    predict_data1<- ifelse(exp(-predict(disease_cox1, type='expected', newdata=dab_test1))<0.955,1,0)
  }
  confusion_matrix1<- score(predict=predict_data1, test=dab_test1)
  
  print("@@@@@@@@@@@")
  cat('     @@   ', disease_target, '  @@', '\n')
  print("@@@@@@@@@@@")
  print('')
  cat('using : ', final_coef, '\n')
  cat('BA_CA exp(coef) : ' ,summary(disease_cox1)$coefficients['BA_CA',2], '\n')
  cat('95CI : ', exp(confint(disease_cox1))['BA_CA',],'\n')
  cat('C-index : ', summary(disease_cox1)$concordance[1], '\n')
  cat('BA_CA p : ' ,summary(disease_cox1)$coefficients['BA_CA',5], '\n')
  
  print('')
  
}

death_cox_fn<-function(BA_data,death_code_excluded){
  

  death_dab1<-death_and_BA(dataset_for_cox=dataset_for_cox,BA_data=BA_data,birth_data=birth_data,death_code_excluded)
  death_dab1_temp<- left_join(death_dab1, BA_data, by='RN_INDI')
  scs<-c(selected_components, 'BA_CA','time','event')
  scs<-scs[-1]
  death_dab1<-death_dab1_temp%>%select(scs)
  set.seed(12345)
  split_dummy <- sample( nrow(death_dab1), size= 0.7*nrow(death_dab1))
  death_dab_train1 <- death_dab1[split_dummy, ] 
  death_dab_test1 <- death_dab1[-split_dummy, ]
  death_cox1 <- coxph(Surv(time, event)~., data=death_dab_train1)
  death_cox_step<-step(death_cox1, direction = 'backward',trace = FALSE)
  
  try( final_c <- c(rownames(summary(death_cox_step)$coefficients), 'BA_CA') , silent = TRUE )
  try( final_c <- c(rownames(extractHR(death_cox_step)), 'BA_CA'), silent = TRUE)
  
  final_coef<-paste(final_c, collapse='+')
  formula_cox<- as.formula(paste('Surv(time, event)', '~', final_coef))
  death_cox_final_result <- coxph(formula_cox, data=death_dab_train1)
  death_cox1<-death_cox_final_result
  predict_data1<- ifelse(exp(-predict(death_cox1, type='expected', newdata=death_dab_test1))<0.7,1,0)
  confusion_matrix1<- score(predict=predict_data1, test=death_dab_test1)
  print("@@@@@@@@@@@")
  print('@@ death @@')
  print("@@@@@@@@@@@")
  print('')
  cat('using : ', final_coef, '\n')
  cat('BA_CA exp(coef) : ' ,summary(death_cox1)$coefficients['BA_CA',2], '\n')
  cat('95CI : ', exp(confint(death_cox1))['BA_CA',],'\n')
  cat('C-index : ', summary(death_cox1)$concordance[1], '\n')
  cat('BA_CA p : ' ,summary(death_cox1)$coefficients['BA_CA',5], '\n')

  print('')
  
  
}


death_and_dm_coxHR  <-function(dataset_for_cox,age_label, gender, ML_TF=FALSE,selected_components){
  file_name <- paste0('working/result_pca_r/dataset_0912_healthy_',age_label,'.csv')
  data_12 = read.csv(file_name)
  data_specific_gender <- data_12 %>% filter(data_12$SEX==gender)
  
  if (ML_TF==TRUE){
    print("######################################################")
    print("######## using MACHINE LEARNING  ###  ##########")
    print("######################################################")
    corrected_BA_parameters <-PCA_model_R(age_label=age_label,gender=gender,selected_components=selected_components, ML_TF=TRUE)
    BA_data<-read.csv( paste0('/userdata06/room058/working/result_pca_r/data_with_BA_',gender,'_',age_label,'_ML.csv'))
  }else if (ML_TF==FALSE) {
    corrected_BA_parameters <-PCA_model_R(age_label=age_label,gender=gender,selected_components=selected_components, ML_TF=FALSE)
    BA_data<-read.csv( paste0('/userdata06/room058/working/result_pca_r/data_with_BA_',gender,'_',age_label,'.csv'))
  }
  
  print('###### coefficients of biologic age #######')
  print(paste('age range : ',age_label, 'sex :', gender))
  print( corrected_BA_parameters)
  print("#########################################")
  
  
  # death cox HR
  death_code_excluded <- c("S", "T") #(사고사인 'S', 'T' 제외)
  
  death_cox_fn(BA_data=BA_data,death_code_excluded)
  
  disease_target<- "DM"
  strings <- c('R81','E10','E11','E12','E13','E14')
  disease_cox_fn(BA_data=BA_data,disease_target,strings)
  
}

print_BA_coef<-function(age_label, gender, ML_TF=FALSE, selected_components){
 
  corrected_BA_parameters <-PCA_model_R(age_label=age_label,gender=gender,selected_components=selected_components, ML_TF=FALSE)
  print('###### coefficients of biologic age #######')
  print(paste('age range',age_label, 'sex :', gender))
  print( corrected_BA_parameters)
  print('###### ### ### ### ### ### ### ### #######')
  
}

disease_coxHR  <-function(data_12,dataset_for_cox,age_label, gender, ML_TF=FALSE,selected_components){
  file_name <- paste0('working/result_pca_r/dataset_0912_healthy_',age_label,'.csv')
  data_12 = read.csv(file_name)
  data_specific_gender <- data_12 %>% filter(data_12$SEX==gender)
  
  if(ML_TF==TRUE) {
    print("######################################################")
    print("#####   ### using MACHINE LEARGING  #######   ######")
    print("######################################################")
    corrected_BA_parameters <-PCA_model_R(age_label=age_label,gender=gender,selected_components=selected_components, ML_TF=TRUE)
    BA_data<-read.csv( paste0('/userdata06/room058/working/result_pca_r/data_with_BA_',gender,'_',age_label,'_ML.csv'))
  }else if (ML_TF==FALSE) {
    corrected_BA_parameters <-PCA_model_R(age_label=age_label,gender=gender,selected_components=selected_components, ML_TF=FALSE)
    BA_data<-read.csv( paste0('/userdata06/room058/working/result_pca_r/data_with_BA_',gender,'_',age_label,'.csv'))
  }
  
  print('###### coefficients of biologic age #######')
  print(paste('age range',age_label, 'sex :', gender))
  print( corrected_BA_parameters)
  
  
  disease_target<- "heart"
  strings <- c('I20','I21','I22','I23','I24','I25')
  disease_cox_fn(BA_data=BA_data,disease_target,strings)
  
  disease_target<- "brain"
  strings <- c('I6')
  disease_cox_fn(BA_data=BA_data,disease_target,strings)
  
  disease_target<- "cancer"
  strings <- c('C')
  disease_cox_fn(BA_data=BA_data,disease_target,strings)
  
  disease_target<- "HTN"
  strings <- c('I10','I11','I12','I13','I14','I15')
  disease_cox_fn(BA_data=BA_data,disease_target,strings)
  
  print("@@@@@@@@@@@@@@@@@@@@@")
  print('all disease finished')
  print("@@@@@@@@@@@@@@@@@@@@@")
}

rf_fn<-function(mtry, ntree){
  t1 <-Sys.time()
  ML_model<-randomForest(age~.-age, mtry=mtry, ntree=ntree, data=ML_tr)
  predicted_age<-predict(ML_model, newdata = ML_te)
  cat('mtry : ', mtry, ',    ntree : ', ntree, '  \n')
  cat('error : ', sum(abs(predicted_age-ML_te[,1]))/dim(ML_te)[1], '\n')
  t2 <-Sys.time()
  cat('waste time : ', t2-t1, '\n')
  
}

RF_DM <- function(data_12, SEED=12345, save=FALSE){
  raw_temp <- raw_g1e0915%>%filter(Q_PHX_DX_DM==1|Q_PHX_TX_DM==1|G1E_FBS>126)
  raw_healthy<-read.csv('working/result_pca_r/dataset_0912_healthy_.csv')
  data_12_nodm <- raw_healthy%>%filter(!RN_INDI %in% raw_temp$RN_INDI)
  sc <- selected_components[-1]
  data_ML<-data_12_nodm %>% select((selected_components))
  set.seed(12345)
  split_dummy <- sample( nrow(data_ML), size= 0.7*nrow(data_ML))
  ML_train <- data_ML[split_dummy, ] 
  ML_test <- data_ML[-split_dummy, ] 
  ML_tr<-ML_train %>% select((selected_components))%>% select(-SEX)
  ML_te<-ML_test %>% select((selected_components))%>% select(-SEX)
 
  ML_tr<-scale(ML_tr)
  ML_te<-scale(ML_te)
  
  alarm()
  
  ML_model<-randomForest(age~.-age, mtry=10, ntree=100, data=ML_tr)
  
  
  if (save==TRUE){
    saveRDS(ML_model, file='working/pcadata/ML_model.rda')
  }
  return(ML_model)
}
xgb_pred <-function(xgparams){
  xgbc = xgboost(data = xgb_train,  nrounds = 500, params=xgparams ,verbose=0)
  pred_y = predict(xgbc, xgb_test)
  
  xgerr <- sum(abs(pred_y-test_y))/length(pred_y)
  print(xgerr)
}
XGB_DM <- function(data_12, SEED=12345, save=FALSE){

  
  
  raw_temp <- raw_g1e0915%>%filter(Q_PHX_DX_DM==1|Q_PHX_TX_DM==1|G1E_FBS>126)
  raw_healthy<-read.csv('working/result_pca_r/dataset_0912_healthy_.csv')

  data_12_nodm <- raw_healthy%>%filter(!RN_INDI %in% raw_temp$RN_INDI)
  sc <- selected_components[-1]
  data_ML<-data_12_nodm %>% select((selected_components))
  set.seed(12345)
  split_dummy <- sample( nrow(data_ML), size= 0.7*nrow(data_ML))
  ML_train <- data_ML[split_dummy, ] 
  ML_test <- data_ML[-split_dummy, ] 
  ML_tr<-ML_train %>% select((selected_components))%>% select(-SEX)
  ML_te<-ML_test %>% select((selected_components))%>% select(-SEX)
  
  ML_tr<-scale(ML_tr)
  ML_te<-scale(ML_te)

  train_x = data.matrix(ML_tr[, -1])
  train_y = ML_tr[,1]
  
  test_x = data.matrix(ML_te[, -1])
  test_y = ML_te[, 1]
  
  xgb_train = xgb.DMatrix(data = train_x, label = train_y)
  xgb_test = xgb.DMatrix(data = test_x, label = test_y)
  
  'parameter finding'
  xgparams = list(eta=0.3, colsample_bylevel=1, subsample=1, max_depth=8, gamma=10, min_child_weight=10)
  # xgb_pred(xgparams)
  ML_model = xgboost(data = xgb_train,  nrounds = 500, params=params ,verbose=0)
  
  
  if (save==TRUE){
    saveRDS(ML_model, file='working/pcadata/ML_model.rda')
  }
  return(ML_model)
}

disease_xgb <-function(age_label,disease_target,strings, gender,selected_components, BA_TF){
  file_name <- paste0('working/result_pca_r/dataset_0912_healthy_',age_label,'.csv')
  data_12 = read.csv(file_name)
  data_specific_gender <- data_12 %>% filter(data_12$SEX==gender)
  corrected_BA_parameters <-PCA_model_R(age_label=age_label,gender=gender,selected_components=selected_components, ML_TF=FALSE)
  BA_data<-read.csv( paste0('/userdata06/room058/working/result_pca_r/data_with_BA_',gender,'_',age_label,'.csv'))
  

  dab <-disease_and_BA(dataset_for_cox=dataset_for_cox,BA_data=BA_data,birth_data=birth_data,disease_target, strings)
  BA_data_event<-dab%>%select(RN_INDI,BA_CA,event)
  BA_data_event<-left_join(BA_data, BA_data_event, by='RN_INDI')  
  BA_data_with_event_temp1<- BA_data_event %>% group_by(RN_INDI) %>% summarise(mage=min(age))
  BA_data_event<- left_join(BA_data_event, BA_data_with_event_temp1, by='RN_INDI')
  BA_data_event<- BA_data_event %>% filter(age==mage) %>%select(-mage)
  BA_data_event$CA<- BA_data_event$age
  
  
  if (BA_TF == TRUE){
  sc<-c(selected_components,'BA_CA' ,'CA' ,'event')[-1]
  } else if (BA_TF != TRUE) {
  sc<-c(selected_components, 'event')[-1]
  }
  data_ML_with_age<-BA_data_event %>% select((sc))
  data_ML<-(data_ML_with_age)[-1]
  set.seed(12345)
  split_dummy <- sample( nrow(data_ML), size= 0.7*nrow(data_ML))
  ML_train <- data_ML[split_dummy, ] 
  ML_test <- data_ML[-split_dummy, ] 
  
  ML_train_temp1 <- (ML_train)%>%select(event)
  ML_train_temp2 <- (ML_train)%>%select(-event)
  ML_tr<-scale(ML_train_temp2)
  ML_tr<-data.frame(ML_tr)
  ML_tr$event<-ML_train_temp1$event
  
  ML_test_temp1 <- (ML_test)%>%select(event)
  ML_test_temp2 <- (ML_test)%>%select(-event)
  ML_te<-scale(ML_test_temp2)
  ML_te<-data.frame(ML_te)
  ML_te$event<-ML_test_temp1$event
  
  target_index<-dim(ML_tr)[2]

  train_x = data.matrix(ML_tr[, -target_index])
  train_y = ML_tr[,target_index]
  
  test_x = data.matrix(ML_te[, -target_index])
  test_y = ML_te[, target_index]
  
  xgb_train = xgb.DMatrix(data = train_x, label = train_y)
  xgb_test = xgb.DMatrix(data = test_x, label = test_y)
  
  xgparams = list(eta=0.03 , max_depth=3, gamma= 3, subsample=0.7,objective='binary:logistic', lambda=1, nthread=2)
  ML_model <- xgb.train(params=xgparams, data= xgb_train, nrounds=200, verbose=0 )

  pred_y = predict(ML_model, xgb_test)
  shap_values<-predict(ML_model, xgb_test, predcontrib=TRUE)
  
  
  "###########"
  "###########"
  xgb.plot.shap(model=ML_model, top_n=6, n_col = 2,test_x)
  
  print("###### importance #######")
  print(xgb.importance(colnames(xgb_train), model=ML_model))

  print("###### roc #######")
  print(roc(test_y,predictor=pred_y))
  return(ML_model)
  }
  

'### ### ### ###'
'do it first time'
# db 전처리##
### raw_g1e0915<-read.csv('working/pcadata/nsc2_g1e_0915_db.csv')
### MK_data<-read.csv('/userdata06/room058/working/pcadata/mk_data.csv')
### birth_data<-read.csv('working/pcadata/nsc2_bnd_db.csv')
###raw_bnc<-read.csv('working/pcadata/nsc2_bnc_db.csv')
###raw_bnc<-raw_bnc %>% select(RN_INDI, SEX)
###dim(raw_bnc)
###raw_bnc<-unique(raw_bnc)
###write.csv(raw_bnc, 'working/result_pca_r/bnc_temp.csv')
###raw_bnc<- read.csv('working/result_pca_r/bnc_temp.csv')

if (exists('MK_data')!=1){
  M_data<-read.csv('working/pcadata/nsc2_m20_db.csv')
  K_data<-read.csv('working/pcadata/nsc2_k20_db.csv')
  MK_data <- rbind(M_data, K_data)
  MK_data_save<-MK_data
  MK_data$SYMcode<-paste0(MK_data$SICK_SYM1, MK_data$SICK_SYM2)
  MK_data$SYMcode<-as.character(MK_data$SYMcode)
  write.csv(MK_data, paste0('/userdata06/room058/working/pcadata/mk_data.csv'))
}
db_parameter<-c('RN_INDI', 'SEX', 'age','G1E_HGHT','G1E_WGHT','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_BP_DIA',
                'G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_LDL','G1E_HGB', 'G1E_URN_PROT','G1E_CRTN','G1E_SGOT','G1E_SGPT','G1E_GGT','eGFR')
DB_parser(raw_g1e0915=raw_g1e0915,birth_data=birth_data,folder_path, parameter=db_parameter)
dataset_for_cox<-read.csv('working/result_pca_r/dataset_for_cox.csv')

'### ### ### ###'


## Phase 0 Settings
age_label<-''
gender<-1
selected_components <- c('SEX','age','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR','DRK','SMK','Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_DM','Q_FHX_HTN')
death_and_dm_coxHR(dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components)
disease_coxHR (data_12=data_12,dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components)


gender<-2
selected_components <- c('SEX','age','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR','DRK','SMK','Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_DM','Q_FHX_HTN')
death_and_dm_coxHR(dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components)
disease_coxHR (data_12=data_12,dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components)


'### ### ### ###'
'### age sex ###'
'### ### ### ###'

'male'
# male
age_label<-'40'
gender<-1

#DrinkSmoke
selected_components <- c('SEX','age','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR','DRK','SMK')
#DrinkSmoke + familyhistory
selected_components <- c('SEX','age','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR','DRK','SMK','Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_DM','Q_FHX_HTN')
#DrinkSmoke + burden
selected_components <- c('SEX','age','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR','DRK','SMK','familyhistory','obesity_burden','bp_burden','fbs_burden','wc_burden','hdl_burden','tg_burden','pa_burden','drinking_burden','smoking_burden')
# BA coef
print_BA_coef(age_label = age_label, gender= gender, selected_components = selected_components)

# coxPH
death_and_dm_coxHR(dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components)
disease_coxHR (data_12=data_12,dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components)
# ML 
death_and_dm_coxHR(dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components,  ML_TF=TRUE)
disease_coxHR (data_12=data_12,dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components,  ML_TF=TRUE)


#BA xgboost # CORRECTION
xgb_fit<-disease_xgb(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
xgb_fit<-disease_xgb(age_label=age_label, disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=FALSE)

'female'
# female
#age_label<-'30'
gender<-2
#DrinkSmoke
selected_components <- c('SEX','age','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR','DRK','SMK')
#DrinkSmoke + familyhistory
selected_components <- c('SEX','age','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR','DRK','SMK','Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_DM','Q_FHX_HTN')
#DrinkSmoke + burden
selected_components <- c('SEX','age','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR','DRK','SMK','familyhistory','obesity_burden','bp_burden','fbs_burden','wc_burden','hdl_burden','tg_burden','pa_burden','drinking_burden','smoking_burden')

# coxPH
death_and_dm_coxHR (dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components)
disease_coxHR(data_12=data_12,dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components)
# ML 
death_and_dm_coxHR(dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components,  ML_TF=TRUE)
disease_coxHR (data_12=data_12,dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components,  ML_TF=TRUE)


#BA xgboost
xgb_fit<-disease_xgb(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
xgb_fit<-disease_xgb(age_label=age_label, disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=FALSE)


disease_target<- "DM"
strings <- c('R81')

disease_target<- "heart"
strings <- c('I20','I21','I22','I23','I24','I25')

disease_target<- "brain"
strings <- c('I6')

disease_target<- "cancer"
strings <- c('C')

disease_target<- "HTN"
strings <- c('I1')

####
library(moonBook)
data_with_BA_path <- paste0('/userdata06/room058/working/result_pca_r/data_with_BA_',gender,'_',age_label,'.csv')
BA_data<-read.csv(data_with_BA_path)
BA_data$ages<-BA_data$age%/%10
mytable(ages~.-RN_INDI-X ,data=BA_data)
count_id(BA_data)
str(BA_data)
