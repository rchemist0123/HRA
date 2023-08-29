
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

# function 

count_id<-function(data){
  data_temp<-data%>%select(RN_INDI)
  data_temp<-unique(data_temp)
  return(dim(data_temp)[1])
}

DB_parser<- function(birth_data,folderpath, parameter){
  
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
  raw_g1e0915<-read.csv('working/pcadata/nsc2_g1e_0915_db.csv')
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
  raw_bnc<-read.csv('working/pcadata/nsc2_bnc_db.csv')
  raw_bnc<-raw_bnc %>% select(RN_INDI, SEX)
  
  #중복행?
  # dim(raw_bnc)
  raw_bnc<-unique(raw_bnc)
  # dim(raw_bnc)
  raw_bnc_save<-raw_bnc 
  
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
                                   all_dataset$Q_PHX_DX_DLD==1|all_dataset$	Q_PHX_DX_PTB==1|all_dataset$	Q_PHX_DX_ETC==1|all_dataset$	Q_PHX_TX_STK==1|all_dataset$Q_PHX_TX_HTDZ==1|
                                   all_dataset$Q_PHX_TX_HTN==1|all_dataset$Q_PHX_TX_DM==1|all_dataset$	Q_PHX_TX_DLD==1|all_dataset$	Q_PHX_TX_PTB==1|all_dataset$Q_PHX_TX_ETC==1),1,0)
  
  all_dataset$history[is.na(all_dataset$history)] = 0
  
  # family_parameter_AND_exercise_parameter <-c('Q_FHX_STK',	'Q_FHX_HTDZ',	'Q_FHX_HTN',	'Q_FHX_DM',	'Q_FHX_ETC','Q_PA_VD',	'Q_PA_MD',	'Q_PA_WALK' 	)
  # other_parameter<-   c( 'Q_SMK_YN'	,'Q_SMK_PRE_DRT',	'Q_SMK_PRE_AMT',	'Q_SMK_NOW_DRT'	,'Q_SMK_NOW_AMT_V09N'	,                       'Q_DRK_FRQ_V09N'	,'Q_DRK_AMT_V09N'	 )
  # exercise
  all_dataset$Q_PA_VD[is.na(all_dataset$Q_PA_VD)] = 0
  all_dataset$Q_PA_MD[is.na(all_dataset$Q_PA_MD)] = 0
  all_dataset$Q_PA_WALK[is.na(all_dataset$Q_PA_WALK)] = 0
  all_dataset$METS<-all_dataset$Q_PA_WALK*2.9*30+all_dataset$Q_PA_MD*4*30+all_dataset$Q_PA_VD*7*20
  #smoke
  all_dataset$Q_SMK_PRE_DRT[is.na(all_dataset$Q_SMK_PRE_DRT)] = 0
  all_dataset$Q_SMK_PRE_AMT[is.na(all_dataset$Q_SMK_PRE_AMT)] = 0
  all_dataset$Q_SMK_NOW_DRT[is.na(all_dataset$Q_SMK_NOW_DRT)] = 0
  all_dataset$Q_SMK_NOW_AMT_V09N[is.na(all_dataset$Q_SMK_NOW_AMT_V09N)] = 0
  all_dataset$PY_now<-all_dataset$Q_SMK_NOW_DRT * all_dataset$Q_SMK_NOW_AMT_V09N /20
  all_dataset$PY_pre<-all_dataset$Q_SMK_PRE_DRT * all_dataset$Q_SMK_PRE_AMT /20
  all_dataset$SMK <- ifelse(all_dataset$PY_now>0|all_dataset$Q_SMK_YN==3,2,ifelse(all_dataset$PY_pre>0|all_dataset$Q_SMK_YN==2,1,0))
  # drink
  all_dataset$Q_DRK_FRQ_V09N[is.na(all_dataset$Q_DRK_FRQ_V09N)] = 0
  all_dataset$Q_DRK_AMT_V09N[is.na(all_dataset$Q_DRK_AMT_V09N)] = 0
  
  if (gender==1){
    
    all_dataset$DRK<-ifelse(all_dataset$Q_DRK_AMT_V09N>=7|all_dataset$Q_DRK_FRQ_V09N>=3,2,ifelse(all_dataset$Q_DRK_AMT_V09N>=1|all_dataset$Q_DRK_FRQ_V09N>=1,1,0))
  } else if (gender==2){
    all_dataset$DRK<-ifelse(all_dataset$Q_DRK_AMT_V09N>=5|all_dataset$Q_DRK_FRQ_V09N>=3,2,ifelse(all_dataset$Q_DRK_AMT_V09N>=1|all_dataset$Q_DRK_FRQ_V09N>=1,1,0))
    
  }
  
  
  all_dataset <- all_dataset %>% select((c(parameter,'history','METS','SMK','DRK')))
  temp2a<- count_id(all_dataset)
  all_dataset<-na.omit(all_dataset)
  temp2b<- count_id(all_dataset)
  print(paste('all_data:',temp2a,'->who has NA in g1e removed :',temp2a-temp2b))
  
  tempa<- count_id(all_dataset)
  all_dataset<-(all_dataset) %>% filter(age>=20)
  tempb<- count_id(all_dataset)
  print(paste('all_data:',tempa,'->who is under 20 :',tempa-tempb))
  
  print(paste('all processed data age over 20 : ', tempb))
  
  print('all age, all condition')
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
  print(paste('60-70 :',count_id(all_dataset60)))
  print(paste('man :',count_id(filter(all_dataset60, SEX==1))))
  print(paste('woman :',count_id(filter(all_dataset60, SEX==2))))

  
  
  # all_dataset 저장
  write.csv(all_dataset, 'working/result_pca_r/mediage_demo_dataset_NHIS_unit_all.csv')
  write.csv(all_dataset20, 'working/result_pca_r/mediage_demo_dataset_NHIS_unit20_all.csv')
  write.csv(all_dataset30, 'working/result_pca_r/mediage_demo_dataset_NHIS_unit30_all.csv')
  write.csv(all_dataset40, 'working/result_pca_r/mediage_demo_dataset_NHIS_unit40_all.csv')
  write.csv(all_dataset50, 'working/result_pca_r/mediage_demo_dataset_NHIS_unit50_all.csv')
  write.csv(all_dataset60, 'working/result_pca_r/mediage_demo_dataset_NHIS_unit60_all.csv')
  write.csv(dataset_for_cox,'working/result_pca_r/dataset_for_cox.csv' )
  print('')
  print('### all condition group saved ###')
  print('')
  
  '## 건강한 dataset 구축 ##'
  dataset_healthy<-all_dataset
  temp3a<-count_id(dataset_healthy)
  # normal range를 정해 이상치 제외 # strict하게 잡아야한다. 건강나이는 건강한 사람이 기준이되어서 하는것.
  
   dataset_healthy<- dataset_healthy %>% filter(age>=20) %>% filter(15<=G1E_BMI & G1E_BMI<=30) %>% 
    filter(105>=G1E_WSTC&G1E_WSTC>=60) %>% filter(80<=G1E_BP_SYS&G1E_BP_SYS<=160) %>% 
    filter(G1E_BP_DIA>=50 & G1E_BP_DIA<=100) %>% 
    filter(G1E_FBS<=140 & G1E_FBS>=50) %>% 
    filter(G1E_TOT_CHOL<=260 & G1E_TOT_CHOL>=50) %>% 
    filter(G1E_TG>=50 & G1E_TG<=400) %>% 
    filter(G1E_HDL<=90 & G1E_HDL>=20) %>% 
    filter(G1E_LDL <=190) %>% filter(G1E_HGB<=18&G1E_HGB>=10) %>% 
    filter(G1E_CRTN<=2&G1E_CRTN>=0.4) %>%
    filter(G1E_SGOT<=60) %>% filter(G1E_SGPT<=80) %>% filter(G1E_GGT<=150)  %>% filter(G1E_URN_PROT %in% c(1,2,3,4))
  
  temp3b<-count_id(dataset_healthy)
  print(paste('all_data:',temp3a,'->who are not in normal range :',temp3a-temp3b))
  
  
  temp4a<-count_id(dataset_healthy)
  dataset_healthy<-dataset_healthy%>% filter(history!=1)%>%select(-history)
  temp4b<-count_id(dataset_healthy)
  
  print(paste('all_data:',temp4a,'->who has already chronic disease :',temp4a-temp4b))
  print('')
  
  print('all age, healthy condition')
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
  print(paste('50-60 :',count_id(dataset_healthy60)))
  print(paste('man :',count_id(filter(dataset_healthy60, SEX==1))))
  print(paste('woman :',count_id(filter(dataset_healthy60, SEX==2))))
  
  
  #  dataset_healthy 저장
  write.csv( dataset_healthy, 'working/result_pca_r/mediage_demo_dataset_NHIS_unit.csv')
  write.csv( dataset_healthy20, 'working/result_pca_r/mediage_demo_dataset_NHIS_unit20.csv')
  write.csv( dataset_healthy30, 'working/result_pca_r/mediage_demo_dataset_NHIS_unit30.csv')
  write.csv( dataset_healthy40, 'working/result_pca_r/mediage_demo_dataset_NHIS_unit40.csv')
  write.csv( dataset_healthy50, 'working/result_pca_r/mediage_demo_dataset_NHIS_unit50.csv')
  write.csv( dataset_healthy60, 'working/result_pca_r/mediage_demo_dataset_NHIS_unit60.csv')
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

PC_to_BAS <- function(data_specific_gender, gender, pc_number_is_1 = TRUE, components=selected_components){
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

PCA_model_R<-function(data_12, age_label, gender,selected_components){
  #### 1. sex, age를 기준으로 데이터 로딩 
  
  components_all= c('SEX', 'age', 'G1E_HGHT', 'G1E_WGHT','G1E_BMI', 'G1E_WSTC', 'G1E_BP_SYS', 'G1E_BP_DIA', 'G1E_FBS','G1E_TOT_CHOL', 'G1E_TG', 'G1E_HDL', 'G1E_LDL', 'G1E_HGB','G1E_URN_PROT', 'G1E_CRTN', 'G1E_SGOT', 'G1E_SGPT', 'G1E_GGT', 'eGFR', 'DRK', 'SMK', 'METS')
  
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
  BAS_parameters <- PC_to_BAS(data_specific_gender=data_specific_gender, gender=gender, pc_number_is_1=T, components=selected_components)
  BA_parameters <- BA(data_specific_gender=data_specific_gender, BAS_parameters)
  
  #### 6. BA-CA fit
  filename_pc <- paste0('working/result_pca_r/mediage_demo_dataset_NHIS_unit_all',age_label,'.csv')
  all_data <- read.csv(filename_pc)
  
  all_data<-all_data%>%filter(SEX==gender) %>%select(-history)

  data_with_BA <- BA_add_to_dataset( all_data=all_data,
                                     dictionary =BA_parameters, 
                                     components=selected_components, corrected=F)
  data_with_BA_linear_fit<- cbind(data_with_BA, age=all_data$age) 
  model <- BA_CA_linear_fit(data_with_BA_linear_fit)
  
  #### 7. Correction
  corrected_BA_parameters <- corrected_BA(data_specific_gender=data_specific_gender, BA_parameters, model=model)
  data_with_BA <- BA_add_to_dataset( all_data=all_data,dictionary =corrected_BA_parameters, components=selected_components, corrected=T)
  write.csv(data_with_BA, paste0('/userdata06/room058/working/result_pca_r/data_with_BA_',gender,'_',age_label,'.csv'))
  colnames(corrected_BA_parameters)<-c('parameter', 'coef')
  write.csv(corrected_BA_parameters, paste0('/userdata06/room058/working/result_pca_r/corrected_biological_age_coef_',gender,'_',age_label,'.csv'))
  return(corrected_BA_parameters)
}

disease_and_BA <- function(BA_data,dataset_for_cox,birth_data,disease_target, strings){
  # 국민기록
  MK_data<- MK_data %>% select(RN_INDI, SICK_SYM1,SICK_SYM2, SYMcode, STD_YYYY)

  # data_with_BA_path <- paste0('/userdata06/room058/working/result_pca_r/data_with_BA_',gender,'_',age_label,'.csv')
  # BA_data<-read.csv(data_with_BA_path)
  
  
  #dataset_for_cox_path<- 'working/result_pca_r/dataset_for_cox.csv'
  #dataset_for_cox<-read.csv('working/result_pca_r/dataset_for_cox.csv')
  
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
  
  
  #get disease at most 10year before from first medical checkup
  ba_mk_temp_tmtm <-ba_mk_temp_tm %>% filter(minmcage<=minage& minage<minmcage+10)
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
  
  dab<- as.data.frame(BA_dz_selected)
  
  return(dab)
}

death_and_BA <-function(BA_data,dataset_for_cox,birth_data,death_code_excluded){
  #data_with_BA_path <- paste0('/userdata06/room058/working/result_pca_r/data_with_BA_',gender,'_',age_label,'.csv') # 건강하지 않은 사람도 포함해야한다. 다시 코딩해야.
  #BA_data<-read.csv(data_with_BA_path)
  
  #dataset_for_cox_path<- 'working/result_pca_r/dataset_for_cox.csv'
  #dataset_for_cox<-read.csv('working/result_pca_r/dataset_for_cox.csv')
  
  BA_data <- left_join(BA_data, dataset_for_cox, by=c('RN_INDI','age'))
  
  #death_data<-read.csv('working/pcadata/nsc2_bnd_db.csv')
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
  
  
  #get dead at most 10year before from first medical checkup
  ba_mk_temp_tmtm <-ba_mk_temp_tm %>% filter(minmcage<=mindage& mindage<minmcage+10)
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

score = function(test,predict){
  confusion_matrix = table(test$event,predict)
  
  accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix)
  Precision = confusion_matrix[2,2]/sum(confusion_matrix[2,])
  Recall = confusion_matrix[2,2]/sum(confusion_matrix[,2])
  f1score = 2*(Precision*Recall)/(Precision+Recall)
  cat("accuracy: ",accuracy,"\nPrecision: ",Precision,"\nRecall: ",Recall,"\n")
  cat("F1-score: ",f1score,"\n")
  
  error_rate = 1 - accuracy
  cat("error_rate:",error_rate,"\n")
  library(pROC)
  auc = auc(test$event,predict)
  cat("AUC: ", auc)   
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


## Phase 0 Settings


'### ### ### ###'
'### age sex ###'
'### ### ### ###'
# male
age_label<-''
gender<-1

# female
age_label<-''
gender<-2

'### ### ### ###'
'do it first time'
# db 전처리##
db_parameter<-c('RN_INDI', 'SEX', 'age','G1E_HGHT','G1E_WGHT','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_BP_DIA','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_LDL','G1E_HGB', 'G1E_URN_PROT','G1E_CRTN','G1E_SGOT','G1E_SGPT','G1E_GGT','eGFR')
birth_data<-read.csv('working/pcadata/nsc2_bnd_db.csv')
DB_parser(birth_data=birth_data,folder_path, parameter=db_parameter)
# MK_data<-read.csv('/userdata06/room058/working/pcadata/mk_data.csv')
if (exists('MK_data')!=1){
  M_data<-read.csv('working/pcadata/nsc2_m20_db.csv')
  K_data<-read.csv('working/pcadata/nsc2_k20_db.csv')
  MK_data <- rbind(M_data, K_data)
  MK_data_save<-MK_data
  MK_data$SYMcode<-paste0(MK_data$SICK_SYM1, MK_data$SICK_SYM2)
  MK_data$SYMcode<-as.character(MK_data$SYMcode)
  write.csv(MK_data, paste0('/userdata06/room058/working/pcadata/mk_data.csv'))
}
'### ### ### ###'



'### ### ### ###'
'### START   ###'
'### ### ### ###'

## PHASE 1 PCA model
# according to age and gender, data loaded

file_name <- paste0('working/result_pca_r/mediage_demo_dataset_NHIS_unit',age_label,'.csv')
data_12 = read.csv(file_name)
data_specific_gender <- data_12 %>% filter(data_12$SEX==gender)
BA_data<-read.csv( paste0('/userdata06/room058/working/result_pca_r/data_with_BA_',gender,'_',age_label,'.csv'))
dataset_for_cox<-read.csv('working/result_pca_r/dataset_for_cox.csv')



'#########parameter selection#######'
#Height, weight excluded because of historical reason in Korea
predictors <-c('G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_BP_DIA','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_LDL','G1E_HGB', 'G1E_URN_PROT','G1E_CRTN','G1E_SGOT','G1E_SGPT','G1E_GGT','eGFR','SMK','DRK','METS')
rwa_temp<-data_specific_gender %>% rwa(outcome = 'age', predictors = predictors, applysigns = T)
rwa_temp$result # rwa>2 chosen

'###################################'

'###############'
'## parameter###'
'###############'

'# male ###'
model <- lm(age~G1E_WSTC+G1E_BP_SYS+G1E_FBS+G1E_HGB+G1E_SGOT+G1E_SGPT+eGFR+DRK, data=data_specific_gender)
vif(model) # bmi removed due to wstc, creatinine removed due to eGFR  ### VIF<10 parameter chosen
#BEST
selected_components <- c('SEX','age','G1E_BMI', 'G1E_BP_SYS', 'G1E_FBS','G1E_TG','G1E_HDL','G1E_HGB', 'G1E_SGOT','G1E_SGPT', 'eGFR')
#TRIAL
selected_components <- c('G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_BP_DIA','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_LDL','G1E_HGB', 'G1E_URN_PROT','G1E_CRTN','G1E_SGOT','G1E_SGPT','G1E_GGT','eGFR','SMK','DRK','METS')

rwa_temp<-data_specific_gender %>% rwa(outcome = 'age', predictors =setdiff(selected_components,c('SEX','age')), applysigns = T)
rwa_temp$result # rwa>2 chosen

'# female ###'
############
model <- lm(age~G1E_WGHT+G1E_WSTC+G1E_BP_SYS+G1E_BP_DIA + G1E_FBS+G1E_TOT_CHOL+G1E_HDL+G1E_LDL+G1E_SGOT+G1E_SGPT+eGFR+DRK, data=data_specific_gender)
vif(model)
#selected_components <- c('SEX','age', 'G1E_WSTC', 'G1E_BP_SYS','G1E_FBS','G1E_HDL','G1E_LDL', 'G1E_SGOT', 'eGFR','DRK','SMK','METS')
selected_components <- c('SEX','age', 'G1E_BMI','G1E_BP_SYS','G1E_BP_DIA','G1E_FBS','G1E_TOT_CHOL','G1E_HDL', 'G1E_LDL','G1E_SGOT','G1E_SGPT', 'eGFR')
rwa_temp<-data_specific_gender %>% rwa(outcome = 'age', predictors =setdiff(selected_components,c('SEX','age')), applysigns = T)
rwa_temp$result # rwa>2 chosen
################


'###############'
'#### pca  #####'
'###############'

corrected_BA_parameters <-PCA_model_R(data_12=data_12, age_label=age_label,gender=gender,selected_components=selected_components)
print('###### coefficients of biologic age #######')
print(paste('age range',age_label, 'sex :', gender))
print( corrected_BA_parameters)
'########################'

# death cox HR
death_code_excluded <- c("S", "T") #(사고사인 'S', 'T' 제외)
death_dab1<-death_and_BA(dataset_for_cox=dataset_for_cox,BA_data=BA_data,birth_data=birth_data,death_code_excluded)
death_dab1%>%dim
split_dummy <- sample(c(rep(0, 0.7 * nrow(death_dab1)), rep(1, 0.3 * nrow(death_dab1))))
dab_train1 <- death_dab1[split_dummy == 0, ] 
dab_test1 <- death_dab1[split_dummy == 1, ] 




death_cox1 <- coxph(Surv(time, event==1)~BA_CA, data=dab_train1,x=TRUE)
extractAIC(death_cox1)
predict_data1<- ifelse(exp(-predict(death_cox1, type='expected', newdata=dab_test1))<0.95,1,0)
confusion_matrix1<- score(predict=predict_data1, test=dab_test1)
print(confusion_matrix1)
summary(death_cox1)

#####
#compare 
selected_components <- c('SEX','age','G1E_HGHT','G1E_WGHT', 'G1E_WSTC', 'G1E_BP_SYS', 'G1E_FBS','G1E_HGB','G1E_CRTN', 'G1E_SGOT','G1E_SGPT', 'eGFR','DRK','SMK')#
corrected_BA_parameters<-PCA_model_R(data_12=data_12, age_label=age_label,gender=gender,selected_components=selected_components)
death_dab2 <-death_and_BA(death_code_excluded)
split_dummy <- sample(c(rep(0, 0.7 * nrow(death_dab2)), rep(1, 0.3 * nrow(death_dab2))))
dab_train2 <- death_dab2[split_dummy == 0, ] 
dab_test2 <- death_dab2[split_dummy == 1, ] 
death_cox2 <- coxph(Surv(time, event==1)~BA_CA, data=dab_train2,x=TRUE)
extractAIC(death_cox2)
predict_data2<- ifelse(exp(-predict(death_cox2, type='expected', newdata=dab_test2))<0.7,1,0)
confusion_matrix2<- score(predict=predict_data2, test=dab_test2)

# nri 계산

calculateNRI(compare=confusion_matrix1, exp=confusion_matrix2)
#####


calculateNRI(compare=confusion_matrix1, exp=confusion_matrix2)


'########################'
cat('age :', age_label, 'sex :', gender)


# disease cox HR
disease_target<- "heart"
strings <- c('I20','I21','I22','I23','I24','I25')

dab <-disease_and_BA(dataset_for_cox=dataset_for_cox,BA_data=BA_data,birth_data=birth_data,disease_target, strings)
disease_cox <- coxph(Surv(dab$time, dab$event==1)~dab$BA_CA, data=dab)
split_dummy <- sample(c(rep(0, 0.7 * nrow(dab)), rep(1, 0.3 * nrow(dab))))
dab_train1 <- dab[split_dummy == 0, ] 
dab_test1 <- dab[split_dummy == 1, ] 


disease_cox1 <- coxph(Surv(time, event==1)~BA_CA, data=dab_train1,x=TRUE)
extractAIC(disease_cox1)
predict_data1<- ifelse(exp(-predict(disease_cox1, type='expected', newdata=dab_test1))<0.5,1,0)
confusion_matrix1<- score(predict=predict_data1, test=dab_test1)
summary(disease_cox1)
print(confusion_matrix1)


disease_target<- "brain"
strings <- c('I6')

disease_target<- "cancer"
strings <- c('C')

disease_target<- "DM"
strings <- c('R81')

disease_target<- "HTN"
strings <- c('I1')

# cox HR

dab <-disease_and_BA(dataset_for_cox=dataset_for_cox,BA_data=BA_data,birth_data=birth_data,disease_target, strings)
disease_cox <- coxph(Surv(dab$time, dab$event==1)~dab$BA_CA, data=dab)
split_dummy <- sample(c(rep(0, 0.7 * nrow(dab)), rep(1, 0.3 * nrow(dab))))
dab_train1 <- dab[split_dummy == 0, ] 
dab_test1 <- dab[split_dummy == 1, ] 
disease_cox1 <- coxph(Surv(time, event==1)~BA_CA, data=dab_train1,x=TRUE)
extractAIC(disease_cox1)
predict_data1<- ifelse(exp(-predict(disease_cox1, type='expected', newdata=dab_test1))<0.5,1,0)
confusion_matrix1<- score(predict=predict_data1, test=dab_test1)
summary(disease_cox1)


'TO BE CONTINUE'
#compare 
selected_components <- c('SEX','age','G1E_HGHT','G1E_WGHT', 'G1E_WSTC', 'G1E_BP_SYS', 'G1E_FBS','G1E_HGB','G1E_CRTN', 'G1E_SGOT','G1E_SGPT', 'eGFR','DRK','SMK')#
corrected_BA_parameters<-PCA_model_R(data_12=data_12, age_label=age_label,gender=gender,selected_components=selected_components)
dab <-disease_and_BA(disease_target, strings)
dab%>%dim
split_dummy <- sample(c(rep(0, 0.7 * nrow(dab)), rep(1, 0.3 * nrow(dab))))
dab_train2 <- dab[split_dummy == 0, ] 
dab_test2 <- dab[split_dummy == 1, ] 
disease_cox2 <- coxph(Surv(time, event==1)~BA_CA, data=dab_train2,x=TRUE)
extractAIC(disease_cox2)
predict_data2<- ifelse(exp(-predict(disease_cox2, type='expected', newdata=dab_test2))<0.5,1,0)
confusion_matrix2<- score(predict=predict_data2, test=dab_test2)

calculateNRI(compare=confusion_matrix1, exp=confusion_matrix2)




'PAST'

# death cox HR
death_code_excluded <- c("S", "T") #(사고사인 'S', 'T' 제외)

# cox HR
death_cox <- coxph(Surv(death_dab$time, death_dab$event==1)~death_dab$BA_CA, data=death_dab)
summary(death_cox)
# cox premise
# cox.zph(death_cox,global = F)

# disease cox HR
disease_target<- "heart"
strings <- c('I20','I21','I22','I23','I24','I25')

dab <-disease_and_BA(dataset_for_cox=dataset_for_cox,BA_data=BA_data,birth_data=birth_data,disease_target, strings)
disease_cox <- coxph(Surv(dab$time, dab$event==1)~dab$BA_CA, data=dab)
print(disease_target)
summary(disease_cox)

disease_target<- "brain"
strings <- c('I6')

dab <-disease_and_BA(dataset_for_cox=dataset_for_cox,BA_data=BA_data,birth_data=birth_data,disease_target, strings)
disease_cox <- coxph(Surv(dab$time, dab$event==1)~dab$BA_CA, data=dab)
print(disease_target)
summary(disease_cox)

disease_target<- "cancer"
strings <- c('C')

dab <-disease_and_BA(dataset_for_cox=dataset_for_cox,BA_data=BA_data,birth_data=birth_data,disease_target, strings)
disease_cox <- coxph(Surv(dab$time, dab$event==1)~dab$BA_CA, data=dab)
print(disease_target)
summary(disease_cox)

disease_target<- "DM"
strings <- c('R81')

dab <-disease_and_BA(dataset_for_cox=dataset_for_cox,BA_data=BA_data,birth_data=birth_data,disease_target, strings)
disease_cox <- coxph(Surv(dab$time, dab$event==1)~dab$BA_CA, data=dab)
print(disease_target)
summary(disease_cox)

disease_target<- "HTN"
strings <- c('I1')

dab <-disease_and_BA(dataset_for_cox=dataset_for_cox,BA_data=BA_data,birth_data=birth_data,disease_target, strings)
disease_cox <- coxph(Surv(dab$time, dab$event==1)~dab$BA_CA, data=dab)
print(disease_target)
summary(disease_cox)

# cox HR


# cox premise
cox.zph(disease_cox,global = F)

