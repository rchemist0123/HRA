
getwd()

# library list #
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
library(moonBook)

# function list #

count_id<-function(data){
  data_temp<-data%>%select(RN_INDI)
  data_temp<-unique(data_temp)
  return(dim(data_temp)[1])
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
  write.csv(cov_norm, paste0('working/result_pca_r/covariance_matrix_all_',gender,'_',age_label,'.csv'))
  
  
  #### 3. get PC vs varaible_all_include_age table
  pca_table <- cov_matrix_to_PCA_table(cov_matrix=cov_norm, components=components_all)
  write.csv(pca_table, paste0('working/result_pca_r/PC_variable_Table_all_',gender,'_',age_label,'.csv'))
  
  #### 4. selected components PCA
  cov_norm_selected <- select_norm_data_and_cov_matrix(data_specific_gender_all_components=data_specific_gender_all_components, gender=gender, components=selected_components)
  write.csv(cov_norm_selected, paste0('working/result_pca_r/covariance_matrix_selected_',gender,'_',age_label,'.csv'))
  result <- cov_matrix_to_PCA_table(cov_matrix=cov_norm_selected, components=selected_components)
  write.csv(result, paste0('working/result_pca_r/PC_variable_table_selected_',gender,'_',age_label,'.csv'))
  
  
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
    write.csv(data_with_BA, paste0('working/result_pca_r/data_with_BA_',gender,'_',age_label,'.csv'))
    colnames(corrected_BA_parameters)<-c('parameter', 'coef')
    write.csv(corrected_BA_parameters, paste0('working/result_pca_r/corrected_biological_age_coef_',gender,'_',age_label,'.csv'))
  } else if(ML_TF==TRUE){
    ML_model<-RF_DM(data_12=data_12 , SEED=12345, save=FALSE)
    # ML_model<-readRDS('working/pcadata/ML_model.rda')
    
    #### 8. Calculate BA to all 0912 dataset
    file_name <- paste0('working/result_pca_r/dataset_0912_',age_label,'.csv')
    data_0912_12 = read.csv(file_name)
    data_0912_12<-data_0912_12%>%filter(SEX==gender)
    data_with_BA_ML <- BA_add_to_dataset_ML(ML_model=ML_model, all_data=data_0912_12, dictionary =corrected_BA_parameters, components=selected_components, corrected=T)
    write.csv(data_with_BA_ML, paste0('working/result_pca_r/data_with_BA_',gender,'_',age_label,'_ML.csv'))
    colnames(corrected_BA_parameters)<-c('parameter', 'coef')
    write.csv(corrected_BA_parameters, paste0('working/result_pca_r/corrected_biological_age_coef_',gender,'_',age_label,'_ML.csv'))
  }
  return(corrected_BA_parameters)
}

disease_and_BA <- function(BA_data,dataset_for_cox, birth_data,disease_target, strings){
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
  BA_data$BA_CA <- BA_data$biologic_age
  
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
            paste0('working/result_pca_r/BA_CA_difference_and_disease_',disease_target, '_',gender,'.csv'))
  
  return(BA_dz_selected)
}

death_and_BA <-function(BA_data,dataset_for_cox,birth_data,death_code_excluded){
  BA_data <- left_join(BA_data, dataset_for_cox, by=c('RN_INDI','age'))
  
  death_data<-birth_data
  death_data_save<-death_data
  death_data<-death_data_save
  
  
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
  BA_data$BA_CA <- BA_data$biologic_age
  
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
  
  write.csv(as.data.frame(BA_dz_selected), paste0('working/result_pca_r/BA_CA_difference_and_death_',gender,'.csv'))
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
 
  cat(' @@  ', disease_target, '  @@', '\n')

  dab <-disease_and_BA(dataset_for_cox=dataset_for_cox,BA_data=BA_data,birth_data=birth_data,disease_target, strings)
  dab<- as.data.frame(dab)
   dab_temp<-dab%>%filter(event==1)
  cat('total num :' , dim(dab)[1],' event num :' , dim(dab_temp)[1],'\n')

  if (disease_target =='HTN'){
    BA_data_temp<- BA_data%>% select(RN_INDI, G1E_BP_SYS)
    BA_data_temp<-BA_data_temp%>%group_by(RN_INDI) %>% summarise(maxsbp=max(G1E_BP_SYS))
    BA_data_temp$sbp_event<-ifelse(BA_data_temp$maxsbp>=140,1,0)
    dab_temp<-left_join(dab, BA_data_temp, by='RN_INDI')
    dab_temp$event_temp<-dab_temp$event
    dab_temp$event<-ifelse(dab_temp$event_temp==1 | dab_temp$sbp_event==1 , 1, 0)
    dab<-dab_temp%>%select(RN_INDI, BA_CA, time, event, whohavedisease, event_time)
  }else if (disease_target =='DM'){
    BA_data_temp <- BA_data %>% select(RN_INDI, G1E_FBS)
    BA_data_temp <- BA_data_temp %>% group_by(RN_INDI) %>% summarise(maxfbs = max(G1E_FBS))
    BA_data_temp$fbs_event <- ifelse(BA_data_temp$maxfbs >= 126, 1, 0)
    dab_temp <- left_join(dab, BA_data_temp, by = 'RN_INDI')
    dab_temp$event_temp <- dab_temp$event
    dab_temp$event <-
      ifelse(dab_temp$event_temp == 1 | dab_temp$fbs_event == 1 , 1, 0)
    dab <- dab_temp %>% select(RN_INDI, BA_CA, time, event, whohavedisease, event_time)
  }else{
    dab<-dab
  }
  dab_temp<- left_join(dab, BA_data, by='RN_INDI')
  scs<-c(selected_components, 'BA_CA','time','event')
  scs<-scs[-c(1)] 
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
   cat('using : ', final_coef, '\n')
  cat('BA exp(coef) : ' ,summary(disease_cox1)$coefficients['BA_CA',2], '\t')
  cat('95CI : ', exp(confint(disease_cox1))['BA_CA',],'\t')
  cat('BA p : ' ,summary(disease_cox1)$coefficients['BA_CA',5], '\t')
  cat('C-index : ', summary(disease_cox1)$concordance[1], '\n')
  print('')
  # print(summary(disease_cox1))
}

death_cox_fn<-function(BA_data,death_code_excluded){

  print('@@ death @@')
 
  death_dab1<-death_and_BA(dataset_for_cox=dataset_for_cox,BA_data=BA_data,birth_data=birth_data,death_code_excluded)
  dab_temp<-death_dab1%>%filter(event==1)
  cat('total num :' , dim(death_dab1)[1],' event num :' , dim(dab_temp)[1],'\n')
  death_dab1_temp<- left_join(death_dab1, BA_data, by='RN_INDI')
  scs<-c(selected_components, 'BA_CA','time','event')
  scs<-scs[-c(1)]
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
  
  cat('using : ', final_coef, '\n')
  cat('BA exp(coef) : ' ,summary(death_cox1)$coefficients['BA_CA',2], '\t')
  cat('95CI : ', exp(confint(death_cox1))['BA_CA',],'\t')
  cat('BA p : ' ,summary(death_cox1)$coefficients['BA_CA',5], '\t')
  cat('C-index : ', summary(death_cox1)$concordance[1], '\n')
  
  #print(summary(death_cox1))
  print('')
  
}


death_coxHR  <-function(dataset_for_cox,age_label, gender, ML_TF=FALSE,selected_components){
  file_name <- paste0('working/result_pca_r/dataset_0912_healthy_',age_label,'.csv')
  data_12 = read.csv(file_name)
  data_specific_gender <- data_12 %>% filter(data_12$SEX==gender)
  
  if (ML_TF==TRUE){
    print("######################################################")
    print("######## using MACHINE LEARNING  ###  ##########")
    print("######################################################")
    corrected_BA_parameters <-PCA_model_R(age_label=age_label,gender=gender,selected_components=selected_components, ML_TF=TRUE)
    BA_data<-read.csv( paste0('working/result_pca_r/data_with_BA_',gender,'_',age_label,'_ML.csv'))
  }else if (ML_TF==FALSE) {
    corrected_BA_parameters <-PCA_model_R(age_label=age_label,gender=gender,selected_components=selected_components, ML_TF=FALSE)
    BA_data<-read.csv( paste0('working/result_pca_r/data_with_BA_',gender,'_',age_label,'.csv'))
  }
  
  # print('###### coefficients of biologic age #######')
  print(paste('### age range',age_label, 'sex :', gender, ' ###'))
  # print( corrected_BA_parameters)
  #print("#########################################")
  
  
  # death cox HR
  death_code_excluded <- c("S", "T") #(사고사인 'S', 'T' 제외)
  
  death_cox_fn(BA_data=BA_data,death_code_excluded)
  
}

print_BA_coef<-function(age_label, gender, ML_TF=FALSE, selected_components){
  
  corrected_BA_parameters <-PCA_model_R(age_label=age_label,gender=gender,selected_components=selected_components, ML_TF=FALSE)
  print('###### coefficients of biologic age #######')
  print(paste('age range',age_label, 'sex :', gender))
  print( corrected_BA_parameters)
  print('###### ### ### ### ### ### ### ### #######')
  
}

disease_coxHR  <-function(disease_target,strings, data_12,dataset_for_cox,age_label, gender, ML_TF=FALSE,selected_components){
  file_name <- paste0('working/result_pca_r/dataset_0912_healthy_',age_label,'.csv')
  data_12 = read.csv(file_name)
  data_specific_gender <- data_12 %>% filter(data_12$SEX==gender)
  
  if(ML_TF==TRUE) {
    print("######################################################")
    print("#####   ### using MACHINE LEARGING  #######   ######")
    print("######################################################")
    corrected_BA_parameters <-PCA_model_R(age_label=age_label,gender=gender,selected_components=selected_components, ML_TF=TRUE)
    BA_data<-read.csv( paste0('working/result_pca_r/data_with_BA_',gender,'_',age_label,'_ML.csv'))
  }else if (ML_TF==FALSE) {
    corrected_BA_parameters <-PCA_model_R(age_label=age_label,gender=gender,selected_components=selected_components, ML_TF=FALSE)
    BA_data<-read.csv( paste0('working/result_pca_r/data_with_BA_',gender,'_',age_label,'.csv'))
  }
  
  # print('###### coefficients of biologic age #######')
  print(paste('### age range',age_label, 'sex :', gender, ' ###'))
  # print( corrected_BA_parameters)
  
  disease_cox_fn(BA_data=BA_data,disease_target,strings)
  
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
xgb_ci_back<-function(xgparams, xgb_train, xgb_test,test_y,multi){

  i<-0
  ROCS<-0
  accuracys<-0
  precisions<-0
  recalls<-0
  f1scores<-0
  # xgb.plot.shap(model=ML_model, top_n=6, n_col = 2,test_x)
  
  
  while(i<1){
    cat(i,' ')
    ML_model <- xgb.train(params=xgparams, data= xgb_train, nrounds=200, verbose=0 )
    if (multi==TRUE){
      pred_y = predict(ML_model, xgb_test, type= 'response')
      # Confusion matrix
      result_ROC<-multiclass.roc(test_y, predictor=pred_y)
    } else if(multi==FALSE) {
      pred_y = predict(ML_model, xgb_test)
      result_ROC<-roc(test_y, predictor=pred_y)
      
      thres<- coords(result_ROC, "best")[1]
      pred_y<- ifelse(pred_y>thres,1,0)
      confusion_matrix = table(test_y,pred_y)
      accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix)
      precision = confusion_matrix[2,2]/sum(confusion_matrix[,2])
      recall = confusion_matrix[2,2]/sum(confusion_matrix[2,])
      f1score = 2*(precision*recall)/(precision+recall)
      
    }
    ROC<-result_ROC$auc
    ROC<-data.frame(ROC)
    ROCS<-rbind(ROCS,ROC)
    
    accuracy<-data.frame(accuracy)
    accuracys<-rbind(accuracys,accuracy)
    
    precision<-data.frame(precision)
    precisions<-rbind(precisions,precision)
    
    recall<-data.frame(recall)
    recalls<-rbind(recalls,recall)
    
    f1score<-data.frame(f1score)
    f1scores<-rbind(f1scores,f1score)
    
    i<-i+1
  }
  ROCS<-ROCS[-1,]
  ROCS<-data.frame(ROCS)
  print('')
  print('################')
  cat('C.I. for ROC','\n')
  model_roc<-lm(ROCS~1, ROCS)
  ci<-confint(model_roc, level=0.95)
  print(ci)
  cat('mean+-sd :', mean(ci), mean(ci)-ci[1],'\n')
  
  accuracys<-accuracys[-1,]
  accuracys<-data.frame(accuracys)
  #cat('C.I. for accuracys','\n')
  model_accuracys<-lm(accuracys~1, accuracys)
  ci<-confint(model_accuracys, level=0.95)
  #print(ci)
  #cat('mean+-sd :', mean(ci), mean(ci)-ci[1],'\n')
  
  #precisions<-precisions[-1,]
  #precisions<-data.frame(precisions)
  #cat('C.I. for precisions','\n')
  #model_precisions<-lm(precisions~1, precisions)
  #ci<-confint(model_precisions, level=0.95)
  #print(ci)
  #cat('mean+-sd :', mean(ci), mean(ci)-ci[1],'\n')
  
 #recalls<-recalls[-1,]
  #recalls<-data.frame(recalls)
  #cat('C.I. for recalls','\n')
  #model_recalls<-lm(recalls~1, recalls)
  #ci<-confint(model_recalls, level=0.95)
  #print(ci)
  #cat('mean+-sd :', mean(ci), mean(ci)-ci[1],'\n')
  
  f1scores<-f1scores[-1,]
  f1scores<-data.frame(f1scores)
  #cat('C.I. for f1scores','\n')
  model_f1scores<-lm(f1scores~1, f1scores)
  ci<-confint(model_f1scores, level=0.95)
  #print(ci)
  #cat('mean+-sd :', mean(ci), mean(ci)-ci[1],'\n')
  print(confusion_matrix)
}
xgb_ci_back_4para<-function(xgparams, xgb_train, xgb_test,test_y,multi){
  
  i<-0
  ROCS<-0
  accuracys<-0
  precisions<-0
  recalls<-0
  f1scores<-0
  # xgb.plot.shap(model=ML_model, top_n=6, n_col = 2,test_x)
  
  
  while(i<3){
    cat(i,' ')
    ML_model <- xgb.train(params=xgparams, data= xgb_train, nrounds=200, verbose=0 )
    if (multi==TRUE){
      pred_y = predict(ML_model, xgb_test, type= 'response')
      # Confusion matrix
      result_ROC<-multiclass.roc(test_y, predictor=pred_y)
    } else if(multi==FALSE) {
      pred_y = predict(ML_model, xgb_test)
      result_ROC<-roc(test_y, predictor=pred_y)
      
      thres<- coords(result_ROC, "best")[1]
      pred_y<- ifelse(pred_y>thres,1,0)
      confusion_matrix = table(test_y,pred_y)
      accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix)
      precision = confusion_matrix[2,2]/sum(confusion_matrix[,2])
      recall = confusion_matrix[2,2]/sum(confusion_matrix[2,])
      f1score = 2*(precision*recall)/(precision+recall)
      
    }
    ROC<-result_ROC$auc
    ROC<-data.frame(ROC)
    ROCS<-rbind(ROCS,ROC)
    
    accuracy<-data.frame(accuracy)
    accuracys<-rbind(accuracys,accuracy)
    
    precision<-data.frame(precision)
    precisions<-rbind(precisions,precision)
    
    recall<-data.frame(recall)
    recalls<-rbind(recalls,recall)
    
    f1score<-data.frame(f1score)
    f1scores<-rbind(f1scores,f1score)
    
    i<-i+1
  }
  ROCS<-ROCS[-1,]
  ROCS<-data.frame(ROCS)
  print('')
  print('################')
  cat('C.I. for ROC','\n')
  model_roc<-lm(ROCS~1, ROCS)
  ci<-confint(model_roc, level=0.95)
  print(ci)
  cat('mean+-sd :', mean(ci), mean(ci)-ci[1],'\n')
  
  accuracys<-accuracys[-1,]
  accuracys<-data.frame(accuracys)
  cat('C.I. for accuracys','\n')
  model_accuracys<-lm(accuracys~1, accuracys)
  ci<-confint(model_accuracys, level=0.95)
  print(ci)
  cat('mean+-sd :', mean(ci), mean(ci)-ci[1],'\n')
  
  #precisions<-precisions[-1,]
  #precisions<-data.frame(precisions)
 # cat('C.I. for precisions','\n')
  #model_precisions<-lm(precisions~1, precisions)
  #ci<-confint(model_precisions, level=0.95)
  #print(ci)
  #cat('mean+-sd :', mean(ci), mean(ci)-ci[1],'\n')
  
  #recalls<-recalls[-1,]
  #recalls<-data.frame(recalls)
  #cat('C.I. for recalls','\n')
  #model_recalls<-lm(recalls~1, recalls)
  #ci<-confint(model_recalls, level=0.95)
  #print(ci)
  #cat('mean+-sd :', mean(ci), mean(ci)-ci[1],'\n')
  
  f1scores<-f1scores[-1,]
  f1scores<-data.frame(f1scores)
  cat('C.I. for f1scores','\n')
  model_f1scores<-lm(f1scores~1, f1scores)
  ci<-confint(model_f1scores, level=0.95)
  print(ci)
  cat('mean+-sd :', mean(ci), mean(ci)-ci[1],'\n')
  print(confusion_matrix)
}

disease_xgb <-function(age_label,disease_target,strings, gender,selected_components, BA_TF){
  #file_name <- paste0('working/result_pca_r/dataset_0912_healthy_',age_label,'.csv')
  #data_12 = read.csv(file_name)
  #data_specific_gender <- data_12 %>% filter(data_12$SEX==gender)
  #corrected_BA_parameters <-PCA_model_R(age_label=age_label,gender=gender,selected_components=selected_components, ML_TF=FALSE)
  BA_data<-read.csv( paste0('working/result_pca_r/data_with_BA_',gender,'_',age_label,'.csv'))
  
  
  dab <-disease_and_BA(dataset_for_cox=dataset_for_cox,BA_data=BA_data,birth_data=birth_data,disease_target, strings)
  BA_data_event<-dab%>%select(RN_INDI,BA_CA,event)
  BA_data_event<-left_join(BA_data, BA_data_event, by='RN_INDI')  
  BA_data_with_event_temp1<- BA_data_event %>% group_by(RN_INDI) %>% summarise(mage=min(age))
  BA_data_event<- left_join(BA_data_event, BA_data_with_event_temp1, by='RN_INDI')
  BA_data_event<- BA_data_event %>% filter(age==mage) %>%select(-mage)
  BA_data_event$CA<- BA_data_event$age
  
  
  if (BA_TF == TRUE){
    sc<-c(selected_components ,'CA','event')[-1]
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
  
  xgparams = list(eta=0.1 , max_depth=2, gamma= 0.1, objective='binary:logistic',min_child_weight=2, colsample_bytree=1, subsample=1)
  ML_model <- xgb.train(params=xgparams, data= xgb_train, nrounds=200, verbose=0 )
  
  pred_y = predict(ML_model, xgb_test)
  # shap_values<-predict(ML_model, xgb_test, predcontrib=TRUE)

  cat('#### age label : ', age_label, 'gender : ' , gender, 'disease : ', disease_target, '####', '\n')
  
  # xgb.plot.shap(model=ML_model, top_n=6, n_col = 2,test_x)
  
  print("###### importance #######")
  print(xgb.importance(colnames(xgb_train), model=ML_model)[c(1,2,3,4,5),])
  #result_ROC<-roc(test_y, predictor=pred_y)
  #thres<- coords(result_ROC, "best")[1]
  #pred_y<- ifelse(pred_y>=thres,1,0)
  confusion_matrix = table(test_y,pred_y)
  #accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix)
  #Precision = confusion_matrix[2,2]/sum(confusion_matrix[,2])
  #Recall = confusion_matrix[2,2]/sum(confusion_matrix[2,])
  #f1score = 2*(Precision*Recall)/(Precision+Recall)
  #spec = confusion_matrix[1,1]/sum(confusion_matrix[1,])
  #print("###### roc #######")
  #print(result_ROC)
  xgb_ci_back_4para(xgparams, xgb_train, xgb_test,test_y, multi=FALSE)
  ###
  'AUPRC'
  ###
  # cat("# postive rates : ",sum(confusion_matrix[2,])/sum(confusion_matrix),"\n")
  
  #print(confusion_matrix)
  #print("###### confusionMatrix #######")
  #cat("# Precision: ",Precision, " # accuracy: ",accuracy,'\t')
  #cat("# Sensitivity: ",Recall," # Specificity: ",spec,"\t")
  #cat("# F1-score: ",f1score,"\n")
  #print('')
  
  return(ML_model)
}

death_xgb <-function(age_label, gender,selected_components, BA_TF){
  #file_name <- paste0('working/result_pca_r/dataset_0912_healthy_',age_label,'.csv')
  #data_12 = read.csv(file_name)
  #data_specific_gender <- data_12 %>% filter(data_12$SEX==gender)
  #corrected_BA_parameters <-PCA_model_R(age_label=age_label,gender=gender,selected_components=selected_components, ML_TF=FALSE)
  BA_data<-read.csv( paste0('working/result_pca_r/data_with_BA_',gender,'_',age_label,'.csv'))
  
  death_code_excluded <- c("S", "T") #(사고사인 'S', 'T' 제외)
  
  dab <-death_and_BA(dataset_for_cox=dataset_for_cox,BA_data=BA_data,birth_data=birth_data,death_code_excluded=death_code_excluded)
  BA_data_event<-dab%>%select(RN_INDI,BA_CA,event)
  BA_data_event<-left_join(BA_data, BA_data_event, by='RN_INDI')  
  BA_data_with_event_temp1<- BA_data_event %>% group_by(RN_INDI) %>% summarise(mage=min(age))
  BA_data_event<- left_join(BA_data_event, BA_data_with_event_temp1, by='RN_INDI')
  BA_data_event<- BA_data_event %>% filter(age==mage) %>%select(-mage)
  BA_data_event$CA<- BA_data_event$age
 
  if (BA_TF == TRUE){
    sc<-c(selected_components,'CA','event')[-1]
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
  
  xgparams = list(eta=0.1 , max_depth=2, gamma= 0.1, objective='binary:logistic',min_child_weight=2, colsample_bytree=1, subsample=1)
  ML_model <- xgb.train(params=xgparams, data= xgb_train, nrounds=200, verbose=0 )
  
  pred_y = predict(ML_model, xgb_test)
  # shap_values<-predict(ML_model, xgb_test, predcontrib=TRUE)

  cat('#### age label : ', age_label, 'gender : ' , gender, '  death ', '####', '\n')
  
  # xgb.plot.shap(model=ML_model, top_n=6, n_col = 2,test_x)
  
  print("###### importance #######")
  print(xgb.importance(colnames(xgb_train), model=ML_model)[c(1,2,3,4,5),])
  #result_ROC<-roc(test_y, predictor=pred_y)
  #thres<- coords(result_ROC, "best")[1]
  #pred_y<- ifelse(pred_y>=thres,1,0)
  confusion_matrix = table(test_y,pred_y)
  #accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix)
  #Precision = confusion_matrix[2,2]/sum(confusion_matrix[,2])
  #Recall = confusion_matrix[2,2]/sum(confusion_matrix[2,])
  #f1score = 2*(Precision*Recall)/(Precision+Recall)
  #spec = confusion_matrix[1,1]/sum(confusion_matrix[1,])
  xgb_ci_back_4para(xgparams, xgb_train, xgb_test,test_y, multi=FALSE)
  #print("###### roc #######")
  #print(result_ROC)
  ###
  'AUPRC'
  ###
  #cat("# postive rates : ",sum(confusion_matrix[2,])/sum(confusion_matrix),"\n")
  
  
  #print("###### confusionMatrix #######")
  #print(confusion_matrix)
  #cat("# Precision: ",Precision, " # accuracy: ",accuracy,'\t')
  #cat("# Sensitivity: ",Recall," # Specificity: ",spec,"\t")
  #cat("# F1-score: ",f1score,"\n")
  
   print('')
  
  
  return(ML_model)
}

cox_series_disease<-function(age_label, disease_target, strings, gender,selected_components){
  
  #age_label<-'30' ; disease_coxHR (disease_target=disease_target,strings=strings,data_12=data_12,dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components)
  #age_label<-'40' ; disease_coxHR (disease_target=disease_target,strings=strings,data_12=data_12,dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components)
  #age_label<-'50' ; disease_coxHR (disease_target=disease_target,strings=strings,data_12=data_12,dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components)
  #age_label<-'60' ; disease_coxHR (disease_target=disease_target,strings=strings,data_12=data_12,dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components)
  age_label<-'' ; disease_coxHR (disease_target=disease_target,strings=strings,data_12=data_12,dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components)
  
}

cox_series_death<-function(age_label,gender,selected_components){
  #age_label<-'30' ; death_coxHR(dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components)
  #age_label<-'40' ; death_coxHR(dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components)
  #age_label<-'50' ; death_coxHR(dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components)
  #age_label<-'60' ; death_coxHR(dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components)
  age_label<-'' ; death_coxHR(dataset_for_cox=dataset_for_cox,age_label=age_label, gender=gender,selected_components=selected_components)
  
}

cox_series_all<-function(age_label,gender,selected_components){
  cox_series_death(age_label=age_label, gender=gender,selected_components=selected_components)
  
   disease_target<- "heart" ; strings <- c('I20','I21','I22','I23','I24','I25')
  cox_series_disease(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components)
  
  
  disease_target<- "brain"; strings <- c('I6')
  cox_series_disease(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components)
  
  if (gender ==1 ){
    disease_target<- "cancer";  strings <- c('C33','C34','C16','C18','C19','C20','C61','C22')
  } else if (gender ==2){
    disease_target<- "cancer"; strings <- c('C50','D05','C18','C19','C20','C16','C33','C34','C22')
  }
  cox_series_disease(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components)
  
  disease_target<- "HTN" ; strings <- c('I10','I11','I12','I13','I14','I15') 
  cox_series_disease(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components)
  disease_target<- "DM" ; strings <- c('R81','E10','E11','E12','E13','E14')
  cox_series_disease(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components)
  
}

xgb_series_disease<-function(age_label, disease_target, strings, gender,selected_components, BA_TF){
  #age_label<-'30' ; xgb_fit<-disease_xgb(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
  #age_label<-'40' ; xgb_fit<-disease_xgb(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
  #age_label<-'50' ; xgb_fit<-disease_xgb(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
  #age_label<-'60' ; xgb_fit<-disease_xgb(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
  age_label<-'' ; xgb_fit<-disease_xgb(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
}

xgb_series_death<-function(age_label,gender,selected_components, BA_TF){
  #age_label<-'30' ; xgb_fit<-death_xgb(age_label=age_label, gender=gender,selected_components=selected_components, BA_TF=TRUE)
  #age_label<-'40' ; xgb_fit<-death_xgb(age_label=age_label, gender=gender,selected_components=selected_components, BA_TF=TRUE)
  #age_label<-'50' ; xgb_fit<-death_xgb(age_label=age_label, gender=gender,selected_components=selected_components, BA_TF=TRUE)
  #age_label<-'60' ; xgb_fit<-death_xgb(age_label=age_label, gender=gender,selected_components=selected_components, BA_TF=TRUE)
  age_label<-'' ; xgb_fit<-death_xgb(age_label=age_label,gender=gender,selected_components=selected_components, BA_TF=TRUE)
}

xgb_series_all<-function(age_label,gender,selected_components,BA_TF){
  xgb_series_death(age_label=age_label, gender=gender,selected_components=selected_components, BA_TF=TRUE)
   
  disease_target<- "heart" ; strings <- c('I20','I21','I22','I23','I24','I25')
  xgb_series_disease(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
  
  disease_target<- "brain"; strings <- c('I6')
  xgb_series_disease(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
  
  if (gender ==1 ){
    disease_target<- "cancer";  strings <- c('C33','C34','C16','C18','C19','C20','C61','C22')
  } else if (gender ==2){
    disease_target<- "cancer"; strings <- c('C50','D05','C18','C19','C20','C16','C33','C34','C22')
  }
  xgb_series_disease(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
  
  disease_target<- "HTN" ; strings <- c('I10','I11','I12','I13','I14','I15') 
  xgb_series_disease(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
  
  disease_target<- "DM" ; strings <- c('R81','E10','E11','E12','E13','E14')
  xgb_series_disease(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
  
}

BA_series<-function(gender){
  age_label<-'30' ;print_BA_coef(age_label = age_label, gender= gender, selected_components = selected_components)
  age_label<-'40' ;print_BA_coef(age_label = age_label, gender= gender, selected_components = selected_components)
  age_label<-'50' ;print_BA_coef(age_label = age_label, gender= gender, selected_components = selected_components)
  age_label<-'60' ;print_BA_coef(age_label = age_label, gender= gender, selected_components = selected_components)
  age_label<-'' ;print_BA_coef(age_label = age_label, gender= gender, selected_components = selected_components)
}
grid_search_death<-function(gender,selected_components, BA_TF){
  BA_TF=TRUE
  age_label=''
  file_name <- paste0('working/result_pca_r/dataset_0912_healthy_',age_label,'.csv')
  data_12 = read.csv(file_name)
  data_specific_gender <- data_12 %>% filter(data_12$SEX==gender)
  corrected_BA_parameters <-PCA_model_R(age_label=age_label,gender=gender,selected_components=selected_components, ML_TF=FALSE)
  BA_data<-read.csv( paste0('working/result_pca_r/data_with_BA_',gender,'_',age_label,'.csv'))
  
  death_code_excluded <- c("S", "T") #(사고사인 'S', 'T' 제외)
  
  dab <-death_and_BA(dataset_for_cox=dataset_for_cox,BA_data=BA_data,birth_data=birth_data,death_code_excluded=death_code_excluded)
  BA_data_event<-dab%>%select(RN_INDI,BA_CA,event)
  BA_data_event<-left_join(BA_data, BA_data_event, by='RN_INDI')  
  BA_data_with_event_temp1<- BA_data_event %>% group_by(RN_INDI) %>% summarise(mage=min(age))
  BA_data_event<- left_join(BA_data_event, BA_data_with_event_temp1, by='RN_INDI')
  BA_data_event<- BA_data_event %>% filter(age==mage) %>%select(-mage)
  BA_data_event$CA<- BA_data_event$age
  
  
  if (BA_TF == TRUE){
    sc<-c(selected_components,'CA' ,'event')[-1]
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
  
  ML_tr_xgb<-ML_tr
  ML_tr_xgb$event<-as.factor(ML_tr_xgb$event)
  ML_te_xgb<-ML_te
  ML_te_xgb$event<-as.factor(ML_te_xgb$event)
  train_control = trainControl(method = "cv", number = 5, search = "grid")
  print('@@@ grid search @@@')
  gbmGrid <-  expand.grid(max_depth = c(2), 
                          nrounds = c(200), 
                          eta = c(0.001,0.005,0.01,0.02,0.05,0.1,0.2),
                          gamma = c(0.1),
                          subsample = c(1),
                          min_child_weight = c(2),
                          colsample_bytree = c(0.75))
  
  model = train(event~., data = rbind(ML_tr_xgb,ML_te_xgb), method = "xgbTree", trControl = train_control, tuneGrid = gbmGrid)
  print(model)
}

'### ### ### ###'
'    process 1  '
'### ### ### ###'
'do it first time'
# db 전처리##
raw_g1e0915<-read.csv('data_source/user_data/nsc2_g1e_0915_db.csv')
MK_data<-read.csv('data_source/user_data/mk_data.csv')
MK_data<-MK_data%>%filter(STD_YYYY>=2009)
birth_data<-read.csv('data_source/user_data/nsc2_bnd_db.csv')
#raw_bnc<-read.csv('data_source/user_data/nsc2_bnc_db.csv')
#raw_bnc<-raw_bnc %>% select(RN_INDI, SEX)
#raw_bnc<-unique(raw_bnc)
#write.csv(raw_bnc, 'data_source/user_data/bnc_temp.csv')
raw_bnc<- read.csv('data_source/user_data/bnc_temp.csv')
MK_data<-MK_data%>%filter(STD_YYYY>=2009)
dataset_for_cox<-read.csv('data_source/user_data/dataset_for_cox.csv')

if (exists('MK_data')!=1){
  M_data<-read.csv('data_source/user_data/nsc2_m20_db.csv')
  K_data<-read.csv('data_source/user_data/nsc2_k20_db.csv')
  M_data_2<-read.csv('data_source/user_data/nsc2_m20_db_1619.csv')
  K_data_2<-read.csv('data_source/user_data/nsc2_k20_db_1619.csv')
  MK_data <- rbind(M_data, K_data)
  MK_data<-rbind(MK_data, M_data_2)
  MK_data<-rbind(MK_data, K_data_2)
  MK_data_save<-MK_data
  MK_data$SYMcode<-paste0(MK_data$SICK_SYM1, MK_data$SICK_SYM2)
  MK_data$SYMcode<-as.character(MK_data$SYMcode)
  write.csv(MK_data, paste0('data_source/user_data/mk_data.csv'))
}
db_parameter<-c('RN_INDI', 'SEX', 'age','G1E_HGHT','G1E_WGHT','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_BP_DIA',
                'G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_LDL','G1E_HGB', 'G1E_URN_PROT','G1E_CRTN','G1E_SGOT','G1E_SGPT','G1E_GGT','eGFR')
#DB_parser(raw_g1e0915=raw_g1e0915,birth_data=birth_data,folder_path, parameter=db_parameter)

age_label=''

##2  cox set
#men
gender<-1 
selected_components <- c('SEX','age','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR','Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_DM','Q_FHX_HTN')
cox_series_all(age_label=age_label, gender=gender,selected_components=selected_components)
gender<-1 
selected_components <- c('SEX','age','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR','DRK','SMK','PA','Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_DM','Q_FHX_HTN')
cox_series_all(age_label=age_label, gender=gender,selected_components=selected_components)


#women 
gender<-2
selected_components <-c('SEX','age','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR','Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_DM','Q_FHX_HTN')
cox_series_all(age_label=age_label, gender=gender,selected_components=selected_components)
gender<-2
selected_components <-c('SEX','age','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR','DRK','SMK','PA','Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_DM','Q_FHX_HTN')
cox_series_all(age_label=age_label, gender=gender,selected_components=selected_components)


## 3  xgb set 

#GRID SEARCH

grid_search_death(gender=gender,selected_components=selected_components, BA_TF)
  

# roc comparison
death_xgb_comparison <-function(age_label, gender,selected_components, BA_TF){
  #file_name <- paste0('working/result_pca_r/dataset_0912_healthy_',age_label,'.csv')
  #data_12 = read.csv(file_name)
  #data_specific_gender <- data_12 %>% filter(data_12$SEX==gender)
  #corrected_BA_parameters <-PCA_model_R(age_label=age_label,gender=gender,selected_components=selected_components, ML_TF=FALSE)
  BA_data<-read.csv( paste0('working/result_pca_r/data_with_BA_',gender,'_',age_label,'.csv'))
  
  death_code_excluded <- c("S", "T") #(사고사인 'S', 'T' 제외)
  
  dab <-death_and_BA(dataset_for_cox=dataset_for_cox,BA_data=BA_data,birth_data=birth_data,death_code_excluded=death_code_excluded)
  BA_data_event<-dab%>%select(RN_INDI,BA_CA,event)
  BA_data_event<-left_join(BA_data, BA_data_event, by='RN_INDI')  
  BA_data_with_event_temp1<- BA_data_event %>% group_by(RN_INDI) %>% summarise(mage=min(age))
  BA_data_event<- left_join(BA_data_event, BA_data_with_event_temp1, by='RN_INDI')
  BA_data_event<- BA_data_event %>% filter(age==mage) %>%select(-mage)
  BA_data_event$CA<- BA_data_event$age
  
  if (BA_TF == TRUE){
    sc<-c(selected_components,'event')[-1]
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
  
  xgparams = list(eta=0.1 , max_depth=2, gamma= 0.1, objective='binary:logistic',min_child_weight=2, colsample_bytree=1, subsample=1)
  ML_model <- xgb.train(params=xgparams, data= xgb_train, nrounds=200, verbose=0 )
  
  pred_y = predict(ML_model, xgb_test)
  # shap_values<-predict(ML_model, xgb_test, predcontrib=TRUE)
  
  cat('#### age label : ', age_label, 'gender : ' , gender, '  death ', '####', '\n')
  
  # xgb.plot.shap(model=ML_model, top_n=6, n_col = 2,test_x)
  
  print("###### importance #######")
  print(xgb.importance(colnames(xgb_train), model=ML_model)[c(1,2,3,4,5),])
  result_ROC<-roc(test_y, predictor=pred_y)
  confusion_matrix = table(test_y,pred_y)
  xgb_ci_back_4para(xgparams, xgb_train, xgb_test,test_y, multi=FALSE)
  print('')
  #print("###### confusion_matrix #######")
  #print(confusion_matrix)
  return(result_ROC)
}
disease_xgb_comparison <-function(age_label,disease_target,strings, gender,selected_components, BA_TF){
  #file_name <- paste0('working/result_pca_r/dataset_0912_healthy_',age_label,'.csv')
  #data_12 = read.csv(file_name)
  #data_specific_gender <- data_12 %>% filter(data_12$SEX==gender)
  #corrected_BA_parameters <-PCA_model_R(age_label=age_label,gender=gender,selected_components=selected_components, ML_TF=FALSE)
  BA_data<-read.csv( paste0('working/result_pca_r/data_with_BA_',gender,'_',age_label,'.csv'))
  
  
  dab <-disease_and_BA(dataset_for_cox=dataset_for_cox,BA_data=BA_data,birth_data=birth_data,disease_target, strings)
  BA_data_event<-dab%>%select(RN_INDI,BA_CA,event)
  BA_data_event<-left_join(BA_data, BA_data_event, by='RN_INDI')  
  BA_data_with_event_temp1<- BA_data_event %>% group_by(RN_INDI) %>% summarise(mage=min(age))
  BA_data_event<- left_join(BA_data_event, BA_data_with_event_temp1, by='RN_INDI')
  BA_data_event<- BA_data_event %>% filter(age==mage) %>%select(-mage)
  BA_data_event$CA<- BA_data_event$age
  
  
  if (BA_TF == TRUE){
    sc<-c(selected_components,'event')[-1]
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
  
  xgparams = list(eta=0.1 , max_depth=2, gamma= 0.1, objective='binary:logistic',min_child_weight=2, colsample_bytree=1, subsample=1)
  ML_model <- xgb.train(params=xgparams, data= xgb_train, nrounds=200, verbose=0 )
  
  pred_y = predict(ML_model, xgb_test)
  # shap_values<-predict(ML_model, xgb_test, predcontrib=TRUE)
  
  cat('#### age label : ', age_label, 'gender : ' , gender, 'disease : ', disease_target, '####', '\n')
  
  # xgb.plot.shap(model=ML_model, top_n=6, n_col = 2,test_x)
  
  print("###### importance #######")
  print(xgb.importance(colnames(xgb_train), model=ML_model)[c(1,2,3,4,5),])
  result_ROC<-roc(test_y, predictor=pred_y)
  confusion_matrix = table(test_y,pred_y)
  xgb_ci_back_4para(xgparams, xgb_train, xgb_test,test_y, multi=FALSE)
  print('')
  #print("###### confusion_matrix #######")
  #print(confusion_matrix)
  return(result_ROC)
}


# 1 AUC elevation
age_label<-'' 
gender<-1

selected_components <- c('SEX','age','CA')
xgb_series_all(age_label=age_label,gender=gender,selected_components=selected_components,BA_TF=TRUE)

selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA')
xgb_series_all(age_label=age_label,gender=gender,selected_components=selected_components,BA_TF=TRUE)

selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA','G1E_BP_SYS','G1E_FBS')
xgb_series_all(age_label=age_label,gender=gender,selected_components=selected_components,BA_TF=TRUE)

selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR','DRK','SMK','PA')
xgb_series_all(age_label=age_label,gender=gender,selected_components=selected_components,BA_TF=TRUE)

#selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR','DRK','SMK','PA','Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_DM','Q_FHX_HTN')
#xgb_series_all(age_label=age_label,gender=gender,selected_components=selected_components,BA_TF=TRUE)

selected_components <- c('SEX','age','CA','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR')
xgb_series_all(age_label=age_label,gender=gender,selected_components=selected_components,BA_TF=TRUE)


age_label<-'' 
gender<-2
selected_components <-c('SEX','age','CA')
xgb_series_all(age_label=age_label,gender=gender,selected_components=selected_components,BA_TF=TRUE)

selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA')
xgb_series_all(age_label=age_label,gender=gender,selected_components=selected_components,BA_TF=TRUE)

selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA','G1E_BP_SYS','G1E_FBS')
xgb_series_all(age_label=age_label,gender=gender,selected_components=selected_components,BA_TF=TRUE)

selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR','DRK','SMK','PA')
xgb_series_all(age_label=age_label,gender=gender,selected_components=selected_components,BA_TF=TRUE)

#selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR','DRK','SMK','PA','Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_DM','Q_FHX_HTN')
#xgb_series_all(age_label=age_label,gender=gender,selected_components=selected_components,BA_TF=TRUE)

selected_components <-c('SEX','age','CA','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR')
xgb_series_all(age_label=age_label,gender=gender,selected_components=selected_components,BA_TF=TRUE)


#_2 replace hospital data xgboost auc comparison
age_label<-'' 
gender<-1

selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA')
selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA','G1E_BP_SYS','G1E_FBS')
xgb_fit_1<-death_xgb_comparison(age_label=age_label,gender=gender,selected_components=selected_components, BA_TF=TRUE)
selected_components <- c('SEX','age','CA','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR')
selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR','DRK','SMK','PA')
xgb_fit_2<-death_xgb_comparison(age_label=age_label,gender=gender,selected_components=selected_components, BA_TF=TRUE)
roc.test(xgb_fit_1,xgb_fit_2)

disease_target<- "heart" ; strings <- c('I20','I21','I22','I23','I24','I25')
selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA')
selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA','G1E_BP_SYS','G1E_FBS')
xgb_fit_1<-disease_xgb_comparison(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
selected_components <- c('SEX','age','CA','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR')
selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR','DRK','SMK','PA')
xgb_fit_2<-disease_xgb_comparison(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
roc.test(xgb_fit_1,xgb_fit_2)

disease_target<- "brain"; strings <- c('I6')
selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA')
selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA','G1E_BP_SYS','G1E_FBS')
xgb_fit_1<-disease_xgb_comparison(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
selected_components <- c('SEX','age','CA','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR')
selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR','DRK','SMK','PA')
xgb_fit_2<-disease_xgb_comparison(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
roc.test(xgb_fit_1,xgb_fit_2)

if (gender ==1 ){
  disease_target<- "cancer";  strings <- c('C33','C34','C16','C18','C19','C20','C61','C22')
} else if (gender ==2){
  disease_target<- "cancer"; strings <- c('C50','D05','C18','C19','C20','C16','C33','C34','C22')
}
selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA')
selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA','G1E_BP_SYS','G1E_FBS')
xgb_fit_1<-disease_xgb_comparison(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
selected_components <- c('SEX','age','CA','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR')
selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR','DRK','SMK','PA')
xgb_fit_2<-disease_xgb_comparison(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
roc.test(xgb_fit_1,xgb_fit_2)

disease_target<- "HTN" ; strings <- c('I10','I11','I12','I13','I14','I15') 
selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA')
selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA','G1E_BP_SYS','G1E_FBS')
xgb_fit_1<-disease_xgb_comparison(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
selected_components <- c('SEX','age','CA','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR')
selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR','DRK','SMK','PA')
xgb_fit_2<-disease_xgb_comparison(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
roc.test(xgb_fit_1,xgb_fit_2)


disease_target<- "DM" ; strings <- c('R81','E10','E11','E12','E13','E14')
selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA')
selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA','G1E_BP_SYS','G1E_FBS')
xgb_fit_1<-disease_xgb_comparison(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
selected_components <- c('SEX','age','CA','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR')
selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR','DRK','SMK','PA')
xgb_fit_2<-disease_xgb_comparison(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
roc.test(xgb_fit_1,xgb_fit_2)

age_label<-'' 
gender<-2

selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA')
selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA','G1E_BP_SYS','G1E_FBS')
xgb_fit_1<-death_xgb_comparison(age_label=age_label,gender=gender,selected_components=selected_components, BA_TF=TRUE)
selected_components <-c('SEX','age','CA','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR')
selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR','DRK','SMK','PA')
xgb_fit_2<-death_xgb_comparison(age_label=age_label,gender=gender,selected_components=selected_components, BA_TF=TRUE)
roc.test(xgb_fit_1,xgb_fit_2)

disease_target<- "heart" ; strings <- c('I20','I21','I22','I23','I24','I25')
selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA')
selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA','G1E_BP_SYS','G1E_FBS')
xgb_fit_1<-disease_xgb_comparison(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
selected_components <-c('SEX','age','CA','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR')
selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR','DRK','SMK','PA')
xgb_fit_2<-disease_xgb_comparison(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
roc.test(xgb_fit_1,xgb_fit_2)

disease_target<- "brain"; strings <- c('I6')
selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA')
selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA','G1E_BP_SYS','G1E_FBS')
xgb_fit_1<-disease_xgb_comparison(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
selected_components <-c('SEX','age','CA','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR')
selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR','DRK','SMK','PA')
xgb_fit_2<-disease_xgb_comparison(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
roc.test(xgb_fit_1,xgb_fit_2)

if (gender ==1 ){
  disease_target<- "cancer";  strings <- c('C33','C34','C16','C18','C19','C20','C61','C22')
} else if (gender ==2){
  disease_target<- "cancer"; strings <- c('C50','D05','C18','C19','C20','C16','C33','C34','C22')
}
selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA')
selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA','G1E_BP_SYS','G1E_FBS')
xgb_fit_1<-disease_xgb_comparison(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
selected_components <-c('SEX','age','CA','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR')
selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR','DRK','SMK','PA')
xgb_fit_2<-disease_xgb_comparison(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
roc.test(xgb_fit_1,xgb_fit_2)

disease_target<- "HTN" ; strings <- c('I10','I11','I12','I13','I14','I15') 
selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA')
selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA','G1E_BP_SYS','G1E_FBS')
xgb_fit_1<-disease_xgb_comparison(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
selected_components <-c('SEX','age','CA','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR')
selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR','DRK','SMK','PA')
xgb_fit_2<-disease_xgb_comparison(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
roc.test(xgb_fit_1,xgb_fit_2)


disease_target<- "DM" ; strings <- c('R81','E10','E11','E12','E13','E14')
selected_components <- c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA')
selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','DRK','SMK','PA','G1E_BP_SYS','G1E_FBS')
xgb_fit_1<-disease_xgb_comparison(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
selected_components <-c('SEX','age','CA','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR')
selected_components <-c('SEX','age','CA','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR','DRK','SMK','PA')
xgb_fit_2<-disease_xgb_comparison(age_label=age_label,disease_target=disease_target,strings=strings, gender=gender,selected_components=selected_components, BA_TF=TRUE)
roc.test(xgb_fit_1,xgb_fit_2)




'###########################'
# ba
calculate_ba<-function(){
  # Man
  input_12 <- read.csv('input/input.csv')
  input_1<-input_12%>%filter(GENDER==1)%>%select(-GENDER)
  # BA
  input<-input_1 %>%select(-ID)
  input<-input %>% select(AGE, BMI, WSTC, SBP, FBS, TCHOL, HDL, HGB, ALT, CRTN_EGFR, DRK, SMK_EX, SMK_PACK, SMK_YR, METS_L, METS_M, METS_H)
  
  input$scrmin<-ifelse(input$CRTN_EGFR<=0.9,input$CRTN_EGFR/0.9,1)
  input$scrmax<-ifelse(input$CRTN_EGFR>=0.9,input$CRTN_EGFR/0.9,1)
  input$EGFR <-141*((input$scrmin)^(-0.411))*((input$scrmax)^(-1.209))*((0.993)^(input$AGE))
  
  input$CRTN_EGFR<-ifelse((input$EGFR >= 90),1,ifelse((input$EGFR >= 60),2, ifelse((input$EGFR >= 30),3, ifelse((input$EGFR >= 15),4, ifelse((input$EGFR >= 0),5)))))
  input$Constant<-1
  
  input_BA<-input%>%select(-EGFR)
  input_BA$SMK_EX<-input_BA$SMK_PACK*input_BA$SMK_YR
  input_BA<-input_BA%>%select(-METS_L)%>%select(-METS_M)%>%select(-METS_H)%>%select(-SMK_YR)%>%select(-SMK_PACK)
  
  BA_dataframe<-matrix(0,dim(input_BA)[1],1)
  for (i in 1:dim(input_BA)[1]){
    i<-as.numeric(i)
    age_label<-input_BA[i,]$AGE%/% 10
    
    if(age_label==2 |age_label==1  ){
      age_label<-3
    }
    if(age_label>=7){
      age_label<-6
    }
    age_label<-age_label*10
    
    filename_coef <- paste0('working/result_pca_r/corrected_biological_age_coef_',1,'_',age_label,'.csv')
    coefs <- read.csv(filename_coef,header = FALSE)
    input_row<-as.data.frame(input_BA[i,])
    input_params<-input_row%>%select(-scrmin)%>%select(-scrmax)
    coefs<-as.data.frame(coefs)
    input_params_mtrx<-as.matrix(input_params)
    coef_mtrx<-as.matrix(coefs[,2])
    BA<-input_params_mtrx %*% coef_mtrx
    BA_dataframe[i,1]<-BA
  }
  
  BA_out<-round(data.frame(BA_dataframe),2)
  BA_out_1<- cbind(input_BA$AGE,BA_out)
  names(BA_out_1)<-c('AGE', 'BA')  
  BA_out_1<-left_join(input_1,BA_out_1, by='AGE') %>% select(ID, AGE, BA)
  
  result_1<-BA_out_1
  
  
  
  
  
  
  # Women
  input_1<-input_12%>%filter(GENDER==2)%>%select(-GENDER)
  # BA
  input<-input_1 %>%select(-ID)
  input<-input %>% select(AGE, BMI, WSTC, SBP, FBS, TCHOL, TG, HDL, ALT, CRTN_EGFR, DRK, SMK_EX, SMK_PACK, SMK_YR, METS_L, METS_M, METS_H)
  
  
  
  input$scrmin<-ifelse(input$CRTN_EGFR<=0.7,input$CRTN_EGFR/0.7,1)
  input$scrmax<-ifelse(input$CRTN_EGFR>=0.7,input$CRTN_EGFR/0.7,1)
  input$EGFR <-1.018*141*((input$scrmin)^(-0.329))*((input$scrmax)^(-1.209))*((0.993)^(input$AGE))
  
  
  input$CRTN_EGFR<-ifelse((input$EGFR >= 90),1,ifelse((input$EGFR >= 60),2, ifelse((input$EGFR >= 30),3, ifelse((input$EGFR >= 15),4, ifelse((input$EGFR >= 0),5)))))
  input$Constant<-1
  
  
  input_BA<-input%>%select(-EGFR)
  input_BA$SMK_EX<-input_BA$SMK_PACK*input_BA$SMK_YR
  input_BA<-input_BA%>%select(-METS_L)%>%select(-METS_M)%>%select(-METS_H)%>%select(-SMK_YR)%>%select(-SMK_PACK)
  
  BA_dataframe<-matrix(0,dim(input_BA)[1],1)
  for (i in 1:dim(input_BA)[1]){
    i<-as.numeric(i)
    age_label<-input_BA[i,]$AGE%/% 10
    if(age_label==2 |age_label==1  ){
      age_label<-3
    }
    if(age_label>=7){
      age_label<-6
    }
    age_label<-age_label*10
    filename_coef <- paste0('working/result_pca_r/corrected_biological_age_coef_',2,'_',age_label,'.csv')
    coefs <- read.csv(filename_coef,header = FALSE)
    input_row<-as.data.frame(input_BA[i,])
    input_params<-input_row%>%select(-scrmin)%>%select(-scrmax)
    coefs<-as.data.frame(coefs)
    input_params_mtrx<-as.matrix(input_params)
    coef_mtrx<-as.matrix(coefs[,2])
    BA<-input_params_mtrx %*% coef_mtrx
    BA_dataframe[i,1]<-BA
  }
  
  BA_out<-round(data.frame(BA_dataframe),2)
  BA_out_2<- cbind(input_BA$AGE,BA_out)
  names(BA_out_2)<-c('AGE', 'BA')  
  BA_out_2<-left_join(input_1,BA_out_2, by='AGE') %>% select(ID, AGE, BA)
  
  result_2<-BA_out_2
  
  result<-rbind(result_1,result_2)
  result<-result%>%arrange((ID))
  
  write.csv(result,'output/output.csv',row.names = F)  
  
  
}
calculate_ba()

##0 Description

description<-function(age_label){
  file_name <- paste0('working/result_pca_r/dataset_0912_',age_label,'.csv')
  data_12 = read.csv(file_name)
  baba<-(data_12) %>% group_by(RN_INDI) %>% summarise(age=min(age))
  data_12<-left_join(baba, data_12, by=c('RN_INDI','age'))
  mytable(SEX~.-RN_INDI-X, data_12)
  #BA_datat<-BA_data
  #BA_datat$temp<-1
  #mytable(temp~.-RN_INDI-X-ages,BA_datat)
}

description(age_label = '')

##1 biologic age
#basic
#gender=1
#selected_components <- c('SEX','age','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR','DRK','SMK')
#BA_series(gender = gender)
#gender=2
#selected_components <- c('SEX','age','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR','DRK','SMK')
#BA_series(gender = gender)

#advanced
#gender=1
#selected_components <- c('SEX','age','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR','DRK','SMK','Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_DM','Q_FHX_HTN')
#BA_series(gender = gender)
#gender=2
#selected_components <- c('SEX','age','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR','DRK','SMK','Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_DM','Q_FHX_HTN')
#BA_series(gender = gender)

library(Hmisc)

# LS AND BA-CA
gender<-1 
selected_components <- c('SEX','age','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR','DRK','SMK','PA','Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_DM','Q_FHX_HTN')
cox_series_all(age_label=age_label, gender=gender,selected_components=selected_components)

gender<-2
selected_components <- c('SEX','age','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR','DRK','SMK','PA','Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_DM','Q_FHX_HTN')
cox_series_all(age_label=age_label, gender=gender,selected_components=selected_components)



param_ls<-read.csv('working/result_pca_r/corrected_biological_age_coef_1_.csv')
print(param_ls$parameter)
ba_ls<-read.csv('working/result_pca_r/BA_CA_difference_and_death_1.csv')
mean(ba_ls$BA_CA)

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_death_1.csv')
lr_ls_death<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_ls_death
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_disease_DM_1.csv')
lr_ls_dm<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_ls_dm
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_disease_heart_1.csv')
lr_ls_heart<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_ls_heart
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_disease_brain_1.csv')
lr_ls_brain<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_ls_brain
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_disease_cancer_1.csv')
lr_ls_cancer<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_ls_cancer
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_disease_HTN_1.csv')
lr_ls_htn<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_ls_htn
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_death_2.csv')
lr_ls_death_w<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_ls_death_w
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_disease_DM_2.csv')
lr_ls_dm_w<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_ls_dm_w
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_disease_heart_2.csv')
lr_ls_heart_w<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_ls_heart_w
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_disease_brain_2.csv')
lr_ls_brain_w<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_ls_brain_w
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_disease_cancer_2.csv')
lr_ls_cancer_w<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_ls_cancer_w
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_disease_HTN_2.csv')
lr_ls_htn_w<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_ls_htn_w
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

#without_ls


gender<-1 
selected_components <- c('SEX','age','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_HDL','G1E_HGB','G1E_SGPT','eGFR','Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_DM','Q_FHX_HTN')
cox_series_all(age_label=age_label, gender=gender,selected_components=selected_components)

gender<-2
selected_components <- c('SEX','age','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_SGPT','eGFR','Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_DM','Q_FHX_HTN')
cox_series_all(age_label=age_label, gender=gender,selected_components=selected_components)

param_<-read.csv('working/result_pca_r/corrected_biological_age_coef_1_.csv')
print(param_$parameter)
ba<-read.csv('working/result_pca_r/BA_CA_difference_and_death_1.csv')
mean(ba$BA_CA)


cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_death_1.csv')
lr_death<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_death
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_disease_DM_1.csv')
lr_dm<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_dm
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_disease_heart_1.csv')
lr_heart<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_heart
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_disease_brain_1.csv')
lr_brain<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_brain
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_disease_cancer_1.csv')
lr_cancer<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_cancer
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_disease_HTN_1.csv')
lr_htn<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_htn
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_death_2.csv')
lr_death_w<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_death_w
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_disease_DM_2.csv')
lr_dm_w<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_dm_w
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_disease_heart_2.csv')
lr_heart_w<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_heart_w
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_disease_brain_2.csv')
lr_brain_w<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_brain_w
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_disease_cancer_2.csv')
lr_cancer_w<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_cancer_w
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

cv1<-read.csv('working/result_pca_r/BA_CA_difference_and_disease_HTN_2.csv')
lr_htn_w<-glm(data = cv1, event~BA_CA, family = 'binomial')
lr1<-lr_htn_w
cat('# BA-CA coef : ', exp(lr1$coefficients[2]),' # 95CI : ', exp(confint(lr1)[2,]), ' # P-VALUE : ', coef(summary(lr1))[,4], ' # BIC : ' , BIC(lr1), '\n')

anova(lr_death, lr_ls_death,test='Chisq')
anova(lr_dm, lr_ls_dm, test='Chisq')
anova(lr_heart, lr_ls_heart, test='Chisq')
anova(lr_brain, lr_ls_brain, test='Chisq')
anova(lr_cancer, lr_ls_cancer, test='Chisq')
anova(lr_htn, lr_ls_htn, test='Chisq')

anova(lr_death_w, lr_ls_death_w, test='Chisq')
anova(lr_dm_w, lr_ls_dm_w, test='Chisq')
anova(lr_heart_w, lr_ls_heart_w, test='Chisq')
anova(lr_brain_w, lr_ls_brain_w, test='Chisq')
anova(lr_cancer_w, lr_ls_cancer_w, test='Chisq')
anova(lr_htn_w, lr_ls_htn_w, test='Chisq')