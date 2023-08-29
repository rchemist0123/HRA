
getwd()
folder_path<-'/userdata06/room058'
setwd(folder_path)


# library  
library(haven)
library(dplyr)
library(readxl)
library(survival)
library(tidyverse)

# function 



DB_parser<-function(folderpath, parameter){
  
  ## 출생 및 사망테이블(출생년도) 추가
  # read csv
  
  raw_year<-read.csv('working/pcadata/nsc2_bnd_db.csv')
  raw_year_save<-raw_year
  
  # 이상 결측치 확인
  # summary(!is.na(raw_year$BTH_YYYY))
  # table(raw_year$BTH_YYYY) # 이상치 발견
  raw_year<-raw_year %>% filter(BTH_YYYY!='1921LE')
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
  
  # 중복행?
  # dim(temp_for_cal_age)
  temp_for_cal_age <- unique(temp_for_cal_age)
  # dim(temp_for_cal_age)
  
  ## sex항 추가
  raw_bnc<-read.csv('working/pcadata/nsc2_bnc_db.csv')
  raw_bnc<-raw_bnc %>% select(RN_INDI, SEX)
  #중복행?
  # dim(raw_bnc)
  raw_bnc<-unique(raw_bnc)
  # dim(raw_bnc)
  raw_bnc_save<-raw_bnc 
  
  # sex항 결합
  temp_dataset <- left_join(raw_bnc, temp_for_cal_age, by ="RN_INDI")
  
  # dataset 구축
  dataset <- temp_dataset %>% select((parameter))
  
  dataset$SEX <- as.numeric(dataset$SEX)
  dataset_save<-dataset
  # summary(dataset)
  
  # 이상 결측치 확인
  # summary(dataset)
  summary(is.na(dataset)) # 결측치 발견
  dataset<-na.omit(dataset)
  
  FBSunit <- 18.0156
  Cholunit <- 38.66976
  TGunit<-88.57396
  Hgbunit<-10
  # 메디에이지 논문과 unit 통일
  dataset$G1E_FBS <- dataset$G1E_FBS/FBSunit
  dataset$G1E_TOT_CHOL <- dataset$G1E_TOT_CHOL/Cholunit
  dataset$G1E_TG <- dataset$G1E_TG/TGunit
  dataset$G1E_HDL <- dataset$G1E_HDL/Cholunit
  dataset$G1E_LDL <- dataset$G1E_LDL/Cholunit
  dataset$G1E_HGB <- dataset$G1E_HGB*Hgbunit
  dataset<- dataset %>% filter(dataset$G1E_URN_PROT %in% c(1,2,3,4))
  dataset$G1E_URN_PROT <- dataset$G1E_URN_PROT -1
  
  #eGFR 계산공식(MDRD GFR equation)
  ifelse((dataset$SEX == 2),
         dataset$eGFR <- 0.742*186.3*((dataset$G1E_CRTN)^(-1.154))*((dataset$age)^(-0.203)),
         dataset$eGFR <- 186.3*((dataset$G1E_CRTN)^(-1.154))*((dataset$age)^(-0.203)))
  
  # 공단 발표지와 unit 통일
  dataset$G1E_FBS <- dataset$G1E_FBS*FBSunit
  dataset$G1E_TOT_CHOL <- dataset$G1E_TOT_CHOL*Cholunit
  dataset$G1E_TG <- dataset$G1E_TG*TGunit
  dataset$G1E_HDL <- dataset$G1E_HDL*Cholunit
  dataset$G1E_LDL <- dataset$G1E_LDL*Cholunit
  dataset$G1E_HGB <- dataset$G1E_HGB/Hgbunit
  dataset_raw <- dataset
  dataset<-dataset_raw
  # summary(dataset)
  
  # normal range를 정해 이상치 제외 # strict하게 잡아야한다. 건강나이는 건강한 사람이 기준이되어서 하는것.
  dataset<-dataset %>% filter(age>=20) %>% filter(15<=G1E_BMI & G1E_BMI<=35) %>% 
    filter(G1E_WSTC>=50) %>% filter(G1E_BP_SYS<=170) %>% 
    filter(G1E_BP_DIA>=50 & G1E_BP_DIA<=120) %>% 
    filter(G1E_FBS<=200 & G1E_FBS>=50) %>% 
    filter(G1E_TOT_CHOL<=300 & G1E_TOT_CHOL>=40) %>% 
    filter(G1E_TG>=40 & G1E_TG<=500) %>% 
    filter(G1E_HDL<=100 & G1E_HDL>=20) %>% 
    filter(G1E_LDL <=220) %>% filter(G1E_HGB<=20&G1E_HGB>=5) %>% 
    filter(G1E_CRTN<=5&G1E_CRTN>=0.2) %>%
    filter(G1E_SGOT<=80) %>% filter(G1E_SGPT<=80) %>% filter(G1E_GGT<=300) 
  
  # age에 따라 수식 다르게 저장
  dataset20<- dataset %>% filter(20<=age & age<30)
  dataset30<- dataset %>% filter(30<=age & age<40)
  dataset40<- dataset %>% filter(40<=age & age<50)
  dataset50<- dataset %>% filter(50<=age & age<60)
  dataset60<- dataset %>% filter(60<=age & age<70)
  dataset70<- dataset %>% filter(70<=age & age<80)
  
  # dataset 저장
  write.csv(dataset, 'working/pcadata/mediage_demo_dataset_NHIS_unit.csv')
  write.csv(dataset20, 'working/pcadata/mediage_demo_dataset_NHIS_unit20.csv')
  write.csv(dataset30, 'working/pcadata/mediage_demo_dataset_NHIS_unit30.csv')
  write.csv(dataset40, 'working/pcadata/mediage_demo_dataset_NHIS_unit40.csv')
  write.csv(dataset50, 'working/pcadata/mediage_demo_dataset_NHIS_unit50.csv')
  write.csv(dataset60, 'working/pcadata/mediage_demo_dataset_NHIS_unit60.csv')
  write.csv(dataset70, 'working/pcadata/mediage_demo_dataset_NHIS_unit70.csv')
}

select_norm_data_and_cov_matrix <- function(data_specific_gender_all_components, gender, components) {
  
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

BA_add_to_dataset <-function(data_specific_gender_all_components, components, dictionary, gender, corrected){
  
  if (corrected==F){
    component2 <- components[-c(1,2)]
    data_specific_gender_temp<-data_specific_gender_all_components[component2]
    data_specific_gender_temp['Constant_column']<-1
    data_specific_gender_matrix <-as.matrix(data_specific_gender_temp)
    data_specific_gender_BA <- data_specific_gender_matrix %*% as.matrix(dictionary[,2])
    final_component <- append('RN_INDI', component2)
    data_with_BA<-data_specific_gender_all_components[final_component]
    data_with_BA['biologic_age']<-data_specific_gender_BA
  }else {
    component2 <- components[-1]
    data_specific_gender_temp<-data_specific_gender_all_components[component2]
    data_specific_gender_temp['Constant_column']<-1
    data_specific_gender_matrix <-as.matrix(data_specific_gender_temp)
    data_specific_gender_BA <- data_specific_gender_matrix %*% as.matrix(dictionary[,2])
    final_component <- append('RN_INDI', component2)
    data_with_BA<-data_specific_gender_all_components[final_component]
    data_with_BA['biologic_age']<-data_specific_gender_BA
    
  }
  return (data_with_BA)
}

PCA_model_R<-function(age_label, gender){
  #### 1. sex, age를 기준으로 데이터 로딩 
  
  file_name <- paste0('working/pcadata/mediage_demo_dataset_NHIS_unit',age_label,'.csv')
  components= c('SEX', 'age', 'G1E_HGHT', 'G1E_WGHT','G1E_BMI', 'G1E_WSTC', 'G1E_BP_SYS', 'G1E_BP_DIA', 'G1E_FBS','G1E_TOT_CHOL', 'G1E_TG', 'G1E_HDL', 'G1E_LDL', 'G1E_HGB','G1E_URN_PROT', 'G1E_CRTN', 'G1E_SGOT', 'G1E_SGPT', 'G1E_GGT', 'eGFR')
  
  data_12 = read.csv(file_name)
  df_raw_12<-data_12 
  data_specific_gender_all_components <- df_raw_12 %>% filter(df_raw_12$SEX==gender)
  data_specific_gender <- data_specific_gender_all_components[,components]
  data_specific_gender_scale<-data_specific_gender
  data_specific_gender_scale[,-1] <- scale(data_specific_gender_scale[,-1]) # normalize 필요한 변수 age~eGFR 에 대해 진행
  
  
  #### 2. select component and normalize data and get covariance matrix
  cov_norm  <- select_norm_data_and_cov_matrix(data_specific_gender_all_components=data_specific_gender_all_components, gender=gender, components=components)
  write.csv(cov_norm, paste0('/userdata06/room058/working/result_pca_r/covariance_matrix_all_',gender,'_',age_label,'.csv'))
  
  
  #### 3. get PC vs varaible_all_include_age table
  pca_table <- cov_matrix_to_PCA_table(cov_matrix=cov_norm, components=components)
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
  data_with_BA <- BA_add_to_dataset( data_specific_gender_all_components=data_specific_gender_all_components,
                                     dictionary =BA_parameters, 
                                     components=selected_components,
                                     gender=gender, corrected=F)
  data_with_BA_linear_fit<- cbind(data_with_BA, age=data_specific_gender_all_components$age) 
  model <- BA_CA_linear_fit(data_with_BA_linear_fit)
  
  #### 7. Correction
  corrected_BA_parameters <- corrected_BA(data_specific_gender=data_specific_gender, BA_parameters, model=model)
  data_with_BA <- BA_add_to_dataset(data_specific_gender_all_components=data_specific_gender_all_components,dictionary =corrected_BA_parameters, components=selected_components, gender=gender, corrected=T)
  write.csv(data_with_BA, paste0('/userdata06/room058/working/result_pca_r/data_with_BA_',gender,'_',age_label,'.csv'))
  colnames(corrected_BA_parameters)<-c('parameter', 'coef')
  write.csv(corrected_BA_parameters, paste0('/userdata06/room058/working/result_pca_r/corrected_biological_age_coef_',gender,'_',age_label,'.csv'))
  
}

disease_and_BA <- function(disease_target, strings){
  MK_data<- MK_data %>% select(RN_INDI, SICK_SYM1,SICK_SYM2, STD_YYYY)
  # 국민기록
  data_with_BA_path <- paste0('/userdata06/room058/working/result_pca_r/data_with_BA_',gender,'_',age_label,'.csv')
  BA_data<-read.csv(data_with_BA_path)

  
  
  # str_detect용 library
  # install.packages('tidyverse')
  
  
  # 상병코드 합쳐서 내원여부 확인 
  MK_data$SYMcode<-paste0(MK_data$SICK_SYM1, MK_data$SICK_SYM2)
  MK_data$SYMcode<-as.character(MK_data$SYMcode)
  MK_data$temp_disease = str_detect(MK_data$SYMcode, paste(strings, collapse = "|"))*1
  MK_data$dz<-MK_data$temp_disease

  # 연 두차례 이상 방문 확인 # time consume too much
  # MK_data$ID_SICK_YEAR <-paste0(MK_data$RN_INDI,MK_data$SICK_SYM1, MK_data$STD_YYYY) 
  # MK_data$go_to_hospital_more_than_twice <- duplicated(MK_data$ID_SICK_YEAR)*1
  # MK_data$dz <- ifelse(MK_data$go_to_hospital_more_than_twice*MK_data$temp_disease==1, 1, 0)
  
  # MK_data_save2<-MK_data
  
  MK_data_with_disease <- MK_data %>% filter(dz==1) %>%select(RN_INDI, SICK_SYM1, SICK_SYM2, dz, STD_YYYY)
  
  
  # 나이 확인
  birth_data<-read.csv('working/pcadata/nsc2_bnd_db.csv')
  birth_data<-birth_data %>% select(RN_INDI, BTH_YYYY)
  MK_data_with_disease <- left_join(birth_data, MK_data_with_disease, by='RN_INDI')
  
  
  # BA-CA 추가
  BA_data<-BA_data %>% select(RN_INDI,age,biologic_age)
  BA_data$BA_CA <- BA_data$biologic_age-BA_data$age
  
  BA_dz <-left_join(BA_data, MK_data_with_disease,  by="RN_INDI")
  BA_dz_selected<-unique(BA_dz)
  BA_dz_selected$event<-BA_dz_selected$dz
  BA_dz_selected$event <- BA_dz_selected$dz
  write.csv(as.data.frame(BA_dz_selected), 
            paste0('/userdata06/room058/working/result_pca_r/BA_CA_difference_and_disease_',disease_target, '_',gender,'.csv'))
  
  dab<- as.data.frame(BA_dz_selected)
  dab$event[is.na(dab$event)] <- 0
  
  # disease is youngest(dab_a), normal is oldest(dab_b)
  dab$invage <- dab$age *-1
  dab_a<-dab %>% filter(event==1) %>% group_by(RN_INDI) %>% top_n(1,invage)
  dab_b<-dab %>% filter(event==0) %>% group_by(RN_INDI) %>% top_n(1,age)
  dab<- rbind(dab_a, dab_b)
  dab<-dab%>%select(RN_INDI, age, event, BA_CA)
  dab_un<-unique(dab)
  return(dab_un)
}



# Settings #

# db 전처리


db_parameter<-c('RN_INDI', 'SEX', 'age','G1E_HGHT','G1E_WGHT','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_BP_DIA','G1E_FBS','G1E_TOT_CHOL','G1E_TG','G1E_HDL','G1E_LDL','G1E_HGB', 'G1E_URN_PROT','G1E_CRTN','G1E_SGOT','G1E_SGPT','G1E_GGT')
# DB_parser(folder_path, parameter=db_parameter)

## PHASE 1 
# pca model
age_label<-''
# gender<-1
# selected_components <-c('SEX','age', 'G1E_HGHT', 'G1E_WGHT', 'G1E_BP_SYS', 'G1E_FBS','G1E_HGB', 'G1E_URN_PROT','G1E_HDL','G1E_SGOT', 'eGFR')
# PCA_model_R(age_label,gender)
 gender<-2
 selected_components <- c('SEX','age','G1E_HGHT', 'G1E_WSTC', 'G1E_BP_SYS', 'G1E_FBS','G1E_TOT_CHOL', 'G1E_TG', 'G1E_HDL', 'G1E_SGOT', 'G1E_GGT', 'eGFR')
 PCA_model_R(age_label,gender)


## PHASE 2  
# 질병 데이터 load
M_data<-read.csv('working/pcadata/nsc2_m20_db.csv')
K_data<-read.csv('working/pcadata/nsc2_k20_db.csv')
MK_data <- rbind(M_data, K_data)
MK_data_save<-MK_data


# disease_target
# disease_target<- "heart"
# strings <- c('I20','I21','I22','I23','I24','I25')

# disease_target<- "brain"
# strings <- c('I6')

# disease_target<- "cancer"
# strings <- c('C')

#disease_target<- "DM"
#strings <- c('R81')

#disease_target<- "HTN"
#strings <- c('I1')

dab <-disease_and_BA(disease_target, strings)

# cox 분석 
disease_cox <- coxph(Surv(dab$age, dab$event==1)~dab$BA_CA, data=dab)
summary(disease_cox)




