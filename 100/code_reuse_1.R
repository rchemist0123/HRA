
basic <- setdiff(names(dt_mace)[3:23],c("G1E_CRTN","DRK_FREQ"))
adv <- c(basic, names(dt_mace)[24:27])

# functions ---------------------------------------------------------------

createCohort <- function(target=NULL, washout=NULL){
  require(data.table)
  require(haven)
  require(dplyr)
  message('Checking necessary data...')
  if(!exists('gj')) gj <- fread('~/data_source/user_data/nsc2_g1e_0915_db.csv')
  if(!exists('bnc')) bnc <- fread('~/data_source/user_data/nsc2_bnc_db.csv')
  if(!exists('bnd')) bnd <- fread('~/data_source/user_data/nsc2_bnd_db.csv')
  message('Raw data is ready!')
  
  message('Merging data...')
  gj[bnc, on=.(RN_INDI,EXMD_BZ_YYYY=STD_YYYY), SEX:=i.SEX]
  gj[bnd, on=.(RN_INDI),
     `:=`(BTH_Y = i.BTH_YYYY,
          DTH_YM = i.DTH_YYYYMM)]
  gj[,AGE := (as.numeric(EXMD_BZ_YYYY)-as.numeric(BTH_Y))]
  
  message('Merging completed!')
  
  message('Creating target variables...')
  
  # need checkup info --> start with all gj data
  if(target %in% c('metsyn','t2dm','htn')){
    dt <- copy(gj[AGE %between% c(30,74)])
    dt[, `:=` (
      BTH_Y = ifelse(BTH_Y=='1921LE','1921',BTH_Y),
      GJ_Y = as.numeric(substr(HME_YYYYMM,1,4)),
      GJ_M = as.numeric(substr(HME_YYYYMM,5,6))
    )]
    if (target=='t2dm'){
      t2dm_id <- fread('~/data_source/user_data/t2dm_id_final.csv')
      t2dm_date_dt <- dt[dt[G1E_FBS>=126,.I[1L],by=RN_INDI]$V1,.(RN_INDI, HME_YYYYMM)]
      dt <- dt[EXMD_BZ_YYYY %in% c('2009','2010')]      
      dt[t2dm_id, on=.(RN_INDI), t2dm_diag_dt := substr(gsub(pattern = '-',replacement = '', i.DIAG_DATE),1,6)]
      dt[t2dm_date_dt, on=.(RN_INDI), t2dm_gj_dt := i.HME_YYYYMM]
      dt[,t2dm_dt := pmin(as.numeric(t2dm_gj_dt), as.numeric(t2dm_diag_dt), na.rm=T)]
      dt[,t2dm := ifelse(!is.na(t2dm_dt),1,0) %>% as.factor()]
      t2dm_except_id <- dt[as.numeric(t2dm_dt)<= as.numeric(HME_YYYYMM), unique(RN_INDI)];message('No. of T2DM EXCEPTION: ',length(t2dm_except_id))
      dt <- dt[!RN_INDI %in% t2dm_except_id]
      dt[,`:=`(t2dm_y = as.numeric(substr(t2dm_dt,1,4)),
               t2dm_m = as.numeric(substr(t2dm_dt,5,6)))]
      dt[,t2dm_dur_month := ifelse(t2dm==0, ((2019-GJ_Y)*12 + (12-GJ_M)),
                                   ifelse(t2dm==1 & t2dm_m>=GJ_M, ((t2dm_y-GJ_Y)*12 + (t2dm_m-GJ_M)),
                                          ((t2dm_y-GJ_Y-1)*12 + (t2dm_m + 12 -GJ_M))))]
      washout_window_id <- dt[get(paste0(target,'_dur_month'))<washout, unique(RN_INDI)]; message('No. of washing out exlcusion: ',length(washout_window_id))
      dt <- dt[!RN_INDI %in% washout_window_id]
    } else if (target=='htn'){
      htn_id <- fread('~/data_source/user_data/htn_id_final.csv')
      htn_date_dt <- dt[dt[G1E_FBS>=126,.I[1L],by=RN_INDI]$V1,.(RN_INDI, HME_YYYYMM)]
      dt <- dt[EXMD_BZ_YYYY %in% c('2009','2010')]      
      dt[htn_id, on=.(RN_INDI), htn_diag_dt := substr(gsub(pattern = '-',replacement = '', i.DIAG_DATE),1,6)]
      dt[htn_date_dt, on=.(RN_INDI), htn_gj_dt := i.HME_YYYYMM]
      dt[,htn_dt := pmin(as.numeric(htn_gj_dt), as.numeric(htn_diag_dt), na.rm=T)]
      dt[,htn := ifelse(!is.na(htn_dt),1,0) %>% as.factor()]
      htn_except_id <- dt[as.numeric(htn_dt)<= as.numeric(HME_YYYYMM), unique(RN_INDI)];message('No. of HTN EXCEPTION: ',length(htn_except_id))
      dt <- dt[!RN_INDI %in% htn_except_id]
      dt[,`:=`(htn_y = as.numeric(substr(htn_dt,1,4)),
               htn_m = as.numeric(substr(htn_dt,5,6)))]
      dt[,htn_dur_month := ifelse(htn==0, ((2019-GJ_Y)*12 + (12-GJ_M)),
                                  ifelse(htn==1 & htn_m>=GJ_M, ((htn_y-GJ_Y)*12 + (htn_m-GJ_M)),
                                         ((htn_y-GJ_Y-1)*12 + (htn_m + 12 -GJ_M))))]
      washout_window_id <- dt[get(paste0(target,'_dur_month'))<washout, unique(RN_INDI)]; message('No. of washing out exlcusion: ',length(washout_window_id))
      dt <- dt[!RN_INDI %in% washout_window_id]
    } else if (target =='metsyn'){
      # 1) increased WC [≥90 cm in men or≥85 cm in women], 
      # 2) elevated TG [≥150 mg/dL (1.7 mmol/L) or medical treatment for elevated TG], 
      # 3) low HDL-C [<40 mg/dL (1 mmol/L) in men and <50 mg/dL (1.3 mmol/L) in women or medical treatment for low HDL-C], 
      # 4) elevated blood pressure [systolic blood pressure (SBP) ≥130 mmHg or diastolic blood pressure (DBP) ≥85 mmHg or current use of antihypertensives medication], and 
      # 5) impaired fasting glucose [fasting plasma glucose ≥ 100 mg/dL (5.6 mmol/L) or current use of anti- diabetic medication] from modified waist circumference (WC) criteria of the Korean Society for the Study of Obesity and the guidelines of the National Cholesterol Education Program Third Adult Treatment Panel (NCEP-ATP III)
      
      htn_drugs <- fread('~/data_source/user_data/htn_drugs.csv'); htn_drugs[,DRUG_YM := as.numeric(substr(MDCARE_STRT_DT,1,6))]
      t2dm_drugs <- fread('~/data_source/user_data/t2dm_drugs.csv'); t2dm_drugs[,DRUG_YM := as.numeric(substr(MDCARE_STRT_DT,1,6))]
      dlpd_drugs <- fread('~/data_source/user_data/dlpd_drugs.csv'); dlpd_drugs[,DRUG_YM := as.numeric(substr(MDCARE_STRT_DT,1,6))]
      
      dt[htn_drugs, on=.(RN_INDI,  HME_YYYYMM=DRUG_YM), htn_drug_ym := i.DRUG_YM]
      dt[t2dm_drugs, on=.(RN_INDI, HME_YYYYMM=DRUG_YM), t2dm_drug_ym := i.DRUG_YM]
      dt[dlpd_drugs, on=.(RN_INDI, HME_YYYYMM=DRUG_YM), dlpd_drug_ym := i.DRUG_YM]
      
      dt[, `:=` (wc_criteria = ifelse((SEX==1 & G1E_WSTC>=95) | (SEX==2 & G1E_WSTC)>=85,1,0),
                 tg_criteria = ifelse(G1E_TG>=150,1,0),
                 hdl_criteria = ifelse((SEX==1 & G1E_HDL<40 | SEX==2 & G1E_LDL<50 ) | !is.na(dlpd_drug_ym),1,0),
                 bp_criteria = ifelse((G1E_BP_SYS>=130 | G1E_BP_DIA>=85) | !is.na(htn_drug_ym),1,0),
                 ifg_criteria = ifelse(G1E_FBS>=100 | !is.na(t2dm_drug_ym),1,0))]
      
      dt[,metsyn:=ifelse(rowSums(.SD,na.rm=T)>=3,1,0),.SDcols=grep('criteria',names(dt))]
      metsyn_date_dt <- dt[dt[metsyn==1,.I[1L],by=RN_INDI]$V1,.(RN_INDI,HME_YYYYMM)][order(RN_INDI)]
      
      dt <- dt[EXMD_BZ_YYYY %in% c('2009','2010')]  
      dt[metsyn_date_dt, on=.(RN_INDI), metsyn_dt := i.HME_YYYYMM]
      dt[,metsyn := ifelse(is.na(metsyn_dt),0,1) %>% as.factor]
      metsyn_except_id <- dt[metsyn_dt<=HME_YYYYMM, unique(RN_INDI)];message('No. of Metabolic Syndrome EXCEPTION: ', length(metsyn_except_id))
      dt <- dt[!RN_INDI %in% metsyn_except_id]
      dt[,`:=`(metsyn_y = as.numeric(substr(metsyn_dt,1,4)),
               metsyn_m = as.numeric(substr(metsyn_dt,5,6)))]
      dt[,metsyn_dur_month := ifelse(metsyn==0, ((2019-GJ_Y)*12 + (12-GJ_M)),
                                     ifelse(metsyn==1 & metsyn_m>=GJ_M, ((metsyn_y-GJ_Y)*12 + (metsyn_m-GJ_M)),
                                            ((metsyn_y-GJ_Y-1)*12 + (metsyn_m + 12 -GJ_M))))]
      washout_window_id <- dt[get(paste0(target,'_dur_month'))<washout, unique(RN_INDI)]; message('No. of washing out exlcusion: ',length(washout_window_id))
      dt <- dt[!RN_INDI %in% washout_window_id]
    }
  } else {
    ## cancer mace death ------------------------------
    dt <- copy(gj[EXMD_BZ_YYYY %in% c('2009','2010') & AGE %between% c(30,74)])
    dt[, `:=` (
      BTH_Y = ifelse(BTH_Y=='1921LE','1921',BTH_Y),
      GJ_Y = as.numeric(substr(HME_YYYYMM,1,4)),
      GJ_M = as.numeric(substr(HME_YYYYMM,5,6))
    )]
    if(target=='mace'){
      mace_id <- fread('~/data_source/user_data/mace_id.csv')
      dt[bnd[substr(COD1,1,3) %in% c('I21','I22','I23','I63','G45','I62','I65','I67','I68','I20','I24','I25','I26','I27','I28','I30','I31','I32','I33','I34','I35','I36','I37','I38','I39','I40','I41','I42','I43','I44','I45','I46','I47','I48','I49','I51','I52','I64') |
               substr(COD2,1,3) %in% c('I21','I22','I23','I63','G45','I62','I65','I67','I68','I20','I24','I25','I26','I27','I28','I30','I31','I32','I33','I34','I35','I36','I37','I38','I39','I40','I41','I42','I43','I44','I45','I46','I47','I48','I49','I51','I52','I64')], on=.(RN_INDI),
         MACE_DEATH_YM := i.DTH_YYYYMM]
      dt[mace_id, on=.(RN_INDI), mace_dt := i.MDCARE_STRT_DT]
      mace_except_id <- dt[as.numeric(substr(mace_dt,1,6))<=as.numeric(HME_YYYYMM), unique(RN_INDI)] ; message('No. of MACE EXCPETION: ', length(mace_except_id))
      dt <- dt[!RN_INDI %in% mace_except_id]
      dt[,mace:= ifelse(!is.na(mace_dt),1,0) %>% as.factor]
      dt[,mace_death:= ifelse(mace==1 & as.numeric(MACE_DEATH_YM) - as.numeric(substr(mace_dt,1,6))<=1,1,0)]
      dt[,`:=`(mace_y = as.numeric(substr(mace_dt,1,4)),
               mace_m = as.numeric(substr(mace_dt,5,6)))]
      dt[,mace_dur_month := ifelse(mace==0, ((2019-GJ_Y)*12 + (12-GJ_M)),
                                   ifelse(mace==1 & mace_m>=GJ_M, ((mace_y-GJ_Y)*12 + (mace_m-GJ_M)),
                                          ((mace_y-GJ_Y-1)*12 + (mace_m + 12 -GJ_M))))]
    } else if (target=='cancer'){
      cancer <- fread('~/data_source/user_data/cancer_male.csv')
      cancer <- cancer[cancer[order(MDCARE_STRT_DT,RN_INDI),.I[1L],by=RN_INDI]$V1,.(RN_INDI, MDCARE_STRT_DT)]
      dt[cancer, on=.(RN_INDI), cancer_dt := i.MDCARE_STRT_DT]
      cancer_except_id <- dt[as.numeric(substr(cancer_dt,1,6)) <= HME_YYYYMM, unique(RN_INDI)] ; message('No. of CANCER EXCPETION: ', length(cancer_except_id))
      dt <- dt[!RN_INDI %in% cancer_except_id]
      dt[,cancer:= ifelse(!is.na(cancer_dt),1,0) %>% as.factor()]
      dt[,`:=`(cancer_y = as.numeric(substr(cancer_dt,1,4)),
               cancer_m = as.numeric(substr(cancer_dt,5,6)))]
      dt[,cancer_dur_month := ifelse(cancer==0, ((2019-GJ_Y)*12 + (12-GJ_M)),
                                     ifelse(cancer==1 & cancer_m>=GJ_M, ((cancer_y-GJ_Y)*12 + (cancer_m-GJ_M)),
                                            ((cancer_y-GJ_Y-1)*12 + (cancer_m + 12 -GJ_M))))]
    } else if (target=='death'){
      setnames(dt,'DTH_YM','death_dt')
      dt[,`:=`(
        death_y = as.numeric(substr(death_dt,1,4)),
        death_m = as.numeric(substr(death_dt,5,6))
      )]
      dt[,death := ifelse(is.na(death_dt),0,1) %>% as.factor()]
      dt[,death_dur_month := ifelse(death==0, ((2019-GJ_Y)*12 + (12-GJ_M)),
                                    ifelse(death==1 & death_m>=GJ_M, ((death_y-GJ_Y)*12 + (death_m-GJ_M)),
                                           ((death_y-GJ_Y-1)*12 + (death_m + 12 -GJ_M))))]
    } else if (target=='illness'){
      message('Importing major illness data...')
      mi <- fread('~/data_source/user_data/mi.csv')
      peri <- fread('~/data_source/user_data/peri_vascular.csv')
      hf <- fread('~/data_source/user_data/heart_failure.csv')
      angina <- fread('~/data_source/user_data/angina.csv')
      stroke <- fread('~/data_source/user_data/stroke.csv')
      renal <- fread('~/data_source/user_data/renal.csv')
      liver <- fread('~/data_source/user_data/liver.csv')
      dm <- fread('~/data_source/user_data/complicated_dm.csv')
      lymphonia <- fread('~/data_source/user_data/cancer_lymphonia.csv')
      meta_cancer <- fread('~/data_source/user_data/cancer_meta.csv')
      nonmeta_cancer <- fread('~/data_source/user_data/cancer_nonmeta.csv')
      plegia <- fread('~/data_source/user_data/plegia.csv')
      disability <- fread('~/data_source/user_data/diability.csv')
      
      mls <- c('mi','peri','hf','angina','stroke','renal','liver','dm','lymphonia',
               'meta_cancer','nonmeta_cancer','plegia','disability')
      for (d in mls){
        temp <- get(d)
        date <- paste0(d,'_dt')
        if(d=='disability'){
          dt[temp[temp[,.I[1L],by=RN_INDI]$V1], on=.(RN_INDI), eval(date):=i.STD_YYYY]
        } else {
          dt[temp[temp[,.I[1L],by=RN_INDI]$V1], on=.(RN_INDI), eval(date):=i.MDCARE_STRT_DT]
        }
      }
      dt[,illness_dt := do.call(pmin, c(.SD,na.rm=T)),.SDcols=mi_dt:plegia_dt]
      except_id <- dt[as.numeric(substr(illness_dt,1,6))<=as.numeric(HME_YYYYMM)|
                        as.numeric(disability_dt)<=as.numeric(EXMD_BZ_YYYY),unique(RN_INDI)]
      message('No. of Major illness people: ', length(except_id))
      dt <- dt[!RN_INDI %in% except_id]
      dt[,illness := ifelse(is.na(illness_dt),0,1) %>% as.factor()]
      dt[,`:=`(
        illness_y = as.numeric(substr(illness_dt,1,4)),
        illness_m = as.numeric(substr(illness_dt,5,6))
      )]
      dt[,illness_dur_month := ifelse(illness==0, ((2019-GJ_Y)*12 + (12-GJ_M)),
                                      ifelse(illness==1 & illness_m>=GJ_M, ((illness_y-GJ_Y)*12 + (illness_m-GJ_M)),
                                             ((illness_y-GJ_Y-1)*12 + (illness_m + 12 -GJ_M))))]
    } 
  }
  
  # outliers to NA
  # clinical range (1 to 99 percentile)
  message('Checking outliers...')
  dt[,`:=`(
    G1E_BMI = ifelse(!G1E_BMI %between% c(10,40),NA, G1E_BMI),
    G1E_BP_SYS = ifelse(!G1E_BP_SYS %between% c(50,300),NA,G1E_BP_SYS),
    G1E_BP_DIA = ifelse(!G1E_BP_DIA %between% c(40,200),NA,G1E_BP_DIA),
    G1E_WSTC = ifelse(!G1E_WSTC %between% c(60,150),NA,G1E_WSTC),
    G1E_FBS = ifelse(!G1E_FBS %between% c(50,400),NA, G1E_FBS),
    G1E_TOT_CHOL = ifelse(!G1E_TOT_CHOL %between% c(50,500),NA, G1E_TOT_CHOL),
    G1E_TG = ifelse(!G1E_TG  %between% c(30,500),NA,G1E_TG),
    G1E_HDL = ifelse(!G1E_HDL %between% c(20,250),NA, G1E_HDL),
    G1E_LDL = ifelse(!G1E_LDL %between% c(0,250),NA, G1E_LDL),
    G1E_HGB = ifelse(!G1E_HGB %between% c(8,20),NA, G1E_HGB),
    G1E_CRTN = ifelse(!G1E_CRTN %between% c(0.3,2.5),NA, G1E_CRTN),
    G1E_SGOT = ifelse(!G1E_SGOT %between% c(0,200),NA, G1E_SGOT),
    G1E_SGPT = ifelse(!G1E_SGPT %between% c(0,200),NA, G1E_SGPT),
    G1E_GGT = ifelse(!G1E_GGT %between% c(0,250),NA, G1E_GGT))]
  
  dt[,`:=`(
    SMK_COMBINED = ifelse(is.na(Q_SMK_YN),NA,Q_SMK_YN-1),
    DRK_CON = ifelse(Q_DRK_FRQ_V09N==0,0,Q_DRK_AMT_V09N),
    DRK_FREQ = Q_DRK_FRQ_V09N,
    DRK_COMBINED = ifelse(Q_DRK_FRQ_V09N==0,0,
                          ifelse((SEX==1 & Q_DRK_FRQ_V09N>=4 & Q_DRK_AMT_V09N >=7) | 
                                   (SEX==2 & Q_DRK_FRQ_V09N>=4 & Q_DRK_AMT_V09N>=5),2,1)),
    METs= Q_PA_WALK*30*2.9 + Q_PA_MD*30*4 + Q_PA_VD*20*7,
    eGFR = ifelse(SEX==1, 186.3*(G1E_CRTN^-1.154)*AGE^-0.203,
                  186.3*(G1E_CRTN^-1.154)*AGE^-0.203*0.742)
  )][,`:=`(
    eGFR = ifelse(!eGFR %between% c(0,250),NA,eGFR),
    METs = ifelse(METs>2500, NA, METs)
  )]
  if(!is.null(washout)){
    washout_window_id <- dt[get(paste0(target,'_dur_month'))<washout, unique(RN_INDI)]; message('No. of washing out exlcusion: ',length(washout_window_id))
    dt <- dt[!RN_INDI %in% washout_window_id]
  }
  message(toupper(target),' Cohort Completed!!')
  vars <- c('RN_INDI','HME_YYYYMM',  'AGE','SEX', 'G1E_TOT_CHOL','G1E_HDL','G1E_LDL',
            'G1E_TG', 'G1E_FBS','G1E_HGB','G1E_SGOT','G1E_SGPT','G1E_GGT','G1E_CRTN',
            'G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_BP_DIA','eGFR','SMK_COMBINED','DRK_CON','DRK_FREQ', 'METs',
            'Q_FHX_STK','Q_FHX_HTN','Q_FHX_HTDZ','Q_FHX_DM',target, paste0(target,'_dt'),paste0(target,'_dur_month'))
  if(target=='mace') vars <- c(vars,c('mace_death','MACE_DEATH_YM'))
  return(dt[dt[!is.na(get(target)) & EXMD_BZ_YYYY %in% c(2009,2010),.I[.N],by=RN_INDI]$V1,..vars])
}

require(R6)
HSC <- R6Class(
  classname = 'HSCalculator',
  public = list(
    target = NULL,
    features = NULL,
    baseDir  = NULL,
    inputDir = NULL,
    seed= 2022,
    # ages = seq(30,70,10), 10세 단위 
    ages = seq(30,70,5),
    initialize = function(target, features, baseDir, inputDir){
      self$target <- target
      self$features <- features
      self$baseDir <- baseDir
      self$inputDir <- inputDir # sample files directory
    },
    lr_platt_recal_func = function(fit, valid_data, test_res, y){
      val_estimates_norm <- predict(fit, valid_data, type='prob')[,2]
      train_re_mtx <- data.table(y=valid_data[[y]], 
                                 yhat=val_estimates_norm)
      
      # create calibration model
      calib.model <- glm(y~yhat, data=train_re_mtx, family=binomial)
      ygrid_norm = data.table(yhat=test_res)
      # colnames(test_res) <- c("yhat")
      
      # recalibrate and measure on test set
      ygrid_cal = predict(calib.model, ygrid_norm, type='response')
      return(ygrid_cal)
    },
    lr_iso_recal_func = function(fit, valid_data, test_res, y){
      # predict validation datasets's estimates with logistic regression
      val_estimates_norm = predict(fit, valid_data,  type='prob')[,2]
      train_re_mtx = data.table(y=as.numeric(as.character(valid_data[[y]])), 
                                yhat=val_estimates_norm)
      # iso_train_mtx = train_re_mtx[order(train_re_mtx[,2])]
      setorder(train_re_mtx, yhat)
      # create calibration model
      calib.model <<- isoreg(train_re_mtx[,yhat], train_re_mtx[,y])
      stepf_data <- cbind(calib.model$x, calib.model$yf)
      step_func <- stepfun(stepf_data[,1], c(0,stepf_data[,2]))
      
      # recalibrate and measure on test set
      exp2_iso_recal <- step_func(test_res)
      return(exp2_iso_recal)
    },
    outlier = function(x){
      y <- ifelse(x<quantile(x,na.rm=T, probs=.25)- 1.5*IQR(x, na.rm=T) |
                    x>quantile(x, na.rm=T, probs=.75) + 1.5*IQR(x, na.rm=T),NA,x)
      return(y)
    },
    preprocessing = function(data,na){
      require(caret)
      set.seed(2022)
      dat <- copy(data)
      message('Spliting data...')
      print(table(dat[[self$target]]))
      dat[,(self$target):= lapply(.SD, function(x) ifelse(as.character(x)=='0','no','yes')),.SDcols=self$target]
      id <- createDataPartition(data[[self$target]],p=0.7)
      train <- dat[id$Resample1,]
      test <- dat[-id$Resample1,]
      
      cat_features <- c('SMK_COMBINED','Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_HTN','Q_FHX_DM')
      numeric_features <- setdiff(self$features,cat_features)
      
      message('Imputating missing values...')
      
      train[,(self$features):=lapply(.SD, function(x) ifelse(is.na(x),median(x, na.rm=T),x)),.SDcols=self$features]
      test[,(self$features):=lapply(.SD, function(x) ifelse(is.na(x),median(x, na.rm=T),x)),.SDcols=self$features]
      print(summary(train))
      train[,(cat_features):=lapply(.SD, as.factor),.SDcols=cat_features]
      test[,(cat_features):=lapply(.SD, as.factor),.SDcols=cat_features]
      vars <- c(self$target,paste0(self$target,'_dur_month'), self$features)
      return(list(train=train[,..vars], test=test[,..vars]))
    },
    saveData = function(type){
      dir <- paste0(c(self$baseDir, type),collapse='/')
      if(!dir.exists(dir)) dir.create(dir)
      return(dir)
    },
    train_model = function(train, strata=NULL, preprocess=NULL,polynomial=F){
      require(caret)
      trctrl <- trainControl(method='cv',number = 5, 
                             classProbs=T, savePredictions=T)
      if(!is.null(strata)){
        fit_list <- list()
        if(isTRUE(polynomial)){
          features_new <- c(setdiff(self$features,strata),'I(METs^2)','I(DRK_CON^2)')
        } else {
          features_new <- c(setdiff(self$features,strata),'I(METs^2)')
        }
        # 'I(G1E_TOT_CHOL^2)','I(G1E_HDL^2)','I(G1E_LDL^2)')
        form <- paste(self$target,'~',paste0(features_new, collapse = '+'))
        for(i in c(1,2)){
          sex <- ifelse(i==1,'male','female')
          message('Modeling of ',sex,'...')
          fit <- train(as.formula(form),
                       data=train[get(strata)==i],
                       method = 'glm',
                       metric = 'Accuracy',
                       trControl = trctrl,
                       # family='binomial',
                       preProcess = preprocess)
          # save coef + p
          message('Saving coefficients of ', sex, '...')
          coefDir <- '~/working/Jang/coefficient'
          coef_dt<-as.data.table(summary(fit$finalModel)$coefficients,keep.rownames = T)[,c(1,2,5)]
          names(coef_dt) <- c('vars','coef','p')
          coef_dt[,p := ifelse(p<0.001,'<0.001', format(round(p,4),4))]
          filename <- paste0(paste0(c(self$target,'coef',sex,preprocess),collapse='_'),'.csv')
          fwrite(coef_dt, paste0(c(coefDir, filename),collapse='/'))
          fit_list[[sex]] <- fit
        }
        return(fit_list)
      } else {
        form <- paste(self$target,'~',paste0(features, collapse = '+'))
        message('Modeling...')
        fit <- train(as.formula(form),
                     data=train,
                     method = 'glm',
                     metric = 'Accuracy',
                     trControl = trctrl,
                     preProcess = 'range')
        message('Saving coefficients of all data...')
        coefDir <- saveData('coefficient')
        coef_dt<-as.data.table(summary(fit$finalModel)$coefficients,keep.rownames = T)[,c(1,2,5)]
        names(coef_dt) <- c('vars','coef','p')
        features <- ifelse("Q_FHX_STK" %in% self$features, "advance","basic")
        coef_dt[,p := ifelse(p<0.001,'<0.001', format(round(p,4),4))]
        filename <- paste0(paste0(c(self$target, features, 'coef','all'),collapse='_'),'.csv')
        fwrite(coef_dt, paste0(c(coefDir, filename),collape='/'))
        return(fit)
      }
    },
    perfTest = function(fit, test,cutoff=0.5){
      require(pROC)
      require(ROCR)
      test[,`:=`(pred=predict(fit, test, type='prob')[,2])][,pred_bin := ifelse(pred>=cutoff,'yes','no')]
      test_dt <-test[,.(pred,AGE, DRK_CON)]
      print(table(test$pred_bin))
      cm <- confusionMatrix(test$pred_bin, test[[self$target]],positive='yes',mode='everything');
      prediction <- ROCR::prediction(test[,pred], test[[self$target]])
      auc <- performance(prediction, 'auc')@y.values
      prc <- MLmetrics::PRAUC(test$pred, test[[self$target]])
      result <- list(auc=auc, prc=prc, 
                     preval = cm$byClass[8],
                     accuracy = cm$overall[1],
                     ppv = cm$byClass[3],npv=cm$byClass[4],
                     sens = cm$byClass[1], spec = cm$byClass[2])
      return(result)
    },
    cutoffThreshold = function(fit){
      probs <- seq(0.1,0.5, by=0.1)
      ths <- thresholder(fit, threshold = probs, final=T, statistics='all')
      print(ths)
    },
    generateSample = function(data, by, n){
      for(i in c(1,2)){
        for(j in self$ages){
          message('Sampling ', n,  ' people of ',i,' ',j)
          set.seed(self$seed)
          setkeyv(data, 'AGE')
          if(by==10) dt2 <- data[SEX==i & AGE %/% 10 == (j/10)]
          else dt2 <- data[SEX==i & AGE %between% c(j, j+4)]
          id <- sample(nrow(dt2),n)
          temp <- dt2[id,setdiff(names(dt2),c('pred')),with=F]
          temp[,pid:=1:n]
          # inputDir <- self$saveData(type = 'sample')
          inputDir <- "~/working/Jang/sample"
          filename <- paste0(paste(c(i,j,self$target,'sample'),collapse='_'),'.csv')
          fwrite(temp, paste0(c(inputDir,filename),collapse='/'))
        }
      }
    },
    generateAllSample = function(data,n){
      require(data.table)
      for(i in c(1,2)){
        set.seed(self$seed)
        dt2 <- data[SEX==i]
        id <- sample(nrow(dt2),n) 
        temp <- dt2[id, setdiff(names(dt2),c('pred')),with=F]
        temp[,pid := 1:n]
        message('Sampling ',n, ' people of ', i)
        inputDir <- self$saveData(type = 'sample')
        filename <- paste0(paste(c(i,'all',self$target,n,'sample'),collapse='_'),'.csv')
        fwrite(temp, paste0(c(inputDir,filename),collapse='/'))
      }      
    },
    getHealthScore = function(fit, test, type=NULL, target=NULL){
      fileDir <- paste(c(self$baseDir, self$inputDir),collapse='/'); print(fileDir)
      fileList <- list.files(fileDir); print(fileList)
      if(!is.null(type)) {
        fileList <- fileList[fileList %like% self$target & fileList %like% 'all'] ;
        print(fileList)
      } else {
        fileList <- fileList[fileList %like% self$target & !fileList %like% 'all'];
        print(fileList)
      }
      for(i in fileList){
        temp <- fread(paste0(c(fileDir,i),collapse='/'))
        sex_chr <- ifelse(substr(i,1,1)==1,'male','female')
        if(!is.null(type)) age <- type
        else age <- substr(i,3,4)
        length <- nrow(temp)
        
        # numeric to factor
        factorVars <- c('SMK_COMBINED','Q_FHX_STK','Q_FHX_HTN','Q_FHX_HTDZ','Q_FHX_DM')
        temp[,(factorVars):=lapply(.SD, as.factor),.SDcols=factorVars]
        
        #health score & rank
        message('Calculating health scores','(',self$target,') for ', sex_chr,' ', age,'...')
        temp[,pred:=predict(fit[[sex_chr]], temp, type='prob')[,2]]
        # self$perfTest(fit[[sex_chr]], temp)
        temp[,score_raw := (1-pred)*1000]
        
        setorder(temp,-score_raw)
        
        # ranking table
        # rankDir <- paste0(c(self$baseDir,'rank'),collapse = '/')
        # if(!dir.exists(rankDir)) dir.create(rankDir)
        # filename <- paste0(paste(c(sex_chr,age,'rank_table'),collapse='_'),'.csv')
        # fwrite(temp[,.(health_score_raw, health_score_rank)],paste0(c(rankDir, filename),collapse = '/'))
        
        # calibration
        # temp[,health_score_platt := (1-self$lr_platt_recal_func(fit[[sex_chr]],test[SEX==sex_num],
        #                                                         pred, self$target))*1000]
        # temp[,health_score_iso := (1-self$lr_iso_recal_func(fit[[sex_chr]],test[SEX==sex_num],
        #                                                     pred, self$target))*1000]
        # output dir
        targetDir <- self$saveData(type='output')
        if(!is.null(target)){
          filename <- paste0(paste(c(target,sex_chr,age,length,'output'),collapse='_'),'.csv')
        } else {
          filename <- paste0(paste(c(self$target,sex_chr,age,length,'output'),collapse='_'),'.csv')
        }
        fwrite(temp[,.(pid, SEX, AGE, pred,score_raw)], paste0(c(targetDir,filename),collapse='/'))
      }
      message('Done!')
    },
    mergeSampleScore = function(target){
      mylist <- list()
      outDir <- "~/working/Jang/sample_score"
      for(i in 1:2){
        sex_num <- i
        sex_chr <- ifelse(i==1,'male','female')
        for(j in seq(30,70,5)){
          sample_name <- paste0('sample/',sex_num,'_',j,'_',target,'_','sample.csv')
          score_name <- paste0('output/',target ,'_',sex_chr,'_',j,'_','1000_output.csv')
          sample <- fread(sample_name);
          score <- fread(score_name);
          merged <- merge(sample, score[,.(pid, pred,score_raw)], by='pid')
          merged[,age_group := paste0(j,'_',j+4) %>% as.character()]
          setkeyv(merged,'score_raw')
          setorder(merged,-score_raw)
          setnames(merged,'pid','region')
          key <- paste0(sex_chr,j,collapse = '_')
          mylist[[key]] <- merged[,c(1,4:31)]
        }
      }
      final <- rbindlist(mylist)
      names(final) <- paste0('Score',1:29)
      fwrite(final,paste0(outDir,'/',target,'_sample_score.csv'))
    }
  )
)
require(rcompanion)

preprocessing <- function(data,target,features,na){
  require(caret)
  set.seed(2022)
  
  message('Spliting data...')
  id <- createDataPartition(data[[target]],p=0.7)
  train <- data[id$Resample1,]
  test <- data[-id$Resample1,]
  
  cat_features <- c('SMK_COMBINED','DRK_COMBINED','Q_FHX_STK','Q_FHX_HTDZ','Q_FHX_HTN','Q_FHX_DM')
  numeric_features <- setdiff(features,cat_features)
  message('Handling outlier...')
  
  train[,(numeric_features):=lapply(.SD,outlier),.SDcols=numeric_features]
  test[,(numeric_features):=lapply(.SD,outlier),.SDcols=numeric_features]
  
  if(na=='omit'){
    train <- na.omit(train)
    test <- na.omit(test)
  } else if (na=='impute'){
    train[,(features):=lapply(.SD, function(x) ifelse(is.na(x),median(x, na.rm=T),x)),.SDcols=features]
    test[,(features):=lapply(.SD, function(x) ifelse(is.na(x),median(x, na.rm=T),x)),.SDcols=features]
    train[,(cat_features):=lapply(.SD, as.factor),.SDcols=cat_features]
    test[,(cat_features):=lapply(.SD, as.factor),.SDcols=cat_features]
  }
  vars <- c(target,'DTH_DUR_M', features)
  return(list(train=train[,..vars], test=test[,..vars]))
}
train_model <- function(train, target, features, strata=NULL){
  require(caret)
  trctrl <- trainControl(method='cv',number = 5)
  if(!is.null(strata)){
    features_new <- c(setdiff(features,strata),'I(METs^2)')
    form <- paste(target,'~',paste0(features_new, collapse = '+'))
    fit1 <- train(as.formula(form),
                  data=train[get(strata)==1],
                  method = 'glm',
                  metric = 'Accuracy',
                  trControl = trctrl,
                  # family='binomial',
                  preProcess = 'range')
    fit2 <- train(as.formula(form),
                  data=train[get(strata)==2],
                  method = 'glm',
                  metric = 'Accuracy',
                  trControl = trctrl,
                  # family='binomial',
                  preProcess = 'range')
    return(list(male=fit1, female=fit2))
  } else {
    form <- paste(target,'~',paste0(features, collapse = '+'))
    fit <- train(as.formula(form),
                 data=train,
                 method = 'glm',
                 metric = 'Accuracy',
                 trControl = trctrl,
                 # family='binomial',
                 preProcess = 'range')
  }
  return(fit)
}

lr_platt_recal_func <- function(fit, valid_data, test_res, y){
  val_estimates_norm <- predict(fit, valid_data, type='prob')[,2]
  train_re_mtx <- data.table(y=valid_data[[y]], 
                             yhat=val_estimates_norm)
  
  # create calibration model
  calib.model <- glm(y~yhat, data=train_re_mtx, family=binomial)
  ygrid_norm = data.table(yhat=test_res)
  # colnames(test_res) <- c("yhat")
  
  # recalibrate and measure on test set
  ygrid_cal = predict(calib.model, ygrid_norm, type='response')
  return(ygrid_cal)
}

lr_iso_recal_func <-  function(model_fit, validate_data, test_res, y){
  # predict validation datasets's estimates with logistic regression
  val_estimates_norm = predict(model_fit, validate_data,  type='prob')[,2]
  train_re_mtx = data.table(y=as.numeric(as.character(validate_data[[y]])), 
                            yhat=val_estimates_norm)
  # iso_train_mtx = train_re_mtx[order(train_re_mtx[,2])]
  setorder(train_re_mtx, yhat)
  # create calibration model
  calib.model <<- isoreg(train_re_mtx[,yhat], train_re_mtx[,y])
  stepf_data <- cbind(calib.model$x, calib.model$yf)
  step_func <- stepfun(stepf_data[,1], c(0,stepf_data[,2]))
  
  # recalibrate and measure on test set
  exp2_iso_recal <- step_func(test_res)
  return(exp2_iso_recal)
}





# cancer ------------------------------------------------------------------
dt_cancer <- createCohort(target='cancer', washout=6)

hsc_cancer <- HSC$new(target='cancer', baseDir='~/working/Jang', inputDir='sample')
hsc_cancer_basic <- HSC$new(target='cancer',
                            features = basic,
                            baseDir='~/working/Jang', inputDir='sample')
tt_cancer <- hsc_cancer$preprocessing(dt_cancer,features = adv)
tt_cancer_basic <- hsc_cancer_basic$preprocessing(dt_cancer)

cancer_fit_basic <- hsc_cancer_basic$train_model(train = tt_cancer_basic$train, 
                                                 strata = 'SEX', polynomial = T)
for(i in seq(0.1,0.5,0.1)){
  cancer_male_perf <- hsc_cancer$perfTest(fit = cancer_fit$male, test = tt_cancer$test[SEX==1], cutoff=i)
  cancer_female_perf <- hsc_cancer$perfTest(fit = cancer_fit$female, test = tt_cancer$test[SEX==2],cutoff=i)
  message('cutoff: ',i)
  print(rbindlist(list(cancer_male_perf, cancer_female_perf),idcol = T))
}


cancer_male_perf <- hsc_cancer$perfTest(fit = cancer_fit$male, test = tt_cancer$test[SEX==1], cutoff=0.2)
cancer_female_perf <- hsc_cancer$perfTest(fit = cancer_fit$female, test = tt_cancer$test[SEX==2],cutoff=0.2)
rbindlist(list(male = cancer_male_perf, female = cancer_female_perf), idcol = T)

hsc_cancer$generateSample(data = rbindlist(tt_cancer),
                          by = 5,
                          n = 1000)
hsc_cancer$getHealthScore(fit = cancer_fit, test=tt_cancer$test)
hsc_cancer$mergeSampleScore(target = 'cancer')

fread('sample_score/cancer_sample_score.csv')


# mace --------------------------------------------------------------------
dt_mace <- createCohort(target='mace', washout= 6)
dt_mace <- createCohort(target='mace', washout= 6)
hsc_mace <- HSC$new(target='mace', features = adv, baseDir='~/working/Jang', inputDir='sample')
hsc_mace_basic <- HSC$new(target='mace', features = basic, baseDir='~/working/Jang', inputDir='sample')

tt_mace <- hsc_mace$preprocessing(dt_mace,features = adv)
tt_mace_basic <- hsc_mace_basic$preprocessing(dt_mace)

mace_fit_basic <- hsc_mace_basic$train_model(train = tt_mace_basic$train, 
                                             strata = 'SEX', polynomial = T)

for(i in seq(0.1,0.5,0.1)){
  mace_male_perf <- hsc_mace$perfTest(fit = mace_fit$male, test = tt_mace$test[SEX==1], cutoff=i)
  mace_female_perf <- hsc_mace$perfTest(fit = mace_fit$female, test = tt_mace$test[SEX==2],cutoff=i)
  message('cutoff: ',i)
  print(rbindlist(list(mace_male_perf, mace_female_perf),idcol = T))
}


mace_male_perf <- hsc_mace$perfTest(fit = mace_fit$male, test = tt_mace$test[SEX==1], cutoff=0.2)
mace_female_perf <- hsc_mace$perfTest(fit = mace_fit$female, test = tt_mace$test[SEX==2],cutoff=0.2)
rbindlist(list(male = mace_male_perf, female = mace_female_perf), idcol = T)

hsc_mace$generateSample(data = rbindlist(tt_mace),
                        by = 5,
                        n = 1000)
hsc_mace$getHealthScore(fit = mace_fit, test=tt_mace$test)
hsc_mace$mergeSampleScore(target = 'mace')

fread('sample_score/mace_sample_score.csv') %>% names()


final <- rbindlist(mylist)


setwd('~/working/mortality/output/')
temp_female <- lapply(list.files('~/working/mortality/output/', pattern = 'mace_female_'),fread);
temp_male <- lapply(list.files('~/working/mortality/output/', pattern = 'mace_male'),fread)
temp_female[1:5]
temp_merge <- rbind(rbindlist(temp_male[1:5]),rbindlist(temp_female[1:5]))
temp_merge
temp_merge[,sex:=rep(c(1,2),each=10000)]
temp_merge[,age:=rep(rep(seq(30,70,10),each=2000),2)]
fwrite(temp_merge,'~/data_out/data_out/mace_score_2000.csv')


# 1000
hsc_mace$generateAllSample(data=rbindlist(tt_mace),n = 1000)
hsc_mace$getHealthScore(fit = mace_fit, test=tt_mace$test, type='all')

names(dt_mace)


# illness -----------------------------------------------------------------
dt_illness <- createCohort(target='illness', washout= 12)

hsc_illness <- HSC$new(target='illness', 
                       features=adv,
                       baseDir='~/working/Jang', inputDir='sample')
hsc_illness_basic <- HSC$new(target='illness', 
                             features=basic,
                             baseDir='~/working/Jang', inputDir='sample')

tt_illness <- hsc_illness$preprocessing(dt_illness,features = adv)
tt_illness_basic <- hsc_illness_basic$preprocessing(dt_illness)

illness_fit <- hsc_illness_basic$train_model(train = tt_illness_basic$train, 
                                             strata = 'SEX', polynomial = T)
for(i in seq(0.1,0.5,0.1)){
  illness_male_perf <- hsc_illness$perfTest(fit = illness_fit$male, test = tt_illness$test[SEX==1], cutoff=i)
  illness_female_perf <- hsc_illness$perfTest(fit = illness_fit$female, test = tt_illness$test[SEX==2],cutoff=i)
  message('cutoff: ',i)
  print(rbindlist(list(illness_male_perf, illness_female_perf),idcol = T))
}


illness_male_perf <- hsc_illness$perfTest(fit = illness_fit$male, test = tt_illness$test[SEX==1], cutoff=0.2)
illness_female_perf <- hsc_illness$perfTest(fit = illness_fit$female, test = tt_illness$test[SEX==2],cutoff=0.2)
rbindlist(list(male = illness_male_perf, female = illness_female_perf), idcol = T)

hsc_illness$generateSample(data = rbindlist(tt_illness),
                           by = 5,
                           n = 1000)
hsc_illness$getHealthScore(fit = illness_fit, test=tt_illness$test)
hsc_illness$mergeSampleScore(target = 'illness')




# t2dm --------------------------------------------------------------------
dt_t2dm <- createCohort(target='t2dm',washout=12) # cohort configuration
hsc_t2dm_basic <- HSC$new(target='t2dm', 
                          features=basic,
                          baseDir='~/working/Jang', inputDir='sample')
tt_t2dm_basic <- hsc_t2dm_basic$preprocessing(dt_t2dm)

t2dm_fit <- hsc_t2dm_basic$train_model(train = tt_t2dm_basic$train, 
                                       strata = 'SEX', polynomial = T)
for(i in seq(0.1,0.5,0.1)){
  t2dm_male_perf <- hsc_t2dm$perfTest(fit = t2dm_fit$male, test = tt_t2dm$test[SEX==1], cutoff=i)
  t2dm_female_perf <- hsc_t2dm$perfTest(fit = t2dm_fit$female, test = tt_t2dm$test[SEX==2],cutoff=i)
  message('cutoff: ',i)
  print(rbindlist(list(t2dm_male_perf, t2dm_female_perf),idcol = T))
}


t2dm_male_perf <- hsc_t2dm$perfTest(fit = t2dm_fit$male, test = tt_t2dm$test[SEX==1], cutoff=0.2)
t2dm_female_perf <- hsc_t2dm$perfTest(fit = t2dm_fit$female, test = tt_t2dm$test[SEX==2],cutoff=0.2)
rbindlist(list(male = t2dm_male_perf, female = t2dm_female_perf), idcol = T)

hsc_t2dm$generateSample(data = rbindlist(tt_t2dm),
                        by = 5,
                        n = 1000)
hsc_t2dm$getHealthScore(fit = t2dm_fit, test=tt_t2dm$test)
hsc_t2dm$mergeSampleScore(target = 't2dm')

fread('sample_score/t2dm_sample_score.csv')




# htn ---------------------------------------------------------------------

dt_htn <- createCohort(target='htn',washout=12)

hsc_htn_basic <- HSC$new(target='htn', 
                         features=basic,
                         baseDir='~/working/Jang', inputDir='sample')
tt_htn_basic <- hsc_htn_basic$preprocessing(dt_htn)

htn_fit_basic <- hsc_htn_basic$train_model(train = tt_htn_basic$train, 
                                           strata = 'SEX', polynomial = T)
for(i in seq(0.1,0.5,0.1)){
  htn_male_perf <- hsc_htn$perfTest(fit = htn_fit$male, test = tt_htn$test[SEX==1], cutoff=i)
  htn_female_perf <- hsc_htn$perfTest(fit = htn_fit$female, test = tt_htn$test[SEX==2],cutoff=i)
  message('cutoff: ',i)
  print(rbindlist(list(htn_male_perf, htn_female_perf),idcol = T))
}


htn_male_perf <- hsc_htn$perfTest(fit = htn_fit$male, test = tt_htn$test[SEX==1], cutoff=0.2)
htn_female_perf <- hsc_htn$perfTest(fit = htn_fit$female, test = tt_htn$test[SEX==2],cutoff=0.2)
rbindlist(list(male = htn_male_perf, female = htn_female_perf), idcol = T)

hsc_htn$generateSample(data = rbindlist(tt_htn),
                       by = 5,
                       n = 1000)
hsc_htn$getHealthScore(fit = htn_fit, test=tt_htn$test)
hsc_htn$mergeSampleScore(target = 'htn')

fread('sample_score/htn_sample_score.csv')



# metsyn ------------------------------------------------------------------
dt_metsyn <- createCohort(target='metsyn',washout=12)
hsc_metsyn_basic <- HSC$new(target='metsyn',
                            features=basic,
                            baseDir='~/working/Jang', inputDir='sample')
tt_metsyn_basic <- hsc_metsyn_basic$preprocessing(dt_metsyn)

metsyn_fit <- hsc_metsyn_basic$train_model(train = tt_metsyn_basic$train, 
                                           strata = 'SEX', polynomial = T)
for(i in seq(0.1,0.5,0.1)){
  metsyn_male_perf <- hsc_metsyn$perfTest(fit = metsyn_fit$male, test = tt_metsyn$test[SEX==1], cutoff=i)
  metsyn_female_perf <- hsc_metsyn$perfTest(fit = metsyn_fit$female, test = tt_metsyn$test[SEX==2],cutoff=i)
  message('cutoff: ',i)
  print(rbindlist(list(metsyn_male_perf, metsyn_female_perf),idcol = T))
}


metsyn_male_perf <- hsc_metsyn$perfTest(fit = metsyn_fit$male, test = tt_metsyn$test[SEX==1], cutoff=0.2)
metsyn_female_perf <- hsc_metsyn$perfTest(fit = metsyn_fit$female, test = tt_metsyn$test[SEX==2],cutoff=0.2)
rbindlist(list(male = metsyn_male_perf, female = metsyn_female_perf), idcol = T)

hsc_metsyn$generateSample(data = rbindlist(tt_metsyn),
                          by = 5,
                          n = 1000)
hsc_metsyn$getHealthScore(fit = metsyn_fit, test=tt_metsyn$test)
hsc_metsyn$mergeSampleScore(target = 'metsyn')

# death -------------------------------------------------------------------
dt_death <- createCohort(target='death', washout=NULL)


hsc_death <- HSC$new(target='death', features=adv, baseDir='~/working/Jang', inputDir='sample')
hsc_death_basic <- HSC$new(target='death', features = basic,baseDir='~/working/Jang', inputDir='sample')
tt_death_basic <- hsc_death_basic$preprocessing(dt_death)

death_fit_basic <- hsc_death_basic$train_model(train = tt_death_basic$train, 
                                               strata = 'SEX', polynomial = T)
for(i in seq(0.1,0.5,0.1)){
  death_male_perf <- hsc_death$perfTest(fit = death_fit$male, test = tt_death$test[SEX==1], cutoff=i)
  death_female_perf <- hsc_death$perfTest(fit = death_fit$female, test = tt_death$test[SEX==2],cutoff=i)
  message('cutoff: ',i)
  print(rbindlist(list(death_male_perf, death_female_perf),idcol = T))
}


death_male_perf <- hsc_death$perfTest(fit = death_fit$male, test = tt_death$test[SEX==1], cutoff=0.2)
death_female_perf <- hsc_death$perfTest(fit = death_fit$female, test = tt_death$test[SEX==2],cutoff=0.2)
rbindlist(list(male = death_male_perf, female = death_female_perf), idcol = T)

hsc_death$generateSample(data = rbindlist(tt_death),
                         by = 5,
                         n = 1000)
hsc_death$getHealthScore(fit = death_fit, test=tt_death$test)
hsc_death$mergeSampleScore(target = 'death')

fread('sample_score/death_sample_score.csv')
