mortalityCohort <- function(major){
  enddate <- '2019-12-31'
  hra_gj <- gj_merge[HCHK_YEAR %in% c('2009','2010') & PERSON_ID %in% id2]
  hra_gj[jk2, on=.(PERSON_ID),DTH_MDY2 := i.DTH_MDY]
  hra_gj[,DTH_MDY := pmax(DTH_MDY, DTH_MDY2)]
  print('Creating checkup variables.')
  hra_gj[,`:=`(
    SEX=ifelse(SEX==1,1,0),
    # REGION = ifelse(SIDO %in% c('11','26','27','28','29','30','31','36','41'),1,0) %>% as.factor,
    SIDO = NULL,
    eGFR = ifelse(SEX==1, 175*CREATININE^-1.154 * AGE^-0.203,
                  175*CREATININE^-1.154 * AGE^-0.203*0.742)
  )][,`:=`(
    SMK = ifelse(SMK_STAT_TYPE_RSPS_CD==1,0,
                 ifelse(SMK_STAT_TYPE_RSPS_CD==2,1,2))
  )][,`:=`(
    MET_WALK = (WLK30_WEK_FREQ_ID * 20 * 3.3),
    MET_MED = (MOV20_WEK_FREQ_ID * 30 * 4.0),
    MET_HIGH = (MOV30_WEK_FREQ_ID * 30 * 8.0),
    DRK = ifelse(DRNK_HABIT_RSPS_CD=='',NA,
                 ifelse(DRNK_HABIT_RSPS_CD==1,0,
                        ifelse(DRNK_HABIT_RSPS_CD>=5 & TM1_DRKQTY_RSPS_CD>=7 & SEX==1 |
                                 DRNK_HABIT_RSPS_CD>=5 & TM1_DRKQTY_RSPS_CD>=5 & SEX==0,2,1)))
  )][,MET_CON := MET_WALK + MET_MED + MET_HIGH
     ][,PA := ifelse(MOV20_WEK_FREQ_ID + MOV30_WEK_FREQ_ID>=4 & MET_CON>=1500 |
                       MOV20_WEK_FREQ_ID + MOV30_WEK_FREQ_ID>=6 |
                       MOV20_WEK_FREQ_ID + MOV30_WEK_FREQ_ID + WLK30_WEK_FREQ_ID>=6 & MET_CON>=600,1,
                     ifelse(MET_CON>=3000,2,0))]
  
  # outcome -----------------------------------------------------------------
  
  # how to define outcome?
  # maces <- c('hf','mace_angina','mace','revas','mi','stroke')
  
  # hra_gj[,PERSON_ID := as.character(PERSON_ID)]
  for(i in major){
    d <- get(i)
    dt <- paste0(i,'_dt')
    d[,PERSON_ID := as.character(PERSON_ID)]
    temp <- d[,year := substr(RECU_FR_DT,1,4)][,.N, by=.(PERSON_ID,year,FORM_CD)]
    if(i %in% c('lymphoma','cancer_meta','cancer_nonmeta')){
      id <- temp[N>=1, unique(PERSON_ID)]
      disease <- d[PERSON_ID %in% id,.(dt=first(RECU_FR_DT)),by=PERSON_ID]
    } else {
      id <- temp[N>=2, unique(PERSON_ID)]
      disease <- d[PERSON_ID %in% id,.(dt=first(RECU_FR_DT)),by=PERSON_ID]
    }
    hra_gj[disease, on=.(PERSON_ID), eval(dt):=i.dt]
  }
  
  print('Outcome date completed.')
  
  # Family History
  target <- setdiff(hra_gj[,.SD, .SDcols=grep('FMLY_',names(hra_gj))] %>% names(),c('FMLY_LIVER_DISE_PATIEN_YN','FMLY_CANCER_PATIEN_YN'))
  hra_gj[,(target):=lapply(.SD, function(x) ifelse(x=='',NA,x) %>% as.numeric),.SDcols=target]
  hra_gj[,(target):=lapply(.SD, function(x) ifelse(is.na(x), max(x, na.rm=T), x)),.SDcols=target, by=PERSON_ID]
  hra_gj[,(target):=lapply(.SD, function(x) ifelse(x==-Inf,NA, x)),.SDcols=target]
  
  print('Family variable completed.')
  
  # JK after 3 years
  # dcast(hra_gj, PERSON_ID ~ HCHK_YEAR)[,sum:=rowSums(.SD),.SDcols=4:6][sum>=2]
    
  df <- hra_gj[hra_gj[,.I[1L],by=PERSON_ID]$V1]
  target <- df[,.SD,.SDcol=grep('_dt|_DT|_MDY',names(df))] %>% names()
  df[,(target):=lapply(.SD, function(x) as.Date(as.character(x),'%Y%m%d')),.SDcols=target]
  
  lag_day <- 0  
  if(major=='major2'){
    except_id <- df[difftime(mi_dt, HME_DT, units='days')<=lag_day |
                      difftime(chronic_heart_dt, HME_DT, units='days')<=lag_day |
                      difftime(peri_vascular_dt, HME_DT, units='days')<=lag_day |
                      difftime(angina_dt, HME_DT, units='days')<=lag_day |
                      difftime(stroke_dt, HME_DT, units='days')<=lag_day |
                      difftime(renal_dt, HME_DT, units='days')<=lag_day |
                      difftime(dm_dt, HME_DT, units='days')<=lag_day |
                      difftime(lymphoma_dt, HME_DT, units='days')<=lag_day |
                      difftime(cancer_meta_dt, HME_DT, units='days')<=lag_day |
                      difftime(cancer_nonmeta_dt, HME_DT, units='days')<=lag_day |
                      difftime(plegia_dt, HME_DT, units='days')<=lag_day |
                      PERSON_ID %in% disability$PERSON_ID, unique(PERSON_ID)]
  } else {
    except_id <- df[difftime(mi_dt, HME_DT, units='days')<=lag_day |
                        difftime(chronic_heart_dt, HME_DT, units='days')<=lag_day |
                        difftime(peri_vascular_dt, HME_DT, units='days')<=lag_day |
                        difftime(angina_dt, HME_DT, units='days')<=lag_day |
                        difftime(stroke_dt, HME_DT, units='days')<=lag_day |
                        difftime(copd_dt, HME_DT, units='days')<=lag_day |
                        difftime(renal_dt, HME_DT, units='days')<=lag_day |
                        difftime(dm_dt, HME_DT, units='days')<=lag_day |
                        difftime(lymphoma_dt, HME_DT, units='days')<=lag_day |
                        difftime(cancer_meta_dt, HME_DT, units='days')<=lag_day |
                        difftime(cancer_nonmeta_dt, HME_DT, units='days')<=lag_day |
                        difftime(rheumatic_dt, HME_DT, units='days')<=lag_day |
                        difftime(plegia_dt, HME_DT, units='days')<=lag_day |
                        difftime(parkinson_dt, HME_DT, units='days')<=lag_day |
                        difftime(dementia_dt, HME_DT, units='days')<=lag_day |
                        difftime(depression_dt, HME_DT, units='days')<=lag_day |
                        PERSON_ID %in% disability$PERSON_ID, unique(PERSON_ID)]
  }
    
    print(paste0("No. of Exclusion for Health Age : ", length(except_id)))
    df <- df[!PERSON_ID %in% except_id]
    
    # df[,mi_yn:= ifelse(is.na(mi_dt),0,1)]
    # df[,stroke_yn := ifelse(is.na(stroke_dt),0,1)]
    df[,death := ifelse(is.na(DTH_MDY),0,1) %>% as.factor]
    
    df[,`:=`(
      death_duration = ifelse(death==1,
                              difftime(DTH_MDY, HME_DT,units='days'),
                              difftime(enddate,HME_DT, units='days')) %>% as.numeric
      
    )]
      target <- df[,.(death, death_duration,
                      SEX,AGE, REGION,TOT_CHOLE,HDL_CHOLE, LDL_CHOLE, TRIGLYCERIDE,
                      BLDS, HMG, SGOT_AST,SGPT_ALT, GAMMA_GTP, BMI,WAIST,
                      BP_LWST,BP_HIGH,eGFR, SMK, DRK, MET_CON,
                      
                      FMLY_HPRTS_PATIEN_YN, FMLY_APOP_PATIEN_YN, FMLY_HDISE_PATIEN_YN, 
                      FMLY_DIABML_PATIEN_YN)] %>% names()
    
  return(df[death_duration>=365,..target])
}
haCohort <- function(major, illness_def){
  require(dplyr)
  require(data.table)
  enddate <- '2019-12-31'
  hra_gj <- gj_merge[HCHK_YEAR %in% c('2009','2010') &
                       PERSON_ID %in% id2]
  hra_gj[jk2, on=.(PERSON_ID),DTH_MDY2 := i.DTH_MDY]
  hra_gj[,DTH_MDY := pmax(DTH_MDY, DTH_MDY2)]
  hra_gj[,PERSON_ID:=as.character(PERSON_ID)]
  message('Creating checkup variables.')
  hra_gj[,`:=`(
    SEX=ifelse(SEX==1,1,0),
    REGION = ifelse(SIDO %in% c('11','26','27','28','29','30','31','36','41'),1,0) %>% as.factor,
    # SIDO = NULL,
    eGFR = ifelse(SEX==1, 175*CREATININE^-1.154 * AGE^-0.203,
                  175*CREATININE^-1.154 * AGE^-0.203*0.742),
    SMK_STATUS = ifelse(SMK_STAT_TYPE_RSPS_CD=="",NA, as.integer(SMK_STAT_TYPE_RSPS_CD)-1L),
    DRNK_DAY= ifelse(DRNK_HABIT_RSPS_CD=="",NA, as.integer(DRNK_HABIT_RSPS_CD)-1L)
  )][,`:=`(
    CUR_SMK_PER_DAY = ifelse(is.na(SMK_STATUS)|SMK_STATUS==0,0, CUR_DSQTY_RSPS_CD),
    PAST_SMK_PER_DAY = ifelse(is.na(SMK_STATUS)|SMK_STATUS==0,0,PAST_DSQTY_RSPS_CD),
    DRNK_AMT_PER_ONCE = ifelse(is.na(DRNK_DAY)|DRNK_DAY==0,0,TM1_DRKQTY_RSPS_CD)
  )][,`:=`(
    MET_WALK = (WLK30_WEK_FREQ_ID * 20 * 3.3),
    MET_MED = (MOV20_WEK_FREQ_ID * 30 * 4.0),
    MET_HIGH = (MOV30_WEK_FREQ_ID * 30 * 8.0),
    DRK = ifelse(DRNK_HABIT_RSPS_CD==1,0,
                        ifelse(DRNK_DAY>=5 & DRNK_AMT_PER_ONCE>=7 & SEX==1 |
                                 DRNK_DAY>=5 & DRNK_AMT_PER_ONCE>=5 & SEX==0,2,1))
  )][,MET_CON := MET_WALK + MET_MED + MET_HIGH
     ][,PA := ifelse(MOV20_WEK_FREQ_ID + MOV30_WEK_FREQ_ID>=4 & MET_CON>=1500 |
                       MOV20_WEK_FREQ_ID + MOV30_WEK_FREQ_ID>=6 |
                       MOV20_WEK_FREQ_ID + MOV30_WEK_FREQ_ID + WLK30_WEK_FREQ_ID>=6 & MET_CON>=600,1,
                     ifelse(MET_CON>=3000,2,0))]
  
  # outcome -----------------------------------------------------------------
  major2 <- c(major,'cancer_male','cancer_female')
  for(i in major2){
    d <- get(i)
    d[,PERSON_ID := as.character(PERSON_ID)]
    dt <- paste0(i,'_dt')
    temp <- d[,year := substr(RECU_FR_DT,1,4)][,.N, by=.(PERSON_ID,FORM_CD,year)]
    if(i %in% c('lymphoma','cancer_meta','cancer_nonmeta','cancer_male','cancer_female')){
      id <- temp[N>=2, unique(PERSON_ID)]
      disease <- d[PERSON_ID %in% id,.(dt=first(RECU_FR_DT)),by=PERSON_ID]
    } 
    # else if(i %in% c()){
    #   id <- temp[N>=1, unique(PERSON_ID)]
    #   disease <- d[PERSON_ID %in% id,.(dt=first(RECU_FR_DT)),by=PERSON_ID]
    # } 
    else {
      # cancer: 1, others: n>=2
      if(illness_def==1){
        id <- temp[(N>=2) , unique(PERSON_ID)]
        disease <- d[PERSON_ID %in% id,.(dt=first(RECU_FR_DT)),by=PERSON_ID]
      }
      # others : inpatients n>=1
      else if(illness_def==2){
        id <- temp[(FORM_CD==2 & N>=2) , unique(PERSON_ID)]
        disease <- d[FORM_CD==2 & PERSON_ID %in% id,.(dt=first(RECU_FR_DT)),by=PERSON_ID]
      }
      # others : inpatients n>=2
      else if (illness_def==3){
        id <- temp[(FORM_CD==2 & N>=2) , unique(PERSON_ID)]
        disease <- d[FORM_CD==2 & PERSON_ID %in% id,.(dt=first(RECU_FR_DT)),by=PERSON_ID]
      }
      # others: inpatents n>=1 | outpatients n>=2
      else if(illness_def==4){
        id <- temp[(FORM_CD==2 & N>=1 | FORM_CD==3 & N>=2) , unique(PERSON_ID)]
        disease <- d[FORM_CD %in% c(2,3) & PERSON_ID %in% id,.(dt=first(RECU_FR_DT)),by=PERSON_ID]
      }
      # others: inpatents n>=2 | outpatients n>=3
      else if(illness_def==5){
        id <- temp[(FORM_CD==2 & N>=2 | FORM_CD==3 & N>=3) , unique(PERSON_ID)]
        disease <- d[FORM_CD %in% c(2,3) & PERSON_ID %in% id,.(dt=first(RECU_FR_DT)),by=PERSON_ID]
      }
    }
    hra_gj[disease, on=.(PERSON_ID), eval(dt):=i.dt]
  }
  
  message('Outcome date completed.')
  
  # Family History
  target <- setdiff(hra_gj[,.SD, .SDcols=grep('FMLY_',names(hra_gj))] %>% names(),c('FMLY_LIVER_DISE_PATIEN_YN','FMLY_CANCER_PATIEN_YN'))
  hra_gj[,(target):=lapply(.SD, function(x) ifelse(x=='',NA,x) %>% as.numeric),.SDcols=target]
  hra_gj[,(target):=lapply(.SD, function(x) ifelse(is.na(x), max(x, na.rm=T), x)),.SDcols=target, by=PERSON_ID]
  hra_gj[,(target):=lapply(.SD, function(x) ifelse(x==-Inf,NA, x)),.SDcols=target]
  
  message('Family variable completed.')
  
  # Earliest GJ by PERSON_ID
  df <- hra_gj[hra_gj[,.I[1L],by=PERSON_ID]$V1]
  
  # last GJ date by PERSON_ID
  # last <- hra_gj[hra_gj[,.I[.N],by=PERSON_ID]$V1, .(PERSON_ID,HME_DT)]
  
  # No. of GJ by PERSON_ID
  # cnt <- hra_gj[,.(n=.N),by=PERSON_ID]
  
  # df[last, on=.(PERSON_ID), last_gj_date := as.Date(i.HME_DT,'%Y%m%d')]
  # df[cnt, on=.(PERSON_ID),gj_count := i.n]
  # DATE format
  target <- df[,.SD,.SDcol=grep('_dt|_DT|_MDY',names(df))] %>% names()
  df[,(target):=lapply(.SD, function(x) as.Date(as.character(x),'%Y%m%d')),.SDcols=target]
  
  lag_day <- 365  
  except_id <- c()
  for(i in major2){
    dt <- paste0(i,'_dt')
    except_id <- c(except_id,df[difftime(get(dt),HME_DT, units='days')<=lag_day,unique(PERSON_ID)])
  }
  except_id <- c(except_id, df[PERSON_ID %in% disability$PERSON_ID, unique(PERSON_ID)])
  df <- df[!PERSON_ID %in% except_id]
  message("No. of Exclusion for Health Age : ", length(except_id))
  
  target <- names(df[,.SD,.SDcols=grep('_dt',names(df))])

  ## target incidence date
  df[,ill_date := do.call(pmin, c(df[,..target],na.rm=T))]
  df[, mace_date := pmin(mi_dt, chronic_heart_dt, peri_vascular_dt, angina_dt,
                     stroke_dt, na.rm=T)]
  df[,cancer_date := pmin(lymphoma_dt, cancer_meta_dt, cancer_nonmeta_dt,na.rm=T)]
  
  # target yn
  df[,`:=`(
    ha = ifelse(is.na(ill_date),0,1) %>% as.factor,
    death = ifelse(is.na(DTH_MDY),0,1) %>% as.factor,
    cancer_male = ifelse(is.na(cancer_male_dt),0,1) %>% as.factor,
    cancer_female = ifelse(is.na(cancer_female_dt),0,1) %>% as.factor,
    cancer = ifelse(is.na(lymphoma_dt)&
             is.na(cancer_meta_dt)&
             is.na(cancer_nonmeta_dt),0,1) %>% as.factor,
    mace = ifelse(is.na(mace_date),0,1) %>% as.factor
  )]
  
  # target duration
  df[,`:=`(
    ha_duration = ifelse(ha==1,
                         difftime(ill_date, HME_DT, units='days'),
                         difftime(enddate, HME_DT, units='days')) %>% as.numeric,
    mace_duration = ifelse(mace==1,
                           difftime(mace_date,HME_DT, units='days'),
                           difftime(enddate, HME_DT, units='days')) %>% as.numeric,
    death_duration = ifelse(death==1,
                            difftime(DTH_MDY, HME_DT,units='days'),
                            difftime(enddate,HME_DT, units='days')) %>% as.numeric,
    cancer_duration = ifelse(cancer==1,
                             difftime(cancer_date, HME_DT, units='days'),
                             difftime(enddate, HME_DT, units='days')) %>% as.numeric,
    cancer_male_duration = ifelse(cancer_male==1,
                                  difftime(cancer_male_dt,HME_DT, units='days'),
                                  difftime(enddate, HME_DT, units='days')) %>% as.numeric,
    cancer_female_duration = ifelse(cancer_female==1,
                                  difftime(cancer_female_dt,HME_DT, units='days'),
                                  difftime(enddate, HME_DT, units='days')) %>% as.numeric
    # gj_gap = difftime(last_gj_date, HME_DT, unit='days') %>% as.numeric
  )]
  target <- df[,.(death, death_duration,ha, ha_duration,mace, mace_duration,
                  cancer, cancer_duration,
                  cancer_male, cancer_male_duration,cancer_female, cancer_female_duration,
                  SEX,AGE, REGION, TOT_CHOLE,HDL_CHOLE, LDL_CHOLE, TRIGLYCERIDE,BLDS, HMG, SGOT_AST,SGPT_ALT, GAMMA_GTP, BMI,WAIST,
                  BP_LWST,BP_HIGH,eGFR, 
                  SMK_STATUS, PAST_SMK_PER_DAY, CUR_SMK_PER_DAY,
                  DRK, DRNK_DAY, DRNK_AMT_PER_ONCE,
                  MET_CON,FMLY_HPRTS_PATIEN_YN, FMLY_APOP_PATIEN_YN, FMLY_HDISE_PATIEN_YN, 
                  FMLY_DIABML_PATIEN_YN)] %>% names()
  return(df[ha_duration>0,..target])
}
disease <- c('mi','chronic_heart','peri_vascular','angina','stroke',
           'copd','renal','dm','lymphoma','cancer_meta','cancer_nonmeta',
           'rheumatic','plegia','parkinson','dementia','depression')
disease2 <- c('mi','chronic_heart','peri_vascular','angina','stroke',
            'renal','dm','lymphoma','cancer_meta','cancer_nonmeta',
            'plegia')

dt_ha <- haCohort(major=disease, illness_def=2)
dt_ha1 <- haCohort(major=disease, illness_def = 1)
dt_ha2 <- haCohort(major=disease, illness_def = 2)
dt_ha3 <- haCohort(major=disease, illness_def = 3)
dt_ha4 <- haCohort(major=disease, illness_def = 4)
dt_ha5 <- haCohort(major=disease, illness_def = 5)

dt_ha2[,AGE_CAT := ifelse(AGE<50,1,
                    ifelse(AGE<60,2,
                     ifelse(AGE<70,3,4))) %>% as.factor()]
dt_ha2[,summary(.SD),.SDcols=grep('_duration',names(dt_ha))]
str(dt_ha)

dt_omit <- na.omit(dt)
dt_omit[,AGE_cat := ifelse(AGE<50,'age40',
                           ifelse(AGE<60,'age50',
                               ifelse(AGE<70,'age60','age70')))]

dt2_omit <- na.omit(dt2)
dt_omit %>% summary()
dt_ha1
