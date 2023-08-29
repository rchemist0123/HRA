require(data.table)
require(dplyr)
require(moonBook)
library(dplyr)
require(haven)


# reproductive  -----------------------------------------------------------

createCohort <- function(cohort, ver, cond){
  enddate <- '2019-12-31'
  hra_gj <- gj_merge[PERSON_ID %in% id2]
  print('Creating checkup variables.')
  hra_gj[,`:=`(
    GAMMA_GPT=NULL,
    SEX=ifelse(SEX==1,1,0),
    REGION = ifelse(SIDO %in% c('11','26','27','28','29','30','31','36','41'),1,0) %>% as.factor,
    SIDO = NULL,
    egfr = ifelse(SEX==1, 183.6*CREATININE^-1.154 * AGE^-0.203,
                     183.6*CREATININE^-1.154 * AGE^-0.203*0.742)
    )][,`:=`(
    HDL = ifelse(HDL_CHOLE>=60,1,
            ifelse(HDL_CHOLE %between% c(50,59) & SEX==0 |
                     HDL_CHOLE %between% c(40,59) & SEX==1,2,3)),
    TG = ifelse(TRIGLYCERIDE<150,1,
           ifelse(TRIGLYCERIDE>=500,3,2)),
    LDL = ifelse(LDL_CHOLE<100,1,
            ifelse(LDL_CHOLE %between% c(100,129),2,
             ifelse(LDL_CHOLE %between% c(130,159),3,
              ifelse(LDL_CHOLE %between% c(160,189),4,5)))),
    TCHOLE = ifelse(TOT_CHOLE<=200,1,
                  ifelse(TOT_CHOLE %between% c(201,240),2,
                   ifelse(TOT_CHOLE %between% c(241,300),3,4))),
    FBG = ifelse(BLDS<100,1,
            ifelse(BLDS %between% c(100,125),2,
             ifelse(BLDS %between% c(126,200),3,4))),
    OBE = ifelse(BMI %between% c(18.5,22.9),1,
            ifelse(BMI <18.5,2,
             ifelse(BMI %between% c(23.0, 24.9),3,
              ifelse(BMI %between% c(25.0,29.9),4,5)))) ,
    BP = ifelse(BP_HIGH %between% c(1,129) & BP_LWST %between% c(1,84),1,
           ifelse(BP_HIGH %between% c(130,139) | BP_LWST %between% c(85,89),2,
            ifelse(BP_HIGH %between% c(140,159) | BP_LWST %between% c(90,99),3,4))) ,
    LIVER = ifelse(SGOT_AST %between% c(1,39) & SGPT_ALT %between% c(1,39) & GAMMA_GTP %between% c(1,39),1,
              ifelse(SGOT_AST %between% c(40,119) | SGPT_ALT %between% c(40,119) | GAMMA_GTP %between% c(40,119),2,
               ifelse(SGOT_AST %between% c(120, 199) | SGPT_ALT %between% c(120,199) | GAMMA_GTP %between% c(120,199),3,4))) ,
    WC = ifelse(WAIST<90 & SEX==1 | WAIST<85 & SEX==0,1,2) ,
    AGE_CAT = ifelse(AGE %between% c(47,49),'4749',
            ifelse(AGE %between% c(50,54),'5054',
             ifelse(AGE %between% c(55,59),'5559',
              ifelse(AGE %between% c(60,64),'6064',
               ifelse(AGE %between% c(65,69),'6569','7074'))))) ,
    DRK = ifelse(DRNK_HABIT_RSPS_CD=='',NA,
            ifelse(DRNK_HABIT_RSPS_CD==1,0,
             ifelse(DRNK_HABIT_RSPS_CD>=4 & TM1_DRKQTY_RSPS_CD>=7 & SEX==1 |
                      DRNK_HABIT_RSPS_CD>=4 & TM1_DRKQTY_RSPS_CD>=5 & SEX==0,2,1))) ,
    PROTEURIA = ifelse(OLIG_PROTE_CD<=3,1,
                  ifelse(OLIG_PROTE_CD==4,2,3)),
    HB = ifelse(HMG %between% c(13,16.5) & SEX==1 |
                   HMG %between% c(12,16) & SEX==0,1,
           ifelse(HMG <12 & SEX==0 | HMG<13 & SEX==1,2,3)),
    eGFR = ifelse(egfr>=90,1,
             ifelse(egfr %between% c(60,89),2,
              ifelse(egfr %between% c(30,59),3,
               ifelse(egfr %between% c(15,29),4,5)))),
    SMK = ifelse(SMK_STAT_TYPE_RSPS_CD==1,0,
            ifelse(SMK_STAT_TYPE_RSPS_CD==2,1,2)) ,
    MOV20_WEK_FREQ_ID = ifelse(MOV20_WEK_FREQ_ID=='',NA,as.numeric(MOV20_WEK_FREQ_ID)-1) %>% as.numeric,
    MOV30_WEK_FREQ_ID = ifelse(MOV30_WEK_FREQ_ID=='',NA,as.numeric(MOV30_WEK_FREQ_ID)-1) %>% as.numeric,
    WLK30_WEK_FREQ_ID = ifelse(WLK30_WEK_FREQ_ID=='',NA,as.numeric(WLK30_WEK_FREQ_ID)-1) %>% as.numeric
  )][,`:=`(
    MET_WALK = (WLK30_WEK_FREQ_ID * 20 * 3.3),
    MET_MED = (MOV20_WEK_FREQ_ID * 30 * 4.0),
    MET_HIGH = (MOV30_WEK_FREQ_ID * 30 * 8.0)
  )][,MET_CON := MET_WALK + MET_MED + MET_HIGH
     ][,PA := ifelse(MOV20_WEK_FREQ_ID + MOV30_WEK_FREQ_ID>=4 & MET_CON>=1500 |
                      MOV20_WEK_FREQ_ID + MOV30_WEK_FREQ_ID>=6 |
                       MOV20_WEK_FREQ_ID + MOV30_WEK_FREQ_ID + WLK30_WEK_FREQ_ID>=6 & MET_CON>=600,1,
               ifelse(MET_CON>=3000,2,0))]

  ## merge cci
  hra_gj[cci, on=.(PERSON_ID, HCHK_YEAR), CCI := i.CCI]
  hra_gj[,CCI := ifelse(is.na(CCI), max(CCI,na.rm=T), CCI),by=PERSON_ID]
  hra_gj[,CCI:=ifelse(CCI==-Inf,NA,CCI)]

## medication ----------------------

hra_gj[,`:=`(
  T2DM_MED = ifelse(PERSON_ID %in% t2dm_med_final,1,0) %>% as.factor,
  HPTN_MED = ifelse(PERSON_ID %in% hptn_med_final,1,0) %>% as.factor,
  DLPM_MED = ifelse(PERSON_ID %in% lpdm_med_final,1,0) %>% as.factor
)]

  print('Medication completed.')
# outcome -----------------------------------------------------------------

  # how to define outcome?
  major <- c('mi','chronic_heart','peri_vascular','angina','stroke',
             'copd','renal','dm','lymphoma','cancer_meta','cancer_nonmeta',
             'rheumatic','plegia','parkinson','dementia','depression')
  maces <- c('hf','mace_angina','mace','revas','mi','stroke')
  
  hra_gj[,PERSON_ID := as.character(PERSON_ID)]
  for(i in major){
    d <- get(i)
    dt <- paste0(i,'_dt')
    d[,PERSON_ID := as.character(PERSON_ID)]
      temp <- d[,year := substr(RECU_FR_DT,1,4)][,.N, by=.(PERSON_ID,year,FORM_CD)]
      if(cond==1){
        id <- temp[FORM_CD==2 & N>=1, unique(PERSON_ID)]
        disease <- d[FORM_CD==2& PERSON_ID %in% id,.(dt=first(RECU_FR_DT)),by=PERSON_ID]
      } else if(cond==2) {
        id <- temp[FORM_CD==2 & N>=2, unique(PERSON_ID)]
        disease <- d[FORM_CD==2& PERSON_ID %in% id,.(dt=first(RECU_FR_DT)),by=PERSON_ID]
      } else if(cond==3){
        id <- temp[(FORM_CD==2 & N>=1) | (FORM_CD==3 & N>=2), unique(PERSON_ID)]
        disease <- d[(FORM_CD==2 | FORM_CD==3) & PERSON_ID %in% id,.(dt=first(RECU_FR_DT)),by=PERSON_ID]
      } else if (cond==4) {
        id <- temp[N>=3, unique(PERSON_ID)]
        disease <- d[PERSON_ID %in% id,.(dt=first(RECU_FR_DT)),by=PERSON_ID]
      } else {
        disease <- d[,.(dt=first(RECU_FR_DT)),by=PERSON_ID]
      }
      hra_gj[disease, on=.(PERSON_ID), eval(dt):=i.dt]
  }
  
  
  print('Outcome date completed.')
  hra_gj[hf, on=.(PERSON_ID), hf_dt := i.RECU_FR_DT]
  hra_gj[mace, on=.(PERSON_ID),mace_death_dt := i.DEATH_DATE]
  hra_gj[revas,on=.(PERSON_ID), revas_dt := i.RECU_FR_DT]
  
  # Family History
  target <- setdiff(hra_gj[,.SD, .SDcols=grep('FMLY_',names(hra_gj))] %>% names(),'FMLY_LIVER_DISE_PATIEN_YN')
  hra_gj[,(target):=lapply(.SD, function(x) ifelse(x=='',NA,x) %>% as.numeric),.SDcols=target]
  hra_gj[,(target):=lapply(.SD, function(x) ifelse(is.na(x), max(x, na.rm=T), x)),.SDcols=target, by=PERSON_ID]
  hra_gj[,(target):=lapply(.SD, function(x) ifelse(x==-Inf,NA, x)),.SDcols=target]
  
  print('Family variable completed.')
  
  # hra_gj[,PERSON_ID := as.character(PERSON_ID)]
  hra_gj[hptn_med_all[year %between% c(2009,2015)], on=.(PERSON_ID, HCHK_YEAR = year), 
         hptn_med_dt := i.RECU_FR_DT]
  hra_gj[lpdm_med_all[year %between% c(2009,2015)], on=.(PERSON_ID, HCHK_YEAR = year), 
         lpdm_med_dt := i.RECU_FR_DT]
  hra_gj[t2dm_med_all[year %between% c(2009,2015)], on=.(PERSON_ID, HCHK_YEAR = year), 
         t2dm_med_dt := i.RECU_FR_DT]
  
  # burden ------------------------------------------------------------------
  if(!is.null(ver)){
    print('Create burden variables...')
    hra_gj[,`:=`(
      WC_burden = ifelse(WC==2,1,0),
      HDL_burden = ifelse(HDL>=2,1,0),
      OBE_burden = ifelse(OBE>=4,1,0),
      PA_burden = ifelse(PA==0,1,0),
      SMK_burden= ifelse(SMK==2,1,0),
      DRK_burden = ifelse(DRK==2,1,0),
      TG_burden = ifelse(TG>=2 | !is.na(lpdm_med_dt),1,0),
      BP_burden = ifelse(BP>=2 | !is.na(hptn_med_dt),1,0),
      FBG_burden = ifelse(FBG>=2 | !is.na(t2dm_med_dt),1,0)
    )]
    
    hra_gj[,METS_burden := ifelse(WC_burden + TG_burden + HDL_burden +
                                    BP_burden + FBG_burden>=3,1,0)]
    
    hra_gj[,PERSON_ID := as.numeric(PERSON_ID)]
    
    target <- c(grep('_burden',names(hra_gj),value = T),'PERSON_ID')
    hra_first_gj <- hra_gj[hra_gj[PERSON_ID %in% ver, .I[1L],by=PERSON_ID]$V1,..target] 
    hra_second_gj <- hra_gj[hra_gj[PERSON_ID %in% ver, .I[2L],by=PERSON_ID]$V1,..target]
    names(hra_second_gj) <- paste0(names(hra_second_gj),'_2nd')
    setnames(hra_second_gj,'PERSON_ID_2nd','PERSON_ID')
    
    hra_third_gj <- hra_gj[hra_gj[PERSON_ID %in% ver, .I[.N],by=PERSON_ID]$V1,..target]
    names(hra_third_gj) <- paste0(names(hra_third_gj),'_3rd')
    setnames(hra_third_gj,'PERSON_ID_3rd','PERSON_ID')
    
    hra_burden <- hra_first_gj[hra_second_gj, on=.(PERSON_ID)]
    hra_burden <- hra_burden[hra_third_gj, on=.(PERSON_ID)]
    
    
    temp <- c('WC_','HDL_','OBE_','TG_','BP_','FBG_','PA_','SMK_','DRK_','METS_')
    for(i in temp){
      name <- paste0(i,'burden_total')
      hra_burden[,eval(name) := rowSums(.SD,na.rm=T) %>% as.factor,.SDcols = grep(i, names(hra_burden))]
    }
    target <- c(grep('burden_total',names(hra_burden),value=T),'PERSON_ID')
    hra_gj <- hra_burden[,..target][hra_gj[PERSON_ID %in% ver], on=.(PERSON_ID)]
    hra_gj[,PERSON_ID := as.integer(PERSON_ID)]
    print('Burden variables completed.')
    df <- hra_gj[hra_gj[PERSON_ID %in% ver & HCHK_YEAR %in% c('2011','2012','2013'),.I[.N],by=PERSON_ID]$V1]
  }
  else if (is.null(ver)){
    df <- hra_gj[hra_gj[HCHK_YEAR %in% c('2009','2010'),.I[.N],by=PERSON_ID]$V1]
    print(paste0("Patients in Cohort first: ",nrow(df)))
    # Over 2 checkup after 2009-2010
    id <- dcast(hra_gj, PERSON_ID ~ HCHK_YEAR, 
                value.var = 'SEX',
                fun.aggregate = length)[, temp := rowSums(.SD),.SDcols=4:8][temp>=2,PERSON_ID]
    df <- df[PERSON_ID %in% id]
  } 
  print(paste0("Patients in Cohort: ",nrow(df)))
  
  # date format
  target <- df[,.SD,.SDcol=grep('_dt|_DT|_MDY',names(df))] %>% names()
  df[,(target):=lapply(.SD, function(x) as.Date(as.character(x),'%Y%m%d')),.SDcols=target]
  # Exclusion: Diagnosed before GJ (2009 or 2010)
  
  target <- c('SEX' , 'AGE_CAT' , 'REGION' , 'HDL' , 'LDL' , 'TG' , 'TCHOLE' ,
                'FBG' , 'OBE' , 'BP' , 'LIVER' , 'WC' , 'DRK' , 'PROTEURIA' , 'HB' , 'eGFR' ,'SMK' , 'PA')
  df[,(target):=lapply(.SD, as.factor),.SDcols=target]
  
  if (cohort =='ha'){
    if(is.null(ver)) {lag_day <- 0}
    else lag_day <- 365
    
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
    
    print(paste0("No. of Exclusion for Health Age : ", length(except_id)))
    df <- df[!PERSON_ID %in% except_id]
    df[,mi_yn:= ifelse(is.na(mi_dt),0,1)]
    df[,stroke_yn := ifelse(is.na(stroke_dt),0,1)]
    
    df[,death := ifelse(is.na(DTH_MDY),0,1)]
    df[,ill_date := pmin(mi_dt,chronic_heart_dt,peri_vascular_dt,angina_dt,
                             stroke_dt,copd_dt, renal_dt,dm_dt, lymphoma_dt,
                             cancer_meta_dt,cancer_nonmeta_dt, rheumatic_dt, plegia_dt,
                             parkinson_dt, dementia_dt, depression_dt, na.rm=T)]
    df[,ill_yn := ifelse(is.na(ill_date),0,1) %>% as.factor]
    
    df[,mace_date := pmin(mi_dt, stroke_dt,angina_dt,
                          hf_dt, mace_death_dt, revas_dt,na.rm=T)]
    df[,mace_yn := ifelse(is.na(mace_date),0,1) %>% as.factor] 
    # df[,cancer_male := ifelse(is.na(cancer_male_dt),0,1) %>% as.factor]
    # df[,cancer_female := ifelse(is.na(cancer_female_dt),0,1) %>% as.factor]
    df[,`:=`(
      # cancer_male_duration = ifelse(cancer_male==1,
      #                               difftime(cancer_male_dt, HME_DT, units='days'),
      #                               difftime(enddate, HME_DT, units='days')) %>% as.numeric,
      # cancer_female_duration = ifelse(cancer_female==1,
      #                               difftime(cancer_female_dt, HME_DT, units='days'),
                                    # difftime(enddate, HME_DT, units='days')) %>% as.numeric,
      ill_duration = ifelse(ill_yn==1,
                                 difftime(ill_date, HME_DT,units='days'),
                                 difftime(enddate,HME_DT, units='days')) %>% as.numeric,
      mace_duration = ifelse(mace_yn==1,
                             difftime(mace_date, HME_DT, units='days'),
                             difftime(enddate, HME_DT, units='days')) %>% as.numeric,
      death_duration = ifelse(death==1,
                                   difftime(DTH_MDY, HME_DT,units='days'),
                                   difftime(enddate,HME_DT, units='days')) %>% as.numeric
      
    )]
    if(is.null(ver)){
      target <- df[,.(ill_yn, ill_duration, death, death_duration,
                      SEX,AGE_CAT, REGION,

                      # HDL,LDL, TG, TCHOLE,
                      # FBG, OBE, LIVER, BP,WC, DRK, PROTEURIA, HB, eGFR,
                      SMK, PA, CCI, T2DM_MED, HPTN_MED, DLPM_MED,
                      FMLY_HPRTS_PATIEN_YN, FMLY_APOP_PATIEN_YN, FMLY_HDISE_PATIEN_YN, 
                      FMLY_DIABML_PATIEN_YN, FMLY_CANCER_PATIEN_YN)] %>% names()
    } else {
      target <- df[,.(ill_yn, ill_duration, 
                      death, death_duration,
                      mace_yn, mace_duration,
                      # cancer_male,cancer_male_duration,
                      # cancer_female, cancer_female_duration,
                 SEX,AGE_CAT, REGION,
                 HDL,LDL, TG, TCHOLE,
                 FBG, OBE, LIVER, BP,WC, DRK, PROTEURIA, HB, eGFR,
                 SMK, MET_CON,
                 PA, CCI, T2DM_MED, HPTN_MED, DLPM_MED,
                 WC_burden_total, HDL_burden_total, OBE_burden_total, 
                 PA_burden_total, SMK_burden_total, DRK_burden_total,
                 TG_burden_total, BP_burden_total,
                 METS_burden_total,
                 FMLY_HPRTS_PATIEN_YN, FMLY_APOP_PATIEN_YN, FMLY_HDISE_PATIEN_YN, 
                 FMLY_DIABML_PATIEN_YN, FMLY_CANCER_PATIEN_YN)] %>% names()
    }
  }
  else if (cohort=='mace'){
    mace_except_id <- df[difftime(hf_dt, HME_DT, units='days')<=365 | 
                           difftime(mace_angina_dt, HME_DT, units='days')<=365 |
                           difftime(mace_dt, HME_DT, units='days')<=365 |
                           difftime(mace_mi_dt, HME_DT, units='days')<=365 |
                           difftime(mace_stroke_dt, HME_DT, units='days')<=365 |
                           difftime(revas_dt, HME_DT, units='days')<=365, unique(PERSON_ID)]
    
    print(paste0("No. of Exclusion for MACE : ", length(mace_except_id)))
    df <- df[!PERSON_ID %in% mace_except_id]
    df[, `:=` (
      mi_yn = ifelse(is.na(mace_mi_dt),0,1),
      stroke_yn = ifelse(is.na(mace_stroke_dt),0,1),
      cvd_death_yn = ifelse(is.na(mace_dt),0,1),
      angina_yn = ifelse(is.na(mace_angina_dt),0,1),
      hf_yn = ifelse(is.na(hf_dt),0,1),
      revas_yn = ifelse(is.na(revas_dt),0,1)
    )][,`:=`(
      mace_yn = pmax(mi_yn, stroke_yn, cvd_death_yn, na.rm=T),
      mi_duration = ifelse(mi_yn==1,
                           difftime(mi_dt, HME_DT, units='days'),
                           difftime(enddate,HME_DT, units='days')) %>% as.numeric,
      stroke_duration = ifelse(mi_yn==1,
                           difftime(mi_dt, HME_DT, units='days'),
                           difftime(enddate,HME_DT, units='days')) %>% as.numeric,
      mace_dt = pmin(mace_mi_dt, mace_stroke_dt, mace_dt, na.rm=T)
    )][,mace_duration:= ifelse(mace_yn==1,
                               difftime(mace_dt, HME_DT, units='days'),
                               difftime(enddate, HME_DT, units='days')) %>% as.numeric
       ][,`:=`(
         mace_plus_dt = pmin(mace_mi_dt, mace_stroke_dt, mace_dt, hf_dt, mace_angina_dt, revas_dt, na.rm=T),
         mace_plus_yn=pmax(mace_yn, angina_yn, hf_yn, revas_yn))
         ][,mace_plus_duration:= ifelse(mace_plus_yn ==1,
                                        difftime(mace_plus_dt,HME_DT, units='days'),
                                        difftime(enddate, HME_DT, units='days')) %>% as.numeric]
    target <- df[,.(mace_yn, mace_duration,mace_plus_yn, mace_plus_duration, 
                    mi_yn, stroke_yn, mi_duration, stroke_duration,
               SEX,AGE_CAT, REGION,HDL,LDL, TG, TOT_CHOLE,
               FBG, OBE, LIVER, BP,WC, DRK, PROTEURIA, HB, eGFR,
               SMK, PA, CCI, T2DM_MED, HPTN_MED, DLPM_MED,
               WC_burden_total, HDL_burden_total, OBE_burden_total, 
               PA_burden_total, SMK_burden_total, DRK_burden_total,
               TG_burden_total, BP_burden_total, METS_burden_total,
               FMLY_HPRTS_PATIEN_YN, FMLY_APOP_PATIEN_YN, FMLY_HDISE_PATIEN_YN, 
               FMLY_DIABML_PATIEN_YN, FMLY_CANCER_PATIEN_YN)] %>% names()
  }
  return(df[,..target])
}
# hra_pre <- createCohort(cohort='ha',ver=NULL, cond=1)
hra_dt <- createCohort(cohort='ha',ver=version1_id, cond=1)
hra_dt2 <- createCohort(cohort='ha',ver=version1_id, cond=2)
hra_dt3 <- createCohort(cohort='ha',ver=version1_id, cond=3)
mace_dt <- createCohort(cohorts='mace',ver=version1_id, cond=1)


