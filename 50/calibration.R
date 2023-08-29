

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
reliability_diagram = function(obs_rep, data_ls, stat_type,title_ls, color_ls, title) {
  
  data_ls_len = length(data_ls)
  
  ### create bin averages
  for (data_i in 1:data_ls_len){
    temp_res = reliability_datapts(obs_rep, data_ls[[data_i]], bins = 10, stat_type = stat_type)
    assign(paste("recal_bins", data_i, sep=""),temp_res)
  }
  
  for (data_i in 1:data_ls_len){
    temp_res = melt(get(paste('recal_bins', data_i, sep='')), id="V2")
    temp_res[, "variable"] <- paste("Vol.x", data_i, sep='')
    assign(paste('melt', data_i, sep=''), temp_res)
  }
  
  data = melt1
  if (data_ls_len > 1){
    for (data_i in 2:data_ls_len){
      data = rbind(data, get(paste('melt', data_i, sep='')))
    }
  }
  
  line_plot = ggplot(data, aes(x=V2,  y=value, color=variable)) +  
    geom_point()+ 
    geom_line() +
    scale_color_manual(labels = title_ls,
                       values = color_ls) +
    guides(color=guide_legend(" ")) + 
    ggtitle(paste(stat_type, 'Statistics')) +
    xlab("Mean Predicted") + ylab("Mean Observed") + 
    xlim(0, 1) + ylim(-0.05, 1.05)
  
  line_plot = line_plot + geom_segment(aes(x=0, xend=1, y=0, yend=1), color="black",
                                       linetype="dashed", size=1) 
  #remove background
  line_plot = line_plot + theme_bw()
  line_plot = line_plot + theme(legend.position="bottom")
  
  ## add data points
  for (data_i in 1:data_ls_len){
    temp_obs = obs_rep
    temp_obs[temp_obs==0] = -0.005 * data_i
    temp_obs[temp_obs==1] = 1 + 0.005 * data_i
    assign(paste('obs_rep_offset', data_i, sep=''), data.frame(cbind(data_ls[[data_i]], temp_obs)))
  }
  
  for (data_i in 1:data_ls_len){
    temp_res = melt(get(paste('obs_rep_offset', data_i, sep='')), id="temp_obs")
    temp_res[, "variable"] <- paste("Vol.x", data_i, sep='')
    assign(paste('obs_rep_offset', data_i, sep=''), temp_res)
  }
  
  data_points = obs_rep_offset1
  if (data_ls_len > 1){
    for (data_i in 2:data_ls_len){
      data_points = rbind(data_points, get(paste('obs_rep_offset', data_i, sep='')))
    }
  }
  
  line_plot = line_plot + 
    geom_point(data = data_points, aes(x=data_points$value,  y=data_points$temp_obs, color=variable),
               alpha=0.2) + 
    return(line_plot)
  # ggsave(title, device = 'png', width = 6, height = 6)
}
reliability_datapts <- function(obs, pred, bins=10, stat_type ='H') {
  min.pred <- min(pred)
  max.pred <- max(pred)
  min.max.diff <- max.pred - min.pred
  
  if (stat_type == 'H'){
    mtx = data.table(obs=as.numeric(obs), pred=pred)
    mtx = mtx[order(mtx$pred),]
    res = data.frame(V1= numeric(0), V2 = numeric(0))
    split_mtx = split(mtx, cut(mtx$pred, seq(0,1,1/10), include.lowest=TRUE))
    for (i in 1:length(split_mtx)){
      col_mean = colMeans(split_mtx[[i]])
      if (sum(is.na(col_mean)) > 0) {
        next
      }
      res[i,] = col_mean
    }
    
  }else{
    ## C statistics, same number of instances in each bin
    mtx = data.table(obs=as.numeric(obs), pred=pred)
    mtx = mtx[order(mtx$pred),]
    n <- length(pred)/10
    nr <- nrow(mtx)
    split_mtx = split(mtx, rep(1:ceiling(nr/n), each=n, length.out=nr))
    res = data.frame(V1= numeric(0), V2 = numeric(0))
    for (i in 1:length(split_mtx)){
      res[i,] = colMeans(split_mtx[[i]])
    }
  }
  
  return(res)
}


auc(test_dt$ha~iso.recal_pred)
auc(test_dt$ha~platt.recal_pred)

set.seed(2022)
train_id <- createDataPartition(dt_ha2$ha, p=.5)

train <- dt_ha2[train_id$Resample1]
rest <- dt_ha2[-train_id$Resample1]

valid_id <- createDataPartition(rest$ha, p=.5)
valid <- rest[valid_id$Resample1]
test <- rest[-valid_id$Resample1]

disease2
temp_dt <- haCohort(major = disease, illness_def = 1)
temp_dt2 <- haCohort(major = disease, illness_def = 2)
temp_dt[,.N,by=ha]
temp_dt2[,.N,by=ha]
temp <- preprocessing(temp_dt,
                      features = basic_features,
                      target='ha',
                      na='impute',
                      valid=T)

tempfit <- train_model(train_data = temp$tr,
                       model = 'glm',
                       features = basic_features,
                       target = 'ha',
                       preprocess = 'range')

test_performance(fit = tempfit,
                 target = 'ha',
                 test = temp$te)
plot_performance(fit=tempfit, test = temp$te, target = 'ha', type='hist')
pred <- predict(tempfit, temp$te, type='prob')[,2]

platt_pred <- lr_platt_recal_func(tempfit, as.data.frame(temp$va),
                    pred, 
                    y='ha')
iso_pred <- lr_iso_recal_func(tempfit, 
                  validate_data = temp$va, 
                  test_res=pred, y='ha')

hist_dt <- data.table(id=1:length(pred),
                      raw = (1-pred)*1000, 
                      platt = (1-platt_pred)*1000, 
                      isotonic = (1-iso_pred)*1000)


# histogram
melt(hist_dt,id.vars = 'id') %>% 
  ggplot(aes(x=value, fill=variable)) + 
  geom_histogram(bins=10000, binwidth = 20)+
  facet_grid(variable ~., labeller = as_labeller(c('raw'='Raw',
                                          'platt' = "Platt's scale",
                                          'isotonic' ='Isotonic'))) +
  theme_bw()+
  theme(legend.position = 'none') 


## recalibration plot
reliability_diagram(as.vector(temp$te$ha),
                    list(as.vector(pred),
                         as.vector(platt_pred),
                         as.vector(iso_pred)),
                    'H',
                    c('LR Raw', 'LR Platt scaling',
                      'LR Isotonic'),
                    c('red','darkgreen','blue'),
                    'LR Platt Iso H')

summary((1-iso_pred)*1000)
summary((1-platt_pred)*1000)
step(tempfit$finalModel)
platt_dt <- data.table(x=(1-platt_pred)*1000)
iso_dt <- data.table(x=(1-iso_pred)*1000)

ggplot(platt_dt,aes(x=x))+
  geom_histogram(bins=10000,
                 color='black',
                 fill='lightblue3',
                 binwidth = 20)+
  theme_bw()
ggplot(iso_dt,aes(x=x))+
  geom_histogram(bins=10000,
                 color='black',
                 fill='lightblue3',
                 binwidth = 20)+
  theme_bw()
summary(platt_dt)
sigmoid <- function(x){
  1/(1+exp(-x))
}
pred
plot(pred, sigmoid(pred))
plot(pred, iso_pred)
plot(pred, platt_pred)
