gj <- setDT(read_sas('working/HRA/gj.sas7bdat')) 
gj2 <- setDT(read_sas('working/HRA/gj2.sas7bdat')) 
dim(gj)
names(gj)
names(gj2)
gj[,`:=`(
  WLK30_WEK_FREQ_ID = ifelse(WLK30_WEK_FREQ_ID=="",0, as.numeric(WLK30_WEK_FREQ_ID)-1),
  MOV30_WEK_FREQ_ID = ifelse(MOV30_WEK_FREQ_ID=="",0, as.numeric(MOV30_WEK_FREQ_ID)-1),
  MOV20_WEK_FREQ_ID = ifelse(MOV20_WEK_FREQ_ID=="",0, as.numeric(MOV20_WEK_FREQ_ID)-1)
)]
target <- intersect(names(gj),names(gj2))
gj_merge <- rbindlist(list(gj[,..target],gj2[,..target]))
gj_merge[,summary(.SD)]

jk <- setDT(read_sas('working/HRA/jk.sas7bdat')) 
cci <- setDT(read_sas('working/HRA/cci_list.sas7bdat'))
names(cci) <- c('PERSON_ID','HCHK_YEAR','CCI')



## 1) major illness -------------------------------------
# cardiovascular
mi <- fread('working/HRA/major_illness/mi.csv', sep = ',')
chronic_heart <- fread('working/HRA/major_illness/chronic_heart.csv',sep=',')
peri_vascular <- fread('working/HRA/major_illness/peripheral_vascular.csv',sep=',')
angina <- fread('working/HRA/major_illness/angina.csv',sep=',')


# neurovascular
stroke <- fread('working/HRA/major_illness/stroke.csv',sep=',')

# lung
copd <- fread('working/HRA/major_illness/copd.csv',sep=',')

# renal
renal <- fread('working/HRA/major_illness/renal.csv',sep=',')

# DM
dm <- fread('working/HRA/major_illness/complicated_dm.csv',sep=',')

# cancer
lymphoma <- fread('working/HRA/major_illness/cancer_lymphoma.csv',sep=',')
cancer_meta <- fread('working/HRA/major_illness/cancer_meta.csv',sep=',')
cancer_nonmeta <- fread('working/HRA/major_illness/cancer_nonmeta.csv',sep=',')

# cancer_male
cancer_male <- setDT(read_sas('working/HRA/cancer_male.sas7bdat'))
cancer_female <- setDT(read_sas('working/HRA/cancer_female.sas7bdat'))

# physical : rheumatic, plegia

rheumatic <- fread('working/HRA/major_illness/rheumatic.csv',sep=',')
plegia <- fread('working/HRA/major_illness/plegia.csv',sep=',')
disability <- setDT(read_sas('working/HRA/major_illness/disability.sas7bdat'))


# cognitive
parkinson <- fread('working/HRA/major_illness/parkinson.csv',sep=',')
dementia <- fread('working/HRA/major_illness/dementia.csv',sep=',')
depression <- fread('working/HRA/major_illness/depression.csv',sep=',')


# MACE plus
mace <- fread('working/HRA/MACE/cvd_disease.csv')
revas <- setDT(read_sas('working/HRA/MACE/revasculization.sas7bdat'))
hf <- fread('working/HRA/MACE/heart_failure.csv')
mace_angina <- fread('working/HRA/MACE/angina.csv')

# death within 30days after CVD onset
mace <- mace[difftime(as.Date(as.character(DEATH_DATE),'%Y%m%d'), as.Date(as.character(FIRST_DIAG_DATE),'%Y%m%d'),units='days')<30]
setnames(mace,'FIRST_DIAG_DATE','RECU_FR_DT')

# medication for adherence
t2dm_med <- setDT(read_sas('working/HRA/t2dm_med.sas7bdat'))
hptn_med <- setDT(read_sas('working/HRA/hptn_med.sas7bdat'))
lpdm_med <- setDT(read_sas('working/HRA/dlpm_med.sas7bdat'))

# medication for burden
hptn_med_all <- setDT(read_sas('working/HRA/hptn_med_all.sas7bdat'))
lpdm_med_all <- setDT(read_sas('working/HRA/lpdm_med_all.sas7bdat'))
t2dm_med_all <- setDT(read_sas('working/HRA/t2dm_med_all.sas7bdat'))

lpdm_med_all[,year := substr(RECU_FR_DT,1,4)]
t2dm_med_all[,year := substr(RECU_FR_DT,1,4)]
setorder(hptn_med_all, PERSON_ID, RECU_FR_DT)


extract_medID <- function(x){
  y <- hra_gj[hra_gj[,.I[1L],by=PERSON_ID]$V1, .(PERSON_ID,HME_DT)]
  x[y, on=.(PERSON_ID), HME_DT := i.HME_DT]
  temp <- x[as.numeric(difftime(as.Date(HME_DT,'%Y%m%d'), as.Date(first(RECU_FR_DT), '%Y%m%d'), units='days'))<730,.(first = as.Date(first(RECU_FR_DT),'%Y%m%d'),
                                                                                                                     last = as.Date(last(RECU_FR_DT),'%Y%m%d'),
                                                                                                                     N= .N,
                                                                                                                     sum=sum(MDCN_EXEC_FREQ)),by=.(PERSON_ID,GNL_NM_CD)]
  temp[,dur := difftime(last, first, units = 'days') %>% as.numeric]
  result <- temp[N>1 & sum/dur>=0.8,unique(PERSON_ID)]
  return(result)
}

t2dm_med_final <- extract_medID(t2dm_med)
hptn_med_final <- extract_medID(hptn_med)
lpdm_med_final <- extract_medID(lpdm_med)

# checkup between 2009-2019 & age between 30 and 74

# id: 2009-2016년 사이 검진 받은 사람 ----------------------------------------------
id <- gj_merge[HCHK_YEAR %between% c(2009,2016) & AGE %between% c(30,75),unique(PERSON_ID)];length(id)
jk[,max_year :=as.numeric(max_year)]

# 검진 이후 2년 자격 유지
gj_merge[,.N,by=PERSON_ID]
jk[gj_merge[gj_merge[PERSON_ID %in% id,.I[1L], by=PERSON_ID]$V1,.(PERSON_ID, HCHK_YEAR)],
   on=.(PERSON_ID), first_gj_year := i.HCHK_YEAR]
id2 <- jk[PERSON_ID %in% id & max_year>=2012,unique(PERSON_ID)];length(id2)
id3 <- jk[PERSON_ID %in% id & (max_year>=as.numeric(first_gj_year)+2),unique(PERSON_ID)];length(id3)
jk[,.(max_year, as.numeric(first_gj_year)+2)]
gj_merge[,min(HCHK_YEAR),by=PERSON_ID]
gj_merge[jk,on=.(PERSON_ID),max_year:=i.max_year]
gj_merge[max_year>=as.numeric(first(HCHK_YEAR))+3,uniqueN(PERSON_ID)]


# cohoft version 1
version1_id <- dcast(hra_gj, PERSON_ID ~ HCHK_YEAR)[,sum:=rowSums(.SD),.SDcol=2:6][sum>=3,PERSON_ID] # 162,123
version2_id <- dcast(hra_gj, PERSON_ID ~ HCHK_YEAR)[`2009`+`2010`+`2011`==3 |
                                                      `2010`+`2011`+`2012`==3|
                                                      `2011`+`2012`+`2013`==3,PERSON_ID] # 80375
version3_id <- dcast(hra_gj,PERSON_ID ~ HCHK_YEAR)[`2009`+`2010`+`2011`==3,PERSON_ID] #67457
version4_id <- dcast(hra_gj, PERSON_ID ~ HCHK_YEAR)[`2009`+`2010`+`2011`+`2012`==4 |
                                                      `2010`+`2011`+`2012` + `2013`==4|
                                                      `2011`+`2012`+`2013` + `2014`==4 |
                                                      `2012` + `2013` + `2014` + `2015`==4, PERSON_ID] # 80375
