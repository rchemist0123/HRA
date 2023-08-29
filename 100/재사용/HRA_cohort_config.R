require(data.table)
require(dplyr)

setwd("~/data_source/user_data/")
df_raw = fread("dataset_0912_.csv")
bnc = fread("CopyOfnsc2_bnc_db.csv")
bnd = fread("CopyOfnsc2_bnd_db.csv")
m20 = fread("CopyOfnsc2_m20_db.csv")
m20_1619 = fread("CopyOfnsc2_m20_db_1619.csv")
m20_all = rbindlist(list(m20,m20_1619))
rm(m20, m20_1619)
cox = fread("dataset_for_cox.csv")
gj = fread("CopyOfnsc2_g1e_0915_db.csv")

df_raw[cox, on=.(RN_INDI, age), GJ_YYYY := i.EXMD_BZ_YYYY]

death_id = bnd[!is.na(DTH_YYYYMM),unique(RN_INDI)] ;length(death_id)
death_df = bnd[!is.na(DTH_YYYYMM),.(RN_INDI, DTH_YYYYMM, COD1, COD2)]
df_first = df_raw[df_raw[,.I[1L],by=RN_INDI]$V1]

df_disease = df_first[death_df,on=.(RN_INDI), DTH_YYYYMM := i.DTH_YYYYMM]

cancer_male = c('C33','C34','C16','C18','C19','C20','C61','C22')
cancer_female = c('C50','D05','C18','C19','C20','C16','C33','C34','C22')
ICD_CODE = list(
  heart = c('I20','I21','I22','I23','I24','I25'),
  brain = c('I6'),
  # cancer_male = c('C33','C34','C16','C18','C19','C20','C61','C22'),
  # cancer_female = c('C50','D05','C18','C19','C20','C16','C33','C34','C22'),
  cancer =union(cancer_male, cancer_female),
  HTN = c('I10','I11','I12','I13','I14','I15'),
  DM = c('R81','E10','E11','E12','E13','E14')
)

require(scales)
for(d in names(ICD_CODE)){
  # 진단명으로 질병 찾기
  if(d == "brain"){
    temp = m20_all[substr(SICK_SYM1,1,2) == "I6" |
              substr(SICK_SYM2,1,2) == "I6"]
  } else {
    temp = m20_all[substr(SICK_SYM1,1,3) %in% ICD_CODE[[d]] |
              substr(SICK_SYM2,1,3) %in% ICD_CODE[[d]]]
  }
  # 처음 진단받은 연도 찾기
  temp2 = temp[temp[,.I[which.min(STD_YYYY)], by=RN_INDI]$V1, .(RN_INDI, STD_YYYY)]
  year_name = paste0(d,"_year")

  # 암: 남성 여성 따로
  # if(d == "cancer"){
  #   # if (substring(d,8) == "male") df_dz = copy(df_disease[SEX==1]) 
  #   # else df_dz = copy(df_disease[SEX==2])
  # } else {
  # }
    df_dz = copy(df_disease)
  # 최초 검진 이전에 이미 해당 질병을 진단받은 경우  
    df_dz[temp2, on=.(RN_INDI), eval(year_name) := i.STD_YYYY]
    except_id = df_dz[get(year_name) <= GJ_YYYY, unique(RN_INDI)]
    cat("##########",toupper(d), "before GJ:", comma(length(except_id)),"##########\n")
    df_dz2 = df_dz[!RN_INDI %in% except_id]
  
  
  # 고혈압, 당뇨 첫 검진에서 제외되는 숫자 제외되는 숫자 파악
  if(d == "HTN"){
    df_htn_year = df_raw[G1E_BP_SYS>=140 | G1E_BP_DIA>=90, .(HTN_GJ_YYYY = first(GJ_YYYY)),RN_INDI]
    df_dz2[df_htn_year, on=.(RN_INDI), HTN_GJ_YYYY := i.HTN_GJ_YYYY]
    htn_except_id = df_dz2[HTN_GJ_YYYY == GJ_YYYY, RN_INDI]; 
    df_dz2 = df_dz2[!RN_INDI %in% htn_except_id]
    df_dz2[,eval(year_name) := pmin(HTN_GJ_YYYY, get(year_name),na.rm=T)]
    cat("           No of BP exclusion:", length(htn_except_id) |> comma(),"\n")
  } else if (d == "DM"){
    df_dm_year = df_raw[G1E_FBS >= 126, .(DM_GJ_YYYY = first(GJ_YYYY)),RN_INDI]
    df_dz2[df_dm_year, on=.(RN_INDI), DM_GJ_YYYY := DM_GJ_YYYY]
    htn_except_id = df_dz2[DM_GJ_YYYY == GJ_YYYY, RN_INDI]; 
    dm_except_id = df_dz2[G1E_FBS >= 126, unique(RN_INDI)]
    df_dz2 = df_dz2[!RN_INDI %in% dm_except_id]
    df_dz2[,eval(year_name) := pmin(DM_GJ_YYYY, get(year_name),na.rm=T)]
    cat("           No of FBS exclusion:", length(dm_except_id) |> comma(),"\n")
  }
  # n년 이후의 질병 여부 데이터셋 만들기
  # 10년 예측의 경우 2009년 검진 받은 사람들만
  for(i in c(3,5,10)){
    varname = paste0(d,"_",i,"_year")
    if(i == 10) df_dz3 = copy(df_dz2[GJ_YYYY==2009])
    else df_dz3 = copy(df_dz2)
    df_dz3[,eval(varname) := fcase(get(year_name)-GJ_YYYY <= i, 1,
                                   default = 0) |> as.factor()]
    
  # 관찰기간 중 사망자 제외  
    death_before_dz_id = df_dz3[(DTH_YYYYMM %/% 100) %between% list(GJ_YYYY, GJ_YYYY+i),RN_INDI]
    cat("No of death during",i, "year:", length(death_before_dz_id) |> comma(),"\n")
    df_dz3 = df_dz3[!RN_INDI %in% death_before_dz_id]
    
    # disease_before_gj_id = df_dz3[get(year_name) <= GJ_YYYY, RN_INDI]
    # cat("Disease before GJ:", length(disease_before_gj_id) |> comma(),"\n")
    # df_dz3 = df_dz3[!RN_INDI %in% disease_before_gj_id]
    df_name = paste0("df_",d,"_",i,"y")
    assign(df_name, df_dz3)
    # save to csv
    if(!dir.exists("~/HRA/data/")) dir.create("~/HRA/data/")
    fwrite(df_dz3, paste0("~/HRA/data/",df_name,".csv"))
    cat("Number of",d,"patients with", i,"year: ", nrow(df_dz3) |> comma(),"\n")
    cat("-----------------------------------------------------------\n")
  }
  cat("                                                           \n")
}

dm_htn_list = list.files("~/HRA/data/", pattern = "_10")
for(i in dm_htn_list){
  path = "~/HRA/data/"
  d = fread(paste0(path,i))
  target_name = paste0(str_extract(i, '(?<=df_).*?(?=y)'),"_year")
  cat('###################',i,'###################\n')
  cat('------------------All-----------------\n')
  print(d[,.N,by=target_name][,.(N, pct = round(N/sum(N)*100,2))])
  cat('------------------Male-----------------\n')
  
  print(d[SEX==1,.N,by=target_name][,.(N, pct = round(N/sum(N)*100,2))])
  cat('------------------Female-----------------\n')
  print(d[SEX==2,.N,by=target_name][,.(N, pct = round(N/sum(N)*100,2))])
  cat('                                         \n ')
}

# Death
df_first[death_df, on=.(RN_INDI), `:=`(DTH_YM = i.DTH_YYYYMM, COD1 = i.COD1, COD2 = i.COD2)]

# GJ year == Death year
death_gj_same_year_id = df_first[GJ_YYYY== as.numeric(substr(DTH_YM,1,4)),unique(RN_INDI)]
df_death_temp = copy(df_first[!RN_INDI %in% death_gj_same_year_id])

# Death by accident
death_accident_id = df_death_temp[COD1 %like% "S|T" | COD2 %like% "S|T",unique(RN_INDI)]
df_death = copy(df_death_temp[!RN_INDI %in% death_accident_id])

for(i in c(1,3,5,10)){
  varname = paste0("death_",i,"_year")
  if(i==10) df_death_tmp = copy(df_death[is.na(DTH_YYYYMM) <= 201912 & GJ_YYYY==2009])
  else df_death_tmp = copy(df_death[is.na(DTH_YYYYMM) | DTH_YYYYMM <= 201912])
  df_death_tmp[,eval(varname) := fcase(as.numeric(substr(DTH_YM,1,4)) - GJ_YYYY <= i,1,
                                   default=0) |> as.factor()]

  df_name = paste0("df_death_",i,"y")
  assign(df_name, df_death_tmp)
  fwrite(df_death_tmp, paste0("~/HRA/data/",df_name,".csv"))
  cat("Number of death with", i,"year: ", nrow(df_death_tmp[get(varname)==1]) |> comma(),"\n")
  cat("-----------------------------------------------------------\n")
}

df_death_10y[,.N,death_10_year]
df_death_3y[,.N,death_3_year]
df_death_5y[,.N,death_5_year]
df_death_10y[,.N,death_10_year]

df_death_3y[,.(DTH_YYYYMM)]
df_cancer_female_1y[,.N]
df_cancer_male_1y[,.N]

df_death[(DTH_YM %/% 100) - GJ_YYYY < 10,.(GJ_YYYY, DTH_YYYYMM)]
df_death[,summary(DTH_YYYYMM)]
df_death[order(-DTH_YYYYMM)]
