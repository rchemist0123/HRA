dt_mace <- createCohort(target='mace', washout= 6)

hsc_mace <- HSC$new(target='mace', baseDir='~/working/mortality', inputDir='sample')
tt_mace <- hsc_mace$preprocessing(dt_mace,features = adv)

mace_fit <- hsc_mace$train_model(train = tt_mace$train, features = adv, strata = 'SEX', polynomial = T)

for(i in seq(0.1,0.5,0.1)){
  mace_male_perf <- hsc_mace$perfTest(fit = mace_fit$male, test = tt_mace$test[SEX==1], cutoff=i)
  mace_female_perf <- hsc_mace$perfTest(fit = mace_fit$female, test = tt_mace$test[SEX==2],cutoff=i)
  message('cutoff: ',i)
  print(rbindlist(list(mace_male_perf, mace_female_perf),idcol = T))
}


mace_male_perf <- hsc_mace$perfTest(fit = mace_fit$male, test = tt_mace$test[SEX==1], cutoff=0.2)
mace_female_perf <- hsc_mace$perfTest(fit = mace_fit$female, test = tt_mace$test[SEX==2],cutoff=0.2)
rbindlist(list(male = mace_male_perf, female = mace_female_perf), idcol = T)

hsc_mace$generateSample(data = rbindlist(tt_mace))
hsc_mace$getHealthScore(fit = mace_fit, test=tt_mace$test)

setwd('~/working/mortality/output/')
temp_female <- lapply(list.files('~/working/mortality/output/', pattern = 'mace_female_'),fread);
temp_male <- lapply(list.files('~/working/mortality/output/', pattern = 'mace_male'),fread)
temp_female[1:5]
temp_merge <- rbind(rbindlist(temp_male[1:5]),rbindlist(temp_female[1:5]))
temp_merge
temp_merge[,sex:=rep(c(1,2),each=10000)]
temp_merge[,age:=rep(rep(seq(30,70,10),each=2000),2)]
fwrite(temp_merge,'~/data_out/data_out/mace_score_2000.csv')



hsc_mace$generateAllSample(data=rbindlist(tt_mace),n = 10000)
hsc_mace$getHealthScore(fit = mace_fit, test=tt_mace$test, type='all')
# coef
cbind(coef(mace_fit$female$finalModel),
      coef(mace_fit$male$finalModel))

# export
setwd('~/working/mortality/output/')
temp_female <- lapply(list.files('~/working/mortality/output/', pattern = 'mace_female_all_10000'),fread);
temp_male <- lapply(list.files('~/working/mortality/output/', pattern = 'mace_male_all_10000'),fread)
temp_merge <- rbind(rbindlist(temp_male), rbindlist(temp_female))
temp_merge[,sex:=rep(c(1,2),each=10000)]
temp_merge[,age:=rep(rep(seq(35,75,5),each=1000),2)]
temp_merge
fwrite(temp_merge,'~/data_out/data_out/mace_score_10000.csv')


# merge all 10000
setwd('~/data_out/data_out')
list.files('~/data_out/data_out/', pattern = '10000')
temp <- lapply(list.files('~/data_out/data_out/', pattern = '10000'),fread);
temp
temp2 <- rbindlist(temp)
temp2[,target:=rep(c('cancer','death','htn','illness','mace','metsyn','t2dm'), each=20000)]
fwrite(temp2, '~/data_out/data_out/score_all_10000.csv')
