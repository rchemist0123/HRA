dt_t2dm <- createCohort(target='t2dm',washout=12) # cohort configuration

hsc_t2dm <- HSC$new(target='t2dm', baseDir='~/working/mortality', inputDir='sample')
tt_t2dm <- hsc_t2dm$preprocessing(dt_t2dm,features = adv)

t2dm_fit <- hsc_t2dm$train_model(train = tt_t2dm$train, features = adv, strata = 'SEX', polynomial = F)

t2dm_male_perf <- hsc_t2dm$perfTest(fit = t2dm_fit$male, test = tt_t2dm$test[SEX==1],cutoff=.5) 
t2dm_female_perf <- hsc_t2dm$perfTest(fit = t2dm_fit$female, test = tt_t2dm$test[SEX==2], cutoff=0.5)
rbindlist(list(male = t2dm_male_perf, female = t2dm_female_perf), idcol = T)

# coef
cbind(coef(t2dm_fit$female$finalModel),
      coef(t2dm_fit$male$finalModel))
# t2dm_male_perf2 <- hsc_t2dm$perfTest(fit = t2dm_fit2$male, test = tt_t2dm$test[SEX==1]) # 0.8200 0.8203
# t2dm_female_perf2 <- hsc_t2dm$perfTest(fit = t2dm_fit2$female, test = tt_t2dm$test[SEX==2]) #0.8265 0.8260
hsc_t2dm$generateSample(data = rbindlist(tt_t2dm))
hsc_t2dm$getHealthScore(fit = t2dm_fit, test=tt_t2dm$test)

setwd('~/working/mortality/output/')
temp_female <- lapply(list.files('~/working/mortality/output/', pattern = 't2dm_female_'),fread);
temp_male <- lapply(list.files('~/working/mortality/output/', pattern = 't2dm_male'),fread)
temp_female[1:5]
temp_merge <- rbind(rbindlist(temp_male[1:5]),rbindlist(temp_female[1:5]))
temp_merge
temp_merge[,sex:=rep(c(1,2),each=10000)]
temp_merge[,age:=rep(rep(seq(30,70,10),each=2000),2)]
temp_male[2]
fwrite(temp_merge,'~/data_out/data_out/t2dm_score_2000.csv')



hsc_t2dm$generateAllSample(data=rbindlist(tt_t2dm),n = 10000)
hsc_t2dm$getHealthScore(fit = t2dm_fit, test=tt_t2dm$test, type='all')

# export
setwd('~/working/mortality/output/')
temp_female <- lapply(list.files('~/working/mortality/output/', pattern = 't2dm_female_all_10000'),fread);
temp_male <- lapply(list.files('~/working/mortality/output/', pattern = 't2dm_male_all_10000'),fread)

temp_merge <- rbind(rbindlist(temp_male),rbindlist(temp_female))
temp_merge[,sex:=rep(c(1,2),each=10000)]
temp_merge[,age:=rep(rep(seq(35,75,5),each=1000),2)]
fwrite(temp_merge,'~/data_out/data_out/t2dm_score_10000.csv')

