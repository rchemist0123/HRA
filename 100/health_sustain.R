dt_illness <- createCohort(target='illness', washout= 12)

hsc_illness <- HSC$new(target='illness', baseDir='~/working/mortality', inputDir='sample')
tt_illness <- hsc_illness$preprocessing(dt_illness,features = adv)

illness_fit <- hsc_illness$train_model(train = tt_illness$train, features = adv, strata = 'SEX', polynomial = T)

illness_male_perf <- hsc_illness$perfTest(fit = illness_fit$male, test = tt_illness$test[SEX==1], cutoff=0.5)
illness_female_perf <- hsc_illness$perfTest(fit = illness_fit$female, test = tt_illness$test[SEX==2],cutoff=0.5)

rbindlist(list(male = illness_male_perf, female = illness_female_perf), idcol = T)

hsc_illness$generateSample(data = rbindlist(tt_illness))
hsc_illness$getHealthScore(fit = illness_fit, test=tt_illness$test)

setwd('~/working/mortality/output/')
temp_female <- lapply(list.files('~/working/mortality/output/', pattern = 'illness_female_'),fread);
temp_male <- lapply(list.files('~/working/mortality/output/', pattern = 'illness_male'),fread)
temp_female[1:5]
temp_merge <- rbind(rbindlist(temp_male[1:5]),rbindlist(temp_female[1:5]))
temp_merge
temp_merge[,sex:=rep(c(1,2),each=10000)]
temp_merge[,age:=rep(rep(seq(30,70,10),each=2000),2)]
fwrite(temp_merge,'~/data_out/data_out/illness_score_2000.csv')




hsc_illness$generateAllSample(data=rbindlist(tt_illness),n = 10000)
hsc_illness$getHealthScore(fit = illness_fit, test=tt_illness$test, type='all')
# coef
cbind(coef(illness_fit$female$finalModel),
      coef(illness_fit$male$finalModel))

# export
setwd('~/working/mortality/output/')
temp_female <- lapply(list.files('~/working/mortality/output/', pattern = 'illness_female_all_10000'),fread);
temp_male <- lapply(list.files('~/working/mortality/output/', pattern = 'illness_male_all_10000'),fread)
temp_merge <- rbind(rbindlist(temp_male), rbindlist(temp_female))
temp_merge[,sex:=rep(c(1,2),each=10000)]
temp_merge[,age:=rep(rep(seq(35,75,5),each=1000),2)]
temp_merge
fwrite(temp_merge,'~/data_out/data_out/illness_score_10000.csv')


