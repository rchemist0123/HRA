dt_death <- createCohort(target='death',washout=NULL)

hsc_death <- HSC$new(target='death',baseDir='~/working/mortality', inputDir='sample')
tt_death <- hsc_death$preprocessing(dt_death,features = c(adv,'G1E_CRTN'))
death_fit <- hsc_death$train_model(train = tt_death$train, features = adv, strata = 'SEX', polynomial=T)

# death_fit2 <- hsc_death$train_model(train = tt_death$train, features = adv, strata = 'SEX')
death_male_perf <- hsc_death$perfTest(fit = death_fit$male, test = tt_death$test[SEX==1]) 
death_female_perf <- hsc_death$perfTest(fit = death_fit$female, test = tt_death$test[SEX==2])
rbindlist(list(male = death_male_perf, female = death_female_perf), idcol = T)

hsc_death$generateSample(data = rbindlist(tt_death))
hsc_death$getHealthScore(fit = death_fit, test=tt_death$test)

setwd('~/working/mortality/output/')
temp_female <- lapply(list.files('~/working/mortality/output/', pattern = 'death_female_'),fread);
temp_male <- lapply(list.files('~/working/mortality/output/', pattern = 'death_male'),fread)
temp_female[1:5]
temp_merge <- rbind(rbindlist(temp_male[1:5]),rbindlist(temp_female[1:5]))
temp_merge
temp_merge[,sex:=rep(c(1,2),each=10000)]
temp_merge[,age:=rep(rep(seq(30,70,10),each=2000),2)]
fwrite(temp_merge,'~/data_out/data_out/death_score_2000.csv')




hsc_death$generateAllSample(data=rbindlist(tt_death),n = 10000)
hsc_death$getHealthScore(fit = death_fit, test=tt_death$test, type='all')
# coef
cbind(coef(death_fit$female$finalModel),
      coef(death_fit$male$finalModel))

# export
setwd('~/working/mortality/output/')
temp_female <- lapply(list.files('~/working/mortality/output/', pattern = 'death_female_all_10000'),fread);
temp_male <- lapply(list.files('~/working/mortality/output/', pattern = 'death_male_all_10000'),fread)
temp_merge <- rbind(rbindlist(temp_male), rbindlist(temp_female))
temp_merge[,sex:=rep(c(1,2),each=10000)]
temp_merge[,age:=rep(rep(seq(35,75,5),each=1000),2)]
temp_merge
fwrite(temp_merge,'~/data_out/data_out/death_score_10000.csv')
