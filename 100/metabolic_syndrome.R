dt_metsyn <- createCohort(target='metsyn',washout=12)

hsc_metsyn <- HSC$new(target='metsyn', baseDir='~/working/mortality', inputDir='sample')
tt_metsyn <- hsc_metsyn$preprocessing(dt_metsyn,features = adv)
metsyn_fit <- hsc_metsyn$train_model(train = tt_metsyn$train, features = adv, strata = 'SEX', polynomial = T)
# metsyn_fit2 <- hsc_metsyn$train_model(train = tt_metsyn$train, features = adv, strata = 'SEX',preprocess='range')
metsyn_male_perf <- hsc_metsyn$perfTest(fit = metsyn_fit$male, test = tt_metsyn$test[SEX==1],cutoff=0.5) 
metsyn_female_perf <- hsc_metsyn$perfTest(fit = metsyn_fit$female, test = tt_metsyn$test[SEX==2], cutoff=0.5)

rbindlist(list(male = metsyn_male_perf, female = metsyn_female_perf), idcol = T)
# metsyn_male_perf2 <- hsc_metsyn$perfTest(fit = metsyn_fit2$male, test = tt_metsyn$test[SEX==1]) # 0.8200 0.8203
# metsyn_female_perf2 <- hsc_metsyn$perfTest(fit = metsyn_fit2$female, test = tt_metsyn$test[SEX==2]) #0.8265 0.8260

hsc_metsyn$generateSample(data = rbindlist(tt_metsyn))
hsc_metsyn$getHealthScore(fit = metsyn_fit, test=tt_metsyn$test)

setwd('~/working/mortality/output/')
temp_female <- lapply(list.files('~/working/mortality/output/', pattern = 'metsyn_female_'),fread);
temp_male <- lapply(list.files('~/working/mortality/output/', pattern = 'metsyn_male'),fread)
temp_female[1:5]
temp_merge <- rbind(rbindlist(temp_male[1:5]),rbindlist(temp_female[1:5]))
temp_merge
temp_merge[,sex:=rep(c(1,2),each=10000)]
temp_merge[,age:=rep(rep(seq(30,70,10),each=2000),2)]
fwrite(temp_merge,'~/data_out/data_out/metsyn_score_2000.csv')


hsc_metsyn$generateAllSample(data=rbindlist(tt_metsyn),n = 10000)
hsc_metsyn$getHealthScore(fit = metsyn_fit, test=tt_metsyn$test, type='all')
# coef
cbind(coef(metsyn_fit$female$finalModel),
      coef(metsyn_fit$male$finalModel))

# export
setwd('~/working/mortality/output/')
temp_female <- lapply(list.files('~/working/mortality/output/', pattern = 'metsyn_female_all_10000'),fread);
temp_male <- lapply(list.files('~/working/mortality/output/', pattern = 'metsyn_male_all_10000'),fread)
temp_merge <- rbind(rbindlist(temp_male), rbindlist(temp_female))
temp_merge[,sex:=rep(c(1,2),each=10000)]
temp_merge[,age:=rep(rep(seq(35,75,5),each=1000),2)]
temp_merge
fwrite(temp_merge,'~/data_out/data_out/metsyn_score_10000.csv')
