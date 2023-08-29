dt_htn <- createCohort(target='htn',washout=12)

hsc_htn <- HSC$new(target='htn', baseDir='~/working/mortality', inputDir='sample')

tt_htn <- hsc_htn$preprocessing(dt_htn,features = adv)
htn_fit <- hsc_htn$train_model(train = tt_htn$train, features = adv, strata = 'SEX',polynomial = T)
# htn_fit2 <- hsc_htn$train_model(train = tt_htn$train, features = adv, strata = 'SEX',preprocess='range')
htn_male_perf <- hsc_htn$perfTest(fit = htn_fit$male, test = tt_htn$test[SEX==1],cutoff=0.5) 
htn_female_perf <- hsc_htn$perfTest(fit = htn_fit$female, test = tt_htn$test[SEX==2], cutoff=0.5)

rbindlist(list(male = htn_male_perf, female = htn_female_perf), idcol = T)
# htn_male_perf2 <- hsc_htn$perfTest(fit = htn_fit2$male, test = tt_htn$test[SEX==1]) # 0.8200 0.8203
# htn_female_perf2 <- hsc_htn$perfTest(fit = htn_fit2$female, test = tt_htn$test[SEX==2]) #0.8265 0.8260

hsc_htn$generateSample(data = rbindlist(tt_htn))
hsc_htn$getHealthScore(fit = htn_fit, test=tt_htn$test)

setwd('~/working/mortality/output/')
temp_female <- lapply(list.files('~/working/mortality/output/', pattern = 'htn_female_'),fread);
temp_male <- lapply(list.files('~/working/mortality/output/', pattern = 'htn_male'),fread)
temp_female[1:5]
temp_merge <- rbind(rbindlist(temp_male[1:5]),rbindlist(temp_female[1:5]))
temp_merge
temp_merge[,sex:=rep(c(1,2),each=10000)]
temp_merge[,age:=rep(rep(seq(30,70,10),each=2000),2)]
fwrite(temp_merge,'~/data_out/data_out/htn_score_2000.csv')



hsc_htn$generateAllSample(data=rbindlist(tt_htn),n = 10000)
hsc_htn$getHealthScore(fit = htn_fit, test=tt_htn$test, type='all')
# coef
cbind(coef(htn_fit$female$finalModel),
      coef(htn_fit$male$finalModel))



# export
setwd('~/working/mortality/output/')
temp_female <- lapply(list.files('~/working/mortality/output/', pattern = 'htn_female_all_10000'),fread);
temp_male <- lapply(list.files('~/working/mortality/output/', pattern = 'htn_male_all_10000'),fread)
temp_merge <- rbind(rbindlist(temp_male), rbindlist(temp_female))
temp_merge[,sex:=rep(c(1,2),each=10000)]
temp_merge[,age:=rep(rep(seq(35,75,5),each=1000),2)]
temp_merge
fwrite(temp_merge,'~/data_out/data_out/htn_score_10000.csv')


