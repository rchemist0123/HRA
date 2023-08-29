dt_cancer <- createCohort(target='cancer', washout=6)

hsc_cancer <- HSC$new(target='cancer', baseDir='~/working/mortality', inputDir='sample')
tt_cancer <- hsc_cancer$preprocessing(dt_cancer,features = adv)
cancer_fit <- hsc_cancer$train_model(train = tt_cancer$train, features = adv, strata = 'SEX', polynomial = T)
# cancer_fit2 <- hsc_cancer$train_model(train = tt_cancer$train, features = adv, strata = 'SEX',preprocess='range')
cancer_male_perf <- hsc_cancer$perfTest(fit = cancer_fit$male, test = tt_cancer$test[SEX==1],cutoff=0.5) 
cancer_female_perf <- hsc_cancer$perfTest(fit = cancer_fit$female, test = tt_cancer$test[SEX==2], cutoff=0.5)

rbindlist(list(male = cancer_male_perf, female = cancer_female_perf), idcol = T)

hsc_cancer$generateSample(data = rbindlist(tt_cancer))
hsc_cancer$getHealthScore(fit = cancer_fit, test=tt_cancer$test)

setwd('~/working/mortality/output/')
temp_female <- lapply(list.files('~/working/mortality/output/', pattern = 'cancer_female_'),fread);
temp_male <- lapply(list.files('~/working/mortality/output/', pattern = 'cancer_male'),fread)
temp_female[1:5]
temp_merge <- rbind(rbindlist(temp_male[1:5]),rbindlist(temp_female[1:5]))
temp_merge
temp_merge[,sex:=rep(c(1,2),each=10000)]
temp_merge[,age:=rep(rep(seq(30,70,10),each=2000),2)]
fwrite(temp_merge,'~/data_out/data_out/cancer_score_2000.csv')



hsc_cancer$generateAllSample(data=rbindlist(tt_cancer),n = 10000)
hsc_cancer$getHealthScore(fit = cancer_fit, test=tt_cancer$test, type='all')
# coef
cbind(coef(cancer_fit$female$finalModel),
      coef(cancer_fit$male$finalModel))

# export
setwd('~/working/mortality/output/')
temp_female <- lapply(list.files('~/working/mortality/output/', pattern = 'cancer_female_all_10000'),fread);
temp_male <- lapply(list.files('~/working/mortality/output/', pattern = 'cancer_male_all_10000'),fread)
temp_merge <- rbind(rbindlist(temp_male), rbindlist(temp_female))
temp_merge[,sex:=rep(c(1,2),each=10000)]
temp_merge[,age:=rep(rep(seq(35,75,5),each=1000),2)]
temp_merge
fwrite(temp_merge,'~/data_out/data_out/cancer_score_10000.csv')
