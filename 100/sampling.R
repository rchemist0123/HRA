
# sampling among all ages
hsc_death$generateAllSample(data=rbindlist(tt_death),n = 5000)
hsc_death$getHealthScore(fit = death_fit, test=tt_death$test, type='all', target='death')
hsc_death$getHealthScore(fit = mace_fit, test=tt_death$test, type='all', target='mace')
hsc_death$getHealthScore(fit = cancer_fit, test=tt_death$test, type='all', target='cancer')
hsc_death$getHealthScore(fit = t2dm_fit, test=tt_death$test, type='all', target='t2dm')
hsc_death$getHealthScore(fit = htn_fit, test=tt_death$test, type='all', target='htn')
hsc_death$getHealthScore(fit = metsyn_fit, test=tt_death$test, type='all', target='metsyn')
hsc_death$getHealthScore(fit = illness_fit, test=tt_death$test, type='all', target='illness')

lists <- list.files('~/working/mortality/output/', pattern = '5000_output.csv')
lists
temp <- lapply(list.files('~/working/mortality/output/', pattern = '5000_output.csv'),fread)
temp2 <- rbindlist(temp)
temp2[,.N,keyby=pid]
# 7 disease * 2 sex * 5000 samples = 70,000
temp2[,disease := rep(target, each=10000)]
temp2[,AGE_GROUP := (AGE%/%10) * 10]
temp2[,.N,by=AGE_GROUP]
fwrite(temp2,'~/data_out/data_out/sex_disease_sampling_5000.csv')

temp <- fread('~/working/mortality/output/mace_male_all_5000_output.csv')
temp2 <- fread('~/working/mortality/output/illness_male_all_5000_output.csv')

# sampling by ages
hsc_death$generateSample(data = rbindlist(tt_death))
hsc_death$getHealthScore(fit = death_fit, test=tt_death$test, target='death')
hsc_death$getHealthScore(fit = mace_fit, test=tt_death$test, target='mace')
hsc_death$getHealthScore(fit = cancer_fit, test=tt_death$test,  target='cancer')
hsc_death$getHealthScore(fit = t2dm_fit, test=tt_death$test, target='t2dm')
hsc_death$getHealthScore(fit = htn_fit, test=tt_death$test, target='htn')
hsc_death$getHealthScore(fit = metsyn_fit, test=tt_death$test, target='metsyn')
hsc_death$getHealthScore(fit = illness_fit, test=tt_death$test, target='illness')


lists <- list.files('~/working/mortality/output/', pattern = '2000_output.csv')
lists

setwd('~/working/mortality/output/')
temp <- lapply(list.files('~/working/mortality/output/', pattern = '2000_output.csv'),fread)
temp2 <- rbindlist(temp)
target <- c('cancer','death','htn','illness','mace','metsyn','t2dm')
# 5 ages_group * 7 disease * 2 sex * 2000 samples = 140,000?
temp2[,disease := rep(target, each=20000)]
temp2[20000:20001]
temp2[,AGE_GROUP := (AGE%/%10) * 10]
fwrite(temp2,'~/data_out/data_out/age_sex_disease_sampling_2000.csv')
