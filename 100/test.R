




require(data.table)
require(dplyr)
require(R6)




### input: sampling 1000
### output: health score + rank
### coef: coefficients of models according to sex
require(data.table)
require(dplyr)





fread('~/working/mortality/coefficient/mace_coef_female.csv')

hsc_mace$getHealthScore(fit = mace_fit, test=tt_mace$test)
for(i in c(1:2)){
  for(j in ages){
    # print(nrow(dt_death[SEX==i & AGE<j]))
    print(sample(nrow(dt_death[SEX==i & AGE<j]),1000) %>% length())
  }
}
 

fread('~/working/mortality/output/female_35_output.csv')


# Mean by age sex -----------------------------------------------------------

lists <- list.files('~/working/mortality/sample', pattern = 'death_sample')
vars <- intersect(names(fread('~/working/mortality/sample/1_30_death_sample.csv')),adv)
lists
vars
require(data.table)
meanslist <- list()
for(i in lists){
  temp <- fread(paste0('~/working/mortality/sample/',i))
  age <- substr(i,3,4)
  sex <- substr(i,1,1)
  meanslist[[paste(c(age,sex),collapse='_')]] <- temp[,lapply(.SD, mean),.SDcols=vars]
  # meanslist[[paste(c(age,sex),collapse='_')]]
  # for(j in vars){ 
  #   print(prop.table(table(temp[[j]]))*100)
  # }
}



rbindlist(meanslist,idcol = T)


for(i in seq(0.1,0.5,0.1)){
  mace_male_perf <- hsc_mace$perfTest(fit = mace_fit$male, test = tt_mace$test[SEX==1], cutoff=i)
  mace_female_perf <- hsc_mace$perfTest(fit = mace_fit$female, test = tt_mace$test[SEX==2],cutoff=i)
  message('cutoff: ',i)
  print(rbindlist(list(mace_male_perf, mace_female_perf),idcol = T))
}


temp[,.(health_score_raw,  rank= frank(health_score_raw))][order(rank)]


# export ------------------------------------------------------------------

setwd('~/working/mortality/output')

temp <- lapply(list.files('~/working/mortality/output/', pattern = 'mace_female'),fread)
fwrite(rbindlist(temp),'~/data_out/data_out/mace_female_output.csv')
temp <- lapply(list.files('~/working/mortality/output/', pattern = 'mace_male'),fread)
fwrite(rbindlist(temp),'~/data_out/data_out/mace_male_output.csv')

temp <- lapply(list.files('~/working/mortality/output/', pattern = 'death_female'),fread)
fwrite(rbindlist(temp),'~/data_out/data_out/death_female_output.csv')
temp <- lapply(list.files('~/working/mortality/output/', pattern = 'death_male'),fread)
fwrite(rbindlist(temp),'~/data_out/data_out/death_male_output.csv')

setwd('~/working/mortality/coefficient')
temp <- lapply(
  list.files('~/working/mortality/coefficient/',
             pattern='cancer_coef'),fread)

fwrite(merge(temp[[1]],temp[[3]],by='vars', suffixes = c('_f','_m')),'cancer_coef.csv')
fwrite(merge(temp[[2]],temp[[4]],by='vars', suffixes = c('_f','_m')),'cancer_coef_range.csv')
temp <- lapply(list.files('~/working/mortality/coefficient/',pattern='mace_coef'),fread);

temp[[4]]
fwrite(merge(temp[[1]],temp[[2]],by='vars', suffixes = c('_f','_m')),'mace_coef.csv')
temp <- lapply(list.files('~/working/mortality/coefficient/',pattern='death_coef'),fread)
temp[[4]]
fwrite(merge(temp[[1]],temp[[2]],by='vars', suffixes = c('_f','_m')),'death_coef.csv')

list.files('~/data_out/data_out', pattern = 'mace_male')
list.files('~/data_out/data_out', pattern = 'mace_female')
list.files('~/data_out/data_out', pattern = 'death_male')
list.files('~/data_out/data_out', pattern = 'death_female')


# poly --------------------------------------------------------------------
cancer_fit <- hsc_cancer$train_model(train = tt_cancer$train, features = adv, strata = 'SEX', polynomial=T)
cancer_fit2 <- hsc_cancer$train_model(train = tt_cancer$train, features = adv, strata = 'SEX', polynomial=F)
hsc_cancer$perfTest(fit = cancer_fit2$male, test = tt_cancer$test[SEX==1])
d_m_p <- copy(test_dt)
d_m_np <- copy(test_dt)
AGE %between% c(51,74)
ggplot(d_m_np[], aes(x=DRK_CON, y=pred))+
  geom_smooth(method='lm', 
              se=F, size=.7, aes(color='Linear'))+
  geom_smooth(data=d_m_p[],
              aes(x=DRK_CON, y=pred, color='Polynomial'),
              method='lm', se=F, size=.5,
              formula = y ~ poly(x,2))+
  labs(x='DRK_CON', y='cancer probability')+
  scale_color_manual(name='Model',values=c('orange','blue'))+
  theme_classic()+
  theme(legend.position = c(0.15,0.15))

hsc_cancer$perfTest(fit = cancer_fit2$female, test = tt_cancer$test[SEX==2])
d_f_p <- copy(test_dt)
d_f_np <- copy(test_dt)
ggplot(d_f_np[], aes(x=DRK_CON, y=pred))+
  geom_smooth(method='lm', 
              se=F, size=.7, aes(color='Linear'))+
  geom_smooth(data=d_f_p[],
              aes(x=DRK_CON, y=pred, color='Polynomial'),
              method='lm', se=F, size=.5,
              formula = y ~ poly(x,2))+
  labs(x='DRK_CON', y='cancer probability')+
  scale_color_manual(name='Model',values=c('orange','blue'))+
  theme_classic()+
  theme(legend.position = c(0.15,0.8))
