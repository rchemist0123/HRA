checkOutlier <- function(x){
  y <- ifelse(x<quantile(x,probs = .25, na.rm = T) -1.5*IQR(x,na.rm=T) |
                x> quantile(x,probs=.75, na.rm=T) + 1.5*IQR(x,na.rm=T),NA,x)
  return(y)
}


preprocessing <- function(data, features, target, na,strata = NULL,
                          valid=NULL) {
  if(!is.null(strata)){
    features <- setdiff(features,strata)
  }
  set.seed(2022)
  dt <- copy(data)
  dt[,REGION:=as.numeric(as.character(REGION))]
  train_id <- createDataPartition(dt[[target]], p=.7)
  train_dt <- dt[train_id$Resample1]
  test_dt <- dt[-train_id$Resample1]
  if(!is.null(valid)) {
    tv_id <- createDataPartition(test_dt[[target]],p=0.5)
    valid_dt <- test_dt[tv_id$Resample1]
    test_dt <- test_dt[-tv_id$Resample1]
  
    if(na %in% c('omit','na.omit')){
        train_dt <- na.omit(train_dt) 
        valid_dt <- na.omit(valid_dt) 
      test_dt <- na.omit(test_dt)
    } else if (na=='impute') {
        tar <- c('MET_CON')
        train_dt[,(tar):=lapply(.SD, function(x) ifelse(is.na(x), mean(x,na.rm=T),x)),.SDcols=tar]
        valid_dt[,(tar):=lapply(.SD, function(x) ifelse(is.na(x), mean(x,na.rm=T),x)),.SDcols=tar]
        test_dt[,(tar):=lapply(.SD, function(x) ifelse(is.na(x), mean(x,na.rm=T),x)),.SDcols=tar]
        tar <- setdiff(names(train_dt),c('death','ha','mace',
                                         grep('duration',names(train),value=T),
                                         grep('cancer',names(train),value=T),
                                         'MET_CON'))
        train_dt<-train_dt[,(tar):=lapply(.SD, function(x) ifelse(is.na(x), median(x,na.rm=T),x)),.SDcols=tar]
        valid_dt<-valid_dt[,(tar):=lapply(.SD, function(x) ifelse(is.na(x), median(x,na.rm=T),x)),.SDcols=tar]
        test_dt<-test_dt[,(tar):=lapply(.SD, function(x) ifelse(is.na(x), median(x,na.rm=T),x)),.SDcols=tar]
      }
    result <- list(tr=train_dt,va=valid_dt, te=test_dt)
  } else {
    if(na %in% c('omit','na.omit')){
      train_dt <- na.omit(train_dt)
      test_dt <- na.omit(test_dt)
    } else if (na=='impute') {
      tar <- c('MET_CON')
      train_dt[,(tar):=lapply(.SD, function(x) ifelse(is.na(x), mean(x,na.rm=T),x)),.SDcols=tar]
      test_dt[,(tar):=lapply(.SD, function(x) ifelse(is.na(x), mean(x,na.rm=T),x)),.SDcols=tar]
      tar <- setdiff(names(train_dt),c('death','ha','mace',
                                       grep('duration',names(train),value=T),
                                       grep('cancer',names(train),value=T),
                                       'MET_CON'))
      train_dt<-train_dt[,(tar):=lapply(.SD, function(x) ifelse(is.na(x), median(x,na.rm=T),x)),.SDcols=tar]
      test_dt<-test_dt[,(tar):=lapply(.SD, function(x) ifelse(is.na(x), median(x,na.rm=T),x)),.SDcols=tar]
    }
    result <- list(tr=train_dt,te=test_dt)
  }
  return(result)
}

train_model <- function(train_data, model, features, target, preprocess){
  # modeling
  if (model=='xgboost'){
    require(xgboost)
    require(fastDummies)
    
    xgbfeatures <- setdiff(features,c(grep('_total',names(train),value = T), 'AGE_CAT'))
    xgb_chr_features <- c(grep('_total',names(train),value=T),'AGE_CAT')
    train_dt[,(xgbfeatures):=lapply(.SD, function(x) as.numeric(as.character(x))),.SDcols=xgbfeatures]
    test_dt[,(xgbfeatures):=lapply(.SD, function(x) as.numeric(as.character(x))),.SDcols=xgbfeatures]
    train_dt[,(xgb_chr_features):=lapply(.SD, as.character),.SDcols=xgb_chr_features]
    test_dt[,(xgb_chr_features):=lapply(.SD, as.character),.SDcols=xgb_chr_features]
    train_dt <- dummy_cols(train, select_columns = xgb_chr_features,
                              remove_first_dummy = T,
                              remove_selected_columns = T)
    test_dt <<- dummy_cols(test, select_columns = xgb_chr_features,
                             remove_first_dummy = T,
                             remove_selected_columns = T)
    xgbparams <- list(gamma=0.05,eta=0.015,max_depth=10,
                      objective = 'binary:logistic')
    finalFeatures <- setdiff(names(train),target)
   fit <- xgboost(data = train[,..finalFeatures] %>% as.matrix,
                  label = train[[target]] %>% as.matrix,
                  params = xgbparams,
                  nrounds = 500,
                  verbose = F)
  } else if (model=='cox'){
    require(survival)
    cox_features <- c(target,'ill_duration',features)
    form <- paste0('Surv(ill_duration, ill_yn==1)','~',paste(features, collapse = '+'))
    fit <- coxph(as.formula(form), data=dt)
  } else {
    form <- paste0(target,'~',paste(features, collapse = '+'))
    trctrl <- trainControl(method='cv', number=5)
    fit <- train(as.formula(form),
                 data=train_data,
                 preProcess=preprocess,
                 method='glm',
                 na.action=na.omit,
                 trControl = trctrl,
                 family='binomial')
  }
  return(fit)
}

test_performance <- function(fit,target,test){
  require(MLmetrics)
  if(class(fit)=='xgb.Booster') {
    features <- setdiff(names(test),target)
    pred <- predict(fit, test[,..features] %>% as.matrix , type='prob') 
    pred_bin <- ifelse(pred>=0.5,1,0) %>% as.factor
  } else {
    pred <<- predict(fit, test, type='prob')[,2]
    pred_bin <<- predict(fit, test, type='raw') %>% as.factor
  }
  cm <- confusionMatrix(pred_bin, test[[target]], mode='everything',positive='1')
  cm_total <- c(cm$overall[1], cm$byClass[c(1:5,7:8)])
  pred2 <<- prediction(pred, test[[target]])
  auroc <- performance(pred2, measure = 'auc')
  auprc <- PRAUC(y_true = test[[target]], y_pred=pred)
  
  if(class(fit)[1]=='xgb.Booster'){
    result <- c(cm_total, auroc = auroc@y.values[[1]], auprc = auprc)
  } else {
    aic <- fit$finalModel$aic
    result <- c(cm_total, aic=aic, auroc = auroc@y.values[[1]], auprc = auprc)
  }
  test[,ha_score := (1-pred)*1000]
  return(result)
}


plot_performance <- function(fit, test, target, type){
  require(MLmetrics)
  require(pROC)
  # pred <- predict(fit, test, type='prob')
  if(type %in% c('roc','auc')){
    roc <- pROC::roc(test[[target]] ~ pred)
    print(plot.roc(roc,
             asp=NA,
             legacy.axes = T,
             print.auc.y=.4,
             print.auc = T,
             col='orange'))
  }else if(type=='prc'){
    pred2 <- prediction(pred,test[[target]])
    perf <- performance(pred_2, 'prec','rec')
    prauc <- PRAUC(y_true = test[[target]], y_pred=pred)
    print(plot(perf,
         avg='threshold',
         main=paste0('Precison/Recall Curve: ',round(prauc,4)),
         col='red'))
  }else if(type=='hist'){
    print(ggplot(test,aes(x=ha_score))+
            geom_histogram(bins=10000,
                           color='black',
                           fill='lightblue3',
                           binwidth = 20)+
            # scale_x_continuous(limits = c(0,1000))+
            theme_bw())
    message('count: ',nrow(test))
    print(test[,summary(ha_score)])
  } else if (type=='poly'){
    p <- ggplot(test, aes(x=MET_CON, y=ha_score/1000))+
      geom_smooth(method='lm', 
                  se=F, size=.7, aes(color='Linear'))+
      geom_smooth(method='lm', se=F, size=.5,
                  formula = y ~ poly(x,2),aes(color='Polynomial'))+
      labs(x='METs', y='Health score')+
      scale_color_manual(name='Model',values=c('orange','blue'))+
      theme_classic()+
      theme(legend.position = c(0.2,0.15))
    print(p)
  }
}


lr_platt_recal_func <- function(fit, valid_data, test_res, test, y){
  val_estimates_norm <- predict(fit, valid_data, type='prob')[,2]
  train_re_mtx <- data.table(y=valid_data[[y]], 
                             yhat=val_estimates_norm)
  
  # create calibration model
  calib.model <- glm(y~yhat, data=train_re_mtx, family=binomial)
  ygrid_norm = data.table(yhat=test_res)
  # colnames(test_res) <- c("yhat")
  
  # recalibrate and measure on test set
  ygrid_cal = predict(calib.model, ygrid_norm, type='response')
  return(ygrid_cal)
}

lr_iso_recal_func <-  function(model_fit, validate_data, test_res, test, y,type=NULL){
  # predict validation datasets's estimates with logistic regression
  val_estimates_norm = predict(model_fit, validate_data,  type='prob')[,2]
  train_re_mtx = data.table(y=as.numeric(as.character(validate_data[[y]])), 
                       yhat=val_estimates_norm)
  # iso_train_mtx = train_re_mtx[order(train_re_mtx[,2])]
  setorder(train_re_mtx, yhat)
  # create calibration model
  calib.model <<- isoreg(train_re_mtx[,yhat], train_re_mtx[,y])
  stepf_data <- cbind(calib.model$x, calib.model$yf)
  step_func <- stepfun(stepf_data[,1], c(0,stepf_data[,2]))
  
  # recalibrate and measure on test set
  exp2_iso_recal <- step_func(test_res)
  return(exp2_iso_recal)
}


perfScore <- function(data, fit, pred_prob, pred_bin, ref){
  cm <- confusionMatrix(data[[pred_bin]], data[[ref]],
                        positive='1',mode = 'everything')$byClass
  recall <- cm[1]
  spe <- cm[2]
  ppv <- cm[3]
  npv <- cm[4]
  precision <- cm[5]
  f1 <- cm[7]
  auc <- pROC::auc(data[[ref]] ~ data[[pred_prob]])
  if(class(fit)[1]!='xgb.Booster'){
    aic <- AIC(fit)
    tab <- c(AIC=aic,recall,spe,ppv,npv,precision,f1,AUC=auc) %>% as.data.table(keep.rownames=T)
  } else {
    tab <- c(recall,spe,ppv,npv,precision,f1,AUC=auc) %>% as.data.table(keep.rownames=T)
  }
  names(tab) <- c('key','value')
  tab[,value := round(value,4)]
  return(tab[])
}


getNRI <- function(data,ref, pred_bin, pred_bin2){
  data[['nri']] <- ifelse(data[[pred_bin]] == data[[ref]] &
                            data[[pred_bin2]] != data[[ref]],'-',
                          ifelse(data[[pred_bin]]!= data[[ref]] &
                                   data[[pred_bin2]] == data[[ref]],'+','0'))
  tab <- addmargins(table(data[['nri']],data[[ref]]), margin = 1)
  print(tab)
  result <- (tab[2]-tab[1])/tab[4] + (tab[6]-tab[5])/tab[8]
  return(result)
}


modeling <- function(data, x, y, model){
  data <- data %>% na.omit()
  if(model %in% c('glm','lr')){
    form <- paste0(y,'~',paste(x,collapse='+'))
    fit <- glm(as.formula(form),family=binomial, data)
  } else if (model %in% c('cox','coxph')){
    if (y=='ill_yn'){
      form <- paste0('Surv(ill_duration, ill_yn==1)','~',paste(x, collapse = '+'))
    } else if (y=='death'){
      form <- paste0('Surv(death_duration, death==1)','~',paste(x, collapse = '+'))
    } else if (y=='mace_yn'){
      form <- paste0('Surv(mace_duration, mace_yn==1)', '~', paste(x, collapse='+'))
    } else if (y=='mace_plus_yn'){
      form <- paste0('Surv(mace_plus_duration, mace_plus_yn==1)', '~', paste(x, collapse='+'))
    }
    fit <- coxph(as.formula(form), data=data)
  }
  return(fit)
}

ml_modeling <- function(data, model, x, seed=2022){
  set.seed(seed)
  data <- na.omit(data)
  if(!is.data.table(data)) setDT(data)
  id <- sample(nrow(data),0.7*nrow(data))
  train <- data[id]
  test <- data[-id]
  
  if(model %in% c('lr','LR','logistic')){
    form <- paste0('ill_duration','~',paste(x,collapse = '+'))
    mod <- glm(as.formula(form), family=binomial, data=train)
    pred <- predict(mod, test, type='response')
    pred_bin <- ifelse(pred>=0.5,1,0) %>% as.factor
  } else if (model %in% c('cox','coxph','COX')){
    form <- paste0('Surv(ill_duration, ill_yn==1)','~',paste(x, collapse = '+'))
    mod <- coxph(as.formula(form), data=train)
    pred <- predict(mod, test, type='response')
    pred_bin <- ifelse(pred>=0.5,1,0) %>% as.factor
  }
}

