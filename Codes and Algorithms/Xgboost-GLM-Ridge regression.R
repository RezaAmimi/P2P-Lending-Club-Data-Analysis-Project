library(tidyverse)
lcdf=read_csv('C:\\Users\\USER\\Documents\\Maria Jose Montalvo\\UIC\\1st Semester\\IDS.- 572 Business Data Mining\\Assigments\\Data\\lcData100K.csv')
dim(lcdf)
######################################PART 2A #################################################

#2.3
library(lubridate)

lcdf$last_pymnt_d<-paste(lcdf$last_pymnt_d, "-01", sep = "")
lcdf$last_pymnt_d<-parse_date_time(lcdf$last_pymnt_d,  "myd")
x<- as.duration(lcdf$issue_d  %--% lcdf$last_pymnt_d)
head(x)
x<- as.duration(lcdf$issue_d  %--% lcdf$last_pymnt_d)/dyears(1)
lcdf$actualTerm <- ifelse(lcdf$loan_status=="Fully Paid", as.duration(lcdf$issue_d  %--% lcdf$last_pymnt_d)/dyears(1), 3)


#2.4
#calculate the annualized percentage return
lcdf$annRet <- ((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(12/36)*100
mean(lcdf$annRet)
lcdf %>% group_by(grade) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), avgInterest= mean(int_rate), stdInterest=sd(int_rate), avgLoanAMt=mean(loan_amnt), avgPmnt=mean(total_pymnt), avgRet=mean(annRet), stdRet=sd(annRet), minRet=min(annRet), maxRet=max(annRet))
lcdf$actualReturn <- ifelse(lcdf$actualTerm>0, ((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(1/lcdf$actualTerm)*100, 0)


#2.7
lcdf$propSatisBankcardAccts <- ifelse(lcdf$num_bc_tl>0, lcdf$num_bc_sats/lcdf$num_bc_tl, 0)
table(lcdf$propSatisBankcardAccts, lcdf$grade)
tablbankcard<-table(lcdf$propSatisBankcardAccts, lcdf$grade)
prop.table(tablbankcard,1) #proportion per column, per row use 1 
lcdf$emp_length <- factor(lcdf$emp_length, levels=c("n/a", "< 1 year","1 year","2 years", "3 years" ,  "4 years",   "5 years",   "6 years",   "7 years" ,  "8 years", "9 years", "10+ years" ))
lcdf %>% group_by(emp_length) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), defaultRate=defaults/nLoans, avgIntRate=mean(int_rate),  avgLoanAmt=mean(loan_amnt),  avgActRet = mean(actualReturn), avgActTerm=mean(actualTerm))
lcdf$openAccRatio <- ifelse(lcdf$open_acc>0, lcdf$open_acc/lcdf$total_acc, 0)
table(lcdf$openAccRatio, lcdf$grade)



###############PART 3 (DROPING VARIABLES) ##########################################################

##Drop some variables for potential leakage, others
#Drop some other columns which are not useful and those which will cause 'leakage'
lcdf <- lcdf %>% select(-c(funded_amnt_inv, term, emp_title, pymnt_plan, title, zip_code,
                           addr_state, out_prncp, out_prncp_inv, total_pymnt_inv, 
                           total_rec_prncp, total_rec_int, total_rec_late_fee,recoveries, 
                           collection_recovery_fee, last_credit_pull_d, policy_code, 
                           disbursement_method, debt_settlement_flag, hardship_flag, 
                           hardship_dpd, settlement_term, application_type, annual_inc_joint,
                           dti_joint, verification_status_joint, mths_since_rcnt_il, earliest_cr_line))



#### PART 2 C ########################################################################################

lcdf <- lcdf %>% select_if(function(x){!all(is.na(x))}) #eliminate the variables that were all missing values

#REPLACING AND DROPPING MISSING VALUES

#values with 99.9% of missing values will be dropped

#Replacing missing values

#lcdf$loan_status <- ifelse(lcdf$loan_status == "Fully Paid", 1, 0)

lcdf$mths_since_last_delinq <- ifelse(lcdf$mths_since_last_delinq  == 'NA', 0, 1)
lcdf$mths_since_last_delinq[is.na(lcdf$mths_since_last_delinq)] = 0
lcdf$mths_since_last_major_derog <- ifelse(lcdf$mths_since_last_major_derog  == 'NA', 0, 1)
lcdf$mths_since_last_major_derog[is.na(lcdf$mths_since_last_major_derog)] = 0 #NA = 0 means doesnt have derogation
lcdf$mths_since_last_record <- ifelse(lcdf$mths_since_last_record  == 'NA', 0, 1) #
lcdf$mths_since_last_record[is.na(lcdf$mths_since_last_record)] = 0
lcdf$mths_since_recent_bc_dlq <- ifelse(lcdf$mths_since_recent_bc_dlq  == 'NA', 0, 1) #
lcdf$mths_since_recent_bc_dlq[is.na(lcdf$mths_since_recent_bc_dlq)] = 0
lcdf$mths_since_recent_inq <- ifelse(lcdf$mths_since_recent_inq  == 'NA', 0, 1) #
lcdf$mths_since_recent_inq[is.na(lcdf$mths_since_recent_inq)] = 0
lcdf$mths_since_recent_revol_delinq <- ifelse(lcdf$mths_since_recent_revol_delinq  == 'NA', 0, 1) #
lcdf$mths_since_recent_revol_delinq[is.na(lcdf$mths_since_recent_revol_delinq)] = 0


lcdf$open_acc_6m[is.na(lcdf$open_acc_6m)] = 0
lcdf$open_act_il[is.na(lcdf$open_act_il)] = 0
lcdf$open_il_12m[is.na(lcdf$open_il_12m)] = 0
lcdf$open_il_24m[is.na(lcdf$open_il_24m)] = 0
lcdf$total_bal_il[is.na(lcdf$total_bal_il)] = 0
lcdf$il_util[is.na(lcdf$il_util)] = 0
lcdf$open_rv_12m[is.na(lcdf$open_rv_12m)] = 0
lcdf$open_rv_24m[is.na(lcdf$open_rv_24m)] = 0
lcdf$max_bal_bc[is.na(lcdf$max_bal_bc)] = 0
lcdf$all_util[is.na(lcdf$all_util)] = 0
lcdf$inq_fi[is.na(lcdf$inq_fi)] = 0
lcdf$total_cu_tl[is.na(lcdf$total_cu_tl)] = 0
lcdf$inq_last_12m[is.na(lcdf$inq_last_12m)] = 0
lcdf$last_pymnt_d[is.na(lcdf$last_pymnt_d)] = '2018/05/01'


lcdf$revol_util[is.na(lcdf$revol_util)] = 0

#install.packages("modeest") 
library(modeest)

a<- mlv(lcdf$avg_cur_bal, method = "meanshift", na.rm=TRUE)
b<-mean(lcdf$avg_cur_bal, na.rm=TRUE)
lcdf$avg_cur_bal[is.na(lcdf$avg_cur_bal)] <- mean(lcdf$avg_cur_bal, na.rm = TRUE)
a<- mlv(lcdf$bc_open_to_buy, method = "meanshift", na.rm=TRUE)
b<-mean(lcdf$bc_open_to_buy, na.rm=TRUE)
lcdf$bc_open_to_buy[is.na(lcdf$bc_open_to_buy)] <- mean(lcdf$bc_open_to_buy, na.rm = TRUE)
b<-mean(lcdf$bc_util, na.rm=TRUE)
lcdf$bc_util[is.na(lcdf$bc_util)] <- mean(lcdf$bc_util, na.rm = TRUE)
max(lcdf$mo_sin_old_il_acct, na.rm=TRUE)
lcdf$mo_sin_old_il_acct[is.na(lcdf$mo_sin_old_il_acct)] = 960

#
a<- mlv(lcdf$mths_since_recent_bc, method = "meanshift", na.rm=TRUE)
b<-mean(lcdf$mths_since_recent_bc, na.rm=TRUE)
max(lcdf$mths_since_recent_bc, na.rm=TRUE)
lcdf$mths_since_recent_bc[is.na(lcdf$mths_since_recent_bc)] <- mean(lcdf$mths_since_recent_bc, na.rm = TRUE)
lcdf$num_rev_accts[is.na(lcdf$num_rev_accts)] <- mean(lcdf$num_rev_accts, na.rm = TRUE)
lcdf$num_tl_120dpd_2m[is.na(lcdf$num_tl_120dpd_2m)] = 0
lcdf$pct_tl_nvr_dlq[is.na(lcdf$pct_tl_nvr_dlq)] <- mean(lcdf$pct_tl_nvr_dlq, na.rm = TRUE)
lcdf$percent_bc_gt_75[is.na(lcdf$percent_bc_gt_75)] <- mean(lcdf$percent_bc_gt_75, na.rm = TRUE)


#check if all missing values where handle
names(lcdf)[colSums(is.na(lcdf))>0]
colMeans(is.na(lcdf))

#####################################################################################################
##Converting character variables

glimpse(lcdf)

lcdf <- lcdf %>% mutate_if(is.character, as.factor)

#######    sPLIT data into training and Test    #####

TRNPROP = 0.7  #proportion of examples in the training sample

nr<-nrow(lcdf)
trnIndex<- sample(1:nr, size = round(TRNPROP * nr), replace=FALSE)

lcdfTrn <- lcdf[trnIndex, ]
lcdfTst <- lcdf[-trnIndex, ]


lcdfTrn$loan_status<- as.factor(lcdfTrn$loan_status)
lcdfTst$loan_status<- as.factor(lcdfTst$loan_status)


############################ xgboost  ############################33


#install.packages("xgboost")
library(xgboost)

#dummies variables
fdum<-dummyVars(~.,data=lcdf %>% select(-loan_status))
dxlcdf <- predict(fdum, lcdf)
levels(lcdf$loan_status)
dylcdf <- class2ind(lcdf$loan_status, drop2nd = FALSE)
colcdf <- dylcdf [, 2] #fully paid is label of interest 

#Training, test subsets
TRNPROP = 0.7  
nr<-nrow(lcdf)
trnIndex<- sample(1:nr, size = round(TRNPROP * nr), replace=FALSE)

dxlcdfTrn <- dxlcdf[trnIndex,]
colcdfTrn <- colcdf[trnIndex]
dxlcdfTst <- dxlcdf[-trnIndex,]
colcdfTst <- colcdf[-trnIndex]

#############         xgb model         ##################

#vars omit 'actualTerm', 'actualReturn', 'issue_d', 'last_pymnt_d','last_pymnt_amnt', 'annRet', 'total_pymnt'
dxTrn <- xgb.DMatrix( subset(dxlcdfTrn, select=-c(annRet, actualTerm, actualReturn, total_pymnt, last_pymnt_d, last_pymnt_amnt)), label=colcdfTrn)
dxTst <- xgb.DMatrix( subset(dxlcdfTst,select=-c(annRet, actualTerm, actualReturn, total_pymnt, last_pymnt_d, last_pymnt_amnt)), label=colcdfTst)

xgbWatchlist <- list(train = dxTrn, eval = dxTst)

## Model 1
xgbParam <- list (
  max_depth = 10, eta = 0.1,
  objective = "binary:logistic",
  eval_metric="error", eval_metric = "auc")

xgb_lsM1.1 <- xgb.train( xgbParam, dxTrn, nrounds = 200,
                       xgbWatchlist, early_stopping_rounds = 10 )
xgb_lsM1.1$best_iteration
xpredTrg<-predict(xgb_lsM1.1, dxTrn) # best_iteration is used
head(xpredTrg)  #these are predicted values for fully paid
xpredTst<-predict(xgb_lsM1.1, dxTst)
xgb_lsM1.1

### AUC ###
#train data
table(pred=as.numeric(xpredTrg>0.5), act=colcdfTrn)
#test data
table(pred=as.numeric(xpredTst>0.5), act=colcdfTst)

xpredTst<-predict(xgb_lsM1.1, dxTst, type='class')
pred_xgb_lsM1<-prediction(xpredTst,lcdfTst$loan_status,
                          label.ordering=c('Charged Off', 'Fully Paid'))
aucPerf_xgb_lsM1<-performance(pred_xgb_lsM1,'tpr','fpr')
plot(aucPerf_xgb_lsM1)
abline(a=0, b= 1)

####   COST EVALUATION  ###################

PROFITVAL <- 18
COSTVAL <- -64.7

xpredTst<-predict(xgb_lsM1.1, dxTst, type='prob')
prPerfXG <- data.frame(xpredTst)
prPerfXG <- cbind(prPerfXG, status=lcdfTst$loan_status)
prPerfXG <- prPerfXG[order(-xpredTst) ,] #sort in desc order of prob(fully_paid)
prPerfXG$profit1 <- ifelse(prPerfXG$status == 'Fully Paid', PROFITVAL, COSTVAL)
prPerfXG$cumProfit1 <- cumsum(prPerfXG$profit)
max(prPerfXG$cumProfit1)
prPerfXG$cumProfit1[which.max(prPerfXG$cumProfit1)]
plot(cumProfit1)

## Model 2
xgbParam <- list (
  max_depth = 5, eta = 0.05,
  objective = "binary:logistic",
  eval_metric="error", eval_metric = "auc")

xgb_lsM1.2 <- xgb.train( xgbParam, dxTrn, nrounds = 200,
                         xgbWatchlist, early_stopping_rounds = 10 )
xgb_lsM1.2$best_iteration
xpredTrg<-predict(xgb_lsM1.2, dxTrn) # best_iteration is used
head(xpredTrg)  #these are predicted values for fully paid


### AUC ###
#train data
table(pred=as.numeric(xpredTrg>0.5), act=colcdfTrn)
#test data
table(pred=as.numeric(xpredTst>0.5), act=colcdfTst)
xpredTst<-predict(xgb_lsM1.2, dxTst)$prediction
pred_xgb_lsM1<-prediction(xpredTst,lcdfTst$loan_status,
                          label.ordering=c( 'Fully Paid','Charged Off' ))
aucPerf_xgb_lsM1<-performance(pred_xgb_lsM1,'tpr','fpr')
plot(aucPerf_xgb_lsM1)
abline(a=0, b= 1)

## Model 4
xgbParam <- list (
  max_depth = 3, eta = 0.1,
  objective = "binary:logistic",
  eval_metric="error", eval_metric = "auc")

xgb_lsM1 <- xgb.train( xgbParam, dxTrn, nrounds = 500,
                       xgbWatchlist, early_stopping_rounds = 10 )
xgb_lsM1$best_iteration
xpredTrg<-predict(xgb_lsM1, dxTrn) # best_iteration is used
head(xpredTrg)  #these are predicted values for fully paid
xpredTst<-predict(xgb_lsM1, dxTst)
xgb_lsM1

##########     confusion matrix      ###############

library(ROCR)

#train data
table(pred=as.numeric(xpredTrg>0.5), act=colcdfTrn)
#test data
table(pred=as.numeric(xpredTst>0.5), act=colcdfTst)

xpredTst<-predict(xgb_lsM1, dxTst)

pred_xgb_lsM1<-prediction(xpredTst,lcdfTst$loan_status,
                          label.ordering=c('Fully Paid', 'Charged Off'))

aucPerf_xgb_lsM1<-performance(pred_xgb_lsM1,'tpr','fpr')
plot(aucPerf_xgb_lsM1)
abline(a=0, b= 1)

#Plot comparing models (RF and rpart ROC)
plot(perfROCTst, add=TRUE, col="red")
abline(a=0, b= 1)


liftPerf <-performance(rocPredTstRF, "lift", "rpp")
plot(liftPerf)

######    use cross-validation     ############

xgbParam <- list (
  max_depth = 2, eta = 0.1,
  objective = "binary:logistic",
  eval_metric="error", eval_metric = "auc")

xgb_lscv <- xgb.cv( xgbParam, dxTrn, nrounds = 500, nfold=5, early_stopping_rounds = 10 )
#best iteration
xgb_lscv$best_iteration
xgb_lscv$evaluation_log

# or for the best iteration based on performance measure (among those specified in xgbParam)
best_cvIter1 <- which.max(xgb_lscv$evaluation_log$test_auc_mean)
best_cvIter2 <- which.min(xgb_lscv$evaluation_log$test_error_mean)

#learn the best model without xval
xgb_lsbest <- xgb.train( xgbParam, dxTrn, nrounds = xgb_lscv$best_iteration )

#variable importance
xgb.importance(model = xgb_lsbest) %>% view()

xpredTrncv<-predict(xgb_lscv, dxTrn) # best_iteration is used
head(xpredTrg)  #these are predicted values for fully paid
xpredTstcv<-predict(xgb_lscv, dxTst)

###########    Hiper parameter - grid      #########

xgbParamGrid <- expand.grid(
  max_depth = c(3, 5, 8),
  eta = c(0.01, 0.1, 0.5) 
  #gamma = c(1, 0.0, 0.2), 
  #min_child_weight = c(1)
  )

for(i in 1:nrow(xgbParamGrid)) {
  xgb_tune<- xgboost(data=dxTrn,, objective = "reg:squarederror", nrounds=50, eta=xgbParamGrid$eta[i],
                     max_depth=xgbParamGrid$max_depth[i], early_stopping_rounds = 8)
  xgbParamGrid$bestTree[i] <- xgb_tune$evaluation_log[xgb_tune$best_iteration]$iter
  xgbParamGrid$bestPerf[i] <- xgb_tune$evaluation_log[xgb_tune$best_iteration]$train_rmse
}

xgbParams <- list (
  booster = "gbtree",
  objective = "reg:squarederror",
  eta=0.001, #learning rate
  max_depth=2,
  min_child_weight=1,
  colsample_bytree=0.6
)

xgb_lsM1 <- xgb.train( xgbParams, dxTrn, nrounds = 500,
                       xgbWatchlist, early_stopping_rounds = 10 )



##################################     GLM          #########################################

install.packages("glmnet")
library(glmnet)
levels(lcdfTrn$loan_status)

#vars omit 'actualTerm', 'actualReturn', 'issue_d', 'last_pymnt_d','last_pymnt_amnt', 'annRet', 'total_pymnt'

yTrn<-factor(if_else(lcdfTrn$loan_status=="Fully Paid", '1', '0') )
xDTrn<-lcdfTrn %>% select(-loan_status, -actualTerm, -annRet, -actualReturn, -total_pymnt, -issue_d, -last_pymnt_d, -last_pymnt_amnt, -funded_amnt )

#####################              LASSO                #####################
glmls_cv<- cv.glmnet(data.matrix(xDTrn), yTrn, family="binomial")

glmls_cv$lambda.min
glmls_cv$lambda.1se
#coef(glmls_cv, s = glmls_cv$lambda.min)
coef(glmls_cv, s = glmls_cv$lambda.1se)
as.matrix(coef(glmls_cv, s=glmls_cv$lambda.min))

glmls_cv$glmnet.fit

which(glmls_cv$lambda == glmls_cv$lambda.1se)
glmls_cv$glmnet.fit$dev.ratio[which(glmls_cv$lambda == glmls_cv$lambda.1se) ]

### Plots
plot(glmls_cv$glmnet.fit)
plot(glmls_cv$glmnet.fit, xvar="lambda")

## AUC
glmls_cv_auc<- cv.glmnet(data.matrix(xDTrn), yTrn, family="binomial", type.measure = "auc")
plot(glmls_cv_auc)

#MSE
glmls_cv_mse<- cv.glmnet(data.matrix(xDTrn), yTrn, family="binomial", type.measure = "mse")
plot(glmls_cv_mse)


#the labmda values used are in glmls_cv$lambda
glmls_cv_auc$lambda
# and the cross-validation 'loss' at each lambda is in glmls_cv$cvm
glmls_cv_auc$cvm

#So, to get the 'loss' value at lambda == lambda.1se
glmls_cv_auc$cvm [ which(glmls_cv_auc$lambda == glmls_cv_auc$lambda.1se) ]

#####  PREDICTIONS  ################

glmPredls_1=predict ( glmls_cv,data.matrix(xDTrn), s="lambda.min" ) # gives the ln(p/(1-p)) values
#i.e. the values of w1*x1 + ...+w2*x2
glmPredls_p=predict(glmls_cv,data.matrix(xDTrn), s="lambda.min", type="response" ) #gives the prob values

predsauc <- prediction(glmPredls_p, lcdfTrn$loan_status, label.ordering = c("Charged Off", "Fully Paid"))
aucPerf <- performance(predsauc, "auc")
aucPerf@y.values


#####################           RIDGE               #####################

glmls_cv<- cv.glmnet(data.matrix(xDTrn), yTrn, alpha=0, family="binomial")

glmls_cv$lambda.min
glmls_cv$lambda.1se
coef(glmls_cv, s = glmls_cv$lambda.min)
coef(glmls_cv, s = glmls_cv$lambda.1se)
as.matrix(coef(glmls_cv, s=glmls_cv$lambda.min))

glmls_cv$glmnet.fit

which(glmls_cv$lambda == glmls_cv$lambda.1se)
glmls_cv$glmnet.fit$dev.ratio[which(glmls_cv$lambda == glmls_cv$lambda.1se) ]

### Plots
plot(glmls_cv$glmnet.fit)
plot(glmls_cv$glmnet.fit, xvar="lambda")

## AUC
glmls_cv_auc<- cv.glmnet(data.matrix(xDTrn), yTrn, family="binomial", type.measure = "auc")
plot(glmls_cv_auc)

#the labmda values used are in glmls_cv_auc
glmls_cv_auc$lambda
# and the cross-validation 'loss' at each lambda is in glmls_cv$cvm
glmls_cv_auc$cvm

#MSE
glmls_cv_mse<- cv.glmnet(data.matrix(xDTrn), yTrn, family="binomial", type.measure = "mse")
plot(glmls_cv_mse)

#So, to get the 'loss' value at lambda == lambda.1se
glmls_cv_auc$cvm [ which(glmls_cv_auc$lambda == glmls_cv_auc$lambda.1se) ]

#####  PREDICTIONS  ###########

glmPredls_1=predict ( glmls_cv,data.matrix(xDTrn), s="lambda.min" ) # gives the ln(p/(1-p)) values
#i.e. the values of w1*x1 + ...+w2*x2
glmPredls_p=predict(glmls_cv,data.matrix(xDTrn), s="lambda.min", type="response" ) #gives the prob values

predsauc <- prediction(glmPredls_p, lcdfTrn$loan_status, label.ordering = c("Charged Off", "Fully Paid"))
aucPerf <- performance(predsauc, "auc")
aucPerf@y.values


#########################            BALANCED DATA         ########################
library(broom)

sum(yTrn==0)
sum(yTrn==1)
1-(sum(yTrn==0)/length(yTrn))
1-(sum(yTrn==1)/length(yTrn))

wts=if_else(yTrn==0,1-(sum(yTrn==0)/length(yTrn)),1-(sum(yTrn==1)/length(yTrn)))
glmlsw_cv<- cv.glmnet(data.matrix(xDTrn), yTrn, family="binomial", weights = wts)
plot(glmlsw_cv)

tidy(coef(glmlsw_cv, s=glmlsw_cv$lambda.1se))
tidy(coef(glmls_cv, s=glmls_cv$lambda.1se)) #for model without weights

glmls_1 <- glmnet(data.matrix(xDTrn), yTrn, family="binomial", lambda = glmls_cv$lambda.1se )
glmls_1
tidy(glmls_1)
tidy(coef(glmls_cv,s=glmls_cv$lambda.1se)) #Compare with coefficients from glmls_cv for lambda=lambda.1se

glmls_1 <- glmnet(data.matrix(xDTrn), yTrn, family="binomial", lambda = glmls_cv$lambda)
tidy(coef(glmls_1, s= glmls_cv$lambda.1se)) #If we specify the same sequence of lambda values, the coeffs will be same.

###  Linear model without regularization, and with significance for coefficients   ###

#get the variables with non-zero coefficients from the regularized model
nzCoef <- coef(glmls_cv, s= 'lambda.1se') %>% tidy()
nzCoefVars <- nzCoef[-1,1]


#Develop a linear model without regularization
glmls_nzv_2 <- glm(yTrn ~ data.matrix(xDTrn %>% select(nzCoefVars)), family='binomial')
glmls_nzv <- glm.fit (data.matrix(xDTrn %>% select(nzCoefVars)), yTrn, family=binomial())

summary(glmls_nzv_2)
