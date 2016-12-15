library(caret)
library(mlbench)
library(rpart)
library(rpart.plot)
# library(rattle)    # fancyRpartPlot

# ---------------------------------------
# ----TRAIN / TEST SLPIT ----------------

set.seed(9)
inTrain <- createDataPartition(df.lead$preq,
                               p = 0.80)

train <- df.lead[inTrain[[1]],]
test  <- df.lead[-inTrain[[1]],]

# ---------------------------------------
# ---- feature list      ----------------

# Data that can be gatherered
# Easily
# 'Country', 'Sector.and.Perishability', 'Sales', 'Sales.Concentration.to.a.Single.Buyer', 'Years_of_Audited_Financials',
#  'Supply.Chain.Type',

# isCoffee
# Exports.vs..Local.Sales

# # 
# 	  Sales_CAGR
# Audited.vs..non.audited.financials..1 # Audited Financials
# Loan.Tenor
# Years Experience of Manager
# Access to Capital  # EC: maybe 3 categories: no financing from other sources, financing from social lenders and MFI, and financing from commercial banks


# Amount / Sales

modelCols <- c('preq', 'Sales', 'Lending.Region', 'Supply.Chain.Type', 'Sector.and.Perishability', 'Years_of_Audited_Financials',
	'Sales.Concentration.to.a.Single.Buyer', 'Supply.Chain.Type', 'Sales_CAGR')

modelCols <- c('preq', 'Sales', 'Lending.Region', 'Supply.Chain.Type',                            'Years_of_Audited_Financials',
	'Sales.Concentration.to.a.Single.Buyer', 'Supply.Chain.Type', 'Sales_CAGR')

# subset to model vars
train_sub <- train[,names(train) %in% modelCols]

# train a preprocess method to impute median for missing values
impute_model <- preProcess(
	train_sub,
	method = 'medianImpute')

# impute missing values
train_pre <- predict(impute_model, newdata = train_sub)

# ---------------------------------------
# ---- rcart model       ----------------

tc_rpart <- trainControl(
	'cv',
	number = 5,
	repeats = 1,
	savePred = TRUE,
	verboseIter = TRUE,
	summaryFunction = twoClassSummary,
	classProbs = TRUE
	)

set.seed(9)

rpart.model <- train(
	preq ~ Sales,
	data = train,
	method = 'rpart',
	# tuneLength = 20,
	metric = 'ROC',
	control = rpart.control(minsplit = 20, minbucket=25, cp = 0.01, maxdepth = 5),
	trControl = tc_rpart)

rpart.plot(rpart.model$finalModel)
# fancyRpartPlot(rpart.model$finalModel)

rpart.model2 <- train(
	preq ~ . ,
	# preq ~ Sales + Lending.Region + Supply.Chain.Type + isCoffee, # + Sales.Concentration.to.a.Single.Buyer 
	data = train_pre,
	method = 'rpart',
	# tuneLength = 20,
	metric = 'ROC',
	control = rpart.control(minsplit = 5, minbucket=10, cp = 0.0001, maxdepth = 5),
	trControl = tc_rpart)

rpart.plot(rpart.model2$finalModel)
# fancyRpartPlot(rpart.model2$finalModel)
print(rpart.model$finalModel)

pred <- predict(rpart.model2, train_pre, type="prob")

  pROC::roc(as.numeric(train_pre$preq), pred[,1])
  confusionMatrix(pred[,2], train_pre$preq)
  plot(rpart.model2)
print(rpart.model2$results)

# ---------------------------------------
# ---- rf model       ----------------

tc_rf <- trainControl(
	'repeatedcv',
	number = 5,
	repeats = 1,
	savePred = TRUE,
	verboseIter = TRUE,
	# importance = 'impurity'
	summaryFunction = twoClassSummary,
	classProbs = TRUE
	)

rf.model1 <- train(
	preq ~ . ,
	data = train_pre,
	# preProcess = c('medianImpute'),
	method = 'rf',
	tuneLength = 20,
	metric = 'ROC',
    tuneGrid = data.frame(mtry = c(2:7)),
	trControl = tc_rf)


names(rf.model1$results)
plot(rf.model1)
varImpPlot(rf.model1$finalModel)
print(rf.model1$results)
print(rf.model1$finalModel)
paste0('rejection rate / false positive ', scales::percent(rf.model1$finalModel$confusion[2,3]))
paste0('bad acceptance rate / false negative ', scales::percent(rf.model1$finalModel$confusion[1,3]))


# getTree(rf.model1$finalModel

# library(tree)
tree <- getTree(rf.model1$finalModel, 1, labelVar=TRUE)
reprtree:::plot.getTree(rf.model1$finalModel$tree, 1, labelVar = TRUE)