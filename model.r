library(caret)
library(mlbench)
library(rpart)

# ---------------------------------------
# ----TRAIN / TEST SLPIT ----------------

inTrain <- createDataPartition(df.lead$preq,
                               p = 0.80)

train <- df.lead[inTrain[[1]],]
test  <- df.lead[-inTrain[[1]],]

# ---------------------------------------
# ---- rcart model       ----------------

tc_rpart <- trainControl(
	'repeatedcv',
	number = 5,
	repeats = 1,
	savePred = TRUE,
	classProbs = TRUE
	)

formula <- 'Sales'

df.model <- train[,c('preq', 'Sales')]
set.seed(9)
rpart.model <- train(
	preq ~ .,
	data = df.model,
	# preq ~ Sales,
	method = 'rpart',
	tuneLength = 20,
	# metric = 'ROC',
	metric = 'Accuracy',
	# minsplit = 1,
	# minbucket = 25,
	# cp = 0.0002,
	# preProcess = 'medianImpute',
	# type = 'class',
	# control = rpart.control(maxdepth = 2),
	control = rpart.control(minsplit = 1, minbucket=25, cp = 0.000051),
	trControl = tc_rpart)

tree1 <- rpart(preq ~ Sales + Sector.and.Perishability, data= train, method='class',
               control = rpart.control(minsplit = 1, minbucket=25, cp = 0.0051))
library(rpart.plot)
rpart.plot(tree1)



# nicer looking treeplot:

# library(rattle)
# fancyRpartPlot(t$finalModel)