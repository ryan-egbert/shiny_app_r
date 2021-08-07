library(caret)
library(randomForest)

data = read.table("data/class-list.csv", header=T, sep=",",
                  colClasses=c("numeric", "factor", "factor", "factor",
                               "factor", "factor"))

summary(data)
names(data) = c("id", "major", "y", "fav1", "fav2", "least")
plot(data$fav1)

data$id = NULL
data$major = NULL

dummies = dummyVars(y ~ ., data=data)
ex = data.frame(predict(dummies, newdata=data))

names(ex) = gsub("\\.", "", names(ex))
ex
data = cbind(data$y, ex)
names(data)[1] = "y"

rm(dummies, ex)

descrCorr = cor(data[,2:ncol(data)])
highCorr = sum(abs(descrCorr[upper.tri(descrCorr)]) > 0.85)

highCorr

highCorrDescr = findCorrelation(descrCorr, cutoff=0.85)
highCorrDescr
filteredDescr = data[,2:ncol(data)][,-highCorrDescr]

data = cbind(data$y, filteredDescr)
names(data)[1] = "y"

rm(descrCorr, filteredDescr, highCorr, highCorrDescr)

y = data$y
data = cbind(rep(1, nrow(data)), data[2:ncol(data)])
names(data)[1] = "ones"

comboInfo = findLinearCombos(data)
comboInfo

data = data[, -comboInfo$remove]
data$ones = NULL
data = cbind(y, data)
data

rm(y, comboInfo)


fit = train(data[,2:ncol(data)], data$y)

predict(fit, data[,2:ncol(data)])

d = data[1,2:ncol(data)]

d[1,3] = 1; d[1,5] = 0
d[1,10] = 0; d[1,12] = 1
d[1,14] = 1;
d
predict(fit, d)
