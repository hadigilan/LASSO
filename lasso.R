
#
# 	Installing & loading the package
#

install.packages("glmnet")						# installing the package
library(glmnet)								# loading the package



#
#	Importing data
#


mydata <- read.csv("C:/cash.csv")
head(mydata)
tail(mydata)
dim(mydata)


#
#	splitting the dataset
#


set.seed (1)
train <- sample (1: nrow(mydata), 0.8*nrow (mydata))


#
#	OLS regression
#

ols.lm<- lm( CASH_ASSET ~ SIZE + MTB + LEV + 
			DIV + CFO + VOL + RD + LIQ + CAPEX + TANG + AGE + SG, data=mydata[train,])
summary(ols.lm)

pred.ols<- predict(ols.lm, newdata=mydata[ -train, ])
mean((mydata$CASH_ASSET[ -train ] - pred.ols )^2)



#
#	LASSO regression
#

x<- as.matrix(mydata[,4:15])
y<- mydata$CASH_ASSET
lam.grid <- 10^ seq (10,-2, length =100)

set.seed(1)
lasso.glmnet<- cv.glmnet(x=x[train,], y=y[train], alpha=1, lambda=grid )
bestlam<- lasso.cv.glmnet$lambda.min

pred.lasso<- predict(lasso.glmnet, s=bestlam, newx =x[-train ,])
mean((y[ -train ] - pred.lasso)^2)
