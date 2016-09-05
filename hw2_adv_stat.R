# rm(list=ls())

setwd("/Users/audreyhendricks/Desktop/CU-Denver/teaching/Fall 2016/6388/Lectures/CHAPTER2")

# read in full data and 
# then reduce to 1's and 9's for hw2 (Last name is Than)

# dat <- read.table("zip.train")
dat <- read.table(file.choose())
dim(dat)
table(dat[,1])
names(dat)

index <- dat[,1] == 1 | dat[,1]==9
dat.hw2 <- dat[index,]
dim(dat.hw2)	
table(dat.hw2[,1])
dat<-dat.hw2
rm(dat.hw2)

# test <- read.table("zip.test")
test <- read.table(file.choose())
dim(test)
index <- test[,1] == 1 | test[,1]==9
test <- test[index,]
dim(test)	
table(test[,1])




# you can ignore this code if you like
# it was for the benefit of lecture and 
# plots to consider

ans <- princomp(dat[,2:257])
col <- rep("orange",1389)
col[(dat[,1] == 1)] <- "blue"
table(col, dat[,1])
plot(ans$scores[,1:10], col=col, xlab="PC1", ylab="PC2",
     main="Blue = 1, Orange=9")

library(scatterplot3d)
scatterplot3d(ans$scores[,1:10], color=col, xlab="PC1", ylab="PC2", zlab="PC3",
              main="Blue = 1, Orange=9")

# fit a linear regression
# I am using Y = 1 to indicate the number 2

Y <- 1*(dat[,1] == 1)
ans <- lm(Y ~ . - V1, data=dat) # fits ALL variables except of course V1!
summary(ans)
names(ans)

# now let's do the classification on the training data set
# for the linear regression
#redo# there were 8 errors out of 1389 on the training data 
#redo# there were 15 errors out of 364 on the test data

reg.class.test <- predict.lm(ans, newdata=test)
reg.class.test <- 1*(reg.class.test > 0.5)
Y.test <- 1*(test[,1]==1)
table(Y.test, reg.class.test) # results for test set 

reg.class.training <- 1*(ans$fitted.values > 0.5)
table(Y, reg.class.training)[2:1,2:1] # results for train set 
 
# and now for the neighest neighbor
# the following summarizes the errors for k nearest neighbors
# k error measured on  # errors out of X
# 1     training       0               1389
# 1     test           9               364
# 3     training       7               1389
# 3     test           11              364
# 5     training       8               1389
# 5     test           11              364
# 7     training       9               1389
# 7     test           12              364
# 15    training       13              1389
# 15    test           14              364

library(class)
?knn
train.dat <- dat[,2:257]
train.dat <- dat[,-1]
test.dat  <- test[,2:257]

###########  k = 1 ###########
ans.knn.1 <- knn(train = train.dat, test=train.dat, 
                 k=1, cl=dat[,1])
table(dat[,1], ans.knn.1) # results for train set
ans.knn.1 <- knn(train = train.dat, test=test.dat, 
                 k=1, cl=dat[,1])
table(test[,1], ans.knn.1) # results for test set

###########  k = 3 ###########
##  training  misclafication##
ans.knn.3 <- knn(train = train.dat, test=train.dat, 
                 k=3, cl=Y)
table(Y, ans.knn.3)
##  test  misclafication##
ans.knn.3 <- knn(train = train.dat, test=test.dat, 
                 k=3, cl=Y)
table(Y.test, ans.knn.3)

###########  k = 5 ###########
ans.knn.5 <- knn(train = train.dat, test=train.dat, 
                 k=5, cl=Y)
table(Y, ans.knn.5)
ans.knn.5 <- knn(train = train.dat, test=test.dat, 
                 k=5, cl=Y)
table(Y.test, ans.knn.5)

###########  k = 7 ######
#####
ans.knn.7 <- knn(train = train.dat, test=train.dat, 
                 k=7, cl=Y)
table(Y, ans.knn.7)
ans.knn.7 <- knn(train = train.dat, test=test.dat, 
                 k=7, cl=Y)
table(Y.test, ans.knn.7)

###########  k = 15 ###########
ans.knn.15 <- knn(train = train.dat, test=train.dat, 
                  k=15, cl=Y)
table(Y, ans.knn.15)
ans.knn.15 <- knn(train = train.dat, test=test.dat, 
                  k=15, cl=Y)
table(Y.test, ans.knn.15)



#########  much easier with a loop!  ############

err.out<-data.frame()  ##establishing the looping var
for (i in c(1,3,5,7,15)){
  ans.knn.temp.train <- knn(train = train.dat, test=train.dat,
                            k=i, cl=Y)
  tab.train<-table(Y, ans.knn.temp.train)
  train.err<-(tab.train[2,1]+tab.train[1,2])/sum(tab.train)
  
  ans.knn.temp.test <- knn(train = train.dat, test=test.dat,
                           k=i, cl=Y)
  tab.test<-table(Y.test, ans.knn.temp.test)
  test.err<-(tab.test[2,1]+tab.test[1,2])/sum(tab.test)
  
  err.out<-rbind(err.out,c(i,train.err, test.err))
}

names(err.out)<-c("k", "training", "testing")



# let's put this together in a plot like Figure 2.4 of the text

#	train <- c(0, 7, 8, 9, 13)/1389
#	test  <- c(9, 11, 11, 12, 14)/364
#   k <- c(1,3,5,7,15)

attach(err.out)

reg.test  <- 15/364
reg.train <- 8/1389

plot(k, testing, xlab="k", ylab="", type="b",
     lty=1, col=1, pch="x", ylim=c(0,0.05),
     main="Error Rate as a Function of k")
lines(k, training, lty=2, col=2, pch="o", type="b")
lines(c(1,15), c(reg.test, reg.test), lty=3, col=3)
lines(c(1,15), c(reg.train, reg.train), lty=4, col=4)
legend(9, .03, c("NN - Test Data","NN - Training Data", "Reg - Test", "Reg - Train"),
       lty=c(0,0,3,4), col=1:4, pch=c("x","o","", ""))

# print out error rates
err.out
reg.train
reg.test

