## Data analysis script
## Authored by Wesley on 12 Jan 2015

#-----------Utility functions--------------

install.packages("psych")
install.packages("polycor")
install.packages("random.polychor.pa")
isntall.packages("GPArotation")

library("psych")
library("polycor")
library("random.polychor.pa")
library("GPArotation")

balancer <- function(data, cols) { #Function to introduce response categories
        balance <- data.frame(matrix(c(1:5),nrow=5,ncol=cols))
        names(balance) <- names(data)
        data <- rbind(data,balance)
        #data <- data[sample(nrow(data),replace=F),]  # reshuffles data (turned off)
        data
}

#-------------Main----------------------

data <- read.csv("filepath") #reads csv file into R

index <- sample(nrow(data), 150) #samples 150 rows into the PCA screening set
train <- data[index, ]
test <- data[-index,]
test.original <- test
train <- balancer(data=train,cols=ncol(data)) #Introduces equal number of response categories for Parallel Analysis
test <- balancer(data=test,cols=ncol(data)) #Introduces equal number of response categories for Parallel Analysis

#--------------Screening via PCA---------------

train.cor <- hetcor(train,ML=T,std.err=F)
#### Run PCA
pca <- principal(train.cor$correlations, nfactors=10, rotate="promax")
pca #Prints PCA results

remove <- c() #Place questions that should be removed here
test <- test[,-remove] #remove questions from test data set

#--------------Parallel Analysis---------------

random.polychor.pa(nrep=500, data.matrix=data.matrix(test), q.eigen = 0.95, r.seed = 1, 
                   diff.fact=FALSE, distr="NULL", comparison = "random", 
                   fit.pa=FALSE, print.all=FALSE) #Horn's Parallel Analysis via Monte Carlo Simulation with 500 replicates
#Set significance of factors at 95th quantile

number <- #set number of factors identified by Parallel Analysis HERE
#--------------Exploratory factor analysis (screening)--------------

test <- test.original #restores unbalanced version
test.cor <- hetcor(test, ML=F, std.err=F)
test.fa <- fa(r=test.cor$correlations,nfactors=number,rotate="promax")
test.fa #Prints EFA screening results

remove <- c() #Place questions that should be removed here
test.final <- test[,-remove]

#----------------Exploratory factor analysis (final)----------------
test.final.cor <- hetcor(test.final, ML=F, std.err=F)
test.final.fa <- fa(r=test.final.cor$correlations,nfactors=number,rotate="promax")
test.final.fa

#---------------Obtain factor scores-----------------

scores <- factor.scores(data.matrix(test.final),f=test.final.fa,method="Anderson")$scores 
#Alternative methods include "regression", "Thurstone", "tenBerge", "Bartlett", "Harman","components"
test.wscores <- cbind(test.final,scores)

#----------------Rename factors----------------
names(test.wscores)[names(test.wscores)=="MR1"] <- "Factor1"
#names(test.wscores)[names(test.wscores)=="MR2"] <- "Factor2"

#---------------Output csv file--------------

write.csv(test.wscores,file="output.csv")