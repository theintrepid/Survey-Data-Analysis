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


# See Skrondal and Laake 2001
#---------------Obtain factor scores [Explanatory - Regression]-----------------

s.explain <- factor.scores(data.matrix(test.final),
                          f=test.final.fa,method="regression")$scores[,1] 
#Alternative methods include "regression", "Thurstone", "tenBerge", "Bartlett", "Harman","components"

#---------------Obtain factor scores [Response - Bartlett]-----------------

s.response <- factor.scores(data.matrix(test.final),
                          f=test.final.fa,method="Bartlett")$scores[,2]

#--------------------Combine into original data.frame-----------------

test.wscores <- cbind(test.final,s.explain,s.response)

#----------------Rename factors----------------
names(test.wscores)[names(test.wscores)=="MR1"] <- "Factor1"
names(test.wscores)[names(test.wscores)=="MR2"] <- "Factor2"

#---------------Output csv file--------------

write.csv(test.wscores,file="output.csv")

#---------------IRT model score--------------

irt.poly <- polychoric(data.matrix(test.final))
irt.mod <- irt.fa(irt.poly,nfactors=2,rotate="promax",fm="wls")
irt.scores <- score.irt(irt.mod, data.matrix(test.final), 
                        keys=NULL,cut = 0.3,bounds=c(-5,5),mod="logistic")
irt.scores.extracted <- irt.scores[,1:2]
names(irt.scores.extracted) <- c("Factor1","Factor2")

lm(Factor2 ~ Factor1, data=irt.scores.extracted)

#-------------Some useful plots-------------

hist(irt.scores.extracted[,1])
hist(irt.scores.extracted[,2])
plot(irt.mod, type="ICC")
plot(irt.mod, type="IIC")
plot(irt.mod, type="test")

#---------SEM-----------

library(lavaan)

myModel <- '
        factor1 =~ Q1 + Q2 + Q3 + Q4 + Q6
        factor2 =~ Q10 + Q11 + Q12 + Q13 + Q14 + Q15'
cfa(myModel,data=test.final, estimator="WLS")

#-------OpenMx------

require(OpenMx)

manifests <- names(test.final)
latents <- c("Factor1","Factor2")
factorModel <- mxModel("One Factor",
                       type="RAM",
                       manifestVars = manifests,
                       latentVars = latents,
                       mxPath(from=latents, to=manifests),
                       mxPath(from=manifests, arrows=2),
                       mxPath(from=latents, arrows=2,
                              free=FALSE, values=1.0),
                       mxData(cov(demoOneFactor), type="cov",
                              numObs=500))
summary(mxRun(factorModel))