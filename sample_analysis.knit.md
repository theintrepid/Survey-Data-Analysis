---
title: ""
author: ""
date: ""
output: html_document
---

### A sample analysis (complete with fake data)

What we can achieve from EFA:

1. Explore the **actual correlations** between our items; not just theorectical correlations (some questionnaire items might end up capturing none or more than one trait and we can't perform hypothesis testing with that item)
2. It allows us to trim away some items or construct new factors (traits) to explain our data
3. Sometime there are traits that are not measured at all properly by all our survey questions (and we have to add some)

- General workflow:
1. PCA screening
2. EFA
3. Regression modeling

#### Generate random survey data (n=500)

- Q1-3 are highly correlated (pearson's r > 0.70)
- Q3-6 are moderately correlated (r < 0.60)
- Q7-9 are noise (r ~ 0)
- Q10-12 are highly correlated (r > 0.70)
- Q13-15 are moderately correlated (r < 0.60)

1. The data is designed such that Q1-6 measure one component (e.g. Knowledge) and Q10-15 measure another component (e.g. Perceptions). 
2. The first 3 questions in each group are high quality and correlate highly with each other
3. The next 3 questions in each group are lower quality and only moderately correlated with each other
4. Q7-9 are pure noise and represent very poorly designed variables


                                 # check correlation = rho

```r
gen.data <- function(seed) { #Create fake data
        set.seed(seed)
        x <- sample(1:3, 500, replace=TRUE)
        x2 <- x - sample(1:2, 500, replace=TRUE)
        x3 <- x - sample(1:2, 500, replace=TRUE)
        x4 <- mod.cor(x,r=0.5)
        x5 <- mod.cor(x,r=0.49)
        x6 <- mod.cor(x,r=0.48)

        z <- sample(-3:3, 500, replace=TRUE) ####Noise
        z2 <- sample(-3:3, 500, replace=TRUE)
        z3 <- sample(-3:3, 500, replace=TRUE)
        
        y <- sample(-3:-1, 500, replace=TRUE) 
        y2 <- y + sample(1:2, 500, replace=TRUE)
        y3 <- y + sample(1:2, 500, replace=TRUE)
        y4 <- mod.cor(y, r=0.50)
        y5 <- mod.cor(y, r=0.51)
        y6 <- mod.cor(y, r=0.53)
        
        q <- -3:3
        data <- as.data.frame(list(Q1=factor(x,levels=levels(factor(q))),
                                Q2=factor(x2,levels=levels(factor(q))),
                                Q3=factor(x3,levels=levels(factor(q))),
                                Q4=factor(x4,levels=levels(factor(q))),
                                Q5=factor(x5,levels=levels(factor(q))),
                                Q6=factor(x6,levels=levels(factor(q))),
                                Q7=factor(z,levels=levels(factor(q))),
                                Q8=factor(z2,levels=levels(factor(q))),
                                Q9=factor(z3,levels=levels(factor(q))),
                                Q10=factor(y,levels=levels(factor(q))),
                                Q11=factor(y2,levels=levels(factor(q))),
                                Q12=factor(y3,levels=levels(factor(q))),
                                Q13=factor(y4,levels=levels(factor(q))),
                                Q14=factor(y5,levels=levels(factor(q))),
                                Q15=factor(y6,levels=levels(factor(q)))))
        data                   
}
data <- gen.data(1)
```

#### View correlation matrix

As terms a non-linear (i.e. likert data), we will use a **polychoric correlation matrix**


```
## Loading required package: mvtnorm
## Loading required package: sfsmisc
```

```
## 
## Two-Step Estimates
## 
## Correlations/Type of Correlation:
##           Q1         Q2         Q3         Q4         Q5         Q6
## Q1         1 Polychoric Polychoric Polychoric Polychoric Polychoric
## Q2    0.9475          1 Polychoric Polychoric Polychoric Polychoric
## Q3    0.9445     0.8289          1 Polychoric Polychoric Polychoric
## Q4    0.5457      0.475     0.4473          1 Polychoric Polychoric
## Q5    0.5343     0.4475     0.4136     0.1712          1 Polychoric
## Q6    0.5175     0.4594     0.4242     0.2643      0.237          1
## Q7   0.01139  -0.006907   0.006316    0.05562   -0.00791   -0.03996
## Q8  -0.03286   -0.06372   -0.01247    0.08527   -0.09009    0.02784
## Q9   0.02232   -0.01486    0.03747   -0.03364    0.01842    0.06286
## Q10   0.0937    0.09774     0.1037    0.02951    0.02371    0.09289
## Q11  0.08607    0.08753    0.09935  -0.007032     0.0239     0.1249
## Q12  0.07851    0.07987    0.09232     0.0291    0.02462    0.06994
## Q13 -0.01584    0.02452   -0.03118   -0.02919  -0.007726   0.008942
## Q14  0.02269    0.01193    0.03716    0.06043    0.01164    0.03597
## Q15   0.0964      0.089    0.08698    0.07043   -0.04817    0.06938
##             Q7         Q8         Q9        Q10        Q11        Q12
## Q1  Polychoric Polychoric Polychoric Polychoric Polychoric Polychoric
## Q2  Polychoric Polychoric Polychoric Polychoric Polychoric Polychoric
## Q3  Polychoric Polychoric Polychoric Polychoric Polychoric Polychoric
## Q4  Polychoric Polychoric Polychoric Polychoric Polychoric Polychoric
## Q5  Polychoric Polychoric Polychoric Polychoric Polychoric Polychoric
## Q6  Polychoric Polychoric Polychoric Polychoric Polychoric Polychoric
## Q7           1 Polychoric Polychoric Polychoric Polychoric Polychoric
## Q8     0.06983          1 Polychoric Polychoric Polychoric Polychoric
## Q9     -0.0456    0.01921          1 Polychoric Polychoric Polychoric
## Q10    0.01569   -0.02711     0.0755          1 Polychoric Polychoric
## Q11    0.03139   -0.06391    0.09345      0.951          1 Polychoric
## Q12    0.01094    0.00709    0.02712     0.9486     0.8536          1
## Q13    0.02977    0.01415    0.01995     0.5438     0.4453     0.4352
## Q14   -0.02799    0.03933   -0.02178     0.5401     0.4493     0.4272
## Q15  0.0008559  -0.005258    0.05926     0.5742     0.4932     0.4808
##            Q13        Q14        Q15
## Q1  Polychoric Polychoric Polychoric
## Q2  Polychoric Polychoric Polychoric
## Q3  Polychoric Polychoric Polychoric
## Q4  Polychoric Polychoric Polychoric
## Q5  Polychoric Polychoric Polychoric
## Q6  Polychoric Polychoric Polychoric
## Q7  Polychoric Polychoric Polychoric
## Q8  Polychoric Polychoric Polychoric
## Q9  Polychoric Polychoric Polychoric
## Q10 Polychoric Polychoric Polychoric
## Q11 Polychoric Polychoric Polychoric
## Q12 Polychoric Polychoric Polychoric
## Q13          1 Polychoric Polychoric
## Q14     0.2502          1 Polychoric
## Q15     0.2681     0.2911          1
```

#### Sample a training set (n=200)


```r
index <- sample(nrow(data), 200)
train <- data[index, ]
test <- data[-index,]
```

#### Step 1a. Generate items

- Create items (i.e. Questions) that supposedly tap into the target construct
- Questions should be as expansive as possible: maximize face value
- Redundancy at this stage is fine but missing items is serious

#### Step 1b Screening items

Non-linear principal components analysis with Promax rotation is performed:


```r
library(psych)
train.cor <- hetcor(train,ML=F,std.err=F)
#### Run PCA
pca <- principal(train.cor$correlations, nfactors=10, rotate="promax")
pca
```

```
## Principal Components Analysis
## Call: principal(r = train.cor$correlations, nfactors = 10, rotate = "promax")
## Standardized loadings (pattern matrix) based upon correlation matrix
##       PC2   PC1   PC6   PC4   PC8   PC9  PC10   PC5   PC3   PC7   h2
## Q1   0.94 -0.04  0.07 -0.02  0.02  0.14  0.00  0.00  0.01  0.07 1.00
## Q2   0.89 -0.19  0.07  0.00  0.13  0.09  0.07  0.02 -0.15  0.14 0.92
## Q3   0.96 -0.10  0.17 -0.08  0.03  0.04  0.02  0.00  0.02 -0.05 0.88
## Q4   0.24  0.14 -0.13  0.04 -0.08  0.95 -0.03 -0.01  0.05 -0.08 0.96
## Q5   0.71  0.22 -0.39  0.07 -0.11 -0.18 -0.04 -0.01  0.06 -0.10 0.88
## Q6   0.15  0.10 -0.08  0.02 -0.06 -0.08 -0.04 -0.01  0.05  0.95 0.98
## Q7   0.02  0.02 -0.01  0.00 -0.01 -0.01 -0.02  1.00  0.01 -0.01 1.00
## Q8  -0.07 -0.08  0.06 -0.02  0.05  0.05  0.03  0.01  0.97  0.05 0.99
## Q9  -0.07 -0.06  0.06  1.01  0.03  0.04  0.02  0.00 -0.02  0.02 0.99
## Q10 -0.07  0.88  0.11 -0.05  0.10  0.02  0.08  0.01 -0.03  0.04 0.99
## Q11 -0.13  0.86  0.03 -0.01  0.00  0.07  0.15  0.04 -0.06  0.08 0.90
## Q12 -0.07  0.98  0.07 -0.01  0.04  0.09 -0.13 -0.03 -0.01  0.00 0.92
## Q13  0.11  0.16 -0.10  0.03  0.98 -0.08 -0.03 -0.01  0.05 -0.06 0.98
## Q14  0.06  0.09 -0.05  0.02 -0.03 -0.03  0.97 -0.01  0.03 -0.04 0.99
## Q15  0.25  0.29  0.93  0.07 -0.11 -0.15 -0.05 -0.01  0.06 -0.09 0.90
##          u2
## Q1  0.00387
## Q2  0.07668
## Q3  0.12378
## Q4  0.04470
## Q5  0.12295
## Q6  0.01992
## Q7  0.00099
## Q8  0.01234
## Q9  0.00897
## Q10 0.00578
## Q11 0.10027
## Q12 0.08193
## Q13 0.02336
## Q14 0.01235
## Q15 0.09551
## 
##                        PC2  PC1  PC6  PC4  PC8  PC9 PC10  PC5  PC3  PC7
## SS loadings           3.33 2.88 1.05 1.01 1.01 0.98 1.02 1.00 1.00 0.98
## Proportion Var        0.22 0.19 0.07 0.07 0.07 0.07 0.07 0.07 0.07 0.07
## Cumulative Var        0.22 0.41 0.48 0.55 0.62 0.68 0.75 0.82 0.89 0.95
## Proportion Explained  0.23 0.20 0.07 0.07 0.07 0.07 0.07 0.07 0.07 0.07
## Cumulative Proportion 0.23 0.44 0.51 0.58 0.65 0.72 0.79 0.86 0.93 1.00
## 
##  With component correlations of 
##        PC2   PC1   PC6   PC4   PC8   PC9  PC10   PC5   PC3  PC7
## PC2   1.00  0.10 -0.12  0.11 -0.21  0.22 -0.07 -0.04 -0.01 0.32
## PC1   0.10  1.00  0.26  0.08  0.35 -0.03  0.43  0.00  0.03 0.04
## PC6  -0.12  0.26  1.00 -0.06  0.39  0.33  0.29  0.03 -0.04 0.20
## PC4   0.11  0.08 -0.06  1.00 -0.05 -0.08 -0.02  0.01  0.06 0.08
## PC8  -0.21  0.35  0.39 -0.05  1.00  0.11  0.24  0.07  0.04 0.07
## PC9   0.22 -0.03  0.33 -0.08  0.11  1.00  0.16 -0.02  0.02 0.29
## PC10 -0.07  0.43  0.29 -0.02  0.24  0.16  1.00 -0.06  0.10 0.03
## PC5  -0.04  0.00  0.03  0.01  0.07 -0.02 -0.06  1.00  0.18 0.02
## PC3  -0.01  0.03 -0.04  0.06  0.04  0.02  0.10  0.18  1.00 0.03
## PC7   0.32  0.04  0.20  0.08  0.07  0.29  0.03  0.02  0.03 1.00
## 
## Test of the hypothesis that 10 components are sufficient.
## 
## The degrees of freedom for the null model are  105  and the objective function was  38.13
## The degrees of freedom for the model are 0  and the objective function was  28.77 
## 
## Fit based upon off diagonal values = 0.99
```

#### Step 1b Screening items (cont)

- We look for variables that load clearly onto one component
- Try to minimize cross-loading: i.e. high loadings on >1 component
- A cut-off that is generally accepted (but should be reported)
1. `>`0.50-0.60 on main factor and <0.20-0.30 on others
2. We can discard questions that don't meet this criteria
- *Make sure that the results make theorectical sense!*

#### Step 2. Exploratory Factor Analysis using test data (n=300)

1. Determine number of factors underlying the data
2. Identify which variables load on to which factors
3. Remove variables which do not load onto any factors

*New data should be used*, we use that data that was not used just now (n=300)

#### Step 2. EFA: Parallel analysis







