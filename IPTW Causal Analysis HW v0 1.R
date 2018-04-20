#Load the Packages

#install.packages("tableone")
#install.packages("Matching")
#install.packages("ipw")
#install.packages("survey")
#install.packages("MatchIt")
library(tableone)
library(Matching)
library(ipw)
library(survey)

#Load matchit package and retrieve lalonde dataset
library(MatchIt)
data(lalonde)
# 
# The data have n=614 subjects and 10 variables
# 
# age age in years.
# educ years of schooling.
# black indicator variable for blacks.
# hispan indicator variable for Hispanics.
# married indicator variable for marital status.
# nodegree indicator variable for high school diploma.
# re74 real earnings in 1974.
# re75 real earnings in 1975.
# re78 real earnings in 1978.
# treat an indicator variable for treatment status.
# The outcome is re78 - post-intervention income.
# 
# The treatment is treat - which is equal to 1 if the subject received the labor training and 
# equal to 0 otherwise.
# 
# The potential confounding variables are: age, educ, black, hispan, married, nodegree, re74,
# re75.
# 
# Fit a propensity score model. Use a logistic regression model, where the outcome is 
# treatment. Include the 8 confounding variables in the model as predictors, with no interaction
# terms or non-linear terms (such as squared terms). Obtain the propensity score for each 
# subject. Next, obtain the inverse probability of treatment weights for each subject.

#label confounders
xvars<-c("age","educ","black","hispan","married","nodegree","re74","re75")

#propensity score model
psmodel <- glm(treat ~ age + educ + black + hispan + married + nodegree + re74 + re75,
               family  = binomial(link ="logit"), data=lalonde)

## value of propensity score for each subject
ps <-predict(psmodel, type = "response")

#create weights
weight<-ifelse(lalonde$treat==1,1/(ps),1/(1-ps))

#What are the minimum and maximum weights?
summary(weights)

# Find the standardized differences for each confounder on the weighted (pseudo) population. 

#apply weights to data
weighteddata<-svydesign(ids = ~ 1, data =lalonde, weights = ~ weight)

#weighted table 1
weightedtable <-svyCreateTableOne(vars = xvars, strata = "treat", 
                                  data = weighteddata, test = FALSE)

# What is the standardized difference for nodegree?
## Show table with SMD
print(weightedtable, smd = TRUE)

# Using IPTW, find the estimate and 95% confidence interval for the average causal effect.
# This can be obtained from svyglm

#fit a marginal structural model (risk difference)
msm <- (svyglm(re78 ~ treat, design = svydesign(~ 1, weights = ~weight, data =lalonde)))
coef(msm)
confint(msm)


# Now truncate the weights at the 1st and 99th percentiles. 
# This can be done with the trunc=0.01 option in svyglm.
#first fit propensity score model to get weights
weightmodel<-ipwpoint(exposure= treat, family = "binomial", link ="logit",
                      denominator= ~ age + educ + black + hispan + married + nodegree + re74 + re75,
                      data=lalonde, trunc=0.02)

#truncated weights
new_weight<-weightmodel$weights.trunc
summary(new_weight)

#fit a marginal structural model (risk difference) with truncated weights
msm_tw <- (svyglm(re78 ~ treat, design = svydesign(~ 1, weights = ~new_weight,
                                                    data =lalonde)))
coef(msm_tw)
confint(msm_tw)
