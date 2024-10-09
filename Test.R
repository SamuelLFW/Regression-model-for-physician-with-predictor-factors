


df_case = read.table( "CDI.txt"   ,header = FALSE, sep = "", dec = ".")
colnames(df_case) = c("id", "county", "state", "land_area", "population",
                      "age_18_24", "age_65", "num_phys", "num_bed", "crimes",
                      "high_school", "bachelor", "below_poverty",
                      "unemployment", "per_capital_income", "personal_income",
                      "geog_region") 

head(df_case)

#Discard categorical variables
df_case_reduced = df_case[ ,-c(1, 2, 3, 17)]



sum(df_case_reduced$bachelor >= 50) / 440


#Fit the full model
case_mlr1 = lm( num_phys ~ ., data=df_case_reduced)
summary(case_mlr1) 


#We choose 5 variables based on their significant level on the full model
#We use variables that are significant in full model as our null model
#(Has 3 stars in full model)


#Fit the null model
null_model = df_case_reduced[, c(2 ,5, 6 , 9, 12, 13)   ] 
head(null_model)

null_mlr = lm( num_phys ~ ., data=null_model)
summary(null_mlr)

null_modelwith2 = null_model[, c(2,3, 5)]
null_mlrwith2 = lm( num_phys ~ ., data=null_modelwith2)
null_mlrwith2sqrt = lm( sqrt(num_phys) ~ ., data=null_modelwith2)
###anova(null_mlrwith2, case_mlr1)


#Test if null model is adequate by performing partial F-test 
anova(null_mlr, case_mlr1)

#If we set alpha = 0.05
#Can not reject null so null model is adequate



#Unusual Observations:
#Test if there's any high leverage point:

leverages = lm.influence(null_mlr)$hat
head(leverages)
n = dim(null_model)[1]; # Sample size
n

p = length(variable.names(null_mlr)); 
p

leverages_high = leverages[leverages>2*p/n]
leverages_high

library(faraway)
halfnorm(leverages, 6, labs=as.character(1:length(leverages)), ylab="Leverages")
#We observe that we have 40 high-leverage points, which are about 40/440 = 9.1% 
#of the observations. Observations 1 and 2 are EXTREMELY far from 
#the rest as we can see in the half-normal plot and based on test as well.

#Find good or bad high leverage point
# Calculate the IQR for the dependent variable 
IQR_y = IQR(null_model$num_phys)

#Define a range with its lower limit being (Q1 - IQR) and upper limit being (Q3 + IQR) 
QT1_y = quantile(null_model$num_phys,0.25)
QT3_y = quantile(null_model$num_phys,0.75)

lower_lim_y = QT1_y - IQR_y
upper_lim_y = QT3_y + IQR_y

vector_lim_y = c(lower_lim_y,upper_lim_y)

# Range for y variable 
vector_lim_y

# Extract observations with high leverage points from the original data frame 
highlev = null_model[leverages>2*p/n,]

# Select only the observations with leverage points outside the range 
highlev_lower = highlev[highlev$num_phys < vector_lim_y[1], ]
highlev_upper = highlev[highlev$num_phys > vector_lim_y[2], ]
highlev_new = rbind(highlev_lower,highlev_upper)
highlev_new

#We have the following bad high leverage points



#Find outliers

n = dim(null_model)[1]; # Sample size
n

p = length(variable.names(null_mlr)); 
p
nullmodel_resid = rstudent(null_mlr); 

bonferroni_cv = qt(.05/(2*n), n-p-1) 
bonferroni_cv

nullmodel_resid_sorted = sort(abs(nullmodel_resid), decreasing=TRUE)[1:10]
print(nullmodel_resid_sorted)

outliers = nullmodel_resid_sorted[abs(nullmodel_resid_sorted) > abs(bonferroni_cv)]
print(outliers)


#Find high influential point
cooks = cooks.distance(null_mlr)
sort(cooks[cooks > 1], decreasing = TRUE)

#Only Observation #1 has cook's distance greater than 1. That means we have 1 high
#influential point.



#Test Constant Variance
plot(null_mlr, which=1)

plot(null_mlrwith2sqrt, which=1)
bptest(null_mlrwith2sqrt)

library(lmtest)
bptest(null_mlr)

#Test Normality 
plot(null_mlr, which=2)
hist(null_mlr$residuals)

ks.test(null_mlr$residuals, "pnorm")


#Fix the normality by using boxcox transformation
library(MASS)
null_model_transformation = boxcox(null_mlr, lambda=seq(-2, 2, length=400))


lambda = null_model_transformation$x[which.max(null_model_transformation$y) ]
lambda

fixed_null_mlr = lm( (num_phys^lambda - 1 )/lambda ~ ., data=null_model)
#fixed_null_mlr = lm(  log(num_phys) ~ ., data=null_model)
ks.test(fixed_null_mlr$residuals, "pnorm")
