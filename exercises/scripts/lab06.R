library(MASS)
library(ggplot2)
attach(Boston)

# https://datascienceplus.com/how-to-apply-linear-regression-in-r/

## Sample the dataset. The return for this is row nos.
set.seed(1)
row.number <- sample(1:nrow(Boston), 0.8*nrow(Boston))
train <- Boston[row.number,]
test <- Boston[-row.number,]
dim(train)
dim(test)

# Let’s check for the distribution of response variable ‘medv’. 
# The following figure shows the three distributions of ‘medv’
# * original, 
# * log transformation and 
# * square root transformation. 
# We can see that both ‘log’ and ‘sqrt’ does a decent job 
# to transform ‘medv’ distribution closer to normal. 
# Далее мы используем логарифмическое преобразование, 
# хотя можно было и ‘sqrt’.

## Explore the data.
ggplot(train, aes(medv)) + geom_density(fill="blue")
ggplot(train, aes(log(medv))) + geom_density(fill="blue")
ggplot(train, aes(sqrt(medv))) + geom_density(fill="blue")

# Let’s make default model.
model1 <- lm(log(medv)~., data=train)
summary(model1)
par(mfrow=c(2,2))
plot(model1)

## Наблюдения из summary

## Is there a relationship between predictor and response variables?
# We can answer this using F stats. 
# This defines the collective effect of all predictor 
# variables on the response variable. 
# In this model, F=102.3 is far greater than 1, 
# and so it can be concluded that there is a relationship 
# between predictor and response variable.

## Which of the predictor variables are significant?
# Based on the ‘p-value’ we can conclude on this. 
# The lesser the ‘p’ value the more significant is the variable. 
# From the ‘summary’ dump we can see that ‘zn’, ‘age’ and ‘indus’ 
# are less significant features as the ‘p’ value is large for them. 
# In next model, we can remove these variables from the model.

## Is this model fit?
# We can answer this based on R2 (multiple-R-squared) 
# value as it indicates how much variation is captured by the model. 
# R2 closer to 1 indicates that the model explains 
# the large value of the variance of the model and hence a good fit. 
# In this case, the value is 0.7733 (closer to 1) 
# and hence the model is a good fit.
# Но ее можно еще улучшить и/или упростить.

## Observation from the plot

## Fitted vs Residual graph
# Residuals plots should be random in nature 
# and there should not be any pattern in the graph. 
# The average of the residual plot should be close to zero. 
# From the above plot, we can see that the red trend line 
# is almost at zero except at the starting location.

## Normal Q-Q Plot
# Q-Q plot shows whether the residuals are normally distributed. 
# Ideally, the plot should be on the dotted line. 
# If the Q-Q plot is not on the line then models need to be reworked 
# to make the residual normal. In the above plot, 
# we see that most of the plots are on the line except at towards the end.

## Scale-Location
# This shows how the residuals are spread 
# and whether the residuals have an equal variance or not.

## Residuals vs Leverage
# The plot helps to find influential observations. 
# Here we need to check for points that are outside the dashed line. 
# A point outside the dashed line will be influential point 
# and removal of that will affect the regression coefficients.

## Remove the less significant feature
model2 <- update(model1, ~.-zn-indus-age) 
summary(model2) 

par(mfrow=c(2,2))
plot(model2)

## Observation from summary (model2)

## Is there a relationship between predictor and response variable?
# F=148.3 is far greater than 1 and 
# this value is more than the F value of the previous model. 
# It can be concluded that there is a relationship between 
# predictor and response variable.

## Which of the variable are significant?
# Now in this model, all the predictors are significant.

## Is this model fit?
# R2 = 0.7905 is closer to 1 and so this model is a good fit. 
# Please note that this value has decreased a little 
# from the first model but this should be fine 
# as removing three predictors caused a drop 
# from 0.7933 to 0.7905 and this is a small drop. 
# In other words, the contribution of three predictors 
# towards explaining the variance is an only small value (0.0028) 
# and hence it is better to drop the predictor.

## Observation of the plot

# All the four plots look similar to the previous model 
# and we don’t see any major effect.

# In the next step, we will check the residual graph 
# for all significant features from Model 2. 
# We need to check if we see any pattern in the residual plot. 
# Ideally, the residual plot should be random plot and 
# we should not see a pattern. 
# In the following plots, we can see some non-linear pattern 
# for features like ‘crim’, ‘rm’, ‘nox’ etc.

##Plot the residual plot with all predictors.
attach(train)
require(gridExtra)
plot1 <- ggplot(train, aes(crim, residuals(model2))) + geom_point() + geom_smooth()
plot3 <- ggplot(train, aes(nox, residuals(model2))) + geom_point() + geom_smooth()
plot4 <- ggplot(train, aes(rm, residuals(model2))) + geom_point() + geom_smooth()
plot5 <- ggplot(train, aes(dis, residuals(model2))) + geom_point() + geom_smooth()
plot6 <- ggplot(train, aes(rad, residuals(model2))) + geom_point() + geom_smooth()
plot7 <- ggplot(train, aes(tax, residuals(model2))) + geom_point() + geom_smooth()
plot8 <- ggplot(train, aes(ptratio, residuals(model2))) + geom_point() + geom_smooth()
plot9 <- ggplot(train, aes(black, residuals(model2))) + geom_point() + geom_smooth()
plot10 <- ggplot(train, aes(lstat, residuals(model2))) + geom_point() + geom_smooth()
grid.arrange(plot1,plot3,plot4,plot5,plot6,plot7,plot8,plot9,plot10,ncol=5,nrow=2)

# We can now enhance the model by adding a square term 
# to check for non-linearity. 
# We can first try model3 by introducing square terms for all features 
# (from model 2). 
# In the next iteration, we can remove the insignificant feature 
# from the model 3.

# Lets  make default model and add square term in the model.
model3 <- lm(log(medv)~crim+chas+nox+rm+dis+rad+tax+ptratio+
              black+lstat+ I(crim^2)+ I(chas^2)+I(nox^2)+ I(rm^2)+ I(dis^2)+ 
              I(rad^2)+ I(tax^2)+ I(ptratio^2)+ I(black^2)+ I(lstat^2), data=train)
summary(model3)

# Removing the insignificant variables.
model4 <- update(model3, ~.-nox-rad-tax-I(crim^2)-I(chas^2)-I(rad^2)-
                I(tax^2)-I(ptratio^2)-I(black^2))
summary(model4)

par(mfrow=c(2,2))
plot(model4)

## Observation from summary (model4)

## Is there a relationship between predictor and response variables?
# F-Stat is 154.2 and it is far greater than 1. 
# So there is a relationship between predictor and response variable.

## Which of the predictor variable are significant?
# All predictor variables are significant.

## Is this model fit?
# R2 is 0.8123 and this is more (and better) 
# than our first and second model.

## Prediction

# Till now we were checking training-error 
# but the real goal of the model is to reduce the testing error. 
# As we already split the sample dataset into training and 
# testing dataset, we will use test dataset to evaluate the model 
# that we have arrived upon. We will make a prediction 
# based on ‘Model 4’ and will evaluate the model. 
# As the last step, we will predict the ‘test’ observation 
# and will see the comparison between predicted response 
# and actual response value. 
# RMSE explains on an average how much of the predicted value 
# will be from the actual value. 
# Based on RMSE = 3.683, we can conclude that on an average 
# predicted value will be off by 3.683 from the actual value.

pred1 <- predict(model4, newdata = test)
rmse <- sqrt(sum((exp(pred1) - test$medv)^2)/length(test$medv))
c(RMSE = rmse, R2=summary(model4)$r.squared)

par(mfrow=c(1,1))
plot(test$medv, exp(pred1))

# The model that is created still has scope for improvement 
# as we can apply techniques like Outlier detection, 
# Correlation detection to further improve the accuracy 
# of more accurate prediction. 
# 
# One can as well use an advanced technique like Random Forest 
# and Boosting technique to check whether the accuracy 
# can be further improved for the model. 
# A piece of warning is that we should refrain from 
# overfitting the model for training data as the test accuracy 
# of the model will reduce for test data in case of overfitting.


## Prestige

Prestige <- read.csv("Prestige.csv")

# Subset the data to capture only income and education.
my_data <- Prestige[,c(1:2)]
summary(my_data)

hist(my_data$education)
hist(my_data$income)

# На гистограмме income заметны выбросы.
# Попробуем преобразовать распределение в нормальное.

hist(log(my_data$income))
bp <- boxplot.stats(log(my_data$income))

bp$out
# [1] 10.161187 10.138876  6.822197  6.415097

# Выбросов стало заметно меньше. 
# Они есть как в большую, так и в меньшую стороны.

# Верхние выбросы хорошо заметны
plot(my_data$education, my_data$income)

## Построим модель без преобразования распределений

set.seed(1)
# Центрируем education
education.c <- scale(my_data$education, center=TRUE, scale=FALSE)
hist(education.c)

model1 <- lm(income ~ education.c, data = my_data)
summary(model1)
# Call:
#   lm(formula = income ~ education.c, data = my_data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5493.2 -2433.8   -41.9  1491.5 17713.1 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   6797.9      344.9  19.709  < 2e-16 ***
#   education.c    898.8      127.0   7.075 2.08e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3483 on 100 degrees of freedom
# Multiple R-squared:  0.3336,	Adjusted R-squared:  0.3269 
# F-statistic: 50.06 on 1 and 100 DF,  p-value: 2.079e-10

# R2 слишком мал.

# Another interesting point from the model output 
# is the residual standard error which measures the 
# average amount of income that will deviate from the true 
# regression line for any given point. 
# In our example, any prediction of income on the basis 
# of education will be off by an average of $3483. A fairly large number.

# Given that the Residual standard error for income is $3483 
# and the mean income value is $6798, we can assume that 
# the average percentage error for any given point 
# is more than 51%! Again, a pretty large error rate.

plot(education.c, my_data$income, ylab = "income")
abline(coef(model1)[1], coef(model1)[2], col = "red")

# Остатки
plot(model1, pch=16, which=1)

## Построим модель с нормализацией распределения доходов

model2 <- lm(log(income) ~ education.c, data = my_data)
summary(model2)

# Снова низкий R2. Снова выбросы, но теперь по обе стороны от 0.

plot(education.c, log(my_data$income), ylab = "income")
abline(coef(model2)[1], coef(model2)[2], col = "red")

plot(model2, pch=16, which=1)

# Все это убеждает в необходимости использовать 
# остальные предикторы, а если и они не помогут, то 
# нелинейную модель регрессии.

#### Abalone

abalone <- read.csv("Abalone_data.csv")

# Splitting dataset in train and test using 70/30 method
set.seed(42)
indexes <- sample(1:nrow(abalone), size = 0.3 * nrow(abalone))
train <- abalone[-indexes,]
test  <- abalone[indexes,]

pairs(abalone)
cor(abalone[,-1])
#                   Length  Diameter    Height Whole_weight Shucked_weight
# Length         1.0000000 0.9868132 0.8275521    0.9252551      0.8979052
# Diameter       0.9868132 1.0000000 0.8337053    0.9254520      0.8931591
# Height         0.8275521 0.8337053 1.0000000    0.8192087      0.7749568
# Whole_weight   0.9252551 0.9254520 0.8192087    1.0000000      0.9694027
# Shucked_weight 0.8979052 0.8931591 0.7749568    0.9694027      1.0000000
# Viscera_weight 0.9030105 0.8997262 0.7982929    0.9663721      0.9319557
# Shell_weight   0.8976970 0.9053281 0.8173261    0.9553511      0.8826063
# Rings          0.5571226 0.5750054 0.5581087    0.5408179      0.4212556
#                Viscera_weight Shell_weight     Rings
# Length              0.9030105    0.8976970 0.5571226
# Diameter            0.8997262    0.9053281 0.5750054
# Height              0.7982929    0.8173261 0.5581087
# Whole_weight        0.9663721    0.9553511 0.5408179
# Shucked_weight      0.9319557    0.8826063 0.4212556
# Viscera_weight      1.0000000    0.9076469 0.5042735
# Shell_weight        0.9076469    1.0000000 0.6280306
# Rings               0.5042735    0.6280306 1.0000000

# For now we will work with original value for variable Sex 
# which have factor levels of F, I and M.
# The factor level F is reference level for Sex variable.

abalone_add <- lm(Rings ~ Sex+Length+Diameter+Height+ Whole_weight
                  +Shucked_weight+Viscera_weight
                  +Shell_weight, data = train)
summary(abalone_add)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -8.9157 -1.2965 -0.3030  0.8309 12.0132 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     3.946559   0.339445  11.627  < 2e-16 ***
#   SexI           -0.883507   0.120215  -7.349 2.57e-13 ***
#   SexM            0.010608   0.098708   0.107    0.914    
# Length         -0.013912   0.010830  -1.285    0.199    
# Diameter        0.074949   0.013414   5.587 2.52e-08 ***
#   Height          0.045139   0.008130   5.552 3.08e-08 ***
#   Whole_weight    0.038447   0.004365   8.809  < 2e-16 ***
#   Shucked_weight -0.088469   0.004812 -18.385  < 2e-16 ***
#   Viscera_weight -0.050926   0.007771  -6.554 6.61e-11 ***
#   Shell_weight    0.044969   0.006636   6.776 1.49e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.164 on 2914 degrees of freedom
# Multiple R-squared:  0.5341,	Adjusted R-squared:  0.5327 
# F-statistic: 371.2 on 9 and 2914 DF,  p-value: < 2.2e-16

plot(abalone_add)

pred <- predict(abalone_add, test)

RMSE <- function(m, o){
  sqrt(mean((m - o)^2))
}

RMSE(pred, test$Rings)
# [1] 2.272948

abalone_add_poly2 <- lm(log(Rings) ~  Diameter + Length + Height + 
                          poly(Whole_weight,2) + poly(Viscera_weight,2) + 
                          poly(Shucked_weight,2) + poly(Shell_weight,2) + 
                          Sex, data=train)
summary(abalone_add_poly2)
pred <- predict(abalone_add_poly2, test)
RMSE(exp(pred), test$Rings)
# [1] 2.267183

abalone_add_poly2.2 <- lm(log(Rings) ~  Diameter + Length + Height + 
                          poly(Whole_weight,2) + poly(Viscera_weight,1) + 
                          poly(Shucked_weight,2) + poly(Shell_weight,2) + 
                          Sex, data=train)
summary(abalone_add_poly2.2)

plot(abalone_add_poly2.2)

pred <- predict(abalone_add_poly2.2, test)
RMSE(exp(pred), test$Rings)
# [1] 2.268344

abalone_add_poly4 <- lm(log(Rings) ~  Diameter + Length + Height + 
                          poly(Whole_weight,4) + poly(Viscera_weight,4) + 
                          poly(Shucked_weight,4) + poly(Shell_weight,4) + 
                          Sex, data=train)
summary(abalone_add_poly4)

pred <- predict(abalone_add_poly4, test)
RMSE(exp(pred), test$Rings)
# [1] 2.228147

abalone_add_poly4.1 <- lm(log(Rings) ~  Diameter + Height + 
                          poly(Whole_weight,2) + poly(Viscera_weight,2) + 
                          poly(Shucked_weight,2) + poly(Shell_weight,4) + 
                          Sex, data=train)
summary(abalone_add_poly4.1)

pred <- predict(abalone_add_poly4.1, test)
RMSE(exp(pred), test$Rings)
# [1] 2.231105

# Оказывается, что отказаться от логорифмирования - даже лучше.

abalone_add_poly4.2 <- lm(Rings ~  Diameter + Height + 
                            poly(Whole_weight,2) + poly(Viscera_weight,2) + 
                            poly(Shucked_weight,2) + poly(Shell_weight,4) + 
                            Sex, data=train)
summary(abalone_add_poly4.2)

plot(abalone_add_poly4.2)

pred <- predict(abalone_add_poly4.2, test)
RMSE(pred, test$Rings)
# [1] 2.198611

