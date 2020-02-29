# Данные из: Ramsey F., Schafer D. The Statistical
# Sleuth: A Course in Methods of Data Analysis, 
# 3rd Edition, 2013

height = c(100, 200, 300, 450, 600, 800, 1000)
distance = c(253, 337, 395, 451, 495, 534, 574)

# Модель в форме квадратичного полинома
lm.r = lm(distance ~ height + I(height^2))

summary(lm.r)
# Call:
#   lm(formula = distance ~ height + I(height^2))
# 
# Residuals:
#   1       2       3       4       5       6       7 
# -14.420   9.192  13.624   2.060  -6.158 -12.912   8.614 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.002e+02  1.695e+01  11.811 0.000294 ***
#   height       7.062e-01  7.568e-02   9.332 0.000734 ***
#   I(height^2) -3.410e-04  6.754e-05  -5.049 0.007237 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 13.79 on 4 degrees of freedom
# Multiple R-squared:  0.9902,	Adjusted R-squared:  0.9852 
# F-statistic: 201.1 on 2 and 4 DF,  p-value: 9.696e-05

# Создадим высоты для прогноза
h = seq(100, 1000, 10)
# Вычислим расстояния для каждой из новых высот
dist = 200.211950 + 0.706182*h - 0.000341*h^2

plot(height, distance)  # исходные данные
lines(h, dist, lty=1)   # показать результаты подгонки
