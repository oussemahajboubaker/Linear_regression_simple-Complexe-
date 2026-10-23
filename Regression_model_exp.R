

data(mtcars)
df<-mtcars[, c("mpg", "wt", "hp")]
head(df)
summary(df)
model1 <- lm(mpg ~ wt, data = df)
model1

#step one the means 
x<- df$wt
y<-df$mpg
n<-length(x)
xbar<-mean(x)
ybar<-mean(y)
c(xbar = xbar, ybar = ybar, n = n)

#step 2 the slope : 
num <- sum((x - xbar) * (y - ybar))
den <- sum((x - xbar)^2)
b1 <- num / den


#step 3 the intercept  : 
b0<- ybar - b1*xbar
c(b1 = b1, b0 = b0)

#  Fitted values, residuals and RSS
y_hat <- fitted(model1)
y_hat
e<- resid(model1)
e


#RSS 
RSS <- sum((y - y_hat)^2)
RSS
mean(e)
#MSE
MSE  <- RSS / n
RMSE <- sqrt(MSE)
MAE  <- mean(abs(y - y_hat))



summary(model1)



sqrt(RSS / (n - 1 - 1))
sigma(model1)


#Matrix form and multiple regression

X <- cbind(1, x)
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
beta_hat



#adding second predict
model2 <- lm(mpg ~ wt + hp, data = df)
summary(model2)


X2 <- model.matrix(model2)
head(X2)


beta2 <- solve(t(X2) %*% X2) %*% t(X2) %*% df$mpg
beta2


predict(model2, newdata = data.frame(wt = 3, hp = 150))

plot(y_hat, e)
abline(h = 0)




#real data 
data(mtcars)
str(mtcars)
summary(mtcars)
table(mtcars$vs)
table(mtcars$am)
table(mtcars$cyl)
table(mtcars$gear)
table(mtcars$carb)

hist(mtcars$mpg, main = "Miles/(US) gallon")
shapiro.test(mtcars$mpg)




pairs(mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")],
      main = "mtcars data", gap = 1/2)
cor(mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")])













