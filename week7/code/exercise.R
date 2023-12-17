require("minpack.lm")
require("ggplot2")
require("dplyr")
library("ggplot2")
library("minpack.lm")
library("dplyr")

rm(list = ls())
graphics.off()

MyData <- read.csv("../data/GenomeSize.csv")

plot(subData$TotalLength, subData$BodyWeight, xlab = "Total Length", ylab = "Body Weight")

ggplot(subData, aes(TotalLength, BodyWeight)) + 
  labs(y = "Body Weight", x = "Total Length") +
  geom_point(size = I(3), colour = "red") + 
  theme_bw()

powMod <- function(x, a, b) {
  return(a * x^b)
}

PowMod2 <- function(z, d, k){
  return(d + k * z)
}

powFit <-
  nlsLM(
    subData$BodyWeight ~ a * subData$TotalLength ^ b,
    data = subData,
    start = list(a = .1, b = .1)
  )

powFit2 <-
  nlsLM(
    BodyWeight ~ powMod(TotalLength, a, b),
    data = subData,
    start = list(a = .1, b = .1)
  )

powFit3 <-
  nlsLM(
    BodyWeight ~ powMod(TotalLength, a, b),
    data = subData,
    start = list(a = 3.94 * 10 ^ -06, b = 2.59)
  )

PowFit4 <-
  nlsLM(
    BodyWeight ~ a * TotalLength ^ b,
    data = subData2,
    start = list(a = 3.94 * 10 ^ -06, b = 2.59)
  )



subData <- subset(MyData, Suborder == "Anisoptera")
subData <- subData[!is.na(subData$TotalLength), ]
Lengths <- seq(min(subData$TotalLength), max(subData$TotalLength), len = 200)

pred2plotPow <- powMod(Lengths, coef(powFit)["a"], coef(powFit)["b"])
plot(subData$TotalLength, subData$BodyWeight)
lines(Lengths, pred2plotPow, col = "blue", lwd = 2.5)

summary(powFit2)
print(confint(powFit2))

ggplot(subData, aes(x = TotalLength, y = BodyWeight)) +
  geom_point(shape = I(1), size = 2) +
  labs(x = "Total Length", y = "Body Weight") +
  geom_line(
    data = data.frame(TotalLength = Lengths, BodyWeight = pred2plotPow),
    col = "blue",
    lwd = 1
  ) +
  theme_bw()

################

subData2 <- subset(MyData, Suborder == "Zygoptera")
subData2 <- subData2[!is.na(subData2$TotalLength), ]

Lengths2 <- seq(min(subData2$TotalLength), max(subData2$TotalLength), len = 200)

predict4PlotMod <- powMod(Lengths2, coef(PowFit4)["a"], coef(PowFit4)["b"])

ggplot(subData2, aes(TotalLength, BodyWeight)) + 
  geom_point(shape = I(1), size = 2) +
  geom_line(data = data.frame(TotalLength = Lengths2, BodyWeight = predict4PlotMod), col = "blue", size = 1) + 
  theme_bw()
  
###############
# Exercise
subData3 <-
  subData2 %>% mutate(BodyWeight = log(BodyWeight),
                      TotalLength = log(TotalLength))

lls_fit <- lm(BodyWeight ~ TotalLength, data = subData3)
Lengths3 <- seq(min(subData3$TotalLength), max(subData3$TotalLength), len = 200)
pred_y <- PowMod2(Lengths3, coef(lls_fit)[1], coef(lls_fit)[2])

ggplot(subData3, aes(TotalLength, BodyWeight)) + 
  geom_point(shape = I(1), size = 2) +
  geom_line(
    data = data.frame(TotalLength = Lengths3, BodyWeight = pred_y),
    colour = "blue",
    size = 1
  ) +
  labs(x = "log Total Length", y = "log Body Weight") + 
  theme_bw()


##########
## 二次函数模型
QuaFit <- lm(BodyWeight ~ poly(TotalLength,2), data = subData)

Predic2PlotQua <- predict.lm(QuaFit, data.frame(TotalLength = Lengths))
plot(subData$TotalLength, subData$BodyWeight)
lines(Lengths, pred2plotPow, col = 'blue', lwd = 2.5)
lines(Lengths, Predic2PlotQua, col = 'red', lwd = 2.5)

##########
### 幂函数与二次函数模型进行比较
## 计算R^2值
RSS_Pow <- sum(residuals(powFit2)^2) # Residual sum of squares
TSS_Pow <- sum((subData$BodyWeight - mean(subData$BodyWeight))^2) # Total sum of squares
RSq_Pow <- 1 - (RSS_Pow/TSS_Pow) # R-squared value

RSS_Qua <- sum(residuals(QuaFit)^2) # Residual sum of squares
TSS_Qua <- sum((subData$BodyWeight - mean(subData$BodyWeight))^2) # Total sum of squares
RSq_Qua <- 1 - (RSS_Qua/TSS_Qua) # R-squared value

RSq_Pow 
RSq_Qua

### 计算AIC以比较 幂函数模型和二次函数模型
## 方法1:
# 分布计算
n <- nrow(subData) #set sample size
pPow <- length(coef(powFit2)) # get number of parameters in power law model
pQua <- length(coef(QuaFit)) # get number of parameters in quadratic model

AIC_Pow <- n + 2 + n * log((2 * pi) / n) + n * log(RSS_Pow) + 2 * pPow
AIC_Qua <- n + 2 + n * log((2 * pi) / n) + n * log(RSS_Qua) + 2 * pQua
AIC_Pow - AIC_Qua

## 方法2
# 直接使用内部AIC函数计算两个拟合模型
AIC(powFit2) - AIC(QuaFit)

### BIC
BIC(powFit2) - BIC(QuaFit)


###########
#将直线拟合到数据并与前两个模型比价
lineFit <- lm(BodyWeight ~ TotalLength, data = subData)

plot(subData$TotalLength, subData$BodyWeight)
lines(Lengths, pred2plotPow, col = 'blue', lwd = 2.5)
lines(Lengths, Predic2PlotQua, col = 'red', lwd = 2.5)
lines(Lengths, Predic2PlotLine, col = 'green', lwd = 2.5)

Predic2PlotLine <- predict.lm(lineFit, data.frame(TotalLength = Lengths))


AIC(lineFit) - AIC(QuaFit)

AIC(lineFit) - AIC(powFit2)


################
#### 信天翁雏鸟的成长 (Albatross chick growth)

###导入数据
alb <- read.csv(file="../data/albatross_grow.csv")
alb <- subset(x=alb, !is.na(alb$wt))
plot(alb$age, alb$wt, xlab="age (days)", ylab="weight (g)", xlim=c(0, 100))


### 定义两个模型
logistic1 <- function(t, r, K, N0){
  N0 * K * exp(r * t)/(K+N0 * (exp(r * t)-1))
}

vonbert.w <- function(t, Winf, c, K){
  Winf * (1 - exp(-K * t) + c * exp(-K * t))^3
}

scale <- 4000

###进行拟合
## 直线:
alb.lin <- lm(wt/scale ~ age, data = alb)
## logistic1模型:
alb.log <- nlsLM(wt/scale ~ logistic1(age, r, K, N0), data = alb, start = list(K=1, r=0.1, N0=0.1))
## vonbert.w模型:
alb.vb <- nlsLM(wt/scale ~ vonbert.w(age, Winf, c, K), data = alb, start = list(Winf=0.75, c=0.01, K=0.01))

pred_age <- seq(0, 100, length = 1000)


pred.lin <- predict(alb.lin, newdata = list(age=pred_age)) * scale
# 等替: pred_lin_2 <- predict.lm(alb.lin, newdata = list(age = pred_age)) * scale

pred.log <- predict(alb.log, newdata = list(age=pred_age)) * scale
# 等替: pred_log <- logistic1(pred_age, coef(alb.log)["r"], coef(alb.log)["K"], coef(alb.log)["N0"]) * scale

pred.vb <- predict(alb.vb, newdata = list(age=pred_age)) * scale
# 等替: pred_vb <- vonbert.w(pred_age, coef(alb.vb)["Winf"], coef(alb.vb)["c"], coef(alb.vb)["K"]) * scale

plot(alb$age, alb$wt, xlab="age (days)", ylab="weight (g)", xlim=c(0, 100))
lines(pred_age, pred.lin, col = 2, lwd = 2)
lines(pred_age, pred.log, col = 3, lwd = 2)
lines(pred_age, pred.vb, col = 4, lwd = 2)
legend("topleft", legend = c("linear", "logistic", "Von Bert"), lwd=2, lty=1, col=2:4, cex = 0.5)


### 检查三个模型间的残差
par(mfrow=c(3,1), bty="n")
plot(alb$age, resid(alb.lin), main="LM resids", xlim=c(0,100))
plot(alb$age, resid(alb.log), main="Logisitic resids", xlim=c(0,100))
plot(alb$age, resid(alb.vb), main="VB resids", xlim=c(0,100))
graphics.off()

### 误差平方和 (SSE)比较
n <- length(alb$wt)
list(lin=signif(sum(resid(alb.lin)^2)/(n-2 * 2), 3), 
     log= signif(sum(resid(alb.log)^2)/(n-2 * 3), 3), 
     vb= signif(sum(resid(alb.vb)^2)/(n-2 * 3), 3))   
### AIC/BIC 比较
AIC(alb.lin) - AIC(alb.log)
AIC(alb.lin) - AIC(alb.vb)
AIC(alb.vb) - AIC(alb.log)

BIC(alb.lin) - BIC(alb.log)
BIC(alb.lin) - BIC(alb.vb)
BIC(alb.vb) - BIC(alb.log)





###############
#### 埃及伊蚊繁殖力
aedes <- read.csv(file = "../data/aedes_fecund.csv")
plot(aedes$T, aedes$EFD, xlab="temperature (C)", ylab="Eggs/day")

### 热能曲线模型 
quad1 <- function(T, T0, Tm, c){
  c * (T-T0) * (T-Tm) * as.numeric(T<Tm) * as.numeric(T>T0)
}

briere <- function(T, T0, Tm, c){
  c * T * (T-T0) * (abs(Tm-T)^(1/2)) * as.numeric(T<Tm) * as.numeric(T>T0)
}


### 进行拟合
scale2 = 20
aed.lin = lm(EFD/scale2 ~ T, data = aedes)
aed.qua = nlsLM(EFD/scale2 ~ quad1(T, T0, Tm, c), data = aedes, start = list(T0=10, Tm=40, c=0.01))
aed.bri = nlsLM(EFD/scale2 ~ briere(T, T0, Tm, c), data = aedes, start = list(T0=10, Tm=40, c=0.01))


### 预测拟合结果
pred_T <- seq(min(aedes$T), max(aedes$T), len = 100)
pred_lin <- predict(aed.lin, newdata = list(T = pred_T)) * scale2
pred_qua <- predict(aed.qua, newdata = list(T = pred_T)) * scale2
pred_bri <- predict(aed.bri, newdata = list(T = pred_T)) * scale2

### 可视化模型
plot(aedes$T, aedes$EFD, xlab = "temperature(C)", ylab = "Eggs/day")
lines(pred_T, pred_lin, col = 2, lwd = 2)
lines(pred_T, pred_qua, col = 3, lwd = 2)
lines(pred_T, pred_bri, col = 4, lwd = 2)
legend("topleft", legend = c("linear", "quad1", "briere"), lwd=2, lty=1, col=2:4, cex = 0.5)

### 检查三个模型间的残差
par(mfrow=c(3,1), bty="n")
plot(aedes$T, resid(aed.lin), main="LM resids")
plot(aedes$T, resid(aed.qua), main="quad1 resids")
plot(aedes$T, resid(aed.bri), main="briere")
graphics.off()


### 误差平方和(SSE) 比较模型
n <- length(aedes$EFD)
list(lin=signif(sum(resid(aed.lin)^2)/(n-2 * 2), 3), 
     qua= signif(sum(resid(aed.qua)^2)/(n-2 * 3), 3), 
     bri= signif(sum(resid(aed.bri)^2)/(n-2 * 3), 3))   

### AIC/BIC 比较模型
AIC(aed.lin) - AIC(aed.qua)
AIC(aed.lin) - AIC(aed.bri)
AIC(aed.bri) - AIC(aed.qua)

BIC(aed.lin) - BIC(aed.qua)
BIC(aed.lin) - BIC(aed.bri)
BIC(aed.bri) - BIC(aed.qua)



###############
####人口增长率
t <- seq(0, 22, 2)
N <- c(32500, 33000, 38000, 105000, 445000, 1430000, 3020000, 4720000, 5670000, 5870000, 5930000, 5940000)

set.seed(2341) # To ensure we always get the same random sequence in this example "dataset" 

data <- data.frame(t, N * (1 + rnorm(length(t), sd = 0.1))) # add some random error

names(data) <- c("Time", "N")

head(data)

ggplot(data, aes(Time, N)) + 
  geom_point(size = 3) + 
  labs(x = "Time (Hours)", y = "Population size (cells)")

data$logN <- log(data$N)

ggplot(data, aes(t, logN)) + 
  geom_point(size = 3) +
  labs(x = "Time (Hours)", y = "log(cell number)")

r <- (data[data$Time == 10,]$logN - data[data$Time == 6,]$logN)/(10-6)

diff(data$logN)
max(diff(data$logN))/2 # 2 is the difference in any successive pair of timepoints

### OLS
lm_growth <- lm(logN ~ Time, data = data[data$Time > 2 & data$Time < 12,])
summary(lm_growth)


### NLLS
#### 逻辑模型
logistic_model <- function(t, r_max, K, N_0){ # The classic logistic equation
  return(N_0 * K * exp(r_max * t)/(K + N_0 * (exp(r_max * t) - 1)))
}

# first we need some starting parameters for the model
N_0_start <- min(data$N) # lowest population size
K_start <- max(data$N) # highest population size
r_max_start <- round(coef(lm_growth)["Time"], digits = 2) # use our estimate from the OLS fitting from above

### 拟合
logi_Fit <-
  nlsLM(
    N ~ logistic_model(Time, r_max, K, N_0),
    start = list(r_max = r_max_start, N_0 = N_0_start, K = K_start),
    data = data
  )

### 预测拟合结果
pred_time <- seq(0, 22, 0.1)
pred_logi_Fit <-
  logistic_model(pred_time, coef(logi_Fit)["r_max.Time"], coef(logi_Fit)["K"], coef(logi_Fit)["N_0"])
# 等替: pred_logi_Fit2 <- predict(logi_Fit, newdata = list(Time = pred_time))

df1 <- data.frame(pred_time, pred_logi_Fit)
df1$model <- "logistic equation"

names(df1) <- c("Time", "N", "model")
ggplot(data, aes(Time, N)) + 
  geom_point(size = 3) + 
  geom_line(data = df1, aes(Time, N, col = model), size = 1)+
  theme(aspect.ratio=1)+ # make the plot square 
  labs(x = "Time", y = "Cell number") +
  theme_bw()
  
## 在对数轴中可视化
ggplot(data, aes(Time, logN)) + 
  geom_point(size = 3) + 
  geom_line(data = df1, aes(Time, log(N), col = model), size = 1)+
  theme(aspect.ratio=1)+ # make the plot square 
  labs(x = "Time", y = "log(Cell number)") +
  theme_bw()
  

ggplot(data, aes(x = N, y = logN)) +
  geom_point(size = 3) +
  theme(aspect.ratio = 1)+ 
  labs(x = "N", y = "log(N)")
  

#### Gompertz模型
gompertz_model <- function(t, r_max, K, N_0, t_lag){ 
  # Modified gompertz growth model (Zwietering 1990)
  return(N_0 + (K - N_0) * exp(-exp(r_max * exp(1) * (t_lag - t)/((K - N_0) * log(10)) + 1)))
}   

N_0_start <- min(data$logN) # lowest population size, note log scale
K_start <- max(data$logN) # highest population size, note log scale
r_max_start <- 0.62 # use our previous estimate from the OLS fitting from above
t_lag_start <- data$Time[which.max(diff(diff(data$logN)))] # find last timepoint of lag phase


### 拟合模型
gom_Fit <-
  nlsLM(
    logN ~ gompertz_model(Time, r_max, K, N_0, t_lag),
    data = data,
    start = list(
      r_max = r_max_start,
      K = K_start,
      N_0 = N_0_start,
      t_lag = t_lag_start
    )
  )

### 预测测试结果

pred_Time2 <- seq(0, 24, 0.1)
pred_gom_Fit <- gompertz_model(pred_Time2, coef(gom_Fit)["r_max"], coef(gom_Fit)["K"], coef(gom_Fit)["N_0"], coef(gom_Fit)["t_lag"])

ggplot(data, aes(Time, logN)) + 
  geom_point(size = 3) + 
  geom_line(data = df2, aes(Time, LogN, col = model), size = 1)+
  theme(aspect.ratio=1)+ # make the plot square 
  labs(x = "Time", y = "log(Cell number)") +
  theme_bw()


df2 <- data.frame(pred_Time2, pred_gom_Fit)
df2$model <- "Gompertz model"
names(df2) <- c("Time", "LogN", "model")
names(df1) <- c("Time", "LogN", "model")
df1$LogN = log(df1$LogN)

model_frame <- rbind(df1, df2)

ggplot(data, aes(Time, logN)) +
  geom_point(size = 3) +
  geom_line(data = model_frame, aes(Time, LogN, col = model), size = 1) + 
  theme_bw() + # make the background white
  theme(aspect.ratio=1) + # make the plot square 
  labs(x = "Time", y = "log(Abundance)")

### 计算拟合模型的置信区间
confint(gom_Fit)
confint(logi_Fit)


### AIC/BIC
AIC(gom_Fit) - AIC(logi_Fit)
AIC(lm_growth) - AIC(gom_Fit)
AIC(lm_growth) - AIC(logi_Fit)

BIC(gom_Fit) - BIC(logi_Fit)
BIC(lm_growth) - BIC(gom_Fit)
BIC(lm_growth) - BIC(logi_Fit)
