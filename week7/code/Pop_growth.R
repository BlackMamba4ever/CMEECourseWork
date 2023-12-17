####人口增长率
t <- seq(0, 22, 2)
N <- c(32500, 33000, 38000, 105000, 445000, 1430000, 3020000, 4720000, 5670000, 5870000, 5930000, 5940000)

set.seed(2341) # To ensure we always get the same random sequence in this example "dataset" 

data <- data.frame(t, N * (1 + rnorm(length(t), sd = 0.1))) # add some random error

names(data) <- c("Time", "N")
data$logN <- log(data$N)

diff(data$logN)
max(diff(data$logN))/2 # 2 is the difference in any successive pair of timepoints


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
pred_logi_Fit <-logistic_model(pred_time, coef(logi_Fit)["r_max.Time"], coef(logi_Fit)["K"], coef(logi_Fit)["N_0"])
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
