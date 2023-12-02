### 加载数据包 ###
library(tidyverse)
library(minpack.lm)
### 清空列表 ###
rm(list = ls())
graphics.off()

### 加载、调整数据 ###
data <- read.csv("../data/LogisticGrowthData.csv")
meta_data <- read.csv("../data/LogisticGrowthMetaData.csv")
## 取零、添加LogPopBio ##
data_remove0_popBio <- data[data$PopBio > 0,]
data_add_log <-
  data_remove0_popBio %>% mutate(LogPopBio = log(PopBio))
## 按ID分类数据 ##
data_add_ID <-
  data_add_log %>% group_by(Species, Temp, Medium, Citation) %>% mutate(ID = cur_group_id())


### 拟合模型 ###
## logistic model ##
logistic_model <- function(t, r_max, K, N_0) {
  # The classic logistic equation
  return(N_0 * K * exp(r_max * t) / (K + N_0 * (exp(r_max * t) - 1)))
}

## Gompertz model ##
gompertz_model <- function(t, r_max, K, N_0, time_lag) {
  return(N_0 + (K - N_0) * exp(-exp(r_max * exp(1) * (time_lag - t) / ((K - N_0) * log(10)) + 1)))
}

## Quadratic model ##
# y ~ a * x^2 + b * x + c #

## Cubic Model
# y ~ a * x^3 + b * x^2 + c * x + d #


### R_squared function ###
R_sq_func <- function(data_in, fit_model) {
  RSS <- sum(residuals(fit_model) ^ 2) # Residual sum of squares
  TSS <- sum((data_in$LogPopBio - mean(data_in$LogPopBio)) ^ 2) # Total sum of squares
  RSq <- 1 - (RSS / TSS) # R-squared value
  return(RSq)
}
### AICC function ###
AICc_func <- function(data_in, fit_model) {
  L <- length(coef(fit_model))
  n <- nrow(data_in)
  AIC_value <- AIC(fit_model)
  AICc_value <- AIC_value + (2 * L * (L + 1)) / (n - L - 1)
  return(AICc_value)
}
### 根据ID分类并分析数据 ###
## 提取相同ID并判断是否符合条件 ###
ID_ls <- group_split(data_add_ID)
# 储存每个ID最适合的模型 #
best_model_fit_ID_data <- matrix(NA, 285, 1)

## 储存每个ID data的最优模型 ##
win_r <- matrix(NA, 285, 1)
err_model_ID <- data.frame(
  Model = c(
    "Logistic model",
    "Gompertz model",
    "Quadratic model",
    "Cubic Model"
  ),
  Count = 0
)

## Out put Data by different model ##
all_ID_data_in_logis_model <- data.frame()
all_ID_data_in_gomp_model <- data.frame()
all_ID_data_in_qua_model <- data.frame()
all_ID_data_in_cub_model <- data.frame()

i <- 1
pdf(file = "../data/ID_model.pdf")
for (data_by_ID in ID_ls) {
  # print(length(data_by_ID$PopBio))
  model_AICc <- matrix(NA, 4, 1)
  data_all <- data.frame()
  if (length(data_by_ID$LogPopBio) < 6) {
    p <-
      paste("ID: ", unique(data_by_ID$ID), " lack of data.", sep = "")
    print(p)
  } else{
    ### find r_max(OLS) ###
    # 初始阶段的lag #
    t_lag <-
      data_by_ID$Time[which.max(diff(diff(data_by_ID$LogPopBio)))]
    
    
    ### 设置参数初始值 ###
    ## logistics & gompertz model ##
    s_r_max <- 0.02
    s_K <- max(data_by_ID$LogPopBio)
    s_N0 <- min(data_by_ID$LogPopBio)
    s_t_lag <- t_lag
    
    pred_time <-
      seq(min(data_by_ID$Time), max(data_by_ID$Time), len = 300)
    ################################################
    ## 开始拟合Logistic model ##
    tryCatch({
      logis_Fit <-
        nlsLM(
          LogPopBio ~ logistic_model(Time, r_max, K, N_0),
          data = data_by_ID,
          start = list(
            r_max = s_r_max,
            K = s_K,
            N_0 = s_N0
          )
        )
      # 计算R_squared & AICc value #
      logis_model_RSq <- R_sq_func(data_by_ID , logis_Fit)
      logis_model_AICc <- AICc_func(data_by_ID , logis_Fit)
      r_max_log <- coef(logis_Fit)["r_max"]
      K_log <- coef(logis_Fit)["K"]
      N_0_log <- coef(logis_Fit)["N_0"]
      
      # 预测 LogPopBio 并储存 #
      pred_logis <-
        predict(logis_Fit, newdata = list(Time = pred_time))
      df1 <-
        data.frame(model_type = "Logistic model",
                   Time = pred_time,
                   LogPopBio = pred_logis)
      tdf <- data.frame(
        unique(data_by_ID$ID),
        "Logistic model",
        unique(data_by_ID$Species),
        unique(data_by_ID$Medium),
        unique(data_by_ID$Temp),
        logis_model_RSq,
        logis_model_AICc,
        r_max_log,
        K_log,
        N_0_log
      )
      all_ID_data_in_logis_model <-
        rbind(all_ID_data_in_logis_model, tdf)
      model_AICc[1] <- logis_model_AICc
      data_all <- rbind(data_all, df1)
    },
    error = function(e) {
      p <- paste("ID:", unique(data_by_ID$ID), "; Logistic model failed")
      print(p)
      err_model_ID[1, 2] <<- err_model_ID[1, 2] + 1
    })

    ################################################
    # 开始拟合gompertz model #
    tryCatch({
      # 开始拟合 #
      gomp_Fit <-
        nlsLM(
          LogPopBio ~ gompertz_model(Time, r_max, K, N_0, time_lag),
          data = data_by_ID,
          start = list(
            r_max = s_r_max,
            K = s_K,
            N_0 = s_N0,
            time_lag = s_t_lag
          )
        )
      # 计算R_squared & AICc value #
      gomp_model_RSq <- R_sq_func(data_by_ID , gomp_Fit)
      gomp_model_AICc <- AICc_func(data_by_ID , gomp_Fit)
      r_max_gomp <- coef(gomp_Fit)["r_max"]
      K_gomp <- coef(gomp_Fit)["K"]
      N_0_gomp <- coef(gomp_Fit)["N_0"]
      t_lag_gomp <- coef(gomp_Fit)["time_lag"]
      # 预测 LogPopBio 并储存 #
      pred_gomp <-
        predict(gomp_Fit, newdata = list(Time = pred_time))
      df2 <-
        data.frame(model_type = "Gompertz model",
                   Time = pred_time,
                   LogPopBio = pred_gomp)
      tdf2 <- data.frame(
        unique(data_by_ID$ID),
        "Gompertz model",
        unique(data_by_ID$Species),
        unique(data_by_ID$Medium),
        unique(data_by_ID$Temp),
        gomp_model_RSq,
        gomp_model_AICc,
        r_max_gomp,
        K_gomp,
        N_0_gomp,
        t_lag_gomp)
      all_ID_data_in_gomp_model <-
        rbind(all_ID_data_in_gomp_model, tdf2)
      
      model_AICc[2] <- gomp_model_AICc
      data_all <- rbind(data_all, df2)
      
    },
    error = function(e) {
      p <- paste("ID:", unique(data_by_ID$ID), "; Gompertz model failed")
      print(p)
      err_model_ID[2, 2] <<- err_model_ID[2, 2] + 1
      
    })
    
    ################################################
    # 开始拟合quadratic model #
    tryCatch({
      qua_Fit <-
        lm(LogPopBio ~ poly(Time, 2),
           data = data_by_ID)
      # 计算R_squared & AICc value #
      qua_model_RSq <- R_sq_func(data_by_ID , qua_Fit)
      qua_model_AICc <- AICc_func(data_by_ID , qua_Fit)
      a_value_qua <- coef(qua_Fit)[3]
      b_value_qua <- coef(qua_Fit)[2]
      c_value_qua <- coef(qua_Fit)[1]
      
      # 预测 LogPopBio 并储存 #
      pred_qua <-
        predict.lm(qua_Fit, newdata = list(Time = pred_time))
      df3 <-
        data.frame(model_type = "Quadratic model",
                   Time = pred_time,
                   LogPopBio = pred_qua)
      tdf3 <- data.frame(
        unique(data_by_ID$ID),
        "Quadratic model",
        unique(data_by_ID$Species),
        unique(data_by_ID$Medium),
        unique(data_by_ID$Temp),
        qua_model_RSq,
        qua_model_AICc,
        a_value_qua,
        b_value_qua,
        c_value_qua
      )
      all_ID_data_in_qua_model <-
        rbind(all_ID_data_in_qua_model, tdf3)
      
      model_AICc[3] <- qua_model_AICc
      data_all <- rbind(data_all, df3)
      
    },
    error = function(e) {
      p <- paste("ID:", unique(data_by_ID$ID), "; Quadratic model failed")
      print(p)
      err_model_ID[3, 2] <<- err_model_ID[3, 2] + 1
      
    })
    
    ################################################
    # 开始拟合cubic model #
    tryCatch({
      cub_Fit <-
        lm(LogPopBio ~ poly(Time, 3),
           data = data_by_ID)
      
      # 计算R_squared & AICc value #
      cub_model_RSq <- R_sq_func(data_by_ID , cub_Fit)
      cub_model_AICc <- AICc_func(data_by_ID , cub_Fit)
      a_value_cub <- coef(cub_Fit)[4]
      b_value_cub <- coef(cub_Fit)[3]
      c_value_cub <- coef(cub_Fit)[2]
      d_value_cub <- coef(cub_Fit)[1]
      
      # 预测 LogPopBio 并储存 #
      pred_cub <- predict.lm(cub_Fit, newdata = list(Time = pred_time))
      df4 <-
        data.frame(model_type = "Cubic model",
                   Time = pred_time,
                   LogPopBio = pred_cub)
      tdf4 <- data.frame(
        unique(data_by_ID$ID),
        "Cubic model",
        unique(data_by_ID$Species),
        unique(data_by_ID$Medium),
        unique(data_by_ID$Temp),
        cub_model_RSq,
        cub_model_AICc,
        a_value_cub,
        b_value_cub,
        c_value_cub,
        d_value_cub
      )
      all_ID_data_in_cub_model <-
        rbind(all_ID_data_in_cub_model, tdf4)
      
      model_AICc[4] <- cub_model_AICc
      data_all <- rbind(data_all, df4)
      
    },
    error = function(e) {
      p <- paste("ID:", unique(data_by_ID$ID), "; Cubic model failed")
      print(p)
      err_model_ID[4, 2] <<- err_model_ID[4, 2] + 1
    })
    
    
    ################################################
    
    ## 比较四种模型的AICc,并找出最优模型并储存 ##
    best_m <- which.min(model_AICc[1:4,])
    worst_m <- which.max(model_AICc[1:4,])
    win_r[unique(data_by_ID$ID)] <- best_m
    ## 绘制图形 ##
    p <- paste("All fitted models for ID:", i, "data")
    pic_model <- ggplot(data_by_ID, aes(Time, LogPopBio)) +
      geom_point(size = 3) +
      geom_line(data = data_all,
                aes(Time, LogPopBio, col = model_type),
                size = 1) +
      theme(aspect.ratio = 1) + # make the plot square
      labs(x = "Time", y = "LogPopBIo") +
      ggtitle(p) +
      theme_bw()
    print(pic_model)
    ## 滞后期 ##
    if (i == 172) {
      p <- paste("All fitted models for ID:", i, "data")
      pdf(file = "../data/ID_172_model.pdf")
      pic_172 <- ggplot(data_by_ID, aes(Time, LogPopBio)) +
        geom_point(size = 3) +
        geom_line(data = data_all,
                  aes(Time, LogPopBio, col = model_type),
                  size = 1) +
        theme(aspect.ratio = 1) + # make the plot square
        labs(x = "Time", y = "LogPopBIo") +
        ggtitle(p) +
        theme_bw()
      print(pic_172)
      dev.off()
    }
    
    if (i == 2) {
      p <- paste("All fitted models for ID:", i, "data")
      pdf(file = "../data/ID_2_model.pdf")
      pic_2 <- ggplot(data_by_ID, aes(Time, LogPopBio)) +
        geom_point(size = 3) +
        geom_line(data = data_all,
                  aes(Time, LogPopBio, col = model_type),
                  size = 1) +
        theme(aspect.ratio = 1) + # make the plot square
        labs(x = "Time", y = "LogPopBIo") +
        ggtitle(p) +
        theme_bw()
      print(pic_2)
      dev.off()
    }
    
    ## 滞后期 ##
    if (i == 19) {
      if (best_m == 1) {
        best_fit_data <- df1
        m <- paste("ID", i, "Best: Logistic model")
      } else if (best_m == 2) {
        m <- paste("ID", i, "Best: Gompertz model")
        best_fit_data <- df2
      } else if (best_m == 3) {
        m <- paste("ID", i, "Best: Quadratic model")
        best_fit_data <- df3
      } else{
        m <- paste("ID", i, "Best: Cubic model")
        best_fit_data <- df4
      }
      pdf(file = "../data/ID_19_best_model.pdf")
      pic_best <- ggplot(data_by_ID, aes(Time, LogPopBio)) +
        geom_point(size = 3) +
        labs(x = "Time", y = "LogPopBIo") +
        ggtitle(m) +
        geom_line(data = best_fit_data,
                  aes(Time, LogPopBio),
                  size = 1, colour = "red") +
        theme(aspect.ratio = 1) + # make the plot square
        theme_bw()
      print(pic_best)
      dev.off()
      
      if (worst_m == 1) {
        m2 <- paste("ID", i, "Worst: Logistic model")
        worst_fit_data <- df1
      } else if (worst_m == 2) {
        m2 <- paste("ID", i, "Worst: Gompertz model")
        worst_fit_data <- df2
      } else if (worst_m == 3) {
        m2 <- paste("ID", i, "Worst:Quadratic model")
        worst_fit_data <- df3
      } else{
        m2 <- paste("ID", i, "Worst: Cubic model")
        worst_fit_data <- df4
      }
      pdf(file = "../data/ID_19_worst_model.pdf")
      pic_worst <- ggplot(data_by_ID, aes(Time, LogPopBio)) +
        geom_point(size = 3) +
        geom_line(data = worst_fit_data,
                  aes(Time, LogPopBio),
                  size = 1, colour = "red") +
        theme(aspect.ratio = 1) + # make the plot square
        labs(x = "Time", y = "LogPopBIo") +
        ggtitle(m2) +
        theme_bw()
      print(pic_worst)
      dev.off()
      
      pdf(file = "../data/ID_19_model.pdf")
      pic_all_model <- ggplot(data_by_ID, aes(Time, LogPopBio)) +
        geom_point(size = 3) +
        geom_line(data = data_all,
                  aes(Time, LogPopBio, col = model_type),
                  size = 1) +
        theme(aspect.ratio = 1) + # make the plot square
        labs(x = "Time", y = "LogPopBIo") +
        ggtitle("All fitted models for data") +
        theme_bw()
      print(pic_all_model)
      dev.off()
    }
  
  }
  i <- i + 1
}
dev.off()

model_table <- table(win_r)
win_model <- which.max(model_table)

if (win_model == 1) {
  print("Best fitting model for this data is Logistic Model.")
} else if (win_model == 2) {
  print("Best fitting model for this data is Gompertz Model.")
} else if (win_model == 3) {
  print("Best fitting model for this data is Quadratic Model.")
} else{
  print("Best fitting model for this data is Cubic Model.")
}

colnames(all_ID_data_in_logis_model) <- c("ID", "Model_type", "Species", "Medium", "Temp", "R_squared", "AICc", "r_max", "K", "N_0")
colnames(all_ID_data_in_gomp_model) <- c("ID", "Model_type", "Species", "Medium", "Temp", "R_squared", "AICc", "r_max", "K", "N_0", "t_lag")
colnames(all_ID_data_in_qua_model) <- c("ID", "Model_type", "Species", "Medium", "Temp", "R_squared", "AICc", "a", "b", "c(Intercept)")
colnames(all_ID_data_in_cub_model) <- c("ID", "Model_type", "Species", "Medium", "Temp", "R_squared", "AICc", "a", "b", "c", "d(Intercept)")
rownames(all_ID_data_in_logis_model) <- NULL
rownames(all_ID_data_in_gomp_model) <- NULL
rownames(all_ID_data_in_qua_model) <- NULL
rownames(all_ID_data_in_cub_model) <- NULL

data_name <- c("Model_type", "R_squared", "AICc")
d1 <- all_ID_data_in_logis_model %>% select(all_of(data_name))
d2 <- all_ID_data_in_gomp_model %>% select(all_of(data_name))
d3 <- all_ID_data_in_qua_model %>% select(all_of(data_name))
d4 <- all_ID_data_in_cub_model %>% select(all_of(data_name))
compare_data_in_four_models <- data.frame()
compare_data_in_four_models <- rbind(compare_data_in_four_models, d1)
compare_data_in_four_models <- rbind(compare_data_in_four_models, d2)
compare_data_in_four_models <- rbind(compare_data_in_four_models, d3)
compare_data_in_four_models <- rbind(compare_data_in_four_models, d4)

pdf(file = "../data/Rsq.pdf")
pic_Rsq <- ggplot(compare_data_in_four_models, aes(x = Model_type, y = R_squared, fill = Model_type)) +
  geom_boxplot() +
  labs(title = "Boxplot for R_squared value", x = "Model_type", y = "R_squared") + 
  ylim(c(0.85, 1.0))
print(pic_Rsq)
dev.off()

pdf(file = "../data/AICc.pdf")
pic_AICc <- ggplot(compare_data_in_four_models, aes(x = Model_type, y = AICc, fill = Model_type)) +
  geom_boxplot() +
  labs(title = "Boxplot for AICc value", x = "Model_type", y = "AICc") + 
  ylim(c(-50, 100))
print(pic_AICc)
dev.off()