library(tidyverse)
library(lme4)
library(lmerTest)
library(splines)
library(segmented)
library(gridExtra)
library(grid)
library(dplyr)

otu <- read.csv("../data/cata/otu.csv", row.names=1)
#otu_norm <- read.csv("normalized_otu.csv")
#tax <- read.csv("tax.csv", row.names=1)
sample <- read.csv("../data/cata/sample.csv")
colnames(sample)[1] <- "sampleID"
nature_stress <- read.csv("../data/cata/combined_nature_stress_data.csv")
anthro_stress <- read.csv("../data/cata/anthro_stress.csv")
colnames(anthro_stress)[1] <- "eventID"
# hill numbers
{
  # 初始化存储结果的数据框
  hill_numbers <- data.frame(
    sampleID = colnames(otu),
    q0 = NA,
    q1 = NA,
    q2 = NA
  )
  #install.packages('vegan',
  #                 repos = c('https://vegandevs.r-universe.dev',
  #                           'https://cloud.r-project.org'))
  library(vegan)
  # 计算Hill numbers
  for (i in 1:ncol(otu)) {
    counts <- otu[, i]
    # 去掉0值
    counts <- counts[counts > 0]
    # q = 0 (Species richness)
    hill_numbers$q0[i] <- length(counts)

    # q = 1 (Exponential of Shannon entropy)
    if (length(counts) > 0) {
      hill_numbers$q1[i] <- exp(diversity(counts, index = "shannon"))
    } else {
      hill_numbers$q1[i] <- NA
    }

    # q = 2 (Inverse of Simpson index)
    if (length(counts) > 0) {
      hill_numbers$q2[i] <- diversity(counts, index = "inv")
    } else {
      hill_numbers$q2[i] <- NA
    }
  }
}



data_nature <- nature_stress %>%
  dplyr::select(eventID, Year, Month, RESET_score) %>%
  distinct() %>%
  group_by(eventID) %>%
  summarize(RESET_score_mean=mean(RESET_score))
data_anthro <- anthro_stress %>%
  dplyr::select(c(1, 8:ncol(anthro_stress)))

data <- sample %>%
  dplyr::select(sampleID, eventID, country, locality, habitat,
         dayDeployed, dayCollected, lengthDeployment,
         depthOfBottomInMeters) %>%
  inner_join(hill_numbers, by="sampleID") %>%
  inner_join(data_anthro, by="eventID") %>%
  inner_join(data_nature, by="eventID")

# Data diagnostics
ggplot(data) +
  geom_point(aes(x=depthOfBottomInMeters, y=RESET_score_mean))
data %>% dplyr::select(sampleID, eventID, depthOfBottomInMeters) %>%
  filter(depthOfBottomInMeters <= quantile(depthOfBottomInMeters, .25, na.rm=T)-1.5*IQR(depthOfBottomInMeters, na.rm=T))
ggplot(data) +
  geom_point(aes(x=RESET_score_mean, y=cumul_score))
ggplot(data) +
  geom_histogram(aes(x=log(q0)))
ggplot(data) +
  geom_point(aes(x=nutrient, y=cumul_score))

# Models(总分分析)

model_lm_q0 <- lm(log(q0) ~ RESET_score_mean + cumul_score, data=data)
summary(model_lm_q0)

model_lm_q1 <- lm(q1 ~ RESET_score_mean + cumul_score, data=data)
summary(model_lm_q1)

model_lm_q2 <- lm(log(q2) ~ RESET_score_mean + cumul_score, data=data)
summary(model_lm_q2)

model_poly_q0 <- lm(log(q0) ~ poly(RESET_score_mean, 2), data = data)
summary(model_poly_q0)

model_poly_q1 <- lm(q1 ~ poly(RESET_score_mean, 2), data = data)
summary(model_poly_q1)

c_model_poly_q1 <- lm(q1 ~ poly(cumul_score, 2), data = data)
summary(c_model_poly_q1)

model_poly_q2 <- lm(log(q2) ~ poly(RESET_score_mean, 2), data = data)
summary(model_poly_q2)

model_lmer <- lmer(log(q0) ~ RESET_score_mean + cumul_score + (1|locality), data=data)
summary(model_lmer)

# Model diagnostics
shapiro.test(residuals(model_lm_q0))
plot(model_lm_q0, which=1)
qqnorm(residuals(model_lm_q0))
qqline(residuals(model_lm_q0))
AIC(model_lm_q0)

shapiro.test(residuals(model_lm_q1))
plot(model_lm_q1, which=1)
qqnorm(residuals(model_lm_q1))
qqline(residuals(model_lm_q1))
AIC(model_lm_q1)

shapiro.test(residuals(model_lm_q2))
plot(model_lm_q2, which=1)
qqnorm(residuals(model_lm_q2))
qqline(residuals(model_lm_q2))
AIC(model_lm_q2)

shapiro.test(residuals(model_poly_q1))
plot(model_poly_q1, which=1)
qqnorm(residuals(model_poly_q1))
qqline(residuals(model_poly_q1))
AIC(model_poly_q1)

# 可视化
# 绘制 RESET_score_mean vs log(q1)
p1 <- ggplot(data, aes(x = RESET_score_mean, y = q1)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  theme_minimal() +
  labs(title = "Scatter plot with linear fit: RESET_score_mean vs q1",
       x = "RESET_score_mean",
       y = "q1")

# 绘制数据点和拟合曲线(二次多项式)
p2 <- ggplot(data, aes(x = RESET_score_mean, y = q1)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red") +
  theme_minimal() +
  labs(title = "Polynomial Regression: RESET_score vs q1",
       x = "RESET_score: The score of the reef environmental stress exposure toolbox",
       y = "q1: A metric to quantify motile diversity")


p5 <- ggplot(data, aes(x = cumul_score, y = q1)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red") +
  theme_minimal() +
  labs(title = "Polynomial Regression: cumul_score vs q1",
       x = "cumul_score: Cumulative stress score",
       y = "q1: A metric to quantify community diversity")

# 绘制 cumul_score vs q1
p3 <- ggplot(data, aes(x = cumul_score, y = q1)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  theme_minimal() +
  labs(title = "Scatter plot with linear fit: cumul_score vs q1",
       x = "cumul_score",
       y = "q1")

residuals1 <- model_lm_q1$residuals
# 生成残差的QQ图
qq_plot <- ggplot(data.frame(sample = residuals1), aes(sample = sample)) +
  stat_qq(size = 2, shape = 1) +
  stat_qq_line() +
  ggtitle("QQ Plot of Model Residuals") +
  theme_minimal()
print(qq_plot)


variables = c("grav_NC", "sediment", "nutrient", "pop_count", "num_ports", "reef_value")
data_random <- data %>% dplyr::select(all_of(variables), q1)
data_random <- na.omit(data_random)
rf_model <- randomForest(as.formula(paste("q1 ~", paste(variables, collapse = " + "))), data = data_random, ntree = 500)


png("../plot/RandomF_AnthroStress.png", width = 8, height = 6, units = "in", res = 300)
random_p <- varImpPlot(rf_model, main = "Variable Importance Plot")
title(xlab = "The importance of the variables:\n", ylab = "Multiple anthropogenic stress factors")
dev.off()


ggsave("../plot/RandomF_AnthroStress.png", plot = random_p, width = 8, height = 6, dpi = 300, bg = "white")

#Models(小分分析)
lm_anthro_q0 <- lm(q0 ~ grav_NC + sediment + pop_count + reef_value + RESET_score_mean, data = data)
summary(lm_anthro_q0)

data_clean <- na.omit(data)
lm_anthro_q1 <- lm(q1 ~ grav_NC + sediment + pop_count + reef_value + RESET_score_mean, data = data_clean)
summary(lm_anthro_q1)

lm_anthro_q2 <- lm(q2 ~ grav_NC + sediment + pop_count + reef_value + RESET_score_mean, data = data)
summary(lm_anthro_q2)

shapiro.test(residuals(lm_anthro_q1))
plot(lm_anthro_q1, which=1)
qqnorm(residuals(lm_anthro_q1))
qqline(residuals(lm_anthro_q1))
AIC(lm_anthro_q1)

data_sub1 <- subset(data, select = c(q1, grav_NC, sediment, pop_count, reef_value))
data_long_q1 <- reshape2::melt(data_sub1, id.vars = "q1")
data_long_q1 <- na.omit(data_long_q1)
p4 <- ggplot(data = data_long_q1, aes(x = value, y = q1)) + 
  geom_point(size = 0.8) + 
  geom_smooth(method = "lm", col = "red") +
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Multiple anthropogenic stress factors vs q1", x = "Quantified measures from anthropogenic stress factors", y = "q1: A metric to quantify motile diversity")

# 绘制并显示图形

pdf("../plot/model_reset_cumul_q1.pdf")
plot.new()
text(0, 1, paste(capture.output(summary(model_lm_q1)), collapse = "\n"), adj = c(0, 1), cex = 1)
dev.off()

ggsave("../plot/cumul_q1.png", plot = p3, width = 8, height = 6, dpi = 300, bg = "white") 
ggsave("../plot/reset_q1_poly.png", plot = p2, width = 8, height = 6, dpi = 300, bg = "white") 
ggsave("../plot/all_anthroStress_q1.png", plot = p4, width = 8, height = 6, dpi = 300, bg = "white") 
ggsave("../plot/reset_q1.png",  plot = p1, width = 8, height = 6, dpi = 300, bg = "white")

pdf("../plot/model_Reset_q1_poly.pdf")
plot.new()
text(0, 1, paste(capture.output(summary(model_poly_q1)), collapse = "\n"), adj = c(0, 1), cex = 1)
dev.off()


pdf("../plot/model_all_anthroStress_q1.pdf")
plot.new()
text(0, 1, paste(capture.output(summary(lm_anthro_q1)), collapse = "\n"), adj = c(0, 1), cex = 1)
dev.off()



print(p1)
print(p2)
print(p3)
print(p4)


