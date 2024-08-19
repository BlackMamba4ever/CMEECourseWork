rm(list = ls())

###### Packages
{
  library(phyloseq)
  library(dplyr)
  library(ggplot2)
  library(tidyverse)
  library(corrplot)
  library(randomForest)
  library(caret)
  library(car)
  library(reshape2)
  library(Matrix)
  library(Matrix)
  library(lme4)
  library(MuMIn)
  library(maps)
  library(mapdata)
  library(rpart)
  library(rpart.plot)
  ###### using func: diversity
  library(lattice)
  library(permute)
  library(vegan)
  
  library(pheatmap)
  
}



###### Data loading
{
  # 设置 stress data RDS文件的路径
  Path_cOTUs <- "../data/ps_COI_motile_cOTUs_clean_Ransome.RDS"
  Path_ESVs <- "../data/ps_COI_motile_ESVs_clean_Ransome.RDS"
  Path_Reset_SE_Score <- "../data/RESET_SE_score_Ransome.RDS"
  Path_Reset_cumul_Score <- "../data/RESET_cumul_score_Ransome.RDS"
  Path_FR_AnthroStress_Bio_csv <- "../data/FR_AnthroStress_Biogeo_data_Ransome_15042024.csv"
  
  # 读取RDS文件
  phyloseq_cOTUs_data <- readRDS(Path_cOTUs)
  phyloseq_ESVs_data <- readRDS(Path_ESVs)
  phyloseq_reset_SE <- readRDS(Path_Reset_SE_Score)
  phyloseq_reset_cumul <- readRDS(Path_Reset_cumul_Score)
  FR_data <- read.csv(Path_FR_AnthroStress_Bio_csv)
  
  
  # 分离OTUS的RDS文件
  cOTUs_otu_data <- otu_table(phyloseq_cOTUs_data)  # 查看完整的OTU表
  cOTUs_sample_data <- sample_data(phyloseq_cOTUs_data)  # 查看全部样本数据
  df_cOTUs_sample_data <- as(cOTUs_sample_data, "data.frame")
  cOTUs_tax_data <- tax_table(phyloseq_cOTUs_data)  # 查看完整的分类表
  df_tax_data <- data.frame(cOTUs_tax_data)
  # ESVs_otu_data <- otu_table(phyloseq_ESVs_data)  # 查看完整的OTU表
  # ESVs_sample_data <- sample_data(phyloseq_ESVs_data)  # 查看全部样本数据
  # ESVs_tax_data <- tax_table(phyloseq_ESVs_data)  # 查看完整的分类表
  
  # Stress data
  phyloseq_reset_cumul$date <- as.Date(phyloseq_reset_cumul$date, format =
                                         "%Y-%m-%d")
  phyloseq_reset_SE$date <- as.Date(phyloseq_reset_SE$date, format = "%Y-%m-%d")
  #合并RESET数据和人为压力数据
  combined_data <- merge(
    phyloseq_reset_SE,
    phyloseq_reset_cumul,
    by = c("eventID", "date", "Year", "Month"),
    all = TRUE
  )# head(combined_data)
  
  # #转存为csv(压力数据)
  # write.csv(phyloseq_reset_SE, "../data/reset_SE.csv", row.names = FALSE)
  # write.csv(phyloseq_reset_cumul,
  #           "../data/reset_cumul.csv",
  #           row.names = FALSE)
  # write.csv(combined_data, "../data/combined_stress_data.csv", row.names = FALSE)
  #
  #
  # # 将原始数据分成3份分别储存为csv文件
  # write.csv(
  #   cOTUs_otu_data,
  #   "../data/ps_COI_motile_cOTUs_clean_Ransome_otu_data.csv",
  #   row.names = TRUE
  # )
  # write.csv(
  #   df_cOTUs_sample_data,
  #   "../data/ps_COI_motile_cOTUs_clean_Ransome_sample_data.csv",
  #   row.names = TRUE
  # )
  # write.csv(
  #   cOTUs_tax_data,
  #   "../data/ps_COI_motile_cOTUs_clean_Ransome_tax_data.csv",
  #   row.names = TRUE
  # )
}


###### tax data ana
{
  # 统计每个等级的频次
  rank_counts <- df_tax_data %>%
    summarise(across(starts_with("Rank"), ~ sum(!is.na(.))))
  
  # 转换为矩阵
  rank_matrix <- as.matrix(rank_counts)
  
  # 绘制热图
  pheatmap(rank_matrix, 
           cluster_rows = FALSE, 
           cluster_cols = FALSE, 
           display_numbers = TRUE, 
           main = "分类等级频数热图")
  
  # 去除Rank_8中的NA值，并只关注包含“motile”的记录
  filtered_data <- df_tax_data %>%
    filter(!is.na(Rank_8)) 
  count_rank8 <- df_tax_data %>%
    filter(!is.na(Rank_7)) %>%
    summarise(count = n())
  # 计算 Rank_8 中每个物种的数量
  rank_8_counts <- filtered_data %>%
    group_by(Rank_8) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  
  # 获取数量排在前十和后十的物种
  top_10_species <- head(rank_8_counts, 10)
  bottom_10_species <- tail(rank_8_counts, 10)
  # 合并前十和后十的物种数据
  highlight_species <- bind_rows(top_10_species, bottom_10_species)
  
  # 绘制 Rank_8 中物种数量分布的条形图
  ph <- ggplot(highlight_species, aes(x = Rank_8, y = count)) +
    geom_bar(data = highlight_species, aes(x = reorder(Rank_8, -count), y = count), stat = "identity", fill = "red") +
    coord_flip() +
    ggtitle("Top & Bottom 10 in Rank_8") +
    xlab("Species") +
    ylab("Freq") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggsave("../plot/tax.png", plot = ph, width = 8, height = 6, dpi = 300, bg = "white")

  
  
  rank_8_data <- filtered_data %>%
  group_by(Rank_8) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  slice(1:20) # 取出现频率最少的前10个物种

  ggplot(rank_8_data, aes(x = reorder(Rank_8, count), y = count)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ggtitle("Lowest abundance Rank_8") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Species", y = "Freq")

  
  
}



###### 描述性分析图
{
  data <- df_cOTUs_sample_data %>% select(Lat = decimalLatitude, long = decimalLongitude, coy = country)
  unique_countries <- data %>% group_by(coy) %>% slice(1)
  # 绘制世界地图
  world_map <- map_data("world")
  data <- na.omit(data)
  
  

  # 绘制世界地图并添加样本位置
  ppp <- ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 fill = "lightgray", color = "white") +
    geom_point(data = data, aes(x = long, y = Lat),
               color = "red", size = 0.8) +
    ggtitle("ARMS Locations on World Map") +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_minimal() +
    theme(
      aspect.ratio = 1/1.4 # 宽高比设置为2:1
    ) +
    # 手动添加海域标注
    annotate("text", x = -30, y = 40, label = "Atlantic Ocean", color = "blue", size = 3) +
    annotate("text", x = 150, y = -10, label = "Indian Ocean", color = "blue", size = 3) +
    annotate("text", x = -160, y = 20, label = "Pacific Ocean", color = "blue", size = 3) +
    annotate("text", x = 20, y = -60, label = "Southern Ocean", color = "blue", size = 3) +
    annotate("text", x = 0, y = 70, label = "Arctic Ocean", color = "blue", size = 3) +
    geom_text(data = unique_countries, aes(x = long, y = Lat, label = coy),
              vjust = -1.1, hjust = 0.7, size = 3, color = "black")
  print(ppp)
  dev.off()
}

ggsave("../plot/Map.png", plot = ppp, width = 8, height = 6, dpi = 300, bg = "white") 
png("../plot/Map.png")
###### Hill Number in Each Samples
{
  # 归一化样本丰度
  # 计算每个样本的总丰度
  sample_sums <- colSums(cOTUs_otu_data[, -1])  # 第一列是OTU标识，不计算
  # 归一化丰度值
  normalized_otu <- sweep(cOTUs_otu_data[, -1], 2, sample_sums, "/")
  # 将归一化丰度值储存为CSV
  write.csv(
    normalized_otu,
    "../data/ps_COI_motile_cOTUs_normalized_otu_data.csv",
    row.names = FALSE
  )
  # 计算Hill numbers
  # 初始化存储结果的数据框
  df_otu_data <- data.frame(cOTUs_otu_data)
  hill_numbers <- data.frame(
    ARMS = colnames(df_otu_data),
    q0 = NA,
    q1 = NA,
    q2 = NA
  )
  
  # 计算Hill numbers
  for (i in 1:ncol(df_otu_data)) {
    counts <- df_otu_data[, i]
    
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
  
  write.csv(hill_numbers,
            "../data/sample_hill_numbers.csv",
            row.names = FALSE)
  
  # uniqueID_data <- combined_data %>%
  #   group_by(eventID) %>%
  #   slice_tail(n = 1) %>%
  #   select(eventID, date, RESET_score)
  #
  # write.csv(uniqueID_data, "../data/uniqueID_RESET_data.csv", row.names = FALSE)
}


####### Combined data
{
  # 提取otu_data中的匹配部分
  otu_hill_number_data <- hill_numbers
  
  # 提取sample_data中的匹配部分
  sample_data <- df_cOTUs_sample_data %>%
    rownames_to_column(var = "ARMS")
  
  
  # 合并OTU数据和采样信息表
  merged_otu_sample <- otu_hill_number_data %>%
    inner_join(sample_data, by = "ARMS")
  
  # 合并FR_data
  FR_data2 <- FR_data %>% rename(eventID = ARMS)
  merged_3_data <- merged_otu_sample %>%
    inner_join(FR_data2, by = "eventID")
  
  merged_RESET_data <- merged_3_data %>%
    inner_join(combined_data, by = "eventID")
  
  
  # 分组并聚合SE_score和RESET_score
  se_reset_mean <- combined_data %>%
    group_by(eventID, variable) %>%
    summarise(
      SE_mean_score = mean(SE_score),
      RESET_mean_score = mean(RESET_score)
    )
  
  
  se_reset_mean_sample <- se_reset_mean %>% inner_join(merged_otu_sample, by = "eventID")
  
    
  write.csv(merged_RESET_data,
            "../data/merged_RESET_data.csv",
            row.names = FALSE)
}

###### 探究q0 & RESET_score & Anthro_stress之间的关系
{
  ### lm 数据处理
  {
    data_pre <- combined_data %>%
      group_by(eventID) %>%
      summarise(RESET_mean = mean(RESET_score)) %>%
      ungroup()
    
    merged_re_mean_data <- merged_3_data %>%
      inner_join(data_pre, by = "eventID")
    
    # 提取q0和RESET_score列
    data1 <- merged_RESET_data[c('q0', 'q1', 'q2', 'RESET_score', 'lengthDeployment')]
    data2 <- merged_re_mean_data[c(
      'eventID',
      'q0',
      'q1',
      'q2',
      'cumul_score',
      'grav_NC',
      'pop_count',
      'num_ports',
      'reef_value',
      'nutrient',
      'sediment'
    )]
    # 拟合线性回归模型
    lm_q0 <- lm(q0 ~ RESET_score, data = data1)
    lm_q1 <- lm(q1 ~ RESET_score, data = data1)
    lm_q2 <- lm(q2 ~ RESET_score, data = data1)
    
    lm_anthro_q0 <- lm(q0 ~ cumul_score, data = merged_re_mean_data)
    

    # 构建混合效应模型
    lmer_q0 <- lmer(q0 ~ RESET_score + (1 |
                                          lengthDeployment), data = data1)
    
    r2_values <- r.squaredGLMM(lmer_q0)
    
    lmer_q1 <- lmer(q1 ~ RESET_score + (1 |
                                          lengthDeployment), data = data1)
    lmer_q2 <- lmer(q2 ~ RESET_score + (1 |
                                          lengthDeployment), data = data1)
    
    # Anthro_stress_lm
    lm_Anthro_stress_q0 <- lm(q0 ~ grav_NC + num_ports + reef_value + sediment, data = data2)
    
    # depth of deployment and fishing
    lmer_a_d_q0 <- lmer(q0 ~ grav_NC + (1 | lengthDeployment), data = merged_re_mean_data)
  }
  ### glm 数据处理
  {
    # glm 模型
    # 绘制QQ图
    qqnorm(data2$q0)
    qqline(data2$q0, col = "red")
    data2_test <- data2 %>% select('grav_NC',
                                   'pop_count',
                                   'num_ports',
                                   'reef_value',
                                   'nutrient',
                                   'sediment')
    
    # 变量列表
    variables <- c('grav_NC',
                   'pop_count',
                   'num_ports',
                   'reef_value',
                   'nutrient',
                   'sediment')
    
    # 绘制每个变量的QQ图
    par(mfrow = c(2, 3))  # 设置图形布局为2行3列
    for (var in variables) {
      qqnorm(data2_test[[var]], main = paste("QQ Plot of", var))
      qqline(data2_test[[var]], col = "red")
    }
    par(mfrow = c(1, 1))  # 恢复默认图形布局
    
    ### 检查共线性
    model <- glm(q0 ~ grav_NC + pop_count + num_ports + reef_value + sediment + nutrient, family = gaussian(link = "identity"), data = data2)
    vif(model)

    ### 模型拟合
    glm_q0 <- glm(
      q0 ~ num_ports + reef_value + sediment + nutrient,
      data = data2,
      family = gaussian(link = "identity")
    )
    glm_q1 <- glm(
      q1 ~ grav_NC + num_ports + reef_value + sediment + nutrient,
      data = data2,
      family = gaussian(link = "identity")
    )
    glm_q2 <- glm(
      q2 ~ grav_NC + num_ports + reef_value + sediment + nutrient,
      data = data2,
      family = gaussian(link = "identity")
    )
  }
  # 显示模型摘要
  summary(lm_q0)
  summary(lm_q1)
  summary(lm_q2)
  summary(lmer_q0)
  summary(lmer_q1)
  summary(lmer_q2)
  summary(glm_q0)
  summary(glm_q1)
  summary(glm_q2)
  summary(lm_anthro_cumul_q0)
  summary(lm_Anthro_stress_q0)
  summary(lmer_a_d_q0)
  ### 线性模型回归可视化 q0, q1, q2 & RESET_score & cucml_score
  {
    ### RESET_score
    ggplot(data1, aes(x = RESET_score, y = q0)) +
      geom_point(size = 0.5) +
      geom_smooth(method = "lm", col = "red") +
      labs(title = "Linear Regression of q0 and RESET_score", x = "RESET_score", y = "q0") +
      theme_minimal()
    
    ggplot(data1, aes(x = RESET_score, y = q1)) +
      geom_point(size = 0.5) +
      geom_smooth(method = "lm", col = "red") +
      labs(title = "Linear Regression of q1 and RESET_score", x = "RESET_score", y = "q1") +
      theme_minimal()
    
    ggplot(data1, aes(x = RESET_score, y = q2)) +
      geom_point(size = 0.5) +
      geom_smooth(method = "lm", col = "red") +
      labs(title = "Linear Regression of q2 and RESET_score", x = "RESET_score", y = "q2") +
      theme_minimal()
    
    ### anthropology cumul_score
    ggplot(merged_re_mean_data, aes(x = cumul_score, y = q0)) +
      geom_point(size = 0.5) +
      geom_smooth(method = "lm", col = "red") +
      labs(title = "Linear Regression of q0 and cumul_score", x = "cumul_score", y = "q0") +
      theme_minimal()
    
    ggplot(merged_re_mean_data, aes(x = cumul_score, y = q1)) +
      geom_point(size = 0.5) +
      geom_smooth(method = "lm", col = "red") +
      labs(title = "Linear Regression of q1 and cumul_score", x = "cumul_score", y = "q1") +
      theme_minimal()
    
    ggplot(merged_re_mean_data, aes(x = cumul_score, y = q2)) +
      geom_point(size = 0.5) +
      geom_smooth(method = "lm", col = "red") +
      labs(title = "Linear Regression of q2 and cumul_score", x = "cumul_score", y = "q2") +
      theme_minimal()
    
    ### 人为压力和q0关系
    pdf("../data/plot_anthroStress_hillNumber.pdf")
    data_clean <- subset(data2, select = c(-q1, -q2, -cumul_score, - eventID, -nutrient, -num_ports))
    data_long_as_q0 <- reshape2::melt(data_clean, id.vars = "q0")
    p1 <- ggplot(data_long_as_q0, aes(x = value, y = q0)) +
      geom_point(size = 0.7) +
      geom_smooth(method = "lm", col = "red") +
      facet_wrap(~ variable, scales = "free_x") +
      labs(title = "Linear Regression of q0 and Anthro Stress", x = "AnthroStress", y = "q0") +
      theme_minimal()
    
    data_clean1 <- subset(data2, select = c(-q0, -q2, -cumul_score, - eventID, -nutrient, -num_ports))
    data_long_as_q1 <- reshape2::melt(data_clean1, id.vars = "q1")
    p2 <- ggplot(data_long_as_q1, aes(x = value, y = q1)) +
      geom_point(size = 0.7) +
      geom_smooth(method = "lm", col = "red") +
      facet_wrap(~ variable, scales = "free_x") +
      labs(title = "Linear Regression of q1 and Anthro Stress", x = "AnthroStress", y = "q1") +
      theme_minimal()
    
    data_clean2 <- subset(data2, select = c(-q1, -q0, -cumul_score, - eventID, -nutrient, -num_ports))
    data_long_as_q2 <- reshape2::melt(data_clean2, id.vars = "q2")
    p3 <- ggplot(data_long_as_q2, aes(x = value, y = q2)) +
      geom_point(size = 0.7) +
      geom_smooth(method = "lm", col = "red") +
      facet_wrap(~ variable, scales = "free_x") +
      labs(title = "Linear Regression of q2 and Anthro Stress", x = "AnthroStress", y = "q2") +
      theme_minimal()
    print(p1)
    print(p2)
    print(p3)
    dev.off()
    
  }
  
  ### 混合线性模型可视化 q0 ,q1, q2 & RESET_score & lengthDeployment
  {
    ### grav_NC 和 放置深度对q0
    ggplot(merged_re_mean_data, aes(x = grav_NC, y = q0, color = lengthDeployment)) +
      geom_point() +  # 绘制散点图
      geom_smooth(method = "lm", se = FALSE) +  # 添加回归线
      facet_wrap(~ lengthDeployment) +  # 按 lengthDeployment 分组
      labs(title = "Scatter Plot of grav_NC vs q0 by lengthDeployment",
           x = "grav_NC",
           y = "q0") +
      theme_minimal()
    
    ### RESET_score 和 样本大小对q0
    pdf("../data/plot_reset_score_q0_inSize.pdf")
    
    p0 <- ggplot(merged_re_mean_data, aes(x = RESET_mean, y = q0, color = sampleSizeFractionation)) +
      geom_point() +  # 绘制散点图
      geom_smooth(method = "lm", se = FALSE) +  # 添加回归线
      facet_wrap(~ sampleSizeFractionation) +  # 按 lengthDeployment 分组
      labs(title = "Scatter Plot of grav_NC vs q0 by sampleSizeFractionation",
           x = "RESET_score",
           y = "q0") +
      theme_minimal()
    
    print(p)
     
    dev.off()
    
    data_t <- merged_re_mean_data %>% select(c(pop_count, q0, lengthDeployment))
    data_t <- data_t %>% filter(lengthDeployment == 36)
    lm_t <- lm(q0 ~ pop_count, data = data_t)
    summary(lm_t)
    ggplot(merged_re_mean_data, aes(x = grav_NC, y = q0, color = country)) +
    geom_point() +  # 绘制散点图
    geom_smooth(method = "lm", se = FALSE) +  # 添加回归线
    facet_wrap(~ country) +  # 按 lengthDeployment 分组
    labs(title = "Scatter Plot of grav_NC vs q0 by sampleSizeFractionation",
         x = "RESET_score",
         y = "q0") +
    theme_minimal()
    
    ggplot(merged_re_mean_data, aes(x = lengthDeployment, y = grav_NC)) +
      geom_point(size = 0.8) +  # 绘制散点图
      geom_smooth(method = "lm", se = FALSE) +  # 添加回归线
      labs(title = "Scatter Plot of lengthDeployment vs grav_NC",
           x = "lengthDeployment",
           y = "grav_NC") +
      theme_minimal()
    
    ### 
    data1$fitted0 <- fitted(lmer_q0)
    ggplot(data1, aes(x = fitted0, y = q0)) +
      geom_point(alpha = 0.8,
                 size = 1,
                 shape = 18) +
      geom_abline(
        slope = 1,
        intercept = 0,
        color = "red",
        linetype = "dashed"
      ) +
      labs(title = "Fitted vs. Observed values", x = "Fitted values", y = "Observed values") +
      theme_minimal()
    
    data1$fitted1 <- fitted(lmer_q1)
    ggplot(data1, aes(x = fitted1, y = q1)) +
      geom_point(alpha = 0.8,
                 size = 1,
                 shape = 18) +
      geom_abline(
        slope = 1,
        intercept = 0,
        color = "red",
        linetype = "dashed"
      ) +
      labs(title = "Fitted vs. Observed values", x = "Fitted values", y = "Observed values") +
      theme_minimal()
    
    data1$fitted2 <- fitted(lmer_q2)
    ggplot(data1, aes(x = fitted2, y = q2)) +
      geom_point(alpha = 0.8,
                 size = 1,
                 shape = 18) +
      geom_abline(
        slope = 1,
        intercept = 0,
        color = "red",
        linetype = "dashed"
      ) +
      labs(title = "Fitted vs. Observed values", x = "Fitted values", y = "Observed values") +
      theme_minimal()
    
  }
  
  ### 广义线性模型预测 Anthropology stress & q0, q1, q2
  {
    # 重构数据框以适应 facet_wrap
    data3 <- subset(data2, select = c(-q1, -q2, -cumul_score, -eventID))
    
    data_long_q0 <- reshape2::melt(data3, id.vars = "q0")
    # 使用 facet_wrap 可视化多个自变量
    ggplot(data_long_q0, aes(x = value, y = q0)) +
      geom_point(alpha = 0.8, size = 0.8) +
      geom_smooth(
        method = "glm",
        method.args = list(family = "gaussian"),
        col = "red"
      ) +
      facet_wrap(~ variable, scales = "free_x") +
      labs(title = "Multiple Variables vs q0 with gaussian GLM Fit", x = "Value", y = "q0") +
      theme_minimal()
    
    
    data4 <- subset(data2, select = c(-q0, -q2, -cumul_score, -eventID))
    data_long_q1 <- reshape2::melt(data4, id.vars = "q1")
    # 使用 facet_wrap 可视化多个自变量
    ggplot(data_long_q1, aes(x = value, y = q1)) +
      geom_point(alpha = 0.8, size = 0.8) +
      geom_smooth(
        method = "glm",
        method.args = list(family = "gaussian"),
        col = "red"
      ) +
      facet_wrap(~ variable, scales = "free_x") +
      labs(title = "Multiple Variables vs q0", x = "Value", y = "q1") +
      theme_minimal()
    
    data5 <- subset(data2, select = c(-q0, -q1, -cumul_score, -eventID))
    data_long_q2 <- reshape2::melt(data5, id.vars = "q2")
    # 使用 facet_wrap 可视化多个自变量
    ggplot(data_long_q2, aes(x = value, y = q2)) +
      geom_point(alpha = 0.8, size = 0.8) +
      geom_smooth(
        method = "glm",
        method.args = list(family = "gaussian"),
        col = "red"
      ) +
      facet_wrap(~ variable, scales = "free_x") +
      labs(title = "Multiple Variables vs q0 with gaussian GLM Fit", x = "Value", y = "q2") +
      theme_minimal()
    
    
    
  }
}

###### q0 对 SE_score
{
  lm_var_q0 <- lm(log(q0) ~  grav_NC + sediment + pop_count + reef_value, data = se_reset_mean_anthrostress)
  summary(lm_var_q0)
  
  ggplot(se_reset_mean_anthrostress, aes(x = reef_value, y = log(q0), colour = variable)) +
    geom_point(size = 0.8) +
    geom_smooth(method = "lm", col = "red") +
    labs(title = paste("Linear Regression of q0 and mean SE_score -", var), x = "SE_score", y = "q0") +      
    theme_minimal()
  
  pdf("../data/q1_SE_mean_plots.pdf")
  p_qq <- ggplot(se_reset_mean_sample, aes(x = SE_mean_score, y = q1)) +
    geom_point(size = 0.8) +
    geom_smooth(method = "lm", col = "red") +
    facet_wrap(~ variable, scales = "free") +
    ggtitle("SE_score vs q1 by Variable") +
    xlab("SE_score: Quantified measures from various environmental factors.") +
    ylab("q1: A metric to quantify motile diversity") +
    theme_minimal()
  print(p_qq)
  dev.off()
  
  wide_data <- se_reset_mean_sample %>% select(variable = variable, SE = SE_mean_score, q1 = q1)
   
  tree_data <- wide_data %>%  pivot_wider(names_from = variable, values_from = c(SE)) 
  trees_data <- tree_data[, -1]
  colnames(trees_data)[4] <- "SST_anomaly"
  colnames(trees_data)[5] <- "SST_variability"
  
  # 将数据分为训练集和测试集
  variables = c('DHW', 'SST', 'SST_anomaly', 'SST_variability', 'cloud', 'current', 'depth', 'salinity', 'wind', "grav_NC", "sediment", "nutrient", "pop_count", "num_ports", "reef_value")
  data_random <- data %>% select(all_of(variables), q1)
  data_random <- na.omit(data_random)
  # 构建随机森林模型（假设数据已经被分割为train_data和test_data）
  rf_model <- randomForest(as.formula(paste("q1 ~", paste(variables, collapse = " + "))), data = data_random, ntree = 500)
  
  random_p <- varImpPlot(rf_model, main = "Variable Importance Plot")
  png("../plot/RandomF_AnthroStress.png", width = 8, height = 6, units = "in", res = 300)
  varImpPlot(rf_model, main = "Variable Importance Plot")
  dev.off()
  
  pdf("../data/")
  # 分图
  variables <- unique(se_reset_mean_sample$variable)
  for (var in variables) {
    lm_logq0 <- lm(q1 ~ SE_mean_score, data = datat)
    print(summary(lm_logq0))
  }
  dataxd <- se_reset_mean_sample %>% filter(variable == "SST anomaly")
  write.csv(datat, "../data/datat.csv", row.names = FALSE)
  lm_se <- lm(q0 ~ SE_mean_score, data = dataxd)
  summary(lm_se)
  
  pdf("../data/q0_SSTAnomaly_SE_score.pdf")
  # 绘制空白页面
  plot.new()
  # 将文本输出到图形设备中
  text(0, 1, paste(capture.output(summary(lm_se)), collapse = "\n"), adj = c(0, 1), cex = 0.7)
  
  p <- ggplot(dataxd, aes(x = SE_mean_score, y = q0)) +
    geom_point(size = 0.8) +
    geom_smooth(method = "lm", col = "red") +
    labs(title = paste("Linear Regression of q0 and mean SE_score -", var), x = "SE_score", y = "q0") +      
    theme_minimal()
  print(p)
  dev.off()
  
  
  datatt <- se_reset_mean_sample %>% filter(variable == "SST anomaly")
  p <- ggplot(datat, aes(x = SE_mean_score, y = q0)) +
    geom_point(size = 0.8) +
    geom_smooth(method = "lm", col = "red") +
    facet_wrap(~ country, scales = "free_x") +
    labs(title = paste("Linear Regression of q0 and mean SE_score -", var), x = "SE_score", y = "q0") +      
    theme_minimal()
  print(p)
  
  
  
  
  #总图
  df_se_wide <- se_reset_mean_sample %>%
    pivot_wider(names_from = variable, values_from = SE_mean_score)
  
  # 运行线性回归
  model <- lm(q1 ~ DHW + SST + `SST anomaly` + `SST variability` + cloud + current + depth + salinity + wind, data = df_se_wide)
  
  # 查看模型摘要
  summary(model)
  
  pdf("../plot/model_se_mean_q1.pdf")
  plot.new()
  text(0, 1, paste(capture.output(summary(model)), collapse = "\n"), adj = c(0, 1), cex = 1)
  dev.off()
  
  
  sep <- ggplot(se_reset_mean_sample, aes(x = SE_mean_score, y = q1)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", col = "red") +
    facet_wrap(~ variable, scales = "free_x") +
    labs(title = "Linear Regression of q0 and mean SE_score", x = "SE_score", y = "q0") +
    theme_minimal()
  ggsave("../plot/se_mean.png", plot = p_qq, width = 8, height = 6, dpi = 300, bg = "white") 
  

}

###### 相关性检测
{
  
  se_reset_mean_anthrostress <- se_reset_mean_sample %>% inner_join(FR_data2, by = "eventID")
  # 计算皮尔逊相关系数
  pearson_corr <- cor(se_reset_mean_anthrostress$RESET_mean_score, se_reset_mean_anthrostress$cumul_score, method = "pearson")
  
  # 打印结果  
  print(paste("Pearson correlation coefficient:", pearson_corr))
}


###### 研究所有ARMS中每个OTU的分布
{
  # 合并两个数据框，使用OTU代号作为键
  merged_otu_tax_data <- merge(df_otu_data, df_tax_data, by = "row.names")
  
  
  
  # 将合并后的数据框的行名设置为原来的OTU代号
  rownames(merged_otu_tax_data) <- merged_otu_tax_data$Row.names
  
  
  # 绘制OTU在各个样本中的分布图
  ggplot(merged_otu_tax_data, aes(x = Sample, y = sum(X))) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Distribution of OTUs in Different Samples", x = "Sample", y = "Count")
  
  
}


{
  grouped_data <- merged_RESET_data %>%
    group_by(variable, eventID) %>%
    summarise(
      q0 = first(q0),
      q1 = first(q1),
      q2 = first(q2),
      RESET_score_mean = mean(RESET_score),
      na.rm = TRUE
    ) %>% ungroup()
  
  # 可视化: x为RESET_score_mean, y为q0, 按variable分成不同的图表

  }


###### Stress Data Analysis
{
  ######压力数据分析
  # 变量回归分析RESET_Score
  wide_data <- combined_data %>%
    select(eventID, date, variable, SE_score, RESET_score) %>%
    spread(key = variable, value = SE_score)
  model_all <- lm(RESET_score ~ ., data = wide_data)
  summary(model_all)
  
  # 研究RESET Score随着时间变化的趋势(显示累积应激评分随时间的趋势)
  ggplot(phyloseq_reset_cumul, aes(x = date, y = RESET_score)) +
    geom_line(color = "blue") +
    geom_point(color = "red", size = 0.5) +
    labs(title = "RESET Score Over Time", x = "Date", y = "RESET Score") +
    theme_minimal()
  
  #箱线图体现 不同变量的SE评分(显示不同变量对总体应激评分的贡献)
  ggplot(phyloseq_reset_SE,
         aes(x = variable, y = SE_score, fill = stress_category)) +
    geom_boxplot() +
    labs(title = "SE Score by Variable", x = "Variable", y = "SE Score") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  
  #研究RESET Score和 SE之间的趋势(直观地看到每个变量的SE评分与RESET评分之间的关系，
  #并识别出哪些变量对总体应激评分有显著影响)
  ggplot(combined_data,
         aes(x = SE_score, y = RESET_score, color = variable)) +
    geom_point(alpha = 0.5,
               shape = 16,
               size = 0.7) +  # 散点图
    geom_smooth(
      method = "lm",
      se = FALSE,
      linetype = "dashed",
      colour = "black"
    ) +  # 回归线
    facet_wrap(~ variable, scales = "free_x") +  # 分面展示每个变量
    labs(title = "RESET Score vs Variable SE Score", x = "SE Score", y = "RESET Score") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  
  
  # 可视化每个威胁因素的分布
  FR_data %>%
    select(grav_NC, pop_count, num_ports, reef_value, sediment, nutrient) %>%
    gather(key = "Threat_Factor", value = "Value") %>%
    ggplot(aes(x = Value)) +
    geom_histogram(
      binwidth = 0.05,
      fill = "blue",
      color = "black",
      alpha = 0.7
    ) +
    facet_wrap(~ Threat_Factor, scales = "free_x") +
    labs(title = "Distribution of Major Threat Factors", x = "Value", y = "Frequency") +
    theme_minimal()
  
  # 可视化每个威胁因素的箱线图
  FR_data %>%
    select(grav_NC, pop_count, num_ports, reef_value, sediment, nutrient) %>%
    gather(key = "Threat_Factor", value = "Value") %>%
    ggplot(aes(x = Threat_Factor, y = Value)) +
    geom_boxplot(fill = "red",
                 color = "black",
                 alpha = 0.7) +
    labs(title = "Boxplot of Major Threat Factors", x = "Threat Factor", y = "Value") +
    theme_minimal()
  
  
  # 拟合线性模型
  lm_model <- lm(cumul_score ~ grav_NC + pop_count + num_ports + reef_value + sediment + nutrient,
                 data = FR_data)
  vif_values <- vif(lm_model)
  print(vif_values)
  
  glm_model <- glm(
    cumul_score ~ grav_NC + pop_count + num_ports + reef_value + sediment + nutrient,
    data = FR_data,
    family = gaussian(link = "identity")
  )
  summary(glm_model)
  
  glm_plot <- ggplot(data = FR_data, aes(x = fitted(glm_model), y = cumul_score)) +
    geom_point() +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = "red"
    ) +
    labs(title = "Fitted vs Actual Values", x = "Fitted Values", y = "Actual Values")
  
  
  lmm <- lm(cumul_score ~ num_ports, data = FR_data)
  summary(lmm)
  
  
  # 可视化 1：绘制散点图和回归线
  plot(
    FR_data$num_ports,
    FR_data$cumul_score,
    main = "Scatter Plot with Regression Line",
    xlab = "Number of Ports",
    ylab = "Cumulative Score",
    pch = 19
  )
  abline(lmm, col = "red", lwd = 2)
  
  # 可视化 2：残差图
  plot(
    glm_model$fitted.values,
    glm_model$residuals,
    main = "Residual Plot",
    xlab = "Fitted Values",
    ylab = "Residuals",
    pch = 19
  )
  abline(h = 0, col = "red", lwd = 2)
  
}


###### Random Forest
{
  # 选择多样性指标作为响应变量
  response_variable <- "RESET_score" # 替换为实际的多样性指标列名
  
  Rforest_test_data <- merged_RESET_data %>% select(
    "q0",
    "eventID",
    "grav_NC",
    "sediment",
    "nutrient",
    "pop_count",
    "num_ports",
    "reef_value",
    "cumul_score",
    "variable",
    "SE_score",
    "RESET_score"
  )
  
  predictor_variables <- setdiff(names(Rforest_test_data),
                                 c("q0", response_variable, "eventID", "cumul_score"))
  
  # 处理缺失值
  Rforest_test_data <- Rforest_test_data %>%
    select(c(response_variable, predictor_variables)) %>%
    na.omit()
  
  # 构建随机森林模型
  set.seed(123)
  rf_model <- randomForest(
    as.formula(paste(response_variable, "~ .")),
    data = Rforest_test_data,
    importance = TRUE,
    ntree = 500
  )
  
  # 输出随机森林模型结果
  print(rf_model)
  
  # 变量重要性图(Percentage Increase in Mean Squared Error & Increase in Node Purity)
  importance <- importance(rf_model)
  varImpPlot(rf_model)
  
}


###### RESET_Score analysis
{
  eventIDs <- unique(combined_data$eventID)
  variables <- unique(combined_data$variable)
  # 定义一个函数来绘制图表
  plot_reset_score <- function(eventID_value, var) {
    subset <- combined_data %>% filter(eventID == eventID_value, variable == var)
    p <- ggplot(subset, aes(x = date, y = RESET_score)) +
      geom_line() +
      geom_point(colour = "red") +
      labs(
        title = paste(
          "Reset Score Over Time for eventID and variable:",
          eventID_value,
          var
        ),
        x = "Date",
        y = "RESET Score"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p)
  }
  
  # 绘制所有eventID的图表
  for (id in eventIDs[1]) {
    for (var in variables) {
      plot_reset_score(id, var)
    }
  }
  
  
  mean_var <- combined_data %>% group_by(eventID, Month) %>% summarise(mean_reset_score = mean(RESET_score, na.rm = TRUE))
  
  Month_data <- unique(combined_data$Month)
  mean_var_month <- mean_var %>% filter(Month == Month_data[1])
  p <- ggplot(mean_var_month, aes(x = eventID, y = mean_reset_score)) +
    geom_line() +
    geom_point(colour = "red") +
    labs(
      title = paste("Reset Score for eventID and variable:"),
      x = "Date",
      y = "RESET Score"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
  
  
  glm_reset_model <- glm(
    RESET_score ~ SE_score + variable,
    data = combined_data,
    family = gaussian(link = "identity")
  )
  summary(glm_reset_model)
}

