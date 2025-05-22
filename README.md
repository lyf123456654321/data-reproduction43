# 基于期刊 Journal of Hazardous Materials 中的文献 Enhanced nitrogen fixation and Cd passivation in rhizosphere soil by biochar-loaded nitrogen-fixing bacteria: Chemisorption and microbial mechanism（https://doi.org/10.1016/j.jhazmat.2024.136588) 进行数据复现。
library(circlize)

# 读取数据
data  = readxl::read_xlsx("circlize_bar_with_errorbar.xlsx")

data$category <- factor(data$category, levels = c("Forks", "Crossings", "Length", "SurfArea", "AvgDiam", "RootVolume", "Tips"))

# 定义颜色
colors <- c("KB" = "#70c3f0", "BC" = "#bfe0f6", "BAc" = "#e0eef6")
category_colors <- c("Forks" = "#fdf2c5", 
                     "Crossings" = "#ffeff9", 
                     "Length" = "#fab8cb", 
                     "SurfArea" = "#e6387e", 
                     "AvgDiam" = "#5aa82d", 
                     "RootVolume" = "#a1ca9e", 
                     "Tips" = "#e1eada")

# 设置全局字体家族为 Times New Roman
par(family = "Times")
