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

circos.clear()
circos.par(start.degree = 90, 
           gap.degree = 7, 
           track.margin = c(0, 0.12), 
           cell.padding = c(0, 0, 0, 0)
)
circos.initialize(factors = data$category, xlim = c(0, 1))

# 添加扇形分类标签和填充颜色
circos.track(
  factors = data$category, 
  ylim = c(0, 1), 
  track.height = 0.035, 
  bg.col = category_colors,
  panel.fun = function(x, y) {
    circos.text(
      CELL_META$xcenter,
      CELL_META$ylim[2] - 3.2,
      CELL_META$sector.index, 
      facing = "bending.inside", 
      cex = 0.8, 
      adj = c(0.5, 0)
    )
  }
)

# 绘制扇形条形图，填充背景色和设置透明度
circos.trackPlotRegion(
  factors = data$category, 
  ylim = c(0, 1), 
  track.height = 0.47, 
  bg.col = adjustcolor(category_colors, alpha.f = 0.4), 
  panel.fun = function(x, y) {
    sector_data <- data[data$category == CELL_META$sector.index, ]
