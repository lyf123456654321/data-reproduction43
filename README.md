# 基于期刊 Journal of Hazardous Materials 中的文献 Enhanced nitrogen fixation and Cd passivation in rhizosphere soil by biochar-loaded nitrogen-fixing bacteria: Chemisorption and microbial mechanism（https://doi.org/10.1016/j.jhazmat.2024.136588) 进行数据复现Fig.2c。

# 解决思路
小组在进行复现文献数据图时，首先明确了目标数据图的类型和结构，通过解析原图的坐标系、核心元素（如柱状图、误差条）及辅助信息（图例、注释），确定适用的工具包（'circlize'绘制环形图）。数据收集阶段通过给文献作者发送邮件获取原始数据。代码实现时通过观看视频学习以及AI工具如deepseek辅助生成代码框架以及解释报错信息。从数据验证、可视化对比到参数微调，整体流程进行了多次迭代优化，最终实现了数据的复现。

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
    
    # 计算最大值并进行调整
    if (max(sector_data$value) < 10) {
      max_value <- ceiling(max(sector_data$value) * 1.3 )   # 调整最大值
    } else if (max(sector_data$value) >= 10 & max(sector_data$value) < 100) {
      max_value <- ceiling(max(sector_data$value) * 1.3 / 10) * 10   # 调整为 10 的整倍数
    } else if (max(sector_data$value) >= 100 & max(sector_data$value) < 1000) {
      max_value <- ceiling(max(sector_data$value) * 1.2 / 40) * 40  # 调整为 40 的整倍数
    } else if (max(sector_data$value) >= 1000) {
      max_value <- ceiling(max(sector_data$value) * 1.2 / 400) * 400  # 调整为 40 的整倍数
    }
    
    # 添加 y 轴刻度线和标签
    at <- seq(0, max_value, by = max_value / 4)
    at <- at[-c(1, length(at))]  # 移除第一个和最后一个刻度线和标签
    for (a in at) {
      circos.lines(
        CELL_META$xlim,
        c(a, a) / max_value, 
        lty = 5, 
        col = "grey40"
      )
      circos.text(
        CELL_META$cell.xlim[1] - mm_h(2), 
        a / max_value, labels = a,
        facing = "clockwise", 
        adj = c(0.5, 0), 
        cex = 0.3
      )

 # 在左边添加刻度线
      circos.lines(
        CELL_META$cell.xlim[1] + c(-0.02, 0), 
        c(a, a) / max_value
      )
      circos.axis(
        h = "top",
        major.at = seq(0, 1, by = 0.3),
        major.tick.length = -0.05,
        minor.ticks = 1,
        labels = FALSE
      )
    }    
    
    bar_width <- 0.2  # 条形图宽度
    gap_width <- 0.1  # 条形图之间的间隔宽度
    
    # 计算每个类别内柱子的数量
    num_bars <- nrow(sector_data)
    
    for (i in 1:num_bars) {
      # 计算每个柱子的起始和结束位置
      xleft <- (i+0.3) * (bar_width + gap_width) - (num_bars - 1) * (bar_width + gap_width) / 2
      xright <- xleft + bar_width
      
      # 添加误差线
      error_top <- sector_data$value[i] + sector_data$std_error[i]
      error_bottom <- sector_data$value[i] - sector_data$std_error[i]
      circos.segments(
        (xleft + xright) / 2, error_bottom / max_value,
        (xleft + xright) / 2, error_top / max_value,
        col = "black",
        lwd = 1
      )
      circos.segments(
        (xleft + xright) / 2 - 0.03, error_top / max_value,
        (xleft + xright) / 2 + 0.03, error_top / max_value,
        col = "black",
        lwd = 1
      )

      # 添加条形图
      circos.rect(
        xleft = xleft, 
        ybottom = 0, 
        xright = xright, 
        ytop = sector_data$value[i] / max_value,
        col = colors[sector_data$group[i]], 
        border = "black",
        lwd = 1
      )
      
      # 添加显著性标记
      circos.text(
        (xleft + xright) / 2,  # 中间位置
        (sector_data$value[i] + sector_data$std_error[i]) / max_value + 0.05,  # 设置符号在柱子上方
        labels = sector_data$significance[i],  # 使用显著性标记
        col = "black", 
        cex = 0.8,
        facing = "inside",
        adj = c(0.5, 0)
      )
    }
  }
)

# 添加图例
legend("topright", legend = c("KB", "BC", "BAc"), fill = colors, bty = "n", cex = 0.8)
