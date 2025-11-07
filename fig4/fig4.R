fig4B_data <- readRDS('./07.analysis/02.Rdata/fig4B_data.RData')
meta_prozotoa <- ggplot(fig4B_data,aes(x = estimate,y = prozotoa))+
  # 绘制点估计和置信区间
  geom_point(aes(color = color,fill=color,size=size),shape=23)+
  # geom_point(aes(x=estimate,y = feature),shape=15,size=2)+
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.1, linewidth = 0.1)+# 添加无效线（x = 0）
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40")+
  theme_bw()+
  scale_color_manual(values = c('grey','red'))+
  scale_fill_manual(values = c('grey','red'))+
  xlab('Estimate')+
  # scale_x_continuous(
  #   limits = c(-5, 8),
  #   breaks = seq(-5,8, by = 2))+
  # geom_text(aes(x = -0.4, y = feature,label = sig),color='red',vjust=0.8)+
  theme(legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(color = 'black'),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'none',
        panel.border = element_blank(),  # 移除面板边框
        axis.line = element_line(color = "black"))  # 设置坐标轴线条为黑色)
fig4C_data <- readRDS('./07.analysis/02.Rdata/fig4C.RData')
fig4C <- ggplot(data = fig4C_data,aes(x = prozotoa,y =name,fill=lgFC))+
  geom_tile(color = "white", linewidth = 0.1)+
  scale_fill_gradient2(
    low = "#003366",     
    mid = "#FFFFFF",     
    high = "#FFA500",    
    midpoint = 0
  )+
  facet_wrap( ~enriched_5, 
              ncol = 1,      # 调整为3列布局（避免过多行）
              scales = "free_y",  # 自由调整x轴（避免标签重叠）
              strip.position = "right"
  ) +  # 标签在面板顶部
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.4),
        axis.title = element_blank())


