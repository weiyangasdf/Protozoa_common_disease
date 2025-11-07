group_tb <- readRDS('group_tb.RData')
fig2A_1 <- ggplot(group_tb, aes(y = Project, x = Disease_type, fill = Group_T)) +
  geom_col(width = 0.7) +        # 条形宽度和边框颜色
  scale_fill_manual(values = c('#A9D06B','#E77C8E')) +          # 使用颜色方案
  # scale_y_continuous(limits = c(0, 220),
  #                    breaks = seq(0, 220, by = 50))+
  labs(
    # title = "堆积条形图示例",
    y = "",
    x = "Number of samples"
  ) +
  theme_bw()+
  theme(
    # axis.text.x = element_text(angle = 90,vjust=1,hjust = 1),
    panel.grid = element_blank(),
    # panel.border = element_blank(),  # 移除外边框
    axis.line = element_line(colour = "black"),
    legend.position = 'none',
    panel.border = element_rect(
      color = "black",      # 边框颜色
      size = 0.5,           # 边框粗细
      linetype = "solid" # 边框线型（可选：solid, dashed, dotted 等）
    )
  )+
  scale_x_continuous( expand = expansion(mult = c(0, 0.05)),
                      limits = c(0, 400),
                      breaks = seq(0, 400, by = 100)
  )
df_prozotoa_present <- readRDS('df_prozotoa_present.RData')
fig2A_2 <- ggplot(df_prozotoa_present, aes(y = Project, x = present_rate)) +
  geom_col(width = 0.7,color='#a67eb7',fill='#a67eb7') +        # 条形宽度和边框颜色
  # scale_fill_discrete(values = c('#5AA684')) +          # 使用颜色方案
  # scale_color_discrete(values = c('#5AA684'))+
  # scale_y_continuous(limits = c(0, 220),
  #                    breaks = seq(0, 220, by = 50))+
  labs(
    # title = "堆积条形图示例",
    y = "",
    x = "Occurrence rate"
  ) +
  theme_bw()+
  theme(
    # axis.text.x = element_text(angle = 90,vjust=1,hjust = 1),
    panel.grid = element_blank(),
    # panel.border = element_blank(),  # 移除外边框
    axis.line = element_line(colour = "black"),
    legend.position = 'none',
    panel.border = element_rect(
      color = "black",      # 边框颜色
      size = 0.5,           # 边框粗细
      linetype = "solid" # 边框线型（可选：solid, dashed, dotted 等）
    )
  )+
  scale_x_continuous( expand = expansion(mult = c(0, 0.05)),
                      limits = c(0, 1),
                      breaks = seq(0, 1, by = 0.2)
  )

prozotoa_present_D <- readRDS('prozotoa_present_D.RData')
fig2A_3 <- ggplot(prozotoa_present_D, aes(y = Project, x = present_rate)) +
  geom_col(width = 0.7,color='#E77C8E',fill='#E77C8E') +        # 条形宽度和边框颜色
  # scale_fill_discrete(values = c('#5AA684')) +          # 使用颜色方案
  # scale_color_discrete(values = c('#5AA684'))+
  # scale_y_continuous(limits = c(0, 220),
  #                    breaks = seq(0, 220, by = 50))+
  labs(
    # title = "堆积条形图示例",
    y = "",
    x = "Occurrence rate"
  ) +
  theme_bw()+
  theme(
    # axis.text.x = element_text(angle = 90,vjust=1,hjust = 1),
    panel.grid = element_blank(),
    # panel.border = element_blank(),  # 移除外边框
    axis.line = element_line(colour = "black"),
    legend.position = 'none',
    panel.border = element_rect(
      color = "black",      # 边框颜色
      size = 0.5,           # 边框粗细
      linetype = "solid" # 边框线型（可选：solid, dashed, dotted 等）
    )
  )+
  scale_x_continuous( expand = expansion(mult = c(0, 0.05)),
                      limits = c(0, 1),
                      breaks = seq(0, 1, by = 0.2)
  )
prozotoa_present_C <- readRDS('prozotoa_present_C.RData')
fig2A_4 <- ggplot(prozotoa_present_C, aes(y = Project, x = present_rate)) +
  geom_col(width = 0.7,color='#A9D06B',fill='#A9D06B') +        # 条形宽度和边框颜色
  # scale_fill_discrete(values = c('#5AA684')) +          # 使用颜色方案
  # scale_color_discrete(values = c('#5AA684'))+
  # scale_y_continuous(limits = c(0, 220),
  #                    breaks = seq(0, 220, by = 50))+
  labs(
    # title = "堆积条形图示例",
    y = "",
    x = "Occurrence rate"
  ) +
  theme_bw()+
  theme(
    # axis.text.x = element_text(angle = 90,vjust=1,hjust = 1),
    panel.grid = element_blank(),
    # panel.border = element_blank(),  # 移除外边框
    axis.line = element_line(colour = "black"),
    legend.position = 'none',
    panel.border = element_rect(
      color = "black",      # 边框颜色
      size = 0.5,           # 边框粗细
      linetype = "solid" # 边框线型（可选：solid, dashed, dotted 等）
    )
  )+
  scale_x_continuous( expand = expansion(mult = c(0, 0.05)),
                      limits = c(0, 1),
                      breaks = seq(0, 1, by = 0.2)
  )
fig2B_data <- readRDS('fig2B_data.RData')
fig2B <- ggplot(fig2B_data, aes(x = value, y = Virus, fill = name)) +
  scale_fill_manual(values =  c('#A9D06B','#E77C8E'))+
  geom_bar(stat = "identity", position = "dodge")+
  theme_bw()+
  labs(x='',y='')+
  # scale_x_continuous( expand = expansion(mult = c(0, 0)),limits = c(0,1.1))+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90,hjust = 0.5,vjust = 0.5),
        legend.position = 'none'
  )
pro_pre_eve_proj_T <- readRDS('pro_pre_eve_proj_T.RData')
text_label <- readRDS('text_label.RData')
fig2C <- ggplot()+
  geom_tile(data = pro_pre_eve_proj_T,aes(y = Species,x = Project1,fill=log10),color = "white", linewidth = 0.1)+
  geom_text(data = text_label,aes(y = Virus,x = Project,label = sig),size =2)+
  scale_fill_gradient2(
    low = "#003366",     
    mid = "#FFFFFF",     
    high = "#FFA500",    
    midpoint = 0
  )+
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.4),
        axis.title = element_blank())

