merge_bac_normalized <- readRDS('merge_bac_normalized.RData')
fig3A <- zy_alpha(dt = merge_bac_normalized,title = '',
                         sample_map = df_prozotoa1,
                         group = 'prozotoa_group',
                         ID = 'Sample',
                         index = 'shannon',
                         sample.color = c('#6db38c','#f0a52c'))

fig3B <- zy_alpha(dt = merge_bac_normalized,title = '',
                          sample_map = df_prozotoa1,
                          group = 'prozotoa_group',
                          ID = 'Sample',
                          index = 'obs',
                          sample.color = c('#6db38c','#f0a52c'))

fig3C <- zy_pcoa(
  dt = merge_bac_normalized,sample.color = c('#779DE9','#F8C1B7'),
  sample_map = df_prozotoa1,
  group = "prozotoa_group",
  ID = "Sample"
)
obs_mean <- readRDS('obs_mean.RData')
fig3D_1 <- ggplot(obs_mean, aes(x = log2fc, y = Project)) +
  geom_col(width = 0.8,aes(fill=Disease_T,color=Disease_T)) +  # 柱形宽度和颜色
  scale_fill_manual(values = rev( c('#6fb3a8','#a2d2e7','#50aa4b', '#b3e19b','#ffc17f','#cf9f88','#dba9a8',
                                    '#e43030','#f36569','#704ba3','#cdb6da'
                                    
  )))+
  scale_color_manual(values = rev( c('#6fb3a8','#a2d2e7','#50aa4b', '#b3e19b','#ffc17f','#cf9f88','#dba9a8',
                                     '#e43030','#f36569','#704ba3','#cdb6da'
                                     
  )))+
  scale_x_continuous(
    limits = c(-0.4, 0.4),  # 设置横坐标范围（确保包含所有正负值）
    breaks = c(-0.4, 0, 0.4) # 设置横坐标断
  ) +
  geom_text(
    aes(y = Project, x = 0.35, label = label),
    size = 3,color='red'
  )+
  labs(x = "Coefficient(#Species)", y = "") +
  theme_bw() +
  theme(axis.title.y = element_blank())+  # 隐藏 y 轴标题（若不需要）
  theme(panel.grid = element_blank())

shannon_mean <- readRDS('shannon_mean.RData')
fig3D_2 <- ggplot(shannon_mean, aes(x = log2fc, y = Project)) +
  geom_col(width = 0.8,aes(fill=Disease_T,color=Disease_T)) +  # 柱形宽度和颜色
  scale_fill_manual(values = rev( c('#6fb3a8','#a2d2e7','#50aa4b', '#b3e19b','#ffc17f','#cf9f88','#dba9a8',
                                    '#e43030','#f36569','#704ba3','#cdb6da'
                                    
  )))+
  scale_color_manual(values = rev( c('#6fb3a8','#a2d2e7','#50aa4b', '#b3e19b','#ffc17f','#cf9f88','#dba9a8',
                                     '#e43030','#f36569','#704ba3','#cdb6da'
                                     
  )))+
  scale_x_continuous(
    limits = c(-0.35, 0.35),  # 设置横坐标范围（确保包含所有正负值）
    breaks = c(-0.35, 0, 0.35) # 设置横坐标断
  ) +
  geom_text(
    aes(y = Project, x = 0.3, label = label),
    size = 3,color='red'
  )+
  labs(x = "Coefficient(Shannon index)", y = "") +
  theme_bw() +
  theme(axis.title.y = element_blank())+  # 隐藏 y 轴标题（若不需要）
  theme(panel.grid = element_blank())


ado_result <- readRDS('ado_result.RData')
fig3D_3 <- ggplot(ado_result, aes(x = bac_adjust.R2, y = Project)) +
  geom_col(width = 0.8,aes(fill=Disease_T,color=Disease_T)) +  # 柱形宽度和颜色
  scale_fill_manual(values = rev( c('#6fb3a8','#a2d2e7','#50aa4b', '#b3e19b','#ffc17f','#cf9f88','#dba9a8',
                                    '#e43030','#f36569','#704ba3','#cdb6da'
                                    
  )))+
  scale_color_manual(values = rev( c('#6fb3a8','#a2d2e7','#50aa4b', '#b3e19b','#ffc17f','#cf9f88','#dba9a8',
                                     '#e43030','#f36569','#704ba3','#cdb6da'
                                     
  )))+
  scale_x_continuous(
    limits = c(-0.002, 0.09) # 设置横坐标范围（确保包含所有正负值）
    # breaks = c(-0.002, 0, 0.09) # 设置横坐标断
  ) +
  geom_text(
    aes(y = Project, x = 0.075, label = bac_label),
    size = 3,color='red'
  )+
  labs(x = "Effect size(R2_adj)", y = "") +
  theme_bw() +
  theme(axis.title.y = element_blank())+  # 隐藏 y 轴标题（若不需要）
  theme(panel.grid = element_blank())

fig3D_4 <- ggplot(ado_result, aes(x = bac_prozotoa_adjust.R2, y = Project)) +
  geom_col(width = 0.8,aes(fill=Disease_T,color=Disease_T)) +  # 柱形宽度和颜色
  scale_fill_manual(values = rev( c('#6fb3a8','#a2d2e7','#50aa4b', '#b3e19b','#ffc17f','#cf9f88','#dba9a8',
                                    '#e43030','#f36569','#704ba3','#cdb6da'
                                    
  )))+
  scale_color_manual(values = rev( c('#6fb3a8','#a2d2e7','#50aa4b', '#b3e19b','#ffc17f','#cf9f88','#dba9a8',
                                     '#e43030','#f36569','#704ba3','#cdb6da'
                                     
  )))+
  scale_x_continuous(
    limits = c(-0.003, 0.1) # 设置横坐标范围（确保包含所有正负值）
    # breaks = c(-0.002, 0, 0.09) # 设置横坐标断
  ) +
  geom_text(
    aes(y = Project, x = 0.075, label = bac_prozotoa_label),
    size = 3,color='red'
  )+
  labs(x = "Effect size(R2_adj)", y = "") +
  theme_bw() +
  theme(axis.title.y = element_blank())+  # 隐藏 y 轴标题（若不需要）
  theme(panel.grid = element_blank())

