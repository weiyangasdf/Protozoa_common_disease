library(clusterProfiler)
library(dplyr)
library(ggplot2)

fig5A_data <- readRDS('./07.analysis/02.Rdata/fig5A.RData')
fig5A <- ggplot(fig5A_data, aes(y = Var1, x = Freq, fill = Freq)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_gradientn(
    colours = c("#E5F5E0", "#A1D99B",'#6db38c') # 自定义绿色节点
  )    +     # 对应数值范围+
  labs(x='',y='')+
  theme_bw()+
  theme(panel.grid = element_blank())



ko <- data.table::fread('./ko.txt',header = F)
xxx <- enrichKEGG(
  gene = ko$V1,                    # 差异表达基因列表
  organism = "ko",                   # 物种：人类
  keyType = "kegg",                   # ID类型：KEGG ID
  pvalueCutoff = 1,                # p值阈值
  pAdjustMethod = "BH",               # p值校正方法：FDR
  # universe = background_genes,      # 自定义背景基因集（可选）
  # minGSSize = 10,                     # 最小通路基因数
  # maxGSSize = 500,                    # 最大通路基因数
  qvalueCutoff = 1,                 # q值阈值
  use_internal_data = FALSE           # 不使用内部数据
)
result <- xxx@result

result_pro_only <- result[result$qvalue<0.05,]
result_pro_only <- result_pro_only%>%
  arrange(desc(Count))
result_pro_only$Description <- factor(result_pro_only$Description,levels = result_pro_only$Description)
result_pro_only$logq <- -log10(result_pro_only$qvalue)

fig5C <- ggplot(result_pro_only, aes(x = Description, y = Count, fill = logq)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_distiller(
    palette = "YlOrRd",    
    direction = 1,          # 反转颜色方向（可选）
    name = "-log10(P)"
  )+
  labs(x='')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.4),
        panel.grid = element_blank()
  )
ggsave('./fig/pro_only_KO.pdf',plot = pro_only_KO,width = 6,height = 6,dpi = 600)




bac_pro_ko <- data.table::fread('./bac_pro.txt',header = F)
bac_pro_result <- enrichKEGG(
  gene = bac_pro_ko$V1,                    # 差异表达基因列表
  organism = "ko",                   # 物种：人类
  keyType = "kegg",                   # ID类型：KEGG ID
  pvalueCutoff = 1,                # p值阈值
  pAdjustMethod = "BH",               # p值校正方法：FDR
  # universe = background_genes,      # 自定义背景基因集（可选）
  # minGSSize = 10,                     # 最小通路基因数
  # maxGSSize = 500,                    # 最大通路基因数
  qvalueCutoff = 1,                 # q值阈值
  use_internal_data = FALSE           # 不使用内部数据
)
bac_pro_result <- bac_pro_result@result
bac_pro_result_filter <- bac_pro_result[bac_pro_result$qvalue<0.05,]
bac_pro_result_filter <- bac_pro_result_filter%>%
  arrange(desc(Count))
bac_pro_result_filter$Description <- factor(bac_pro_result_filter$Description,levels = bac_pro_result_filter$Description)
bac_pro_result_filter$logq <- -log10(bac_pro_result_filter$qvalue)

fig5D <- ggplot(bac_pro_result_filter, aes(x = Description, y = Count, fill = logq)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_distiller(
    palette = "YlOrRd",    
    direction = 1,          # 反转颜色方向（可选）
    name = "-log10(P)"
  )+
  labs(x='')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.4),
        panel.grid = element_blank()
  )