library(ggplot2)
library(ggpubr)
library(hchinamap)

local <- data.table::fread('clinical_data_local2.txt',sep = '\t',header = T)
p <- hchinamap(name = local$name,value = local$Location_prozotoa_rate,
               maxColor = '#123263',#最小值的颜色 
               width = "100%", #地图的宽度  
               height = "400px",#地图的高度  
               minColor = "#ffdad2",#最大值的颜色 
               region = "China"#要绘制的区域
)
ggsave(filename = './07.analysis/01.fig/local.pdf',plot = p,width = 6,height = 4,dpi = 600)

clinical_data_BMI <- readRDS('clinical_data_BMI.RData')
BMI_new <- ggplot(clinical_data_BMI, aes(x = prozotoa_group, y = BMI,fill=prozotoa_group)) +
  geom_boxplot()+
  labs(x='',y='BMI')+
  theme_bw()+
  scale_fill_manual(values = c('#A9CC98','#fdd835'))+
  theme(panel.grid = element_blank())+
  stat_compare_means(comparisons = list(
  c("non-prozotoa","prozotoa")
  ), 
  method.args = list(exact = F),  # 允许近似计算
  label = "p.signif",
  hide.ns = F,method = 'wilcox.test')+
  theme(axis.text.x = element_text(angle = 30,vjust = 1,hjust = 1),
        legend.position = 'none')
ggsave(filename = './07.analysis/01.fig/BMI_new.pdf',plot = BMI_new,width = 3,height = 4,dpi = 600)
 
clinical_data_BMI <- clinical_data_BMI%>%
  left_join(merge_group[,c(1,4)],by = 'Sample')%>%
  as.data.frame()
clinical_data_BMI$Group_T <- factor(clinical_data_BMI$Group_T,levels = c('CON','Disease'))

# BMI_new_Group <- ggplot(clinical_data_BMI, aes(x = prozotoa_group, y = BMI,fill=Group_T)) +
#   geom_boxplot()+
#   labs(x='',y='BMI')+
#   theme_bw()+
#   scale_fill_manual(values = c('#A9D06B','#E77C8E'))+
#   # theme(panel.grid = element_blank())+
#   # stat_signif(comparisons = list(c('CON','Disease')),
#   #             test = "t.test",
#   #             map_signif_level = TRUE,
#   #             na.rm = TRUE)
#   stat_compare_means(aes(group=Group_T),
#   method.args = list(exact = F),  # 允许近似计算
#   label = "p.signif",na.rm = T,
#   hide.ns = F,method = 'wilcox.test')+
#   theme(axis.text.x = element_text(angle = 30,vjust = 1,hjust = 1)
#         # legend.position = 'none'
#         )
# ggsave(filename = './07.analysis/01.fig/BMI_new_Group.pdf',plot = BMI_new_Group,width = 4,height = 4,dpi = 600)


# clinical_data_age <- clinical_data[!clinical_data$Age%in%c(NA,'Yes'),]
# clinical_data_age$Age <- as.numeric(clinical_data_age$Age)
# clinical_data_age$Age_group <- ifelse(clinical_data_age$Age<15,'1~15',
#                                       ifelse(clinical_data_age$Age>16&clinical_data_age$Age<59,'16~59','>=60'))
# clinical_data_age <- clinical_data_age%>%
#   group_by(Age_group,Project1)%>%
#   mutate(Age_Group_count=n())
# clinical_data_age <- clinical_data_age%>%
#   group_by(prozotoa_group,Age_group,Project1)%>%
#   mutate(prozotoa_count=n())%>%
#   as.data.frame()
# clinical_data_age$prozotoa_rate <- clinical_data_age$prozotoa_count/clinical_data_age$Age_Group_count
# clinical_data_age2 <- clinical_data_age[clinical_data_age$prozotoa_group=='prozotoa',11:16]
# clinical_data_age2 <- clinical_data_age2[!duplicated(clinical_data_age2),]
# clinical_data_age2$Age_group <- factor(clinical_data_age2$Age_group,levels = c('1~15','16~59','>=60'))
clinical_data_age2 <- readRDS('clinical_data_age2.RData')
age_plot <- ggplot(clinical_data_age2, aes(x = Age_group, y = prozotoa_rate,fill=Age_group)) +
  geom_boxplot()+
  labs(x='',y='prozotoa prevalence')+
  theme_bw()+
  scale_fill_manual(values = c('#fff59d','#ffee58','#fdd835'))+
  theme(panel.grid = element_blank())+
  stat_compare_means(comparisons = list(
    # c('1~15','16~59'),
    # c('1~15','>=60'),
    # c('16~59','>=60')
  ), 
  method.args = list(exact = F),  # 允许近似计算
  label = "p.signif",
  hide.ns = F,method = 'wilcox.test')+
  theme(axis.text.x = element_text(angle = 30,vjust = 1,hjust = 1),
        legend.position = 'none')
age_plot
ggsave(filename = './07.analysis/01.fig/age_plot.pdf',plot = age_plot,width = 3.5,height = 4,dpi = 600)




# clinical_data_gender <- clinical_data[!clinical_data$Age%in%NA,]
# 
# clinical_data_gender$Gender <- ifelse(clinical_data_gender$Gender%in%c("Female",'female'),'Female','Male')
# clinical_data_gender <- clinical_data_gender%>%
#   group_by(Gender,Project1)%>%
#   mutate(Gender_count=n())
# clinical_data_gender <- clinical_data_gender%>%
#   group_by(prozotoa_group,Gender,Project1)%>%
#   mutate(prozotoa_count=n())%>%
#   as.data.frame()
# clinical_data_gender$prozotoa_rate <- clinical_data_gender$prozotoa_count/clinical_data_gender$Gender_count
# clinical_data_gender2 <- clinical_data_gender[clinical_data_gender$prozotoa_group=='prozotoa',c(7,11:15)]
# clinical_data_gender2 <- clinical_data_gender2[!duplicated(clinical_data_gender2),]
# clinical_data_age2$Age_group <- factor(clinical_data_age2$Age_group,levels = c('1~15','16~59','>=60'))
clinical_data_gender2 <- readRDS('clinical_data_gender2.RData')
gender_plot <- ggplot(clinical_data_gender2, aes(x = Gender, y = prozotoa_rate,fill=Gender)) +
  geom_boxplot()+
  labs(x='',y='prozotoa prevalence')+
  theme_bw()+
  scale_fill_manual(values = c('#A9D06B','#E77C8E'))+
  theme(panel.grid = element_blank())+
  stat_compare_means(comparisons = list(
    # c('Female','Male')
  ), 
  method.args = list(exact = F),  # 允许近似计算
  label = "p.signif",
  hide.ns = F,method = 'wilcox.test')+
  theme(axis.text.x = element_text(angle = 30,vjust = 1,hjust = 1),
        legend.position = 'none')
gender_plot
ggsave(filename = './07.analysis/01.fig/gender_plot.pdf',plot = gender_plot,width = 3,height = 4,dpi = 600)

