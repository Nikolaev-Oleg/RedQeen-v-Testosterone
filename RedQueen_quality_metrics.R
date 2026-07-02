library(tidyverse)
library(DescTools)
library(ggpubr)
library(cowplot)

# Validation set quality metrics ####
yolo_RBC_quality <- read.csv('./YOLO_RBC/metrics_out.csv')[-1, -1] %>%
  mutate(tp = as.numeric(tp),
         fp = as.numeric(fp),
         fn = as.numeric(fn),
         conf = as.numeric(conf),
         precision = tp/(tp+fp),
         recall = tp/(tp+fn),
         f1 = 2*precision*recall/(precision+recall))

yolo_par_quality <- read.csv('./YOLO_paras/metrics_out2.csv')[-1, -1] %>%
  mutate(tp = as.numeric(tp),
         fp = as.numeric(fp),
         fn = as.numeric(fn),
         conf = as.numeric(conf),
         precision = tp/(tp+fp),
         recall = tp/(tp+fn),
         f1 = 2*precision*recall/(precision+recall))

AUC(yolo_RBC_quality$recall, yolo_RBC_quality$precision, na.rm = T)
AUC(yolo_par_quality$recall, yolo_par_quality$precision, na.rm = T)

fig1a <- ggplot(yolo_RBC_quality, aes(recall, precision))+
  geom_line(colour = '#cc4411', linewidth = 2)+
  geom_line(data = yolo_par_quality, colour = '#2288cc', linewidth = 2)+
  theme_classic()+
  xlab('Recall')+
  ylab('Precision')+
  ggtitle('a')+
  theme(axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))

max.f1 <- yolo_RBC_quality %>%
  subset(f1 == max(f1, na.rm = T)) %>%
  select(c(conf, f1))

fig1b <- ggplot(yolo_RBC_quality, aes(conf, f1))+
  geom_line(colour = '#cc4411', linewidth = 2)+
  geom_segment(data = max.f1, aes(conf, f1+0.02, yend = f1+0.1),
               arrow = arrow(length = unit(0.02, 'npc'),
                             type = 'closed',
                             ends = 'first'),
               colour = 'gray20',
               linewidth = 1.5)+
  annotate('text', label = max.f1$conf,
           x = max.f1$conf, y = max.f1$f1 + 0.13)+
  theme_classic()+
  xlab('Confidence threshold')+
  ylab('F1')+
  ggtitle('b')+
  theme(axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))

max.f1 <- yolo_par_quality %>%
  subset(f1 == max(f1, na.rm = T)) %>%
  select(c(conf, f1))

fig1c <- ggplot(yolo_par_quality, aes(conf, f1))+
  geom_line(colour = '#2288cc', linewidth = 2)+
  geom_segment(data = max.f1, aes(conf, f1+0.02, yend = f1+0.1),
               arrow = arrow(length = unit(0.02, 'npc'),
                             type = 'closed',
                             ends = 'first'),
               colour = 'gray20',
               linewidth = 1.5)+
  annotate('text', label = max.f1$conf,
           x = max.f1$conf, y = max.f1$f1 + 0.13)+
  theme_classic()+
  xlab('Confidence threshold')+
  ylab('F1')+
  ggtitle('c')+
  theme(axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))

# Compare to manual calculation ####

gs <- read.csv('./data/main_check.csv',
               header = T) %>%
  mutate(sum = RBC + hcRBC + lysRBC + paras,
         check_rbc = case_when(!is.na(check_rbc) ~ RBC + hcRBC + lysRBC + check_rbc,
                               .default = RBC + hcRBC + lysRBC),
         check_par = case_when(!is.na(check_par) ~ paras + check_par,
                               .default = paras),
         prop = paras/(RBC + hcRBC + lysRBC + paras),
         check_prop = check_par/check_sum,
         rbc =  RBC + hcRBC + lysRBC) %>%
  select(c(rbc, paras, check_rbc, check_par, prop, check_prop)) %>%
  na.omit()

equal <- data_frame(rbc = c(0, max(gs$rbc)),
                    check_rbc = c(0, max(gs$rbc)))
fig1d <- ggplot(gs, aes(rbc, check_rbc))+
  geom_point(size = 2.5, 
             stroke = 1,
             fill = '#cc4411',
             shape = 21)+
  geom_line(data = equal, aes(rbc, check_rbc),
            colour = 'gray70',
            linewidth = 1,
            linetype = 2)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Manual RBC count')+
  ylab('YOLO RBC count')+
  ggtitle('d')

equal <- data_frame(paras = c(0, max(gs$paras)),
                    check_par = c(0, max(gs$paras)))
fig1e <- ggplot(gs, aes(paras, check_par))+
  geom_point(size = 2.5, 
             stroke = 1,
             fill = '#2288cc',
             shape = 21)+
  geom_line(data = equal, aes(paras, check_par),
            colour = 'gray70',
            linewidth = 1,
            linetype = 2)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Manual infected RBC count')+
  ylab('YOLO infected RBC count')+
  ggtitle('e')

equal <- data_frame(prop = c(0, max(gs$prop)),
                    check_prop = c(0, max(gs$prop)))
fig1f <- ggplot(gs, aes(prop, check_prop))+
  geom_point(size = 2.5, 
             stroke = 1,
             fill = '#44cc66',
             shape = 21)+
  geom_line(data = equal, aes(prop, check_prop),
            colour = 'gray70',
            linewidth = 1,
            linetype = 2)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Manual parasitic load estimation')+
  ylab('YOLO parasitic load estimation')+
  ggtitle('f')

# Fig. 1 ####
legend.df <- data_frame(x = 1:10,
                        y = 2:11,
                        g = c(rep('a', 5), rep('b', 5)))
legend.plot <- ggplot(legend.df, aes(x, y, fill = g))+
  geom_point(shape = 21, size = 5.5, stroke = 1.2)+
  theme_classic()+
  theme(legend.title = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 12),
        legend.position = 'top')+
  scale_fill_manual(values = c('#cc4411', '#2288cc'),
                    labels = c(expression(YOLO[rbc]),
                               expression(YOLO[infected])),
                    name = '')
legend <- get_plot_component(legend.plot, 'guide-box-top', return_all = TRUE)

fig1a <- plot_grid(legend, fig1a,
                   ncol = 1,
                   rel_heights = c(0.05, 0.95))
fig1bc <- plot_grid(fig1b, fig1c,
                  nrow = 1)
fig1abc <- plot_grid(fig1a, fig1bc,
                     ncol = 1)
fig1def <- plot_grid(fig1d, fig1e, fig1f,
                     nrow = 1)
Fig1 <- plot_grid(fig1abc, fig1def,
                  ncol = 1)
