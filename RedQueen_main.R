# Packages ####
library(tidyverse)
library(dunn.test)
library(ggpubr)
library(cowplot)
library(ARTool) 
library(emmeans)
library(glue)
library(ggeasy)

# Prepare data ####

df.full <- read.csv('./data/main_check.csv',
                   header = T) %>%
  mutate(horm = as.numeric(horm),
         date = date %>%
           as.character(),
         date = case_when(nchar(date) == 8 ~ date,
                          nchar(date) == 7 ~ glue('0{date}')),
         timepoint = glue('{str_sub(date, 3, 4)}{str_sub(date, 7, 8)}'),
         group = glue('{sex}_{sp}') %>% 
           as_factor(),
         check_rbc = case_when(!is.na(check_rbc) ~ check_rbc,
                               .default = 0),
         check_par = case_when(!is.na(check_par) ~ check_par,
                               .default = 0),
         prop = (paras+check_par)/check_sum,
         timepoint = as.factor(timepoint)) %>%
  subset(timepoint != '0822') %>% # only two points
  subset(sex == 'm' | sex == 'f') # remove observations with unidentified sex
df.horm_v_prop <- subset(df.full, !is.na(horm) & !is.na(prop))
df.horm <- subset(df.full, !is.na(horm))
df.prop <- subset(df.full, !is.na(prop) & check_sum >= 1000)

ac.prop<-subset(df.prop, loc == 'amrakits canyon')
gg.prop<-subset(df.prop, loc == 'gar-gar')
sr.prop<-subset(df.prop, loc == 'sepasar river')
sh.prop<-subset(df.prop, loc == 'sepasar hill')
hr.prop<-subset(df.prop, loc == 'hrazdan restaurant')

ac.horm<-subset(df.horm, loc == 'amrakits canyon')
gg.horm<-subset(df.horm, loc == 'gar-gar')
sr.horm<-subset(df.horm, loc == 'sepasar river')
sh.horm<-subset(df.horm, loc == 'sepasar hill')
hr.horm<-subset(df.horm, loc == 'hrazdan restaurant')

ac.horm_v_prop<-subset(df.horm_v_prop, loc == 'amrakits canyon')
gg.horm_v_prop<-subset(df.horm_v_prop, loc == 'gar-gar')
sr.horm_v_prop<-subset(df.horm_v_prop, loc == 'sepasar river')
sh.horm_v_prop<-subset(df.horm_v_prop, loc == 'sepasar hill')
hr.horm_v_prop<-subset(df.horm_v_prop, loc == 'hrazdan restaurant')
# AC ...................................................................... ####
# Hormones through time points ####
with(subset(ac.horm, group =='f_D.armeniaca'), dunn.test(horm, timepoint, method = 'holm'))
with(subset(ac.horm, group =='f_D.dahli'), dunn.test(horm, timepoint, method = 'holm'))
with(subset(ac.horm, group =='f_D.portschinskii'), dunn.test(horm, timepoint, method = 'holm'))
with(subset(ac.horm, group =='m_D.portschinskii'), dunn.test(horm, timepoint, method = 'holm'))

# Hormones between species ####
art<-art(horm~group+Error(timepoint),
         data = subset(ac.horm, sp !='DportXDdah' & sex != 'm') %>% # too few observations on hybrids
           mutate(group = as.factor(group),
                  timepoint = as.factor(timepoint)))
anova(art)
art.con(art, 'group', adjust = 'tukey')

Fig2e<-ggplot(subset(ac.horm, sp !='DportXDdah' & sex == 'f'), aes(timepoint, horm))+
  geom_boxplot(aes(colour = sp), linewidth = 1,
               outliers = F)+
  geom_jitter(aes(fill = sp), shape = 21, size = 2.5, position = position_jitterdodge(jitter.width = 0.1), stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Time of collection')+
  ylab('Progesterone concentration, nM')+
  ggtitle('e')+
  scale_x_discrete(labels = c('May 2023', 'June 2023', 'September 2023'))+
  scale_colour_manual(values = c('#eebb22', '#2288cc', '#cc4411'))+
  scale_fill_manual(values = c('#eebb22', '#2288cc', '#cc4411'))

Fig2f<-ggplot(subset(ac.horm, sp !='DportXDdah' & sex == 'm'), aes(timepoint, horm))+
  geom_boxplot(linewidth = 1,
               outliers = F,
               colour='#44cc66')+
  geom_jitter(shape = 21, size = 2.5, stroke = 1, fill = '#44cc66', width = 0.1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Time of collection')+
  ylab('Testosterone concentration, nM')+
  ggtitle('f')+
  scale_x_discrete(labels = c('May 2023', 'June 2023', 'September 2023'))

# Parasitic load between species ####
art<-art(prop~group+Error(timepoint),
    data = subset(ac.prop, sp !='DportXDdah') %>%
      mutate(group = as.factor(group),
             timepoint = as.factor(timepoint)))
anova(art)
art.con(art, 'group', adjust = 'tukey')

Fig2a<-ggplot(subset(ac.prop, sp !='DportXDdah'), aes(timepoint, prop))+
  geom_boxplot(aes(colour = group), linewidth = 1,
               outliers = F)+
  geom_jitter(aes(fill = group), shape = 21, size = 2.5, position = position_jitterdodge(jitter.width = 0.1), stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Time of collection')+
  ylab('Parasitic load')+
  ggtitle('a')+
  scale_x_discrete(labels = c('April', 'May', 'June', 'September'))+
  scale_colour_manual(values = c('#eebb22', '#2288cc', '#cc4411', '#44cc66'))+
  scale_fill_manual(values = c('#eebb22', '#2288cc', '#cc4411', '#44cc66'))

# Parasitic load through time points ####
#| No difference between time points were observed for any species
with(subset(ac.prop, group =='f_D.armeniaca'), dunn.test(prop, timepoint))
with(subset(ac.prop, group =='f_D.dahli'), dunn.test(prop, timepoint))
with(subset(ac.prop, group =='f_D.portschinskii'), dunn.test(prop, timepoint))
with(subset(ac.prop, group =='m_D.portschinskii'), dunn.test(prop, timepoint))

# Parasitic load v sex steroids ####
#|No significant correlation was observed between sex steroids concentration
#|and parasitic load in either species

with(subset(ac.prop, group =='f_D.armeniaca'), cor.test(prop,horm, method = 'spearman')) # R = .28; p =.13
with(subset(ac.prop, group =='f_D.armeniaca' & timepoint == '0523'), cor.test(prop,horm, method = 'spearman')) # R = .02; p =.98
with(subset(ac.prop, group =='f_D.armeniaca' & timepoint == '0623'), cor.test(prop,horm, method = 'spearman')) # R = .47; p =.11
with(subset(ac.prop, group =='f_D.armeniaca' & timepoint == '0923'), cor.test(prop,horm, method = 'spearman')) # R = .25; p =.52

with(subset(ac.prop, group =='f_D.dahli'), cor.test(prop,horm, method = 'spearman')) # R = .002; p =.99
with(subset(ac.prop, group =='f_D.dahli' & timepoint == '0523'), cor.test(prop,horm, method = 'spearman')) # R = -.61; p =.02
with(subset(ac.prop, group =='f_D.dahli' & timepoint == '0623'), cor.test(prop,horm, method = 'spearman')) # R = .18; p =.53
with(subset(ac.prop, group =='f_D.dahli' & timepoint == '0923'), cor.test(prop,horm, method = 'spearman')) # R = -.38; p =.36

with(subset(ac.prop, group =='f_D.portschinskii'), cor.test(prop,horm, method = 'spearman')) # R = -.14, p = .53
with(subset(ac.prop, group =='f_D.portschinskii' & timepoint == '0523'), cor.test(prop,horm, method = 'spearman')) # R = 1; p =.08
with(subset(ac.prop, group =='f_D.portschinskii' & timepoint == '0623'), cor.test(prop,horm, method = 'spearman')) # R = -.57; p =.07
with(subset(ac.prop, group =='f_D.portschinskii' & timepoint == '0923'), cor.test(prop,horm, method = 'spearman')) # R = -.40; p =.33

with(subset(ac.prop, group =='m_D.portschinskii'), cor.test(prop,horm, method = 'spearman')) # R = -.17, p = .94
with(subset(ac.prop, group =='m_D.portschinskii' & timepoint == '0523'), cor.test(prop,horm, method = 'spearman')) # R = -.60; p =.35
with(subset(ac.prop, group =='m_D.portschinskii' & timepoint == '0623'), cor.test(prop,horm, method = 'spearman')) # R = .57; p =.20
with(subset(ac.prop, group =='m_D.portschinskii' & timepoint == '0923'), cor.test(prop,horm, method = 'spearman')) # R = -.27; p =.44

Fig2c<-ggplot(subset(ac.horm_v_prop, sp !='DportXDdah' & group !='m_D.portschinskii'), aes(horm, prop))+
  geom_smooth(aes(colour = group),
              linewidth = 1,
              se = F,
              method = 'lm')+
  geom_point(aes(fill = group),
             size = 2.5,
             stroke = 1,
             shape = 21)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Progesterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('c')+
  scale_colour_manual(values = c('#eebb22', '#2288cc', '#cc4411'))+
  scale_fill_manual(values = c('#eebb22', '#2288cc', '#cc4411'))

Fig2d<-ggplot(subset(ac.horm_v_prop, group =='m_D.portschinskii'), aes(horm, prop))+
  geom_smooth(colour = '#44cc66',
              linewidth = 1,
              se = F,
              method = 'lm')+
  geom_point(fill = '#44cc66',
             size = 2.5,
             stroke = 1,
             shape = 21)+
  theme_classic()+
  theme(legend.position = 'none',
        legend.title = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9),
        legend.key.width = unit(2, 'cm'))+
  xlab('Testosterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('d')

# Body condition v parasitic load ####
#| SVL ~ m is close to linearity, so we can use proportion as a measure of
#| the body condition.
#| No significant effect of parasitic load on body condition was observed

ggplot(subset(ac.prop, sp !='DportXDdah'), aes(svl, m))+
  geom_smooth(aes(colour = group), linewidth = 1, se = F, method = 'lm')+
  geom_point(aes(fill = group), shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('SVL, mm')+
  ylab('m, g')+
  ggtitle('b')+
  scale_colour_manual(values = c('#eebb22', '#2288cc', '#cc4411', '#44cc66'))+
  scale_fill_manual(values = c('#eebb22', '#2288cc', '#cc4411', '#44cc66'))

ac.prop$bc<-ac.prop$svl/ac.prop$m
with(subset(ac.prop, group =='f_D.armeniaca'), cor.test(bc, prop, method = 'spearman'))
with(subset(ac.prop, group =='f_D.dahli'), cor.test(bc, prop, method = 'spearman'))
with(subset(ac.prop, group =='f_D.portschinskii'), cor.test(bc, prop, method = 'spearman'))
with(subset(ac.prop, group =='m_D.portschinskii'), cor.test(bc, prop, method = 'spearman'))

Fig2b<-ggplot(subset(ac.prop, sp !='DportXDdah'), aes(bc, prop))+
  geom_smooth(aes(colour = group), linewidth = 1, se = F, method = 'lm')+
  geom_point(aes(fill = group), shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Body condition, mm/g')+
  ylab('Parasitic load')+
  ggtitle('b')+
  scale_colour_manual(values = c('#eebb22', '#2288cc', '#cc4411', '#44cc66'))+
  scale_fill_manual(values = c('#eebb22', '#2288cc', '#cc4411', '#44cc66'))

# Infection prevalence between species ####
ac.arm.prev <- ac.prop %>%
  subset(group == 'f_D.armeniaca') %>%
  mutate(infected = case_when(prop == 0 ~ 'healthy',
                              prop > 0 ~ 'infected')) %>%
  group_by(infected) %>%
  summarise(darm = n(), .groups = 'keep')

ac.dah.prev <- ac.prop %>%
  subset(group == 'f_D.dahli') %>%
  mutate(infected = case_when(prop == 0 ~ 'healthy',
                              prop > 0 ~ 'infected')) %>%
  group_by(infected) %>%
  summarise(ddah = n(), .groups = 'keep')

ac.fpor.prev <- ac.prop %>%
  subset(group == 'f_D.portschinskii') %>%
  mutate(infected = case_when(prop == 0 ~ 'healthy',
                              prop > 0 ~ 'infected')) %>%
  group_by(infected) %>%
  summarise(fpor = n(), .groups = 'keep')

ac.mpor.prev <- ac.prop %>%
  subset(group == 'm_D.portschinskii') %>%
  mutate(infected = case_when(prop == 0 ~ 'healthy',
                              prop > 0 ~ 'infected')) %>%
  group_by(infected) %>%
  summarise(mpor = n(), .groups = 'keep')

fisher.p <- c()
x <- full_join(ac.arm.prev, ac.dah.prev) %>%
  column_to_rownames('infected') %>%
  as.matrix()
fisher.p <- append(fisher.p, fisher.test(x)$p.value)

x <- full_join(ac.arm.prev, ac.fpor.prev) %>%
  column_to_rownames('infected') %>%
  as.matrix()
fisher.p <- append(fisher.p, fisher.test(x)$p.value)
  
x <- full_join(ac.arm.prev, ac.mpor.prev) %>%
  column_to_rownames('infected') %>%
  as.matrix()
fisher.p <- append(fisher.p, fisher.test(x)$p.value)

x <- full_join(ac.dah.prev, ac.fpor.prev) %>%
  column_to_rownames('infected') %>%
  as.matrix()
fisher.p <- append(fisher.p, fisher.test(x)$p.value)

x <- full_join(ac.dah.prev, ac.mpor.prev) %>%
  column_to_rownames('infected') %>%
  as.matrix()
fisher.p <- append(fisher.p, fisher.test(x)$p.value)

x <- full_join(ac.fpor.prev, ac.mpor.prev) %>%
  column_to_rownames('infected') %>%
  as.matrix()
fisher.p <- append(fisher.p, fisher.test(x)$p.value)

names(fisher.p) <- c('Darm v Ddah', 'Darm v Dport_f', 'Darm v Dport_m', 
                     'Ddah v Dport_f', 'Ddah v Dport_m',
                     'Dport_f v Dport_m')
fisher.p <- p.adjust(fisher.p, method = 'holm')
fisher.p

Fig2g1 <- ggplot(ac.arm.prev, aes(infected, darm/sum(darm)))+
  geom_col(fill = '#eebb22',
           colour = 'gray20')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = 'bold'))+
  ylim(c(0, 1))+
  ylab('Prevalence')+
  scale_x_discrete(labels = c('Healthy', 'Infected'))
Fig2g2 <- ggplot(ac.dah.prev, aes(infected, ddah/sum(ddah)))+
  geom_col(fill = '#2288cc',
           colour = 'gray20')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title.x = element_blank())+
  ylim(c(0, 1))+
  easy_remove_y_axis()+
  scale_x_discrete(labels = c('Healthy', 'Infected'))
Fig2g3 <- ggplot(ac.fpor.prev, aes(infected, fpor/sum(fpor)))+
  geom_col(fill = '#cc4411',
           colour = 'gray20')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title.x = element_blank())+
  ylim(c(0, 1))+
  easy_remove_y_axis()+
  scale_x_discrete(labels = c('Healthy', 'Infected'))
Fig2g4 <- ggplot(ac.mpor.prev, aes(infected, mpor/sum(mpor)))+
  geom_col(fill = '#44cc66',
           colour = 'gray20')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text = element_text(size = 12, face = 'bold'),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16, face = 'bold', hjust = .8))+
  ggtitle('g')+
  ylim(c(0, 1))+
  easy_remove_y_axis()+
  scale_x_discrete(labels = c('Healthy', 'Infected'))

Fig2g <- plot_grid(Fig2g1, Fig2g2, Fig2g3, Fig2g4,
                   nrow = 1,
                   align = 'hv',
                   axis = 'tb')
# Fig2####
legend.plot<-ggplot(subset(ac.prop, sp !='DportXDdah'), aes(svl, m))+
  geom_point(aes(fill = group), shape = 21, size = 5.5, stroke = 1.2)+
  theme_classic()+
  theme(legend.title = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 12),
        legend.position = 'top',
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing = unit(0, "cm"),
        legend.box.spacing = unit(0, "cm"))+
  scale_fill_manual(values = c('#eebb22', '#2288cc', '#cc4411', '#44cc66'),
                    labels = c(expression(italic('D. armeniaca')),
                               expression(italic('D. dahli')),
                               expression(italic('D. portschinskii') ~bold('♀ ')),
                               expression(italic('D. portschinskii') ~bold('♂'))
                               ),
                    name = 'Cohort')

leg2<-get_plot_component(legend.plot, 'guide-box-top', return_all = TRUE)

Fig2<-ggarrange(Fig2a, Fig2b, Fig2c, Fig2d, Fig2e, Fig2f,
          ncol = 2,
          nrow = 3,
          align = 'hv')
Fig2 <- plot_grid(Fig2, Fig2g,
                  ncol = 1,
                  rel_heights = c(0.8, 0.2))

plot_grid(Fig2, leg2,
          ncol = 1,
          rel_heights = c(0.95, 0.05))

# HR....................................................................... ####
# Hormones through time points ####
with(subset(hr.horm, group =='f_D.armeniaca'), wilcox.test(horm ~ timepoint))
with(subset(hr.horm, group =='f_D.unisexualis'), wilcox.test(horm ~ timepoint))
with(subset(hr.horm, group =='f_D.nairensis'), wilcox.test(horm ~ timepoint))
with(subset(hr.horm, group =='m_D.nairensis'), wilcox.test(horm ~ timepoint))

# Hormones between species ####
art<-art(horm~group+Error(timepoint),
         data = subset(hr.horm, sex == 'f') %>%
           mutate(group = as.factor(group),
                  timepoint = as.factor(timepoint)))
anova(art)
art.con(art, 'group', adjust = 'tukey')


Fig3e<-ggplot(subset(hr.horm, sp !='DportXDdah' & sex == 'f'), aes(timepoint, horm))+
  geom_boxplot(aes(colour = factor(sp, levels = c('D.armeniaca', 'D.unisexualis', 'D.nairensis'))), linewidth = 1,
               outliers = F)+
  geom_jitter(aes(fill = factor(sp, levels = c('D.armeniaca', 'D.unisexualis', 'D.nairensis'))), shape = 21, size = 2.5, position = position_jitterdodge(jitter.width = 0.1), stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Time of collection')+
  ylab('Progesterone concentration, nM')+
  ggtitle('e')+
  scale_x_discrete(labels = c('June 2023', 'September 2023'))+
  scale_colour_manual(values = c('#eebb22', '#2288cc', '#cc4411'))+
  scale_fill_manual(values = c('#eebb22', '#2288cc', '#cc4411'))

Fig3f<-ggplot(subset(hr.horm, sex == 'm'), aes(timepoint, horm))+
  geom_boxplot(linewidth = 1,
               outliers = F,
               colour='#44cc66')+
  geom_jitter(shape = 21, size = 2.5, stroke = 1, fill = '#44cc66', width = 0.1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Time of collection')+
  ylab('Testosterone concentration, nM')+
  ggtitle('f')+
  scale_x_discrete(labels = c('June 2023', 'September 2023'))


# Parasitic load between species ####
#| Two-way ANOVA was applied as a robust method
#| parthenogenetic species were the most parasitized cohorts
#| followed by male D. portschinskii
#| However, KW-test did not show any sinificant differences
#| when processing each timepoint separately (probably due to low power)

art<-art(prop~group+Error(timepoint),
         data = hr.prop %>%
           mutate(group = as.factor(group),
                  timepoint = as.factor(timepoint)))
anova(art)
art.con(art, 'group', adjust = 'tukey')

Fig3a<-ggplot(hr.prop, aes(timepoint, prop))+
  geom_boxplot(aes(colour = factor(group, levels = c("f_D.armeniaca", "f_D.unisexualis", "f_D.nairensis", "m_D.nairensis"))),
               linewidth = 1,
               outliers = F)+
  geom_jitter(aes(fill = factor(group, levels = c("f_D.armeniaca", "f_D.unisexualis", "f_D.nairensis", "m_D.nairensis"))), 
              shape = 21,
              size = 2.5, 
              position = position_jitterdodge(jitter.width = 0.1), stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Time of collection')+
  ylab('Parasitic load')+
  ggtitle('a')+
  scale_x_discrete(labels = c('May 2023', 'June 2023', 'September 2023'))+
  scale_colour_manual(values = c('#eebb22', '#2288cc', '#cc4411', '#44cc66'))+
  scale_fill_manual(values = c('#eebb22', '#2288cc', '#cc4411', '#44cc66'))

# Parasitic load through time points ####
#| No difference between time points were observed for any species

with(subset(hr.prop, group =='f_D.armeniaca'), dunn.test(prop, timepoint))
with(subset(hr.prop, group =='f_D.unisexualis'), dunn.test(prop, timepoint))
with(subset(hr.prop, group =='f_D.nairensis'), dunn.test(prop, timepoint))
with(subset(hr.prop, group =='m_D.nairensis'), dunn.test(prop, timepoint))
# Parasitic load v sex steroids ####

with(subset(hr.prop, group =='f_D.armeniaca'), cor.test(prop,horm, method = 'spearman')) # R = .36; p =.04
with(subset(hr.prop, group =='f_D.armeniaca' & timepoint == '0623'), cor.test(prop,horm, method = 'spearman')) # R = .08; p =.75
with(subset(hr.prop, group =='f_D.armeniaca' & timepoint == '0923'), cor.test(prop,horm, method = 'spearman')) # R = .38; p =.13

with(subset(hr.prop, group =='f_D.unisexualis'), cor.test(prop,horm, method = 'spearman')) # R = .30; p =.08
with(subset(hr.prop, group =='f_D.unisexualis' & timepoint == '0623'), cor.test(prop,horm, method = 'spearman')) # R = -.01; p =.98
with(subset(hr.prop, group =='f_D.unisexualis' & timepoint == '0923'), cor.test(prop,horm, method = 'spearman')) # R = .51; p =.05

with(subset(hr.prop, group =='f_D.nairensis'), cor.test(prop,horm, method = 'spearman')) # R = .42; p =.02
with(subset(hr.prop, group =='f_D.nairensis' & timepoint == '0623'), cor.test(prop,horm, method = 'spearman')) # R = .45; p =.27
with(subset(hr.prop, group =='f_D.nairensis' & timepoint == '0923'), cor.test(prop,horm, method = 'spearman')) # R = .42; p =.07

with(subset(hr.prop, group =='m_D.nairensis'), cor.test(prop,horm, method = 'spearman')) # R = .42; p =.01
with(subset(hr.prop, group =='m_D.nairensis' & timepoint == '0623'), cor.test(prop,horm, method = 'spearman')) # R = -.66; p =.04
with(subset(hr.prop, group =='m_D.nairensis' & timepoint == '0923'), cor.test(prop,horm, method = 'spearman')) # R = .24; p =.34

Fig3c<-ggplot(subset(hr.prop, group !='m_D.nairensis' & timepoint != '0523'), aes(horm, prop))+
  geom_smooth(aes(colour = group),
              linewidth = 1,
              se = F,
              method = 'lm')+
  geom_point(aes(fill = group),
             size = 2.5,
             stroke = 1,
             shape = 21)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Progesterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('c')+
  scale_colour_manual(values = c('#eebb22', '#2288cc', '#cc4411'))+
  scale_fill_manual(values = c('#eebb22', '#2288cc', '#cc4411'))

Fig3d<-ggplot(subset(hr.prop, group =='m_D.nairensis' & timepoint != '0523'), aes(horm, prop))+
  geom_smooth(colour = '#44cc66',
              linewidth = 1,
              se = F,
              method = 'lm')+
  geom_point(shape = 21,
             fill = '#44cc66',
             size = 2.5,
             stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Testosterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('d')


# Body condition v parasitic load ####
ggplot(hr.prop, aes(svl, m))+
  geom_smooth(aes(colour = factor(group, levels = c("f_D.armeniaca", "f_D.unisexualis", "f_D.nairensis", "m_D.nairensis"))), linewidth = 1, se = F, method = 'lm')+
  geom_point(aes(fill = factor(group, levels = c("f_D.armeniaca", "f_D.unisexualis", "f_D.nairensis", "m_D.nairensis"))), shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('SVL, mm')+
  ylab('m, g')+
  ggtitle('b')+
  scale_colour_manual(values = c('#eebb22', '#2288cc', '#cc4411', '#44cc66'))+
  scale_fill_manual(values = c('#eebb22', '#2288cc', '#cc4411', '#44cc66'))

hr.prop$bc<-hr.prop$svl/hr.prop$m
with(subset(hr.prop, group =='f_D.armeniaca'), cor.test(bc, prop, method = 'spearman'))
with(subset(hr.prop, group =='f_D.unisexualis'), cor.test(bc, prop, method = 'spearman'))
with(subset(hr.prop, group =='f_D.nairensis'), cor.test(bc, prop, method = 'spearman'))
with(subset(hr.prop, group =='m_D.nairensis'), cor.test(bc, prop, method = 'spearman'))

Fig3b<-ggplot(hr.prop, aes(bc, prop))+
  geom_smooth(aes(colour = factor(group, levels = c("f_D.armeniaca", "f_D.unisexualis", "f_D.nairensis", "m_D.nairensis"))), linewidth = 1, se = F, method = 'lm')+
  geom_point(aes(fill = factor(group, levels = c("f_D.armeniaca", "f_D.unisexualis", "f_D.nairensis", "m_D.nairensis"))), shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Body condition, mm/g')+
  ylab('Parasitic load')+
  ggtitle('b')+
  scale_colour_manual(values = c('#eebb22', '#2288cc', '#cc4411', '#44cc66'))+
  scale_fill_manual(values = c('#eebb22', '#2288cc', '#cc4411', '#44cc66'))
# Infection prevalence between species ####
hr.arm.prev <- hr.prop %>%
  subset(group == 'f_D.armeniaca') %>%
  mutate(infected = case_when(prop == 0 ~ 'healthy',
                              prop > 0 ~ 'infected')) %>%
  group_by(infected) %>%
  summarise(darm = n(), .groups = 'keep')

hr.uni.prev <- hr.prop %>%
  subset(group == 'f_D.unisexualis') %>%
  mutate(infected = case_when(prop == 0 ~ 'healthy',
                              prop > 0 ~ 'infected')) %>%
  group_by(infected) %>%
  summarise(duni = n(), .groups = 'keep')

hr.fnai.prev <- hr.prop %>%
  subset(group == 'f_D.nairensis') %>%
  mutate(infected = case_when(prop == 0 ~ 'healthy',
                              prop > 0 ~ 'infected')) %>%
  group_by(infected) %>%
  summarise(fnai = n(), .groups = 'keep')
hr.fnai.prev <- rbind(hr.fnai.prev, data_frame(infected = 'healthy', fnai = 0))

hr.mnai.prev <- hr.prop %>%
  subset(group == 'm_D.nairensis') %>%
  mutate(infected = case_when(prop == 0 ~ 'healthy',
                              prop > 0 ~ 'infected')) %>%
  group_by(infected) %>%
  summarise(mnai = n(), .groups = 'keep')

fisher.p <- c()
x <- full_join(hr.arm.prev, hr.uni.prev) %>%
  column_to_rownames('infected') %>%
  as.matrix()
fisher.p <- append(fisher.p, fisher.test(x)$p.value)

x <- full_join(hr.arm.prev, hr.fnai.prev) %>%
  column_to_rownames('infected') %>%
  as.matrix()
fisher.p <- append(fisher.p, fisher.test(x)$p.value)

x <- full_join(hr.arm.prev, hr.mnai.prev) %>%
  column_to_rownames('infected') %>%
  as.matrix()
fisher.p <- append(fisher.p, fisher.test(x)$p.value)

x <- full_join(hr.uni.prev, hr.fnai.prev) %>%
  column_to_rownames('infected') %>%
  as.matrix()
fisher.p <- append(fisher.p, fisher.test(x)$p.value)

x <- full_join(hr.uni.prev, hr.mnai.prev) %>%
  column_to_rownames('infected') %>%
  as.matrix()
fisher.p <- append(fisher.p, fisher.test(x)$p.value)

x <- full_join(hr.fnai.prev, hr.mnai.prev) %>%
  column_to_rownames('infected') %>%
  as.matrix()
fisher.p <- append(fisher.p, fisher.test(x)$p.value)

names(fisher.p) <- c('Darm v Duni', 'Darm v Dnai_f', 'Darm v Dnai_m', 
                     'Duni v Dnai_f', 'Duni v Dnai_m',
                     'Dnai_f v Dnai_m')
fisher.p <- p.adjust(fisher.p, method = 'holm')
fisher.p

Fig3g1 <- ggplot(hr.arm.prev, aes(infected, darm/sum(darm)))+
  geom_col(fill = '#eebb22',
           colour = 'gray20')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = 'bold'))+
  ylim(c(0, 1))+
  ylab('Prevalence')+
  scale_x_discrete(labels = c('Healthy', 'Infected'))
Fig3g2 <- ggplot(hr.uni.prev, aes(infected, duni/sum(duni)))+
  geom_col(fill = '#2288cc',
           colour = 'gray20')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title.x = element_blank())+
  ylim(c(0, 1))+
  easy_remove_y_axis()+
  scale_x_discrete(labels = c('Healthy', 'Infected'))
Fig3g3 <- ggplot(hr.fnai.prev, aes(infected, fnai/sum(fnai)))+
  geom_col(fill = '#cc4411',
           colour = 'gray20')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title.x = element_blank())+
  ylim(c(0, 1))+
  easy_remove_y_axis()+
  scale_x_discrete(labels = c('Healthy', 'Infected'))
Fig3g4 <- ggplot(hr.mnai.prev, aes(infected, mnai/sum(mnai)))+
  geom_col(fill = '#44cc66',
           colour = 'gray20')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text = element_text(size = 12, face = 'bold'),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16, face = 'bold', hjust = .8))+
  ggtitle('g')+
  ylim(c(0, 1))+
  easy_remove_y_axis()+
  scale_x_discrete(labels = c('Healthy', 'Infected'))

Fig3g <- plot_grid(Fig3g1, Fig3g2, Fig3g3, Fig3g4,
                   nrow = 1,
                   align = 'hv',
                   axis = 'tb')

# Fig3 ####
legend.plot<-ggplot(hr.prop, aes(svl, m))+
  geom_point(aes(fill = factor(group, levels = c("f_D.armeniaca", "f_D.unisexualis", "f_D.nairensis", "m_D.nairensis"))), shape = 21, size = 5.5, stroke = 1.2)+
  theme_classic()+
  theme(legend.title = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 12),
        legend.position = 'top')+
  scale_fill_manual(values = c('#eebb22', '#2288cc', '#cc4411', '#44cc66'),
                    labels = c(expression(italic('D. armeniaca')),
                               expression(italic('D. unisexualis')),
                               expression(italic('D. r. nairensis') ~bold('♀ ')),
                               expression(italic('D. r. nairensis') ~bold('♂'))
                    ),
                    name = 'Cohort')

leg3<-get_plot_component(legend.plot, 'guide-box-top', return_all = TRUE)

Fig3<-ggarrange(Fig3a, Fig3b, Fig3c, Fig3d, Fig3e, Fig3f,
                ncol = 2,
                nrow = 3,
                align = 'hv')
Fig3 <- plot_grid(Fig3, Fig3g,
                  ncol = 1,
                  rel_heights = c(0.8, 0.2))

plot_grid(Fig3, leg3,
          ncol = 1,
          rel_heights = c(0.95, 0.05))

# GG ...................................................................... ####
# Hormones between species ####
with(subset(gg.horm, sp != 'DportXDdah' & sex == 'f'), wilcox.test(horm~sp))

Fig4e<-ggplot(subset(gg.horm, sp !='DportXDdah' & sex == 'f'), aes(sp, horm))+
  geom_boxplot(aes(colour = sp), linewidth = 1,
               outliers = F)+
  geom_jitter(aes(fill = sp), shape = 21, size = 2.5, position = position_jitterdodge(jitter.width = 0.1), stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('May 2023')+
  ylab('Progesterone concentration, nM')+
  ggtitle('e')+
  scale_x_discrete(labels = c(expression(italic('D. dahli')),
                              expression(italic('D. portschinskii') ~bold('♀ '))
                              ))+
  scale_colour_manual(values = c('#2288cc', '#cc4411'))+
  scale_fill_manual(values = c('#2288cc', '#cc4411'))


# Parasitic load between species ####
with(subset(gg.prop, sp !='DportXDdah'), dunn.test(prop, group))
gg.prop$group <- factor(gg.prop$group, levels = c('f_D.dahli', 'f_D.portschinskii', 'm_D.portschinskii'))

Fig4a<-ggplot(subset(gg.prop, sp !='DportXDdah'), aes(group, prop))+
  geom_boxplot(aes(colour = group), linewidth = 1,
               outliers = F)+
  geom_jitter(aes(fill = group), shape = 21, size = 2.5, position = position_jitterdodge(jitter.width = 0.1), stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Cohort')+
  ylab('Parasitic load')+
  ggtitle('a')+
  scale_x_discrete(labels = c(expression(italic('D. dahli')),
                              expression(italic('D. portschinskii') ~bold('♀ ')),
                              expression(italic('D. portschinskii') ~bold('♂'))
                              ))+
  scale_colour_manual(values = c('#2288cc', '#cc4411', '#44cc66'))+
  scale_fill_manual(values = c('#2288cc', '#cc4411', '#44cc66'))

# Parasitic load v sex steroids ####
#|No significant correlation was observed between sex steroids concentration
#|and parasitic load in either species

with(subset(gg.prop, group =='f_D.dahli'), cor.test(prop, horm, method = 'spearman'))
with(subset(gg.prop, group =='f_D.portschinskii'), cor.test(prop, horm, method = 'spearman'))
with(subset(gg.prop, group =='m_D.portschinskii'), cor.test(prop, horm, method = 'spearman'))

Fig4b<-ggplot(subset(gg.prop, sp !='DportXDdah' & group !='m_D.portschinskii'), aes(horm, prop))+
  geom_smooth(aes(colour = group), linewidth = 1, se = F, method = 'lm')+
  geom_point(aes(fill = group), shape = 21, size = 2.5, stroke = 1, width = 5)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Progesterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('b')+
  scale_colour_manual(values = c('#2288cc', '#cc4411'))+
  scale_fill_manual(values = c('#2288cc', '#cc4411'))

Fig4c<-ggplot(subset(gg.prop, group =='m_D.portschinskii'), aes(horm, prop))+
  geom_smooth(colour = '#44cc66', linewidth = 1, se = F, method = 'lm')+
  geom_point(fill = '#44cc66', shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Testosterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('c')

# Body condition v parasitic load ####
#| SVL ~ m is close to linearity, so we can use proportion as a measure of
#| the body condition.
#| No significant effect of parasitic load on body condition was observed

ggplot(subset(gg.prop, sp !='DportXDdah'), aes(svl, m))+
  geom_smooth(aes(colour = group), linewidth = 1, se = F, method = 'lm')+
  geom_point(aes(fill = group), shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('SVL, mm')+
  ylab('m, g')+
  ggtitle('b')+
  scale_colour_manual(values = c('#2288cc', '#cc4411', '#44cc66'))+
  scale_fill_manual(values = c('#2288cc', '#cc4411', '#44cc66'))

gg.prop$bc<-gg.prop$svl/gg.prop$m
with(subset(gg.prop, group =='f_D.dahli'), cor.test(bc, prop, method = 'spearman'))
with(subset(gg.prop, group =='f_D.portschinskii'), cor.test(bc, prop, method = 'spearman'))
with(subset(gg.prop, group =='m_D.portschinskii'), cor.test(bc, prop, method = 'spearman'))

Fig4d<-ggplot(subset(gg.prop, sp !='DportXDdah'), aes(bc, prop))+
  geom_smooth(aes(colour = group), linewidth = 1, se = F, method = 'lm')+
  geom_point(aes(fill = group), shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Body condition, mm/g')+
  ylab('Parasitic load')+
  ggtitle('d')+
  scale_colour_manual(values = c('#2288cc', '#cc4411', '#44cc66'))+
  scale_fill_manual(values = c('#2288cc', '#cc4411', '#44cc66'))

# Infection prevalence between species ####
gg.dah.prev <- gg.prop %>%
  subset(group == 'f_D.dahli') %>%
  mutate(infected = case_when(prop == 0 ~ 'healthy',
                              prop > 0 ~ 'infected')) %>%
  group_by(infected) %>%
  summarise(ddah = n(), .groups = 'keep')
gg.dah.prev <- rbind(gg.dah.prev, data_frame(infected = 'healthy', ddah = 0))

gg.fpor.prev <- gg.prop %>%
  subset(group == 'f_D.portschinskii') %>%
  mutate(infected = case_when(prop == 0 ~ 'healthy',
                              prop > 0 ~ 'infected')) %>%
  group_by(infected) %>%
  summarise(fpor = n(), .groups = 'keep')

gg.mpor.prev <- gg.prop %>%
  subset(group == 'm_D.portschinskii') %>%
  mutate(infected = case_when(prop == 0 ~ 'healthy',
                              prop > 0 ~ 'infected')) %>%
  group_by(infected) %>%
  summarise(mpor = n(), .groups = 'keep')

fisher.p <- c()

x <- full_join(gg.dah.prev, gg.fpor.prev) %>%
  column_to_rownames('infected') %>%
  as.matrix()
fisher.p <- append(fisher.p, fisher.test(x)$p.value)

x <- full_join(gg.dah.prev, gg.mpor.prev) %>%
  column_to_rownames('infected') %>%
  as.matrix()
fisher.p <- append(fisher.p, fisher.test(x)$p.value)

x <- full_join(gg.fpor.prev, gg.mpor.prev) %>%
  column_to_rownames('infected') %>%
  as.matrix()
fisher.p <- append(fisher.p, fisher.test(x)$p.value)

names(fisher.p) <- c('Ddah v Dport_f', 'Ddah v Dport_m',
                     'Dport_f v Dport_m')
fisher.p <- p.adjust(fisher.p, method = 'holm')
fisher.p

Fig4e1 <- ggplot(gg.dah.prev, aes(infected, ddah/sum(ddah)))+
  geom_col(fill = '#2288cc',
           colour = 'gray20')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = 'bold'))+
  ylim(c(0, 1))+
  ylab('Prevalence')+
  scale_x_discrete(labels = c('Healthy', 'Infected'))
Fig4e2 <- ggplot(gg.fpor.prev, aes(infected, fpor/sum(fpor)))+
  geom_col(fill = '#cc4411',
           colour = 'gray20')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title.x = element_blank())+
  ylim(c(0, 1))+
  easy_remove_y_axis()+
  scale_x_discrete(labels = c('Healthy', 'Infected'))
Fig4e3 <- ggplot(gg.mpor.prev, aes(infected, mpor/sum(mpor)))+
  geom_col(fill = '#44cc66',
           colour = 'gray20')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text = element_text(size = 12, face = 'bold'),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16, face = 'bold', hjust = .85))+
  ggtitle('e')+
  ylim(c(0, 1))+
  easy_remove_y_axis()+
  scale_x_discrete(labels = c('Healthy', 'Infected'))

Fig4e <- plot_grid(Fig4e1, Fig4e2, Fig4e3,
                   nrow = 1,
                   align = 'hv',
                   axis = 'tb')

# Fig4 ####
legend.plot<-ggplot(subset(gg.prop, sp !='DportXDdah'), aes(svl, m))+
  geom_point(aes(fill = group), shape = 21, size = 5.5, stroke = 1.2)+
  theme_classic()+
  theme(legend.title = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 12),
        legend.position = 'top')+
  scale_fill_manual(values = c('#2288cc', '#cc4411', '#44cc66'),
                    labels = c(expression(italic('D. dahli')),
                               expression(italic('D. portschinskii') ~bold('♀ ')),
                               expression(italic('D. portschinskii') ~bold('♂'))
                    ),
                    name = 'Cohort')

leg4<-get_plot_component(legend.plot, 'guide-box-top', return_all = TRUE)

Fig4 <- ggarrange(Fig4a, Fig4b, Fig4c, Fig4d,
          ncol = 2,
          nrow = 2,
          align = 'hv',
          heights = c(4,4,4,4))

Fig4 <- plot_grid(Fig4, Fig4e,
                  ncol = 1,
                  rel_heights = c(0.8, 0.2))

plot_grid(Fig4, leg4,
          ncol = 1,
          rel_heights = c(0.95, 0.05))

# SH....................................................................... ####
# Hormones through time points ####
with(subset(sh.horm, group =='f_D.valentini'), wilcox.test(horm ~ timepoint))
with(subset(sh.horm, group =='m_D.valentini'), wilcox.test(horm ~ timepoint))

Fig5e<-ggplot(subset(sh.horm, sex == 'f'), aes(timepoint, horm))+
  geom_boxplot(colour = '#cc4411', linewidth = 1, outliers = F)+
  geom_jitter(fill = '#cc4411', shape = 21, size = 2.5, position = position_jitterdodge(jitter.width = 0.1), stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Time of collection')+
  ylab('Progesterone concentration, nM')+
  ggtitle('e')+
  scale_x_discrete(labels = c('May 2023', 'June 2023'))

Fig5f<-ggplot(subset(sh.horm, sex == 'm'), aes(timepoint, horm))+
  geom_boxplot(linewidth = 1, outliers = F, colour='#44cc66')+
  geom_jitter(shape = 21, size = 2.5, stroke = 1, fill = '#44cc66', width = 0.1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Time of collection')+
  ylab('Testosterone concentration, nM')+
  ggtitle('f')+
  scale_x_discrete(labels = c('May 2023', 'June 2023'))


# Parasitic load between sexes ####
art<-art(prop~group+Error(timepoint),
         data = sh.prop %>%
           mutate(group = as.factor(group),
                  timepoint = as.factor(timepoint)))
anova(art)
art.con(art, 'group', adjust = 'tukey')

Fig5a<-ggplot(sh.prop, aes(factor(timepoint, levels = c('0722', '0523', '0623')), prop))+
  geom_boxplot(aes(colour = sex), linewidth = 1,
               outliers = F)+
  geom_jitter(aes(fill = sex), shape = 21, size = 2.5, position = position_jitterdodge(jitter.width = 0.1), stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Time of collection')+
  ylab('Parasitic load')+
  ggtitle('a')+
  scale_x_discrete(labels = c('July 2022', 'May 2023', 'June 2023'))+
  scale_colour_manual(values = c('#cc4411', '#44cc66'))+
  scale_fill_manual(values = c('#cc4411', '#44cc66'))
# Parasitic load through time points ####
#| No difference between time points were observed for any species
with(subset(sh.prop, sex =='f'), dunn.test(prop, timepoint))
with(subset(sh.prop, sex =='m'), dunn.test(prop, timepoint))

# Parasitic load v sex steroids ####

#| No data for may 2023
with(subset(sh.prop, sex == 'f'), cor.test(prop,horm, method = 'spearman')) # R = .39; p =.12

with(subset(sh.prop, sex == 'm'), cor.test(prop,horm, method = 'spearman')) # R = -.50; p =.03
with(subset(sh.prop, sex == 'm' & timepoint == '0523'), cor.test(prop,horm, method = 'spearman')) # R = -.37; p =.50
with(subset(sh.prop, sex == 'm' & timepoint == '0623'), cor.test(prop,horm, method = 'spearman')) # R = -.38; p =.18

Fig5c<-ggplot(subset(sh.prop, sex =='f' & timepoint == '0623'), aes(horm, prop))+
  geom_smooth(colour = '#cc4411',
              linewidth = 1,
              se = F,
              method = 'lm')+
  geom_point(fill = '#cc4411',
             shape = 21,
             size = 2.5,
             stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Progesterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('c')

Fig5d<-ggplot(subset(sh.prop, sex =='m' & timepoint != '0722'), aes(horm, prop))+
  geom_smooth(colour = '#44cc66',
              linewidth = 1,
              se = F,
              method = 'lm')+
  geom_point(shape = 21,
             fill = '#44cc66',
             size = 2.5,
             stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Testosterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('d')


# Body condition v parasitic load ####
#| SVL ~ m is close to linearity, so we can use proportion as a measure of
#| the body condition.
#| No significant effect of parasitic load on body condition was observed

ggplot(sh.prop, aes(svl, m))+
  geom_smooth(aes(colour = sex), linewidth = 1, se = F, method = 'lm')+
  geom_point(aes(fill = sex), shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('SVL, mm')+
  ylab('m, g')+
  ggtitle('b')+
  scale_colour_manual(values = c('#cc4411', '#44cc66'))+
  scale_fill_manual(values = c('#cc4411', '#44cc66'))

sh.prop$bc<-sh.prop$svl/sh.prop$m
with(subset(sh.prop, sex =='f'), cor.test(bc, prop))
with(subset(sh.prop, sex =='m'), cor.test(bc, prop))

Fig5b<-ggplot(sh.prop, aes(bc, prop))+
  geom_smooth(aes(colour = sex), linewidth = 1, se = F, method = 'lm')+
  geom_point(aes(fill = sex), shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Body condition, mm/g')+
  ylab('Parasitic load')+
  ggtitle('b')+
  scale_colour_manual(values = c('#cc4411', '#44cc66'))+
  scale_fill_manual(values = c('#cc4411', '#44cc66'))


# Infection prevalence between species ####

sh.fval.prev <- sh.prop %>%
  subset(group == 'f_D.valentini') %>%
  mutate(infected = case_when(prop == 0 ~ 'healthy',
                              prop > 0 ~ 'infected')) %>%
  group_by(infected) %>%
  summarise(fval = n(), .groups = 'keep')

sh.mval.prev <- sh.prop %>%
  subset(group == 'm_D.valentini') %>%
  mutate(infected = case_when(prop == 0 ~ 'healthy',
                              prop > 0 ~ 'infected')) %>%
  group_by(infected) %>%
  summarise(mval = n(), .groups = 'keep')

x <- full_join(sh.fval.prev, sh.mval.prev) %>%
  column_to_rownames('infected') %>%
  as.matrix()
fisher.test(x)

Fig5g1 <- ggplot(sh.fval.prev, aes(infected, fval/sum(fval)))+
  geom_col(fill = '#cc4411',
           colour = 'gray20')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = 'bold'))+
  ylim(c(0, 1))+
  ylab('Prevalence')+
  scale_x_discrete(labels = c('Healthy', 'Infected'))
Fig5g2 <- ggplot(sh.mval.prev, aes(infected, mval/sum(mval)))+
  geom_col(fill = '#44cc66',
           colour = 'gray20')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text = element_text(size = 12, face = 'bold'),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  ggtitle('g')+
  ylim(c(0, 1))+
  easy_remove_y_axis()+
  scale_x_discrete(labels = c('Healthy', 'Infected'))

Fig5g <- plot_grid(Fig5g1, Fig5g2,
                   nrow = 1,
                   align = 'hv',
                   axis = 'tb')

# Fig5 ####
legend.plot<-ggplot(sh.prop, aes(svl, m))+
  geom_point(aes(fill = sex), shape = 21, size = 5.5, stroke = 1.2)+
  theme_classic()+
  theme(legend.title = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 12),
        legend.position = 'top')+
  scale_fill_manual(values = c('#cc4411', '#44cc66'),
                    labels = c(expression(italic('D. valentini') ~bold('♀ ')),
                               expression(italic('D. valentini') ~bold('♂'))
                    ),
                    name = 'Cohort')

leg5<-get_plot_component(legend.plot, 'guide-box-top', return_all = TRUE)

Fig5<-ggarrange(Fig5a, Fig5b, Fig5c, Fig5d, Fig5e, Fig5f,
                ncol = 2,
                nrow = 3,
                align = 'hv')

Fig5 <- plot_grid(Fig5, Fig5g,
                  ncol = 1,
                  rel_heights = c(0.8, 0.2))

plot_grid(Fig5, leg5,
          ncol = 1,
          rel_heights = c(0.95, 0.05))

# SR....................................................................... ####
sr.prop<-subset(sr.prop, sex != 'sex')
# Hormones through time points ####
with(subset(sr.horm, group =='f_D.armeniaca'), dunn.test(horm, timepoint, method = 'holm'))
with(subset(sr.horm, group =='f_D.valentini'), dunn.test(horm, timepoint, method = 'holm'))
with(subset(sr.horm, group =='m_D.valentini'), dunn.test(horm, timepoint, method = 'holm'))

# Hormones between species ####
art<-art(horm~group+Error(timepoint),
         data = subset(sr.horm, sex == 'f') %>%
           mutate(group = as.factor(group),
                  timepoint = as.factor(timepoint)))
anova(art)
art.con(art, 'group', adjust = 'tukey')


Fig6e<-ggplot(subset(sr.horm, sex == 'f'), aes(timepoint, horm))+
  geom_boxplot(aes(colour = factor(sp, levels = c('D.armeniaca', 'D.valentini'))), linewidth = 1,
               outliers = F)+
  geom_jitter(aes(fill = factor(sp, levels = c('D.armeniaca', 'D.valentini'))), shape = 21, size = 2.5, position = position_jitterdodge(jitter.width = 0.1), stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Time of collection')+
  ylab('Progesterone concentration, nM')+
  ggtitle('e')+
  scale_x_discrete(labels = c('May 2023', 'June 2023', 'September 2023'))+
  scale_colour_manual(values = c('#eebb22', '#2288cc'))+
  scale_fill_manual(values = c('#eebb22', '#2288cc'))

Fig6f<-ggplot(subset(sr.horm, sex == 'm'), aes(timepoint, horm))+
  geom_boxplot(linewidth = 1,
               outliers = F,
               colour='#44cc66')+
  geom_jitter(shape = 21, size = 2.5, stroke = 1, fill = '#44cc66', width = 0.1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Time of collection')+
  ylab('Testosterone concentration, nM')+
  ggtitle('f')+
  scale_x_discrete(labels = c('May 2023', 'June 2023', 'September 2023'))



# Parasitic load between groups ####
art<-art(prop~group+Error(timepoint),
         data = sr.prop %>%
           mutate(group = as.factor(group),
                  timepoint = as.factor(timepoint)
                  ))
anova(art)
art.con(art, 'group', adjust = 'tukey')

Fig6a<-ggplot(sr.prop, aes(factor(timepoint, levels = c('0522', '0722', '0523', '0623', '0923')), prop))+
  geom_boxplot(aes(colour = group), linewidth = 1,
               outliers = F)+
  geom_jitter(aes(fill = group), shape = 21, size = 2.5, position = position_jitterdodge(jitter.width = 0.1), stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Time of collection')+
  ylab('Parasitic load')+
  ggtitle('a')+
  scale_x_discrete(labels = c('May2022', 'July 2022', 'May 2023', 'June 2023', 'September 2023'))+
  scale_colour_manual(values = c('#eebb22','#cc4411', '#44cc66'))+
  scale_fill_manual(values = c('#eebb22','#cc4411', '#44cc66'))

# Parasitic load through time points ####
#| No difference between time points were observed for any species

model<-with(subset(sr.prop, group =='f_D.armeniaca'), lm(prop~timepoint))
anova(model)
TukeyHSD(aov(model))
with(subset(sr.prop, group =='f_D.armeniaca' & str_detect(timepoint, '23')), dunn.test(prop, timepoint))
with(subset(sr.prop, group =='f_D.armeniaca' & str_detect(timepoint, '22')), dunn.test(prop, timepoint))

#Paired design not enough data
temp1<-subset(sr.prop, timepoint == '0522' & sp == 'D.armeniaca') %>%
  select(c('id', 'prop'))
temp2<-subset(sr.prop, timepoint == '0722' & sp == 'D.armeniaca') %>%
  select(c('id', 'prop'))
temp<-inner_join(temp1, temp2, by = 'id')

temp1<-subset(sr.prop, timepoint == '0523' & sp == 'D.armeniaca') %>%
  select(c('id', 'prop'))
temp2<-subset(sr.prop, timepoint == '0623' & sp == 'D.armeniaca') %>%
  select(c('id', 'prop'))
temp3<-subset(sr.prop, timepoint == '0923' & sp == 'D.armeniaca') %>%
  select(c('id', 'prop'))
temp<-inner_join(temp1, temp2, by = 'id')
temp<-inner_join(temp3, temp2, by = 'id')
temp<-inner_join(temp3, temp1, by = 'id')

model<-with(subset(sr.prop, group =='f_D.valentini'), lm(prop~timepoint))
anova(model)
TukeyHSD(aov(model))

temp1<-subset(sr.prop, timepoint == '0522' & group == 'f_D.valentini') %>%
  select(c('id', 'prop'))
temp2<-subset(sr.prop, timepoint == '0722' & group == 'f_D.valentini') %>%
  select(c('id', 'prop'))
temp<-inner_join(temp1, temp2)

temp1<-subset(sr.prop, timepoint == '0523' & group == 'f_D.valentini') %>%
  select(c('id', 'prop'))
temp2<-subset(sr.prop, timepoint == '0623' & group == 'f_D.valentini') %>%
  select(c('id', 'prop'))
temp<-inner_join(temp1, temp2)

model<-with(subset(sr.prop, group =='m_D.valentini'), lm(prop~timepoint))
anova(model)
TukeyHSD(aov(model))

temp1<-subset(sr.prop, timepoint == '0522' & group == 'm_D.valentini') %>%
  select(c('id', 'prop'))
temp2<-subset(sr.prop, timepoint == '0722' & group == 'm_D.valentini') %>%
  select(c('id', 'prop'))
temp<-inner_join(temp1, temp2)

temp1<-subset(sr.prop, timepoint == '0523' & group == 'm_D.valentini') %>%
  select(c('id', 'prop'))
temp2<-subset(sr.prop, timepoint == '0623' & group == 'm_D.valentini') %>%
  select(c('id', 'prop'))
temp<-inner_join(temp1, temp2)

# Parasitic load v sex steroids ####

with(subset(sr.prop, group =='f_D.armeniaca'), cor.test(prop,horm, method = 'spearman')) # R = .36; p =.04
with(subset(sr.prop, group =='f_D.armeniaca' & timepoint == '0523' ), cor.test(prop,horm, method = 'spearman')) # R = -.71; p =.09
with(subset(sr.prop, group =='f_D.armeniaca' & timepoint == '0623' ), cor.test(prop,horm, method = 'spearman')) # R = -.40; p =.11
with(subset(sr.prop, group =='f_D.armeniaca' & timepoint == '0923' ), cor.test(prop,horm, method = 'spearman')) # R = .48; p =.14

with(subset(sr.prop, group =='f_D.valentini'), cor.test(prop,horm, method = 'spearman')) # R = .30; p =.08
with(subset(sr.prop, group =='f_D.valentini' & timepoint == '0523'), cor.test(prop,horm, method = 'spearman')) # R = -.1; p =.95
with(subset(sr.prop, group =='f_D.valentini' & timepoint == '0623'), cor.test(prop,horm, method = 'spearman')) # R = .12; p =.67
with(subset(sr.prop, group =='f_D.valentini' & timepoint == '0923'), cor.test(prop,horm, method = 'spearman')) # R = 1; p =.33

with(subset(sr.prop, group =='m_D.valentini'), cor.test(prop,horm, method = 'spearman')) # R = .42; p =.01
with(subset(sr.prop, group =='m_D.valentini' & timepoint == '0523'), cor.test(prop,horm, method = 'spearman')) # R = -.33; p =.23
with(subset(sr.prop, group =='m_D.valentini' & timepoint == '0623'), cor.test(prop,horm, method = 'spearman')) # R = -.43; p =.15
#with(subset(sr.prop, group =='m_D.valentini' & timepoint == '0923'), cor.test(prop,horm, method = 'spearman'))


Fig6c<-ggplot(subset(sr.prop, group !='m_D.valentini' & timepoint != '0522' & timepoint != '0722'), aes(horm, prop))+
  geom_smooth(aes(colour = group),
              linewidth = 1,
              se = F,
              method = 'lm')+
  geom_point(aes(fill = group), 
             size = 2.5, 
             stroke = 1,
             shape = 21)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Progesterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('c')+
  scale_colour_manual(values = c('#eebb22', '#cc4411'))+
  scale_fill_manual(values = c('#eebb22', '#cc4411'))

Fig6d<-ggplot(subset(sr.prop, group =='m_D.valentini' & timepoint != '0522' & timepoint != '0722' & timepoint != '0923'), aes(horm, prop))+
  geom_smooth(linewidth = 1,
              se = F,
              method = 'lm',
              colour='#44cc66')+
  geom_point(shape = 21,
             size = 2.5,
             stroke = 1, 
             fill='#44cc66')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Testosterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('d')+
  scale_shape_manual(values = c(21, 22),
                     labels = c('May 2023', 'June 2023'),
                     name = 'Time point')+
  scale_linetype_manual(values = c(1,2,3),
                        labels = c('May 2023', 'June 2023'),
                        name = 'Time point')


# Body condition v parasitic load ####
ggplot(sr.prop, aes(svl, m))+
  geom_smooth(aes(colour = factor(group, levels = c("f_D.armeniaca", "f_D.valentini", "m_D.valentini"))), linewidth = 1, se = F, method = 'lm')+
  geom_point(aes(fill = factor(group, levels = c("f_D.armeniaca", "f_D.valentini", "m_D.valentini"))), shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Progesterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('b')+
  scale_colour_manual(values = c('#eebb22', '#cc4411', '#44cc66'))+
  scale_fill_manual(values = c('#eebb22', '#cc4411', '#44cc66'))

sr.prop$bc<-sr.prop$svl/sr.prop$m

with(subset(sr.prop, group =='f_D.armeniaca'), cor.test(bc, prop, method = 'spearman'))
with(subset(sr.prop, group =='f_D.valentini'), cor.test(bc, prop, method = 'spearman'))
with(subset(sr.prop, group =='m_D.valentini'), cor.test(bc, prop, method = 'spearman'))

Fig6b<-ggplot(sr.prop, aes(bc, prop))+
  geom_smooth(aes(colour = factor(group, levels = c("f_D.armeniaca", "f_D.valentini", "m_D.valentini"))), linewidth = 1, se = F, method = 'lm')+
  geom_point(aes(fill = factor(group, levels = c("f_D.armeniaca", "f_D.valentini", "m_D.valentini"))), shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Body condition, mm/g')+
  ylab('Parasitic load')+
  ggtitle('b')+
  scale_colour_manual(values = c('#eebb22', '#cc4411', '#44cc66'))+
  scale_fill_manual(values = c('#eebb22', '#cc4411', '#44cc66'))

# Infection prevalence between species ####
sr.darm.prev <- sr.prop %>%
  subset(group == 'f_D.armeniaca') %>%
  mutate(infected = case_when(prop == 0 ~ 'healthy',
                              prop > 0 ~ 'infected')) %>%
  group_by(infected) %>%
  summarise(darm = n(), .groups = 'keep')

sr.fval.prev <- sr.prop %>%
  subset(group == 'f_D.valentini') %>%
  mutate(infected = case_when(prop == 0 ~ 'healthy',
                              prop > 0 ~ 'infected')) %>%
  group_by(infected) %>%
  summarise(fval = n(), .groups = 'keep')

sr.mval.prev <- sr.prop %>%
  subset(group == 'm_D.valentini') %>%
  mutate(infected = case_when(prop == 0 ~ 'healthy',
                              prop > 0 ~ 'infected')) %>%
  group_by(infected) %>%
  summarise(mval = n(), .groups = 'keep')

fisher.p <- c()

x <- full_join(sr.darm.prev, sr.fval.prev) %>%
  column_to_rownames('infected') %>%
  as.matrix()
fisher.p <- append(fisher.p, fisher.test(x)$p.value)

x <- full_join(sr.darm.prev, sr.mval.prev) %>%
  column_to_rownames('infected') %>%
  as.matrix()
fisher.p <- append(fisher.p, fisher.test(x)$p.value)

x <- full_join(sr.fval.prev, sr.mval.prev) %>%
  column_to_rownames('infected') %>%
  as.matrix()
fisher.p <- append(fisher.p, fisher.test(x)$p.value)

names(fisher.p) <- c('Darm v Dval_f', 'Darm v Dval_m',
                     'Dval_f v Dval_m')
fisher.p <- p.adjust(fisher.p, method = 'holm')
fisher.p

Fig6g1 <- ggplot(sr.darm.prev, aes(infected, darm/sum(darm)))+
  geom_col(fill = '#eebb22',
           colour = 'gray20')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = 'bold'))+
  ylim(c(0, 1))+
  ylab('Prevalence')+
  scale_x_discrete(labels = c('Healthy', 'Infected'))
Fig6g2 <- ggplot(sr.fval.prev, aes(infected, fval/sum(fval)))+
  geom_col(fill = '#cc4411',
           colour = 'gray20')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title.x = element_blank())+
  ylim(c(0, 1))+
  easy_remove_y_axis()+
  scale_x_discrete(labels = c('Healthy', 'Infected'))
Fig6g3 <- ggplot(sr.mval.prev, aes(infected, mval/sum(mval)))+
  geom_col(fill = '#44cc66',
           colour = 'gray20')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text = element_text(size = 12, face = 'bold'),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16, face = 'bold', hjust = .85))+
  ggtitle('g')+
  ylim(c(0, 1))+
  easy_remove_y_axis()+
  scale_x_discrete(labels = c('Healthy', 'Infected'))

Fig6g <- plot_grid(Fig6g1, Fig6g2, Fig6g3,
                   nrow = 1,
                   align = 'hv',
                   axis = 'tb')

# Fig6 ####
legend.plot<-ggplot(sr.prop, aes(svl, m))+
  geom_point(aes(fill = group), shape = 21, size = 5.5, stroke = 1.2)+
  theme_classic()+
  theme(legend.title = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 12),
        legend.position = 'top')+
  scale_fill_manual(values = c('#eebb22','#cc4411', '#44cc66'),
                    labels = c(expression(italic('D. armeniaca')),
                               expression(italic('D. valentini') ~bold('♀ ')),
                               expression(italic('D. valentini') ~bold('♂'))
                    ),
                    name = 'Cohort')

leg6<-get_plot_component(legend.plot, 'guide-box-top', return_all = TRUE)

Fig6<-ggarrange(Fig6a, Fig6b, Fig6c, Fig6d, Fig6e, Fig6f,
                ncol = 2,
                nrow = 3,
                align = 'hv')

Fig6 <- plot_grid(Fig6, Fig6g,
                  ncol = 1,
                  rel_heights = c(0.8, 0.2))
plot_grid(Fig6, leg6,
          ncol = 1,
          rel_heights = c(0.95, 0.05))
