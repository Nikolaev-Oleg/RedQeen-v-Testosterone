#Packages####
library(gsheet)
library(tidyverse)
library(dunn.test)
library(glue)
library(ggpubr)
library(cowplot)
library(ARTool) 
library(emmeans)

#Prepare data####
df.full <- read.csv('./data/main_check.csv',
                   header = T) %>%
  mutate(horm = as.numeric(horm),
         timepoint = glue('{str_sub(date, 3, 4)}{str_sub(date, 7, 8)}'),
         group = glue('{sex}_{sp}')) %>%
  subset(timepoint != '0822')
df.horm_v_prop <- subset(df.full, !is.na(horm) & !is.na(prop))
df.horm <- subset(df.full, !is.na(horm))
df.prop <- subset(df.full, !is.na(prop))

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
#AC (Correct plot 22.08.2025)####
#Hormones through time points####
with(subset(ac.horm, group =='f_D.armeniaca'), dunn.test(horm, timepoint, method = 'holm'))
with(subset(ac.horm, group =='f_D.dahli'), dunn.test(horm, timepoint, method = 'holm'))
with(subset(ac.horm, group =='f_D.portschinskii'), dunn.test(horm, timepoint, method = 'holm'))
with(subset(ac.horm, group =='m_D.portschinskii'), dunn.test(horm, timepoint, method = 'holm'))

#Hormones between species####
art<-art(horm~group+Error(timepoint),
         data = subset(ac.horm, sp !='DportXDdah' & sex != 'm') %>%
           mutate(group = as.factor(group),
                  timepoint = as.factor(timepoint)))
anova(art)
art.con(art, 'group', adjust = 'tukey')

Fig1e<-ggplot(subset(ac.horm, sp !='DportXDdah' & sex == 'f'), aes(timepoint, horm))+
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

Fig1f<-ggplot(subset(ac.horm, sp !='DportXDdah' & sex == 'm'), aes(timepoint, horm))+
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

#Parasitic load between species####
#| Two-way ANOVA was applied as a robust method
#| parthenogenetic species were the most parasitized cohorts
#| followed by male D. portschinskii
#| However, KW-test did not show any sinificant differences
#| when processing each timepoint separately (probably due to low power)

art<-art(prop~group+Error(timepoint),
    data = subset(ac.prop, sp !='DportXDdah') %>%
      mutate(group = as.factor(group),
             timepoint = as.factor(timepoint)))
anova(art)
art.con(art, 'group', adjust = 'tukey')

Fig1a<-ggplot(subset(ac.prop, sp !='DportXDdah'), aes(timepoint, prop))+
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

#Parasitic load through time points####
#| No difference between time points were observed for any species
with(subset(ac.prop, group =='f_D.armeniaca'), dunn.test(prop, timepoint))
with(subset(ac.prop, group =='f_D.dahli'), dunn.test(prop, timepoint))
with(subset(ac.prop, group =='f_D.portschinskii'), dunn.test(prop, timepoint))
with(subset(ac.prop, group =='m_D.portschinskii'), dunn.test(prop, timepoint))

#Parasitic load v sex steroids####
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

Fig1c<-ggplot(subset(ac.horm_v_prop, sp !='DportXDdah' & group !='m_D.portschinskii'), aes(horm, prop))+
  geom_smooth(aes(colour = group, linetype = timepoint), linewidth = 1, se = F, method = 'lm')+
  geom_point(aes(fill = group, shape = timepoint), size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Progesterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('c')+
  scale_colour_manual(values = c('#eebb22', '#2288cc', '#cc4411'))+
  scale_fill_manual(values = c('#eebb22', '#2288cc', '#cc4411'))+
  scale_shape_manual(values = c(21, 22, 24),
                     labels = c('May 2023', 'June 2023', 'September 2023'),
                     name = 'Time point')+
  scale_linetype_manual(values = c(1,2,3),
                        labels = c('May 2023', 'June 2023', 'September 2023'),
                        name = 'Time point')

Fig1d<-ggplot(subset(ac.horm_v_prop, group =='m_D.portschinskii'), aes(horm, prop))+
  geom_smooth(aes(linetype = timepoint),
              colour = '#44cc66', linewidth = 1, se = F, method = 'lm')+
  geom_point(aes(shape = timepoint),
             fill = '#44cc66', size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        legend.title = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9),
        legend.key.width = unit(2, 'cm'))+
  xlab('Testosterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('d')+
  scale_shape_manual(values = c(21, 22, 24),
                     labels = c('May 2023', 'June 2023', 'September 2023'),
                     name = 'Time point')+
  scale_linetype_manual(values = c(1,2,3),
                        labels = c('May 2023', 'June 2023', 'September 2023'),
                        name = 'Time point')

#Body condition v parasitic load####
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

Fig1b<-ggplot(subset(ac.prop, sp !='DportXDdah'), aes(bc, prop))+
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

#Fig1####
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

legend<-get_plot_component(legend.plot, 'guide-box-top', return_all = TRUE)

legend.plot<-ggplot(subset(ac.horm_v_prop, group =='m_D.portschinskii'), aes(horm, prop))+
  geom_smooth(aes(linetype = timepoint),
              colour = 'gray70', linewidth = 1, se = F, method = 'lm')+
  geom_point(aes(shape = timepoint),
             fill = 'gray80', size = 3.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'top',
        legend.title = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9),
        legend.key.width = unit(2, 'cm'),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing = unit(0, "cm"),
        legend.box.spacing = unit(0, "cm"))+
  xlab('Testosterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('c')+
  scale_shape_manual(values = c(21, 22, 24),
                     labels = c('May 2023', 'June 2023', 'September 2023'),
                     name = 'Time of collection')+
  scale_linetype_manual(values = c(1,2,3),
                        labels = c('May 2023', 'June 2023', 'September 2023'),
                        name = 'Time of collection')

legend2<-get_plot_component(legend.plot, 'guide-box-top', return_all = TRUE)

Fig1<-ggarrange(Fig1a, Fig1b, Fig1c, Fig1d, Fig1e, Fig1f,
          ncol = 2,
          nrow = 3,
          align = 'hv')
leg1<-ggarrange(legend, legend2, nrow = 1)

plot_grid(Fig1, leg1,
          ncol = 1,
          rel_heights = c(0.95, 0.05))

################################################################################
#HR####
#Hormones through time points####
with(subset(hr.horm, group =='f_D.armeniaca'), wilcox.test(horm ~ timepoint))
with(subset(hr.horm, group =='f_D.unisexualis'), wilcox.test(horm ~ timepoint))
with(subset(hr.horm, group =='f_D.nairensis'), wilcox.test(horm ~ timepoint))
with(subset(hr.horm, group =='m_D.nairensis'), wilcox.test(horm ~ timepoint))

#Hormones between species####
art<-art(horm~group+Error(timepoint),
         data = subset(hr.horm, sex == 'f') %>%
           mutate(group = as.factor(group),
                  timepoint = as.factor(timepoint)))
anova(art)
art.con(art, 'group', adjust = 'tukey')


#Fig0####
Fig2e<-ggplot(subset(hr.horm, sp !='DportXDdah' & sex == 'f'), aes(timepoint, horm))+
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
  ggtitle('c')+
  scale_x_discrete(labels = c('June 2023', 'September 2023'))+
  scale_colour_manual(values = c('#eebb22', '#2288cc', '#cc4411'))+
  scale_fill_manual(values = c('#eebb22', '#2288cc', '#cc4411'))

Fig2f<-ggplot(subset(hr.horm, sex == 'm'), aes(timepoint, horm))+
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
  ggtitle('d')+
  scale_x_discrete(labels = c('June 2023', 'September 2023'))


#Parasitic load between species####
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

Fig2a<-ggplot(hr.prop, aes(timepoint, prop))+
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

#Parasitic load through time points####
#| No difference between time points were observed for any species

with(subset(hr.prop, group =='f_D.armeniaca'), dunn.test(prop, timepoint))
with(subset(hr.prop, group =='f_D.unisexualis'), dunn.test(prop, timepoint))
with(subset(hr.prop, group =='f_D.nairensis'), dunn.test(prop, timepoint))
with(subset(hr.prop, group =='m_D.nairensis'), dunn.test(prop, timepoint))
#Parasitic load v sex steroids####

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

Fig2b<-ggplot(subset(hr.prop, group !='m_D.nairensis'), aes(horm, prop))+
  geom_smooth(data=subset(hr.prop, group !='m_D.nairensis' & timepoint == '0623'),
              aes(colour = factor(group, levels = c("f_D.armeniaca", "f_D.unisexualis", "f_D.nairensis"))), linewidth = 1, se = F, method = 'lm')+
  geom_smooth(data=subset(hr.prop, group !='m_D.nairensis' & timepoint == '0923'),
              aes(colour = factor(group, levels = c("f_D.armeniaca", "f_D.unisexualis", "f_D.nairensis"))), linewidth = 1, se = F, method = 'lm')+
  geom_point(aes(fill = factor(group, levels = c("f_D.armeniaca", "f_D.unisexualis", "f_D.nairensis"))), shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Progesterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('b')+
  scale_colour_manual(values = c('#eebb22', '#2288cc', '#cc4411'))+
  scale_fill_manual(values = c('#eebb22', '#2288cc', '#cc4411'))

Fig2c<-ggplot(subset(hr.prop, group =='m_D.nairensis'), aes(horm, prop))+
  geom_smooth(data = subset(hr.prop, group =='m_D.nairensis' & timepoint == '0623'),
              colour = '#44cc66', linewidth = 1, se = F, method = 'lm')+
  geom_smooth(data = subset(hr.prop, group =='m_D.nairensis' & timepoint == '0923'),
              colour = '#44cc66', linewidth = 1, se = F, method = 'lm')+
  geom_point(fill = '#44cc66', shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Testosterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('c')


#Body condition v parasitic load####
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

Fig2d<-ggplot(hr.prop, aes(bc, prop))+
  geom_smooth(aes(colour = factor(group, levels = c("f_D.armeniaca", "f_D.unisexualis", "f_D.nairensis", "m_D.nairensis"))), linewidth = 1, se = F, method = 'lm')+
  geom_point(aes(fill = factor(group, levels = c("f_D.armeniaca", "f_D.unisexualis", "f_D.nairensis", "m_D.nairensis"))), shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Body condition, mm/g')+
  ylab('Parasitic load')+
  ggtitle('d')+
  scale_colour_manual(values = c('#eebb22', '#2288cc', '#cc4411', '#44cc66'))+
  scale_fill_manual(values = c('#eebb22', '#2288cc', '#cc4411', '#44cc66'))
#Fig2####
legend.plot<-ggplot(hr.prop, aes(svl, m))+
  geom_point(aes(fill = factor(group, levels = c("f_D.armeniaca", "f_D.unisexualis", "f_D.nairensis", "m_D.nairensis"))), shape = 21, size = 5.5, stroke = 1.2)+
  theme_classic()+
  theme(legend.title = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 12),
        legend.position = 'top')+
  scale_fill_manual(values = c('#eebb22', '#2288cc', '#cc4411', '#44cc66'),
                    labels = c(expression(italic('D. armeniaca')),
                               expression(italic('D. dahli')),
                               expression(italic('D. portschinskii') ~bold('♀ ')),
                               expression(italic('D. portschinskii') ~bold('♂'))
                    ),
                    name = 'Cohort')

legend<-get_plot_component(legend.plot, 'guide-box-top', return_all = TRUE)

ggarrange(Fig2a, Fig2b, Fig2c, Fig2d, legend,
          ncol = 1,
          align = 'hv',
          heights = c(4,4,4,4,1))


################################################################################
#GG####
#Hormones between species####
with(subset(gg.horm, sp != 'DportXDdah' & sex == 'f'), wilcox.test(horm~sp))

#Fig0####
Fig3e<-ggplot(subset(gg.horm, sp !='DportXDdah' & sex == 'f'), aes(sp, horm))+
  geom_boxplot(aes(colour = sp), linewidth = 1,
               outliers = F)+
  geom_jitter(aes(fill = sp), shape = 21, size = 2.5, position = position_jitterdodge(jitter.width = 0.1), stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Cohort')+
  ylab('Progesterone concentration, nM')+
  ggtitle('e')+
  scale_x_discrete(labels = c(expression(italic('D. dahli')),
                              expression(italic('D. portschinskii') ~bold('♀ '))
                              ))+
  scale_colour_manual(values = c('#2288cc', '#cc4411'))+
  scale_fill_manual(values = c('#2288cc', '#cc4411'))


#Parasitic load between species####
#| Two-way ANOVA was applied as a robust method
#| parthenogenetic species were the most parasitized cohorts
#| followed by male D. portschinskii
#| However, KW-test did not show any sinificant differences
#| when processing each timepoint separately (probably due to low power)

with(subset(gg.prop, sp !='DportXDdah'), dunn.test(prop, group))

Fig3a<-ggplot(subset(gg.prop, sp !='DportXDdah'), aes(group, prop))+
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
  scale_x_discrete(labels = c(expression(italic('D. dahli')),
                              expression(italic('D. portschinskii') ~bold('♀ ')),
                              expression(italic('D. portschinskii') ~bold('♂'))
                              ))+
  scale_colour_manual(values = c('#2288cc', '#cc4411', '#44cc66'))+
  scale_fill_manual(values = c('#2288cc', '#cc4411', '#44cc66'))

#Parasitic load v sex steroids####
#|No significant correlation was observed between sex steroids concentration
#|and parasitic load in either species

with(subset(gg.prop, group =='f_D.dahli'), cor.test(prop, horm, method = 'spearman'))
with(subset(gg.prop, group =='f_D.portschinskii'), cor.test(prop, horm, method = 'spearman'))
with(subset(gg.prop, group =='m_D.portschinskii'), cor.test(prop, horm, method = 'spearman'))

Fig3b<-ggplot(subset(gg.prop, sp !='DportXDdah' & group !='m_D.portschinskii'), aes(horm, prop))+
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

Fig3c<-ggplot(subset(gg.prop, group =='m_D.portschinskii'), aes(horm, prop))+
  geom_smooth(colour = '#44cc66', linewidth = 1, se = F, method = 'lm')+
  geom_point(fill = '#44cc66', shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Testosterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('c')

#Body condition v parasitic load####
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

Fig3d<-ggplot(subset(gg.prop, sp !='DportXDdah'), aes(bc, prop))+
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

#Fig3####
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

legend<-get_plot_component(legend.plot, 'guide-box-top', return_all = TRUE)

ggarrange(Fig3a, Fig3b, Fig3c, Fig3d, legend,
          ncol = 1,
          align = 'hv',
          heights = c(4,4,4,4,1))


################################################################################
#SH####
#Hormones through time points####
with(subset(sh.horm, group =='f_D.valentini'), wilcox.test(horm ~ timepoint))
with(subset(sh.horm, group =='m_D.valentini'), wilcox.test(horm ~ timepoint))

#Fig0####
Fig4e<-ggplot(subset(sh.horm, sp !='DportXDdah' & sex == 'f'), aes(timepoint, horm))+
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
  ggtitle('c')+
  scale_x_discrete(labels = c('June 2023', 'September 2023'))+
  scale_colour_manual(values = c('#eebb22', '#2288cc', '#cc4411'))+
  scale_fill_manual(values = c('#eebb22', '#2288cc', '#cc4411'))

Fig4f<-ggplot(subset(hr.horm, sex == 'm'), aes(timepoint, horm))+
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
  ggtitle('d')+
  scale_x_discrete(labels = c('June 2023', 'September 2023'))


#Parasitic load between sexes####
art<-art(prop~group+Error(timepoint),
         data = sh.prop %>%
           mutate(group = as.factor(group),
                  timepoint = as.factor(timepoint)))
anova(art)
art.con(art, 'group', adjust = 'tukey')

Fig4a<-ggplot(sh.prop, aes(factor(timepoint, levels = c('0722', '0523', '0623')), prop))+
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
#Parasitic load through time points####
#| No difference between time points were observed for any species
with(subset(sh.prop, sex =='f'), dunn.test(prop, timepoint))
with(subset(sh.prop, sex =='m'), dunn.test(prop, timepoint))

#Parasitic load v sex steroids####
#|No significant correlation was observed between sex steroids concentration
#|and parasitic load in either species

#| No data for may 2023
with(subset(sh.prop, sex == 'f'), cor.test(prop,horm, method = 'spearman')) # R = .39; p =.12

with(subset(sh.prop, sex == 'm'), cor.test(prop,horm, method = 'spearman')) # R = -.50; p =.03
with(subset(sh.prop, sex == 'm' & timepoint == '0523'), cor.test(prop,horm, method = 'spearman')) # R = -.37; p =.50
with(subset(sh.prop, sex == 'm' & timepoint == '0623'), cor.test(prop,horm, method = 'spearman')) # R = -.38; p =.18

Fig4b<-ggplot(subset(sh.prop, sex =='f'), aes(horm, prop))+
  geom_smooth(colour = '#cc4411', linewidth = 1, se = F, method = 'lm')+
  geom_point(fill = '#cc4411', shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Progesterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('b')

Fig4c<-ggplot(subset(sh.prop, sex =='m'), aes(horm, prop))+
  geom_smooth(data = subset(sh.prop, sex =='m' & timepoint == '0523'),
              colour = '#44cc66', linewidth = 1, se = F, method = 'lm')+
  geom_smooth(data = subset(sh.prop, sex =='m' & timepoint == '0623'),
              colour = '#44cc66', linewidth = 1, se = F, method = 'lm')+
  geom_point(fill = '#44cc66', shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Testosterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('c')


#Body condition v parasitic load####
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

Fig4d<-ggplot(sh.prop, aes(bc, prop))+
  geom_smooth(aes(colour = sex), linewidth = 1, se = F, method = 'lm')+
  geom_point(aes(fill = sex), shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Body condition, mm/g')+
  ylab('Parasitic load')+
  ggtitle('d')+
  scale_colour_manual(values = c('#cc4411', '#44cc66'))+
  scale_fill_manual(values = c('#cc4411', '#44cc66'))


#Fig4####
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

legend<-get_plot_component(legend.plot, 'guide-box-top', return_all = TRUE)

ggarrange(Fig4a, Fig4b, Fig4c, Fig4d, legend,
          ncol = 1,
          align = 'hv',
          heights = c(4,4,4,4,1))
################################################################################
#SR####
sr.prop<-subset(sr.prop, sex != 'sex')
#Hormones through time points####
with(subset(sr.horm, group =='f_D.armeniaca'), dunn.test(horm, timepoint, method = 'holm'))
with(subset(sr.horm, group =='f_D.valentini'), dunn.test(horm, timepoint, method = 'holm'))
with(subset(sr.horm, group =='m_D.valentini'), dunn.test(horm, timepoint, method = 'holm'))

#Hormones between species####
art<-art(horm~group+Error(timepoint),
         data = subset(sr.horm, sex == 'f') %>%
           mutate(group = as.factor(group),
                  timepoint = as.factor(timepoint)))
anova(art)
art.con(art, 'group', adjust = 'tukey')


#Fig0####
Fig5e<-ggplot(subset(sr.horm, sp !='DportXDdah' & sex == 'f'), aes(timepoint, horm))+
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
  ggtitle('c')+
  scale_x_discrete(labels = c('May 2023', 'June 2023', 'September 2023'))+
  scale_colour_manual(values = c('#eebb22', '#2288cc'))+
  scale_fill_manual(values = c('#eebb22', '#2288cc'))

Fig5f<-ggplot(subset(sr.horm, sex == 'm'), aes(timepoint, horm))+
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
  ggtitle('d')+
  scale_x_discrete(labels = c('May 2023', 'June 2023', 'September 2023'))



#Parasitic load between groups####
art<-art(prop~group+Error(timepoint),
         data = sr.prop %>%
           mutate(group = as.factor(group),
                  timepoint = as.factor(timepoint)
                  ))
anova(art)
art.con(art, 'group', adjust = 'tukey')

Fig5a<-ggplot(sr.prop, aes(factor(timepoint, levels = c('0522', '0722', '0523', '0623', '0923')), prop))+
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

#Parasitic load through time points####
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

#Parasitic load v sex steroids####

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


Fig5b<-ggplot(subset(sr.prop, group !='m_D.valentini'), aes(horm, prop))+
  geom_smooth(data = subset(sr.prop, group !='m_D.valentini' & timepoint == '0523'),
              aes(colour = factor(group, levels = c("f_D.armeniaca", "f_D.valentini"))), linewidth = 1, se = F, method = 'lm')+
  geom_smooth(data = subset(sr.prop, group !='m_D.valentini' & timepoint == '0623'),
              aes(colour = factor(group, levels = c("f_D.armeniaca", "f_D.valentini"))), linewidth = 1, se = F, method = 'lm')+
  geom_smooth(data = subset(sr.prop, group !='m_D.valentini' & timepoint == '0923'),
              aes(colour = factor(group, levels = c("f_D.armeniaca", "f_D.valentini"))), linewidth = 1, se = F, method = 'lm')+
  geom_point(aes(fill = factor(group, levels = c("f_D.armeniaca", "f_D.valentini"))), shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Progesterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('b')+
  scale_colour_manual(values = c('#eebb22', '#cc4411'))+
  scale_fill_manual(values = c('#eebb22', '#cc4411'))

Fig5c<-ggplot(subset(sr.prop, group =='m_D.valentini'), aes(horm, prop))+
  geom_smooth(data = subset(sr.prop, group =='m_D.valentini' & timepoint == '0523'),
              colour = '#44cc66', linewidth = 1, se = F, method = 'lm')+
  geom_smooth(data = subset(sr.prop, group =='m_D.valentini' & timepoint == '0623'),
              colour = '#44cc66', linewidth = 1, se = F, method = 'lm')+
  geom_smooth(data = subset(sr.prop, group =='m_D.valentini' & timepoint == '0923'),
              colour = '#44cc66', linewidth = 1, se = F, method = 'lm')+
  geom_point(fill = '#44cc66', shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Testosterone concentration, ng/mL')+
  ylab('Parasitic load')+
  ggtitle('c')




#Body condition v parasitic load####
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

Fig5d<-ggplot(sr.prop, aes(bc, prop))+
  geom_smooth(aes(colour = factor(group, levels = c("f_D.armeniaca", "f_D.valentini", "m_D.valentini"))), linewidth = 1, se = F, method = 'lm')+
  geom_point(aes(fill = factor(group, levels = c("f_D.armeniaca", "f_D.valentini", "m_D.valentini"))), shape = 21, size = 2.5, stroke = 1)+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 16, face = 'bold', hjust = .9))+
  xlab('Body condition, mm/g')+
  ylab('Parasitic load')+
  ggtitle('d')+
  scale_colour_manual(values = c('#eebb22', '#cc4411', '#44cc66'))+
  scale_fill_manual(values = c('#eebb22', '#cc4411', '#44cc66'))

#Fig5####
legend.plot<-ggplot(sr.prop, aes(svl, m))+
  geom_point(aes(fill = sex), shape = 21, size = 5.5, stroke = 1.2)+
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

legend<-get_plot_component(legend.plot, 'guide-box-top', return_all = TRUE)

ggarrange(Fig5a, Fig5b, Fig5c, Fig5d, legend,
          ncol = 1,
          align = 'hv',
          heights = c(4,4,4,4,1))
