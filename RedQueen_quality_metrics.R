library(tidyverse)

yolo_RBC_quality <- read.csv('./YOLO/metrics_out.csv')[-1, -1] %>%
  mutate(tp = as.numeric(tp),
         fp = as.numeric(fp),
         fn = as.numeric(fn),
         conf = as.numeric(conf),
         precision = tp/(tp+fp),
         recall = tp/(tp+fn),
         f1 = 2*precision*recall/(precision+recall))

yolo_par_quality <- read.csv('./YOLO2/metrics_out2.csv')[-1, -1] %>%
  mutate(tp = as.numeric(tp),
         fp = as.numeric(fp),
         fn = as.numeric(fn),
         conf = as.numeric(conf),
         precision = tp/(tp+fp),
         recall = tp/(tp+fn),
         f1 = 2*precision*recall/(precision+recall))

ggplot(yolo_RBC_quality, aes(recall, precision))+
  geom_line(colour = '#cc4a22', linewidth = 1)+
  geom_line(data = yolo_par_quality, colour = '#2277cc', linewidth = 1)+
  theme_classic()

ggplot(yolo_RBC_quality, aes(conf, f1))+
  geom_line(colour = '#cc4a22', linewidth = 1)+
  theme_classic()

ggplot(yolo_par_quality, aes(conf, f1))+
  geom_line(colour = '#2277cc', linewidth = 1)+
  theme_classic()