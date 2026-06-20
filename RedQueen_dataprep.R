library(tidyverse)
library(gsheet)

RBC<-read.csv('C:/Users/nikol/OneDrive/Рабочий стол/Статьи в работе/Haemogregarina/YOLO/YOLO1.5b_RBC_main2.csv',
              header = T)

par<-read.csv('C:/Users/nikol/OneDrive/Рабочий стол/Статьи в работе/Haemogregarina/YOLO2/YOLO2.23l_paras_main2.csv',
              header = T)

info<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1iC9XA06jhmk0V_LYnQzpbVzkRmqKg5iJ_IlcHV2bFUg/edit?gid=0#gid=0')
info$id<-toupper(info$id)

horm<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1iC9XA06jhmk0V_LYnQzpbVzkRmqKg5iJ_IlcHV2bFUg/edit?gid=212620556#gid=212620556')
horm$id<-toupper(horm$id)

df<-full_join(RBC, par) %>%
  mutate(sum = RBC + hcRBC + lysRBC + paras,
         prop = paras/sum,
         id = str_replace_all(id, '__', '_')) %>%
  separate(id, c('id', 'sp', 'sex', 'date'), sep = '_') %>%
  mutate(id = toupper(id),
         timepoint = str_sub(date, 3, 6))

horm <-horm %>%
  mutate(id = str_replace_all(id, '__', '_')) %>%
  separate(id, c('id', 'timepoint'), sep = '_')
horm.unpaired<-horm %>%
  subset(is.na(timepoint)) %>%
  mutate(timepoint = NULL)
horm.paired<-horm %>%
  subset(!is.na(timepoint))


info<-info %>%
  mutate(month = str_sub(date, 4, 5),
         year = str_sub(date, 9, 10),
         timepoint = paste0(month, year),
         month = NULL,
         year = NULL)


info_horm<-info %>%
  full_join(horm.unpaired) %>%
  full_join(horm.paired, by = join_by(id, timepoint)) %>%
  mutate(horm = case_when(is.na(horm.x) ~ horm.y,
                          is.na(horm.y) ~ horm.x),
         horm.x = NULL,
         horm.y = NULL)

#write.csv(df, file = './paras_data.csv')
#| df saved to paras_data.csv,
#| adjusted manually,
#| then reimported from google sheets

df<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1iC9XA06jhmk0V_LYnQzpbVzkRmqKg5iJ_IlcHV2bFUg/edit?gid=1142253146#gid=1142253146')

df.knowndate<-subset(df, str_detect(id, 'ASH') & timepoint != 'mmyy')
df.unknowndate<-subset(df, !(str_detect(id, 'ASH') & timepoint != 'mmyy'))

df<-info_horm %>%
  full_join(df.knowndate, by = join_by(id, timepoint)) %>%
  full_join(df.unknowndate, by = join_by(id)) %>%
  mutate(sp = case_when(is.na(sp.x) & !is.na(sp.y) ~ sp.y,
                        is.na(sp.x) & is.na(sp.y) ~ sp,
                        .default = sp.x),
         sex = case_when(is.na(sex.x) & !is.na(sex.y) ~ sex.y,
                        is.na(sex.x) & is.na(sex.y) ~ sex,
                        .default = sex.x),
         date = case_when(is.na(date.x) & !is.na(date.y) ~ date.y,
                         is.na(date.x) & is.na(date.y) ~ date,
                         .default = date.x),
         RBC = case_when(is.na(RBC.x) ~ RBC.y,
                         is.na(RBC.y) ~ RBC.x),
         hcRBC = case_when(is.na(hcRBC.x) ~ hcRBC.y,
                           is.na(hcRBC.y) ~ hcRBC.x),
         lysRBC = case_when(is.na(lysRBC.x) ~ lysRBC.y,
                            is.na(lysRBC.y) ~ lysRBC.x),
         paras = case_when(is.na(paras.x) ~ paras.y,
                           is.na(paras.y) ~ paras.x),
         prop = case_when(is.na(prop.x) ~ prop.y,
                          is.na(prop.y) ~ prop.x),
         RBC.x = NULL,
         RBC.y = NULL,
         hcRBC.x = NULL,
         hcRBC.y = NULL,
         lysRBC.x = NULL,
         lysRBC.y = NULL,
         paras.x = NULL,
         paras.y = NULL,
         prop.x = NULL,
         prop.y = NULL,
         comment = NULL,
         gene_sample_dry = NULL,
         gene_sample_etoh = NULL,
         sp.x = NULL,
         sp.y = NULL,
         sex.x = NULL,
         sex.y = NULL,
         date.x = NULL,
         date.y = NULL,
         sum.x = NULL,
         sum.y = NULL,
         timepoint.x = NULL,
         timepoint.y = NULL) %>%
  subset(!is.na(sp))

write.csv(df, file = './RedQueen_data.csv')
