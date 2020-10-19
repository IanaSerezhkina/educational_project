library(dplyr)
library(ggplot2)
library(psych)

file_name <- 'Пример.csv'

df <- read.csv(file_name, sep = ";", stringsAsFactors = FALSE, header = TRUE, dec = ",")

#удаляем слои, по которым анализ не проводился

for (i in nrow(df):1){
  if (df[i, 31] == 0){
    df <- df[-i,]
  }
}

#вычисление средних/минимума/максимума, сведение их в сводные таблицы, которые потом выводим через Markdown в виде отчета

df <- df[c("Станция", "Слой", "Тыс..клеток.мл..с.учетом.разведения", "тыс..кл.мл", "биомасса.мгС.м3", "биомасса.общая.мгС.м3")]
colnames(df) <- c("Станция", "Слой", "Число клеток, тыс. кл/мл", "Суммарное число микроорганизмов, тыс. кл/мл", 
                  "Биомасса бактериопланктона, мгС/м3", "Общая биомасса бактериопланктона, мгС/м3")
mean_count <- round(mean(df$`Суммарное число микроорганизмов, тыс. кл/мл`), digits = 2)
mean_biomassa <- round(mean(df$`Общая биомасса бактериопланктона, мгС/м3`), digits = 2)
min_count <- round(min(df$`Суммарное число микроорганизмов, тыс. кл/мл`), digits = 2)
min_biomassa <- round(min(df$`Общая биомасса бактериопланктона, мгС/м3`), digits = 2)
max_count <- round(max(df$`Суммарное число микроорганизмов, тыс. кл/мл`), digits = 2)
max_biomassa <- round(max(df$`Общая биомасса бактериопланктона, мгС/м3`), digits = 2)
summary_count <- data.frame("Средняя" = mean_count, "Минимальная" = min_count, "Максимальная" =  max_count)
summary_biomassa <- data.frame("Средняя" = mean_biomassa, "Минимальная" = min_biomassa, "Максимальная" =  max_biomassa)
summary_par <- rbind(summary_count, summary_biomassa)
rownames(summary_par) <- c("Численность", "Биомасса")

df_pov <- df %>% group_by(Станция) %>% filter(Слой == "п") %>% 
  select(c(Станция, `Число клеток, тыс. кл/мл`, `Биомасса бактериопланктона, мгС/м3`)) %>% 
  summarise("Общее число микроорганизмов, тыс. кл/мл" = sum(`Число клеток, тыс. кл/мл`), "Общая биомасса бактериопланктона, мгС/м3" = sum(`Биомасса бактериопланктона, мгС/м3`))

df_don <- df %>% group_by(Станция) %>% filter(Слой == "д") %>% 
  select(c(Станция, `Число клеток, тыс. кл/мл`, `Биомасса бактериопланктона, мгС/м3`)) %>% 
  summarise("Общее число микроорганизмов, тыс. кл/мл" = sum(`Число клеток, тыс. кл/мл`), "Общая биомасса бактериопланктона, мгС/м3" = sum(`Биомасса бактериопланктона, мгС/м3`))

df_ss <- df %>% group_by(Станция) %>% filter(Слой == "сс") %>% 
  select(c(Станция, `Число клеток, тыс. кл/мл`, `Биомасса бактериопланктона, мгС/м3`)) %>% 
  summarise("Общее число микроорганизмов, тыс. кл/мл" = sum(`Число клеток, тыс. кл/мл`), "Общая биомасса бактериопланктона, мгС/м3" = sum(`Биомасса бактериопланктона, мгС/м3`))

df_pov_count <- df_pov %>% summarise("Среднее_значение" = mean(`Общее число микроорганизмов, тыс. кл/мл`),
                                     "Минимальное_значение" = min(`Общее число микроорганизмов, тыс. кл/мл`),
                                     "Максимальное_значение" = max(`Общее число микроорганизмов, тыс. кл/мл`))
df_pov_biomassa <- df_pov %>% summarise("Среднее_значение" = mean(`Общая биомасса бактериопланктона, мгС/м3`),
                                        "Минимальное_значение" = min(`Общая биомасса бактериопланктона, мгС/м3`),
                                        "Максимальное_значение" = max(`Общая биомасса бактериопланктона, мгС/м3`))
summary_pov <- data.frame(rbind(df_pov_count, df_pov_biomassa))
rownames(summary_pov) <- c("Численность микроорганизмов, тыс. кл/мл", "Биомасса бактериопланктона, мгС/м3")

df_don_count <- df_don %>% summarise("Среднее_значение" = mean(`Общее число микроорганизмов, тыс. кл/мл`),
                                     "Минимальное_значение" = min(`Общее число микроорганизмов, тыс. кл/мл`),
                                     "Максимальное_значение" = max(`Общее число микроорганизмов, тыс. кл/мл`))
df_don_biomassa <- df_don %>% summarise("Среднее_значение" = mean(`Общая биомасса бактериопланктона, мгС/м3`),
                                        "Минимальное_значение" = min(`Общая биомасса бактериопланктона, мгС/м3`),
                                        "Максимальное_значение" = max(`Общая биомасса бактериопланктона, мгС/м3`))
summary_don <- data.frame(rbind(df_don_count, df_don_biomassa))
rownames(summary_don) <- c("Численность микроорганизмов, тыс. кл/мл", "Биомасса бактериопланктона, мгС/м3")

df_ss_count <- df_ss %>% summarise("Среднее_значение" = mean(`Общее число микроорганизмов, тыс. кл/мл`),
                                   "Минимальное_значение" = min(`Общее число микроорганизмов, тыс. кл/мл`),
                                   "Максимальное_значение" = max(`Общее число микроорганизмов, тыс. кл/мл`))
df_ss_biomassa <- df_ss %>% summarise("Среднее_значение" = mean(`Общая биомасса бактериопланктона, мгС/м3`),
                                      "Минимальное_значение" = min(`Общая биомасса бактериопланктона, мгС/м3`),
                                      "Максимальное_значение" = max(`Общая биомасса бактериопланктона, мгС/м3`))
summary_ss <- data.frame(rbind(df_ss_count, df_ss_biomassa))
rownames(summary_ss) <- c("Численность микроорганизмов, тыс. кл/мл", "Биомасса бактериопланктона, мгС/м3")

#строим гистограммы распределения показателей микроорганизмов в разных слоях и сохраняем их в виде png файла

hist_pov <- ggplot(df_pov, aes(Станция, `Общее число микроорганизмов, тыс. кл/мл`)) + 
  geom_col(colour = "black", fill = "#ffcc00ff", width=0.6) + 
  scale_y_continuous(name = "ОЧБ, тыс. кл/мл", breaks = seq(0, max(df_pov$`Общее число микроорганизмов, тыс. кл/мл`), by = 50), limits = c(0, max(df_pov$`Общее число микроорганизмов, тыс. кл/мл`))) + 
  scale_x_discrete(name = "Номер станции", limits = c(1:nrow(df_pov))) +
  theme(text = element_text(size = 15), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))

png(file="Численность поверхность.png", width = 1200, height = 600, units = "px", pointsize = 12)
plot(hist_pov)
dev.off()

hist_don <- ggplot(df_don, aes(Станция, `Общее число микроорганизмов, тыс. кл/мл`)) + 
  geom_col(colour = "black", fill = "#ffcc00ff", width=0.6) + 
  scale_y_continuous(name = "ОЧБ, тыс. кл/мл", breaks = seq(0, max(df_don$`Общее число микроорганизмов, тыс. кл/мл`), by = 50), limits = c(0, max(df_don$`Общее число микроорганизмов, тыс. кл/мл`))) + 
  scale_x_discrete(name = "Номер станции", limits = c(1:nrow(df_don))) +
  theme(text = element_text(size = 15), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))

png(file="Численность дно.png", width = 1200, height = 600, units = "px", pointsize = 12)
plot(hist_don)
dev.off()

hist_ss <- ggplot(df_ss, aes(Станция, `Общее число микроорганизмов, тыс. кл/мл`)) + 
  geom_col(colour = "black", fill = "#ffcc00ff", width=0.6) + 
  scale_y_continuous(name = "ОЧБ, тыс. кл/мл", breaks = seq(0, max(df_ss$`Общее число микроорганизмов, тыс. кл/мл`), by = 50), limits = c(0, max(df_ss$`Общее число микроорганизмов, тыс. кл/мл`))) + 
  scale_x_discrete(name = "Номер станции", limits = c(1:max(df_ss$Станция))) +
  theme(text = element_text(size = 15), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))

png(file="Численность слой скачка.png", width = 1200, height = 600, units = "px", pointsize = 12)
plot(hist_ss)
dev.off()

hist_pov_b <- ggplot(df_pov, aes(Станция, `Общая биомасса бактериопланктона, мгС/м3`)) + 
  geom_col(colour = "black", fill = "#ffcc00ff", width=0.6) + 
  scale_y_continuous(name = "Значение биомассы, мгС/м3", breaks = seq(0, max(df_pov$`Общая биомасса бактериопланктона, мгС/м3`), by = 2), limits = c(0, max(df_pov$`Общая биомасса бактериопланктона, мгС/м3`))) + 
  scale_x_discrete(name = "Номер станции", limits = c(1:nrow(df_pov))) +
  theme(text = element_text(size = 15), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))

png(file="Биомасса поверхность.png", width = 1200, height = 600, units = "px", pointsize = 12)
plot(hist_pov_b)
dev.off()

hist_don_b <- ggplot(df_don, aes(Станция, `Общая биомасса бактериопланктона, мгС/м3`)) + 
  geom_col(colour = "black", fill = "#ffcc00ff", width=0.6) + 
  scale_y_continuous(name = "Значение биомассы, мгС/м3", breaks = seq(0, max(df_don$`Общая биомасса бактериопланктона, мгС/м3`), by = 2), limits = c(0, max(df_don$`Общая биомасса бактериопланктона, мгС/м3`))) + 
  scale_x_discrete(name = "Номер станции", limits = c(1:nrow(df_don))) +
  theme(text = element_text(size = 15), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))

png(file="Биомасса дно.png", width = 1200, height = 600, units = "px", pointsize = 12)
plot(hist_don_b)
dev.off()

hist_ss_b <- ggplot(df_ss, aes(Станция, `Общая биомасса бактериопланктона, мгС/м3`)) + 
  geom_col(colour = "black", fill = "#ffcc00ff", width=0.6) + 
  scale_y_continuous(name = "Значение биомассы, мгС/м3", breaks = seq(0, max(df_ss$`Общая биомасса бактериопланктона, мгС/м3`), by = 2), limits = c(0, max(df_ss$`Общая биомасса бактериопланктона, мгС/м3`))) + 
  scale_x_discrete(name = "Номер станции", limits = c(1:max(df_ss$Станция))) +
  theme(text = element_text(size = 15), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))

png(file="Биомасса слой скачка.png", width = 1200, height = 600, units = "px", pointsize = 12)
plot(hist_ss_b)
dev.off()

#преподготовка данных: отделение станций, где исследовали только 2 слоя от тех, где есть 3 слоя

df_group <- df[c("Станция", "Слой", "Число клеток, тыс. кл/мл", "Биомасса бактериопланктона, мгС/м3")]

df_3 <- df_group %>% group_by(Станция) %>% filter(Слой == "сс") %>% 
  select(c(Станция, `Число клеток, тыс. кл/мл`, `Биомасса бактериопланктона, мгС/м3`))

df_3_layers <- data.frame()
num <- c()
for (i in unique(df_3$Станция)){
  num <- c(num, which(df_group$Станция == i))
}
for (j in num){
  df_3_layers <- rbind(df_3_layers, df_group[j,])
}

df_group_3 <- df_3_layers %>% 
  mutate(Слой = factor(Слой,levels=unique(Слой))) %>% 
  group_by(Станция, Слой) %>%  
  summarise("Общее число микроорганизмов, тыс. кл/мл" = sum(`Число клеток, тыс. кл/мл`), "Общая биомасса бактериопланктона, мгС/м3" = sum(`Биомасса бактериопланктона, мгС/м3`)) 
levels(df_group_3$Слой) <- c("поверхностный", "слой скачка" , "придонный")

df_2_layers <- data.frame()
num_2 <- c(1:nrow(df_group))

for (i in 1:length(num)){
  num_2 <- num_2[-(which(num_2 == num[i]))]
}
for (k in num_2){
  df_2_layers <- rbind(df_2_layers, df_group[k,])
}

df_group_2 <- df_2_layers %>% 
  mutate(Слой = factor(Слой,levels=unique(Слой))) %>% 
  group_by(Станция, Слой) %>%  
  summarise("Общее число микроорганизмов, тыс. кл/мл" = sum(`Число клеток, тыс. кл/мл`), "Общая биомасса бактериопланктона, мгС/м3" = sum(`Биомасса бактериопланктона, мгС/м3`)) 
levels(df_group_2$Слой) <- c("поверхностный", "придонный")

# Построение гистограмм для численности

df_group_3$Группа <- 0
df_group_2$Группа <- 0
col2 <- seq(1, (nrow(df_group_2) - 1), 2)
col3 <- seq(1, (nrow(df_group_3) - 2), 3)

# Разбиение на группы тех станций, которые с тремя слоями
for (i in col3){
  if ((df_group_3[i, 3] - df_group_3[(i + 1), 3]) > 0.001 & (df_group_3[(i + 1), 3] - df_group_3[(i + 2), 3]) > 0.001){
    df_group_3$Группа[c(i, i + 1, i + 2)] <- 1
  } else if ((df_group_3[i, 3] - df_group_3[(i + 1), 3]) < 0 & (df_group_3[(i + 1), 3] - df_group_3[(i + 2), 3]) < 0){
    df_group_3$Группа[c(i, i + 1, i + 2)] <- 2
  } else if ((df_group_3[i, 3] - df_group_3[(i + 1), 3]) > 0.001 & (df_group_3[(i + 1), 3] - df_group_3[(i + 2), 3]) < 0){
    df_group_3$Группа[c(i, i + 1, i + 2)] <- 3
  } else if ((df_group_3[i, 3] - df_group_3[(i + 1), 3]) < 0 & (df_group_3[(i + 1), 3] - df_group_3[(i + 2), 3]) > 0.001){
    df_group_3$Группа[c(i, i + 1, i + 2)] <- 4
  } else {
    df_group_3$Группа[c(i, i + 1, i + 2)] <- 5
  }
}

# Разбиение на группы тех станций, которые с двумя слоями
for (i in col2){
  if (df_group_2[i, 3] > df_group_2[(i + 1), 3]){
    df_group_2$Группа[c(i, i + 1)] <- 1
  } else if (df_group_2[i, 3] < df_group_2[(i + 1), 3]){
    df_group_2$Группа[c(i, i + 1)] <- 2
  } else {
    df_group_2$Группа[c(i, i + 1)] <- 3
  }
}

# Объединение таблицы и построение гистограмм
df_group_sum <- rbind(df_group_3, df_group_2)

graf1 <- df_group_sum %>% filter(Группа == 1) %>% 
  mutate(Слой = factor(Слой,levels=unique(Слой)))
graf1$Станция <- as.factor(graf1$Станция)
graf1$Слой <- as.factor(graf1$Слой)

    graf2 <- df_group_sum %>% filter(Группа == 2) %>% 
      mutate(Слой = factor(Слой,levels=unique(Слой)))
    graf2$Станция <- as.factor(graf2$Станция)
    graf2$Слой <- as.factor(graf2$Слой)
    
        graf3 <- df_group_sum %>% filter(Группа == 3) %>% 
          mutate(Слой = factor(Слой,levels=unique(Слой)))
        graf3$Станция <- as.factor(graf3$Станция)
        graf3$Слой <- as.factor(graf3$Слой)
        
            graf4 <- df_group_sum %>% filter(Группа == 4) %>% 
              mutate(Слой = factor(Слой,levels=unique(Слой)))
            graf4$Станция <- as.factor(graf4$Станция)
            graf4$Слой <- as.factor(graf4$Слой)
            
cfu_1 <- ggplot(graf1, aes(Станция, `Общее число микроорганизмов, тыс. кл/мл`, fill = factor(Слой,levels=unique(Слой)))) + 
    geom_col(colour="black",width=0.5,    
            position=position_dodge(0.5)) +
    scale_y_continuous(name = "ОЧБ, тыс. кл/мл", breaks = seq(0, max(graf1$`Общее число микроорганизмов, тыс. кл/мл`), by = 50), limits = c(0, max(graf1$`Общее число микроорганизмов, тыс. кл/мл`))) + 
    scale_x_discrete(name = "Номер станции") +
    scale_fill_manual(values=c("#ffcc00ff", "#fff7bc", "#ffffff"), name = "Горизонт") +
  theme(text = element_text(size = 15), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))

png(file="Численность 1 группа.png", width = 1200, height = 600, units = "px", pointsize = 12)
plot(cfu_1)
dev.off()

cfu_2 <- ggplot(graf2, aes(Станция, `Общее число микроорганизмов, тыс. кл/мл`, fill = factor(Слой,levels=unique(Слой)))) + 
  geom_col(colour="black",width=0.5,    
           position=position_dodge(0.5)) +
  scale_y_continuous(name = "ОЧБ, тыс. кл/мл", breaks = seq(0, max(graf2$`Общее число микроорганизмов, тыс. кл/мл`), by = 50), limits = c(0, max(graf2$`Общее число микроорганизмов, тыс. кл/мл`))) + 
  scale_x_discrete(name = "Номер станции") +
  scale_fill_manual(values=c("#ffcc00ff", "#fff7bc", "#ffffff"), name = "Горизонт")  +
  theme(text = element_text(size = 15), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))

png(file="Численность 2 группа.png", width = 1200, height = 600, units = "px", pointsize = 12)
plot(cfu_2)
dev.off()

cfu_3 <- ggplot(graf3, aes(Станция, `Общее число микроорганизмов, тыс. кл/мл`, fill = factor(Слой,levels=unique(Слой)))) + 
  geom_col(colour="black",width=0.6,    
           position=position_dodge(0.6)) +
  scale_y_continuous(name = "ОЧБ, тыс. кл/мл", breaks = seq(0, max(graf3$`Общее число микроорганизмов, тыс. кл/мл`), by = 50), limits = c(0, max(graf3$`Общее число микроорганизмов, тыс. кл/мл`))) + 
  scale_x_discrete(name = "Номер станции") +
  scale_fill_manual(values=c("#ffcc00ff", "#fff7bc", "#ffffff"), name = "Горизонт") +
  theme(text = element_text(size = 15), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))

png(file="Численность 3 группа.png", width = 1200, height = 600, units = "px", pointsize = 12)
plot(cfu_3)
dev.off()

cfu_4 <- ggplot(graf4, aes(Станция, `Общее число микроорганизмов, тыс. кл/мл`, fill = factor(Слой,levels=unique(Слой)))) + 
  geom_col(colour="black",width=0.6,    
           position=position_dodge(0.6)) +
  scale_y_continuous(name = "ОЧБ, тыс. кл/мл", breaks = seq(0, max(graf4$`Общее число микроорганизмов, тыс. кл/мл`), by = 50), limits = c(0, max(graf4$`Общее число микроорганизмов, тыс. кл/мл`))) + 
  scale_x_discrete(name = "Номер станции") +
  scale_fill_manual(values=c("#ffcc00ff", "#fff7bc", "#ffffff"), name = "Горизонт") +
  theme(text = element_text(size = 15), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))

png(file="Численность 4 группа.png", width = 1200, height = 600, units = "px", pointsize = 12)
plot(cfu_4)
dev.off()

# Построение гистограмм для биомассы

df_group_3$Группа_био <- 0
df_group_2$Группа_био <- 0

for (i in col3){
if (df_group_3[i, 4] > df_group_3[(i + 1), 4] & df_group_3[(i + 1), 4] > df_group_3[(i + 2), 4]){
    df_group_3$Группа_био[c(i, i + 1, i + 2)] <- 1
} else if (df_group_3[i, 4] < df_group_3[(i + 1), 4] & df_group_3[(i + 1), 4] < df_group_3[(i + 2), 4]){
    df_group_3$Группа_био[c(i, i + 1, i + 2)] <- 2
  } else if (df_group_3[i, 4] > df_group_3[(i + 1), 4] & df_group_3[(i + 1), 4] < df_group_3[(i + 2), 4]){
    df_group_3$Группа_био[c(i, i + 1, i + 2)] <- 3
  } else if (df_group_3[i, 4] < df_group_3[(i + 1), 4] & df_group_3[(i + 1), 4] > df_group_3[(i + 2), 4]){
    df_group_3$Группа_био[c(i, i + 1, i + 2)] <- 4
  } else {
    df_group_3$Группа_био[c(i, i + 1, i + 2)] <- 5
  }
}


for (i in col2){
  if (df_group_2[i, 4] > df_group_2[(i + 1), 4]){
    df_group_2$Группа_био[c(i, i + 1)] <- 1
  } else if (df_group_2[i, 4] < df_group_2[(i + 1), 4]){
    df_group_2$Группа_био[c(i, i + 1)] <- 2
  } else {
    df_group_2$Группа_био[c(i, i + 1)] <- 3
  }
}
df_group_sum <- rbind(df_group_3, df_group_2)


graf_bio1 <- df_group_sum %>% filter(Группа_био == 1) %>% 
  mutate(Слой = factor(Слой,levels=unique(Слой)))
graf_bio1$Станция <- as.factor(graf_bio1$Станция)
graf_bio1$Слой <- as.factor(graf_bio1$Слой)

    graf_bio2 <- df_group_sum %>% filter(Группа_био == 2) %>% 
      mutate(Слой = factor(Слой,levels=unique(Слой)))
    graf_bio2$Станция <- as.factor(graf_bio2$Станция)
    graf_bio2$Слой <- as.factor(graf_bio2$Слой)
    
        graf_bio3 <- df_group_sum %>% filter(Группа_био == 3) %>% 
          mutate(Слой = factor(Слой,levels=unique(Слой)))
        graf_bio3$Станция <- as.factor(graf_bio3$Станция)
        graf_bio3$Слой <- as.factor(graf_bio3$Слой)
        
            graf_bio4 <- df_group_sum %>% filter(Группа_био == 4) %>% 
              mutate(Слой = factor(Слой,levels=unique(Слой)))
            graf_bio4$Станция <- as.factor(graf_bio4$Станция)
            graf_bio4$Слой <- as.factor(graf_bio4$Слой)
            
biomassa_1 <- ggplot(graf_bio1, aes(Станция, `Общая биомасса бактериопланктона, мгС/м3`, fill = factor(Слой,levels=unique(Слой)))) + 
  geom_col(colour="black", width=0.5, 
           position=position_dodge(0.5)) +
  scale_y_continuous(name = "Значение биомассы, мгС/м3", breaks = seq(0, max(graf_bio1$`Общая биомасса бактериопланктона, мгС/м3`), by = 2), limits = c(0, max(graf_bio1$`Общая биомасса бактериопланктона, мгС/м3`))) + 
  scale_x_discrete(name = "Номер станции") +
  scale_fill_manual(values=c("#ffcc00ff", "#fff7bc", "#ffffff"), name = "Горизонт") +
  theme(text = element_text(size = 15), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))

png(file="Биомасса 1 группа.png", width = 1200, height = 600, units = "px", pointsize = 12)
plot(biomassa_1)
dev.off()

biomassa_2 <- ggplot(graf_bio2, aes(Станция, `Общая биомасса бактериопланктона, мгС/м3`, fill = factor(Слой,levels=unique(Слой)))) + 
  geom_col(colour="black",width=0.5,    
           position=position_dodge(0.5)) +
  scale_y_continuous(name = "Значение биомассы, мгС/м3", breaks = seq(0, max(graf_bio2$`Общая биомасса бактериопланктона, мгС/м3`), by = 2), limits = c(0, max(graf_bio2$`Общая биомасса бактериопланктона, мгС/м3`))) + 
  scale_x_discrete(name = "Номер станции") +
  scale_fill_manual(values=c("#ffcc00ff", "#fff7bc", "#ffffff"), name = "Горизонт") +
  theme(text = element_text(size = 15), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))

png(file="Биомасса 2 группа.png", width = 1200, height = 600, units = "px", pointsize = 12)
plot(biomassa_2)
dev.off()

biomassa_3 <- ggplot(graf_bio3, aes(Станция, `Общая биомасса бактериопланктона, мгС/м3`, fill = factor(Слой,levels=unique(Слой)))) + 
  geom_col(colour="black",width=0.6,    
           position=position_dodge(0.6)) +
  scale_y_continuous(name = "Значение биомассы, мгС/м3", breaks = seq(0, max(graf_bio3$`Общая биомасса бактериопланктона, мгС/м3`), by = 2), limits = c(0, max(graf_bio3$`Общая биомасса бактериопланктона, мгС/м3`))) + 
  scale_x_discrete(name = "Номер станции") +
  scale_fill_manual(values=c("#ffcc00ff", "#fff7bc", "#ffffff"), name = "Горизонт") +
  theme(text = element_text(size = 15), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))

png(file="Биомасса 3 группа.png", width = 1200, height = 600, units = "px", pointsize = 12)
plot(biomassa_3)
dev.off()

biomassa_4 <- ggplot(graf_bio4, aes(Станция, `Общая биомасса бактериопланктона, мгС/м3`, fill = factor(Слой,levels=unique(Слой)))) + 
  geom_col(colour="black",width=0.6,    
           position=position_dodge(0.6)) +
  scale_y_continuous(name = "Значение биомассы, мгС/м3", breaks = seq(0, max(graf_bio4$`Общая биомасса бактериопланктона, мгС/м3`), by = 2), limits = c(0, max(graf_bio4$`Общая биомасса бактериопланктона, мгС/м3`))) + 
  scale_x_discrete(name = "Номер станции") +
  scale_fill_manual(values=c("#ffcc00ff", "#fff7bc", "#ffffff"), name = "Горизонт") +
  theme(text = element_text(size = 15), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))

png(file="Биомасса 4 группа.png", width = 1200, height = 600, units = "px", pointsize = 12)
plot(biomassa_4)
dev.off()


