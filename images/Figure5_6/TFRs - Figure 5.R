library(tidyverse)


tfr <- read_csv("TFRs.csv", col_names = c("country", "education_level", "Non-considering pandemic", "Considering pandemic"), skip = 1)

tfr$country <- str_replace_all(tfr$country, pattern = "BRA", replacement = "Brazil")
tfr$country <- str_replace_all(tfr$country, pattern = "CHI", replacement = "Chile")
tfr$country <- str_replace_all(tfr$country, pattern = "CRC", replacement = "Costa Rica")
tfr$country <- str_replace_all(tfr$country, pattern = "MEX", replacement = "Mexico")

tfr$country <- as.factor(tfr$country)
tfr <- pivot_longer(tfr, 3:4, names_to = "pandemic", values_to = "tfr")
tfr$education_level <- factor(tfr$education_level, levels = c("Less or until 7", "8-11 years", "12 or more years"))
tfr$pandemic <- factor(tfr$pandemic, levels = c("Non-considering pandemic", "Considering pandemic"))

tfr1<-tfr %>% ggplot(aes(x = education_level, y = tfr)) +
  geom_bar(stat = "identity", aes(fill = pandemic), position = "dodge", color = "#8f999b") +
  geom_hline(yintercept = 2, linetype = "dashed") +
  labs(x = "Years of study", y = "Rate", fill = "TFR") +
  facet_grid(~ country, switch = "x") +
  scale_fill_manual(values = c("#00008b", "#9ac0cd")) +
  theme_bw()


tfr2<-tfr %>% ggplot(aes(x = education_level, y = tfr)) +
  geom_bar(stat = "identity", aes(fill = pandemic), position = "dodge", color = "#8f999b") +
  geom_hline(yintercept = 2, linetype = "dashed") +
  labs(x = "Years of study", y = "Rate", fill = "TFR") +
  facet_grid(~ country, switch = "x") +
  scale_fill_manual(values = c("#00008b", "#9ac0cd")) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


tfr3<-tfr %>% ggplot(aes(x = education_level, y = tfr)) +
  geom_bar(stat = "identity", aes(fill = pandemic), position = "dodge", color = "#8f999b") +
  geom_hline(yintercept = 2, linetype = "dashed") +
  labs(x = "Years of study", y = "Rate", fill = "TFR") +
  facet_grid(~ country, switch = "x") +
  scale_fill_manual(values = c("#00008b", "#9ac0cd")) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### Save images------------------------------------

# Save in png
ggsave("Figure5_TFR_version1.png", tfr1, width = 13, height = 6.5, units = "in", bg = "white")
ggsave("Figure5_TFR_Version2.png", tfr2, width = 10, height = 6, units = "in", bg = "white")
ggsave("Figure5_TFR_Version3.png", tfr3, width = 8, height = 5.25, units = "in", bg = "white")

#Save in EPS format
ggsave("Figure5_TFR_Version1.eps", tfr1, width = 13, height = 6.5, units = "in", bg = "white",device="eps")
ggsave("Figure5_TFR_Version2.eps", tfr2, width = 10, height = 6, units = "in", bg = "white",device="eps")
ggsave("Figure5_TFR_Version3.eps", tfr3, width = 8, height = 5.25, units = "in", bg = "white",device="eps")

#Save in pdf
ggsave("Figure5_TFR_Version1.pdf", tfr1, width = 13, height = 6.5, units = "in", bg = "white",device="pdf")
ggsave("Figure5_TFR_Version2.pdf", tfr2, width = 10, height = 6, units = "in", bg = "white",device="pdf")
ggsave("Figure5_TFR_Version3.pdf", tfr3, width = 8, height = 5.25, units = "in", bg = "white",device="pdf")

