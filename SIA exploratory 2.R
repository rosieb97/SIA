###Packages

library(dplyr)
library(patchwork)
library(ggplot2)
library(broom)
library(lubridate)

##Visualisations 

my_theme <- theme_minimal() +
  theme(
    axis.title.x = element_text(size = 30, margin = margin(t = 20)),
    axis.title.y = element_text(size = 30, margin = margin(r = 20)),
    axis.text.x = element_text(size = 24),
    axis.text.y = element_text(size = 24),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.title = element_text(size = 30, face = "bold"),
    legend.text = element_text(size = 24),
    strip.text = element_text(size = 30, face = "bold"),
    axis.line = element_line(colour = "black", linewidth = 0.8)) 

cbf <- c("orange", "#56B4E9", "#009E73", "#F0E442","pink")

##Upload lab outputs

setwd("~/R/SIA analyses/Prelim")
lab_outputs<-read.csv("Lab_outputs.csv",header=TRUE)
head(lab_outputs)
lab_outputs <- lab_outputs[, 1:(ncol(lab_outputs) - 4)]

##Upload metadata

metadata<-read.csv("Metadata.csv",header=TRUE)
head(metadata)
metadata <- metadata[, 1:(ncol(metadata) - 88)]
  
##Combine

lab_outputs <- lab_outputs %>%
  mutate(Sample_ID = case_when(
    Sample_type == "Blood" ~ sub("^RBC([0-9]+)[a-zA-Z]$", "\\1", Sample_name), 
    Sample_type == "Skin"  ~ Sample_name,                       
    TRUE ~ Sample_name
  ))

blood_labs <- lab_outputs %>%
  filter(Sample_type == "Blood") %>%
  mutate(Sample_ID = as.integer(Sample_ID)) %>%
  left_join(metadata %>% select(Blood_RB, Sex, Life_stage, Date),
            by = c("Sample_ID" = "Blood_RB")) %>%
  mutate(Sample_ID = as.character(Sample_ID))

skin_labs <- lab_outputs %>%
  filter(Sample_type == "Skin") %>%
  left_join(metadata %>% select(Biopsy_no, Genetics_no, Sex, Life_stage, Date) %>%
              pivot_longer(c(Biopsy_no, Genetics_no), values_to = "Sample_ID") %>%
              select(-name),
            by = "Sample_ID")

SIA_full <- bind_rows(blood_labs, skin_labs) %>%
  select(-Sample_ID)

SIA_full$Life_stage

SIA_full <- SIA_full %>%
  mutate(
    Month = month(Date),
    Season = if_else(Month %in% 11:12 | Month %in% 1:4, "Wet", "Dry")
  )

head(SIA_full)

###Visualise data

SIA_full %>%
  group_by(Sex, Season) %>%
  summarise(
    mean_C = mean(C, na.rm = TRUE),
    sd_C = sd(C, na.rm = TRUE),
    mean_N = mean(N, na.rm = TRUE),
    sd_N = sd(N, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

##First with biopsies

SIA_biopsy<-SIA_full[SIA_full$Sample_type=="Skin",]

#By season
ggplot(SIA_biopsy, aes(x = Season, y = C, fill = Season)) +
  geom_boxplot() +
  labs(title = "δ13C by season", x = "Season", y = "δ13C") +
  theme_minimal() +
  theme(legend.position = "none")
#higher C in wet season

ggplot(SIA_biopsy, aes(x = Season, y = N, fill = Season)) +
  geom_boxplot() +
  labs(title = "δ15N by season", x = "Season", y = "δ15N") +
  theme_minimal() +
  theme(legend.position = "none")
#lower N in wet season

#By sex and season
ggplot(SIA_biopsy, aes(x = Sex, y = C, fill = Sex)) +
  geom_boxplot() +
  facet_wrap(~Season) +
  labs(title = "δ13C by Sex and Season", x = "Sex", y = "δ13C") +
  theme_minimal()
#females lower C in dry season? Both lower C in dry

ggplot(SIA_biopsy, aes(x = Sex, y = N, fill = Sex)) +
  geom_boxplot() +
  facet_wrap(~Season) +
  labs(title = "δ15N by sex and season", x = "Sex", y = "δ15N") +
  theme_minimal()
#females lower N in wet season

##Now with bloods

head(SIA_full)
SIA_bloods<-SIA_full[SIA_full$Sample_type=="Blood",]

#By season
ggplot(SIA_bloods, aes(x = Season, y = C, fill = Season)) +
  geom_boxplot() +
  labs(title = "δ13C by season", x = "Season", y = "δ13C") +
  theme_minimal() +
  theme(legend.position = "none")
#similar

ggplot(SIA_bloods, aes(x = Season, y = N, fill = Season)) +
  geom_boxplot() +
  labs(title = "δ15N by season", x = "Season", y = "δ15N") +
  theme_minimal() +
  theme(legend.position = "none")
#lower N in wet season


#By sex and season
ggplot(SIA_bloods, aes(x = Sex, y = C, fill = Sex)) +
  geom_boxplot() +
  facet_wrap(~Season) +
  labs(title = "δ13C by Sex and Season", x = "Sex", y = "δ13C") +
  theme_minimal()
#females lower C in dry season

ggplot(SIA_bloods, aes(x = Sex, y = N, fill = Sex)) +
  geom_boxplot() +
  facet_wrap(~Season) +
  labs(title = "δ15N by sex and season", x = "Sex", y = "δ15N") +
  theme_minimal()
#females lower N in wet season, generally lower than males




##More complex visualisations

head(SIA_full)
SIA_full %>%
  group_by(Sample_type, Sex, Season) %>%
  summarise(
    mean_C = round(mean(C, na.rm = TRUE), 2),
    sd_C   = round(sd(C, na.rm = TRUE), 2),
    mean_N = round(mean(N, na.rm = TRUE), 2),
    sd_N   = round(sd(N, na.rm = TRUE), 2),
    n      = n(),
    .groups = "drop"
  )


##BIOPSIES
SIA_full$Date <- as.Date(SIA_full$Date, format = "%d/%m/%Y")
SIA_full$Year <- year(SIA_full$Date)

C_summary <- SIA_full %>%
  group_by(Year, Season) %>%
  summarise(mean_C = mean(C, na.rm = TRUE),
            se_C = sd(C, na.rm = TRUE) / sqrt(n()),
            n = n()) %>%
  ungroup()

ggplot(C_summary, aes(x = Year, y = mean_C, colour = Season, group = Season)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = mean_C - se_C, ymax = mean_C + se_C), width = 0.3, alpha = 0.6)+
  scale_colour_manual(values = c("#4A90C4", "darkgreen")) +
  labs(
    x = "Year",
    y = "Mean dC",
    colour = "Season"
  ) +
  theme_bw() + my_theme


N_summary <- SIA_full %>%
  group_by(Year, Season) %>%
  summarise(mean_N = mean(N, na.rm = TRUE),
            se_N = sd(N, na.rm = TRUE) / sqrt(n()),
            n = n()) %>%
  ungroup()

ggplot(N_summary, aes(x = Year, y = mean_N, colour = Season, group = Season)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = mean_N - se_N, ymax = mean_N + se_N), width = 0.3, alpha = 0.6)+
  scale_colour_manual(values = c("#4A90C4", "darkgreen")) +
  labs(
    x = "Year",
    y = "Mean dN",
    colour = "Season"
  ) +
  theme_bw() +my_theme


##Short-term trends

#By sex, season and sample type

SIA_full %>%
  pivot_longer(cols = c(C, N), names_to = "Isotope", values_to = "Value") %>%
  mutate(Isotope = recode(Isotope, C = "δ13C", N = "δ15N")) %>%
  ggplot(aes(x = Sex, y = Value, fill = Season)) +
  geom_boxplot(position = position_dodge(0.8), outlier.size = 1) +
  facet_wrap(~ Sample_type + Isotope, ncol= 4, scales="free_y") +
  labs(x = "Sex / Lifestage", y = "Isotope Value", fill = "Season") +
  scale_fill_manual(values=c("#4A90C4","darkgreen"))+
  my_theme


p_c <- SIA_full %>%
  pivot_longer(cols = c(C, N), names_to = "Isotope", values_to = "Value") %>%
  mutate(Isotope = recode(Isotope, C = "δ13C", N = "δ15N")) %>%
  filter(Isotope == "δ13C") %>%
  ggplot(aes(x = Sex, y = Value, fill = Season)) +
  geom_boxplot(position = position_dodge(0.8), outlier.size = 1) +
  facet_wrap(~ Sample_type, ncol = 4) +
  labs(x = "Sex", y = "δ13C", fill = "Season") +
  scale_fill_manual(values = c("#4A90C4", "darkgreen")) +
  my_theme+theme(
    legend.position="none"
  )

p_n <- SIA_full %>%
  pivot_longer(cols = c(C, N), names_to = "Isotope", values_to = "Value") %>%
  mutate(Isotope = recode(Isotope, C = "δ13C", N = "δ15N")) %>%
  filter(Isotope == "δ15N") %>%
  ggplot(aes(x = Sex, y = Value, fill = Season)) +
  geom_boxplot(position = position_dodge(0.8), outlier.size = 1) +
  facet_wrap(~ Sample_type, ncol = 4) +
  labs(x = "Sex", y = "δ15N", fill = "Season") +
  scale_fill_manual(values = c("#4A90C4", "darkgreen")) +
  my_theme

p_c + p_n

#As above with only PhD data

SIA_full_RB<-SIA_full[SIA_full$Date > "2020-02-22",]
head(SIA_full_RB)

p_c_1 <- SIA_full_RB %>%
  pivot_longer(cols = c(C, N), names_to = "Isotope", values_to = "Value") %>%
  mutate(Isotope = recode(Isotope, C = "δ13C", N = "δ15N")) %>%
  filter(Isotope == "δ13C") %>%
  ggplot(aes(x = Sex, y = Value, fill = Season)) +
  geom_boxplot(position = position_dodge(0.8), outlier.size = 1) +
  facet_wrap(~ Sample_type, ncol = 4) +
  labs(x = "Sex", y = "δ13C", fill = "Season") +
  scale_fill_manual(values = c("#4A90C4", "darkgreen")) +
  my_theme+theme(
    legend.position="none"
  )

p_n_1 <- SIA_full_RB %>%
  pivot_longer(cols = c(C, N), names_to = "Isotope", values_to = "Value") %>%
  mutate(Isotope = recode(Isotope, C = "δ13C", N = "δ15N")) %>%
  filter(Isotope == "δ15N") %>%
  ggplot(aes(x = Sex, y = Value, fill = Season)) +
  geom_boxplot(position = position_dodge(0.8), outlier.size = 1) +
  facet_wrap(~ Sample_type, ncol = 4) +
  labs(x = "Sex", y = "δ15N", fill = "Season") +
  scale_fill_manual(values = c("#4A90C4", "darkgreen")) +
  my_theme

p_c_1 + p_n_1


###More advanced

##Long-term data with biopsies

#First exploring biopsy data
#Scatter by sex
first.plot<-ggplot(data = SIA_biopsy, 
                   mapping = aes(x = C, 
                                 y = N)) + 
  geom_point(aes(color = Sex, shape = Sex), size = 5) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  theme(text = element_text(size=20))
print(first.plot)

classic.first.plot <- first.plot + theme_classic() + 
  theme(text = element_text(size=35)) + 
  coord_equal() + 
  theme(axis.ticks.length = unit(-0.2, "cm")) +
  scale_colour_viridis_d(end = 0.9)
print(classic.first.plot)

fbmeans <- SIA_biopsy %>% 
  group_by(Sex) %>% 
  summarise(count = n(),
            mC = mean(C), 
            sdC = sd(C), 
            mN = mean(N), 
            sdN = sd(N) )
print(fbmeans)

second.plot <- first.plot + 
  geom_errorbar(data = fbmeans, 
                mapping = aes(x = mC, y = mN,
                              ymin = mN - 1.96*sdN, 
                              ymax = mN + 1.96*sdN), 
                width = 0)+
  geom_errorbarh(data = fbmeans, 
                 mapping = aes(x = mC, y = mN,
                               xmin = mC - 1.96*sdC,
                               xmax = mC + 1.96*sdC),
                 height = 0) + 
  geom_point(data = fbmeans, aes(x = mC, 
                                 y = mN,
                                 fill = Sex), 
             color = "black", shape = 22, size = 5,
             alpha = 0.7, show.legend = FALSE) + 
  coord_equal()
print(second.plot)

p.ell <- 0.95

ellipse.plot3 <- first.plot + 
  stat_ellipse(aes(group =Sex, 
                   fill = Sex, 
                   color = Sex), 
               alpha = 0.25, 
               level = p.ell,
               type = "t",
               geom = "polygon")
print(ellipse.plot3)

#Scatter by season

first.plot <- ggplot(data = SIA_biopsy, 
                     mapping = aes(x = C, 
                                   y = N)) + 
  geom_point(aes(color = Season, shape = Season), size = 5) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  theme(text = element_text(size=20))
print(first.plot)

classic.first.plot <- first.plot + theme_classic() + 
  theme(text = element_text(size=35)) + 
  coord_equal() + 
  theme(axis.ticks.length = unit(-0.2, "cm")) +
  scale_colour_viridis_d(end = 0.9)
print(classic.first.plot)

fbmeans <- SIA_biopsy %>% 
  group_by(Season) %>% 
  summarise(count = n(),
            mC = mean(C), 
            sdC = sd(C), 
            mN = mean(N), 
            sdN = sd(N) )
print(fbmeans)

second.plot <- first.plot + 
  geom_errorbar(data = fbmeans, 
                mapping = aes(x = mC, y = mN,
                              ymin = mN - 1.96*sdN, 
                              ymax = mN + 1.96*sdN), 
                width = 0)+
  geom_errorbarh(data = fbmeans, 
                 mapping = aes(x = mC, y = mN,
                               xmin = mC - 1.96*sdC,
                               xmax = mC + 1.96*sdC),
                 height = 0) + 
  geom_point(data = fbmeans, aes(x = mC, 
                                 y = mN,
                                 fill = Season), 
             color = "black", shape = 22, size = 5,
             alpha = 0.7, show.legend = FALSE) + 
  coord_equal()

print(second.plot)

p.ell <- 0.95

ellipse.plot <- first.plot + 
  stat_ellipse(aes(group = Season, 
                   fill = Season, 
                   color = Season), 
               alpha = 0.25, 
               level = p.ell,
               type = "norm",
               geom = "polygon")
print(ellipse.plot)

#Scatter by season and sex

SIA_biopsy$SS<-paste(SIA_biopsy$Season,SIA_biopsy$Sex)
SIA_biopsy$SS<-as.factor(SIA_biopsy$SS)
first.plot <- ggplot(data = SIA_biopsy, 
                     mapping = aes(x = C, 
                                   y = N)) + 
  geom_point(aes(color = SS, shape = SS), size = 5) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  theme(text = element_text(size=20))
print(first.plot)

classic.first.plot <- first.plot + theme_classic() + 
  theme(text = element_text(size=35)) + 
  coord_equal() + 
  theme(axis.ticks.length = unit(-0.2, "cm")) +
  scale_colour_viridis_d(end = 0.9)
print(classic.first.plot)

fbmeans <- SIA_biopsy %>% 
  group_by(SS) %>% 
  summarise(count = n(),
            mC = mean(C), 
            sdC = sd(C), 
            mN = mean(N), 
            sdN = sd(N) )
print(fbmeans)

second.plot <- first.plot + 
  geom_errorbar(data = fbmeans, 
                mapping = aes(x = mC, y = mN,
                              ymin = mN - 1.96*sdN, 
                              ymax = mN + 1.96*sdN), 
                width = 0)+
  geom_errorbarh(data = fbmeans, 
                 mapping = aes(x = mC, y = mN,
                               xmin = mC - 1.96*sdC,
                               xmax = mC + 1.96*sdC),
                 height = 0) + 
  geom_point(data = fbmeans, aes(x = mC, 
                                 y = mN,
                                 fill = SS), 
             color = "black", shape = 22, size = 5,
             alpha = 0.7, show.legend = FALSE) + 
  coord_equal()
print(second.plot)

p.ell <- 0.95

ellipse.plot <- first.plot + 
  stat_ellipse(aes(group = SS, 
                   fill = SS, 
                   color = SS), 
               alpha = 0.25, 
               level = p.ell,
               type = "norm",
               geom = "polygon")
print(ellipse.plot)

#By year
class(SIA_biopsy$Date)
SIA_biopsy$Date <- as.Date(SIA_biopsy$Date, format = "%d/%m/%Y")
SIA_biopsy$Year <- year(SIA_biopsy$Date)

SIA_biopsy$Year<-as.factor(SIA_biopsy$Year)
first.plot <- ggplot(data = SIA_biopsy, 
                     mapping = aes(x = C, 
                                   y = N)) + 
  geom_point(aes(color = Year, shape = Year), size = 5) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  theme(text = element_text(size=20))
print(first.plot)

classic.first.plot <- first.plot + theme_classic() + 
  theme(text = element_text(size=35)) + 
  coord_equal() + 
  theme(axis.ticks.length = unit(-0.2, "cm")) +
  scale_colour_viridis_d(end = 0.9)
print(classic.first.plot)

fbmeans <- SIA_biopsy %>% 
  group_by(Year) %>% 
  summarise(count = n(),
            mC = mean(C), 
            sdC = sd(C), 
            mN = mean(N), 
            sdN = sd(N) )
print(fbmeans)

second.plot <- first.plot + 
  geom_errorbar(data = fbmeans, 
                mapping = aes(x = mC, y = mN,
                              ymin = mN - 1.96*sdN, 
                              ymax = mN + 1.96*sdN), 
                width = 0)+
  geom_errorbarh(data = fbmeans, 
                 mapping = aes(x = mC, y = mN,
                               xmin = mC - 1.96*sdC,
                               xmax = mC + 1.96*sdC),
                 height = 0) + 
  geom_point(data = fbmeans, aes(x = mC, 
                                 y = mN,
                                 fill = Year), 
             color = "black", shape = 22, size = 5,
             alpha = 0.7, show.legend = FALSE) + 
  coord_equal()
print(second.plot)

p.ell <- 0.5

ellipse.plot <- first.plot + 
  stat_ellipse(aes(group = Year, 
                   fill = Year, 
                   color = Year), 
               alpha = 0.25, 
               level = p.ell,
               type = "norm",
               geom = "polygon")
print(ellipse.plot)

SIA_biopsy_long <- SIA_biopsy %>%
  pivot_longer(cols = c(C, N),
               names_to = "isotope",
               values_to = "value") %>%
  mutate(isotope = recode(isotope,
                          "C" = "δ¹³C (‰)",
                          "N" = "δN (‰)"))

ggplot(SIA_biopsy_long, aes(x = Year, y = value, fill = Year)) +
  geom_jitter(aes(color = Year), 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
              alpha = 0.9, size = 1.5) +
  geom_boxplot(alpha=0.5,outlier.shape=NA) +
  facet_wrap(~ isotope, scales = "free_y") +
  scale_fill_manual(values = cbf) +
  scale_color_manual(values = cbf) + 
  theme_bw(base_size = 16) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Year", y = NULL)

#by season as well

ggplot(SIA_biopsy_long, aes(x = Year, y = value, fill = Season)) +
  geom_jitter(aes(color = Season), 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
              alpha = 0.9, size = 1.5) +
  geom_boxplot(alpha=0.5,outlier.shape=NA) +
  facet_wrap(~ isotope, scales = "free_y") +
  scale_fill_manual(values = cbf) +
  scale_color_manual(values = cbf) + 
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Year", y = NULL, fill = "Season")

##Short-term data with bloods

#By sex

first.plot <- ggplot(data = SIA_bloods, 
                     mapping = aes(x = C, 
                                   y = N)) + 
  geom_point(aes(color = Sex, shape = Sex), size = 5) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  theme(text = element_text(size=20))
print(first.plot)

classic.first.plot <- first.plot + theme_classic() + 
  theme(text = element_text(size=35)) + 
  coord_equal() + 
  theme(axis.ticks.length = unit(-0.2, "cm")) +
  scale_colour_viridis_d(end = 0.9)
print(classic.first.plot)

fbmeans <- SIA_bloods %>% 
  group_by(Sex) %>% 
  summarise(count = n(),
            mC = mean(C), 
            sdC = sd(C), 
            mN = mean(N), 
            sdN = sd(N) )
print(fbmeans)

second.plot <- first.plot + 
  geom_errorbar(data = fbmeans, 
                mapping = aes(x = mC, y = mN,
                              ymin = mN - 1.96*sdN, 
                              ymax = mN + 1.96*sdN), 
                width = 0)+
  geom_errorbarh(data = fbmeans, 
                 mapping = aes(x = mC, y = mN,
                               xmin = mC - 1.96*sdC,
                               xmax = mC + 1.96*sdC),
                 height = 0) + 
  geom_point(data = fbmeans, aes(x = mC, 
                                 y = mN,
                                 fill = Sex), 
             color = "black", shape = 22, size = 5,
             alpha = 0.7, show.legend = FALSE) + 
  coord_equal()
print(second.plot)

p.ell <- 0.95

ellipse.plot3 <- first.plot + 
  stat_ellipse(aes(group =Sex, 
                   fill = Sex, 
                   color = Sex), 
               alpha = 0.25, 
               level = p.ell,
               type = "t",
               geom = "polygon")
print(ellipse.plot3)

#By season

first.plot <- ggplot(data = SIA_bloods, 
                     mapping = aes(x = C, 
                                   y = N)) + 
  geom_point(aes(color = Season, shape = Season), size = 5) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  theme(text = element_text(size=20))
print(first.plot)

classic.first.plot <- first.plot + theme_classic() + 
  theme(text = element_text(size=35)) + 
  coord_equal() + 
  theme(axis.ticks.length = unit(-0.2, "cm")) +
  scale_colour_viridis_d(end = 0.9)
print(classic.first.plot)

fbmeans <- SIA_bloods %>% 
  group_by(Season) %>% 
  summarise(count = n(),
            mC = mean(C), 
            sdC = sd(C), 
            mN = mean(N), 
            sdN = sd(N) )
print(fbmeans)

second.plot <- first.plot + 
  geom_errorbar(data = fbmeans, 
                mapping = aes(x = mC, y = mN,
                              ymin = mN - 1.96*sdN, 
                              ymax = mN + 1.96*sdN), 
                width = 0)+
  geom_errorbarh(data = fbmeans, 
                 mapping = aes(x = mC, y = mN,
                               xmin = mC - 1.96*sdC,
                               xmax = mC + 1.96*sdC),
                 height = 0) + 
  geom_point(data = fbmeans, aes(x = mC, 
                                 y = mN,
                                 fill = Season), 
             color = "black", shape = 22, size = 5,
             alpha = 0.7, show.legend = FALSE) + 
  coord_equal()
print(second.plot)

p.ell <- 0.95

ellipse.plot <- first.plot + 
  stat_ellipse(aes(group = Season, 
                   fill = Season, 
                   color = Season), 
               alpha = 0.25, 
               level = p.ell,
               type = "norm",
               geom = "polygon")

print(ellipse.plot)

#By season and sex

SIA_bloods$SS<-paste(SIA_bloods$Season,SIA_bloods$Sex)
SIA_bloods$SS<-as.factor(SIA_bloods$SS)

SIA_bloods_long<-SIA_bloods %>%
  pivot_longer(cols = c(C, N),
               names_to = "isotope",
               values_to = "value") %>%
  mutate(isotope = recode(isotope,
                          "C" = "δ¹³C (‰)",
                          "N" = "δN (‰)"))

ggplot(SIA_bloods_long, aes(x = Sex, y = value, fill = Season)) +
  geom_boxplot() +
  facet_wrap(~ isotope, scales = "free_y") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") +
  labs(x = "Year", y = NULL)

first.plot <- ggplot(data = SIA_bloods, 
                     mapping = aes(x = C, 
                                   y = N)) + 
  geom_point(aes(color = SS, shape = SS), size = 5) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  theme(text = element_text(size=20))
print(first.plot)

classic.first.plot <- first.plot + theme_classic() + 
  theme(text = element_text(size=35)) + 
  coord_equal() + 
  theme(axis.ticks.length = unit(-0.2, "cm")) +
  scale_colour_viridis_d(end = 0.9)
print(classic.first.plot)

fbmeans <- SIA_bloods %>% 
  group_by(SS) %>% 
  summarise(count = n(),
            mC = mean(C), 
            sdC = sd(C), 
            mN = mean(N), 
            sdN = sd(N) )
print(fbmeans)

second.plot <- first.plot + 
  geom_errorbar(data = fbmeans, 
                mapping = aes(x = mC, y = mN,
                              ymin = mN - 1.96*sdN, 
                              ymax = mN + 1.96*sdN), 
                width = 0)+
  geom_errorbarh(data = fbmeans, 
                 mapping = aes(x = mC, y = mN,
                               xmin = mC - 1.96*sdC,
                               xmax = mC + 1.96*sdC),
                 height = 0) + 
  geom_point(data = fbmeans, aes(x = mC, 
                                 y = mN,
                                 fill = SS), 
             color = "black", shape = 22, size = 5,
             alpha = 0.7, show.legend = FALSE) + 
  coord_equal()

print(second.plot)

p.ell <- 0.95

SIA_bloods$SS
first.plot <- ggplot(SIA_bloods, aes(x = C, y = N,
                                     colour = SS,
                                     shape = SS)) +
  geom_point(size = 4) +
  scale_colour_manual(values = c("Dry F" = "#E8799E",
                                 "Wet F" = "#E8799E",
                                 "Dry M" = "#56B4E9",
                                 "Wet M" = "#56B4E9")) +
  scale_shape_manual(values = c("Dry F" = 17,   # triangle
                                "Wet F" = 16,   # circle
                                "Dry M" = 17,   # triangle
                                "Wet M" = 16))  # circle

ellipse.plot <- first.plot + 
  stat_ellipse(aes(group = SS, 
                   colour = SS,
                   fill = SS,
                   linetype = SS), 
               alpha = 0.05,
               size = 2,
               level = p.ell,
               type = "norm",
               geom = "polygon") +
  scale_shape_manual(name = "Season/Sex",
                     values = c("Dry F" = 1,    # open circle
                                "Wet F" = 16,   # filled circle
                                "Dry M" = 1,    # open circle
                                "Wet M" = 16),  # filled circle
                     labels = c("Dry F" = "Dry / Female",
                                "Wet F" = "Wet / Female",
                                "Dry M" = "Dry / Male",
                                "Wet M" = "Wet / Male"))+
  scale_colour_manual(name = "Season/Sex",
                      values = c("Dry F" = "#E8799E",
                                 "Wet F" = "#E8799E",
                                 "Dry M" = "#56B4E9",
                                 "Wet M" = "#56B4E9"),
                      labels = c("Dry F" = "Dry / Female",
                                 "Wet F" = "Wet / Female",
                                 "Dry M" = "Dry / Male",
                                 "Wet M" = "Wet / Male")) +
  scale_fill_manual(name = "Season/Sex",
                    values = c("Dry F" = "#E8799E",
                               "Wet F" = "#E8799E",
                               "Dry M" = "#56B4E9",
                               "Wet M" = "#56B4E9"),
                    labels = c("Dry F" = "Dry / Female",
                               "Wet F" = "Wet / Female",
                               "Dry M" = "Dry / Male",
                               "Wet M" = "Wet / Male")) +
  scale_linetype_manual(name = "Season/Sex",
                        values = c("Dry F" = "dashed",
                                   "Wet F" = "solid",
                                   "Dry M" = "dashed",
                                   "Wet M" = "solid"),
                        labels = c("Dry F" = "Dry / Female",
                                   "Wet F" = "Wet / Female",
                                   "Dry M" = "Dry / Male",
                                   "Wet M" = "Wet / Male")) +
  my_theme +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))+
  guides(shape = guide_legend(override.aes = list(size = 4)),
         colour = guide_legend(override.aes = list(linewidth = 1.5, size = 4)),
         linetype = guide_legend(override.aes = list(linewidth = 1.5, size = 4)),
         fill = "none")
print(ellipse.plot)


SIA_bloods$Season <- ifelse(grepl("Dry", SIA_bloods$SS), "Dry", "Wet")

first.plot <- ggplot(SIA_bloods, aes(x = C, y = N,
                                     colour = SS,
                                     shape = Season)) +
  geom_point(size = 4) +
  scale_colour_manual(guide = "none",
                      values = c("Dry F" = "#E8799E",
                                 "Wet F" = "#E8799E",
                                 "Dry M" = "#56B4E9",
                                 "Wet M" = "#56B4E9")) +
  scale_shape_manual(name = "Season",
                     values = c("Dry" = 1,
                                "Wet" = 16))

ellipse.plot <- first.plot + 
  stat_ellipse(aes(group = SS, 
                   colour = SS,
                   fill = SS,
                   linetype = Season), 
               alpha = 0.05,
               size = 2,
               level = p.ell,
               type = "norm",
               geom = "polygon") +
  scale_colour_manual(guide = "none",
                      values = c("Dry F" = "#E8799E",
                                 "Wet F" = "#E8799E",
                                 "Dry M" = "#56B4E9",
                                 "Wet M" = "#56B4E9")) +
  scale_fill_manual(guide = "none",
                    values = c("Dry F" = "#E8799E",
                               "Wet F" = "#E8799E",
                               "Dry M" = "#56B4E9",
                               "Wet M" = "#56B4E9")) +
  scale_linetype_manual(name = "Season",
                        values = c("Dry" = "dashed",
                                   "Wet" = "solid")) +
  guides(shape = guide_legend(override.aes = list(colour = "black", size = 5),
                              key_height = unit(1.5, "cm"),
                              key_width = unit(1.5, "cm")),
         linetype = guide_legend(override.aes = list(colour = "black", linewidth = 0.5),
                                 key_height = unit(1, "cm"),
                                 key_width = unit(1, "cm")))+
  my_theme +theme(legend.spacing.y = unit(1, "cm"),
                  legend.key.height = unit(1, "cm")) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))

print(ellipse.plot)

image=ellipse.plot
ggsave(file="test 2.svg", plot=image, width=10, height=8,device="svg")


#only with RB's data

means <- SIA_full_RB %>%
  group_by(Sex, Season, Sample_type) %>%
  summarise(count = n(),
            mC = mean(C), sdC = sd(C),
            mN = mean(N), sdN = sd(N),
            .groups = "drop")

ggplot(SIA_full_RB, aes(x = C, y = N, color = Season)) +
  geom_point(size = 3, alpha = 0.4) +
  # error bars on means
  geom_errorbar(data = means,
                aes(x = mC, y = mN,
                    ymin = mN - 1.96*sdN,
                    ymax = mN + 1.96*sdN),
                width = 0) +
  geom_errorbarh(data = means,
                 aes(x = mC, y = mN,
                     xmin = mC - 1.96*sdC,
                     xmax = mC + 1.96*sdC),
                 height = 0) +
  # mean points
  geom_point(data = means,
             aes(x = mC, y = mN, fill = Season),
             color = "black", shape = 21, size = 5) +
  facet_grid(Sex ~ Sample_type) +
  scale_color_manual(values = cbf) +
  scale_fill_manual(values = cbf) +
  guides(shape = "none") + 
  theme_bw(base_size = 14) +
  labs(x = expression(delta^13*C~"‰"),
       y = expression(delta^15*N~"‰"),
       color = "Season", fill = "Season", shape = "Sex")


#By year out of curiosity

class(SIA_bloods$Date)
SIA_bloods$Date <- as.Date(SIA_bloods$Date, format = "%d/%m/%Y")
SIA_bloods$Year <- year(SIA_bloods$Date)
SIA_bloods$Year<-as.factor(SIA_bloods$Year)

first.plot <- ggplot(data = SIA_bloods, 
                     mapping = aes(x = C, 
                                   y = N)) + 
  geom_point(aes(color = Year, shape = Year), size = 5) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  theme(text = element_text(size=20))
print(first.plot)


classic.first.plot <- first.plot + theme_classic() + 
  theme(text = element_text(size=35)) + 
  coord_equal() + 
  theme(axis.ticks.length = unit(-0.2, "cm")) +
  scale_colour_viridis_d(end = 0.9)
print(classic.first.plot)

fbmeans <- SIA_bloods %>% 
  group_by(Year) %>% 
  summarise(count = n(),
            mC = mean(C), 
            sdC = sd(C), 
            mN = mean(N), 
            sdN = sd(N) )
print(fbmeans)

second.plot <- first.plot + 
  geom_errorbar(data = fbmeans, 
                mapping = aes(x = mC, y = mN,
                              ymin = mN - 1.96*sdN, 
                              ymax = mN + 1.96*sdN), 
                width = 0)+
  geom_errorbarh(data = fbmeans, 
                 mapping = aes(x = mC, y = mN,
                               xmin = mC - 1.96*sdC,
                               xmax = mC + 1.96*sdC),
                 height = 0) + 
  geom_point(data = fbmeans, aes(x = mC, 
                                 y = mN,
                                 fill = Year), 
             color = "black", shape = 22, size = 5,
             alpha = 0.7, show.legend = FALSE) + 
  coord_equal()
print(second.plot)

p.ell <- 0.95

ellipse.plot <- first.plot + 
  stat_ellipse(aes(group = Year, 
                   fill = Year, 
                   color = Year), 
               alpha = 0.25, 
               level = p.ell,
               type = "norm",
               geom = "polygon")
print(ellipse.plot)

#Some Siber code for these same ellipses in another file

###Testing relationships

str(SIA_full)

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use = "complete.obs")  # add this
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = 2)
}

pairs(SIA_explor[, sapply(SIA_explor, is.numeric)],
      diag.panel = panel.hist,
      upper.panel = panel.smooth,
      lower.panel = panel.cor)

hist(SIA_biopsy$N)
str(SIA_full)

#not normal
shapiro.test(SIA_biopsy$C[SIA_biopsy$Sex == "F"])

# skin carbon by sex 
SIA_biopsy$Sex<-as.factor(SIA_biopsy$Sex)
skinC.aov <- aov(C ~ Sex, data = SIA_biopsy)
summary(skinC.aov)
plot(SIA_biopsy$C~SIA_biopsy$Sex)
#significant

# skin N by sex
skinN.aov <- aov(N ~ Sex, data = SIA_biopsy)
summary(skinN.aov)
plot(SIA_biopsy$N~SIA_biopsy$Sex)
#not significant

# blood carbon by sex 
bloodC.aov <- aov(C ~ Sex, data = SIA_bloods)
summary(bloodC.aov)
plot(SIA_bloods$C~SIA_bloods$Sex)
#significant

# blood N by sex
bloodN.aov <- aov(N ~ Sex, data = SIA_bloods)
summary(bloodN.aov)
plot(SIA_bloods$N~SIA_bloods$Sex)
#significant


multivar.model <- manova(cbind(C, N) ~ Sex, 
                         data = SIA_biopsy)

summary(multivar.model)
#significant

multivar.model <- manova(cbind(C, N) ~ Sex, 
                         data = SIA_bloods)

summary(multivar.model)
#significant

#bloods
model_C <- lm(C ~ Sex + Season + Year, data = SIA_bloods)
summary(model_C)



ggplot(SIA_bloods, aes(x = Season, y = C, fill = Sex)) +
  geom_boxplot() +
  facet_wrap(~ Year) +
  labs(x = "Season", y = expression(delta^13*C~"‰"),
       fill = "Sex") +
  theme_bw()

model_N <- lm(N ~ Sex + Season + Year, data = SIA_bloods)
summary(model_N)


ggplot(SIA_bloods, aes(x = Season, y = N, fill = Sex)) +
  geom_boxplot() +
  facet_wrap(~ Year) +
  labs(x = "Season", y = expression(delta^15*N~"‰"),
       fill = "Sex") +
  theme_bw()

#biopsies
model_C <- lm(C ~ Sex + Season + Year, data = SIA_biopsy)
summary(model_C)


ggplot(SIA_biopsy, aes(x = Season, y = C, fill = Sex)) +
  geom_boxplot() +
  facet_wrap(~ Year) +
  labs(x = "Season", y = expression(delta^13*C~"‰"),
       fill = "Sex") +
  theme_bw()

model_N <- lm(N ~ Sex + Season + Year, data = SIA_biopsy)
summary(model_N)


ggplot(SIA_biopsy, aes(x = Season, y = N, fill = Sex)) +
  geom_boxplot() +
  facet_wrap(~ Year) +
  labs(x = "Season", y = expression(delta^15*N~"‰"),
       fill = "Sex") +
  theme_bw()

# tidy all 4 models into one dataframe
results <- bind_rows(
  tidy(lm(C ~ Sex + Season + Year, data = SIA_bloods), conf.int = TRUE) %>% 
    mutate(tissue = "Blood", isotope = "δ13C"),
  tidy(lm(N ~ Sex + Season + Year, data = SIA_bloods), conf.int = TRUE) %>% 
    mutate(tissue = "Blood", isotope = "δ15N"),
  tidy(lm(C ~ Sex + Season + Year, data = SIA_biopsy), conf.int = TRUE) %>% 
    mutate(tissue = "Biopsy", isotope = "δ13C"),
  tidy(lm(N ~ Sex + Season + Year, data = SIA_biopsy), conf.int = TRUE) %>% 
    mutate(tissue = "Biopsy", isotope = "δ15N")
) %>% filter(term != "(Intercept)")

ggplot(results, aes(x = estimate, y = term, colour = isotope, shape = tissue)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(size = 3) +
  facet_wrap(~ tissue + isotope, scales = "free_x") +
  labs(x = "Estimate (‰)", y = NULL) +
  theme_bw() +
  theme(legend.position = "none")

head(SIA_full)
ggplot(SIA_full, aes(x = Sex, y = N, fill = Sex)) +
  geom_boxplot() +
  facet_wrap(~ Sample_type) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = NULL, y = expression(delta^13*C~"‰"))

ggplot(SIA_full, aes(x = Season, y = C, fill = Season)) +
  geom_boxplot() +
  facet_wrap(~ Sample_type) +
  theme_bw() +
  theme(legend.position = "none")

ggplot(SIA_full, aes(x = Sex, y = N, fill = Season)) +
  geom_boxplot() +
  facet_wrap(~ Sample_type) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = expression(delta^15*N~"‰"))

ggplot(SIA_full, aes(x = Sex, y = N, fill = Sex)) +
  geom_boxplot() +
  facet_grid(Season ~ Sample_type) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = NULL, y = expression(delta^15*N~"‰"))

ggplot(SIA_full, aes(x = Season, y = N, fill = Season)) +
  geom_boxplot() +
  facet_grid(Sex ~ Sample_type) +
  theme_bw() +
  scale_fill_manual(values = cbf) +
  labs(x = NULL, y = expression(delta^15*N~"‰"))

ggplot(SIA_full, aes(x = Season, y = N, fill = Sex)) +
  geom_boxplot() +
  facet_wrap(~ Sample_type, ncol = 2) +
  theme_bw() +
  scale_fill_manual(values = cbf) +
  labs(x = NULL, y = expression(delta^15*N~"‰"), fill = "Sex")



