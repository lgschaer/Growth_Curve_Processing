#---------------Growth Curve for Top 3 Cultures at 3 Concentrations on 2 Substrates---------------#
#Packages used
library(csv)
library(tidyverse)

#Load data
##Your data should be formatted in a CSV file
growth <- as.csv("/Users/lgschaer/Desktop/MTU_Projects/04192021_ReSource_Concentration/Concentration_Growth_Curve_Data.csv", row.names = 1, header = TRUE, sep = ",", check.names = TRUE, stringsAsFactors = TRUE)
head(growth)

growth2 <- growth %>%
  mutate(Time_Point = factor(Time_Point, levels = c("T0", "T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10", "T11", "T12", "T13")),
         #Concentration = as.character(Concentration_g_L),
         ConcentrationB = ifelse(Concentration_g_L == 25 | Concentration_g_L == 12.5, "High", Concentration_g_L),
         ConcentrationC = ifelse(ConcentrationB == 17.5 | ConcentrationB == 8, "Medium", ConcentrationB),
         Concentration = ifelse(ConcentrationC == 10 | ConcentrationC == 3, "Low", ConcentrationC)) %>%
  select(-c("ConcentrationB", "ConcentrationC")) %>%
  group_by(Inocula, Substrate, Concentration, Time_Point, Hours) %>%
  filter(Substrate != "Inocula" & !is.na(Inocula)) %>%
  summarise(#Enrichment = ifelse(Enrichment == "Control_Control", "Control", Enrichment),
    maxAbsorbance = max(Absorbance),
    minAbsorbance = min(Absorbance),
    avgAbsorbance = mean(Absorbance))
head(growth2)

colors <- c("lightblue","Purple","Red", "Orange", "Green", "Magenta", "Grey", "Black")

#plot with ggplot
ggplot(growth2, mapping = aes(x = Hours, y = avgAbsorbance, 
                              fill = Inocula), show.legend = TRUE)+
  facet_grid(cols = vars(Inocula), rows = vars(Substrate), shrink = TRUE)+
  #geom_col(show.legend = FALSE, position = "dodge", color = "black")+
  geom_errorbar(aes(ymax = maxAbsorbance, ymin = minAbsorbance, color = Inocula), width = 2)+
  geom_point(aes(x= Hours, y = avgAbsorbance), shape =21, color = "black", size = 6)+
  geom_line(aes(linetype = Concentration, color = Inocula))+
  ylab("Absorbance (600nm)") +
  xlab("Time (h)")+
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  theme_linedraw()+
  theme(strip.text = element_text(face = "bold", size = 12),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        axis.text.y.left = element_text(size = 20),
        axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title.y = element_text(size = 20, hjust = 0.5),
        title = element_text(size = 25))


growth3 <- growth %>%
  filter(Substrate != "Inocula" & !is.na(Inocula)) %>%
  mutate(Time_Point = factor(Time_Point, levels = c("T0", "T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10", "T11", "T12", "T13")),
         ConcentrationB = ifelse(Concentration_g_L == 25 | Concentration_g_L == 12.5, "High", Concentration_g_L),
         ConcentrationC = ifelse(ConcentrationB == 17.5 | ConcentrationB == 8, "Medium", ConcentrationB),
         Concentration = ifelse(ConcentrationC == 10 | ConcentrationC == 3, "Low", ConcentrationC),
         Concentration = factor(Concentration, levels = c("High", "Medium", "Low"))) %>%
  select(-c("ConcentrationB", "ConcentrationC")) %>%
  group_by(Inocula, Substrate, Concentration, Time_Point, Hours) %>%
  mutate(
    avgAbsorbance = mean(Absorbance)
  )
head(growth3)

#tpaGrowth <- filter(growth3, Substrate == "TPA")

#pyroGrowth <- filter(growth3, Substrate == "Pyrolysis")

#colors <- c("lightblue","Blue", "Purple", "Red", "Orange", "Green", "Magenta", "Grey", "Black")
colors2 <- c("Purple", "Red", "Orange", "Magenta")

#plot with ggplot
ggplot(growth3, mapping = aes(x = Hours, y = avgAbsorbance, 
                              fill = Inocula), show.legend = TRUE)+
  facet_grid(cols = vars(Inocula), rows = vars(Substrate), shrink = TRUE)+
  geom_point(aes(x= Hours, y = Absorbance, shape = Replicate), color = "black", size = 2)+
  geom_line(aes(x = Hours, y = avgAbsorbance, linetype = Concentration, color = Inocula))+
  geom_point(aes(x= Hours, y = avgAbsorbance), shape = 21, color = "black", size = 6)+
  ylab("Absorbance (600nm)") +
  xlab("Time (h)")+
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = c(22,23,24))+
  theme_linedraw()+
  theme(strip.text = element_text(face = "bold", size = 12),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        axis.text.y.left = element_text(size = 20),
        axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title.y = element_text(size = 20, hjust = 0.5),
        title = element_text(size = 25))

#subset by substrate
tpa_only <- filter(growth3, Substrate == "TPA")
pyro_only <- filter(growth3, Substrate == "Pyrolysis")

dim(growth3)
dim(tpa_only)
dim(pyro_only)

#plot with ggplot
ggplot(pyro_only, mapping = aes(x = Hours, y = avgAbsorbance, 
                              fill = Inocula), show.legend = TRUE)+
  facet_grid(cols = vars(Inocula), rows = vars(Concentration), shrink = TRUE)+
  geom_point(aes(x= Hours, y = Absorbance, shape = Replicate), color = "black", size = 2)+
  geom_line(aes(x = Hours, y = avgAbsorbance, linetype = Concentration, color = Inocula))+
  geom_point(aes(x= Hours, y = avgAbsorbance), shape = 21, color = "black", size = 6)+
  ylab("Absorbance (600nm)") +
  xlab("Time (h)")+
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = c(22,23,24))+
  theme_linedraw()+
  theme(strip.text = element_text(face = "bold", size = 12),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        axis.text.y.left = element_text(size = 20),
        axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title.y = element_text(size = 20, hjust = 0.5),
        title = element_text(size = 25))

#Calculate remaining TPA
head(growth)

tpadeg <- growth3 %>%
  filter(!is.na(TPA_Absorbance) & Time_Point != "T0")%>%
  group_by(Inocula, Substrate, Time_Point, Concentration) %>%
  mutate(
    tpaConc = (TPA_Absorbance-0.4887)/0.1161
  ) %>%
  summarise(
    maxConc = max(tpaConc),
    minConc = min(tpaConc),
    avgConc = mean(tpaConc))
head(tpadeg)

head(growth3)

tpadeg2 <- growth3 %>%
  filter(!is.na(X0.5_diluted_TPA) & Time_Point != "T0")%>%
  group_by(Inocula, Substrate, Time_Point, Concentration) %>%
  mutate(
    tpaConc = ((X0.5_diluted_TPA-0.4887)/0.1161)*2
  ) %>%
  summarise(
    maxConc = max(tpaConc),
    minConc = min(tpaConc),
    avgConc = mean(tpaConc))
head(tpadeg2)

colors <- c("lightblue", "Purple", "Red", "Orange", "Green", "Magenta", "Grey", "Black")
colors2 <- c("Blue", "Purple", "Red", "Orange", "Green", "Magenta", "Grey", "Black")

#plot with ggplot
ggplot(tpadeg2, mapping = aes(x = Time_Point, y = avgConc, 
                              fill = Inocula), show.legend = TRUE)+
  geom_errorbar(aes(ymax = maxConc, ymin = minConc), color = "black", width = .9, position = "dodge")+
  geom_col(color = "black", position = "dodge")+
  facet_grid(cols = vars(Concentration))+
  ylab("Remaining TPA (g/L)") +
  xlab("Starting Concentration of TPA (High = 25 g/L, Medium = 17.5 g/L, Low = 10 g/L)")+
  scale_fill_manual(values = colors) +
  #scale_color_manual(values = colors2) +
  theme_linedraw()+
  theme(strip.text = element_text(face = "bold", size = 12),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        axis.text.y.left = element_text(size = 20),
        axis.text.x = element_text(size = 20, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title.y = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 15, hjust = 0.5),
        title = element_text(size = 25))

#Biomass Measurements
head(growth)

weights <- filter(growth, !is.na(Pellet_Weight_mg))
range(weights$Pellet_Weight_mg)

biomass <- growth %>%
  filter(!is.na(Pellet_Weight_mg)) %>%
  mutate(Time_Point = factor(Time_Point, levels = c("T0", "T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10", "T11", "T12", "T13")),
         ConcentrationB = ifelse(Concentration_g_L == 25 | Concentration_g_L == 12.5, "High", Concentration_g_L),
         ConcentrationC = ifelse(ConcentrationB == 17.5 | ConcentrationB == 8, "Medium", ConcentrationB),
         Concentration = ifelse(ConcentrationC == 10 | ConcentrationC == 3, "Low", ConcentrationC),
         Concentration = factor(Concentration, levels = c("High", "Medium", "Low"))) %>%
  select(-c("ConcentrationB", "ConcentrationC")) %>%
  group_by(Inocula, Substrate, Time_Point, Concentration) %>%
  mutate(grams_per_100mL = ((Pellet_Weight_mg)/1000)*100) %>%
  summarise(
    maxMass = max(grams_per_100mL),
    minMass = min(grams_per_100mL),
    avgMass = mean(grams_per_100mL))
head(biomass)

colors <- c("lightblue","Purple", "Red", "Orange", "Green", "Magenta", "Grey", "Black")
colors2 <- c("Blue", "Purple", "Red", "Orange", "Green", "Magenta", "Grey", "Black")

#plot with ggplot
ggplot(biomass, mapping = aes(x = Concentration, y = avgMass, 
                               fill = Inocula), show.legend = FALSE)+
  geom_errorbar(aes(ymax = maxMass, ymin = minMass), color = "black", width = 0.5)+
  geom_col(color = "black", show.legend = FALSE)+
  facet_grid(cols = vars(Inocula), rows = vars(Substrate))+
  ylab("Average Biomass (g per 100mL)") +
  xlab("Starting Concentration of TPA (High = 25 g/L, Medium = 17.5 g/L, Low = 10 g/L)\n Starting Concentration of Pyrolysis (High = 12.5g/L, Medium = 8g/L, Low = 3 g/L)")+
  scale_fill_manual(values = colors) +
  theme_linedraw()+
  theme(strip.text = element_text(face = "bold", size = 12),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        axis.text.y.left = element_text(size = 20),
        axis.text.x = element_text(size = 20, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title.y = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 15, hjust = 0.5),
        title = element_text(size = 25))


#Plot growth rates
growthrates <- as.csv("/Users/lgschaer/Desktop/MTU_Projects/Lab Data/Concentration_Growth_Rates_04262021.csv", row.names = 1, header = TRUE, sep = ",", check.names = TRUE, stringsAsFactors = TRUE)
head(growthrates)

growthrates2 <- growthrates %>%
  filter(!is.na(SampleID)) %>%
  left_join(growth3, by = "SampleID") %>%
  select(-c("X", "Time_Point", "Date", "Time_Collected", "Minutes", "Hours", "TPA_Absorbance", "Absorbance", "X0.5_diluted_TPA", "Pellet_Weight_mg"))
View(growthrates2)

summary_gr <- growthrates2 %>%
  group_by(Substrate, Concentration, Inocula) %>%
  summarise(
    mean_no_generations = mean(No_Generations),
    mean_gen_time_min = mean(Generation_Time_Minutes),
    mean_gen_time_hr = mean(Generation_Time_Hours),
    mean_spec_gr_rate_min = mean(Specific_Growth_Rate_Minutes),
    mean_spec_gr_rate_hr = mean(Specific_Growth_Rate_Hours),
    min_no_generations = min(No_Generations),
    min_gen_time_min = min(Generation_Time_Minutes),
    min_gen_time_hr = min(Generation_Time_Hours),
    min_spec_gr_rate_min = min(Specific_Growth_Rate_Minutes),
    min_spec_gr_rate_hr = min(Specific_Growth_Rate_Hours),
    max_no_generations = max(No_Generations),
    max_gen_time_min = max(Generation_Time_Minutes),
    max_gen_time_hr = max(Generation_Time_Hours),
    max_spec_gr_rate_min = max(Specific_Growth_Rate_Minutes),
    max_spec_gr_rate_hr = max(Specific_Growth_Rate_Hours)
  )
head(summary_gr)

write_csv(summary_gr, "/Users/lgschaer/Desktop/summary_gr.csv")

#plot with ggplot
head(growthrates2)

colors <- c("Purple", "Red", "Orange")

ggplot(summary_gr, mapping = aes(x = Concentration, y = mean_no_generations, 
                              fill = Inocula), show.legend = TRUE)+
  facet_grid(cols = vars(Inocula), rows = vars(Substrate), shrink = TRUE)+
  geom_col(color = "black")+
  geom_errorbar(aes(ymax = max_no_generations, ymin = min_no_generations), color = "black", width = 0.5)+
  ylab("No_Generations") +
  xlab("Concentration")+
  scale_fill_manual(values = colors) +
  theme_linedraw()+
  theme(strip.text = element_text(face = "bold", size = 12),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        axis.text.y.left = element_text(size = 20),
        axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title.y = element_text(size = 20, hjust = 0.5),
        title = element_text(size = 25))

ggplot(summary_gr, mapping = aes(x = Concentration, y = mean_gen_time_hr, 
                                 fill = Inocula), show.legend = TRUE)+
  facet_grid(cols = vars(Inocula), rows = vars(Substrate), shrink = TRUE)+
  geom_col(color = "black")+
  geom_errorbar(aes(ymax = max_gen_time_hr, ymin = min_gen_time_hr), color = "black", width = 0.5)+
  ylab("Generation Time (Hours)") +
  xlab("Concentration")+
  scale_fill_manual(values = colors) +
  theme_linedraw()+
  theme(strip.text = element_text(face = "bold", size = 12),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        axis.text.y.left = element_text(size = 20),
        axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title.y = element_text(size = 20, hjust = 0.5),
        title = element_text(size = 25))


ggplot(summary_gr, mapping = aes(x = Concentration, y = mean_spec_gr_rate_hr, 
                                 fill = Inocula), show.legend = FALSE)+
  facet_grid(cols = vars(Inocula), rows = vars(Substrate), shrink = TRUE)+
  geom_col(color = "black", show.legend = FALSE)+
  geom_errorbar(aes(ymax = max_spec_gr_rate_hr, ymin = min_spec_gr_rate_hr), color = "black", width = 0.5)+
  ylab("Specific Growth Rate (Hours)") +
  xlab("Concentration")+
  scale_fill_manual(values = colors) +
  theme_linedraw()+
  theme(strip.text = element_text(face = "bold", size = 12),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        axis.text.y.left = element_text(size = 20),
        axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title.y = element_text(size = 20, hjust = 0.5),
        title = element_text(size = 25))

