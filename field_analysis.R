### 1) Load libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(reshape2)
library(cowplot)
library(ggExtra)
library(ggrepel)
library(ggpmisc)
library(lme4)
library(car)
library(lmerTest)
library(emmeans)
library(MetBrewer)

# Set working directory
setwd("~/Library/CloudStorage/GoogleDrive-kellykerr@ucsb.edu/My Drive/Projects/SEKI Fieldwork/2023/Seedlings")

### 2) Load datasets and fix conductivity dataset
df <- read.csv("./seedling_data_set.csv") 
head(df)
str(df)

tleaf <- read.csv('./tleaf_seedlings.csv')


df$therm.safety.tcrit.tleaf <- df$tcrit - df$mean.leaf.temp.c
df$therm.safety.tcrit.tday <- df$tcrit - df$max.air.temp.day.c
df$therm.safety.tcrit.tseason <- df$tcrit - df$max.air.temp.season.c
df$therm.margin <- df$t95 - df$tcrit
df$therm.margin.low <- df$t50 - df$tcrit
df$therm.margin.high <- df$t95 - df$t50

adult.df <- read.csv("./adult_cond_data.csv")
adult.df2 <- subset(adult.df, max_type == "native")
adult.tlp <- subset(adult.df, max_type == "tlp")

### 3) Explore raw data
# Specify the variables you want to plot
vars <- c("TLP.MPa", "Ks", "t95", "t50", "tcrit", "lma.g.cm2", "al.as.cm2.mm2", "therm.safety.tcrit")  

# Create a loop to generate boxplots for each variable
for (variable in vars) {
  # dev.off()
  # rm(p)
  # Create a ggplot object
  p <- ggplot(df, aes(x = treat, y = df[[variable]], fill = species)) +
    geom_boxplot() +
    labs(title = variable, x = "Site", y = variable) +
    theme_minimal()
  
  # Facet the plot by species
  p <- p + facet_grid(~species)
  
  # Save or display the plot
  # ggsave(paste0(variable, "_boxplot.png"), p, width = 10, height = 6)  # Save the plot
  print(p)  # Display the plot
}


### 4) Graphs for traits
## Calculate mean, SDs, SEs, and CIs by site, species, and treatment
df.therm.sum <- df %>% 
  group_by(site, species, treat) %>%
  dplyr::summarise(mean.tcrit = mean(tcrit, na.rm = TRUE),
                   median.tcrit = median(tcrit, na.rm=TRUE),
                   sd.tcrit = sd(tcrit, na.rm = TRUE),
                   n.tcrit = length(tcrit[!is.na(tcrit)]),
                   mean.t50 = mean(t50, na.rm = TRUE),
                   median.t50 = median(t50, na.rm=TRUE),
                   sd.t50 = sd(t50, na.rm = TRUE),
                   n.t50 = length(t50[!is.na(t50)]),
                   mean.t95 = mean(t95, na.rm = TRUE),
                   median.t95 = median(t95, na.rm=TRUE),
                   sd.t95 = sd(t95, na.rm = TRUE),
                   n.t95 = length(t95[!is.na(t95)]),
                   mean.therm.safety.tcrit.tleaf = mean(therm.safety.tcrit.tleaf, na.rm = TRUE),
                   median.therm.safety.tcrit.tleaf = median(therm.safety.tcrit.tleaf, na.rm=TRUE),
                   sd.therm.safety.tcrit.tleaf = sd(therm.safety.tcrit.tleaf, na.rm = TRUE),
                   n.therm.safety.tcrit.tleaf = length(therm.safety.tcrit.tleaf[!is.na(therm.safety.tcrit.tleaf)]),
                   mean.therm.safety.tcrit.tday = mean(therm.safety.tcrit.tday, na.rm = TRUE),
                   median.therm.safety.tcrit.tday = median(therm.safety.tcrit.tday, na.rm=TRUE),
                   sd.therm.safety.tcrit.tday = sd(therm.safety.tcrit.tday, na.rm = TRUE),
                   n.therm.safety.tcrit.tday = length(therm.safety.tcrit.tday[!is.na(therm.safety.tcrit.tday)]),
                   mean.therm.safety.tcrit.tseason = mean(therm.safety.tcrit.tseason, na.rm = TRUE),
                   median.therm.safety.tcrit.tseason = median(therm.safety.tcrit.tseason, na.rm=TRUE),
                   sd.therm.safety.tcrit.tseason = sd(therm.safety.tcrit.tseason, na.rm = TRUE),
                   n.therm.safety.tcrit.tseason = length(therm.safety.tcrit.tseason[!is.na(therm.safety.tcrit.tseason)]),
                   mean.therm.margin = mean(therm.margin, na.rm = TRUE),
                   median.therm.margin = median(therm.margin, na.rm=TRUE),
                   sd.therm.margin = sd(therm.margin, na.rm = TRUE),
                   n.therm.margin = length(therm.margin[!is.na(therm.margin)]),
                   mean.therm.margin.low = mean(therm.margin.low, na.rm = TRUE),
                   median.therm.margin.low = median(therm.margin.low, na.rm=TRUE),
                   sd.therm.margin.low = sd(therm.margin.low, na.rm = TRUE),
                   n.therm.margin.low = length(therm.margin.low[!is.na(therm.margin.low)]),
                   mean.therm.margin.high = mean(therm.margin.high, na.rm = TRUE),
                   median.therm.margin.high = median(therm.margin.high, na.rm=TRUE),
                   sd.therm.margin.high = sd(therm.margin.high, na.rm = TRUE),
                   n.therm.margin.high = length(therm.margin.high[!is.na(therm.margin.high)]),
                   mean.TLP.MPa = mean(TLP.MPa, na.rm = TRUE),
                   median.TLP.MPa = median(TLP.MPa, na.rm=TRUE),
                   sd.TLP.MPa = sd(TLP.MPa, na.rm = TRUE),
                   n.TLP.MPa = length(TLP.MPa[!is.na(TLP.MPa)]),
                   mean.Ks = mean(Ks, na.rm = TRUE),
                   median.Ks = median(Ks, na.rm=TRUE),
                   sd.Ks = sd(Ks, na.rm = TRUE),
                   n.Ks = length(Ks[!is.na(Ks)])) %>%
  mutate(se.tcrit = sd.tcrit / sqrt(n.tcrit),
         lowerci.tcrit = mean.tcrit - qt(1 - (0.05 / 2), n.tcrit - 1) * se.tcrit,
         upperci.tcrit = mean.tcrit + qt(1 - (0.05 / 2), n.tcrit - 1) * se.tcrit,
         cv.tcrit = sd.tcrit/mean.tcrit,
         se.t50 = sd.t50 / sqrt(n.t50),
         lowerci.t50 = mean.t50 - qt(1 - (0.05 / 2), n.t50 - 1) * se.t50,
         upperci.t50 = mean.t50 + qt(1 - (0.05 / 2), n.t50 - 1) * se.t50,
         cv.t50 = sd.t50/mean.t50,
         se.t95 = sd.t95 / sqrt(n.t95),
         lowerci.t95 = mean.t95 - qt(1 - (0.05 / 2), n.t95 - 1) * se.t95,
         upperci.t95 = mean.t95 + qt(1 - (0.05 / 2), n.t95 - 1) * se.t95,
         cv.t95 = sd.t95/mean.t95,
         se.therm.safety.tcrit.tleaf = sd.therm.safety.tcrit.tleaf / sqrt(n.therm.safety.tcrit.tleaf),
         lowerci.therm.safety.tcrit.tleaf = mean.therm.safety.tcrit.tleaf - qt(1 - (0.05 / 2), n.therm.safety.tcrit.tleaf - 1) * se.therm.safety.tcrit.tleaf,
         upperci.therm.safety.tcrit.tleaf = mean.therm.safety.tcrit.tleaf + qt(1 - (0.05 / 2), n.therm.safety.tcrit.tleaf - 1) * se.therm.safety.tcrit.tleaf,
         cv.therm.safety.tcrit.tleaf = sd.therm.safety.tcrit.tleaf/mean.therm.safety.tcrit.tleaf,
         se.therm.safety.tcrit.tday = sd.therm.safety.tcrit.tday / sqrt(n.therm.safety.tcrit.tday),
         lowerci.therm.safety.tcrit.tday = mean.therm.safety.tcrit.tday - qt(1 - (0.05 / 2), n.therm.safety.tcrit.tday - 1) * se.therm.safety.tcrit.tday,
         upperci.therm.safety.tcrit.tday = mean.therm.safety.tcrit.tday + qt(1 - (0.05 / 2), n.therm.safety.tcrit.tday - 1) * se.therm.safety.tcrit.tday,
         cv.therm.safety.tcrit.tday = sd.therm.safety.tcrit.tday/mean.therm.safety.tcrit.tday,
         se.therm.safety.tcrit.tseason = sd.therm.safety.tcrit.tseason / sqrt(n.therm.safety.tcrit.tseason),
         lowerci.therm.safety.tcrit.tseason = mean.therm.safety.tcrit.tseason - qt(1 - (0.05 / 2), n.therm.safety.tcrit.tseason - 1) * se.therm.safety.tcrit.tseason,
         upperci.therm.safety.tcrit.tseason = mean.therm.safety.tcrit.tseason + qt(1 - (0.05 / 2), n.therm.safety.tcrit.tseason - 1) * se.therm.safety.tcrit.tseason,
         cv.therm.safety.tcrit.tseason = sd.therm.safety.tcrit.tseason/mean.therm.safety.tcrit.tseason,
         se.therm.margin = sd.therm.margin / sqrt(n.therm.margin),
         lowerci.therm.margin = mean.therm.margin - qt(1 - (0.05 / 2), n.therm.margin - 1) * se.therm.margin,
         upperci.therm.margin = mean.therm.margin + qt(1 - (0.05 / 2), n.therm.margin - 1) * se.therm.margin,
         cv.therm.margin = sd.therm.margin/mean.therm.margin,
         se.therm.margin.low = sd.therm.margin.low / sqrt(n.therm.margin.low),
         lowerci.therm.margin.low = mean.therm.margin.low - qt(1 - (0.05 / 2), n.therm.margin.low - 1) * se.therm.margin.low,
         upperci.therm.margin.low = mean.therm.margin.low + qt(1 - (0.05 / 2), n.therm.margin.low - 1) * se.therm.margin.low,
         cv.therm.margin.low = sd.therm.margin.low/mean.therm.margin.low,
         se.therm.margin.high = sd.therm.margin.high / sqrt(n.therm.margin.high),
         lowerci.therm.margin.high = mean.therm.margin.high - qt(1 - (0.05 / 2), n.therm.margin.high - 1) * se.therm.margin.high,
         upperci.therm.margin.high = mean.therm.margin.high + qt(1 - (0.05 / 2), n.therm.margin.high - 1) * se.therm.margin.high,
         cv.therm.margin.high = sd.therm.margin.high/mean.therm.margin.high,
         se.TLP.MPa = sd.TLP.MPa / sqrt(n.TLP.MPa),
         lowerci.TLP.MPa = mean.TLP.MPa - qt(1 - (0.05 / 2), n.TLP.MPa - 1) * se.TLP.MPa,
         upperci.TLP.MPa = mean.TLP.MPa + qt(1 - (0.05 / 2), n.TLP.MPa - 1) * se.TLP.MPa,
         cv.TLP.MPa = sd.TLP.MPa/mean.TLP.MPa,
         se.Ks = sd.Ks / sqrt(n.Ks),
         lowerci.Ks = mean.Ks - qt(1 - (0.05 / 2), n.Ks - 1) * se.Ks,
         upperci.Ks = mean.Ks + qt(1 - (0.05 / 2), n.Ks - 1) * se.Ks,
         cv.Ks = sd.Ks/mean.Ks)

df.adult.sum <- adult.df2 %>% 
  group_by(sp, small) %>%
  dplyr::summarise(mean.adult.Ks = mean(k.g.s.MPa, na.rm = TRUE),
                   median.adult.Ks = median(k.g.s.MPa, na.rm=TRUE),
                   sd.adult.Ks = sd(k.g.s.MPa, na.rm = TRUE),
                   n.adult.Ks = length(k.g.s.MPa[!is.na(k.g.s.MPa)])) %>%
  mutate(se.adult.Ks = sd.adult.Ks / sqrt(n.adult.Ks),
         lowerci.adult.Ks = mean.adult.Ks - qt(1 - (0.05 / 2), n.adult.Ks - 1) * se.adult.Ks,
         upperci.adult.Ks = mean.adult.Ks + qt(1 - (0.05 / 2), n.adult.Ks - 1) * se.adult.Ks,
         cv.adult.Ks = sd.adult.Ks/mean.adult.Ks)

tlp.adult.sum <- adult.tlp %>% 
  group_by(sp) %>%
  dplyr::summarise(mean.adult.tlp = mean(tlp, na.rm = TRUE),
                   median.adult.tlp = median(tlp, na.rm=TRUE),
                   sd.adult.tlp = sd(tlp, na.rm = TRUE),
                   n.adult.tlp = length(tlp[!is.na(tlp)])) %>%
  mutate(se.adult.tlp = sd.adult.tlp / sqrt(n.adult.tlp),
         lowerci.adult.tlp = mean.adult.tlp - qt(1 - (0.05 / 2), n.adult.tlp - 1) * se.adult.tlp,
         upperci.adult.tlp = mean.adult.tlp + qt(1 - (0.05 / 2), n.adult.tlp - 1) * se.adult.tlp,
         cv.adult.tlp = sd.adult.tlp/mean.adult.tlp)

tleaf.sum <- tleaf %>% 
  group_by(species,treat) %>%
  dplyr::summarise(mean.tleaf = mean(tleaf, na.rm = TRUE),
                   median.tleaf = median(tleaf, na.rm=TRUE),
                   sd.tleaf = sd(tleaf, na.rm = TRUE),
                   n.tleaf = length(tleaf[!is.na(tleaf)])) %>%
  mutate(se.tleaf = sd.tleaf / sqrt(n.tleaf),
         lowerci.tleaf = mean.tleaf - qt(1 - (0.05 / 2), n.tleaf - 1) * se.tleaf,
         upperci.tleaf = mean.tleaf + qt(1 - (0.05 / 2), n.tleaf - 1) * se.tleaf,
         cv.tleaf = sd.tleaf/mean.tleaf)

# Change pop names to match growth data
df.adult.sum <- df.adult.sum %>%
  mutate(sp = case_when(
    sp == "ABCO" ~ "abco",
    sp == "PILA" ~ "pila",
    sp == "CADE" ~ "cade"))
colnames(df.adult.sum)[colnames(df.adult.sum) %in% c("sp","small")] <- c("species","size")

df.adult.sum <- df.adult.sum %>%
  pivot_wider(
    id_cols = species,
    names_from = size,
    values_from = c(mean.adult.Ks, median.adult.Ks, sd.adult.Ks, n.adult.Ks, se.adult.Ks, 
                    lowerci.adult.Ks, upperci.adult.Ks, cv.adult.Ks)
  )

df.merge <- merge(df.therm.sum, df.adult.sum, by = "species", all = TRUE)

tlp.adult.sum <- tlp.adult.sum %>%
  mutate(sp = case_when(
    sp == "ABCO" ~ "abco",
    sp == "PILA" ~ "pila",
    sp == "CADE" ~ "cade",
    sp == "PIPO" ~ "pipo",
    sp == "SEGI" ~ "segi"))
colnames(tlp.adult.sum)[colnames(tlp.adult.sum) %in% c("sp")] <- c("species")


# sjnf2$elev <- factor(sjnf2$elev,levels = c("L","M","H"))
# sjnf.elev.sum1$elev <- factor(sjnf.elev.sum1$elev,levels = c("L","M","H"))
# sjnf.elev.sum$elev <- factor(sjnf.elev.sum$elev,levels = c("L","M","H"))
# multi2$pop.plot <- factor(multi2$pop.plot,levels = c("DIXIE","SJNF","UNCO","WR","UINTA"))
# multi.pop.sum$pop.plot <- factor(multi.pop.sum$pop.plot,levels = c("DIXIE","SJNF","UNCO","WR","UINTA"))
# aspen.pop.sum$pop.plot <- factor(aspen.pop.sum$pop.plot,levels = c("DIXIE","SJNF","UNCO","WR","UINTA"))
# pop.labs <- c("DIXIE","SJNF","UNCO","WR","UINTA")
# names(pop.labs) <- c("Dixie","San Juan","Uncompaghre","White River","Uinta")
# formula <- y ~ x #formula to print on plots

sp.labs <- c("ABCO", "CADE", "PILA", "PIPO", "SEGI")
names(sp.labs) <- c("abco", "cade", "pila", "pipo", "segi")

## Al:As 
ggplot(data = df, aes(x=treat, y=al.as.cm2.mm2)) +
  geom_boxplot(na.rm=T, aes(fill=species)) +
  # geom_jitter(aes(color=elev), size=2, shape=15, width=0.1) +
  facet_grid(~species, labeller = labeller(species = sp.labs)) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x="Microsite", y=expression(atop(A[L]:A[S], paste(~ (m^2 ~ cm^-2))))) +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  guides(fill="none") +
  scale_y_continuous(expand=c(0,0), limits=c(0, 42)) + 
  scale_x_discrete(labels = c("shade"="Shade", "sun" = "Sun")) +
  scale_fill_met_d("Hokusai3")


## LMA
ggplot(data = df, aes(x=treat, y=lma.g.cm2)) +
  geom_boxplot(na.rm=T, aes(fill=species)) +
  # geom_jitter(aes(color=elev), size=2, shape=15, width=0.1) +
  facet_grid(~species, labeller = labeller(species = sp.labs)) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x="Microsite", y=expression(LMA ~ (m^2 ~ cm^-2))) +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  guides(fill="none") +
  scale_y_continuous(expand=c(0,0), limits=c(0, .016)) +
  scale_x_discrete(labels = c("shade"="Shade", "sun" = "Sun")) +
  scale_fill_met_d("Hokusai3")


## Ks - Seedlings
ggplot(data = df, aes(x=treat, y=(-Ks))) +
  geom_hline(data = df.adult.sum, aes(yintercept = -mean.adult.Ks_large), color = "darkorange", size=2) + #adding mean Ks for adults too
  geom_hline(data = df.adult.sum, aes(yintercept = -mean.adult.Ks_small), color = "brown", size=2) + #adding mean Ks for adults too
  geom_boxplot(na.rm=T, aes(fill=species)) +
  # geom_jitter(aes(color=elev), size=2, shape=15, width=0.1) +
  facet_grid(~species, labeller = labeller(species = sp.labs)) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x="Microsite", 
       y=expression(atop(K[s-native], paste(~ (kg ~ m^-1 ~ kPa^-1 ~ s^-1))))) +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  guides(fill="none") +
  # scale_y_continuous(expand=c(0,0), limits=c(0, .016)) +
  scale_x_discrete(labels = c("shade"="Shade", "sun" = "Sun")) +
  scale_fill_met_d("Hokusai3") 

## Ks - All stages
ggplot(data = df.merge, aes(x=treat, y=(-mean.Ks), group=treat)) +
  geom_bar(na.rm=T, aes(fill=species), stat="identity", position=position_dodge(width=0.5)) +
  geom_hline(data = df.adult.sum, aes(yintercept = -mean.adult.Ks_large), color = "darkorange", size=2) + #adding mean Ks for adults too
  geom_hline(data = df.adult.sum, aes(yintercept = -mean.adult.Ks_small), color = "brown", size=2) + #adding mean Ks for adults too
  # geom_jitter(aes(color=elev), size=2, shape=15, width=0.1) +
  facet_grid(~species, labeller = labeller(species = sp.labs)) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x="Microsite", 
       y=expression(atop(K[s-native], paste(~ (kg ~ m^-1 ~ kPa^-1 ~ s^-1))))) +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  guides(fill="none") +
  scale_y_continuous(expand=c(0,0), limits=c(0, 1.1)) +
  scale_x_discrete(labels = c("shade"="Shade", "sun" = "Sun")) +
  scale_fill_met_d("Hokusai3") 


## TLP
ggplot(data = df, aes(x = treat, y = TLP.MPa)) +
  geom_boxplot(na.rm = TRUE, aes(fill = species)) +
  geom_hline(data = tlp.adult.sum, aes(yintercept = mean.adult.tlp), color = "brown", size = 2) + 
  facet_grid(~species, labeller = labeller(species = sp.labs)) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(x = "Microsite", 
       y = expression(Psi[TLP] ~ (MPa))) +
  theme(axis.text.y = element_text(size = 24, color = "black"),
        axis.text.x = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 26),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),
        panel.background = element_rect(fill = NA, colour = "black"),
        panel.spacing = unit(0, "mm"), 
        axis.line = element_line(color = 'black'),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 26),
        strip.placement = "outside", 
        strip.text = element_text(size = 20)) +
  guides(fill = "none") +
  scale_x_discrete(labels = c("shade" = "Shade", "sun" = "Sun")) +
  scale_fill_met_d("Hokusai3")


## TLeaf
ggplot(data = tleaf.sum, aes(x = treat, y = mean.tleaf, fill=species)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean.tleaf-se.tleaf, ymax=mean.tleaf+se.tleaf), width=0.2) +
  facet_grid(~species) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(x = "Burn severity", 
       y= "Leaf Temperature (°C)") +
  theme(axis.text.y = element_text(size = 24, color = "black"),
        axis.text.x = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 26),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),
        panel.background = element_rect(fill = NA, colour = "black"),
        panel.spacing = unit(0, "mm"), 
        axis.line = element_line(color = 'black'),
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 26),
        strip.placement = "outside", 
        strip.text = element_text(size = 20)) +
  guides(fill = "none") +
  scale_x_discrete(labels = c("shade" = "Low", "sun" = "High")) +
  scale_y_continuous(expand=c(0,0), limits=c(0,40)) +
  scale_fill_met_d("Hokusai3")


## Thermal (tcrit, t50, t95)
# Reaction norms - Thermal
df.therm2 <- df.therm.sum %>%
  select(site, species, treat, mean.tcrit, mean.t50, mean.t95, se.tcrit, se.t50, 
         se.t95)

# Reshape the dataframe to long format
df.therm.long <- df.therm2 %>%
  pivot_longer(
    cols = -c(site, species, treat),
    names_to = c(".value", "trait"),
    names_pattern = c("([^\\.]+)\\.([^\\.]+)"))

# Find mean daily high temps for thermal fig
df %>% 
  group_by(site, species, treat) %>%
  dplyr::summarise(mean.leaf.temp.c = mean(mean.leaf.temp.c, na.rm = TRUE))

df.therm.long$trait <- factor(df.therm.long$trait, levels = c("tcrit", "t50", "t95"))

ggplot(data = df.therm.long, aes(x=treat, y=mean, color = trait, group=trait)) +
  geom_point(size=7, na.rm=T) +
  geom_errorbar(na.rm=T, aes(x=treat, ymin=mean - se, ymax=mean + se, color=trait), 
                width=.2, size=2) +
  geom_line(size=2) +
  facet_grid(~species, labeller = labeller(species = sp.labs)) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x= "Microsite", 
       y= "Temperature (°C)", 
       color="Temperature \nPoint") +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, .016)) +
  scale_x_discrete(labels = c("shade"="Shade", "sun" = "Sun")) +
  scale_colour_manual(values = met.brewer("Greek", 3, direction = -1),
                      labels = c("Tcrit", "T50", "T95"))


## Revised for SEGI meeting (5/1/24) - Filter the dataframe to include only Tcrit values
df_tcrit <- df.therm.long %>% 
  filter(trait == "tcrit", species %in% c("abco", "pila", "segi"))


# Plot with filtered dataframe
ggplot(data = df_tcrit, aes(x=treat, y=mean, color = trait, group=trait)) +
  geom_point(size=7, na.rm=T) +
  geom_errorbar(na.rm=T, aes(x=treat, ymin=mean - se, ymax=mean + se, color=trait), 
                width=.2, size=2) +
  geom_line(size=2) +
  facet_grid(~species, labeller = labeller(species = sp.labs)) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x= "Microsite", 
       y= "Temp when damage\n occurs (°C)") +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, .016)) +
  scale_x_discrete(labels = c("shade"="Shade", "sun" = "Sun")) +
  scale_colour_manual(values = met.brewer("Greek", 3, direction = -1),
                      labels = c("Tcrit", "T50", "T95"), guide="none")


# Revised for CalFIRE proposal
df.short <- df.therm.long %>%
  filter(species %in% c("abco","pila"))

ggplot(data = df.short, aes(x=treat, y=mean, color = trait, group=trait)) +
  geom_point(size=7, na.rm=T) +
  geom_errorbar(na.rm=T, aes(x=treat, ymin=mean - se, ymax=mean + se, color=trait), 
                width=.2, size=2) +
  geom_line(size=2) +
  facet_grid(~species, labeller = labeller(species = sp.labs)) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x= "Burn Severity", 
       y= "Temperature (°C)", 
       color="Temp \nDamage \nThreshold") +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, .016)) +
  scale_x_discrete(labels = c("shade"="Low", "sun" = "High")) +
  scale_colour_manual(values = met.brewer("Greek", 3, direction = -1),
                      labels = c("Tcrit", "T50", "T95"))


## Safety margins
# Ks
ggplot(data = df.therm.sum, aes(x=(-mean.Ks), y=mean.therm.safety.tcrit.tday)) +
  geom_point(size=7, na.rm=T, aes(color = species, shape = treat)) +
  geom_errorbar(na.rm=T, aes(ymin=mean.therm.safety.tcrit.tday - se.therm.safety.tcrit.tday, 
                             ymax=mean.therm.safety.tcrit.tday + se.therm.safety.tcrit.tday, color=species)) +
  geom_errorbar(na.rm=T, aes(xmin=(-mean.Ks) - se.Ks, 
                             xmax=(-mean.Ks) + se.Ks, color=species)) +
  geom_smooth(method="lm", se=F, fullrange = T, linetype="dashed") +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(after_stat(rr.label),
                                 after_stat(p.value.label), sep = "*\", \"*")),
               parse = TRUE, size=7, label.x="right",label.y=0.9) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x=expression(atop(K[s-native], paste(~ (kg ~ m^-1 ~ kPa^-1 ~ s^-1)))), 
       y= "Thermal Safety Margin (Tcrit - Max Tleaf, °C)", 
       color="Species") +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, .016)) +
  scale_colour_manual(values = met.brewer("Hokusai3", 5),
                      labels = c("ABCO", "CADE", "PILA", "PIPO", "SEGI"))


ggplot(data = df.therm.sum, aes(x=(-mean.Ks), y=mean.therm.margin)) +
  geom_point(size=7, na.rm=T, aes(color = species, shape = treat)) +
  geom_errorbar(na.rm=T, aes(ymin=mean.therm.margin - se.therm.margin, 
                             ymax=mean.therm.margin + se.therm.margin, color=species)) +
  geom_errorbar(na.rm=T, aes(xmin=(-mean.Ks) - se.Ks, 
                             xmax=(-mean.Ks) + se.Ks, color=species)) +
  geom_smooth(method="lm", se=F, fullrange = T, linetype="dashed") +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(after_stat(rr.label),
                                 after_stat(p.value.label), sep = "*\", \"*")),
               parse = TRUE, size=7, label.x="right",label.y=0.9) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x=expression(atop(K[s-native], paste(~ (kg ~ m^-1 ~ kPa^-1 ~ s^-1)))), 
       y= "Thermal Margin (T95 - Tcrit, °C)", 
       color="Species") +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, .016)) +
  scale_colour_manual(values = met.brewer("Hokusai3", 5),
                      labels = c("ABCO", "CADE", "PILA", "PIPO", "SEGI"))


ggplot(data = df.therm.sum, aes(x=(-mean.Ks), y=mean.therm.margin.low)) +
  geom_point(size=7, na.rm=T, aes(color = species, shape = treat)) +
  geom_errorbar(na.rm=T, aes(ymin=mean.therm.margin.low - se.therm.margin.low, 
                             ymax=mean.therm.margin.low + se.therm.margin.low, color=species)) +
  geom_errorbar(na.rm=T, aes(xmin=(-mean.Ks) - se.Ks, 
                             xmax=(-mean.Ks) + se.Ks, color=species)) +
  geom_smooth(method="lm", se=F, fullrange = T, linetype="dashed") +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(after_stat(rr.label),
                                 after_stat(p.value.label), sep = "*\", \"*")),
               parse = TRUE, size=7, label.x="right",label.y=0.9) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x=expression(atop(K[s-native], paste(~ (kg ~ m^-1 ~ kPa^-1 ~ s^-1)))), 
       y= "Thermal Margin Low (T50 - Tcrit, °C)", 
       color="Species") +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, .016)) +
  scale_colour_manual(values = met.brewer("Hokusai3", 5),
                      labels = c("ABCO", "CADE", "PILA", "PIPO", "SEGI"))


# TLP
ggplot(data = df.therm.sum, aes(x=mean.TLP.MPa, y=mean.therm.safety.tcrit)) +
  geom_point(size=7, na.rm=T, aes(color = species, shape = treat)) +
  geom_errorbar(na.rm=T, aes(ymin=mean.therm.safety.tcrit - se.therm.safety.tcrit, 
                             ymax=mean.therm.safety.tcrit + se.therm.safety.tcrit, color=species)) +
  geom_errorbar(na.rm=T, aes(xmin=mean.TLP.MPa - se.TLP.MPa, 
                             xmax=mean.TLP.MPa + se.TLP.MPa, color=species)) +
  geom_smooth(method="lm", se=F, fullrange = T, linetype="dashed") +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(after_stat(rr.label),
                                 after_stat(p.value.label), sep = "*\", \"*")),
               parse = TRUE, size=7, label.x="right",label.y=0.9) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x=expression(Psi[TLP] ~ (MPa)), 
       y= "Thermal Safety Margin (Tcrit - Max Tleaf, °C)", 
       color="Species") +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, .016)) +
  scale_colour_manual(values = met.brewer("Hokusai3", 5),
                      labels = c("ABCO", "CADE", "PILA", "PIPO", "SEGI"))


# Tcrit versus thermal margins
ggplot(data = df.therm.sum, aes(x=mean.tcrit, y=mean.therm.margin)) +
  geom_point(size=7, na.rm=T, aes(color = species, shape = treat)) +
  geom_errorbar(na.rm=T, aes(ymin=mean.therm.margin - se.therm.margin, 
                             ymax=mean.therm.margin + se.therm.margin, color=species)) +
  geom_errorbar(na.rm=T, aes(xmin=mean.tcrit - se.tcrit, 
                             xmax=mean.tcrit + se.tcrit, color=species)) +
  geom_smooth(method="lm", se=T, fullrange = T, linetype="solid") +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(after_stat(rr.label),
                                 after_stat(p.value.label), sep = "*\", \"*")),
               parse = TRUE, size=7, label.x="right",label.y=0.9) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x="Tcrit (°C)", 
       y= "Thermal Margin (T95 - Tcrit, °C)", 
       color="Species", shape="Treatment") +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, .016)) +
  scale_colour_manual(values = met.brewer("Hokusai3", 5),
                      labels = c("ABCO", "CADE", "PILA", "PIPO", "SEGI"))


ggplot(data = df.therm.sum, aes(y=mean.tcrit, x=mean.therm.margin.low)) +
  geom_point(size=7, na.rm=T, aes(color = species, shape = treat)) +
  geom_errorbar(na.rm=T, aes(xmin=mean.therm.margin.low - se.therm.margin.low, 
                             xmax=mean.therm.margin.low + se.therm.margin.low, color=species)) +
  geom_errorbar(na.rm=T, aes(ymin=mean.tcrit - se.tcrit, 
                             ymax=mean.tcrit + se.tcrit, color=species)) +
  geom_smooth(method="lm", se=T, fullrange = T, linetype="solid") +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(after_stat(rr.label),
                                 after_stat(p.value.label), sep = "*\", \"*")),
               parse = TRUE, size=7, label.x="right",label.y=0.9) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(y="Tcrit (°C)", 
       x= "Thermal Margin Low (T50 - Tcrit, °C)", 
       color="Species") +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, .016)) +
  scale_colour_manual(values = met.brewer("Hokusai3", 5),
                      labels = c("ABCO", "CADE", "PILA", "PIPO", "SEGI"))

ggplot(data = df.therm.sum, aes(y=mean.tcrit, x=mean.therm.margin.high)) +
  geom_point(size=7, na.rm=T, aes(color = species, shape = treat)) +
  geom_errorbar(na.rm=T, aes(xmin=mean.therm.margin.high - se.therm.margin.high, 
                             xmax=mean.therm.margin.high + se.therm.margin.high, color=species)) +
  geom_errorbar(na.rm=T, aes(ymin=mean.tcrit - se.tcrit, 
                             ymax=mean.tcrit + se.tcrit, color=species)) +
  geom_smooth(method="lm", se=T, fullrange = T, linetype="solid") +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(after_stat(rr.label),
                                 after_stat(p.value.label), sep = "*\", \"*")),
               parse = TRUE, size=7, label.x="right",label.y=0.9) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(y="Tcrit (°C)", 
       x= "Thermal Margin High (T95 - T50, °C)", 
       color="Species") +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, .016)) +
  scale_colour_manual(values = met.brewer("Hokusai3", 5),
                      labels = c("ABCO", "CADE", "PILA", "PIPO", "SEGI"))

# Just safety margins (tcrit - max tleaf, tday, tseason)
ggplot(data = df.therm.sum, aes(x=treat, y=mean.therm.safety.tcrit.tleaf, fill=species)) +
  geom_bar(size=7, na.rm = T, stat="identity") + 
  geom_errorbar(na.rm=T, aes(ymin=mean.therm.safety.tcrit.tleaf - se.therm.safety.tcrit.tleaf, 
                             ymax=mean.therm.safety.tcrit.tleaf + se.therm.safety.tcrit.tleaf), width=0.2) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x="Microsite", 
       y= "Thermal Safety Margin (Tcrit - Max Tleaf, °C)", 
       fill="Species") +
  facet_grid(~species, labeller = labeller(species = sp.labs)) +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  guides(fill="none") +
  scale_y_continuous(expand=c(0,0), limits=c(0, 20)) +
  scale_fill_manual(values = met.brewer("Hokusai3", 5),
                      labels = c("ABCO", "CADE", "PILA", "PIPO", "SEGI")) +
  scale_x_discrete(labels = c("shade"="Shade", "sun" = "Sun")) 

# Revised for CalFIRE proposal
df.tsm.short <- df.therm.sum %>%
  filter(species %in% c("abco","pila"))

ggplot(data = df.tsm.short, aes(x=treat, y=mean.therm.safety.tcrit.tleaf, fill=species)) +
  geom_bar(size=7, na.rm = T, stat="identity") + 
  geom_errorbar(na.rm=T, aes(ymin=mean.therm.safety.tcrit.tleaf - se.therm.safety.tcrit.tleaf, 
                             ymax=mean.therm.safety.tcrit.tleaf + se.therm.safety.tcrit.tleaf), width=0.2) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x="Burn Severity", 
       y= "Thermal Safety Margin (Tcrit - Max Tleaf, °C)", 
       fill="Species") +
  facet_grid(~species, labeller = labeller(species = sp.labs)) +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black", angle=90),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  guides(fill="none") +
  scale_y_continuous(expand=c(0,0), limits=c(0, 20)) +
  scale_fill_manual(values = met.brewer("Hokusai3", 5),
                    labels = c("ABCO", "PILA")) +
  scale_x_discrete(labels = c("shade"="Low", "sun" = "High")) 

ggplot(data = df.therm.sum, aes(x=treat, y=mean.therm.safety.tcrit.tday, fill=species)) +
  geom_bar(size=7, na.rm = T, stat="identity") + 
  geom_errorbar(na.rm=T, aes(ymin=mean.therm.safety.tcrit.tday - se.therm.safety.tcrit.tday, 
                             ymax=mean.therm.safety.tcrit.tday + se.therm.safety.tcrit.tday), width=0.2) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x="Microsite", 
       y= "Thermal Safety Margin (Tcrit - Max Tair of Day, °C)", 
       fill="Species") +
  facet_grid(~species, labeller = labeller(species = sp.labs)) +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  guides(fill="none") +
  scale_y_continuous(expand=c(0,0), limits=c(0, 20)) +
  scale_fill_manual(values = met.brewer("Hokusai3", 5),
                    labels = c("ABCO", "CADE", "PILA", "PIPO", "SEGI")) +
  scale_x_discrete(labels = c("shade"="Shade", "sun" = "Sun")) 

# Revised for CalFIRE
ggplot(data = df.tsm.short, aes(x=treat, y=mean.therm.safety.tcrit.tday, fill=species)) +
  geom_bar(size=7, na.rm = T, stat="identity") + 
  geom_errorbar(na.rm=T, aes(ymin=mean.therm.safety.tcrit.tday - se.therm.safety.tcrit.tday, 
                             ymax=mean.therm.safety.tcrit.tday + se.therm.safety.tcrit.tday), width=0.2) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x="Burn Severity", 
       y= "Thermal Safety Margin (Tcrit - Max Tair, °C)", 
       fill="Species") +
  facet_grid(~species, labeller = labeller(species = sp.labs)) +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  guides(fill="none") +
  scale_y_continuous(expand=c(0,0), limits=c(0, 20)) +
  scale_fill_manual(values = met.brewer("Hokusai3", 5),
                    labels = c("ABCO", "PILA")) +
  scale_x_discrete(labels = c("shade"="Low", "sun" = "High"))

ggplot(data = df.therm.sum, aes(x=treat, y=mean.therm.safety.tcrit.tseason, fill=species)) +
  geom_bar(size=7, na.rm = T, stat="identity") + 
  geom_errorbar(na.rm=T, aes(ymin=mean.therm.safety.tcrit.tseason - se.therm.safety.tcrit.tseason, 
                             ymax=mean.therm.safety.tcrit.tseason + se.therm.safety.tcrit.tseason), width=0.2) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x="Microsite", 
       y= "Thermal Safety Margin (Tcrit - Max Tair, °C)", 
       fill="Species") +
  facet_grid(~species, labeller = labeller(species = sp.labs)) +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  guides(fill="none") +
  scale_y_continuous(expand=c(0,0), limits=c(-5, 20)) +
  scale_fill_manual(values = met.brewer("Hokusai3", 5),
                    labels = c("ABCO", "CADE", "PILA", "PIPO", "SEGI")) +
  scale_x_discrete(labels = c("shade"="Shade", "sun" = "Sun")) 



# Revised for CalFIRE
ggplot(data = df.tsm.short, aes(x=treat, y=mean.therm.safety.tcrit.tseason, fill=species)) +
  geom_bar(size=7, na.rm = T, stat="identity") + 
  geom_errorbar(na.rm=T, aes(ymin=mean.therm.safety.tcrit.tseason - se.therm.safety.tcrit.tseason, 
                             ymax=mean.therm.safety.tcrit.tseason + se.therm.safety.tcrit.tseason), width=0.2) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x="Burn Severity", 
       y= "Thermal Safety Margin (Tcrit - Max Tair, °C)", 
       fill="Species") +
  facet_grid(~species, labeller = labeller(species = sp.labs)) +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black", angle=90),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  guides(fill="none") +
  scale_y_continuous(expand=c(0,0), limits=c(-5, 20)) +
  scale_fill_manual(values = met.brewer("Hokusai3", 5),
                    labels = c("ABCO", "PILA")) +
  scale_x_discrete(labels = c("shade"="Low", "sun" = "High")) 

## Trade-offs btw heat and drought tolerance
# TLP vs Tcrit
ggplot(data = df.therm.sum, aes(y=mean.TLP.MPa, x=mean.tcrit)) +
  geom_point(size=7, na.rm=T, aes(color = species, shape = treat)) +
  geom_errorbar(na.rm=T, aes(xmin=mean.tcrit - se.tcrit, 
                             xmax=mean.tcrit + se.tcrit, color=species)) +
  geom_errorbar(na.rm=T, aes(ymin=mean.TLP.MPa - se.TLP.MPa, 
                             ymax=mean.TLP.MPa + se.TLP.MPa, color=species)) +
  geom_smooth(method="lm", se=F, fullrange = T, linetype="dashed") +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(after_stat(rr.label),
                                 after_stat(p.value.label), sep = "*\", \"*")),
               parse = TRUE, size=7, label.x="left",label.y=0.9) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(y=expression(Psi[TLP] ~ (MPa)), 
       x= "Tcrit (°C)", 
       color="Species") +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, .016)) +
  scale_colour_manual(values = met.brewer("Hokusai3", 5),
                      labels = c("ABCO", "CADE", "PILA", "PIPO", "SEGI"))

# TLP vs T50
ggplot(data = df.therm.sum, aes(y=mean.TLP.MPa, x=mean.t50)) +
  geom_point(size=7, na.rm=T, aes(color = species, shape = treat)) +
  geom_errorbar(na.rm=T, aes(xmin=mean.t50 - se.t50, 
                             xmax=mean.t50 + se.t50, color=species)) +
  geom_errorbar(na.rm=T, aes(ymin=mean.TLP.MPa - se.TLP.MPa, 
                             ymax=mean.TLP.MPa + se.TLP.MPa, color=species)) +
  geom_smooth(method="lm", se=F, fullrange = T, linetype="dashed") +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(after_stat(rr.label),
                                 after_stat(p.value.label), sep = "*\", \"*")),
               parse = TRUE, size=7, label.x="left",label.y=0.9) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(y=expression(Psi[TLP] ~ (MPa)), 
       x= "T50 (°C)", 
       color="Species") +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, .016)) +
  scale_colour_manual(values = met.brewer("Hokusai3", 5),
                      labels = c("ABCO", "CADE", "PILA", "PIPO", "SEGI"))

# TLP vs T95
ggplot(data = df.therm.sum, aes(y=mean.TLP.MPa, x=mean.t95)) +
  geom_point(size=7, na.rm=T, aes(color = species, shape = treat)) +
  geom_errorbar(na.rm=T, aes(xmin=mean.t95 - se.t95, 
                             xmax=mean.t95 + se.t95, color=species)) +
  geom_errorbar(na.rm=T, aes(ymin=mean.TLP.MPa - se.TLP.MPa, 
                             ymax=mean.TLP.MPa + se.TLP.MPa, color=species)) +
  geom_smooth(method="lm", se=F, fullrange = T, linetype="dashed") +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(after_stat(rr.label),
                                 after_stat(p.value.label), sep = "*\", \"*")),
               parse = TRUE, size=7, label.x="right",label.y=0.9) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(y=expression(Psi[TLP] ~ (MPa)), 
       x= "T95 (°C)", 
       color="Species") +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, .016)) +
  scale_colour_manual(values = met.brewer("Hokusai3", 5),
                      labels = c("ABCO", "CADE", "PILA", "PIPO", "SEGI"))

# Ks vs Tcrit
ggplot(data = df.therm.sum, aes(x=(-mean.Ks), y=mean.tcrit)) +
  geom_point(size=7, na.rm=T, aes(color = species, shape = treat)) +
  geom_errorbar(na.rm=T, aes(ymin=mean.tcrit - se.tcrit, 
                             ymax=mean.tcrit + se.tcrit, color=species)) +
  geom_errorbar(na.rm=T, aes(xmin=(-mean.Ks) - se.Ks, 
                             xmax=(-mean.Ks) + se.Ks, color=species)) +
  geom_smooth(method="lm", se=T, fullrange = T, linetype="solid") +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(after_stat(rr.label),
                                 after_stat(p.value.label), sep = "*\", \"*")),
               parse = TRUE, size=7, label.x="left",label.y=0.9) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x=expression(atop(K[s-native], paste(~ (kg ~ m^-1 ~ kPa^-1 ~ s^-1)))), 
       y= "Tcrit (°C)", 
       color="Species") +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, .016)) +
  scale_colour_manual(values = met.brewer("Hokusai3", 5),
                      labels = c("ABCO", "CADE", "PILA", "PIPO", "SEGI"))

# Ks vs T50
ggplot(data = df.therm.sum, aes(x=(-mean.Ks), y=mean.t50)) +
  geom_point(size=7, na.rm=T, aes(color = species, shape = treat)) +
  geom_errorbar(na.rm=T, aes(ymin=mean.t50 - se.t50, 
                             ymax=mean.t50 + se.t50, color=species)) +
  geom_errorbar(na.rm=T, aes(xmin=(-mean.Ks) - se.Ks, 
                             xmax=(-mean.Ks) + se.Ks, color=species)) +
  geom_smooth(method="lm", se=F, fullrange = T, linetype="dashed") +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(after_stat(rr.label),
                                 after_stat(p.value.label), sep = "*\", \"*")),
               parse = TRUE, size=7, label.x="right",label.y=0.9) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x=expression(atop(K[s-native], paste(~ (kg ~ m^-1 ~ kPa^-1 ~ s^-1)))), 
       y= "T50 (°C)", 
       color="Species") +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, .016)) +
  scale_colour_manual(values = met.brewer("Hokusai3", 5),
                      labels = c("ABCO", "CADE", "PILA", "PIPO", "SEGI"))

# Ks vs T95
ggplot(data = df.therm.sum, aes(x=(-mean.Ks), y=mean.t95)) +
  geom_point(size=7, na.rm=T, aes(color = species, shape = treat)) +
  geom_errorbar(na.rm=T, aes(ymin=mean.t95 - se.t95, 
                             ymax=mean.t95 + se.t95, color=species)) +
  geom_errorbar(na.rm=T, aes(xmin=(-mean.Ks) - se.Ks, 
                             xmax=(-mean.Ks) + se.Ks, color=species)) +
  geom_smooth(method="lm", se=F, fullrange = T, linetype="dashed") +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(after_stat(rr.label),
                                 after_stat(p.value.label), sep = "*\", \"*")),
               parse = TRUE, size=7, label.x="right",label.y=0.9) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x=expression(atop(K[s-native], paste(~ (kg ~ m^-1 ~ kPa^-1 ~ s^-1)))), 
       y= "T95 (°C)", 
       color="Species") +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, .016)) +
  scale_colour_manual(values = met.brewer("Hokusai3", 5),
                      labels = c("ABCO", "CADE", "PILA", "PIPO", "SEGI"))

# TLP vs Ks
ggplot(data = df.therm.sum, aes(y=mean.TLP.MPa, x=(-mean.Ks))) +
  geom_point(size=7, na.rm=T, aes(color = species, shape = treat)) +
  geom_errorbar(na.rm=T, aes(xmin=(-mean.Ks) - se.Ks, 
                             xmax=(-mean.Ks) + se.Ks, color=species)) +
  geom_errorbar(na.rm=T, aes(ymin=mean.TLP.MPa - se.TLP.MPa, 
                             ymax=mean.TLP.MPa + se.TLP.MPa, color=species)) +
  geom_smooth(method="lm", se=T, fullrange = T, linetype="solid") +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(after_stat(rr.label),
                                 after_stat(p.value.label), sep = "*\", \"*")),
               parse = TRUE, size=7, label.x="right",label.y=0.9) +
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(y=expression(Psi[TLP] ~ (MPa)), 
       x= expression(atop(K[s-native], paste(~ (kg ~ m^-1 ~ kPa^-1 ~ s^-1)))), 
       color="Species") +
  theme(axis.text.y=element_text(size=24, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        axis.title=element_text(size=26),
        axis.title.y = element_text(margin = unit(c(0,3,0,0), "mm")),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.spacing = unit(0,"mm"), 
        axis.line = element_line(color = 'black'),
        legend.text=element_text(size=24),
        legend.title=element_text(size=26),
        strip.placement = "outside", 
        strip.text = element_text(size=20)) +
  # scale_y_continuous(expand=c(0,0), limits=c(0, .016)) +
  scale_colour_manual(values = met.brewer("Hokusai3", 5),
                      labels = c("ABCO", "CADE", "PILA", "PIPO", "SEGI"))



### 5) Statistics
## Al.As 
alas.mod1 <- lmer(al.as.cm2.mm2 ~ species * treat + (1|plant), data = df, REML=F) 
plot(fitted(alas.mod1), residuals(alas.mod1))
hist(residuals(alas.mod1))
qqPlot(residuals(alas.mod1)) 
# Build reduced models to test effects of fixed effects
alas.mod1.treat <- lmer(al.as.cm2.mm2 ~ treat + (1|plant), data = df, REML=F) 
alas.mod1.species <- lmer(al.as.cm2.mm2 ~ species + (1|plant), data = df, REML=F) 
alas.mod1.reduced <- lmer(al.as.cm2.mm2 ~ species + treat + (1|plant), data = df, REML=F) 
anova(alas.mod1, alas.mod1.treat) #Species affects AL:AS (chisq=56.514, p=2.24e-09 ***)
anova(alas.mod1, alas.mod1.species) #Treat does NOT AL:AS (chisq=7.2933, p=0.1997)
anova(alas.mod1, alas.mod1.reduced) #Interaction is sig (chisq=6.6195, p=0.1574)
# Remove REML for final testing
alas.mod2 <- lmer(al.as.cm2.mm2 ~ species + treat + (1|plant), data = df)
emmeans(alas.mod2, ~ species + treat, type="response")
# 
pairs(emmeans(alas.mod2, ~ treat | species))
# 
pairs(emmeans(alas.mod2, ~ species + treat))
# contrast                estimate   SE   df t.ratio p.value
# abco shade - pipo shade  -15.070 2.53 39.5  -5.963  <.0001
# cade shade - pipo shade  -18.182 2.52 37.6  -7.207  <.0001
# pila shade - pipo shade  -19.500 2.52 37.4  -7.741  <.0001
# pipo shade - segi shade   12.068 2.52 37.6   4.787  0.0010
# abco sun - pipo sun      -15.070 2.53 39.5  -5.963  <.0001
# cade sun - pipo sun      -18.182 2.52 37.6  -7.207  <.0001
# pila sun - pipo sun      -19.500 2.52 37.4  -7.741  <.0001
# pipo sun - segi sun       12.068 2.52 37.6   4.787  0.0010
# 
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: tukey method for comparing a family of 10 estimates  
# 


## LMA 
lma.mod1 <- lmer(lma.g.cm2 ~ species * treat + (1|plant), data = df, REML=F) 
plot(fitted(lma.mod1), residuals(lma.mod1))
hist(residuals(lma.mod1))
qqPlot(residuals(lma.mod1)) 
# Build reduced models to test effects of fixed effects
lma.mod1.treat <- lmer(lma.g.cm2 ~ treat + (1|plant), data = df, REML=F) 
lma.mod1.species <- lmer(lma.g.cm2 ~ species + (1|plant), data = df, REML=F) 
lma.mod1.reduced <- lmer(lma.g.cm2 ~ species + treat + (1|plant), data = df, REML=F) 
anova(lma.mod1, lma.mod1.treat) #Species affects LMA (chisq=57.932, p=1.184e-09 ***)
anova(lma.mod1, lma.mod1.species) #Treat affects LMA (chisq=22.288, p=0.0004615 ***)
anova(lma.mod1, lma.mod1.reduced) #Interaction is sig (chisq=14.658, p=0.005466 **)
# Remove REML for final testing
lma.mod2 <- lmer(lma.g.cm2 ~ species * treat + (1|plant), data = df) 
emmeans(lma.mod2, ~ species + treat, type="response")
# 
pairs(emmeans(lma.mod2, ~ treat | species))
# species = segi:
# contrast     estimate      SE   df t.ratio p.value
# shade - sun -5.29e-03 0.00118 38.4  -4.465  0.0001
pairs(emmeans(lma.mod2, ~ species * treat))
# contrast                 estimate      SE   df t.ratio p.value
# abco shade - pipo shade  5.29e-03 0.00118 38.4   4.469  0.0024
# cade shade - pipo shade  5.73e-03 0.00119 38.4   4.832  0.0008
# segi shade - segi sun   -5.29e-03 0.00118 38.4  -4.465  0.0025
# abco sun - pipo sun      5.43e-03 0.00118 38.4   4.610  0.0016
# cade sun - pila sun      3.64e-03 0.00119 38.4   3.069  0.0977
# cade sun - pipo sun      6.26e-03 0.00117 36.6   5.351  0.0002
# pila sun - segi sun     -4.84e-03 0.00117 36.6  -4.126  0.0069
# pipo sun - segi sun     -7.46e-03 0.00118 38.4  -6.315  <.0001
# 
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: tukey method for comparing a family of 10 estimates 
# 


## Ks
Ks.mod1 <- lmer(Ks ~ species * treat + (1|plant), data = df, REML=F) 
plot(fitted(Ks.mod1), residuals(Ks.mod1))
hist(residuals(Ks.mod1))
qqPlot(residuals(Ks.mod1)) 
# Build reduced models to test effects of fixed effects
Ks.mod1.treat <- lmer(Ks ~ treat + (1|plant), data = df, REML=F) 
Ks.mod1.species <- lmer(Ks ~ species + (1|plant), data = df, REML=F) 
Ks.mod1.reduced <- lmer(Ks ~ species + treat + (1|plant), data = df, REML=F) 
anova(Ks.mod1, Ks.mod1.treat) #Species affects LMA (chisq=17.929, p=0.02176 *)
anova(Ks.mod1, Ks.mod1.species) #Treat affects LMA (chisq=5.8953, p=0.3165)
anova(Ks.mod1, Ks.mod1.reduced) #Interaction is NOT sig (chisq=5.7729, p=0.2168)
# Remove REML for final testing
Ks.mod2 <- lmer(Ks ~ species * treat + (1|plant), data = df) 
pairs(emmeans(Ks.mod2, ~ treat | species))
# species = pila:
#   contrast    estimate    SE   df t.ratio p.value
# shade - sun   0.6110 0.339 16.0   1.802  0.0904
pairs(emmeans(Ks.mod2, ~ species * treat))
# 


## TLP
TLPmod1 <- lmer(TLP.MPa ~ species * treat + (1|plant), data = df, REML=F) 
plot(fitted(TLPmod1), residuals(TLPmod1))
hist(residuals(TLPmod1))
qqPlot(residuals(TLPmod1)) 
# Build reduced models to test effects of fixed effects
TLPmod1.treat <- lmer(TLP.MPa ~ treat + (1|plant), data = df, REML=F) 
TLPmod1.species <- lmer(TLP.MPa ~ species + (1|plant), data = df, REML=F) 
TLPmod1.reduced <- lmer(TLP.MPa ~ species + treat + (1|plant), data = df, REML=F) 
anova(TLPmod1, TLPmod1.treat) #Species affects LMA (chisq=36.816, p=1.244e-05 ***)
anova(TLPmod1, TLPmod1.species) #Treat affects LMA (chisq=24.166, p=0.0002017 ***)
anova(TLPmod1, TLPmod1.reduced) #Interaction is NOT sig (chisq=15.72, p=0.003418 **)
# Remove REML for final testing
TLPmod2 <- lmer(TLP.MPa ~ species * treat + (1|plant), data = df) 
pairs(emmeans(TLPmod2, ~ treat | species))
# species = abco:
# contrast    estimate    SE   df t.ratio p.value
# shade - sun   -0.531 0.150 39.6  -3.537  0.0010
# 
# species = pila:
#   contrast    estimate    SE   df t.ratio p.value
# shade - sun   -0.423 0.147 36.6  -2.876  0.0067
# 
pairs(emmeans(TLPmod2, ~ species * treat))
#  contrast                estimate    SE   df t.ratio p.value
# abco shade - pipo shade  -0.6824 0.148 38.4  -4.610  0.0016
# abco shade - abco sun    -0.5306 0.150 39.6  -3.537  0.0312
# cade shade - pipo shade  -0.5854 0.148 38.4  -3.949  0.0107
# pila shade - pipo shade  -0.6384 0.147 36.6  -4.344  0.0037
# pila shade - pila sun    -0.4232 0.147 36.6  -2.876  0.1495
# 
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: tukey method for comparing a family of 10 estimates 


## LMA 
mod1 <- lmer(therm.safety.tcrit.tleaf ~ species * treat + (1|plant), data = df, REML=F) 
plot(fitted(mod1), residuals(mod1))
hist(residuals(mod1))
qqPlot(residuals(mod1)) 
# Build reduced models to test effects of fixed effects
mod1.treat <- lmer(therm.safety.tcrit.tleaf ~ treat + (1|plant), data = df, REML=F) 
mod1.species <- lmer(therm.safety.tcrit.tleaf ~ species + (1|plant), data = df, REML=F) 
mod1.reduced <- lmer(therm.safety.tcrit.tleaf ~ species + treat + (1|plant), data = df, REML=F) 
anova(mod1, mod1.treat) #Species affects LMA (chisq=13.462, p=0.03626 *)
anova(mod1, mod1.species) #Treat affects LMA (chisq=25.922, p=3.281e-05 ***)
anova(mod1, mod1.reduced) #Interaction is NOT sig (chisq=3.0984, p=0.3767)
# Remove REML for final testing
mod2 <- lmer(therm.safety.tcrit.tleaf ~ species * treat + (1|plant), data = df) 
pairs(emmeans(mod2, ~ treat | species))
# species = abco:
#   contrast    estimate   SE   df t.ratio p.value
# shade - sun     6.82 1.99 29.6   3.421  0.0018
# 
# species = cade:
#   contrast    estimate   SE   df t.ratio p.value
# shade - sun     6.54 2.35 30.0   2.779  0.0093
# 
# species = pila:
#   contrast    estimate   SE   df t.ratio p.value
# shade - sun     4.54 1.96 26.6   2.315  0.0286
# 
# species = segi:
#   contrast    estimate   SE   df t.ratio p.value
# shade - sun     2.88 2.00 28.4   1.442  0.1603
# 
# pairs(emmeans(mod2, ~ species * treat))
# 


## HOBOs
hobo <- read.csv("./seedling_hobo_combined.csv")

hobo$time2 <- paste(hobo$time, hobo$am.pm, sep=" ")

hobo$date <- as.Date(hobo$date, format = "%m/%d/%y")
hobo$time_military <- format(strptime(hobo$time2, format = "%I:%M %p"), format = "%H:%M:%S")

# Define your start and end dates
start_date <- as.Date("2023-08-01")
end_date <- as.Date("2023-08-31")

start_time <- "12:00:00"
end_time <- "15:00:00"

# Filter the DataFrame between the start and end dates using dplyr
hobo_aug <- hobo %>%
  filter(date >= start_date & date <= end_date)

hobo_aug$time_military <- as.character(hobo_aug$time_military)
hobo_aug_midday <- hobo_aug %>%
  filter(time_military >= start_time & time_military <= end_time)

hobo_aug <- hobo_aug[hobo_aug$site != "", ]
hobo_aug_midday <- hobo_aug_midday[hobo_aug_midday$site != "", ]

hobo_aug_sum <- hobo_aug %>% 
  group_by(time_military, site) %>%
  dplyr::summarise(mean.temp_c = mean(temp_c, na.rm = TRUE),
                   median.temp_c = median(temp_c, na.rm=TRUE),
                   sd.temp_c = sd(temp_c, na.rm = TRUE),
                   n.temp_c = length(temp_c[!is.na(temp_c)]),
                   mean.relative_humidity = mean(relative_humidity, na.rm = TRUE),
                   median.relative_humidity = median(relative_humidity, na.rm=TRUE),
                   sd.relative_humidity = sd(relative_humidity, na.rm = TRUE),
                   n.relative_humidity = length(relative_humidity[!is.na(relative_humidity)])) %>%
  mutate(se.temp_c = sd.temp_c / sqrt(n.temp_c),
         lowerci.temp_c = mean.temp_c - qt(1 - (0.05 / 2), n.temp_c - 1) * se.temp_c,
         upperci.temp_c = mean.temp_c + qt(1 - (0.05 / 2), n.temp_c - 1) * se.temp_c,
         cv.temp_c = sd.temp_c/mean.temp_c)

hobo_aug_midday_sum <- hobo_aug_midday %>% 
  group_by(time_military, site) %>%
  dplyr::summarise(mean.temp_c = mean(temp_c, na.rm = TRUE),
                   median.temp_c = median(temp_c, na.rm=TRUE),
                   sd.temp_c = sd(temp_c, na.rm = TRUE),
                   n.temp_c = length(temp_c[!is.na(temp_c)]),
                   mean.relative_humidity = mean(relative_humidity, na.rm = TRUE),
                   median.relative_humidity = median(relative_humidity, na.rm=TRUE),
                   sd.relative_humidity = sd(relative_humidity, na.rm = TRUE),
                   n.relative_humidity = length(relative_humidity[!is.na(relative_humidity)])) %>%
  mutate(se.temp_c = sd.temp_c / sqrt(n.temp_c),
         lowerci.temp_c = mean.temp_c - qt(1 - (0.05 / 2), n.temp_c - 1) * se.temp_c,
         upperci.temp_c = mean.temp_c + qt(1 - (0.05 / 2), n.temp_c - 1) * se.temp_c,
         cv.temp_c = sd.temp_c/mean.temp_c)

# hobo_aug_sum$time_military <- as.POSIXct(hobo_aug_sum$time_military, format = "%H:%M:%S")
hobo$time_military <- factor(hobo$time_military)

ggplot(hobo_aug, aes(x=time_military, y=temp_c, group = site)) +
  geom_smooth(aes(color=site)) +
  scale_x_discrete(limits=hobo_aug_sum$time_military, 
                   breaks=hobo_aug_sum$time_military[seq(1, length(hobo_aug_sum$time_military), by=12)]) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(hobo_aug_sum, aes(x=time_military, y=mean.temp_c, group = site)) +
  geom_smooth(aes(color=site)) +
  scale_x_discrete(limits=hobo_aug_sum$time_military, 
                   breaks=hobo_aug_sum$time_military[seq(1, length(hobo_aug_sum$time_military), by=12)]) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

august_summary <- hobo_aug_sum %>%
  group_by(site) %>%
  summarise(
    avg_temp = mean(mean.temp_c),
    min_temp = min(mean.temp_c),
    max_temp = max(mean.temp_c),
    avg_rh = mean(mean.relative_humidity),
    min_rh = min(mean.relative_humidity),
    max_rh = max(mean.relative_humidity)
  )  

august_summary

august_midday_summary <- hobo_aug_midday_sum %>%
  group_by(site) %>%
  summarise(
    avg_temp = mean(mean.temp_c),
    min_temp = min(mean.temp_c),
    max_temp = max(mean.temp_c),
    avg_rh = mean(mean.relative_humidity),
    min_rh = min(mean.relative_humidity),
    max_rh = max(mean.relative_humidity)
  )  

august_midday_summary
