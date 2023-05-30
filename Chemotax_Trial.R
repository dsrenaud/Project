
library(tidyverse)
library(ggplot2)
library(effects)
library(ggpubr)
library(broom)


#Dye Trial Data
DyeTrial <- read.csv("Dye measurement.csv")
summary(DyeTrial)
head(DyeTrial)

# converting'Type' and 'Plate.ID' into factors
DyeTrial$Type <- as.factor(DyeTrial$Type)
DyeTrial$Plate.ID <- as.factor(DyeTrial$Plate.ID)

# Calculate net growth 
DyeTrial$Net_growth <- DyeTrial$pos_distance - DyeTrial$neg_distance

# Adding column with Hour as a factor
DyeTrial$fac_hour <- as.factor(DyeTrial$Hour)

# COnverting distances into factors
DyeTrial$neg_edge <- as.factor(DyeTrial$neg_edge)
DyeTrial$pos_edge <- as.factor(DyeTrial$pos_edge)
DyeTrial$dye_edge <- as.factor(DyeTrial$dye_edge)


summary(DyeTrial)

# Subset to exclude chips once growth reached either edge
# (You'll need to note that this was done in your Methods, to explain why the sample size drops after x number of hours)

DyeTrial <- subset(DyeTrial, neg_edge == 'No')
DyeTrial <- subset(DyeTrial, pos_edge == 'No')

summary(DyeTrial)


#Egg Trial Data
EggTrial <- read.csv("egg_measurement2.csv")
summary(EggTrial)
head(EggTrial)

# Converting 'Type' and 'Plate.ID' into factors
EggTrial$Type <- as.factor(EggTrial$Type)
EggTrial$Plate.ID <- as.factor(EggTrial$Plate.ID)

# Calculate net growth 
EggTrial$Net_growth <- EggTrial$pos_distance - EggTrial$neg_distance

# Add column with Hour as a factor
EggTrial$fac_hour <- as.factor(EggTrial$Hour)

# Making distance a factor.
EggTrial$neg_edge <- as.factor(EggTrial$neg_edge)
EggTrial$pos_edge <- as.factor(EggTrial$pos_edge)


summary(EggTrial)

## Subset EggTrial to remove the contaminated and replacement chips
# (since they can't be used - the contaminated one because it's contaminated,
# the replacement, because the chip was different (clear) so results can't be compared)

#Exclusion of contiminated or visually disimilar chips
EggTrial <- subset(EggTrial, Plate.ID != "A(contaminated)")
EggTrial <- subset(EggTrial, Plate.ID != "A(visual)")

#Exclusion of chips one growth has reached either side
EggTrial <- subset(EggTrial, neg_edge == 'No')
EggTrial <- subset(EggTrial, pos_edge == 'No')

summary(EggTrial)


# Plotting Dye Control data

DyeControl <- subset(DyeTrial, Type == "Control")

DyeControl_plot <- ggplot(DyeControl, aes(fac_hour, Net_growth)) + 
  theme_classic() +
  geom_boxplot(outlier.size = 0, fill = "cadetblue2") +
  geom_dotplot(binaxis='y', stackdir='centerwhole', dotsize=0.7) +
  scale_y_continuous(name="Net growth (mm)", limits=c(-5, 5), breaks=seq(-5,5,1)) + 
  labs(title = "Dye control", x = "Hour")

DyeControl_plot 

# Plotting Dye Treatment data

DyeTreat <- subset(DyeTrial, Type == "Test")

DyeTreat_plot <- ggplot(DyeTreat, aes(fac_hour, Net_growth)) + 
  theme_classic() +
  geom_boxplot(outlier.size = 0, fill = "brown2") +
  geom_dotplot(binaxis='y', stackdir='centerwhole', dotsize=0.7) +
  scale_y_continuous(name="Net growth (mm)", limits=c(-5, 5), breaks=seq(-5,5,1)) +
  labs(title = "Dye treatment", x = "Hour")

DyeTreat_plot

# Combining Dye plots 

Dyeplot_Full <- ggarrange(DyeControl_plot, DyeTreat_plot, 
                         ncol = 2, nrow = 1, align = "hv", widths = 1, heights = 1,
                         common.legend=TRUE, legend="bottom")

Dyeplot_Full  


# Plotting egg Control data

EggControl <- subset(EggTrial, Type == "Control")


EggControl_plot <- ggplot(EggControl, aes(fac_hour, Net_growth)) + 
  theme_classic() +
  geom_boxplot(outlier.size = 0, fill = "cadetblue2") +
  geom_dotplot(binaxis='y', stackdir='centerwhole', dotsize=0.7) +
  scale_y_continuous(name="Net growth (mm)", limits=c(-5, 5), breaks=seq(-5,5,1)) + 
  labs(title = "Egg control", x = "Hour")

EggControl_plot 

# Plotting egg Treatment data

EggTreat <- subset(EggTrial, Type == "Test")

EggTreat_plot <- ggplot(EggTreat, aes(fac_hour, Net_growth)) + 
  theme_classic() +
  geom_boxplot(outlier.size = 0, fill = "chartreuse3") +
  geom_dotplot(binaxis='y', stackdir='centerwhole', dotsize=0.7) +
  scale_y_continuous(name="Net growth (mm)", limits=c(-5, 5), breaks=seq(-5,5,1)) +
  labs(title = "Egg treatment", x = "Hour") 

EggTreat_plot

# Combining the Egg plots 

Eggplot_Full <- ggarrange(EggControl_plot, EggTreat_plot, 
                          ncol = 2, nrow = 1, align = "hv", widths = 1, heights = 1,
                          common.legend=TRUE, legend="bottom")

Eggplot_Full  


# Combining Dye and Egg plots


Bothplot_Full <- ggarrange(DyeControl_plot, DyeTreat_plot, EggControl_plot, EggTreat_plot, 
                          ncol = 2, nrow = 2, align = "hv", widths = 1, heights = 1,
                          common.legend=TRUE, legend="bottom")

Bothplot_Full  



#t-tests

# Detrmining whether the net growth of any of the treatments (egg, dye, or controls) are statistically different from zero net growth. 


#Egg Trial t-tests: Comparing treatment to control

Et0 <- tidy(t.test(Net_growth ~ Type, data = subset(EggTrial, fac_hour == "0")))
Et18 <- tidy(t.test(Net_growth ~ Type, data = subset(EggTrial, fac_hour == "18")))
Et21 <- tidy(t.test(Net_growth ~ Type, data = subset(EggTrial, fac_hour == "21")))
Et24 <- tidy(t.test(Net_growth ~ Type, data = subset(EggTrial, fac_hour == "24")))
Et42 <- tidy(t.test(Net_growth ~ Type, data = subset(EggTrial, fac_hour == "42")))
Et45 <- tidy(t.test(Net_growth ~ Type, data = subset(EggTrial, fac_hour == "45")))
Et48 <- tidy(t.test(Net_growth ~ Type, data = subset(EggTrial, fac_hour == "48")))
Et66 <- tidy(t.test(Net_growth ~ Type, data = subset(EggTrial, fac_hour == "66")))
Et72 <- tidy(t.test(Net_growth ~ Type, data = subset(EggTrial, fac_hour == "72")))
Et90 <- tidy(t.test(Net_growth ~ Type, data = subset(EggTrial, fac_hour == "90")))
Et93 <- tidy(t.test(Net_growth ~ Type, data = subset(EggTrial, fac_hour == "93")))

# Combining results:
EggTrial_t.Tests <- (rbind(Et0, Et18, Et21, Et24,Et42, Et45, Et48, Et66, Et72, Et90, Et93))


EggTrial_t.Tests[,c("statistic","p.value")]

#Dye Trial t-tests: Comparing treatment to control

Dt0 <- tidy(t.test(Net_growth ~ Type, data = subset(DyeTrial, fac_hour == "0")))
Dt18 <- tidy(t.test(Net_growth ~ Type, data = subset(DyeTrial, fac_hour == "18")))
Dt23 <- tidy(t.test(Net_growth ~ Type, data = subset(DyeTrial, fac_hour == "23")))
Dt42 <- tidy(t.test(Net_growth ~ Type, data = subset(DyeTrial, fac_hour == "42")))
Dt46 <- tidy(t.test(Net_growth ~ Type, data = subset(DyeTrial, fac_hour == "46")))
Dt68 <- tidy(t.test(Net_growth ~ Type, data = subset(DyeTrial, fac_hour == "68")))
Dt72 <- tidy(t.test(Net_growth ~ Type, data = subset(DyeTrial, fac_hour == "72")))
Dt93 <- tidy(t.test(Net_growth ~ Type, data = subset(DyeTrial, fac_hour == "93")))

DyeTrial_t.Tests <- (rbind(Dt0, Dt18, Dt23, Dt42, Dt46, Dt68, Dt72, Dt93))

DyeTrial_t.Tests[,c("statistic","p.value")]

#Dye Trial t-tests: Comparing control to 0

df <-  subset(DyeControl, fac_hour == "0")
Dt0_0 <- tidy(t.test(df$Net_growth, mu = 0)) 

df <-  subset(DyeControl, fac_hour == "18")
Dt18_0 <- tidy(t.test(df$Net_growth, mu = 0)) 

df <-  subset(DyeControl, fac_hour == "23")
Dt23_0 <- tidy(t.test(df$Net_growth, mu = 0)) 

df <-  subset(DyeControl, fac_hour == "42")
Dt42_0 <- tidy(t.test(df$Net_growth, mu = 0)) 

df <-  subset(DyeControl, fac_hour == "46")
Dt46_0 <- tidy(t.test(df$Net_growth, mu = 0))

df <-  subset(DyeControl, fac_hour == "68")
Dt68_0 <- tidy(t.test(df$Net_growth, mu = 0))

df <-  subset(DyeControl, fac_hour == "72")
Dt72_0 <- tidy(t.test(df$Net_growth, mu = 0))

df <-  subset(DyeControl, fac_hour == "93")
Dt93_0 <- tidy(t.test(df$Net_growth, mu = 0))

DyeTrial_control_t.Tests <- (rbind(Dt0_0, Dt18_0, Dt23_0, Dt42_0, Dt46_0, Dt68_0, Dt72_0, Dt93_0))

DyeTrial_control_t.Tests [,c("statistic","p.value")]


#Dye Trial t-tests: Comparing treat to 0



df <-  subset(DyeTreat, fac_hour == "0")
Dt0_0 <- tidy(t.test(df$Net_growth, mu = 0)) 

df <-  subset(DyeTreat, fac_hour == "18")
Dt18_0 <- tidy(t.test(df$Net_growth, mu = 0)) 

df <-  subset(DyeTreat, fac_hour == "23")
Dt23_0 <- tidy(t.test(df$Net_growth, mu = 0)) 

df <-  subset(DyeTreat, fac_hour == "42")
Dt42_0 <- tidy(t.test(df$Net_growth, mu = 0)) 

df <-  subset(DyeTreat, fac_hour == "46")
Dt46_0 <- tidy(t.test(df$Net_growth, mu = 0))

df <-  subset(DyeTreat, fac_hour == "68")
Dt68_0 <- tidy(t.test(df$Net_growth, mu = 0))

df <-  subset(DyeTreat, fac_hour == "72")
Dt72_0 <- tidy(t.test(df$Net_growth, mu = 0))

df <-  subset(DyeTreat, fac_hour == "93")
Dt93_0 <- tidy(t.test(df$Net_growth, mu = 0))

DyeTrial_treat_t.Tests <- (rbind(Dt0_0, Dt18_0, Dt23_0, Dt42_0, Dt46_0, Dt68_0, Dt72_0, Dt93_0))

DyeTrial_treat_t.Tests [,c("statistic","p.value")]



#Dye Trial t-tests: Comparing control to 0


df <-  subset(EggControl, fac_hour == "0")
Dt0_0 <- tidy(t.test(df$Net_growth, mu = 0)) 

df <-  subset(EggControl, fac_hour == "18")
Dt18_0 <- tidy(t.test(df$Net_growth, mu = 0)) 

df <-  subset(EggControl, fac_hour == "21")
Dt21_0 <- tidy(t.test(df$Net_growth, mu = 0)) 

df <-  subset(EggControl, fac_hour == "24")
Dt24_0 <- tidy(t.test(df$Net_growth, mu = 0)) 

df <-  subset(EggControl, fac_hour == "42")
Dt42_0 <- tidy(t.test(df$Net_growth, mu = 0)) 

df <-  subset(EggControl, fac_hour == "45")
Dt45_0 <- tidy(t.test(df$Net_growth, mu = 0))

df <-  subset(EggControl, fac_hour == "48")
Dt48_0 <- tidy(t.test(df$Net_growth, mu = 0))

df <-  subset(EggControl, fac_hour == "66")
Dt66_0 <- tidy(t.test(df$Net_growth, mu = 0))

df <-  subset(EggControl, fac_hour == "69")
Dt69_0 <- tidy(t.test(df$Net_growth, mu = 0))

df <-  subset(EggControl, fac_hour == "72")
Dt72_0 <- tidy(t.test(df$Net_growth, mu = 0))

df <-  subset(EggControl, fac_hour == "90")
Dt90_0 <- tidy(t.test(df$Net_growth, mu = 0))

df <-  subset(EggControl, fac_hour == "93")
Dt93_0 <- tidy(t.test(df$Net_growth, mu = 0))

EggTrial_control_t.Tests <- (rbind(Dt0_0, Dt18_0, Dt21_0, Dt24_0,
                                   Dt42_0, Dt45_0, Dt48_0, Dt66_0, 
                                   Dt69_0, Dt72_0, Dt90_0, Dt93_0))

EggTrial_control_t.Tests [,c("statistic","p.value")]


#Dye Trial t-tests: Comparing treat to 0



df <-  subset(EggTreat, fac_hour == "0")
Dt0_0 <- tidy(t.test(df$Net_growth, mu = 0)) 

df <-  subset(EggTreat, fac_hour == "18")
Dt18_0 <- tidy(t.test(df$Net_growth, mu = 0)) 

df <-  subset(EggTreat, fac_hour == "21")
Dt21_0 <- tidy(t.test(df$Net_growth, mu = 0)) 

df <-  subset(EggTreat, fac_hour == "24")
Dt24_0 <- tidy(t.test(df$Net_growth, mu = 0)) 

df <-  subset(EggTreat, fac_hour == "42")
Dt42_0 <- tidy(t.test(df$Net_growth, mu = 0)) 

df <-  subset(EggTreat, fac_hour == "45")
Dt45_0 <- tidy(t.test(df$Net_growth, mu = 0))

df <-  subset(EggTreat, fac_hour == "48")
Dt48_0 <- tidy(t.test(df$Net_growth, mu = 0))

df <-  subset(EggTreat, fac_hour == "66")
Dt66_0 <- tidy(t.test(df$Net_growth, mu = 0))

df <-  subset(EggTreat, fac_hour == "69")
Dt69_0 <- tidy(t.test(df$Net_growth, mu = 0))

df <-  subset(EggTreat, fac_hour == "72")
Dt72_0 <- tidy(t.test(df$Net_growth, mu = 0))

df <-  subset(EggTreat, fac_hour == "90")
Dt90_0 <- tidy(t.test(df$Net_growth, mu = 0))

df <-  subset(EggTreat, fac_hour == "93")
Dt93_0 <- tidy(t.test(df$Net_growth, mu = 0))

EggTrial_treat_t.Tests <- (rbind(Dt0_0, Dt18_0, Dt21_0, Dt24_0,
                                   Dt42_0, Dt45_0, Dt48_0, Dt66_0, 
                                   Dt69_0, Dt72_0, Dt90_0, Dt93_0))

EggTrial_treat_t.Tests [,c("statistic","p.value")]


## Summery of Dye experiments.

DyeTrial_t.Tests[,c("statistic","p.value")]
DyeTrial_treat_t.Tests [,c("statistic","p.value")]
DyeTrial_control_t.Tests [,c("statistic","p.value")]

#summary results for the Egg experiments. 

EggTrial_t.Tests[,c("statistic","p.value")]
EggTrial_control_t.Tests [,c("statistic","p.value")]
EggTrial_treat_t.Tests [,c("statistic","p.value")]





