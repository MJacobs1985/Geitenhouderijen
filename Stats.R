#### EMPTY ENVIRONMENT ----
rm(list = ls())

##### LOAD LIBRARIES ----
library(readxl)
library(tidyverse)
library(Hmisc)
library(tidytext)
library(stringr)
library(metafor)
library(meta)
library(dmetar)
#### LOAD & WRANGLE DATA ----
df <- read_excel("Data/Oddsratios.xlsx")
View(df)

#### GRAPHICAL EXPLORATION - alle bevindingen ----
`%nin%` <- Negate(`%in%`)
df%>%
  mutate(Schatting = as.numeric(Schatting), 
         Laag      = as.numeric(Laag), 
         Hoog      = as.numeric(Hoog))%>%
  mutate(Afstand=as.factor(Afstand))%>%
  filter(!is.null(Afstand))%>%
  filter(Auteur %nin% c("Kalkowska", "Beninca"))%>%
  ggplot(aes(x=Schatting, y=Auteur, col=Afstand, shape=Significant))+
  geom_point(position = position_dodge(width = 0.9))+
  geom_pointrange(aes(xmin = Laag  , xmax = Hoog), position = position_dodge(width = 0.9))+
  geom_vline(xintercept = 1, lty=2, col="black", alpha=0.5)+
  theme_bw()+
  theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(-1,7,1), limits=c(-1,7))+
  labs(title="Schattingen van de verhoogde kans op longontsteking", 
       subtitle= "Voor een bepaalde afstand vanaf de dichtsbijzijnde geitenboerderij", 
       caption="Grafiek is afgekapt op (-1,7) voor het verhogen van de leesbaarheid. 
                NBL= Noord-Brabant & Limburg; UGO = Utrecht, Gelderland & Overrijssel")
ggsave(file="Plots/Alle afstanden.png")


df%>%
  mutate(Schatting = as.numeric(Schatting), 
         Laag      = as.numeric(Laag), 
         Hoog      = as.numeric(Hoog))%>%
  mutate(Afstand=as.factor(Afstand))%>%
  filter(!is.null(Afstand))%>%
  filter(Auteur %nin% c("Kalkowska", "Beninca"))%>%
  ggplot(aes(x=Schatting, y=Auteur, col=Methode, shape=Significant))+
  geom_point(position = position_dodge(width = 0.9))+
  geom_pointrange(aes(xmin = Laag  , xmax = Hoog), position = position_dodge(width = 0.9))+
  geom_vline(xintercept = 1, lty=2, col="black", alpha=0.5)+
  theme_bw()+
  theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(-1,7,1), limits=c(-1,7))+
  labs(title="Schattingen van de verhoogde kans op longontsteking", 
       subtitle= "per methode", 
       caption="Grafiek is afgekapt op (-1,7) voor het verhogen van de leesbaarheid. 
                NBL= Noord-Brabant & Limburg; UGO = Utrecht, Gelderland & Overrijssel")
ggsave(file="Plots/Alle methoden.png")

df%>%
  mutate(Schatting = as.numeric(Schatting), 
         Laag      = as.numeric(Laag), 
         Hoog      = as.numeric(Hoog))%>%
  mutate(Afstand=as.factor(Afstand))%>%
  filter(!is.null(Afstand))%>%
  filter(Auteur %nin% c("Kalkowska", "Beninca"))%>%
  ggplot(aes(x=Schatting, y=Auteur, col=Analysejaar, shape=Significant))+
  geom_point(position = position_dodge(width = 0.9))+
  geom_pointrange(aes(xmin = Laag  , xmax = Hoog), position = position_dodge(width = 0.9))+
  geom_vline(xintercept = 1, lty=2, col="black", alpha=0.5)+
  theme_bw()+
  theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(-1,7,1), limits=c(-1,7))+
  labs(title="Schattingen van de verhoogde kans op longontsteking", 
       subtitle= "per analysejaar", 
       caption="Grafiek is afgekapt op (-1,7) voor het verhogen van de leesbaarheid. 
                NBL= Noord-Brabant & Limburg; UGO = Utrecht, Gelderland & Overrijssel")
ggsave(file="Plots/Alle analysejaren.png")

df%>%
  mutate(Schatting = as.numeric(Schatting), 
         Laag      = as.numeric(Laag), 
         Hoog      = as.numeric(Hoog))%>%
  mutate(Afstand=as.factor(Afstand))%>%
  filter(!is.null(Afstand))%>%
  filter(Auteur %nin% c("Kalkowska", "Beninca"))%>%
  ggplot(aes(x=Schatting, y=Auteur, col=Controlegroep, shape=Significant))+
  geom_point(position = position_dodge(width = 0.9))+
  geom_pointrange(aes(xmin = Laag  , xmax = Hoog), position = position_dodge(width = 0.9))+
  geom_vline(xintercept = 1, lty=2, col="black", alpha=0.5)+
  theme_bw()+
  theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(-1,7,1), limits=c(-1,7))+
  labs(title="Schattingen van de verhoogde kans op longontsteking", 
       subtitle= "per soort controle", 
       caption="Grafiek is afgekapt op (-1,7) voor het verhogen van de leesbaarheid. 
                NBL= Noord-Brabant & Limburg; UGO = Utrecht, Gelderland & Overrijssel")
ggsave(file="Plots/Per controlegroep.png")


df%>%
  mutate(Schatting = as.numeric(Schatting), 
         Laag      = as.numeric(Laag), 
         Hoog      = as.numeric(Hoog))%>%
  mutate(Afstand=as.factor(Afstand))%>%
  filter(!is.null(Afstand))%>%
  filter(Auteur %nin% c("Kalkowska", "Beninca"))%>%
  ggplot(aes(x=Schatting, y=Auteur, col=VGO, shape=Significant))+
  geom_point(position = position_dodge(width = 0.9))+
  geom_pointrange(aes(xmin = Laag  , xmax = Hoog), position = position_dodge(width = 0.9))+
  geom_vline(xintercept = 1, lty=2, col="black", alpha=0.5)+
  theme_bw()+
  theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(-1,7,1), limits=c(-1,7))+
  labs(title="Schattingen van de verhoogde kans op longontsteking", 
       subtitle= "per VGO lijn", 
       caption="Grafiek is afgekapt op (-1,7) voor het verhogen van de leesbaarheid. 
                NBL= Noord-Brabant & Limburg; UGO = Utrecht, Gelderland & Overrijssel")
ggsave(file="Plots/Per VGO lijn.png")

df%>%
  mutate(Schatting = as.numeric(Schatting), 
         Laag      = as.numeric(Laag), 
         Hoog      = as.numeric(Hoog))%>%
  mutate(Afstand=as.factor(Afstand))%>%
  filter(!is.null(Afstand))%>%
  filter(Auteur %nin% c("Kalkowska", "Beninca"))%>%
  ggplot(aes(x=Schatting, y=Auteur, col=Regio, shape=Significant))+
  geom_point(position = position_dodge(width = 0.9))+
  geom_pointrange(aes(xmin = Laag  , xmax = Hoog), position = position_dodge(width = 0.9))+
  geom_vline(xintercept = 1, lty=2, col="black", alpha=0.5)+
  theme_bw()+
  theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(-1,7,1), limits=c(-1,7))+
  labs(title="Schattingen van de verhoogde kans op longontsteking", 
       subtitle= "per VGO lijn", 
       caption="Grafiek is afgekapt op (-1,7) voor het verhogen van de leesbaarheid. 
                NBL= Noord-Brabant & Limburg; UGO = Utrecht, Gelderland & Overrijssel")
ggsave(file="Plots/Per regio.png")



#### GRAPHICAL EXPLORATION - per afstand ----
df%>%
  mutate(Schatting = as.numeric(Schatting), 
         Laag      = as.numeric(Laag), 
         Hoog      = as.numeric(Hoog))%>%
  filter(!is.null(Afstand))%>%
  filter(Auteur != "Yzermans")%>%
  filter(Afstand==500)%>%
  ggplot(aes(x=Schatting, y=Auteur, col=Methode, shape=Significant))+
  geom_point(position = position_dodge(width = 0.9))+
  geom_pointrange(aes(xmin = Laag  , xmax = Hoog), position = position_dodge(width = 0.9))+
  geom_vline(xintercept = 1, lty=2, col="black", alpha=0.5)+
  theme_bw()+
  theme(legend.position = "right")+
  facet_grid(Regio~"Odds-ratio [Betrouwbaarheidsinterval]", scales="free", space="free")+
  scale_x_continuous(breaks=seq(-1,7,1), limits=c(-1,7))+
  labs(title="Schattingen van de verhoogde kans op longontsteking", 
       subtitle= "Voor een afstand van 500 meter vanaf de dichtsbijzijnde geitenboerderij", 
       caption="Grafiek is afgekapt op (-1,7) voor het verhogen van de leesbaarheid. 
                NBL= Noord-Brabant & Limburg; UGO = Utrecht, Gelderland & Overrijssel")

df%>%
  mutate(Schatting = as.numeric(Schatting), 
         Laag      = as.numeric(Laag), 
         Hoog      = as.numeric(Hoog))%>%
  filter(!is.null(Afstand))%>%
  filter(Auteur != "Yzermans")%>%
  filter(Afstand==1000)%>%
  ggplot(aes(x=Schatting, y=Auteur, col=Methode, shape=Significant))+
  geom_point(position = position_dodge(width = 0.9))+
  geom_pointrange(aes(xmin = Laag  , xmax = Hoog), position = position_dodge(width = 0.9))+
  geom_vline(xintercept = 1, lty=2, col="black", alpha=0.5)+
  theme_bw()+
  theme(legend.position = "right")+
  facet_grid(Regio~"Odds-ratio [Betrouwbaarheidsinterval]", scales="free", space="free")+
  scale_x_continuous(breaks=seq(-1,7,1), limits=c(-1,7))+
  labs(title="Schattingen van de verhoogde kans op longontsteking", 
       subtitle= "Voor een afstand van 1000 meter vanaf de dichtsbijzijnde geitenboerderij", 
       caption="Grafiek is afgekapt op (-1,5) voor het verhogen van de leesbaarheid. 
                NBL= Noord-Brabant & Limburg; UGO = Utrecht, Gelderland & Overrijssel")

df%>%
  mutate(Schatting = as.numeric(Schatting), 
         Laag      = as.numeric(Laag), 
         Hoog      = as.numeric(Hoog))%>%
  filter(!is.null(Afstand))%>%
  filter(Auteur != "Yzermans")%>%
  filter(Afstand==1500)%>%
  ggplot(aes(x=Schatting, y=Auteur, col=Methode, shape=Significant))+
  geom_point(position = position_dodge(width = 0.9))+
  geom_pointrange(aes(xmin = Laag  , xmax = Hoog), position = position_dodge(width = 0.9))+
  geom_vline(xintercept = 1, lty=2, col="black", alpha=0.5)+
  theme_bw()+
  theme(legend.position = "right", aspect.ratio = 1/1.5)+
  facet_grid(Regio~"Odds-ratio [Betrouwbaarheidsinterval]", scales="free")+
  scale_x_continuous(breaks=c(-1,0,1,2,3,4,5), limits=c(-1,5))+
  labs(title="Schattingen van de verhoogde kans op longontsteking", 
       subtitle= "Voor een afstand van 1500 meter vanaf de dichtsbijzijnde geitenboerderij", 
       caption="Grafiek is afgekapt op (-1,5) voor het verhogen van de leesbaarheid. 
                NBL= Noord-Brabant & Limburg; UGO = Utrecht, Gelderland & Overrijssel")

df%>%
  mutate(Schatting = as.numeric(Schatting), 
         Laag      = as.numeric(Laag), 
         Hoog      = as.numeric(Hoog))%>%
  filter(!is.null(Afstand))%>%
  filter(Auteur != "Yzermans")%>%
  filter(Afstand==2000)%>%
  ggplot(aes(x=Schatting, y=Auteur, col=Methode, shape=Significant))+
  geom_point(position = position_dodge(width = 0.9))+
  geom_pointrange(aes(xmin = Laag  , xmax = Hoog), position = position_dodge(width = 0.9))+
  geom_vline(xintercept = 1, lty=2, col="black", alpha=0.5)+
  theme_bw()+
  theme(legend.position = "right", aspect.ratio = 1/2)+
  facet_grid(Regio~"Odds-ratio [Betrouwbaarheidsinterval]", scales="free")+
  scale_x_continuous(breaks=c(-1,0,1,2,3,4,5), limits=c(-1,5))+
  labs(title="Schattingen van de verhoogde kans op longontsteking", 
       subtitle= "Voor een afstand van 2000 meter vanaf de dichtsbijzijnde geitenboerderij", 
       caption="Grafiek is afgekapt op (-1,5) voor het verhogen van de leesbaarheid. 
                NBL= Noord-Brabant & Limburg; UGO = Utrecht, Gelderland & Overrijssel")


#### GRAPHICAL EXPLORATION - per afstand ----
df%>%
  mutate(Schatting = as.numeric(Schatting), 
         Laag      = as.numeric(Laag), 
         Hoog      = as.numeric(Hoog))%>%
  mutate(Afstand=as.factor(Afstand))%>%
  filter(Auteur %nin% c("Kalkowska", "Beninca"))%>%
  filter(!is.na(Afstand))%>%
  ggplot(aes(x=Schatting, y=Analysejaar, col=Significant))+
  geom_point(position = position_dodge(width = 0.9))+
  geom_pointrange(aes(xmin = Laag  , xmax = Hoog), position = position_dodge(width = 0.9))+
  geom_vline(xintercept = 1, lty=2, col="black", alpha=0.5)+
  facet_grid(VGO~Afstand, scales="free", space="free")+
  theme_bw()+
  theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(-1,3,1), limits=c(0,3))+
  labs(title="Schattingen van de verhoogde kans op longontsteking", 
       subtitle= "Voor een bepaalde afstand vanaf de dichtsbijzijnde geitenboerderij per VGO-lijn", 
       caption="Grafiek is afgekapt op (0,3) voor het verhogen van de leesbaarheid. 
                NBL= Noord-Brabant & Limburg; UGO = Utrecht, Gelderland & Overrijssel")
ggsave(file="Plots/Split_Afstand_VGO.png")

df%>%
  mutate(Schatting = as.numeric(Schatting), 
         Laag      = as.numeric(Laag), 
         Hoog      = as.numeric(Hoog))%>%
  mutate(Afstand=as.factor(Afstand))%>%
  filter(Auteur %nin% c("Kalkowska", "Beninca"))%>%
  filter(!is.na(Afstand))%>%
  ggplot(aes(x=Analysejaar, y=Schatting, col=Significant))+
  geom_point(position = position_dodge(width = 0.9))+
  geom_hline(yintercept = 1, lty=2, col="black", alpha=0.5)+
  geom_pointrange(aes(ymin = Laag  , ymax = Hoog), position = position_dodge(width = 0.9))+
  facet_grid(~VGO, scales="free", space="free")+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_y_continuous(breaks=seq(0,7,1), limits=c(0,7))+
  labs(title="Schattingen van de verhoogde kans op longontsteking", 
       subtitle= "Per analysejaar en per VGO-lijn", 
       caption="Grafiek is afgekapt op (0,7) voor het verhogen van de leesbaarheid. 
                NBL= Noord-Brabant & Limburg; UGO = Utrecht, Gelderland & Overrijssel")
ggsave(file="Plots/Split_Analysejaar_VGO.png")

df%>%
  mutate(Schatting = as.numeric(Schatting), 
         Laag      = as.numeric(Laag), 
         Hoog      = as.numeric(Hoog))%>%
  mutate(Afstand=as.factor(Afstand))%>%
  filter(Auteur %nin% c("Kalkowska", "Beninca"))%>%
  filter(!is.na(Afstand))%>%
  ggplot(aes(x=Schatting, y=Analysejaar, col=Significant))+
  geom_point(position = position_dodge(width = 0.9))+
  geom_pointrange(aes(xmin = Laag  , xmax = Hoog), position = position_dodge(width = 0.9))+
  geom_vline(xintercept = 1, lty=2, col="black", alpha=0.5)+
  facet_grid(Controlegroep~Afstand, scales="free", space="free")+
  theme_bw()+
  theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(0,3,1), limits=c(0,3))+
  labs(title="Schattingen van de verhoogde kans op longontsteking", 
       subtitle= "Per analysejaar en per controlegroep", 
       caption="Grafiek is afgekapt op (0,3) voor het verhogen van de leesbaarheid. 
                NBL= Noord-Brabant & Limburg; UGO = Utrecht, Gelderland & Overrijssel")
ggsave(file="Plots/Split_Analysejaar_Controle.png")

df%>%
  mutate(Schatting = as.numeric(Schatting), 
         Laag      = as.numeric(Laag), 
         Hoog      = as.numeric(Hoog))%>%
  mutate(Afstand=as.factor(Afstand))%>%
  filter(Auteur %nin% c("Kalkowska", "Beninca"))%>%
  filter(!is.na(Afstand))%>%
  ggplot(aes(x=Schatting, y=Analysejaar, col=Methode))+
  geom_point(position = position_dodge(width = 0.9))+
  geom_pointrange(aes(xmin = Laag  , xmax = Hoog), position = position_dodge(width = 0.9))+
  geom_vline(xintercept = 1, lty=2, col="black", alpha=0.5)+
  facet_grid(Significant~Afstand, scales="free")+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks=seq(0,3,1), limits=c(0,3))+
  labs(title="Schattingen van de verhoogde kans op longontsteking", 
       subtitle= "Per analysejaar en per gebruikte statistische methode", 
       caption="Grafiek is afgekapt op (0,3) voor het verhogen van de leesbaarheid. 
                NBL= Noord-Brabant & Limburg; UGO = Utrecht, Gelderland & Overrijssel")
ggsave(file="Plots/Split_Analysejaar_Methode.png")


df%>%
  mutate(Schatting = as.numeric(Schatting), 
         Laag      = as.numeric(Laag), 
         Hoog      = as.numeric(Hoog))%>%
  mutate(Afstand=as.factor(Afstand))%>%
  filter(Auteur %nin% c("Kalkowska", "Beninca"))%>%
  filter(!is.na(Afstand))%>%
  ggplot(aes(x=Schatting, y=Analysejaar, col=Significant))+
  geom_point(position = position_dodge(width = 0.9))+
  geom_pointrange(aes(xmin = Laag  , xmax = Hoog), position = position_dodge(width = 0.9))+
  geom_vline(xintercept = 1, lty=2, col="black", alpha=0.5)+
  facet_grid(Regio~Afstand, scales="free", space="free")+
  theme_bw()+
  theme(legend.position = "right")+
  scale_x_continuous(breaks=seq(0,3,1), limits=c(0,3))+
  labs(title="Schattingen van de verhoogde kans op longontsteking", 
       subtitle= "Per analysejaar en per regio", 
       caption="Grafiek is afgekapt op (0,3) voor het verhogen van de leesbaarheid. 
                NBL= Noord-Brabant & Limburg; UGO = Utrecht, Gelderland & Overrijssel")
ggsave(file="Plots/Split_Analysejaar_regio.png")


#### SIMULATE DATA FROM ODDS-RATIOS ----
N <- 46000  # Total sample size
desired_OR <- 1.73
baseline_prob <- 0.019  # Probability of outcome in unexposed group (e.g., control)
# 2. Calculate model coefficients
# Log-odds (logit) of the baseline probability
b0 <- log(baseline_prob / (1 - baseline_prob))
# Log-odds ratio (effect size)
b1 <- log(desired_OR)
# 3. Generate data
exposure <- rbinom(N, 1, prob = 0.5) # Randomly assign exposure 50/50
# Calculate linear predictor (log-odds for each person)
logit_p <- b0 + b1 * exposure
# Convert log-odds to probability using the inverse logit function (plogis)
probability <- plogis(logit_p)
# Generate binary outcome (Y) based on these probabilities
outcome <- rbinom(N, 1, prob = probability)
# Create a data frame
simulated_data <- data.frame(exposure = exposure, Outcome = outcome)
# Optional: Verify the OR from the simulated data
model <- glm(Outcome ~ exposure, data = simulated_data, family = "binomial")
# Extract coefficients and standard errors
model_summary <- summary(model)
coefs <- model_summary$coefficients
# The log-odds ratio and its standard error (assuming 'exposure' is the second coefficient)
log_or <- coefs["exposure", "Estimate"]
se_log_or <- coefs["exposure", "Std. Error"]
# For a 95% interval (Z-score 1.96)
z_score <- 1.96 
lower_bound_log <- log_or - z_score * se_log_or
upper_bound_log <- log_or + z_score * se_log_or
# Exponentiate the limits
or_lower_bound <- exp(lower_bound_log)
or_upper_bound <- exp(upper_bound_log)
# Display the prediction interval
cat("Odds Ratio Confidence Interval (95%):", or_lower_bound, "to", or_upper_bound, "\n")

#### META-ANALYSE ----
Lotterman_500m <- read_excel("Data/Lotterman_500m.xlsx")
Lotterman_500m

test<-Lotterman_500m%>%
  select(GP, Region, Estimate, Lower, Higher)%>%
  mutate(vi = ((Higher-Lower)/3.92)^2,
         yi = Estimate)%>%
  select(GP, Estimate, Region, Lower, Higher, yi, vi)%>%
  arrange(GP)
res_meta__test_REMLDL<-meta::metagen(TE = Estimate,
                                     lower=Lower,
                                     upper=Higher, 
                                     studlab = GP,
                                     data = test,
                                     subgroup=Region,
                                     sm = "OR",
                                     transf=FALSE,
                                     fixed = FALSE,
                                     random = TRUE,
                                     prediction = TRUE,
                                     prediction.subgroup = TRUE,
                                     null.effect=1, 
                                     adhoc.hakn.ci ="ci",
                                     method.predict = "HK",
                                     adhoc.hakn.pi = "se",
                                     tau.common=FALSE,
                                     method.tau = "REML",
                                     title = "Goat Farm 500m and Pneumonia")
summary(res_meta__test_REMLDL)
meta::forest(res_meta__test_REMLDL, 
             sortvar = TE, prediction = TRUE, 
             print.tau2 = TRUE,layout = "RevMan5")







#### CREATE FIGURE LOOKING AT TREND ----
df2 <- read_excel("Data/Oddsratios.xlsx", 
                 sheet = "Sheet 2")
View(df2)
colnames(df2)

df2%>%
  mutate(Analysejaar=as.numeric(Analysejaar), 
         Afstand=as.factor(Afstand))%>%
  dplyr::select(Auteur, Analysejaar, Extra_longontsteking, Afstand)%>%
  print(n=63)

df2%>%
  mutate(Analysejaar=as.numeric(Analysejaar), 
         Afstand=as.factor(Afstand))%>%
  dplyr::select(Auteur, Analysejaar, Extra_longontsteking, Afstand, 
                Extra_longontsteking_laag, Extra_longontsteking_hoog)%>%
  drop_na()%>%
  group_by(Analysejaar, Afstand)%>%
  summarise(mean_estimate = mean(Extra_longontsteking), 
            mean_low = mean(Extra_longontsteking_laag), 
            mean_high = mean(Extra_longontsteking_hoog))%>%
  ggplot(aes(x=Analysejaar, 
         y=mean_estimate, 
         col=Afstand,
         fill=Afstand,
         ymin=mean_low, 
         ymax=mean_high, 
         group=Afstand, 
         label=round(mean_estimate)))+
  geom_point(show.legend = FALSE)+
  geom_line()+
  geom_ribbon(alpha=0.1, show.legend = FALSE)+
  geom_hline(yintercept = 0, lty=2, col="black")+
  geom_text(col="black", nudge_y = -100)+
  theme_bw()+
  facet_grid(~Afstand)+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+
  labs(x="Analysejaar", 
       y="Geschatte extra aantal longontstekingen", 
       col="Afstand (m)", 
       title="Relatie tussen afstand en geschat aantal extra longontstekingen per jaar.", 
       subtitle="Incidentie van longontstekingen is niet significant op 500 meter en neemt af op 1000m.",
       caption="Schattingen zijn gebaseerd op de studies van Borlée (2015), Huijskens (2016), Post (2019) en Lotterman (2023). 
       Deze studies publiceerden als enge de odds-ratios per afstand en per jaar. 
       De geschatte extra longontstekingen plus 95% betrouwbaarheidsintervallen zijn gemiddelden over de studies heen.
       Een meer gedegen analyse vereist het samenvoegen van alle epidemiologische studies ten behoeve van een grootschalige analyse.")
ggsave("Longontstekingen per jaar per afstand VGO.png", dpi=600)

tiff("test.png", units="in", width=5, height=5, res=300)
# insert ggplot code
dev.off()






  