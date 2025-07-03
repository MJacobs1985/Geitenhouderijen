#### EMPTY ENVIRONMENT ----
rm(list = ls())

##### LOAD LIBRARIES ----
library(tidyverse)
library(readxl)

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

