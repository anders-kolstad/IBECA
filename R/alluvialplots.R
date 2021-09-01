
library(dplyr)
library(ggplot2)
library(ggalluvial)
library(nycflights13)
library(forcats)
library(readxl)







# datasett1 ---------------------------------------------------------------


indikatorer <- read_excel("data/Indikatorer.xlsx", 
                  sheet = "indikatorer")
indikatorer$freq <- 1
indikatorer$utgår[is.na(indikatorer$utgår)] <- "Nei"


ind <- indikatorer%>%
  filter(utgår == "Nei")%>%
  to_lodes_form(key = key, value = node, id = "Cohort", axes=c("node","datasetName", "paavirkning1")) %>%
  filter(node != "NA")

all1 <- ind%>%
  ggplot(aes(y = freq, x = key, stratum = node, alluvium = Cohort)) +
  geom_alluvium(aes(fill = egenskap1), aes.bind='flows', width = 1/12) +   # aes.bind har ingen effekt her
  #geom_stratum(width = 1/4,  aes(fill = Egenskap), color = "black") +
  stat_stratum(width = 1/4,  aes(fill = egenskap1), color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("datasetName", "node", "paavirkning1"),
                   labels = c("Datasett", "Indikator", "Påvirkning"),
                   expand = c(.1, .1),
                   position="top") +
  #scale_fill_manual(values = c("red", "orange", "blue", "green", "yellow")) +
  labs(y = "") +
  theme_minimal() +
  theme(legend.position = "right") +
  ggtitle("")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size=30),
        panel.grid = element_blank(),
        axis.title.x = element_blank()
  )

all1








# Bare datasett og indikator ----------------------------------------------



ind2 <- indikatorer%>%
  filter(utgår == "Nei")%>%
  to_lodes_form(key = key, value = node, id = "Cohort", axes=c("node","datasetName", "egenskap1"))


ind2%>%
  ggplot(aes(x = key, stratum = node, alluvium = Cohort)) +
  geom_alluvium(colour = "grey",  width = 1/12) +
  geom_stratum(width = 1/4,   color = "black", aes(fill = key), alpha = 1) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("egenskap1", "node", "datasetName"),
                   labels = c("Egenskap", "Indikator", "Datasett"),
                   expand = c(.1, .1),
                   position="top") +
  #scale_fill_manual(values = c("red", "orange", "blue", "green", "yellow")) +
  labs(y = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size=30),
        panel.grid = element_blank(),
        axis.title.x = element_blank()
  )



# wide