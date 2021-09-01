libs <- c('dplyr', 'stringr', 'forcats',     # wrangling
          'knitr','kableExtra',               # table styling
          'ggplot2','ggalluvial',  # plots
          'nycflights13')                     # data
invisible(lapply(libs, library, character.only = TRUE))

library(dplyr)
library(ggplot2)
library(ggalluvial)
library(nycflights13)
library(forcats)


# Data --------------------------------------------------------------------

#using only the tow 5 destination and carriers:
top_dest <- flights %>% 
  count(dest) %>% 
  top_n(5, n) %>% 
  pull(dest)
top_carrier <- flights %>% 
  filter(dest %in% top_dest) %>% 
  count(carrier) %>% 
  top_n(4, n) %>% 
  pull(carrier)
fly <- flights %>% 
  filter(dest %in% top_dest & carrier %in% top_carrier) %>% 
  count(origin, carrier, dest) %>% 
  mutate(origin = fct_relevel(as.factor(origin), c("EWR", "LGA", "JFK")))


# Plot --------------------------------------------------------------------

fly <- fly %>% 
  mutate(origin = fct_rev(as.factor(origin)),
         carrier = fct_rev(as.factor(carrier)),
         dest = fct_rev(as.factor(dest))) %>% 
  filter(n > 150)


fly %>% 
  mutate(origin = fct_rev(as.factor(origin)),
         carrier = fct_rev(as.factor(carrier)),
         dest = fct_rev(as.factor(dest))) %>% 
  filter(n > 150) %>% 
  ggplot(aes(y = n, axis1 = origin, axis2 = carrier, axis3 = dest)) +
  geom_alluvium(aes(fill = origin), aes.bind='flows', width = 1/12) +
  geom_stratum(width = 1/4, fill = "white", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Origin", "Carrier", "Destination"),
                   expand = c(.05, .05)) +
  scale_fill_manual(values = c("red", "orange", "blue")) +
  labs(y = "Cases") +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("NYC flights volume for top destinations and airlines")


# Titanic example ---------------------------------------------------------


titanic_wide <- data.frame(Titanic)
# current plot
ggplot(data = titanic_wide, aes(axis1 = Class, axis2 = Sex, axis3 = Age, y = Freq)) +
  scale_x_discrete(limits = c("Class", "Sex", "Age"), expand = c(.2, .05)) +
  xlab("Demographic") +
  geom_alluvium(aes(fill=Class)) +
  geom_stratum(aes(fill=Class)) +
  geom_text(stat = "stratum", infer.label = TRUE) +
  theme_minimal() +
  ggtitle("passengers on the maiden voyage of the Titanic", "stratified by demographics and survival")+
  scale_fill_manual(values=c("red","orange","green","blue"), breaks=c("1st","2nd","3rd","Crew"), labels=c("1st","2nd","3rd","Crew"))


# long data
titanic_long <- to_lodes_form(data.frame(Titanic),
                              key = "Demographic", value = "Group", id = "Cohort",
                              axes = 1:3)
# plot with all strata colored
ggplot(data = titanic_long,
       aes(x = Demographic, stratum = Group, alluvium = Cohort, y = Freq)) +
  geom_alluvium(aes(fill=Group)) +
  geom_stratum(aes(fill=Group)) +
  geom_text(stat = "stratum", aes(label = Group)) +
  theme_minimal() +
  ggtitle("passengers on the maiden voyage of the Titanic", "stratified by demographics and survival")
#> Warning in f(...): Some differentiation aesthetics vary within alluvia, and will be diffused by their first value.
#> Consider using `geom_flow()` instead.

# My own example data -----------------------------------------------------


dat <- data.frame(Dataset = c("TOV-E", "Rovbase", "MODIS", "INON", "Rovbase", "Rovbase"),
                  Indikator = c("Fjellrype", "Fjellvåk", "NDVI", "Inngrepsfri natur", "Kongeørn", "Jerv"),
                  Egenskap = c("A", #"Funksjonell sammensetning innen trofiske nivåer",
                               "A", #Funksjonelt viktige arter og strukturer",
                               "Primærproduksjon",
                               "Landskapsøkologiske mønstre",
                               "Biomasse mellom trofiske nivåer",
                               "Biomasse mellom trofiske nivåer"),
                  n = 1)

dat2 <- to_lodes_form(dat,
                     key = keytest, value = Dataset, id = "Cohort", axes=1:2)

dat2%>%
ggplot(aes(y = n, x = keytest, stratum = Dataset, alluvium = Cohort)) +
  geom_alluvium(aes(fill = Egenskap), aes.bind='flows', width = 1/12) +
  geom_stratum(width = 1/4,  aes(fill = Egenskap), color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Dataset", "Indikator"),
                   expand = c(.05, .05),
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

# Ekte data ---------------------------------------------------------------


library(readxl)
ind <- read_excel("data/Indikatorer.xlsx", 
                          sheet = "indikatorer")

ind$freq <- 1

ind$utgår[is.na(ind$utgår)] <- "Nei"
ind <- ind%>%
  filter(utgår == "Nei")%>%
  to_lodes_form(key = key, value = node, id = "Cohort", axes=c("node","datasetName", "paavirkning1"))


ind%>%
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



# Bare datasett og indikator ----------------------------------------------




library(readxl)
ind <- read_excel("data/Indikatorer.xlsx", 
                  sheet = "alluvial-indicators")


ind$Utgår[is.na(ind$Utgår)] <- "Nei"
ind <- ind%>%
  filter(Utgår == "Nei")%>%
  to_lodes_form(key = key, value = node, id = "Cohort", axes=c("node","Datasettnavn", "Egenskap"))


ind%>%
  ggplot(aes(x = key, stratum = node, alluvium = Cohort)) +
  geom_alluvium(colour = "grey",  width = 1/12) +
  geom_stratum(width = 1/4,   color = "black", aes(fill = key), alpha = 0.5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Egenskap", "node", "Datasettnavn"),
                   labels = c("Egenskap", "Indikator", "Datasett"),
                   expand = c(.1, .1),
                   position="top") +
  #scale_fill_manual(values = c("red", "orange", "blue", "green", "yellow")) +
  labs(y = "") +
  theme_minimal() +
  #theme(legend.position = "right") +
  ggtitle("")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size=30),
        panel.grid = element_blank(),
        axis.title.x = element_blank()
  )
