################################################################################
# 1. INSTALL AND LOAD PACKAGES #################################################
################################################################################
## Installs pacman ("package manager") if needed
require(pacman)
if (!require("pacman")) install.packages("pacman")
## Loads additional packages
pacman::p_load(pacman, BBEST, data.table, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, psych, rio, rmarkdown, shiny, 
               stringr, tibble, tidyr) 
################################################################################
# 2. Set Working Dir & Import Data ############################################# 
################################################################################
## Define Parent Working Directory
setwd("//192.168.178.32/bubu/_bus/MSc KIS/9 Vertiefungsmodul - MRT - 24LP/TAM-Similarities-Data/Datasets/")
### Importing Videolist XLSX
vidlist <- import("Videoliste_TAM_gesture.xlsx")
## Defining Sub Directories
setwd("evr_at_once_sim/")
### CSV Import - Similarity Audio Text
simAT <- import("sim_Text_Audio.csv")
################################################################################
# 3. Stats! ####################################################################
################################################################################
## Create statistics object and set column names
simAT_statob <- data.frame(matrix(0, ncol = 2, nrow = 673))
x <- c("Filename", "abstr_code")
colnames(simAT_statob) <- x
rm(x)
## Insert additional information to domain specific stats object
simAT_statob$"Filename" <- simAT$"V1"
## extract abstractness code from filename
simAT_statob <- simAT_statob %>% 
  group_by(Filename) %>% 
    mutate(abstr_code = str_split(
      Filename, pattern = "_", simplify = TRUE)[1]) %>%
        ungroup()
vidlist <- vidlist %>%
  separate(Gesamt, into = c("Filename", "Extension"), sep = "\\.")
simAT_statob <- simAT_statob %>%
  left_join(., select(vidlist, Filename, `Rating Abstractness (18 VP)`),
            by = "Filename")
rm(vidlist)
## Calculate statistical values
simAT_deviance <- as.data.frame(
  simAT %>%  
    summarize(
      across(
        c(`mp_dt_schrupfen-x_dunkel`:`dk_dt_zettelaufdemboden_hell`), 
        list(sd = sd, min = min, max = max, mean = mean, median = median), 
        .names = "{.col}.{.fn}")) %>%
    pivot_longer(
      everything(), 
      names_to = c(".value", "var"),
      names_sep = "\\.") %>%
    t() # transpose
)
rm(simAT)
## rename column names
simAT_deviance <- simAT_deviance[-1,]
simAT_deviance$Filename <- rownames(simAT_deviance)
rownames(simAT_deviance) <- 1:nrow(simAT_deviance)
colnames(simAT_deviance) <- c("sd", "min", "max", "mean", "median", "Filename")
## merge statistical values into statistics object
simAT_statob <- simAT_statob %>%
  left_join(., simAT_deviance, by = "Filename")
rm(simAT_deviance)
simAT_statob$sd <- as.numeric(simAT_statob$sd)
simAT_statob$min <- as.numeric(simAT_statob$min)
simAT_statob$max <- as.numeric(simAT_statob$max)
simAT_statob$mean <- as.numeric(simAT_statob$mean)
simAT_statob$median <- as.numeric(simAT_statob$median)
simAT_statob <- simAT_statob %>%
  mutate_at(vars(abstr_code), factor)
colnames(simAT_statob) <- c("Filename", "abstr_code", "abstr_rate", "sd", "min",
                            "max", "mean", "median")
## ANOVA
summary(aov(mean ~ abstr_code, simAT_statob))
################################################################################
# 4. Plots! ####################################################################
################################################################################
plot(simAT_statob$abstr_code, simAT_statob$mean)