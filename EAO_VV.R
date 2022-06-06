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
### CSV Import - Similarity Video Video
simVV <- import("sim_Video_Video.csv")
################################################################################
# 3. Stats! ####################################################################
################################################################################
## Create statistics object and set column names
simVV_statob <- data.frame(matrix(0, ncol = 2, nrow = 673))
x <- c("Filename", "abstr_code")
colnames(simVV_statob) <- x
rm(x)
## Insert additional information to domain specific stats object
simVV_statob$"Filename" <- simVV$"V1"
## extract abstractness code from filename
simVV_statob <- simVV_statob %>% 
  group_by(Filename) %>% 
    mutate(abstr_code = str_split(
      Filename, pattern = "_", simplify = TRUE)[1]) %>%
        ungroup()
vidlist <- vidlist %>%
  separate(Gesamt, into = c("Filename", "Extension"), sep = "\\.")
simVV_statob <- simVV_statob %>%
  left_join(., select(vidlist, Filename, `Rating Abstractness (18 VP)`),
            by = "Filename")
rm(vidlist)
## Calculate statistical values
simVV_deviance <- as.data.frame(
  simVV %>%  
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
rm(simVV)
## rename column names
simVV_deviance <- simVV_deviance[-1,]
simVV_deviance$Filename <- rownames(simVV_deviance)
rownames(simVV_deviance) <- 1:nrow(simVV_deviance)
colnames(simVV_deviance) <- c("sd", "min", "max", "mean", "median", "Filename")
## merge statistical values into statistics object
simVV_statob <- simVV_statob %>%
  left_join(., simVV_deviance, by = "Filename")
rm(simVV_deviance)
simVV_statob$sd <- as.numeric(simVV_statob$sd)
simVV_statob$min <- as.numeric(simVV_statob$min)
simVV_statob$max <- as.numeric(simVV_statob$max)
simVV_statob$mean <- as.numeric(simVV_statob$mean)
simVV_statob$median <- as.numeric(simVV_statob$median)
simVV_statob <- simVV_statob %>%
  mutate_at(vars(abstr_code), factor)
colnames(simVV_statob) <- c("Filename", "abstr_code", "abstr_rate", "sd", "min",
                            "max", "mean", "median")
## ANOVA
summary(aov(mean ~ abstr_code, simVV_statob))
################################################################################
# 4. Plots! ####################################################################
################################################################################
plot(simVV_statob$abstr_code, simVV_statob$mean)