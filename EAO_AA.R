################################################################################
# 1. INSTALL AND LOAD PACKAGES #################################################
################################################################################
## Installs pacman ("package manager") if needed
require(pacman)
if (!require("pacman")) install.packages("pacman")
## Loads additional packages
pacman::p_load(pacman, BBEST, data.table, dplyr, GGally, ggplot2, ggstatsplot,
               ggthemes, ggvis, httr, lubridate, plotly, psych, rio, rmarkdown,
               shiny, stringr, tibble, tidyr)
################################################################################
# 2. Set Working Dir & Import Data ############################################# 
################################################################################
## Define Parent Working Directory
setwd("~/TAM-Similarities-Data/Datasets/")
### Importing Videolist XLSX
vidlist <- import("Videoliste_TAM_gesture.xlsx")
## Defining Sub Directories
setwd("evr_at_once_sim/")
### CSV Import - Similarity Audio Audio
simAA <- import("sim_Audio_Audio.csv")
################################################################################
# 3. Stats! ####################################################################
################################################################################
## Create statistics object and set column names
simAA_statob <- data.frame(matrix(0, ncol = 2, nrow = 673))
x <- c("Filename", "abstr_code")
colnames(simAA_statob) <- x
rm(x)
## Insert additional information to domain specific stats object
simAA_statob$"Filename" <- simAA$"V1"
## extract abstractness code from filename
simAA_statob <- simAA_statob %>% 
  group_by(Filename) %>% 
    mutate(abstr_code = str_split(
      Filename, pattern = "_", simplify = TRUE)[1]) %>%
        ungroup()
vidlist <- vidlist %>%
  separate(Gesamt, into = c("Filename", "Extension"), sep = "\\.")
simAA_statob <- simAA_statob %>%
  left_join(., select(vidlist, Filename, `Rating Abstractness (18 VP)`),
            by = "Filename")
rm(vidlist)
## Calculate statistical values
simAA_deviance <- as.data.frame(
  simAA %>%  
    summarize(
      across(
        c(`mp_dt_schrupfen-x_dunkel`:`dk_dt_zettelaufdemboden_hell`), 
        list(sd = sd, mini = min, maxi = max, mean = mean, median = median), 
        .names = "{.col}.{.fn}")) %>%
    pivot_longer(
      everything(), 
      names_to = c(".value", "var"),
      names_sep = "\\.") %>%
    t() # transpose
)
rm(simAA)
## rename column names
simAA_deviance <- simAA_deviance[-1,]
simAA_deviance$Filename <- rownames(simAA_deviance)
rownames(simAA_deviance) <- 1:nrow(simAA_deviance)
colnames(simAA_deviance) <- c("sd", "mini", "maxi", "mean", "median", "Filename")
## merge statistical values into statistics object
simAA_statob <- simAA_statob %>%
  left_join(., simAA_deviance, by = "Filename")
rm(simAA_deviance)
simAA_statob$sd <- as.numeric(simAA_statob$sd)
simAA_statob$mini <- as.numeric(simAA_statob$mini)
simAA_statob$maxi <- as.numeric(simAA_statob$maxi)
simAA_statob$mean <- as.numeric(simAA_statob$mean)
simAA_statob$median <- as.numeric(simAA_statob$median)
simAA_statob <- simAA_statob %>%
  mutate_at(vars(abstr_code), factor)
colnames(simAA_statob) <- c("Filename", "abstr_code", "abstr_rate", "sd", "mini",
                            "maxi", "mean", "median")
## export stats object to working directory
write.csv(simAA_statob, "EAO_stats_simAA.csv")
## ANOVA
summary(aov(mean ~ abstr_code, simAA_statob))
################################################################################
# 4. Plots! ####################################################################
################################################################################
## GGStatPlot
ggstatsplot::ggbetweenstats(
  data = simAA_statob, 
  x = abstr_code, 
  y = mini,
  xlab = "abstraction_code",
  ylab = "minimum",
  messages = FALSE
)
ggstatsplot::ggbetweenstats(
  data = simAA_statob, 
  x = abstr_code, 
  y = mean,
  xlab = "abstraction_code",
  ylab = "mean",
  messages = FALSE
)
ggstatsplot::ggbetweenstats(
  data = simAA_statob, 
  x = abstr_code, 
  y = maxi,
  xlab = "abstraction_code",
  ylab = "maximum",
  messages = FALSE
)