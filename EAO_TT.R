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
### CSV Import - Similarity Text Text
simTT <- import("sim_Text_Text.csv")
################################################################################
# 3. Stats! ####################################################################
################################################################################
## Create statistics object and set column names
simTT_statob <- data.frame(matrix(0, ncol = 2, nrow = 673))
x <- c("Filename", "abstr_code")
colnames(simTT_statob) <- x
rm(x)
## Insert additional information to domain specific stats object
simTT_statob$"Filename" <- simTT$"V1"
## extract abstractness code from filename
simTT_statob <- simTT_statob %>% 
  group_by(Filename) %>% 
    mutate(abstr_code = str_split(
      Filename, pattern = "_", simplify = TRUE)[1]) %>%
        ungroup()
vidlist <- vidlist %>%
  separate(Gesamt, into = c("Filename", "Extension"), sep = "\\.")
simTT_statob <- simTT_statob %>%
  left_join(., select(vidlist, Filename, `Rating Abstractness (18 VP)`),
            by = "Filename")
rm(vidlist)
## Calculate statistical values
simTT_deviance <- as.data.frame(
  simTT %>%  
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
rm(simTT)
## rename column names
simTT_deviance <- simTT_deviance[-1,]
simTT_deviance$Filename <- rownames(simTT_deviance)
rownames(simTT_deviance) <- 1:nrow(simTT_deviance)
colnames(simTT_deviance) <- c("sd", "mini", "maxi", "mean", "median", "Filename")
## merge statistical values into statistics object
simTT_statob <- simTT_statob %>%
  left_join(., simTT_deviance, by = "Filename")
rm(simTT_deviance)
simTT_statob$sd <- as.numeric(simTT_statob$sd)
simTT_statob$mini <- as.numeric(simTT_statob$mini)
simTT_statob$maxi <- as.numeric(simTT_statob$maxi)
simTT_statob$mean <- as.numeric(simTT_statob$mean)
simTT_statob$median <- as.numeric(simTT_statob$median)
simTT_statob <- simTT_statob %>%
  mutate_at(vars(abstr_code), factor)
colnames(simTT_statob) <- c("Filename", "abstr_code", "abstr_rate", "sd", "mini",
                            "maxi", "mean", "median")
## export stats object to working directory
write.csv(simTT_statob, "EAO_stats_simTT.csv")
## ANOVA
summary(aov(mean ~ abstr_code, simTT_statob))
################################################################################
# 4. Plots! ####################################################################
################################################################################
## GGStatPlot
ggstatsplot::ggbetweenstats(
  data = simTT_statob, 
  x = abstr_code, 
  y = mini,
  xlab = "abstraction_code",
  ylab = "minimum",
  messages = FALSE
)
ggstatsplot::ggbetweenstats(
  data = simTT_statob, 
  x = abstr_code, 
  y = mean,
  xlab = "abstraction_code",
  ylab = "mean",
  messages = FALSE
)
ggstatsplot::ggbetweenstats(
  data = simTT_statob, 
  x = abstr_code, 
  y = maxi,
  xlab = "abstraction_code",
  ylab = "maximum",
  messages = FALSE
)