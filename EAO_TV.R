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
### CSV Import - Similarity Text Video
simTV <- import("sim_Text_Video.csv")
################################################################################
# 3. Stats! ####################################################################
################################################################################
## Create statistics object and set column names
simTV_statob <- data.frame(matrix(0, ncol = 2, nrow = 673))
x <- c("Filename", "abstr_code")
colnames(simTV_statob) <- x
rm(x)
## Insert additional information to domain specific stats object
simTV_statob$"Filename" <- simTV$"V1"
## extract abstractness code from filename
simTV_statob <- simTV_statob %>% 
  group_by(Filename) %>% 
    mutate(abstr_code = str_split(
      Filename, pattern = "_", simplify = TRUE)[1]) %>%
        ungroup()
vidlist <- vidlist %>%
  separate(Gesamt, into = c("Filename", "Extension"), sep = "\\.")
simTV_statob <- simTV_statob %>%
  left_join(., select(vidlist, Filename, `Rating Abstractness (18 VP)`),
            by = "Filename")
rm(vidlist)
## Calculate statistical values
simTV_deviance <- as.data.frame(
  simTV %>%  
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
rm(simTV)
## rename column names
simTV_deviance <- simTV_deviance[-1,]
simTV_deviance$Filename <- rownames(simTV_deviance)
rownames(simTV_deviance) <- 1:nrow(simTV_deviance)
colnames(simTV_deviance) <- c("sd", "mini", "maxi", "mean", "median", "Filename")
## merge statistical values into statistics object
simTV_statob <- simTV_statob %>%
  left_join(., simTV_deviance, by = "Filename")
rm(simTV_deviance)
simTV_statob$sd <- as.numeric(simTV_statob$sd)
simTV_statob$mini <- as.numeric(simTV_statob$mini)
simTV_statob$maxi <- as.numeric(simTV_statob$maxi)
simTV_statob$mean <- as.numeric(simTV_statob$mean)
simTV_statob$median <- as.numeric(simTV_statob$median)
simTV_statob <- simTV_statob %>%
  mutate_at(vars(abstr_code), factor)
colnames(simTV_statob) <- c("Filename", "abstr_code", "abstr_rate", "sd", "mini",
                            "maxi", "mean", "median")
## export stats object to working directory
write.csv(simTV_statob, "EAO_stats_simTV.csv")
## ANOVA
summary(aov(mean ~ abstr_code, simTV_statob))
################################################################################
# 4. Plots! ####################################################################
################################################################################
## GGStatPlot
ggstatsplot::ggbetweenstats(
  data = simTV_statob, 
  x = abstr_code, 
  y = mini,
  xlab = "abstraction_code",
  ylab = "minimum",
  messages = FALSE
)
ggstatsplot::ggbetweenstats(
  data = simTV_statob, 
  x = abstr_code, 
  y = mean,
  xlab = "abstraction_code",
  ylab = "mean",
  messages = FALSE
)
ggstatsplot::ggbetweenstats(
  data = simTV_statob, 
  x = abstr_code, 
  y = maxi,
  xlab = "abstraction_code",
  ylab = "maximum",
  messages = FALSE
)