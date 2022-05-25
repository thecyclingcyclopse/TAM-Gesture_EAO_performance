# 1. INSTALL AND LOAD PACKAGES #################################################
## Installs pacman ("package manager") if needed
require(pacman)
if (!require("pacman")) install.packages("pacman")
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
### CSV Import - Similarity Text Text
simTT <- import("sim_Text_Text.csv")


# 3. Stats!
## Create new Median Objects and set column names
### simTT
#mclapply(
simTT_statob <- data.frame(matrix(0, ncol = 2, nrow = 673))
x <- c("Filename", "abstr_code")
colnames(simTT_statob) <- x
rm(x)
## Insert additional information to domain specific median table ###############
##########  (eg. abstractness rating value etc.) ###############################
### for simTT
simTT_statob$"Filename" <- simTT$"V1"
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

# Assessing fit of the mean
## Deviance
### simTT
simTT_deviance <- as.data.frame(
  simTT %>%  
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
rm(simTT)
simTT_deviance <- simTT_deviance[-1,]
simTT_deviance$Filename <- rownames(simTT_deviance)
rownames(simTT_deviance) <- 1:nrow(simTT_deviance)
colnames(simTT_deviance) <- c("sd", "min", "max", "mean", "median", "Filename")
simTT_statob <- simTT_statob %>%
  left_join(., simTT_deviance, by = "Filename")
rm(simTT_deviance)
simTT_statob$sd <- as.numeric(simTT_statob$sd)
simTT_statob$min <- as.numeric(simTT_statob$min)
simTT_statob$max <- as.numeric(simTT_statob$max)
simTT_statob$mean <- as.numeric(simTT_statob$mean)
simTT_statob$median <- as.numeric(simTT_statob$median)
simTT_statob <- simTT_statob %>%
  mutate_at(vars(abstr_code), factor)
colnames(simTT_statob) <- c("Filename", "abstr_code", "abstr_rate", "sd", "min",
                            "max", "mean", "median")
#) #mclapply

################################################################################
# 4. Plots! ####################################################################
################################################################################
plot(simTT_statob$sd, simTT_statob$abstr_rate)

plot(simTT_statob$abstr_code, simTT_statob$abstr_rate)

plot(simTT_statob$abstr_code, simTT_statob$sd)

