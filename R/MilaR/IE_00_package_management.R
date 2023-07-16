## R version 4.3.0

if (!require("pacman")) install.packages("pacman")
library(pacman)

# load and install packages
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, data.table, tidyr, stringr,
               survey, flextable, rlang, srvyr, gtsummary, tibble, jtools,
               broom.mixed, ggpubr, BayesFactor, ggrepel, reshape2, performance,
               bayestestR, extrafont, patchwork, MatchIt)

# load helpers
source('R/parseTaskData.R')
source('R/data.R')
source('R/statistics.R')

source('R/MilaR/helper.R')