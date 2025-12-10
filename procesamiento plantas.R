if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, rio, flextable, officer,  scales, ggExtra, writexl, janitor, corrr, ggpubr, ggExtra, gtsummary, tableone, broom, epiDisplay, car )

base_plantas<- import("INSPLANTAS.csv")
keywords<- import("keywords zotero.csv")
df_plantas <- base_plantas %>%
  left_join(keywords, by = "Key")
