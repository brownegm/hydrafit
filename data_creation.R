## Making a version of the data that can be saved in the hydrafit package

##@author: Marvin

# packages

library(here)
library(dplyr)
library(readxl)

# load data
dd_full <- readxl::read_xlsx("~/Documents/Research_UCLA/NEON_EFM/data/all_NEON_20230120.xlsx") %>%
  mutate(species.site = paste(site, Species, sep = "_"))

dd <- dd_full %>%
  rename(species = Species, psi = Psi_lowest, kl = K) %>% # rename var names to fit with the rest of the syntax below.
  mutate(data.type = rep(1, dim(dd_full)[1])) %>%
  select(species.site, data.type, psi, kl) # function's expecting this order


head(dd)

# subset the data

dd.filter <- dd %>%
  dplyr::filter(species.site=="HF_QURU")

dd.mani <- dd.filter %>%
  mutate(kl=kl+0.6828)

