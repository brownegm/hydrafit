## Making a version of the data that can be saved in the hydrafit package
## Collected evaporative flux data from Scoffoni et al. 2011
## https://doi-org.stanford.idm.oclc.org/10.1104/pp.111.173856

##@author: Marvin

# packages

library(here)
library(dplyr)
library(readr)

# load raw
cebe_efm <- read_csv("~/Downloads/scoffonietal2012_cebe_efm.csv",
                     col_names = FALSE)
View(cebe_efm)

names(cebe_efm)<-c("psi", "kl")

cebe_efm <- cebe_efm%>%
  mutate(psi = psi*-1)

plot(cebe_efm$kl, cebe_efm$psi)

## save data
use_data(cebe_efm,
         version = 3,
         overwrite = T)# sometimes things change
