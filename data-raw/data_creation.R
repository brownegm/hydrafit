## Making a version of the data that can be saved in the hydrafit package
## Collected evaporative flux data from Scoffoni et al. 2011
## https://doi-org.stanford.idm.oclc.org/10.1104/pp.111.173856

##@author: Marvin Browne

## packages
library(here)
library(dplyr)
library(readr)

## load raw
cebe_efm <- read_csv(here("data-raw", "scoffonietal2012_cebe_efm.csv"),
                     col_names = FALSE)

heca_efm <- read_csv(here("data-raw", "scoffonietal2012_heca_efm.csv"),
                     col_names = FALSE)

quag_efm <- read_csv(here("data-raw", "quag_efm.csv"),
                     col_names = FALSE)

laca_efm <- read_csv(here("data-raw", "laca_efm.csv"),
                     col_names = FALSE)

hear_efm <- read_csv(here("data-raw", "hear_efm.csv"),
                     col_names = FALSE)

## modify for clarity

# update column names
names(cebe_efm)<-c("psi", "kl")
names(heca_efm)<-c("psi", "kl")
names(quag_efm)<-c("psi", "kl")
names(laca_efm)<-c("psi", "kl")
names(hear_efm)<-c("psi", "kl")


# create each df for CEBE alone and modify so psi is negative and there is
# a column for species name before psi column
cebe_efm <- cebe_efm%>%
  mutate(psi = psi*-1)%>%
  mutate(species = rep("cebe"), .before = psi)

heca_efm <- heca_efm%>%
  mutate(psi = psi*-1)%>%
  mutate(species = rep("heca"), .before = psi)

quag_efm <- quag_efm%>%
  mutate(psi = psi*-1)%>%
  mutate(species = rep("quag"), .before = psi)

laca_efm <- laca_efm%>%
  mutate(psi = psi*-1)%>%
  mutate(species = rep("laca"), .before = psi)

hear_efm <- hear_efm%>%
  mutate(psi = psi*-1)%>%
  mutate(species = rep("hear"), .before = psi)

# check the changes
head(cebe_efm)

# plot to make sure
plot(cebe_efm$kl~cebe_efm$psi, xlim=c(0,-7))

## combine all dataframes into one for multi-species comparisons

scof2012 <-do.call(rbind, list(cebe_efm, heca_efm, quag_efm, laca_efm,hear_efm))

## save data
usethis::use_data(cebe_efm,
         version = 3,
         overwrite = T)# sometimes things change

usethis::use_data(scof2012,
         version = 3,
         overwrite = T)

