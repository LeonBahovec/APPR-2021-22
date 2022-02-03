library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(tmap)
library(shiny)
library(readr)
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(terra)
library(mosaic)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(ranger)
library(rmarkdown)



options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")

