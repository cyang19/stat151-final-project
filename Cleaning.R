# load packages 
library(tidyverse)
library(arm)
library(readstata13)
library(ggsci) # for cool color pallets

person <- read.dta13("data/CFPS_2022/ecfps2022person_202410.dta")
child <- read.dta13("data/CFPS_2022/ecfps2022child_202410.dta")
fameconf <- read.dta13("data/CFPS_2022/ecfps2022famconf_202410.dta")
famecone <- read.dta13("data/CFPS_2022/ecfps2022famecon_202410.dta")


