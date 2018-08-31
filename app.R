library(shiny)
require(RJSONIO)

# lat = 3.65129, long = 4.52163, 
# rateUrea = 292, rateNafaka = 229, rateMOP = 0, currentY = 7.8, 
# targetY = 14, netRev = 1445.32, totalCost = 228.38, 
# N = 134.32, P = 0, K = 114.5, plDate = 150)

source("scripts/fet_wrapper.R")

lat <- 0.567
long <- 5.6788
IntendedPlantingDate=150
country="Nigeria"
rootUP=270
investment=300

#output here
n <- FR_recommendation(lat, long, IntendedPlantingDate, country, rootUP, investment)

