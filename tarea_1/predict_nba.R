library(readr)
library(dplyr)
library(ggplot2)
library(knitr)
library(gvlma)
library(MASS)
library(car)

nba <- read_csv("nba.csv")
View(nba) 

nba <- rename_with(nba, ~ tolower(gsub('%', '', .x, fixed = T)))
nba <- rename_with(nba, ~ tolower(gsub('3', 'tres', .x, fixed = T)))
nba <- rename_with(nba, ~ tolower(gsub('/', '_', .x, fixed = T)))

duplicated(nba)
nba <- nba[!duplicated(nba), ]

nba <- na.omit(nba)

tam_muestra <- floor(0.8*nrow(nba))
muestra <- sample(nrow(nba), size = tam_muestra) 
train <- nba[muestra, ]
test <- nba[-muestra, ]

regresion = lm(salary~nba_draftnumber + age + g + mp + per + ts + trespar + ftr
              + orb + drb + trb  + ast + stl + blk + tov + usg + ows + dws + ws 
              + ws_48 + obpm + dbpm + bpm + vorp,data = train)

summary(regresion)

regresion$coefficients

gvlma(regresion)

 
vif(regresion)

sqrt(vif(regresion)) > 2

regresion1 = lm(salary~nba_draftnumber + age + g + mp + per + ts + trespar + ftr
                + orb + drb + trb  + ast + stl + blk + tov + usg + ows + dws + ws 
                + ws_48 + obpm + dbpm  + vorp,data = train)


vif(regresion1)

sqrt(vif(regresion1)) > 2

regresion2 = lm(salary~nba_draftnumber + age + g + mp + per + ts + trespar + ftr
                + orb + drb + trb  + ast + stl + blk + tov + usg + ows + dws 
                + ws_48 + obpm + dbpm  + vorp,data = train)

vif(regresion2)

sqrt(vif(regresion2)) > 2

regresion3 = lm(salary~nba_draftnumber + age + g + mp + per + ts + trespar + ftr
                + orb + drb + ast + stl + blk + tov + usg + ows + dws + ws_48 
                + obpm + dbpm  + vorp,data = train)

vif(regresion3)

sqrt(vif(regresion3)) > 2


stepAIC(regresion3, direction = "both")










