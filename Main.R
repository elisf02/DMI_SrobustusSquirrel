# libraries
library(tidyr)
library(dplyr)
# Importing parameters and model function ----
source('Functions/Det_Model.R')  # model
source('Functions/Model_parameters.R')  # model parameters

# time to run (all) the simulations ----
time <- seq(0, 300, 0.1) 

# SCENARIO 1A) Only grey ----
# Initial state, only grey squirrel (2), with 10 par/host
ini_state = c(Hr = 0,
              Hg = 2,
              Pr = 0,
              Pg = 10,
              L = 0)

# running the model (lsode solver)
y <- Strongy_func(t = time,
                  var = ini_state, 
                  parms = parms, 
                  competition = F) 

# code to build a dataframe to save with all simulations
ySi.1 = y
ySi <- as.data.frame(y) %>%
  mutate('Grey squirrels' = Hg, 
         'Parasite abundance in grey' = Pg) %>%
  select(time, 'Grey squirrels', 'Parasite abundance in grey') %>%
  pivot_longer(cols = 2:3) %>%
  mutate(name = as.factor(name),
         Scenario = as.factor('Si'))


# SCENARIO 1B) Only red ----
ini_state = c(Hr = 60,
              Hg = 0,
              Pr = 2,
              Pg = 0,
              L = 0)

# running the model (lsode solver)
y <- Strongy_func(t = time,
                  var = ini_state, 
                  parms = parms, 
                  competition = F) 

# code to build a dataframe to save with all simulations
ySii.1 = y
ySii <- as.data.frame(y) %>%
  mutate('Red squirrels' = Hr, 
         'Parasite abundance in red' = Pr) %>%
  select(time, 'Red squirrels', 'Parasite abundance in red') %>%
  pivot_longer(cols = 2:3) %>%
  mutate(name = as.factor(name),
         Scenario = as.factor('Sii'))

# save the .RDS file with the simulations of scenarios 2A,B and C
saveRDS(rbind(ySi, ySii),
        file = 'Results_S1A_B.RDS')

# SCENARIO 2A) Parasite + NO Competition ----
ini_state = c(Hr = as.numeric(parms['Kr']), # settling red s. at their carrying capacity
              Hg = 2,
              Pr = 0,
              Pg = 10,
              L = 0)

y <- Strongy_func(t = time,
                  var = ini_state, 
                  parms = parms, 
                  competition = F) 

ySiii.1 = y
ySiii <- as.data.frame(y) %>%
  mutate(Scenario = as.factor('Siii'))


# SCENARIO 2B) Parasite + Competition ----
ini_state = c(Hr = as.numeric(parms['Kr']), # settling red s. at their carrying capacity
              Hg = 2,
              Pr = 0,
              Pg = 10,
              L = 0)

y <- Strongy_func(t = time,
                  var = ini_state, 
                  parms = parms, 
                  competition = T) 

ySiv.1 = y
ySiv <- as.data.frame(y) %>%
  mutate(Scenario = as.factor('Siv'))




# SCENARIO 2C) NO Parasite + Competition ----
ini_state = c(Hr = as.numeric(parms['Kr']), # settling red s. at their carrying capacity
              Hg = 2,
              Pr = 0,
              Pg = 0,
              L = 0)

y <- Strongy_func(t = time,
                  var = ini_state, 
                  parms = parms, 
                  competition = T) 

ySv.1 = y
ySv <- as.data.frame(y) %>%
  mutate(Scenario = as.factor('Sv'))

# save the .RDS file with the simulations of scenarios 2A,B and C
saveRDS(rbind(ySiii, ySiv, ySv),
        file = 'Results_S2A_B_C.RDS')



