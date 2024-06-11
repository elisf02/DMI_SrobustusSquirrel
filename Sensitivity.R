# Importing parameters and model function ----
source('Functions/Det_Model.R')  # model
source('Functions/Model_parameters.R')  # model parameters

# setting the initial state for the simulations for the sensitivity analysis 
time <- seq(0, 300, 0.1) 
ini_state = c(Hr = 60,
              Hg = 2,
              Pr = 0,
              Pg = 10,
              L = 0)

# alfag ----
parmname = 'alfag' # set the name of the parameter
parm = seq(0.01, 0.095, length.out = 20) # and the ranges in which I want it to vary

time_extinction_red = c()
list_y = list()
for (i in 1:length(parm)) {
  parmstmp = parms
  parmstmp[parmname] = parm[i]
  y <- Strongy_func(t = time,
                    var = ini_state, 
                    parms = parmstmp, 
                    competition = T)  #lsode
  list_y[[i]] = y
}
saveRDS(list_y, file = paste0('Results_Sens_', parmname,'.RDS'))

# betag----
parmname = 'betag'
parm = seq(4.000e-04, 7.000e-04, length.out = 20)

time_extinction_red = c()
list_y = list()
for (i in 1:length(parm)) {
  parmstmp = parms
  parmstmp[parmname] = parm[i]
  y <- Strongy_func(t = time,
                    var = ini_state, 
                    parms = parmstmp, 
                    competition = T)  #lsode
  list_y[[i]] = y
}
saveRDS(list_y, file = paste0('Results_Sens_', parmname,'.RDS'))

# betar ----
parmname = 'betar'
parm = seq(1.000e-04, 1.000e-03, length.out = 20) #seq(4.000e-04, 7.000e-04, length.out = 20)

time_extinction_red = c()
list_y = list()
for (i in 1:length(parm)) {
  parmstmp = parms
  parmstmp[parmname] = parm[i]
  y <- Strongy_func(t = time,
                    var = ini_state, 
                    parms = parmstmp, 
                    competition = T)  #lsode
  list_y[[i]] = y
}
saveRDS(list_y, file = paste0('Results_Sens_', parmname,'.RDS'))

# hr -----
parmname = 'hr'
parm = seq(17520, 46720, length.out = 20)

time_extinction_red = c()
list_y = list()
for (i in 1:length(parm)) {
  parmstmp = parms
  parmstmp[parmname] = parm[i]
  y <- Strongy_func(t = time,
                    var = ini_state, 
                    parms = parmstmp, 
                    competition = T)  #lsode
  list_y[[i]] = y
}
saveRDS(list_y, file = paste0('Results_Sens_', parmname,'.RDS'))

# psig -----
parmname = 'psig'
parm = seq(0.35, 0.7, length.out = 20)

time_extinction_red = c()
list_y = list()
for (i in 1:length(parm)) {
  parmstmp = parms
  parmstmp[parmname] = parm[i]
  y <- Strongy_func(t = time,
                    var = ini_state, 
                    parms = parmstmp, 
                    competition = T)  #lsode
  list_y[[i]] = y
}
saveRDS(list_y, file = paste0('Results_Sens_', parmname,'.RDS'))

# sigmar -----
parmname = 'sigmar'
parm = seq(2, 12, length.out = 20)

time_extinction_red = c()
list_y = list()
for (i in 1:length(parm)) {
  parmstmp = parms
  parmstmp[parmname] = parm[i]
  y <- Strongy_func(t = time,
                    var = ini_state, 
                    parms = parmstmp, 
                    competition = T)  #lsode
  list_y[[i]] = y
}
saveRDS(list_y, file = paste0('Results_Sens_', parmname,'.RDS'))
