# libraries 
library(ggplot2)
library(ggpubr)

# import the results of the simulations from "Main.R"
y1 = readRDS('Results_S1A_B.RDS') # import the results of the simulations of the main
y2 = readRDS('Results_S2A_B_C.RDS') # import the results of the simulations of the main

# Plot Fig2 ----
custom_labels <- c("Si" = "(a)", "Sii" = "(b)")

y1 %>% 
  ggplot() +
  geom_line(aes(x = time, y = value, colour = name, linetype = name), linewidth = 1) +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 23)) +
  labs(title = "", 
       x = "time", y = "") + 
  scale_color_manual(breaks = c("Red squirrels", "Parasite abundance in red", 
                                "Grey squirrels", "Parasite abundance in grey"),
                     values=c("red", "red",  
                              "black", "black")) + 
  scale_linetype_manual(breaks = c("Red squirrels", "Parasite abundance in red", 
                                   "Grey squirrels", "Parasite abundance in grey"),
                        values=c(1,2,
                                 1,2)) + 
  theme(# aspect.ratio = 1/4,
    axis.title = element_text(size=14),
    axis.text = element_text(size=12),
    # legend.text = element_text(size = 0),
    strip.text.x.top = element_text(size = 18, hjust = 0),
    legend.position = '',
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_line(color = 'white'), #"#E6E6E6"),
    panel.grid.minor = element_line(color = "white")) +
  xlab("time (years)") +
  ylab("Population size") +
  facet_wrap(~Scenario,  nrow = 2, strip.position = 'top',  
             labeller = as_labeller(custom_labels))
# to have the same figure as the manuscript I can save it as jpg --> dimension used 632x501

# Plot Fig3 ----
# Custom labels for facet_wrap
custom_labels <- c("Siii" = "(a)", "Siv" = "(b)", "Sv" = "(c)")

y2 %>% 
  mutate(Hr = ifelse(trunc(Hr) < 2, NA, Hr),
         Hg = ifelse(trunc(Hg) < 2, NA, Hg),
         Pr = ifelse(trunc(Hr) < 2, NA, Pr),
         Pg = ifelse(trunc(Hg) < 2, NA, Pg),
         Pr = ifelse(trunc(Pr) == 0, NA, Pr),
         Pg = ifelse(trunc(Pg) == 0, NA, Pg)) %>%
  pivot_longer(cols = 2:6) %>%
  filter(name != 'L') %>%
  ggplot() +
  geom_line(aes(x = time, y = value, colour = name, linetype = name), linewidth = 1) +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 23)) +
  labs(title = "", 
       x = "time", y = "") + 
  scale_color_manual(breaks = c("Hr", "Pr", 
                                "Hg", "Pg", 'L'),
                     values=c("red", "red",  
                              "black", "black",
                              "orange")) + 
  scale_linetype_manual(breaks = c("Hr", "Pr", 
                                   "Hg", "Pg", 'L'),
                        values=c(1,2,
                                 1,2,
                                 3)) + 
  theme(# aspect.ratio = 1/4,
    axis.title = element_text(size=14),
    axis.text = element_text(size=12),
    # legend.text = element_text(size = 0),
    strip.text.x.top = element_text(size = 18, hjust = 0),
    legend.position = '',
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_line(color = 'white'), #"#E6E6E6"),
    panel.grid.minor = element_line(color = "white")) +
  xlab("time (years)") +
  ylab("Population size") +
  facet_wrap(~Scenario,  nrow = 3, strip.position = 'top',  
             labeller = as_labeller(custom_labels))

# to have the same figure as the manuscript I can save it as jpg --> dimension used 632x751

# plot Fig4 ----
# loading the .RDS with the sensitivity ----
# alphag
parmname = 'alfag'
parm = seq(0.01, 0.095, length.out = 20)

# importing the sensitivity performed 
list_y = readRDS(paste0('Results_Sens_', parmname,'.RDS'))
# rounding of the results to find the rounded number of squirrel/intensity of parasites at equilibrium 
Hr = c()
Hg = c()
Pg = c()
Pr = c()
time_extinction_red = c()
for(i in 1:length(list_y)){
  y = list_y[[i]]
  time_extinction_red = c(time_extinction_red, 
                          as.numeric(y[which(trunc(y[,'Hr']) == trunc(y[nrow(y),'Hr']))[1], 'time']))
  Hr = c(Hr, trunc(y[nrow(y),'Hr']))
  Hg = c(Hg,  trunc(y[nrow(y),'Hg']))
  Pg = c(Pg, y[nrow(y),'Pg'])
  Pr = c(Pr, y[nrow(y),'Pr'])
}

# save and arrange in a dataframe
alfag_df = as.data.frame(cbind ('alfag' = parm, 
                                'Red squirrels' = as.numeric(Hr), 
                                'Grey squirrels' = as.numeric(Hg), 
                                'Parasite abundance in grey' = as.numeric(Pg), 
                                'Parasite abundance in red' = as.numeric(Pr), 
                                'Extinction time red' = as.numeric(time_extinction_red)))

# betag
parmname = 'betag'
parm = seq(4.000e-04, 7.000e-04, length.out = 20)

# importing the sensitivity performed 
list_y = readRDS(paste0('Results_Sens_', parmname,'.RDS'))
# rounding of the results to find the rounded number of squirrel/intensity of parasites at equilibrium 
Hr = c()
Hg = c()
Pg = c()
Pr = c()
time_extinction_red = c()
for(i in 1:length(list_y)){
  y = list_y[[i]]
  time_extinction_red = c(time_extinction_red, 
                          as.numeric(y[which(trunc(y[,'Hr']) == trunc(y[nrow(y),'Hr']))[1], 'time']))
  Hr = c(Hr, trunc(y[nrow(y),'Hr']))
  Hg = c(Hg,  trunc(y[nrow(y),'Hg']))
  Pg = c(Pg, y[nrow(y),'Pg'])
  Pr = c(Pr, y[nrow(y),'Pr'])
}

# save and arrange in a dataframe
betag_df = as.data.frame(cbind ('betag' = parm, 
                                'Red squirrels' = as.numeric(Hr), 
                                'Grey squirrels' = as.numeric(Hg), 
                                'Parasite abundance in grey' = as.numeric(Pg), 
                                'Parasite abundance in red' = as.numeric(Pr), 
                                'Extinction time red' = as.numeric(time_extinction_red)))


# betar
parmname = 'betar'
parm = seq(1.000e-04, 1.000e-03, length.out = 20) #seq(4.000e-04, 7.000e-04, length.out = 20)

# importing the sensitivity performed 
list_y = readRDS(paste0('Results_Sens_', parmname,'.RDS'))
# rounding of the results to find the rounded number of squirrel/intensity of parasites at equilibrium 
Hr = c()
Hg = c()
Pg = c()
Pr = c()
time_extinction_red = c()
for(i in 1:length(list_y)){
  y = list_y[[i]]
  time_extinction_red = c(time_extinction_red, 
                          as.numeric(y[which(trunc(y[,'Hr']) == trunc(y[nrow(y),'Hr']))[1], 'time']))
  Hr = c(Hr, trunc(y[nrow(y),'Hr']))
  Hg = c(Hg,  trunc(y[nrow(y),'Hg']))
  Pg = c(Pg, y[nrow(y),'Pg'])
  Pr = c(Pr, y[nrow(y),'Pr'])
}

# save and arrange in a dataframe
betar_df = as.data.frame(cbind ('betar' = parm, 
                                'Red squirrels' = as.numeric(Hr), 
                                'Grey squirrels' = as.numeric(Hg), 
                                'Parasite abundance in grey' = as.numeric(Pg), 
                                'Parasite abundance in red' = as.numeric(Pr), 
                                'Extinction time red' = as.numeric(time_extinction_red)))

# hr 
parmname = 'hr'
parm = seq(17520, 46720, length.out = 20)

# importing the sensitivity performed 
list_y = readRDS(paste0('Results_Sens_', parmname,'.RDS'))
# rounding of the results to find the rounded number of squirrel/intensity of parasites at equilibrium 
Hr = c()
Hg = c()
Pg = c()
Pr = c()
time_extinction_red = c()
for(i in 1:length(list_y)){
  y = list_y[[i]]
  time_extinction_red = c(time_extinction_red, 
                          as.numeric(y[which(trunc(y[,'Hr']) == trunc(y[nrow(y),'Hr']))[1], 'time']))
  Hr = c(Hr, trunc(y[nrow(y),'Hr']))
  Hg = c(Hg,  trunc(y[nrow(y),'Hg']))
  Pg = c(Pg, y[nrow(y),'Pg'])
  Pr = c(Pr, y[nrow(y),'Pr'])
}

# save and arrange in a dataframe
hr_df = as.data.frame(cbind ('hr' = parm, 
                                'Red squirrels' = as.numeric(Hr), 
                                'Grey squirrels' = as.numeric(Hg), 
                                'Parasite abundance in grey' = as.numeric(Pg), 
                                'Parasite abundance in red' = as.numeric(Pr), 
                                'Extinction time red' = as.numeric(time_extinction_red)))


# psig 
parmname = 'psig'
parm = seq(0.35, 0.7, length.out = 20)

# importing the sensitivity performed 
list_y = readRDS(paste0('Results_Sens_', parmname,'.RDS'))
# rounding of the results to find the rounded number of squirrel/intensity of parasites at equilibrium 
Hr = c()
Hg = c()
Pg = c()
Pr = c()
time_extinction_red = c()
for(i in 1:length(list_y)){
  y = list_y[[i]]
  time_extinction_red = c(time_extinction_red, 
                          as.numeric(y[which(trunc(y[,'Hr']) == trunc(y[nrow(y),'Hr']))[1], 'time']))
  Hr = c(Hr, trunc(y[nrow(y),'Hr']))
  Hg = c(Hg,  trunc(y[nrow(y),'Hg']))
  Pg = c(Pg, y[nrow(y),'Pg'])
  Pr = c(Pr, y[nrow(y),'Pr'])
}

# save and arrange in a dataframe
psig_df = as.data.frame(cbind ('psig' = parm, 
                                'Red squirrels' = as.numeric(Hr), 
                                'Grey squirrels' = as.numeric(Hg), 
                                'Parasite abundance in grey' = as.numeric(Pg), 
                                'Parasite abundance in red' = as.numeric(Pr), 
                                'Extinction time red' = as.numeric(time_extinction_red)))


# sigmar 
parmname = 'sigmar'
parm = seq(2, 12, length.out = 20)

# importing the sensitivity performed 
list_y = readRDS(paste0('Results_Sens_', parmname,'.RDS'))
# rounding of the results to find the rounded number of squirrel/intensity of parasites at equilibrium 
Hr = c()
Hg = c()
Pg = c()
Pr = c()
time_extinction_red = c()
for(i in 1:length(list_y)){
  y = list_y[[i]]
  time_extinction_red = c(time_extinction_red, 
                          as.numeric(y[which(trunc(y[,'Hr']) == trunc(y[nrow(y),'Hr']))[1], 'time']))
  Hr = c(Hr, trunc(y[nrow(y),'Hr']))
  Hg = c(Hg,  trunc(y[nrow(y),'Hg']))
  Pg = c(Pg, y[nrow(y),'Pg'])
  Pr = c(Pr, y[nrow(y),'Pr'])
}

# save and arrange in a dataframe
sigmar_df = as.data.frame(cbind ('sigmar' = parm, 
                                'Red squirrels' = as.numeric(Hr), 
                                'Grey squirrels' = as.numeric(Hg), 
                                'Parasite abundance in grey' = as.numeric(Pg), 
                                'Parasite abundance in red' = as.numeric(Pr), 
                                'Extinction time red' = as.numeric(time_extinction_red)))

# plot (in the manuscript) ----
Extinction_df = as.data.frame(rbind(cbind('par' = rep('sigmar', 20),
                                          'extinction time' = sigmar_df$`Extinction time red`, 
                                          'value' = sigmar_df$sigmar),
                                    cbind('par' = rep('hr', 20),
                                          'extinction time' = hr_df$`Extinction time red`, 
                                          'value' = hr_df$hr),
                                    cbind('par' = rep('psig', 20),
                                          'extinction time' = psig_df$`Extinction time red`, 
                                          'value' = psig_df$psig),
                                    cbind('par' = rep('alfag', 20),
                                          'extinction time' = alfag_df$`Extinction time red`, 
                                          'value' = alfag_df$alfag),
                                    cbind('par' = rep('betar', 20),
                                          'extinction time' = betar_df$`Extinction time red`, 
                                          'value' = betar_df$betar),
                                    cbind('par' = rep('betag', 20),
                                          'extinction time' = betag_df$`Extinction time red`, 
                                          'value' = betag_df$betag))) %>%
  mutate('par' = as.factor(par),
         extinction_time = as.numeric(`extinction time`),
         'value' = as.numeric(value))


labelsx_parametri = c(expression(alpha[G]),
                      expression(beta[G]),
                      expression(beta[R]),
                      expression(h[R]),
                      expression(psi[G]),
                      expression(sigma[R]))
labelsy_parametri = c('time to extinction',
                      '',
                      'time to extinction',
                      '',
                      'time to extinction',
                      '')
labels_plot = c('(a)',
                '(b)',
                '(c)',
                '(d)',
                '(e)',
                '(f)')
custom_labels = c('alfag' = 'a)',
                  'betag' = 'b)',
                  'betar' = 'c)',
                  'hr' = 'd)',
                  'psig' = 'e)',
                  'sigmar' = 'f)')

listtmp = list()
for (i in 1:length(levels(Extinction_df$par))) {
  parametro = levels(Extinction_df$par)[i]
  listtmp[[i]] =
    Extinction_df %>%
    mutate(labels = factor(par, labels = custom_labels)) %>%
    filter(par == parametro) %>%
    ggplot() +
    geom_line(aes(x = value, y = extinction_time, colour = par), 
              linewidth = 1.1, lineend = 'round', show.legend = T) +
    geom_point(data = data.frame(x = as.numeric(parms[parametro]),
                                 y = 8), aes(x, y), col = 'black', size = 3, shape =73) + 
    theme_minimal() +
    scale_color_manual(breaks = c('sigmar', 'hr',    'psig',   'alfag', 'betar',   'betag'),
                       values=c("skyblue4", "skyblue4",  "skyblue4", 
                                "skyblue4",  'skyblue4', 'skyblue4')) + 
    theme(# aspect.ratio = 1/3,
      axis.title = element_text(size=12),
      axis.text = element_text(size=11),
      strip.text.x.top = element_text(size = 18, hjust = 0),
      legend.position = '',
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      panel.grid.major = element_line(color = 'white'), #"#E6E6E6"),
      panel.grid.minor = element_line(color = "white")) +
    xlab(labelsx_parametri[i]) +
    ylab(labelsy_parametri[i]) +
    geom_hline(yintercept = 13.6, linetype = 2, color = 'purple') +
    ggtitle(labels_plot[i]) +
    ylim(8,15.5) 
}

g1 = listtmp[[1]]
g2 = listtmp[[2]]
g3 = listtmp[[3]]
g4 = listtmp[[4]]
g5 = listtmp[[5]]
g6 = listtmp[[6]]

ggarrange(g1, g2,
          g3, g4,
          g5, g6,
          ncol = 2, nrow = 3,
          font.label = list(size = 8, color = "black", face = "bold", family = NULL))








