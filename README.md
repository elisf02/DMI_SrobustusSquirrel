# DMI_SrobustusSquirrel
code to reproduce the results of: "_Silent enemies: sublethal macroparasites can drive disease-mediated invasions_".

**Abstract**: Shared pathogens can alter the interaction between native and alien species resulting in disease-mediated invasions (DMIs). Invasive species often harbour low-virulence macroparasites, but empirical evidence for macroparasite-driven DMIs is still limited due to their sublethal impacts and scarce prominence. Here we modelled the dynamics of native red squirrels, invasive grey squirrels and their shared nematode Strongyloides robustus to assess whether macroparasites can drive DMIs and lead to native species extinction. Our simulations showed that spillover of the alien parasite can lead to red squirrel extinction, that grey squirrels amplify the infection in the native host and that the infection accelerates the replacement of red squirrels compared to direct competition alone, ultimately facilitating invasion by grey squirrels. These results demonstrate that sublethal macroparasites can mediate invasions, suggesting that we are overlooking key drivers of native species decline.

The code includes 4 files: 
1) "Main.R": the main code to reproduce the simulations of the scenarios
2) "Sensitivity.R"": the main code for the sensitivity analysis

3) "Functions/DetMod.R": the funciton for the deterministic model
4) "Functions/Parameters.R": the file with the parameters

You need the four of them to reproduce the same set of simulations

There is an additional file (Plots.R) to produce the plots as they are reported in the paper. 


