library(deSolve)
Strongy_func = function(t, var, parms, 
                        competition) {
  if(!competition) {
    parms['cg'] = 0                # competition grey cr=0 when there is no competition 
    parms['cr'] = 0                # competition red cr=0 when there is no competition 
  } 
  
  if(var['Hg'] == 0 && var['Pg'] == 0) {
    model_function <- function(t, var, parms) {
      
      Hr  = var['Hr']
      Hg =  0
      Pr = var['Pr']
      Pg = 0
      L  = var['L']
      
      dHr  = with(as.list(parms),
                 (br-dr)*Hr*(1-(Hr/Kr)-Hg*(cg/Kr)-((alfar/(br-dr))*Pr)))
      dHg =  0
      
      dPr = with(as.list(parms),
                 betar*psir*L-((sigmar+alfar+br)+(alfar/kr)*Pr)*Pr)
      dPg = 0
      
      dL  = with(as.list(parms),
                 hr*Hr*Pr+ hg*Hg*Pg-delta*L-L*(betar*Hr+betag*Hg))
      
      list(c(dHr, dHg, dPr, dPg, dL))
    }
    
  }
  if(var['Hr'] == 0 && var['Pr'] == 0) {

    model_function <- function(t, var, parms) {
      
      Hr  = 0
      Hg =  var['Hg']
      Pr = 0
      Pg = var['Pg']
      L  = var['L']
      
      dHr  = 0
      dHg =  with(as.list(parms),
                  (bg-dg)*Hg*(1-(Hg/Kg)-Hr*(cr/Kg)-((alfag/(bg-dg))*Pg)))
      dPr = 0
      dPg = with(as.list(parms),
                 betag*psig*L-((sigmag+alfag+bg)+(alfag/kg)*Pg)*Pg)
      dL  = with(as.list(parms),
                 hr*Hr*Pr+ hg*Hg*Pg-delta*L-L*(betar*Hr+betag*Hg))
      
      list(c(dHr, dHg, dPr, dPg, dL))
    }
  }
  if(!(var['Hg'] == 0 && var['Pg'] == 0) && !(var['Hr'] == 0 && var['Pr'] == 0)) {
    model_function <- function(t, var, parms, 
                               competition) {
      
      
      Hr  = var['Hr']
      Hg =  var['Hg']
      Pr = var['Pr']
      Pg = var['Pg']
      L  = var['L']
      
      dHr  = with(as.list(parms),
                 (br-dr)*Hr*(1-(Hr/Kr)-Hg*(cg/Kr)-((alfar/(br-dr))*Pr)))
      dHg =  with(as.list(parms),
                 (bg-dg)*Hg*(1-(Hg/Kg)-Hr*(cr/Kg)-((alfag/(bg-dg))*Pg)))
      dPr = with(as.list(parms),
                 betar*psir*L-((sigmar+alfar+br)+(alfar/kr)*Pr)*Pr)
      dPg = with(as.list(parms),
                 betag*psig*L-((sigmag+alfag+bg)+(alfag/kg)*Pg)*Pg)
      dL  = with(as.list(parms),
                 hr*Hr*Pr+ hg*Hg*Pg-delta*L-L*(betar*Hr+betag*Hg))
      
      list(c(dHr, dHg, dPr, dPg, dL))
    }
  }
  
  y = lsoda(y = ini_state, 
            times = time, 
            func = model_function, 
            parms = parms)  #lsode
  return(y)
  
}





