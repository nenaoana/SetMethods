rob.cases <-
  function(test_sol, 
           initial_sol, 
           outcome)
  {
    ND <- rob.evaluation(test_sol = test_sol, 
                                initial_sol = initial_sol, 
                                outcome=outcome)
    INT <- robust.intersections(test_sol = test_sol, 
                                initial_sol = initial_sol,
                                sol_i = 1, 
                                use.tilde = TRUE)
    RCR <- rob.case.ratio(test_sol = test_sol, 
                          initial_sol = initial_sol, 
                          outcome=outcome)
    CTE <- list(
      'S1S2Y'=list('Intersection'='Robust Typical Cases (IS*TS and Y > 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT$S1S2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                  sum((ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5)),"/",nrow(ND),
                                  "=", round((sum((ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',
                                   sum((ND$'S1*S2'>0.5)&(ND$'Outcome'>0.5)),"/",sum((ND$'Outcome'>0.5)),
                                   "=", round((sum((ND$'S1*S2'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5)]}),
      'S1S2y'=list('Intersection'='Robust Deviant Cases (IS*TS and Y < 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT$S1S2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                  sum((ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5)),"/",nrow(ND),
                                  "=", round((sum((ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',
                                   sum((ND$'S1*S2'>0.5)&(ND$'Outcome'<0.5)),"/",sum((ND$'Outcome'<0.5)),
                                   "=", round((sum((ND$'S1*S2'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5)]}),
      'S1s2Y'=list('Intersection'='Shaky Typical Cases (IS*ts and Y > 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT$S1s2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                  sum((ND$'S1*s2'>0.5)&(ND$'Outcome' >0.5)),"/", nrow(ND),
                                  "=", round((sum((ND$'S1*s2'>0.5)&(ND$'Outcome' >0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',
                                   sum((ND$'S1*s2'>0.5)&(ND$'Outcome'>0.5)),"/",sum((ND$'Outcome'>0.5)),
                                   "=", round((sum((ND$'S1*s2'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'S1*s2'>0.5)&(ND$'Outcome' >0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'S1*s2'>0.5)&(ND$'Outcome' >0.5)]}),
      'S1s2y'=list('Intersection'='Shaky Deviant Cases(IS*ts and Y < 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT$S1s2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                  sum((ND$'S1*s2'>0.5)&(ND$'Outcome' <0.5)),"/", nrow(ND),
                                  "=", round((sum((ND$'S1*s2'>0.5)&(ND$'Outcome' <0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',
                                   sum((ND$'S1*s2'>0.5)&(ND$'Outcome'<0.5)),"/",sum((ND$'Outcome'<0.5)),
                                   "=", round((sum((ND$'S1*s2'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'S1*s2'>0.5)&(ND$'Outcome' <0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'S1*s2'>0.5)&(ND$'Outcome' <0.5)]}),
      's1S2Y'=list('Intersection'='Possible Typical Cases (is*TS and Y > 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT$s1S2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                  sum((ND$'s1*S2'>0.5)&(ND$'Outcome' >0.5)),"/",nrow(ND),
                                  "=", round((sum((ND$'s1*S2'>0.5)&(ND$'Outcome' >0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',
                                   sum((ND$'s1*S2'>0.5)&(ND$'Outcome'>0.5)),"/",sum((ND$'Outcome'>0.5)),
                                   "=", round((sum((ND$'s1*S2'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'s1*S2'>0.5)&(ND$'Outcome' >0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'s1*S2'>0.5)&(ND$'Outcome' >0.5)]}),
      's1S2y'=list('Intersection'='Possible Deviant Cases (is*TS and Y < 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT$s1S2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                  sum((ND$'s1*S2'>0.5)&(ND$'Outcome' <0.5)),"/",nrow(ND),
                                  "=", round((sum((ND$'s1*S2'>0.5)&(ND$'Outcome' <0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',
                                   sum((ND$'s1*S2'>0.5)&(ND$'Outcome'<0.5)),"/",sum((ND$'Outcome'<0.5)),
                                   "=", round((sum((ND$'s1*S2'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'s1*S2'>0.5)&(ND$'Outcome' <0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'s1*S2'>0.5)&(ND$'Outcome' <0.5)]}),
      's1s2Y'=list('Intersection'='Extreme Deviant Coverage Cases (is*ts and Y > 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT$s1s2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                  sum((ND$'s1*s2'>0.5)&(ND$'Outcome' >0.5)),"/", nrow(ND),
                                  "=", round((sum((ND$'s1*s2'>0.5)&(ND$'Outcome' >0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',
                                   sum((ND$'s1*s2'>0.5)&(ND$'Outcome'>0.5)),"/",sum((ND$'Outcome'>0.5)),
                                   "=", round((sum((ND$'s1*s2'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'s1*s2'>0.5)&(ND$'Outcome' >0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'s1*s2'>0.5)&(ND$'Outcome' >0.5)]}),
      's1s2y'=list('Intersection'='Irrelevant Cases (is*ts and Y < 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT$s1s2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                  sum((ND$'s1*s2'>0.5)&(ND$'Outcome' <0.5)),"/", nrow(ND),
                                  "=", round((sum((ND$'s1*s2'>0.5)&(ND$'Outcome' <0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',
                                   sum((ND$'s1*s2'>0.5)&(ND$'Outcome'<0.5)),"/",sum((ND$'Outcome'<0.5)),
                                   "=", round((sum((ND$'s1*s2'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'s1*s2'>0.5)&(ND$'Outcome' <0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'s1*s2'>0.5)&(ND$'Outcome' <0.5)]}))
    class(CTE) <- "casestheoryeval"
    CR = list('CaseParameters' = RCR,
              'CaseNames' = CTE)
    return(CR)
  }