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
    INT_maxTS <- robust.intersections(test_sol = test_sol, 
                                      initial_sol = initial_sol,
                                      sol_i = 1, 
                                      use.tilde = TRUE, maxTS = TRUE)
    RCR <- rob.case.ratio(test_sol = test_sol, 
                          initial_sol = initial_sol, 
                          outcome=outcome)
    CTE <- list(
      'S1S2Y'=list('Intersection'='Robust Typical Cases (IS*MIN_TS and Y > 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT$S1S2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                  sum((ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5)),"/",nrow(ND),
                                  "=", round((sum((ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',
                                   sum((ND$'S1*S2'>0.5)&(ND$'Outcome'>0.5)),"/",sum((ND$'Outcome'>0.5)),
                                   "=", round((sum((ND$'S1*S2'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'S1*S2'>0.5)&(ND$'Outcome' >0.5)]}),
      'S1S2y'=list('Intersection'='Robust Deviant Cases (IS*MIN_TS and Y < 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT$S1S2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                  sum((ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5)),"/",nrow(ND),
                                  "=", round((sum((ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',
                                   sum((ND$'S1*S2'>0.5)&(ND$'Outcome'<0.5)),"/",sum((ND$'Outcome'<0.5)),
                                   "=", round((sum((ND$'S1*S2'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'S1*S2'>0.5)&(ND$'Outcome' <0.5)]}),
      'S1s2Y'=list('Intersection'='Shaky Typical Cases (IS*~MIN_TS and Y > 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT$S1s2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                  sum((ND$'S1*s2'>0.5)&(ND$'Outcome' >0.5)),"/", nrow(ND),
                                  "=", round((sum((ND$'S1*s2'>0.5)&(ND$'Outcome' >0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',
                                   sum((ND$'S1*s2'>0.5)&(ND$'Outcome'>0.5)),"/",sum((ND$'Outcome'>0.5)),
                                   "=", round((sum((ND$'S1*s2'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'S1*s2'>0.5)&(ND$'Outcome' >0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'S1*s2'>0.5)&(ND$'Outcome' >0.5)]}),
      'S1s2y'=list('Intersection'='Shaky Deviant Cases(IS*~MIN_TS and Y < 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT$S1s2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                  sum((ND$'S1*s2'>0.5)&(ND$'Outcome' <0.5)),"/", nrow(ND),
                                  "=", round((sum((ND$'S1*s2'>0.5)&(ND$'Outcome' <0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',
                                   sum((ND$'S1*s2'>0.5)&(ND$'Outcome'<0.5)),"/",sum((ND$'Outcome'<0.5)),
                                   "=", round((sum((ND$'S1*s2'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'S1*s2'>0.5)&(ND$'Outcome' <0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'S1*s2'>0.5)&(ND$'Outcome' <0.5)]}),
      's1S2Y'=list('Intersection'='Possible Typical Cases (~IS*MAX_TS and Y > 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT_maxTS$s1S2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                  sum((ND$'s1*S3'>0.5)&(ND$'Outcome' >0.5)),"/",nrow(ND),
                                  "=", round((sum((ND$'s1*S3'>0.5)&(ND$'Outcome' >0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',
                                   sum((ND$'s1*S3'>0.5)&(ND$'Outcome'>0.5)),"/",sum((ND$'Outcome'>0.5)),
                                   "=", round((sum((ND$'s1*S3'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'s1*S3'>0.5)&(ND$'Outcome' >0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'s1*S3'>0.5)&(ND$'Outcome' >0.5)]}),
      's1S2y'=list('Intersection'='Possible Deviant Cases (~IS*MAX_TS and Y < 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT_maxTS$s1S2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                  sum((ND$'s1*S3'>0.5)&(ND$'Outcome' <0.5)),"/",nrow(ND),
                                  "=", round((sum((ND$'s1*S3'>0.5)&(ND$'Outcome' <0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',
                                   sum((ND$'s1*S3'>0.5)&(ND$'Outcome'<0.5)),"/",sum((ND$'Outcome'<0.5)),
                                   "=", round((sum((ND$'s1*S3'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'s1*S3'>0.5)&(ND$'Outcome' <0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'s1*S3'>0.5)&(ND$'Outcome' <0.5)]}),
      's1s2Y'=list('Intersection'='Extreme Deviant Coverage Cases (~IS*~MAX_TS and Y > 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT_maxTS$s1s2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                  sum((ND$'s1*s3'>0.5)&(ND$'Outcome' >0.5)),"/", nrow(ND),
                                  "=", round((sum((ND$'s1*s3'>0.5)&(ND$'Outcome' >0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',
                                   sum((ND$'s1*s3'>0.5)&(ND$'Outcome'>0.5)),"/",sum((ND$'Outcome'>0.5)),
                                   "=", round((sum((ND$'s1*s3'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'s1*s3'>0.5)&(ND$'Outcome' >0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'s1*s3'>0.5)&(ND$'Outcome' >0.5)]}),
      's1s2y'=list('Intersection'='Irrelevant Cases (~IS*~MAX_TS and Y < 0.5)',
                   'Boolean'=paste('Boolean Expression:', INT_maxTS$s1s2),
                   'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                  sum((ND$'s1*s3'>0.5)&(ND$'Outcome' <0.5)),"/", nrow(ND),
                                  "=", round((sum((ND$'s1*s3'>0.5)&(ND$'Outcome' <0.5))/nrow(ND))*100, digits=2), "%"),
                   'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',
                                   sum((ND$'s1*s3'>0.5)&(ND$'Outcome'<0.5)),"/",sum((ND$'Outcome'<0.5)),
                                   "=", round((sum((ND$'s1*s3'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),
                   'CaseNames'= if(sum((ND$'s1*s3'>0.5)&(ND$'Outcome' <0.5)) == 0) {'No cases in this intersection'} 
                   else {rownames(ND)[(ND$'s1*s3'>0.5)&(ND$'Outcome' <0.5)]}))
    class(CTE) <- "casestheoryeval"
    CR = list('CaseParameters' = RCR,
              'CaseTypes' = CTE)
    return(CR)
  }