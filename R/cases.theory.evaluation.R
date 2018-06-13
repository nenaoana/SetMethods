cases.theory.evaluation <-
  function(theory_data)
  {
    ND <- theory_data
    CTE <- list('TEY'=list('Intersection'='Covered Most Likely (T*E and Y > 0.5)', 
                           'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                          sum((ND$'T*E'>0.5)&(ND$'Outcome'>0.5)),"/",nrow(ND),
                                          "=", round((sum((ND$'T*E'>0.5)&(ND$'Outcome'>0.5))/nrow(ND))*100, digits=2), "%"),
                           'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',
                                          sum((ND$'T*E'>0.5)&(ND$'Outcome'>0.5)),"/",sum((ND$'Outcome'>0.5)),
                                          "=", round((sum((ND$'T*E'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),
                           'CaseNames'= if(sum((ND$'T*E'>0.5)&(ND$'Outcome'>0.5)) == 0) {'No cases in this intersection'} 
                           else {rownames(ND)[(ND$'T*E'>0.5)&(ND$'Outcome'>0.5)]}),
                'tEY'=list('Intersection'='Covered Least Likely (t*E and Y > 0.5)', 
                           'CaseNo'=paste('Cases in the intersection/Total number of cases:',
                                          sum((ND$'t*E'>0.5)&(ND$'Outcome'>0.5)),"/",nrow(ND),
                                          "=", round((sum((ND$'t*E'>0.5)&(ND$'Outcome'>0.5))/nrow(ND))*100, digits=2), "%"),
                           'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',
                                          sum((ND$'t*E'>0.5)&(ND$'Outcome'>0.5)),"/",sum((ND$'Outcome'>0.5)),
                                          "=", round((sum((ND$'t*E'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),
                           'CaseNames'= if(sum((ND$'t*E'>0.5)&(ND$'Outcome'>0.5)) == 0) {'No cases in this intersection'} 
                           else {rownames(ND)[(ND$'t*E'>0.5)&(ND$'Outcome'>0.5)]}),
                'TeY'=list('Intersection'='Uncovered Most Likely (T*e and Y > 0.5)', 
                           'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                          sum((ND$'T*e'>0.5)&(ND$'Outcome'>0.5)),"/", nrow(ND),
                                          "=", round((sum((ND$'T*e'>0.5)&(ND$'Outcome'>0.5))/nrow(ND))*100, digits=2), "%"),
                           'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',
                                          sum((ND$'T*e'>0.5)&(ND$'Outcome'>0.5)),"/", sum((ND$'Outcome'>0.5)),
                                          "=", round((sum((ND$'T*e'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),
                           'CaseNames'= if(sum((ND$'T*e'>0.5)&(ND$'Outcome'>0.5)) == 0) {'No cases in this intersection'} 
                           else {rownames(ND)[(ND$'T*e'>0.5)&(ND$'Outcome'>0.5)]}),
                'teY'=list('Intersection'='Uncovered Least Likely (t*e and Y > 0.5)', 
                           'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                          sum((ND$'t*e'>0.5)&(ND$'Outcome'>0.5)),"/", nrow(ND),
                                          "=", round((sum((ND$'t*e'>0.5)&(ND$'Outcome'>0.5))/nrow(ND))*100, digits=2), "%"),
                           'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',
                                          sum((ND$'t*e'>0.5)&(ND$'Outcome'>0.5)),"/", sum((ND$'Outcome'>0.5)),
                                          "=", round((sum((ND$'t*e'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),
                           'CaseNames'= if(sum((ND$'t*e'>0.5)&(ND$'Outcome'>0.5)) == 0) {'No cases in this intersection'} 
                           else {rownames(ND)[(ND$'t*e'>0.5)&(ND$'Outcome'>0.5)]}),
                'TEy'=list('Intersection'='Inconsistent Most Likely (T*E and Y < 0.5)', 
                           'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                          sum((ND$'T*E'>0.5)&(ND$'Outcome'<0.5)),"/", nrow(ND),
                                          "=", round((sum((ND$'T*E'>0.5)&(ND$'Outcome'<0.5))/nrow(ND))*100, digits=2), "%"),
                           'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',
                                          sum((ND$'T*E'>0.5)&(ND$'Outcome'<0.5)),"/", sum((ND$'Outcome'<0.5)),
                                          "=", round((sum((ND$'T*E'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),
                           'CaseNames'= if(sum((ND$'T*E'>0.5)&(ND$'Outcome'<0.5)) == 0) {'No cases in this intersection'} 
                           else {rownames(ND)[(ND$'T*E'>0.5)&(ND$'Outcome'<0.5)]}),
                'tEy'=list('Intersection'='Inconsistent Least Likely (t*E and Y < 0.5)', 
                           'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                          sum((ND$'t*E'>0.5)&(ND$'Outcome'<0.5)),"/", nrow(ND),
                                          "=", round((sum((ND$'t*E'>0.5)&(ND$'Outcome'<0.5))/nrow(ND))*100, digits=2), "%"),
                           'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',
                                          sum((ND$'t*E'>0.5)&(ND$'Outcome'<0.5)),"/", sum((ND$'Outcome'<0.5)),
                                          "=", round((sum((ND$'t*E'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),
                           'CaseNames'= if(sum((ND$'t*E'>0.5)&(ND$'Outcome'<0.5)) == 0) {'No cases in this intersection'} 
                           else {rownames(ND)[(ND$'t*E'>0.5)&(ND$'Outcome'<0.5)]}),
                'Tey'=list('Intersection'='Consistent Most Likely (T*e and Y < 0.5)', 
                           'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                          sum((ND$'T*e'>0.5)&(ND$'Outcome'<0.5)),"/", nrow(ND),
                                          "=", round((sum((ND$'T*e'>0.5)&(ND$'Outcome'<0.5))/nrow(ND))*100, digits=2), "%"),
                           'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',
                                          sum((ND$'T*e'>0.5)&(ND$'Outcome'<0.5)),"/", sum((ND$'Outcome'<0.5)),
                                          "=", round((sum((ND$'T*e'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),
                           'CaseNames'= if(sum((ND$'T*e'>0.5)&(ND$'Outcome'<0.5)) == 0) {'No cases in this intersection'} 
                           else {rownames(ND)[(ND$'T*e'>0.5)&(ND$'Outcome'<0.5)]}),
                'tey'=list('Intersection'='Consistent Least Likely (t*e and Y < 0.5)', 
                           'CaseNo'=paste('Cases in the intersection/Total number of cases: ',
                                          sum((ND$'t*e'>0.5)&(ND$'Outcome'<0.5)),"/", nrow(ND),
                                          "=", round((sum((ND$'t*e'>0.5)&(ND$'Outcome'<0.5))/nrow(ND))*100, digits=2), "%"),
                           'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',
                                          sum((ND$'t*e'>0.5)&(ND$'Outcome'<0.5)),"/", sum((ND$'Outcome'<0.5)),
                                          "=", round((sum((ND$'t*e'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),
                           'CaseNames'= if(sum((ND$'t*e'>0.5)&(ND$'Outcome'<0.5)) == 0) {'No cases in this intersection'} 
                           else {rownames(ND)[(ND$'t*e'>0.5)&(ND$'Outcome'<0.5)]}))
    class(CTE) <- 'casestheoryeval'
    return(CTE)
  }
