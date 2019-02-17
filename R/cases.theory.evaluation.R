cases.theory.evaluation <-

  function(theory, 

           empirics, 

           outcome,

           sol=1, 

           use.tilde = TRUE)

  {

    ND <- theory.data(theory=theory, empirics=empirics, outcome=outcome, sol=sol, use.tilde = use.tilde)

    INT <- theory.intersections(theory=theory, empirics=empirics, sol=sol, use.tilde = use.tilde)

    CTE <- list('TEY'=list('Intersection'='Covered Most Likely (T*E and Y > 0.5)',

                           'Boolean'=paste('Boolean Expression:', INT$TE),

                           'CaseNo'=paste('Cases in the intersection/Total number of cases:',

                                          sum((ND$'T*E'>0.5)&(ND$'Outcome'>0.5)),"/",nrow(ND),

                                          "=", round((sum((ND$'T*E'>0.5)&(ND$'Outcome'>0.5))/nrow(ND))*100, digits=2), "%"),

                           'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',

                                          sum((ND$'T*E'>0.5)&(ND$'Outcome'>0.5)),"/",sum((ND$'Outcome'>0.5)),

                                          "=", round((sum((ND$'T*E'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),

                           'CaseNames'= if(sum((ND$'T*E'>0.5)&(ND$'Outcome'>0.5)) == 0) {'No cases in this intersection'} 

                           else {rownames(ND)[(ND$'T*E'>0.5)&(ND$'Outcome'>0.5)]}),

                '~TEY'=list('Intersection'='Covered Least Likely (~T*E and Y > 0.5)',

                           'Boolean'=paste('Boolean Expression:', INT$tE),

                           'CaseNo'=paste('Cases in the intersection/Total number of cases:',

                                          sum((ND$'~T*E'>0.5)&(ND$'Outcome'>0.5)),"/",nrow(ND),

                                          "=", round((sum((ND$'~T*E'>0.5)&(ND$'Outcome'>0.5))/nrow(ND))*100, digits=2), "%"),

                           'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',

                                          sum((ND$'~T*E'>0.5)&(ND$'Outcome'>0.5)),"/",sum((ND$'Outcome'>0.5)),

                                          "=", round((sum((ND$'~T*E'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),

                           'CaseNames'= if(sum((ND$'~T*E'>0.5)&(ND$'Outcome'>0.5)) == 0) {'No cases in this intersection'} 

                           else {rownames(ND)[(ND$'~T*E'>0.5)&(ND$'Outcome'>0.5)]}),

                'T~EY'=list('Intersection'='Uncovered Most Likely (T*~E and Y > 0.5)',

                           'Boolean'=paste('Boolean Expression:', INT$Te),

                           'CaseNo'=paste('Cases in the intersection/Total number of cases: ',

                                          sum((ND$'T*~E'>0.5)&(ND$'Outcome'>0.5)),"/", nrow(ND),

                                          "=", round((sum((ND$'T*~E'>0.5)&(ND$'Outcome'>0.5))/nrow(ND))*100, digits=2), "%"),

                           'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',

                                          sum((ND$'T*~E'>0.5)&(ND$'Outcome'>0.5)),"/", sum((ND$'Outcome'>0.5)),

                                          "=", round((sum((ND$'T*~E'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),

                           'CaseNames'= if(sum((ND$'T*~E'>0.5)&(ND$'Outcome'>0.5)) == 0) {'No cases in this intersection'} 

                           else {rownames(ND)[(ND$'T*~E'>0.5)&(ND$'Outcome'>0.5)]}),

                '~T~EY'=list('Intersection'='Uncovered Least Likely (~T*~E and Y > 0.5)',

                           'Boolean'=paste('Boolean Expression:', INT$te),

                           'CaseNo'=paste('Cases in the intersection/Total number of cases: ',

                                          sum((ND$'~T*~E'>0.5)&(ND$'Outcome'>0.5)),"/", nrow(ND),

                                          "=", round((sum((ND$'~T*~E'>0.5)&(ND$'Outcome'>0.5))/nrow(ND))*100, digits=2), "%"),

                           'CaseNo2'=paste('Cases in the intersection/Total number of cases Y > 0.5: ',

                                          sum((ND$'~T*~E'>0.5)&(ND$'Outcome'>0.5)),"/", sum((ND$'Outcome'>0.5)),

                                          "=", round((sum((ND$'~T*~E'>0.5)&(ND$'Outcome'>0.5))/sum((ND$'Outcome'>0.5)))*100, digits=2), "%"),

                           'CaseNames'= if(sum((ND$'~T*~E'>0.5)&(ND$'Outcome'>0.5)) == 0) {'No cases in this intersection'} 

                           else {rownames(ND)[(ND$'~T*~E'>0.5)&(ND$'Outcome'>0.5)]}),

                'TE~Y'=list('Intersection'='Inconsistent Most Likely (T*E and Y < 0.5)',

                           'Boolean'=paste('Boolean Expression:', INT$TE),

                           'CaseNo'=paste('Cases in the intersection/Total number of cases: ',

                                          sum((ND$'T*E'>0.5)&(ND$'Outcome'<0.5)),"/", nrow(ND),

                                          "=", round((sum((ND$'T*E'>0.5)&(ND$'Outcome'<0.5))/nrow(ND))*100, digits=2), "%"),

                           'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',

                                          sum((ND$'T*E'>0.5)&(ND$'Outcome'<0.5)),"/", sum((ND$'Outcome'<0.5)),

                                          "=", round((sum((ND$'T*E'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),

                           'CaseNames'= if(sum((ND$'T*E'>0.5)&(ND$'Outcome'<0.5)) == 0) {'No cases in this intersection'} 

                           else {rownames(ND)[(ND$'T*E'>0.5)&(ND$'Outcome'<0.5)]}),

                '~TE~Y'=list('Intersection'='Inconsistent Least Likely (~T*E and Y < 0.5)',

                           'Boolean'=paste('Boolean Expression:', INT$tE),

                           'CaseNo'=paste('Cases in the intersection/Total number of cases: ',

                                          sum((ND$'~T*E'>0.5)&(ND$'Outcome'<0.5)),"/", nrow(ND),

                                          "=", round((sum((ND$'~T*E'>0.5)&(ND$'Outcome'<0.5))/nrow(ND))*100, digits=2), "%"),

                           'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',

                                          sum((ND$'~T*E'>0.5)&(ND$'Outcome'<0.5)),"/", sum((ND$'Outcome'<0.5)),

                                          "=", round((sum((ND$'~T*E'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),

                           'CaseNames'= if(sum((ND$'~T*E'>0.5)&(ND$'Outcome'<0.5)) == 0) {'No cases in this intersection'} 

                           else {rownames(ND)[(ND$'~T*E'>0.5)&(ND$'Outcome'<0.5)]}),

                'T~E~Y'=list('Intersection'='Consistent Most Likely (T*~E and Y < 0.5)',

                           'Boolean'=paste('Boolean Expression:', INT$Te),

                           'CaseNo'=paste('Cases in the intersection/Total number of cases: ',

                                          sum((ND$'T*~E'>0.5)&(ND$'Outcome'<0.5)),"/", nrow(ND),

                                          "=", round((sum((ND$'T*~E'>0.5)&(ND$'Outcome'<0.5))/nrow(ND))*100, digits=2), "%"),

                           'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',

                                          sum((ND$'T*~E'>0.5)&(ND$'Outcome'<0.5)),"/", sum((ND$'Outcome'<0.5)),

                                          "=", round((sum((ND$'T*~E'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),

                           'CaseNames'= if(sum((ND$'T*~E'>0.5)&(ND$'Outcome'<0.5)) == 0) {'No cases in this intersection'} 

                           else {rownames(ND)[(ND$'T*~E'>0.5)&(ND$'Outcome'<0.5)]}),

                '~T~E~Y'=list('Intersection'='Consistent Least Likely (~T*~E and Y < 0.5)',

                           'Boolean'=paste('Boolean Expression:', INT$te),

                           'CaseNo'=paste('Cases in the intersection/Total number of cases: ',

                                          sum((ND$'~T*~E'>0.5)&(ND$'Outcome'<0.5)),"/", nrow(ND),

                                          "=", round((sum((ND$'~T*~E'>0.5)&(ND$'Outcome'<0.5))/nrow(ND))*100, digits=2), "%"),

                           'CaseNo2'=paste('Cases in the intersection/Total number of cases Y < 0.5: ',

                                          sum((ND$'~T*~E'>0.5)&(ND$'Outcome'<0.5)),"/", sum((ND$'Outcome'<0.5)),

                                          "=", round((sum((ND$'~T*~E'>0.5)&(ND$'Outcome'<0.5))/sum((ND$'Outcome'<0.5)))*100, digits=2), "%"),

                           'CaseNames'= if(sum((ND$'~T*~E'>0.5)&(ND$'Outcome'<0.5)) == 0) {'No cases in this intersection'} 

                           else {rownames(ND)[(ND$'~T*~E'>0.5)&(ND$'Outcome'<0.5)]}))

    class(CTE) <- 'casestheoryeval'

    return(CTE)

  }
