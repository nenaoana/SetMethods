cases.theory.evaluation <-
function(theory_eval)
  {
    ND <- theory_eval
    list('Covered Most Likely (T*E and Y > 0.5)'=rownames(ND)[(ND$'T*E'>0.5)&(ND$'Outcome'>0.5)],
         'Covered Least Likely (t*E and Y > 0.5)'=rownames(ND)[(ND$'t*E'>0.5)&(ND$'Outcome'>0.5)],
         'Uncovered Most Likely (T*e and Y > 0.5)'=rownames(ND)[(ND$'T*e'>0.5)&(ND$'Outcome'>0.5)],
         'Uncovered Least Likely (t*e and Y > 0.5)'=rownames(ND)[(ND$'t*e'>0.5)&(ND$'Outcome'>0.5)],
         'Inconsistent Most Likely (T*E and Y < 0.5)'=rownames(ND)[(ND$'T*E'>0.5)&(ND$'Outcome'<0.5)],
         'Inconsistent Least Likely (t*E and Y < 0.5)'=rownames(ND)[(ND$'t*E'>0.5)&(ND$'Outcome'<0.5)],
         'Consistent Most Likely (T*e and Y < 0.5)'=rownames(ND)[(ND$'T*e'>0.5)&(ND$'Outcome'<0.5)],
         'Consistent Least Likely (t*e and Y < 0.5)'=rownames(ND)[(ND$'t*e'>0.5)&(ND$'Outcome'<0.5)])
  }
