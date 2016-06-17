MMR <-
function(results, 
           outcome, 
           neg.out=F, 
           intermed=F, 
           sol=1)
  
{
    x <- readline("Sufficiency single case MMR ==> type 1\nSufficiency Comparative MMR ==> type 2: ")
    if (x==1){
    z <-readline("\nTypical ==> type 1\nMost Typical ==> type 2\nUniquely Covered ==> type 3\nDeviant Consistency ==> type 4\nDeviant Coverage ==> type 5\nIIR ==> type 6\nNB. For multiple analyses separate imputed numbers by a comma (,): ")
    z <- as.numeric(unlist(strsplit(z, ",")))
    mmr <- list()
    if (any(z %in% c(1,2,3,4,5,6))){ 
    for (i in 1:length(z)) {
    if (z[i]==1) { 
                    typ <- cases.suf.typ(results = results, outcome = outcome,
                                          neg.out = neg.out, intermed = intermed, 
                                          sol = sol)
                    mmr<-list(mmr, 'typ'=typ)
    }
      else{
    if (z[i]==2) { 
      typ.most <- cases.suf.typ.most(results = results, outcome = outcome,
                           neg.out = neg.out, intermed = intermed, 
                           sol = sol)
      mmr<-list(mmr, 'typ.most'=typ.most)
    
    }
        else{
    if (z[i]==3) { 
      typ.unique <- cases.suf.typ.unique(results = results, outcome = outcome,
                                neg.out = neg.out, intermed = intermed, 
                                sol = sol)
      mmr<-list(mmr, 'typ.unique'=typ.unique)
    
    }
          else{
    if (z[i]==4) { 
      dcn <- cases.suf.dcn(results = results, outcome = outcome,
                                neg.out = neg.out, intermed = intermed, 
                                sol = sol)
      mmr<-list(mmr, 'dcn'=dcn)
  
    }
          else{
    if (z[i]==5) { 
      dcv <- cases.suf.dcv(results = results, outcome = outcome,
                           neg.out = neg.out, intermed = intermed, 
                           sol = sol)
      mmr<-list(mmr, 'dcv'=dcv)
     
    }
          else{  
    if (z[i]==6) { 
      iir <- cases.suf.iir(results = results, outcome = outcome,
                           neg.out = neg.out, intermed = intermed, 
                           sol = sol)
      mmr<-list(mmr, 'iir'=iir)
  
    }}}}}}}
      class(mmr) <- 'multimethod'
      return(mmr)
      }
    else
        {print(paste("\nSorry, you entered a wrong number! Start again and enter a number or a list from 1 to 6 according to the analysis you want."))}
    }
    else {
      if (x==2){
        z<-readline("\nMatch Typical with Deviant Consistency ==> type 1\nMatch Deviant Coverage with IRR ==> type 2\nMatch Typical with Typical for a Suff. Term  ==> type 3\nMatch Typical with IRR for a sufficient term ==> type 4\nNB. For multiple analyses separate imputed numbers by a comma (,): ")
        z <- as.numeric(unlist(strsplit(z, ",")))
        mmr <- list()
        if (any(z %in% c(1,2,3,4))){ 
        for (i in 1:length(z)) {
        if (z[i]==1) {
          max_pairs<-as.numeric(readline("\nEnter maximum number of pairs Typical-Dev.Cons. (default is 5) "))
          typdcn <- matches.suf.typdcn(results = results, outcome = outcome,
                               neg.out = neg.out, intermed = intermed, 
                               sol = sol, max_pairs=max_pairs)
          mmr<-list(mmr, 'typdcn'=typdcn)
        }
        else{  
        if (z[i]==2) {
          max_pairs<-as.numeric(readline("\nEnter maximum number of pairs Dev.Cov.-IIR (default is 5) "))
          dcviir <- matches.suf.dcviir(results = results, outcome = outcome,
                                    neg.out = neg.out, intermed = intermed, 
                                    sol = sol, max_pairs=max_pairs)
          mmr<-list(mmr, 'dcviir'=dcviir)
        }
        else {  
        if (z[i]==3) {
          term<-as.numeric(readline("\nEnter sufficient term (default is 1) "))
          max_pairs<-as.numeric(readline("\nEnter maximum number of pairs Typical-Typical (default is 5) "))
          typtyp <- matches.suf.typtyp(results = results, outcome = outcome, term=term,
                                      neg.out = neg.out, intermed = intermed, 
                                      sol = sol, max_pairs=max_pairs)
          mmr<-list(mmr, 'typtyp'=typtyp)
        }
        else {  
        if (z[i]==4) { 
          term<-as.numeric(readline("\nEnter sufficient term (default is 1) "))
          max_pairs<-as.numeric(readline("\nEnter maximum number of pairs Typical-IIR (default is 5) "))
          typiir <- matches.suf.typiir(results = results, outcome = outcome, term=term,
                                    neg.out = neg.out, intermed = intermed, 
                                    sol = sol, max_pairs=max_pairs)
          mmr<-list(mmr, 'typiir'=typiir)
        }}}}}
          class(mmr) <- 'multimethod'
          return(mmr)
          }
        else
            {print(paste("\nSorry, you entered a wrong number! Start again and enter a number or a list from 1 to 4 according to the analysis you want."))}
      }
      else {print(paste("\nSorry, you entered a wrong number! Start again and enter a 1 for single case MMR or a 2 for comparative MMR."))}
    }
}
