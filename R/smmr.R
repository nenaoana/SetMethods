smmr <-
  function(results,
           outcome,
           sol=1,
           match=NULL,
           cases=NULL,
           max_pairs=5,
           term = 1,
           ...)
  { 
    #print("Note that the name of the mmr function has been changes to smmr!")
    if (is.null(match)) stop('You have not specifyied whether you want single case MMR or comparative MMR. 
                             Please use match= FALSE for single case MMR or match= TRUE for comparative MMR.')
    if (is.null(cases)) stop('You have not specifyied the cases to be identified. 
                             Please do so by providing the argument cases with a numerical value 
                             according to the helpfile for function mmr.')
    dots <- list(...)
    if(length(dots) != 0){
      if ("neg.out" %in% names(dots)){print("Argument neg.out is deprecated. The negated outcome is identified automatically from the minimize solution.")}
      if ("use.tilde" %in% names(dots)){print("Argument use.tilde is deprecated. The usage of the tilde is identified automatically from the minimize solution.")}
    }
    if (!match) {
    if (cases==1) {R <- cases.suf.typ(results, outcome, sol)
    return(R)}
    if (cases==2) {R <- cases.suf.typ.fct(results, outcome, term, sol, max_pairs=max_pairs)
    return(R)}
    if (cases==3) {R <- cases.suf.dcn(results, outcome, sol)
    return(R)}
    if (cases==4) {R <- cases.suf.dcv(results, outcome, sol)
    return(R)}
    if (cases==5) {R <- cases.suf.iir(results, outcome, sol)
    return(R)}
    if (cases==6) {
            M<-list()
            R1 <- cases.suf.typ(results, outcome, sol)
            M[[1]] <- list(title="Typical", results=R1[[1]]$results)
            R2 <- cases.suf.typ.fct(results, outcome, term, sol, max_pairs = max_pairs)
            M[[2]] <- list(title="Typical for each Focal Conjunct", results=R2[[1]]$results)
            R3 <- cases.suf.dcn(results, outcome, sol)
            M[[3]] <- list(title="Deviant Consistency", results=R3[[1]]$results)
            R4 <- cases.suf.dcv(results, outcome, sol)
            M[[4]] <- list(title="Deviant Coverage", results=R4[[1]]$results)
            R5 <- cases.suf.iir(results, outcome, sol)
            M[[5]] <- list(title="Individually Irrelevant", results=R5[[1]]$results)
            class(M) <- 'matchessuf'
            return(M)}  
    if (cases>6 | cases<1) {print("Invalid case type. 
                                  Check help file using ?mmr for selecting a case type")}
  }
  else {
    if (cases==1) {R <- matches.suf.typtyp(results,
               outcome,
               term,
               sol,
               max_pairs) 
    return(R)}
    if (cases==2) {R <- matches.suf.typiir(results,
                                           outcome,
                                           term,
                                           sol,
                                           max_pairs) 
    return(R)}
    if (cases==3) {R <- matches.suf.typdcn(results,
                                           outcome,
                                           sol,
                                           max_pairs) 
    return(R)}
    if (cases==4) {R <- matches.suf.dcviir(results,
                                           outcome,
                                           sol,
                                           max_pairs) 
    return(R)}
    if (cases==5) {R <- matches.suf.typtypnfc(results,
                                           outcome,
                                           sol,
                                           max_pairs) 
    return(R)}
    if (cases==6) {R <- matches.suf.typiirnfc(results,
                                              outcome,
                                              sol,
                                              max_pairs) 
    return(R)}
    if (cases==7) {
      M <- list()
      R1 <-  matches.suf.typtyp(results,
                                outcome,
                                term,
                                sol,
                                max_pairs)
      M[[1]] <- list(title="Typical-Typical", results=R1)
      R2 <- matches.suf.typiir(results,
                               outcome,
                               term,
                               sol,
                               max_pairs) 
      M[[2]] <- list(title="Typical-IIR", results=R2)
      R3 <- matches.suf.typdcn(results,
                               outcome,
                               sol,
                               max_pairs)
      M[[3]] <- list(title="Typical-Dev.Cons.", results=R3)
      R4 <- matches.suf.dcviir(results,
                               outcome,
                               sol,
                               max_pairs) 
      M[[4]] <- list(title="Dev.Cov.-IIR", results=R4[[1]]$results)
      R5 <- matches.suf.typtypnfc(results,
                               outcome,
                               sol,
                               max_pairs) 
      M[[5]] <- list(title="Typical-Typical - Term", results=R5[[1]]$results)
      R6 <- matches.suf.typtypnfc(results,
                                  outcome,
                                  sol,
                                  max_pairs) 
      M[[6]] <- list(title="Typical-IIR - Term", results=R6[[1]]$results)
      class(M) <- 'matchessuf'
      return(M)}  
    if (cases>7 | cases<1) {print("Invalid case type. 
                                  Check help file using ?mmr for selecting a case type")}
  }
  }

mmr <- smmr

