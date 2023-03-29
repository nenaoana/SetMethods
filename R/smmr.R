smmr <-
  function(results,
           outcome,
           sol=1,
           match=NULL,
           cases=NULL,
           max_pairs=5,
           term = 1,
           nec.cond =NULL,
           necessity = FALSE,
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
    if (!necessity){
    if (!match) {
    if (cases==1) {R <- cases.suf.typ(results=results, outcome=outcome, sol=sol)
    return(R)}
    if (cases==2) {R <- cases.suf.typ.fct(results=results,
                                          outcome=outcome,
                                          term=term,
                                          sol=sol,
                                          max_pairs = max_pairs,
                                          nec.cond = nec.cond)
    return(R)}
    if (cases==3) {R <- cases.suf.dcn(results=results, outcome=outcome, sol=sol)
    return(R)}
    if (cases==4) {R <- cases.suf.dcv(results=results, outcome=outcome, sol=sol)
    return(R)}
    if (cases==5) {R <- cases.suf.iir(results=results, outcome=outcome, sol=sol)
    return(R)}
    if (cases==6) {
            M<-list()
            R1 <- cases.suf.typ(results=results, outcome=outcome, sol=sol)
            M[[1]] <- list(title="Typical", results=R1[[1]]$results)
            R2 <- cases.suf.typ.fct(results=results,
                                    outcome=outcome,
                                    term=term,
                                    sol=sol,
                                    max_pairs = max_pairs,
                                    nec.cond = nec.cond)
            M[[2]] <- list(title="Typical for each Focal Conjunct", results=R2[[1]]$results)
            R3 <- cases.suf.dcn(results=results, outcome=outcome, sol=sol)
            M[[3]] <- list(title="Deviant Consistency", results=R3[[1]]$results)
            R4 <- cases.suf.dcv(results=results, outcome=outcome, sol=sol)
            M[[4]] <- list(title="Deviant Coverage", results=R4[[1]]$results)
            R5 <- cases.suf.iir(results=results, outcome=outcome, sol=sol)
            M[[5]] <- list(title="Individually Irrelevant", results=R5[[1]]$results)
            class(M) <- 'matchessuf'
            return(M)}  
    if (cases>6 | cases<1) {print("Invalid case type. 
                                  Check help file using ?smmr for selecting a case type")}
  }
  else {
    if (cases==1) {R <- matches.suf.typtyp(results=results,
               outcome=outcome,
               term=term,
               sol=sol,
               max_pairs = max_pairs,
               nec.cond = nec.cond) 
    return(R)}
    if (cases==2) {R <- matches.suf.typiir(results=results,
                                           outcome=outcome,
                                           term=term,
                                           sol=sol,
                                           max_pairs = max_pairs,
                                           nec.cond = nec.cond) 
    return(R)}
    if (cases==3) {R <- matches.suf.typdcn(results=results, 
                                           outcome=outcome, 
                                           sol=sol,
                                           max_pairs=max_pairs) 
    return(R)}
    if (cases==4) {R <- matches.suf.dcviir(results=results, 
                                           outcome=outcome, 
                                           sol=sol,
                                           max_pairs=max_pairs) 
    return(R)}
    if (cases==5) {R <- matches.suf.typtypnfc(results=results, 
                                              outcome=outcome, 
                                              sol=sol,
                                              max_pairs=max_pairs) 
    return(R)}
    if (cases==6) {R <- matches.suf.typiirnfc(results=results, 
                                              outcome=outcome, 
                                              sol=sol,
                                              max_pairs=max_pairs) 
    return(R)}
    if (cases==7) {
      M <- list()
      R1 <-  matches.suf.typtyp(results=results,
                                outcome=outcome,
                                term=term,
                                sol=sol,
                                max_pairs = max_pairs,
                                nec.cond = nec.cond)
      M[[1]] <- list(title="Typical-Typical", results=R1)
      R2 <- matches.suf.typiir(results=results,
                               outcome=outcome,
                               term=term,
                               sol=sol,
                               max_pairs = max_pairs,
                               nec.cond = nec.cond) 
      M[[2]] <- list(title="Typical-IIR", results=R2)
      R3 <- matches.suf.typdcn(results=results, 
                               outcome=outcome, 
                               sol=sol,
                               max_pairs=max_pairs)
      M[[3]] <- list(title="Typical-Dev.Cons.", results=R3)
      R4 <- matches.suf.dcviir(results=results, 
                               outcome=outcome, 
                               sol=sol,
                               max_pairs=max_pairs) 
      M[[4]] <- list(title="Dev.Cov.-IIR", results=R4[[1]]$results)
      R5 <- matches.suf.typtypnfc(results=results, 
                                  outcome=outcome, 
                                  sol=sol,
                                  max_pairs=max_pairs) 
      M[[5]] <- list(title="Typical-Typical - Term", results=R5[[1]]$results)
      R6 <- matches.suf.typiirnfc(results=results, 
                                  outcome=outcome, 
                                  sol=sol,
                                  max_pairs=max_pairs) 
      M[[6]] <- list(title="Typical-IIR - Term", results=R6[[1]]$results)
      class(M) <- 'matchessuf'
      return(M)}  
    if (cases>7 | cases<1) {print("Invalid case type. 
                                  Check help file using ?smmr for selecting a case type")}
  }
    }
    else{
      if (!match) {
        if (cases==1) {R <- cases.nec.typ(nec.cond=nec.cond,
                                          results=results,
                                          outcome=outcome,
                                          sol=sol)
        return(R)}
        if (cases==2) {R <- cases.nec.dcn(nec.cond=nec.cond,
                                          results=results,
                                          outcome=outcome,
                                          sol=sol)
        return(R)}
        if (cases==3) {R <- cases.nec.drel(nec.cond=nec.cond,
                                           results=results,
                                           outcome=outcome,
                                           sol=sol)
        return(R)}
        if (cases==4) {
          M<-list()
          R1 <- cases.nec.typ(nec.cond=nec.cond,
                              results=results,
                              outcome=outcome,
                              sol=sol)
          M[[1]] <- list(title="Typical", results=R1[[1]]$results)
          R2 <- cases.nec.dcn(nec.cond=nec.cond,
                              results=results,
                              outcome=outcome,
                              sol=sol)
          M[[2]] <- list(title="Deviant Consistency", results=R2[[1]]$results)
          R3 <- cases.nec.drel(nnec.cond=nec.cond,
                               results=results,
                               outcome=outcome,
                               sol=sol)
          M[[3]] <- list(title="Deviant Relevance", results=R3[[1]]$results)
          class(M) <- 'matchessuf'
          return(M)}  
        if (cases>4 | cases<1) {print("Invalid case type. 
                                  Check help file using ?smmr for selecting a case type")}
      }
      else {
        if (cases==1) {R <- matches.nec.typdrel(nec.cond=nec.cond,
                                                results=results,
                                                outcome=outcome,
                                                sol=sol,
                                                max_pairs=max_pairs) 
        return(R)}
        if (cases==2) {R <- matches.nec.typdcn(nec.cond=nec.cond,
                                               results=results,
                                               outcome=outcome,
                                               sol=sol,
                                               max_pairs=max_pairs) 
        return(R)}
        if (cases==3) {R <- matches.nec.typiir(nec.cond=nec.cond,
                                               results=results,
                                               outcome=outcome,
                                               sol=sol,
                                               max_pairs=max_pairs) 
        return(R)}
        if (cases==4) {R <- matches.nec.typtyp(nec.cond=nec.cond,
                                               results=results,
                                               outcome=outcome,
                                               sol=sol,
                                               max_pairs=max_pairs) 
        return(R)}
        if (cases==5) {
          M <- list()
          R1 <-  matches.nec.typdrel(nec.cond=nec.cond,
                                     results=results,
                                     outcome=outcome,
                                     sol=sol,
                                     max_pairs=max_pairs)
          M[[1]] <- list(title="Typical-Dev.Rel.", results=R1)
          R2 <- matches.nec.typdcn(nec.cond=nec.cond,
                                   results=results,
                                   outcome=outcome,
                                   sol=sol,
                                   max_pairs=max_pairs) 
          M[[2]] <- list(title="Typical-Dev.Cons.", results=R2)
          R3 <- matches.nec.typiir(nec.cond=nec.cond,
                                   results=results,
                                   outcome=outcome,
                                   sol=sol,
                                   max_pairs=max_pairs)
          M[[3]] <- list(title="Typical-IIR", results=R3)
          R4 <- matches.nec.typtyp(nec.cond=nec.cond,
                                   results=results,
                                   outcome=outcome,
                                   sol=sol,
                                   max_pairs=max_pairs) 
          M[[4]] <- list(title="Typical-Typical", results=R4[[1]]$results)
          class(M) <- 'matchessuf'
          return(M)}  
        if (cases>5 | cases<1) {print("Invalid case type. 
                                  Check help file using ?smmr for selecting a case type")}
      }
      
    }
  }

mmr <- smmr

