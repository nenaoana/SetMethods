rob.ncutrange <-
  function(
    data,
    step = 1,
    max.runs = 20,
    outcome,
    conditions,
    incl.cut = 1,
    n.cut = 1,
    include = "",
    ...
  )
  {
    
    suppressWarnings(init.sol <- minimize(input = data,
                                          outcome  = outcome,
                                          conditions = conditions,
                                          incl.cut = incl.cut,
                                          n.cut = n.cut,
                                          include = include,
                                          ...))
    
    # Test range n.cut lower:
    suppressWarnings(sol <- minimize(input = data,
                                     outcome  = outcome,
                                     conditions = conditions,
                                     incl.cut = incl.cut,
                                     n.cut = n.cut,
                                     include = include,
                                     ...))
    if (is.null(init.sol$i.sol)) {
      is = init.sol$solution[[1]]
      s = sol$solution[[1]]
    }
    else {
      is = init.sol$i.sol$C1P1$solution[[1]]
      s = sol$i.sol$C1P1$solution[[1]]
    }
    n.cut.tl = n.cut
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...")
      n.cut.tl = n.cut.tl - step
      if (n.cut.tl < 1) { break }
      sol <- try(suppressWarnings(minimize(input = data,
                                           outcome  = outcome,
                                           conditions = conditions,
                                           incl.cut = incl.cut,
                                           n.cut = n.cut.tl,
                                           include = include,
                                           ...)), silent = TRUE)
      if ("try-error" %in% class(sol)) {break}
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
      
      if ((n.cut-n.cut.tl) >= max.runs*step) 
      {n.cut.tl = NA
      break}
    }
    
    # Test range n.cut upper:
    suppressWarnings(sol <- minimize(input = data,
                                     outcome  = outcome,
                                     conditions = conditions,
                                     incl.cut = incl.cut,
                                     n.cut = n.cut,
                                     include = include,
                                     ...))
    if (is.null(init.sol$i.sol)) {
      is = init.sol$solution[[1]]
      s = sol$solution[[1]]
    }
    else {
      is = init.sol$i.sol$C1P1$solution[[1]]
      s = sol$i.sol$C1P1$solution[[1]]
    }
    n.cut.tu = n.cut
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...")
      n.cut.tu = n.cut.tu + step
      if (n.cut.tl == nrow(data)) { break }
      sol <- try(suppressWarnings(minimize(input = data,
                                           outcome  = outcome,
                                           conditions = conditions,
                                           incl.cut = incl.cut,
                                           n.cut = n.cut.tu,
                                           include = include,
                                           ...)), silent = TRUE)
      if ("try-error" %in% class(sol)) {break}
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
      if ((n.cut.tu - n.cut) >= max.runs*step) 
      {n.cut.tu = NA
      break}
    }
    
    NCUT = c(n.cut.tl+step, n.cut.tu-step)
    TH <- data.frame(NCUT)
    row.names(TH) <- c("Lower bound", "Upper bound")
    cat(c("N.Cut: ","Lower bound ", n.cut.tl + step, "Threshold ", n.cut , "Upper bound ", n.cut.tu-step, "\n"))
    invisible(TH)
  }  