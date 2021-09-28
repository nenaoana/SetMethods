rob.inclrange <-
  function(
    data,
    step = 0.1,
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
    # Test range raw consistency threshold lower:
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
    incl.cut.tl = incl.cut
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...")
      incl.cut.tl = incl.cut.tl - step
      if (incl.cut.tl <= 0) { break }
      sol <- suppressWarnings(minimize(input = data,
                                       outcome  = outcome,
                                       conditions = conditions,
                                       incl.cut = incl.cut.tl,
                                       n.cut = n.cut,
                                       include = include,
                                       ...))
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
      if ((incl.cut-incl.cut.tl) >= (max.runs*step)) 
      {incl.cut.tl = NA
      break}
      if (incl.cut.tl<=0) {break}
    }
    
    # Test range raw consistency threshold upper:
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
    incl.cut.tu = incl.cut
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...")
      incl.cut.tu = incl.cut.tu + step
      if (incl.cut.tu >= 1) { break }
      sol <- try(suppressWarnings(minimize(input = data,
                                           outcome  = outcome,
                                           conditions = conditions,
                                           incl.cut = incl.cut.tu,
                                           n.cut = n.cut,
                                           include = include,
                                           ...)), silent = TRUE)
      if (class(sol) == "try-error") {break}
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
      if ((incl.cut.tu-incl.cut) >= (max.runs*step)) 
      {incl.cut.tu = NA
      break}
      if (incl.cut.tu>=0) {break}
    }
    RCT = c(incl.cut.tl+step, incl.cut.tu-step)
    TH <- data.frame(RCT)
    row.names(TH) <- c("Lower bound", "Upper bound")
    cat(c("Raw Consistency T.: ","Lower bound ", incl.cut.tl+step, "Threshold ", incl.cut , "Upper bound ", incl.cut.tu - step, "\n"))
    invisible(TH)
  }  
