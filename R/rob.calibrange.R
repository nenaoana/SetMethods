rob.calibrange <-
  function(
    raw.data,
    calib.data,
    test.cond.raw,
    test.cond.calib,
    test.thresholds,
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
    calib.data[,test.cond.calib] <- calibrate(raw.data[,test.cond.raw], type="fuzzy", thresholds = test.thresholds)
    suppressWarnings(init.sol <- minimize(input = calib.data,
                                          outcome  = outcome,
                                          conditions = conditions,
                                          incl.cut = incl.cut,
                                          n.cut = n.cut,
                                          include = include,
                                          ...))
    
    # Testing the 0.5 range:
    tu.thresholds = test.thresholds
    suppressWarnings(sol <- minimize(input = calib.data,
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
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...")
      tu.thresholds[2] = tu.thresholds[2] + step;
      c.data = calib.data;
      c.data[,test.cond.calib] <- calibrate(raw.data[,test.cond.raw], type="fuzzy", thresholds = tu.thresholds);
      suppressWarnings(sol <- minimize(input = c.data,
                                       outcome  = outcome,
                                       conditions = conditions,
                                       incl.cut = incl.cut,
                                       n.cut = n.cut,
                                       include = include,
                                       ...));
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
      if ((tu.thresholds[2] - test.thresholds[2]) == max.runs*step) 
      {tu.thresholds[2] = NA
      break}
      if (tu.thresholds[2]>= range(raw.data[,test.cond.raw])[2]) {break}
    }
    
    tl.thresholds = test.thresholds
    suppressWarnings(sol <- minimize(input = calib.data,
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
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...")
      tl.thresholds[2] = tl.thresholds[2] - step
      c.data = calib.data
      c.data[,test.cond.calib] <- calibrate(raw.data[,test.cond.raw], type="fuzzy", thresholds = tl.thresholds)
      suppressWarnings(sol <- minimize(input = c.data,
                                       outcome  = outcome,
                                       conditions = conditions,
                                       incl.cut = incl.cut,
                                       n.cut = n.cut,
                                       include = include,
                                       ...))
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
      if ((test.thresholds[2]-tl.thresholds[2]) == max.runs*step) 
      {tl.thresholds[2] = NA
      break}
      if (tl.thresholds[2]<= range(raw.data[,test.cond.raw])[1]) {break}
    }
    
    # Testing the 1  range:
    tu1.thresholds = test.thresholds
    suppressWarnings(sol <- minimize(input = calib.data,
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
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...")
      tu1.thresholds[3] = tu1.thresholds[3] + step
      c.data = calib.data
      c.data[,test.cond.calib] <- calibrate(raw.data[,test.cond.raw], type="fuzzy", thresholds = tu1.thresholds)
      suppressWarnings(sol <- minimize(input = c.data,
                                       outcome  = outcome,
                                       conditions = conditions,
                                       incl.cut = incl.cut,
                                       n.cut = n.cut,
                                       include = include,
                                       ...))
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
      if ((tu1.thresholds[3]-test.thresholds[3]) == max.runs*step) 
      {tu1.thresholds[3] = NA
      break}
    }
    
    tl1.thresholds = test.thresholds
    suppressWarnings(sol <- minimize(input = calib.data,
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
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...");
      tl1.thresholds[3] = tl1.thresholds[3] - step
      c.data = calib.data
      c.data[,test.cond.calib] <- calibrate(raw.data[,test.cond.raw], type="fuzzy", thresholds = tl1.thresholds)
      suppressWarnings(sol <- minimize(input = c.data,
                                       outcome  = outcome,
                                       conditions = conditions,
                                       incl.cut = incl.cut,
                                       n.cut = n.cut,
                                       include = include,
                                       ...))
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
      if ((test.thresholds[3]-tl1.thresholds[3]) == max.runs*step) 
      {tl1.thresholds[3] = NA
      break}
    }
    
    # Testing the 0 range:
    tu0.thresholds = test.thresholds
    suppressWarnings(sol <- minimize(input = calib.data,
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
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...")
      tu0.thresholds[1] = tu0.thresholds[1] + step
      c.data = calib.data
      c.data[,test.cond.calib] <- calibrate(raw.data[,test.cond.raw], type="fuzzy", thresholds = tu0.thresholds)
      suppressWarnings(sol <- minimize(input = c.data,
                                       outcome  = outcome,
                                       conditions = conditions,
                                       incl.cut = incl.cut,
                                       n.cut = n.cut,
                                       include = include,
                                       ...))
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
      if ((tu0.thresholds[1]-test.thresholds[1]) == max.runs*step) 
      {tu0.thresholds[1] = NA
      break}
    }
    
    tl0.thresholds = test.thresholds
    suppressWarnings(sol <- minimize(input = calib.data,
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
    while (setequal(is,s))
    { print("Searching for thresholds, this takes me a while for now, sorry...")
      tl0.thresholds[1] = tl0.thresholds[1] - step
      c.data = calib.data
      c.data[,test.cond.calib] <- calibrate(raw.data[,test.cond.raw], type="fuzzy", thresholds = tl0.thresholds)
      sol <- suppressWarnings(minimize(input = c.data,
                                       outcome  = outcome,
                                       conditions = conditions,
                                       incl.cut = incl.cut,
                                       n.cut = n.cut,
                                       include = include,
                                       ...))
      if (is.null(init.sol$i.sol)) {
        s = sol$solution[[1]]
      }
      else {
        s = sol$i.sol$C1P1$solution[[1]]
      }
      if ((test.thresholds[1]-tl0.thresholds[1]) == max.runs*step) 
      {tl0.thresholds[1] = NA
      break}
    }
    
    E = c(tl0.thresholds[1]+step, tu0.thresholds[1]-step)
    C = c(tl.thresholds[2]+step, tu.thresholds[2]-step)
    I = c(tl1.thresholds[3]+step, tu1.thresholds[3]-step)
    TH <- data.frame(E,C,I)
    row.names(TH) <- c("Lower bound", "Upper bound")
    cat(c("Exclusion: ","Lower bound ", tl0.thresholds[1]+step, "Threshold ", test.thresholds[1] , "Upper bound ", tu0.thresholds[1]-step, "\n"))
    cat(c("Crossover: ","Lower bound ", tl.thresholds[2]+step, "Threshold ", test.thresholds[2] , "Upper bound ", tu.thresholds[2]-step, "\n"))
    cat(c("Inclusion: ","Lower bound ", tl1.thresholds[3]+step, "Threshold ", test.thresholds[3] , "Upper bound ", tu1.thresholds[3]-step, "\n"))
    invisible(TH)
  }  