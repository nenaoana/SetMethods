stargazerTT <- 
  function(truthtable,
           show.cases = FALSE,
           type = "latex", 
           title = "",
           out = NULL,
           digits = 3)
{
if (!("QCA_tt" %in% class(truthtable))) stop ("The result provided must be a truth table obtained with the truthTable function!")
    suppressWarnings(truthtable$tt$incl <- round(as.numeric(truthtable$tt$incl), digits = digits))
    suppressWarnings(truthtable$tt$PRI <- round(as.numeric(truthtable$tt$PRI), digits = digits))
    suppressWarnings(truthtable$tt$OUT <- as.numeric(truthtable$tt$OUT))
orderTT <- truthtable$tt
if (show.cases != TRUE){orderTT <- orderTT[,-which(names(orderTT) %in% c("cases"))]}
orderTT <- orderTT[order(-orderTT$OUT, -orderTT$incl, -orderTT$n, -orderTT$PRI),]
orderTT$OUT[is.na(orderTT$OUT)] <- "?"
suppressWarnings(stargazer(orderTT, summary = FALSE, type = type, title = title, out = out))
}