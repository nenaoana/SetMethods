esa <-
  function (oldtt,
            nec_cond=c(),
            imposs_LR=c(),
            contrad_rows=c()
  )
  {TT<-oldtt
  if (length(nec_cond)>0){
    for (i in 1:length(nec_cond)){
      if(length(grep("+",nec_cond[i])) > 0){
        tl <- gsub('\\s', '', nec_cond[i])         
        tl <- unlist(strsplit(tl, '\\+'))
        ncon<-c()
        pcon<-c()
        for (i in 1:length(tl)){
          if(length(grep("~",tl[i])) > 0){
            cstr<-strsplit(tl[i],"~")
            cstr1<-unlist(cstr)
            ncon<-c(ncon, cstr1[2])}
          else {pcon<-c(pcon, tl[i])}}
        if (length(ncon)>0 & length(pcon)>0){
          for (i in 1:nrow(TT$tt)){
            if(all(sapply(TT$tt[i,ncon], identical, 0)) & all(sapply(TT$tt[i,pcon], identical, 0)) & TT$tt$OUT[i]=="?"){
              TT$tt[i, "OUT"] <- 0}}}
        else {if (length(ncon)>0) {
          for (i in 1:nrow(TT$tt)){
            if(all(sapply(TT$tt[i,ncon], identical, 0)) & TT$tt$OUT[i]=="?"){
              TT$tt[i, "OUT"] <- 0}}}
          else{if (length(pcon)>0) {
            for (i in 1:nrow(TT$tt)){
              if(all(sapply(TT$tt[i,pcon], identical, 0)) & TT$tt$OUT[i]=="?"){
                TT$tt[i, "OUT"] <- 0}}}}}
      }
      else {if(length(grep("~", nec_cond[i])) > 0){
        cstr<-strsplit(nec_cond[i],"~")
        cstr1<-unlist(cstr)
        TT$tt[(TT$tt[,cstr1[2]]==1) & (TT$tt$OUT=="?"), "OUT"] <- 0}
        else {TT$tt[(TT$tt[, nec_cond[i]]==0) & (TT$tt$OUT=="?"), "OUT"] <- 0}
      }
    } }
  if (length(imposs_LR)>0){
    for (i in 1:length(imposs_LR)){
      if(length(grep("*",imposs_LR[i])) > 0){
        tl <- gsub('\\s', '', imposs_LR[i])         
        tl <- unlist(strsplit(tl, '\\*'))
        ncon<-c()
        pcon<-c()
        for (i in 1:length(tl)){
          if(length(grep("~",tl[i])) > 0){
            cstr<-strsplit(tl[i],"~")
            cstr1<-unlist(cstr)
            ncon<-c(ncon, cstr1[2])}
          else {pcon<-c(pcon, tl[i])}}
        if (length(ncon)>0 & length(pcon)>0){
          for (i in 1:nrow(TT$tt)){
            if(all(sapply(TT$tt[i,ncon], identical, 0)) & all(sapply(TT$tt[i,pcon], identical, 1)) & TT$tt$OUT[i]=="?"){
              TT$tt[i, "OUT"] <- 0}}}
        else {if (length(ncon)>0) {
          for (i in 1:nrow(TT$tt)){
            if(all(sapply(TT$tt[i,ncon], identical, 0)) & TT$tt$OUT[i]=="?")
            {TT$tt[i, "OUT"] <- 0}}}
          else{if (length(pcon)>0) {
            for (i in 1:nrow(TT$tt)){
              if(all(sapply(TT$tt[i,pcon], identical, 1)) & TT$tt$OUT[i]=="?"){
                TT$tt[i, "OUT"] <- 0}}
          }}}
      }
      else { if(length(grep("~", imposs_LR[i])) > 0){
        cstr<-strsplit(imposs_LR[i],"~")
        cstr1<-unlist(cstr)
        TT$tt[(TT$tt[,cstr1[2]]==0) & (TT$tt$OUT=="?"), "OUT"] <- 0}
        else {TT$tt[(TT$tt[, nec_cond[i]]==1) & (TT$tt$OUT=="?"), "OUT"] <- 0}
      }
    }  
  }
  if (length(contrad_rows)>0){TT$tt[contrad_rows, "OUT"] <- 0}
  return(TT)}
