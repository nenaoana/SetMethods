esa <-
  function (oldtt,
            nec_cond=c(),
            untenable_LR=c(),
            contrad_rows=c()
  )
  {TT<-oldtt
  if (length(nec_cond)>0){
    for (i in 1:length(nec_cond)){
      if(length(grep("\\+",nec_cond[i])) > 0){
        tl <- gsub('\\s', '', nec_cond[i])         
        tl <- unlist(strsplit(tl, '\\+'))
        ncon<-c()
        pcon<-c()
        ncond<-c()
        pcond<-c()
        for (k in 1:length(tl)){
          if(length(grep("\\*",tl[k])) > 0) {
            tld <- gsub('\\s', '', tl[k])         
            tld <- unlist(strsplit(tld, '\\*'))
            for (j in 1:length(tld)){
              if(length(grep("~",tld[j])) > 0){
                cstrd<-strsplit(tld[j],"~")
                cstrd1<-unlist(cstrd)
                ncond<-c(ncond, cstrd1[2])}
              else {pcond<-c(pcond, tld[j])}
            }
          }
          else {
          if(length(grep("~",tl[k])) > 0){
            cstr<-strsplit(tl[k],"~")
            cstr1<-unlist(cstr)
            ncon<-c(ncon, cstr1[2])}
          else {pcon<-c(pcon, tl[k])}
          }
        }
        if (length(ncond)==1 & length(pcond)==1){
          
          if (length(ncon)>0 & length(pcon)>0){
            for (l in 1:nrow(TT$tt)){
              if(all(sapply(TT$tt[l,ncon], identical, 1)) & all(sapply(TT$tt[l,pcon], identical, 0)) & (all(sapply(TT$tt[l,ncond], identical, 1)) | all(sapply(TT$tt[l,pcond], identical, 0))) & TT$tt$OUT[l]=="?"){
                TT$tt[l, "OUT"] <- 0}}}
          else {if (length(ncon)>0) {
            for (l in 1:nrow(TT$tt)){
              if(all(sapply(TT$tt[l,ncon], identical, 1)) & (all(sapply(TT$tt[l,ncond], identical, 1)) | all(sapply(TT$tt[l,pcond], identical, 0))) & TT$tt$OUT[l]=="?"){
                TT$tt[l, "OUT"] <- 0}}}
            else{if (length(pcon)>0) {
              for (l in 1:nrow(TT$tt)){
                if(all(sapply(TT$tt[l,pcon], identical, 0)) & (all(sapply(TT$tt[l,ncond], identical, 1)) | all(sapply(TT$tt[l,pcond], identical, 0))) & TT$tt$OUT[l]=="?"){
                  TT$tt[l, "OUT"] <- 0}}}}}
          }
        
        else {
          if (length(ncond)==2 & length(pcond)==0) {
            
            if (length(ncon)>0 & length(pcon)>0){
              for (l in 1:nrow(TT$tt)){
                if(all(sapply(TT$tt[l,ncon], identical, 1)) & all(sapply(TT$tt[l,pcon], identical, 0)) & (all(sapply(TT$tt[l,ncond[1]], identical, 1)) | all(sapply(TT$tt[l,ncond[2]], identical, 1))) & TT$tt$OUT[l]=="?"){
                  TT$tt[l, "OUT"] <- 0}}}
            else {if (length(ncon)>0) {
              for (l in 1:nrow(TT$tt)){
                if(all(sapply(TT$tt[l,ncon], identical, 1)) & (all(sapply(TT$tt[l,ncond[1]], identical, 1)) | all(sapply(TT$tt[l,ncond[2]], identical, 1))) & TT$tt$OUT[l]=="?"){
                  TT$tt[l, "OUT"] <- 0}}}
              else{if (length(pcon)>0) {
                for (l in 1:nrow(TT$tt)){
                  if(all(sapply(TT$tt[l,pcon], identical, 0)) & (all(sapply(TT$tt[l,ncond[1]], identical, 1)) | all(sapply(TT$tt[l,ncond[2]], identical, 1))) & TT$tt$OUT[l]=="?"){
                    TT$tt[l, "OUT"] <- 0}}}}}
          }
          else {
            if (length(pcond)==2 & length(ncond)==0){
              
              if (length(ncon)>0 & length(pcon)>0){
                for (l in 1:nrow(TT$tt)){
                  if(all(sapply(TT$tt[l,ncon], identical, 1)) & all(sapply(TT$tt[l,pcon], identical, 0)) & (all(sapply(TT$tt[l,pcond[1]], identical, 0)) | all(sapply(TT$tt[l,pcond[2]], identical, 0))) & TT$tt$OUT[l]=="?"){
                    TT$tt[l, "OUT"] <- 0}}}
              else {if (length(ncon)>0) {
                for (l in 1:nrow(TT$tt)){
                  if(all(sapply(TT$tt[l,ncon], identical, 1)) & (all(sapply(TT$tt[l,pcond[1]], identical, 0)) | all(sapply(TT$tt[l,pcond[2]], identical, 0))) & TT$tt$OUT[l]=="?"){
                    TT$tt[l, "OUT"] <- 0}}}
                else{
                  if (length(pcon)>0) {
                  for (l in 1:nrow(TT$tt)){
                    if(all(sapply(TT$tt[l,pcon], identical, 0)) & (all(sapply(TT$tt[l,pcond[1]], identical, 0)) | all(sapply(TT$tt[l,pcond[2]], identical, 0))) & TT$tt$OUT[l]=="?"){
                      TT$tt[l, "OUT"] <- 0}}}}}
            }
            else {
              if (length(pcond)>2 | length(ncond)>2) {
              stop ("The expression you entered for nec_cond is too complex! Please try with a simpler expression")}
              if (length(pcond)==0 | length(ncond)==0) { 
                if (length(ncon)>0 & length(pcon)>0){
                for (l in 1:nrow(TT$tt)){
                  if(all(sapply(TT$tt[l,ncon], identical, 1)) & all(sapply(TT$tt[l,pcon], identical, 0)) & TT$tt$OUT[l]=="?"){
                    TT$tt[l, "OUT"] <- 0}}}
                else {
                  if (length(ncon)>0) {
                    for (l in 1:nrow(TT$tt)){
                      if(all(sapply(TT$tt[l,ncon], identical, 1)) & TT$tt$OUT[l]=="?"){
                        TT$tt[l, "OUT"] <- 0}}}
                  else{
                    if (length(pcon)>0) {
                      for (l in 1:nrow(TT$tt)){
                        if(all(sapply(TT$tt[l,pcon], identical, 0)) & TT$tt$OUT[l]=="?"){
                          TT$tt[l, "OUT"] <- 0}}}}}
              }
              }
            }
          }
      }
      else {
        ncond<-c()
        pcond<-c()
        if(length(grep("\\*",nec_cond[i])) > 0) {
          tld <- gsub('\\s', '', nec_cond[i])         
          tld <- unlist(strsplit(tld, '\\*'))
          for (j in 1:length(tld)){
            if(length(grep("~",tld[j])) > 0){
              cstrd<-strsplit(tld[j],"~")
              cstrd1<-unlist(cstrd)
              ncond<-c(ncond, cstrd1[2])}
            else {pcond<-c(pcond, tld[j])}
          }
          if (length(ncond)==1 & length(pcond)==1){
              for (l in 1:nrow(TT$tt)){
                if((all(sapply(TT$tt[l,ncond], identical, 1)) | all(sapply(TT$tt[l,pcond], identical, 0))) & TT$tt$OUT[l]=="?")
                  {
                  TT$tt[l, "OUT"] <- 0}
                }
          }
          else {
            if (length(ncond)==2 & length(pcond)==0) {
                for (l in 1:nrow(TT$tt)){
                  if((all(sapply(TT$tt[l,ncond[1]], identical, 1)) | all(sapply(TT$tt[l,ncond[2]], identical, 1))) & TT$tt$OUT[l]=="?")
                  {TT$tt[l, "OUT"] <- 0}
                  }
            }
            else {
              if (length(pcond)==2 & length(ncond)==0){
                  for (l in 1:nrow(TT$tt)){
                    if((all(sapply(TT$tt[l,pcond[1]], identical, 0)) | all(sapply(TT$tt[l,pcond[2]], identical, 0))) & TT$tt$OUT[l]=="?")
                      {TT$tt[l, "OUT"] <- 0}
                    }
              }
              else {
                if (length(pcond)>2 | length(ncond)>2) {
                  stop ("The expression you entered for nec_cond is too complex! Please try with a simpler expression")}
              }
            }
          }
        }
        else {
        if(length(grep("~", nec_cond[i])) > 0){
        cstr<-strsplit(nec_cond[i],"~")
        cstr1<-unlist(cstr)
        TT$tt[(TT$tt[,cstr1[2]]==1) & (TT$tt$OUT=="?"), "OUT"] <- 0}
        else {TT$tt[(TT$tt[, nec_cond[i]]==0) & (TT$tt$OUT=="?"), "OUT"] <- 0}
        }
      }
    } }
  if (length(untenable_LR)>0){
    for (i in 1:length(untenable_LR)){
      if(length(grep("\\*",untenable_LR[i])) > 0){
        tl <- gsub('\\s', '', untenable_LR[i])         
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
      else { if(length(grep("~", untenable_LR[i])) > 0){
        cstr<-strsplit(untenable_LR[i],"~")
        cstr1<-unlist(cstr)
        TT$tt[(TT$tt[,cstr1[2]]==0) & (TT$tt$OUT=="?"), "OUT"] <- 0}
        else {TT$tt[(TT$tt[, untenable_LR[i]]==1) & (TT$tt$OUT=="?"), "OUT"] <- 0}
      }
    }  
  }
  if (length(contrad_rows)>0){TT$tt[contrad_rows, "OUT"] <- 0}
  return(TT)}
