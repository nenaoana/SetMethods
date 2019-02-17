theory.intersections <- function(theory, empirics, sol = 1, use.tilde = TRUE)
{
  if (is.null(empirics$i.sol)){
    if (is.character(sol)) stop('For conservative or parsimonious solutions, the model must be specificied numerically (e.g. sol=2).')
    s <- empirics$solution[[sol]]}
  else{
    if (is.numeric(sol)){
      s <- empirics$i.sol$C1P1$solution[[sol]]}
    else {
      if (is.character(sol)){
        if (!nchar(sol)==6) stop('The model is specified in the wrong format. Please check the helpfile for pimdata using ?pimdata for the appropiate format.')
        sol <- toupper(sol)  
        int <- as.numeric(unlist(strsplit(sol, "I"))[2])
        mod <- toupper(unlist(strsplit(sol, "I"))[1])
        if (int > length(get(mod, pos = empirics$i.sol)$solution))  stop('The intermediate solution given by the model does not exist. Check model again!')
        s <- get(mod, pos = empirics$i.sol)$solution[[int]]
      }
      else {return("The model given to argument sol= is invalid or in the wrong format. Please check the helpfile for pimdata using ?pimdata for the appropiate format.")}
    }
  }

tild <- function(x)
{
  x <- unlist(strsplit(x, '\\*'))
  x <- as.vector(unlist(sapply(x, function (y) 
       if (!y==toupper(y)){y <- paste("~",toupper(y),sep="")} 
       else { y <- y})))
  x <- paste(x, collapse = "*")
}

if (!use.tilde){  
emp <- as.vector(unlist(sapply(s, function(x)  tild(x))))
emp <- paste(emp, collapse = "+")
th <- unlist(strsplit(theory, '\\+'))
th <- as.vector(unlist(sapply(th, function(x)  tild(x))))
theory <- paste(th, collapse = "+")}
else {
  emp <- toupper(s)
  theory <- toupper(theory)}

thintersect <- list()

thintersect$TE <- intersectExp(theory,emp)
thintersect$tE <- intersectExp(sop(negateExp(theory)),emp)
thintersect$Te <- intersectExp(theory,sop(negateExp(emp)))
thintersect$te <- intersectExp(sop(negateExp(theory)),sop(negateExp(emp)))

class(thintersect) <- 'thintersect'
return(thintersect)
}