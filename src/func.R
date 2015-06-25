# Fichier contenant des fonctions de base diverses

#Definition de l'operateur ternaire ?:
`?` <- function(x, y) {
  xs <- as.list(substitute(x))
  if (xs[[1]] == as.name("<-")) x <- eval(xs[[3]])
  r <- eval(sapply(strsplit(deparse(substitute(y)), ":"), function(e) parse(text = e))[[2 - as.logical(x)]])
  if (xs[[1]] == as.name("<-")) {
    xs[[3]] <- r
        eval.parent(as.call(xs))
  } else {
    r
  }
}       
