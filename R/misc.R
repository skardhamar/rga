# Tab completions for rga clas
.DollarNames.rga <- function(x, pattern) {
    grep(pattern, getRefClass(class(x))$methods(), value = TRUE)
} 
