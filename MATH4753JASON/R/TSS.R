
 #' computes the Total sum of squares using a data set and a responce var
 #'
 #' @param data the data set
 #' @param response_var A Var in a data set
 #' @returns A numeric vector which represents the Total sum of squares
 #'
 #' @export
computeTS <- function(data, response_var) {

  y <- data[[response_var]]
  x <- data

  TSS <- with(x,sum((y-mean(y))^2))


  return(TSS)
}






