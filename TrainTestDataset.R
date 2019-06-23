Train <- function (Data, Spl) {
  train = dplyr::filter(Data, Spl == TRUE)
  return(train)
}

Test <- function (Data, Spl) {
  train = dplyr::filter(Data, Spl == FALSE)
  return(train)
}