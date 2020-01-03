#' Mobile Check1
#'
#' Description: Takes dataframe and a column as input and check for
#' mobile number validation, following properties are currently being
#' verified:
#' 1) Length of mobile number must be 10
#' 2) It should start from any number from 6 to 9
#' 3) It should not cantain any alpha characters
#'
#' @param data dataframe to be operated on
#' @param var variable number which contains mobile numbers inside the dataframe
#' @param replace whether to delete or replace rows without mobile numbers, default: FALSE
#' @param stopNumbers any user defined numbers want to replace or remove from the data
#'
#' @return dataframe with valid mobile numbers
#' @export
#' @examples
#' mob_check()

mob_check1 <- function(data,var = "CUSTOMER_MOBILE",replace=FALSE,stopNumbers = c(9999999999,9999999990,9999999993)){
  if(var %in% colnames(data)){
    if(replace == F){
      data <- data[grepl("^[6-9]\\d{9}$",data[[var]]),]
      data <- data[!(data[[var]] %in% stopNumbers),]
      print("Sucees!! Deleted non Indian or fraud numbers")
    }
    if(replace == T){
      data[! (grepl("^[6-9]\\d{9}$",data[[var]])),var] <- NA
      data[(data[[var]] %in% stopNumbers),var] <- NA
      print("Sucess!! Replaced non Indian or fraud numbers with NA")
    }
  }else{
    warning("Column not found in data")
  }
  return(data)
}
