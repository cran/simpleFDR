#' Simple False Discovery Rate (FDR) Calculation
#' 
#' Using the Benjamini & Hochberg adjustment method, determine which variables are significant under repeated testing with a given dataframe of p values and an user defined "q" threshold.
#' @param df dataframe with variable names in column 1, and p values in column 2.  For dataframes with more than these 2 columns, the additional columns will be ignored.  Example: df<- data.frame("variable"= c("a","b","c","d","e","f","g","h","i","j","k"),"p_value" = c(0.04,0.03,0.04,0.02,0.03,0.02,0.02,0.01,0.04,0.1,0.02))
#' @param q user defined FDR threshold.  Defaults to 0.05.
#' @param sig_only logical value indicating whether to return just the variables that are significant, or all input variables.  If TRUE, only significant variables are returned.  If FALSE, all variables are returned with the significant variables at the top.  Defaults to TRUE.
#' @return Returns the original dataframe with a significance column where an asterisk denotes a significant p value after FDR calculation, and NA denotes all other p values.
#' @author Stephen C Wisser
#' @references Lee, S., & Lee, D. K. (2018). What is the proper way to apply the multiple comparison test?. Korean journal of anesthesiology, 71(5), 353.
#' @references Benjamini, Y., & Hochberg, Y. (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. Journal of the Royal statistical society: series B (Methodological), 57(1), 289-300.
#' @examples
#' df <- data.frame("variable"= c("a","b","c","d","e"),"p_value" = c(0.04,0.03,0.04,0.02,0.03))
#' 
#' # defaults to q = 0.05 and shows only significant p values
#' FDR_values <- simFDR(df)
#' 
#' # q = 0.1 and shows only significant p values
#' FDR_values <- simFDR(df, q = 0.1)
#' 
#' # q = 0.05 and shows all p values, with significant p values at the top
#' FDR_values <- simFDR(df, sig_only = FALSE)
#' @export
simFDR <- function(df,q = 0.05,sig_only = TRUE) {
  
  x <- df[,1:2]
  colnames(x)[1:2] <- c("name","pvalue")
  x <- dplyr::arrange(x,pvalue)
  x <- dplyr::mutate(x,"rank" = 1:nrow(x))
  x <- dplyr::mutate(x, "critical_value" = (x$rank/nrow(x))*q)
  x <- dplyr::mutate(x, "significant" = ifelse(x$pvalue<x$critical_value,"*",NA))
  x <- dplyr::transmute(x,name,pvalue,significant)
  x <- tidyr::fill(x,significant,.direction = "up")
  
  if(sig_only == FALSE){
    return(x)
  }
  
  if(sig_only == TRUE) {
    x <- dplyr::filter(x,!is.na(significant))
  }
}