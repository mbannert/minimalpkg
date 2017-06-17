#' Compute Scores
#' 
#' This and that
#'
#'@param dframe data.frame input, typically a psychological dataset.
#'@param maplist list maps... 
#'@return returns the initial data.frame and adds the result columns.
#'@examples
#' data("big5")
#' big5_n <- selectNumeric(big5)
#' out <- computeScores(big5_n,
#' list(A_sum = "^A",
#' E_sum = "^E",
#' N_sum = "^N",
#' O_sum = "^O",
#' C_sum = "^C"))
#' 
#' out <- computeScores(out,
#' list(A_mean = "^A[0-9]",
#' E_mean = "^E[0-9]",
#' N_mean = "^N[0-9]",
#' O_mean = "^O[0-9]",
#' C_mean = "^C[0-9]"),
#' fct = mean)
#' out
#' @export
computeScores <- function(dframe,
                          maplist,
                          fct = sum,
                          na.rm = T){
  li <- lapply(maplist,function(x){
    out <- dframe[,grep(x,names(dframe),value=T)]
    apply(out,1,fct, na.rm = na.rm)
  })
  names(li) <- names(maplist)
  cbind(dframe,as.data.frame(li))
}





