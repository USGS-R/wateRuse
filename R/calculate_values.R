#' caluculate_values
#'
#' Calculate values
#' 
#' @param w.use dataframe, the water use data 
#' 
#' @export
#' 
#' 
#' @examples 
#' w.use <- wUseSample
#' w.use.new <- caluculate_values(w.use)
caluculate_values <- function(w.use){ 
  
  calculation.formula <- calculation.formula
  calculation.formula$CALCULATION <- gsub("-",".",calculation.formula$CALCULATION)
  calculation.formula$EXPANDEDFORMULA <- gsub("-",".",calculation.formula$EXPANDEDFORMULA)
  
  make.index <- which(!(gsub("-",".",calculation.formula$CALCULATION) %in% names(wUseSample)))
  
  columns.to.make <- calculation.formula$CALCULATION[make.index]
  formulas.to.use <- calculation.formula$EXPANDEDFORMULA[make.index]
  
  g1 = function(df, s){
    q = quote(mutate(df, z = s))
    eval(parse(text=sub("z = s", s, deparse(q))))
  }
  
  wUseNew <- w.use
  
  for(i in 1:length(columns.to.make)){
    formula.here <- formulas.to.use[i]
    wUseNew <- tryCatch({
        g1(wUseNew, paste(columns.to.make[i], "=", formula.here))
      },
      error = function(e){
        wUseNew
      })
    
  }
  
  return(wUseNew)
  
}