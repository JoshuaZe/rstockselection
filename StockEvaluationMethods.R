source("util.R",encoding = "UTF-8")
source("StockEvaluationMethods/DirectionalMovementIndexMethod.R",encoding = "UTF-8")
source("StockEvaluationMethods/TianXueSongMethod.R",encoding = "UTF-8")
#####
# switcher
#####
switchStockEvaluator<-function(evaluatorname){
  cmpfun(
    switch(evaluatorname,
           stockEvaluatedByDMI=stockEvaluatedByDMI,
           stockEvaluatedByTXS=stockEvaluatedByTXS
    ))
}

switchStockSorter<-function(sortername){
  cmpfun(
    switch(sortername,
           stockSortedByDMITOPSIS=stockSortedByDMITOPSIS,
           stockSortedByTXSTOPSIS=stockSortedByTXSTOPSIS
    ))
}