source("StockEvaluationMethods.R",encoding = "UTF-8")
stockSelection <- function(conn,num,dayperiod,end = Sys.Date(),evaluatorname,sortername,parameters){
  stockSelector<-NULL
  # parameter setting
  stockSelector$num<-num
  stockSelector$end<-as.Date(end)
  stockSelector$dayperiod<-dayperiod
  stockSelector$start <- stockSelector$end - stockSelector$dayperiod
  stockSelector$stockEvaluator<-switchStockEvaluator(evaluatorname)
  stockSelector$stockSorter<-switchStockSorter(sortername)
  # stock blacklist filter
  stock_table_array<-dbListTables(conn)
  stock_table_array<-symbols
  stockSelector$blacklist<-c("yahoo_601318_ss")
  stock_table_array<-stock_table_array[which(!(stock_table_array %in% stockSelector$blacklist))]
  # stock evaluation
  stock_evaluation_df<-NULL
  print(system.time(
  for(i in 1:length(stock_table_array)){
    #stock_df<-dbReadTable(conn, stock_table_array[i])
    # within period
    #stock_df_withinperiod<-stock_df[which((as.Date(stock_df$date)>=stockSelector$start)&
    #                               (as.Date(stock_df$date)<=stockSelector$end)),]
    stock_df_withinperiod<-dbFetch(dbSendQuery(conn, 
                          paste("SELECT * FROM ", stock_table_array[i]," WHERE date BETWEEN '",stockSelector$start,"' AND '",stockSelector$end,"';",sep = "")))
    if(nrow(stock_df_withinperiod)<=1){next}
    #change symbol to stock_table name
    stock_df_withinperiod$symbol<-stock_table_array[i]
    # stock Price Adjust
    stock_df_withinperiod<-stockPriceAdjust(stock_df_withinperiod)
    # stock evaluator
    stock_evaluation_list<-stockSelector$stockEvaluator(stock_df_withinperiod,parameters)
    if(is.null(stock_evaluation_list)){next}
    stock_evaluation_df<-rbind(stock_evaluation_df,as.data.frame(stock_evaluation_list,stringsAsFactors = F))
  }
  ))
  if(is.null(stock_evaluation_df)){
    stockSelector$stock_evaluation_df_sorted<-NULL
    stockSelector$stockSelected<-NULL
    return(stockSelector)
  }
  # stock sorterstock Order By Evaluation Preference
  stockSelector$stock_evaluation_df_sorted<-stockSelector$stockSorter(stock_evaluation_df,parameters)
  # stock Top Num Selected
  stockSelector$stockSelected<-head(stockSelector$stock_evaluation_df_sorted$symbol,num)
  return(stockSelector)
}

# stock data filtering and Adjusting
stockPriceAdjust<-function(stock_df_withinperiod){
  k_adj<-stock_df_withinperiod$adjClose/stock_df_withinperiod$close
  stock_df_withinperiod$open<-stock_df_withinperiod$open*k_adj
  stock_df_withinperiod$high<-stock_df_withinperiod$high*k_adj
  stock_df_withinperiod$low<-stock_df_withinperiod$low*k_adj
  stock_df_withinperiod$close<-stock_df_withinperiod$close*k_adj
  return(stock_df_withinperiod)
}
# cmpfun
stockSelection<-cmpfun(stockSelection)