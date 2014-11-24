source("StockEvaluationMethods.R",encoding = "UTF-8")
stockSelection <- function(conn,num,dayperiod,end = Sys.Date(),evaluatorname,sortername){
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
  stockSelector$blacklist<-NULL
  stock_table_array<-stock_table_array[which(!(stock_table_array %in% stockSelector$blacklist))]
  # stock evaluation
  stock_evaluation_df<-NULL
  for(i in 1:length(stock_table_array)){
    stock_df<-dbReadTable(conn, stock_table_array[i])
    # within period
    stock_df_withinperiod<-stock_df[which((as.Date(stock_df$date)>=stockSelector$start)&
                                   (as.Date(stock_df$date)<=stockSelector$end)),]
    # Filter stock data
    stock_df_withinperiod<-stockDataFilter(stock_df_withinperiod)
    if(nrow(stock_df_withinperiod)<=1){next}
    # stock evaluator
    stock_evaluation_list<-stockSelector$stockEvaluator(stock_df_withinperiod)
    stock_evaluation_df<-rbind(stock_evaluation_df,as.data.frame(stock_evaluation_list,stringsAsFactors = F))
  }
  stock_evaluation_df<-stock_evaluation_df
  # stock sorterstock Order By Evaluation Preference
  stockSelector$stock_evaluation_df_sorted<-stockSelector$stockSorter(stock_evaluation_df)
  # stock Top Num Selected
  stockSelector$stockSelected<-head(stockSelector$stock_evaluation_df_sorted$symbol,num)
  return(stockSelector)
}
# stock data filtering
stockDataFilter<-function(stock_df_withinperiod){
  # filtering data before no vol
  if(length(which(stock_df_withinperiod$vol==0))!=0){
    stock_df_withinperiod<-stock_df_withinperiod[max(which(stock_df_withinperiod$vol==0)):nrow(stock_df_withinperiod),]
  }
  return(stock_df_withinperiod)
}
# cmpfun
stockSelection<-cmpfun(stockSelection)