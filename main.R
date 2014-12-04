require(ggplot2)
require(RSQLite)
require(compiler)
require(topsis)
require(zoo)
require(lubridate)
source("StockSelectionProcess.R",encoding = "UTF-8")
conn <- dbConnect(SQLite(), "F:/Stocks/STOCK20141115.db3")

#dbListTables(conn)
#head(stock_df<-dbReadTable(conn, "yahoo_000333_sz"))
# 蜡烛图
Candle <- function(xxx,n){
   xxx <- tail(xxx,n)
   xxx$date <- ymd(xxx$date) 
   candle <- ggplot(xxx)+geom_errorbar(aes(x=date,ymax=high,ymin=low))+geom_rect(aes(xmin=date-43000, xmax=date+43000, ymax=ifelse(open>=close, open, close), ymin=ifelse(open>close, close, open), fill=ifelse(open>close,"gray80","black")), colour="black")+scale_fill_identity()+ylab("price")
   candle
}
# xxx <- dbReadTable(conn,"yahoo_601800_ss")
# Candle(xxx,10)
num<-10
dayperiod <- 30
end <- "2012-12-31"
evaluatorname <- "stockEvaluatedByDMI"
sortername <- "stockSortedByDMITOPSIS"
system.time(stockSelectResult <- stockSelection(conn,num,dayperiod,end,
                                    evaluatorname = evaluatorname, sortername = sortername))
StockSelectedFileName<-paste("StockSelected",evaluatorname,sortername,sep = "-")
write(stockSelectResult$stockSelected, StockSelectedFileName, sep = "\t")
dbDisconnect(conn)
