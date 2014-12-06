#####
# Evaluator:stockEvaluatedByDMI
# Special Sorter-Preference Order: stockSortedByDMITOPSIS
# suggest n_DX = 7
# suggest n_ADX = 7
# suggest dayperiod = 14
#####
# Evaluator
stockEvaluatedByDMI<-function(stock_df){
  # calculation
  n_DX<-7
  n_ADX<-7
  # Filter stock data
  stock_df<-stockDataFilter_novol(stock_df)
  if(nrow(stock_df)<=(n_DX+n_ADX)){return(NULL)}
  stock_df_new<-cbind(stock_df,TR1=NA,PLUSDM1=NA,MINUSDM1=NA,TRn=NA,PLUSDMn=NA,MINUSDMn=NA,PLUSDIn=NA,MINUSDIn=NA,DXn=NA,ADXn=NA)
  # True Range
  stock_df_new$TR1<-pmax(abs(stock_df_new$close-stock_df_new$low),
                         c(rep(NA,1),abs(stock_df_new$high[-1]-stock_df_new$close[-nrow(stock_df_new)])),
                         c(rep(NA,1),abs(stock_df_new$low[-1]-stock_df_new$close[-nrow(stock_df_new)])))
  # Directional Movement
  stock_df_new$PLUSDM1 <- c(rep(NA,1),ifelse(stock_df_new$high[-1]-stock_df_new$high[-nrow(stock_df_new)]>0,
                                             stock_df_new$high[-1]-stock_df_new$high[-nrow(stock_df_new)],0))
  stock_df_new$MINUSDM1 <- c(rep(NA,1),ifelse(stock_df_new$low[-nrow(stock_df_new)]-stock_df_new$low[-1]>0,
                                              stock_df_new$low[-nrow(stock_df_new)]-stock_df_new$low[-1],0))
  # DX
  stock_df_new$TRn <- c(rep(NA,n_DX-1),rollapply(stock_df_new$TR1,n_DX,na.rm = TRUE,sum))
  stock_df_new$PLUSDMn <- c(rep(NA,n_DX-1),rollapply(stock_df_new$PLUSDM1,n_DX,na.rm = TRUE,sum))
  stock_df_new$MINUSDMn <- c(rep(NA,n_DX-1),rollapply(stock_df_new$MINUSDM1,n_DX,na.rm = TRUE,sum))
  stock_df_new$PLUSDIn<-stock_df_new$PLUSDMn/stock_df_new$TRn*100
  stock_df_new$MINUSDIn<-stock_df_new$MINUSDMn/stock_df_new$TRn*100
  stock_df_new$DXn<-ifelse((stock_df_new$PLUSDIn+stock_df_new$MINUSDIn)>0,
                           abs((stock_df_new$PLUSDIn-stock_df_new$MINUSDIn))/(stock_df_new$PLUSDIn+stock_df_new$MINUSDIn)*100,0)
  # ADX
  stock_df_new$ADXn <- c(rep(NA,n_ADX-1),rollapply(stock_df_new$DXn,n_ADX,na.rm = TRUE,mean))
  #
  #   ggplot() + 
  #     geom_line(data = tail(stock_df_new,20),aes(x = ymd(date),y = PLUSDIn),color = 'red') +
  #     geom_line(data = tail(stock_df_new,20),aes(x = ymd(date),y = MINUSDIn),color = 'green')+
  #     geom_line(data = tail(stock_df_new,20),aes(x = ymd(date),y = DXn),color = 'grey') + 
  #     geom_line(data = tail(stock_df_new,20),aes(x = ymd(date),y = ADXn),color = 'blue')
  #     
  # prediction
  n_predict<-10
  stock_df_new<-tail(stock_df_new,n_predict)
  #stockEvaluation
  stockEvaluation<-NULL
  stockEvaluation$symbol<-stock_df_new$symbol[1]
  #the Last Day +DI
  stockEvaluation$PLUSDI<-tail(stock_df_new$PLUSDIn,1)
  #The Last Day -DI
  stockEvaluation$MINUSDI<-tail(stock_df_new$MINUSDIn,1)
  #The Last Day DX
  stockEvaluation$DX<-tail(stock_df_new$DXn,1)
  #The Last Day ADX
  stockEvaluation$ADX<-tail(stock_df_new$ADXn,1)
  # self-defined - 斜率
  stockEvaluation$PLUSDIK <- lm(PLUSDIn~c(1:nrow(stock_df_new)),data=stock_df_new)$coefficients[2]
  stockEvaluation$MINUSDIK <- lm(MINUSDIn~c(1:nrow(stock_df_new)),data=stock_df_new)$coefficients[2]
  stockEvaluation$ADXK <- lm(ADXn~c(1:nrow(stock_df_new)),data=stock_df_new)$coefficients[2]
  #stockEvaluation$DXPDICOR <- cor(stock_df_new$DXn,stock_df_new$PLUSDIn)
  #stockEvaluation$DXMDICOR <- cor(stock_df_new$DXn,stock_df_new$MINUSDIn)
  return(stockEvaluation)
}
# Sorter
stockSortedByDMITOPSIS<-function(stock_evaluation_df){
  # TOPSIS
  decision<-as.matrix(stock_evaluation_df[,c("PLUSDI","MINUSDI","PLUSDIK","MINUSDIK","ADXK")])
  weight <- c(1, 1, 1, 1, 1)
  impacts <- c("+", "-", "+", "-", "+")
  result<-topsis(decision, weight, impacts)
  # Sort
  stock_evaluation_df<-cbind(stock_evaluation_df,rank=result$rank)
  stock_evaluation_df_sorted<-stock_evaluation_df[order(stock_evaluation_df$rank,decreasing=F),]
  return(stock_evaluation_df_sorted)
}