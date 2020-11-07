library(dplyr)
library(quantmod)
library(stats)
library(ggplot2)
library(ggpubr)
library(plotly)


#letoltendo tickerek
ticker_list <-
  list("EUFN", "HSBC", "BCS", "RBS", "RDS-A", "BP", "EZJ", "BATS.L")

#becslesi idoszak megadasa
estim_start = as.Date("2015-06-22")
estim_end = as.Date("2016-06-22")

#function adott tickeru idosorok letoltese, adj close es date kimentese, ha hianyzo van, elozo ertekkel fillelem, szamolok loghozamot
date_and_adjclose <-
  function(ticker, path = "~/DK-TNN-HF3-Empir", name, start=estim_start-1, end=estim_end) {
    data_zoo <- getSymbols(
        ticker,
        env = globalenv(),
        source = "yahoo",
        auto.assign = FALSE,
        from = start,
        to = end
      )[, 6] %>% zoo::na.locf() %>% log() %>% diff
    return(data.frame(date=index(data_zoo[2:nrow(data_zoo)]), data_zoo[2:nrow(data_zoo)]))
    
  }

#date_and_adjclose fuggveny alkalmazasa mindegyik tickeren + egy adatbazisba mergelem+hianyzok helyett elozo ertek bemasolasa
data_output <- lapply(ticker_list, date_and_adjclose)
event_study_df<- purrr::reduce(data_output, full_join, by="date")%>% arrange(date) %>%  zoo::na.locf()


#linearis modelt generalo fuggeny
lin_modell <- function(y){
  f <- as.formula(paste(y, "EUFN.Adjusted", sep="~"))
  linearMod <- lm(f, data=event_study_df)
  return(linearMod)
}
#minden tickerre futtatok linearis modellt
ticker_columnnames <- colnames(event_study_df[3:ncol(event_study_df)])
linear_Models<- lapply(ticker_columnnames, lin_modell)

#predictalt idoszakra adatbazis betoltese
data_output_test_y <- lapply(ticker_list[2:length(ticker_list)], date_and_adjclose, start="2016-06-22", end="2016-07-01")
data_output_test_x <- date_and_adjclose("EUFN", start="2016-06-22", end="2016-07-01")
EUFN.Adjusted <- tibble(EUFN.Adjusted=data_output_test_x$EUFN.Adjusted)
data_output_test_xy <- lapply(data_output_test_y, cbind, EUFN.Adjusted)

#loghozamok prediktalasa mindegyik reszvenyre
predicted_logreturns <- mapply(predict, linear_Models, data_output_test_xy) %>% as_tibble
names(predicted_logreturns)[1:length(predicted_logreturns)] <- ticker_list[2:length(ticker_list)]

#abnormalis hozamok kiszamitasa
  #teszt eseményablak valos hozamainak egy data frambe rendezese
  test_actual_return <- purrr::reduce(data_output_test_y, full_join, by="date")
  test_actual_return_only <- test_actual_return[2:length(ticker_list)]
  #valos hozamok-prediktalt hozamok
  abnormal_returns <- cbind(test_actual_return$date, test_actual_return_only-predicted_logreturns)
  
#kumulált abnormális hozamok kiszámítása és ábrázolása
cum_abnormal_returns<-lapply(abnormal_returns[,2:length(ticker_list)],cumsum)
cum_abnormal_returns <-cbind(abnormal_returns[,1],data.frame(cum_abnormal_returns))
names(cum_abnormal_returns)<-c("Date","HSBC", "BCS", "RBS", "RDSA", "BP", "EZJ", "BATS")
p1 <- ggplot(cum_abnormal_returns) + geom_line(aes(y=HSBC,x=Date))
p2 <- ggplot(cum_abnormal_returns) + geom_line(aes(y=BCS, x=Date))
p3 <- ggplot(cum_abnormal_returns) + geom_line(aes(y=RBS, x= Date))
p4 <- ggplot(cum_abnormal_returns) + geom_line(aes(y=RDSA, x=Date))
p5 <- ggplot(cum_abnormal_returns) + geom_line(aes(y=BP, x=Date))
p6 <- ggplot(cum_abnormal_returns) + geom_line(aes(y=EZJ, x= Date))
p7 <- ggplot(cum_abnormal_returns) + geom_line(aes(y=BATS, x=Date))
gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,p7,nrow=4, ncol=2, top = "Kumulált abnormális hozamok")

#statistical test
x_star<-data.frame(V1=c(rep(1,length(EUFN.Adjusted))),EUFN.Adjusted)

