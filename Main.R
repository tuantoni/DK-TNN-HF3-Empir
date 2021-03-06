install.packages("dplyr")
install.packages("quantmod")
install.packages("stats")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("plotly")
install.packages("data.table")
install.packages("matlib")


library(dplyr)
library(quantmod)
library(stats)
library(ggplot2)
library(ggpubr)
library(plotly)
library(data.table)
library(matlib)

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
event_study_df['Mean']<-apply(event_study_df[,3:ncol(event_study_df)],1,mean)

#linearis modelt generalo fuggeny
lin_modell <- function(y){
  f <- as.formula(paste(y, "EUFN.Adjusted", sep="~"))
  linearMod <- lm(f, data=event_study_df)
  return(linearMod)
}
#minden tickerre futtatok linearis modellt
ticker_columnnames <- colnames(event_study_df[3:ncol(event_study_df)])
linear_Models<- lapply(ticker_columnnames, lin_modell)
linear_ModelswoMean<-linear_Models
linear_ModelswoMean[[8]]<-NULL

#predictalt idoszakra adatbazis betoltese
data_output_test_y <- lapply(ticker_list[2:length(ticker_list)], date_and_adjclose, start="2016-06-22", end="2016-07-01")
data_output_test_x <- date_and_adjclose("EUFN", start="2016-06-22", end="2016-07-01")
EUFN.Adjusted <- tibble(EUFN.Adjusted=data_output_test_x$EUFN.Adjusted)
data_output_test_xy <- lapply(data_output_test_y, cbind, EUFN.Adjusted)

#loghozamok prediktalasa mindegyik reszvenyre
predicted_logreturns <- mapply(predict, linear_ModelswoMean, data_output_test_xy) %>% as_tibble
names(predicted_logreturns)[1:length(predicted_logreturns)] <- ticker_list[2:length(ticker_list)]

#abnormalis hozamok kiszamitasa és ábrázolása

  #teszt eseményablak valos hozamainak egy data frambe rendezese
  test_actual_return <- purrr::reduce(data_output_test_y, full_join, by="date")
  test_actual_return_only <- test_actual_return[2:length(ticker_list)]
  names(test_actual_return_only)<-c("HSBC", "BCS", "RBS", "RDSA", "BP", "EZJ", "BATS")
  
  #valos hozamok-prediktalt hozamok és azok átlaga
  abnormal_returns <- cbind(test_actual_return$date, test_actual_return_only-predicted_logreturns)
  names(abnormal_returns)<-c("Date","HSBC", "BCS", "RBS", "RDSA", "BP", "EZJ", "BATS")
  abnormal_returns['Mean']<-apply(abnormal_returns[,2:ncol(abnormal_returns)],1,mean)
  
  a1 <- ggplot(data=abnormal_returns, aes(x=index(abnormal_returns), y=HSBC)) +
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=scales::percent(HSBC)), color="black", size=3)+
    theme_minimal()+labs(x=NULL)+scale_y_continuous(labels = scales::percent)
  a2 <- ggplot(data=abnormal_returns, aes(x=index(abnormal_returns), y=BCS)) +
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=scales::percent(BCS)), color="black", size=3)+
    theme_minimal()+labs(x=NULL)+scale_y_continuous(labels = scales::percent)
  a3 <- ggplot(data=abnormal_returns, aes(x=index(abnormal_returns), y=RBS)) +
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=scales::percent(RBS)), color="black", size=3)+
    theme_minimal()+labs(x=NULL)+scale_y_continuous(labels = scales::percent)
  a4 <- ggplot(data=abnormal_returns, aes(x=index(abnormal_returns), y=RDSA)) +
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=scales::percent(RDSA)),color="black", size=3)+
    theme_minimal()+labs(x=NULL)+scale_y_continuous(labels = scales::percent)
  a5 <- ggplot(data=abnormal_returns, aes(x=index(abnormal_returns), y=BP)) +
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=scales::percent(BP)), color="black", size=3)+
    theme_minimal()+labs(x=NULL)+scale_y_continuous(labels = scales::percent)
  a6 <- ggplot(data=abnormal_returns, aes(x=index(abnormal_returns), y=EZJ)) +
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=scales::percent(EZJ)), color="black", size=3)+
    theme_minimal()+labs(x=NULL)+scale_y_continuous(labels = scales::percent)
  a7 <- ggplot(data=abnormal_returns, aes(x=index(abnormal_returns), y=BATS)) +
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=scales::percent(BATS)), color="black", size=3)+
    theme_minimal()+labs(x=NULL)+scale_y_continuous(labels = scales::percent)
  a8 <- ggplot(data=abnormal_returns, aes(x=index(abnormal_returns), y=Mean)) +
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=scales::percent(Mean)), color="black", size=3)+
    theme_minimal()+labs(x=NULL)+scale_y_continuous(labels = scales::percent)
  gridExtra::grid.arrange(a1,a2,a3,a4,a5,a6,a7,a8,nrow=4, ncol=2, top = "Abnormális hozamok")
  
#kumulált abnormális hozamok kiszámítása és ábrázolása
cum_abnormal_returns<-lapply(abnormal_returns[,2:ncol(abnormal_returns)],cumsum)
cum_abnormal_returns <-cbind(abnormal_returns[,1],data.frame(cum_abnormal_returns))
names(cum_abnormal_returns)<-c("Date","HSBC", "BCS", "RBS", "RDSA", "BP", "EZJ", "BATS", "Mean")
p1 <- ggplot(cum_abnormal_returns) + geom_line(aes(y=HSBC,x=Date))+scale_y_continuous(labels = scales::percent)
p2 <- ggplot(cum_abnormal_returns) + geom_line(aes(y=BCS, x=Date))+scale_y_continuous(labels = scales::percent)
p3 <- ggplot(cum_abnormal_returns) + geom_line(aes(y=RBS, x= Date))+scale_y_continuous(labels = scales::percent)
p4 <- ggplot(cum_abnormal_returns) + geom_line(aes(y=RDSA, x=Date))+scale_y_continuous(labels = scales::percent)
p5 <- ggplot(cum_abnormal_returns) + geom_line(aes(y=BP, x=Date))+scale_y_continuous(labels = scales::percent)
p6 <- ggplot(cum_abnormal_returns) + geom_line(aes(y=EZJ, x= Date))+scale_y_continuous(labels = scales::percent)
p7 <- ggplot(cum_abnormal_returns) + geom_line(aes(y=BATS, x=Date))+scale_y_continuous(labels = scales::percent)
p8 <- ggplot(cum_abnormal_returns) + geom_line(aes(y=Mean, x=Date))+scale_y_continuous(labels = scales::percent)

gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,nrow=4, ncol=2, top = "Kumulált abnormális hozamok")

#statistical test
x_star<-as.matrix(data.frame(V1=c(rep(1,length(EUFN.Adjusted))),EUFN.Adjusted))

eps_hat<-function(ticker){
  event_study_df[,ticker+2]-linear_Models[[ticker]]$coefficients[1]-
    event_study_df$EUFN.Adjusted*linear_Models[[ticker]]$coefficients[2]
}

est_eps<-lapply(1:8,eps_hat)

#függvény a Var(CAR) kiszámítására
test<-function(n){
epshat<-matrix(unlist(est_eps[n]), ncol=1)
var_eps_hat<-drop((t(epshat)%*%epshat)/(length(epshat)-2))
munit<-matrix(0,nrow(x_star),nrow(x_star))
diag(munit)<-1
X<-as.matrix(data.frame(V1=c(rep(1,length(event_study_df[,2]))),event_study_df$EUFN.Adjusted))
trimat<-matrix(1,ncol=nrow(x_star),nrow=nrow(x_star))
trimat[lower.tri(trimat)] <- 0
V<-munit*var_eps_hat+((x_star%*%solve(t(X)%*%X))%*%t(x_star))*var_eps_hat
Var_Car<-vector()
for(i in 1:nrow(x_star)){
 Var_Car[i]<-(t(trimat[,i])%*%V)%*%trimat[,i]
}
return(Var_Car)
}

#függvény a p-érték számításhoz
hip<-function(n){
tick_hip<-data.frame(Date=cum_abnormal_returns[,1],CAR=cum_abnormal_returns[,n],Var_Car=test(n-1), SCAR=cum_abnormal_returns[,n]/sqrt(test(n-1)))
tick_hip<-cbind(tick_hip,p_value=1-pt(q=abs(tick_hip$SCAR),df=length(event_study_df[,n+1])))
}

#hipotézisvizsgálat a vállalatokra külön-külön, illetve az átlagra is
hsbc<-hip(2)
bcs<-hip(3)
rbs<-hip(4)
rdsa<-hip(5)
bp<-hip(6)
ezj<-hip(7)
bats<-hip(8)
mean<-hip(9)


#A következőek mind az interpretációhoz kell!

#együtthatók
alfa<-vector()
for (i in 1:8){
  alfa[i]<-linear_Models[[i]]$coefficients[1]
}

beta<-vector()
for (i in 1:8){
  beta[i]<-linear_Models[[i]]$coefficients[2]
}

omega<-rbind(alfa,beta)
colnames(omega)<-c("HSBC", "BCS", "RBS", "RDSA", "BP", "EZJ", "BATS", "Mean")

#külön-külön ábrák cégekre
names(test_actual_return)<-c("Date","HSBC", "BCS", "RBS", "RDSA", "BP", "EZJ", "BATS")

ggfun<-function(x.var, y.var,t,c="black",s=3){
  x.var<-enquo(x.var)
  y.var<-enquo(y.var)
  ggp<-ggplot() + geom_line(data=abnormal_returns,aes(y= !! y.var,x= !! x.var, color="Abnormal Returns"))+
    geom_line(data=test_actual_return,aes(y=!! y.var,x= !! x.var,color="Actual Returns"))+
    geom_bar(data=cum_abnormal_returns,aes(y= !! y.var,x= !! x.var, color="Cumulative Abnormal Returns"),stat="identity", alpha=0.4)+
    geom_text(data=abnormal_returns,aes(y= !! y.var,x= !! x.var,label=scales::percent(!! y.var)), color=c, size=s)+
    geom_text(data=test_actual_return,aes(y= !! y.var,x= !! x.var,label=scales::percent( !! y.var)), color=c, size=s)+
    scale_y_continuous(labels = scales::percent)+
    labs(x=NULL, y=NULL, title=paste(t,"returns during the 2016.06.23-2016.06.30"))+
    theme(legend.position="bottom", legend.title=element_blank())
  return(ggp)
  }
ggfun(x.var=Date,y.var=HSBC, t="HSBC")
ggfun(x.var=Date,y.var=BCS, t="BCS")
ggfun(x.var=Date,y.var=RBS, t="RBS")
ggfun(x.var=Date,y.var=RDSA, t="RDS-A")
ggfun(x.var=Date,y.var=BP, t="BP")
ggfun(x.var=Date,y.var=EZJ, t="EZJ")
ggfun(x.var=Date,y.var=BATS, t="BATS")


