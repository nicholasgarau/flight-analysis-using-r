#+++ REPORT ANALISI DEI BIG DATA - Nicholas Garau - matr. 11/82/00200 +++#

load("/Users/nicholasgarau/Library/Mobile Documents/com~apple~CloudDocs/📊💻DSBAI/Analisi dei Big Data - Conversano /Report x Conversano /Dati_Garau.RData")
ds1 <- dati #creo una copia del Dataset 
library(tidyverse)
View(ds1)
summary(ds1) 

sum(is.na(ds1)) # I dati si presentano già puliti, non sembrano esserci problemi di data cleaning...
str(ds1) #...lo stesso vale per la struttura dei dati che si presenta pronta all' analisi esplorativa. 

#########################################++   ANALISI ESPLORATIVA   ++##################################################

ds1 %>% ggplot(aes(Unit_Ticket)) + geom_boxplot(varwidth = TRUE, orientation = 'y')
#grossomodo notiamo una distribuzione simmetrica nei prezzi 

#############Grafici univariati discreti##################

install.packages('cowplot')
library(cowplot)

tab1 <- ds1 %>% count(Seat) %>% mutate(proportion=n/sum(n))
Seat_bar <- ggplot(data = tab1, aes(Seat,proportion)) + geom_bar(stat = 'identity', color = 'red') 
tab1

tab2 <- ds1 %>% count(PriorBoard) %>% mutate(proportion=n/sum(n))
PB_bar <- ggplot(data = tab2, aes(PriorBoard,proportion)) + geom_bar(stat = 'identity', color = 'blue')
tab2

tab3 <- ds1 %>% count(Luggage) %>% mutate(proportion=n/sum(n))
Luggage_bar <- ggplot(data = tab3, aes(Luggage,proportion)) + geom_bar(stat = 'identity', color = 'yellow')
tab3

tab4 <- ds1 %>% count(Return) %>% mutate(proportion=n/sum(n))
Ret_bar <- ggplot(data = tab4, aes(Return,proportion)) + geom_bar(stat = 'identity', color = 'orange')
tab4

plot_grid(Seat_bar,PB_bar,Luggage_bar,Ret_bar, labels = 'AUTO')

tab5 <- ds1 %>% count(Departure) %>% mutate(proportion=n/sum(n))
Dep_bar <- ggplot(data = tab5, aes(Departure,proportion)) + geom_bar(stat = 'identity', color = 'green')
tab5

tab6 <- ds1 %>% count(Arrival) %>% mutate(proportion=n/sum(n))
Arr_bar <- ggplot(data = tab6, aes(Arrival,proportion)) + geom_bar(stat = 'identity', color = 'violet')
tab6

plot_grid(Dep_bar,Arr_bar, labels = 'AUTO')
 

#Un aspetto molto interessante da analizzare potrebbe essere quello di vedere quali sono le rotte più frequentate:
tab7 <- ds1 %>% unite(Route, Departure, Arrival) %>% count(Route) %>% mutate(proportion=n/sum(n))
tab7
install.packages('scales')
library(scales)

tab7 %>% ggplot(aes(Route, n, fill = Route)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = scales::percent(proportion)), vjust= -0.25, size=2) +
  ggtitle("Airport routes in proportion") + 
  ylab('Number of prenotations') +
  theme(axis.text.x = element_text(size = 6, angle = 65), legend.position = 'top')


tab8 <- ds1 %>% count(ModPag) %>% mutate(proportion=n/sum(n))
tab8
tab8 %>% ggplot(aes(ModPag,proportion, fill = ModPag)) + geom_bar(stat = 'identity')

###########

Nbook_bp <- ds1 %>% ggplot(aes(NBookMonth)) + 
  geom_boxplot(orientation = 'y') + 
  ggtitle('Monthly bookings')
NAccWebW_bp <- ds1 %>% ggplot(aes(NAccWebWeek)) + 
  geom_boxplot(orientation = 'y') + 
  ggtitle('Number of access on web site x week')
NCompl_bp <- ds1 %>% ggplot(aes(NComplaintsYear)) + 
  geom_boxplot(orientation = 'y') + 
  ggtitle('Number of complaints x year')
NRefund_bp <- ds1 %>% ggplot(aes(NRefundYear)) + 
  geom_boxplot(orientation = 'y') +
  ggtitle('Number of refunds x year')
NCancel_bp <- ds1 %>% ggplot(aes(NCancelYear)) + 
  geom_boxplot(orientation = 'y') +
  ggtitle('Number of cancelled annual bookings')

plot_grid(Nbook_bp,NAccWebW_bp,NCompl_bp,NRefund_bp,NCancel_bp, labels = 'AUTO')

###########
NRefund_bp
ds1 %>% filter(NRefundYear > 1) %>% 
  count(NRefundYear) %>%
  mutate(proportion = n/sum(n))

#Notiamo come esista un gruppo di osservazioni che effettua più di 20 richieste di rimborso all' anno:

tab9 <- ds1 %>% filter(NRefundYear > 19) %>% 
  count(NRefundYear) %>%
  mutate(proportion = n/sum(n)) 
sum(tab9$n) #Notiamo che su 20001 osservazioni, 3737 hanno chiesto almeno 20 rimborsi all' anno, pari al..
3737/20001 #...18.6% circa del totale, a segnalare potenzialmente un particolare cluster di clienti con il 'rimborso facile' 

#+++ Andiamo ora a realizzare qualche serie storica per poi passare ai grafici bivariati
#e ad analizzare la correlazione tra variabili: +++#

install.packages('lubridate')
library(lubridate)

lbls <- paste0(month.abb[month(ds1$Data)], " ", lubridate::year(ds1$Data))
brks <- ds1$Data

ggplot(data=ds1, aes(x=Data)) + 
  geom_line(aes(y=Unit_Ticket)) + 
  labs(title="Monthly Price Trend", 
       y="Unit ticket price") +  
  theme(axis.text.x = element_text(angle = 60, vjust=0.5),  
        panel.grid.minor = element_blank()) 
 
######

ggplot(data=ds1, aes(x=Data)) + 
  geom_line(aes(y=Fidelity)) + 
  labs(title='Fidelity index in 10 years', 
       y="Fidelity Index") +  
  theme(axis.text.x = element_text(angle = 60, vjust=0.5),  
        panel.grid.minor = element_blank()) 

#######

ggplot(data=ds1, aes(x=Data)) + 
  geom_line(aes(y=Taxes)) + 
  labs(title='Taxes x passenger in 10 years', 
       y="Taxes(per capta)") +  
  theme(axis.text.x = element_text(angle = 60, vjust=0.5),  
        panel.grid.minor = element_blank()) 

####
ds1 %>% ggplot(aes(Taxes)) + geom_boxplot()
#iandamento simmetrico...
summary(ds1$Taxes)#... con media 23.18 e  mediana a 23.21
#################################################################################################

#Grafici Bivariati:
pairs(ds1[,c(2,7:11,15:19)])#computer intensive, analizzo la correlazione con corrplot
library(corrplot)
par(mfrow=c(1,1))
corrplot( cor(ds1[,-c(1,3:6,12:14)]), diag = F, method = "pie", type = "upper" )
corrplot( cor(ds1[,-c(1,3:6,12:14)]), diag = F, method = "number", type = "upper" )

##########

#Grafici Bivariati 

ds1 %>% ggplot(aes(Fidelity,Unit_Ticket)) + 
  geom_point(aes(col=Departure)) +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth(method="loess", se=F) +
  labs(subtitle="for different departure airports", 
       y="Price", 
       x="Fidelity", 
       title="Fidelity vs UnitTicket Price")

####################

ds1 %>% ggplot(aes(Fidelity,Unit_Ticket)) + 
  geom_point(aes(col=Arrival)) +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth(method="loess", se=F) +
  labs(subtitle="for different arrival airports", 
       y="Price", 
       x="Fidelity", 
       title="Fidelity vs UnitTicket Price")

###################

ds1 %>% ggplot(aes(Arrival, fill=ModPag)) + geom_bar() 

##########

#Ora analizzeremo l' andamento dei prezzi in funzione dei servizi extra presenti in prenotazione: 

ds1 %>% ggplot(aes(Fidelity,Unit_Ticket)) + 
  geom_point(aes(col=Seat)) +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth(method="loess", se=F) +
  labs(subtitle="with seat booking or not", 
       y="Price", 
       x="Fidelity", 
       title="Fidelity vs UnitTicket Price")

########

ds1 %>% ggplot(aes(Fidelity,Unit_Ticket)) + 
  geom_point(aes(col=PriorBoard)) +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth(method="loess", se=F) +
  labs(subtitle="with PriorBoard or not", 
       y="Price", 
       x="Fidelity", 
       title="Fidelity vs UnitTicket Price")

#stesso discorso per l' imbarco prioritario.



ds1 %>% ggplot(aes(Fidelity,Unit_Ticket)) + 
  geom_point(aes(col=Luggage)) +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth(method="loess", se=F) +
  labs(subtitle="with Luggage or not", 
       y="Price", 
       x="Fidelity", 
       title="Fidelity vs UnitTicket Price")

##########

ds1 %>% filter(Luggage == 'Yes') %>% 
  unite(Route,Departure,Arrival) %>% ggplot(aes(Route)) +
  geom_bar(stat = 'identity') 
  
  

tab10 <- ds1 %>% filter(Luggage == 'Yes') %>% 
  unite(Route, Departure, Arrival) %>% count(Route) %>% 
  mutate(proportion=n/sum(n))
tab10

tab10 %>% ggplot(aes(Route, n, fill = Route)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = scales::percent(proportion)), vjust= -0.25, size=2) +
  ggtitle("Luggage extra x route") + 
  ylab('Luggage Extra = yes') +
  theme(axis.text.x = element_text(size = 6, angle = 65), legend.position = 'top')

. 
# tassi areoportuali e modpag negli anni

ds1 %>% filter(year(Data) %in% 2010) %>%  
  ggplot(aes(Unit_Ticket,Taxes, col = ModPag))+
  geom_point()+
  facet_grid(.~year(Data))

ds1 %>% filter(year(Data) %in% 2015) %>%  
  ggplot(aes(Unit_Ticket,Taxes, col = ModPag))+
  geom_point()+
  facet_grid(.~year(Data))

ds1 %>% filter(year(Data) %in% 2019) %>%  
  ggplot(aes(Unit_Ticket,Taxes, col = ModPag))+
  geom_point()+
  facet_grid(.~year(Data))

############


#### la rotta con tasse maggiori??

tab11 <- ds1 %>%
  unite(Route, Departure, Arrival) %>% 
  select(Route,Taxes) 
tab11
head(tab11)

tab11 %>%
  group_by(Route) %>%
  summarize(average_taxes=mean(Taxes)) %>% 
  arrange(desc(average_taxes))
  
#c###########

cor(ds1$Unit_Ticket,ds1$Taxes)
#potremmo immaginare che questo aspetto possa essere collegato al prezzo unitario per biglietto, 
#ma come vediamo ha solo una correlazione positiva di 0.038.

# -- (confronta prezzi e numero di accessi)

ds1 %>% ggplot(aes(NAccWebWeek,Unit_Ticket)) + geom_point()
ds1 %>% ggplot(aes(Unit_Ticket,NAccWebWeek)) + geom_point()

###############################+++++++++ -- MODELLI -- +++++++++################################################

lm_full <- lm(Unit_Ticket~.,data = ds2)
summary(lm_full)
#dal modello di regresisone completo notiamo che le variabili significative sono:
#Seat
#PriorBoard
#Luggage
#Fidelity 
#Departure 
#con però un R quadro sotto lo 0.5 (45%)

lm.fit <- lm(Unit_Ticket~Seat+PriorBoard+Luggage+Fidelity+Departure, data = ds2)
summary(lm.fit)
preds <-  predict(lm.fit, ds2)
mean( (preds - ds2$Unit_Ticket)^2 )  #  Mean Square Error n[1]   227.54


#Semplificando il modello vengono confermate le variabili significative viste in precedenza con praticamente un R2 invariato
lm.fit2 <- lm(Unit_Ticket~Seat+PriorBoard+Luggage+Fidelity, data = ds2)
summary(lm.fit2)
#Semplifico il modello togliendo Departure e R2 scende a 44.5% con un RSE di 15.18

preds2 <-  predict(lm.fit2, ds2)
mean( (preds2 - ds2$Unit_Ticket)^2 ) #  Mean Square Error n[2]   230.44 

#Mean square error che sale leggermente, fitto entrambe i modelli dividendo in training e test set: 

set.seed(123)
train=sample(nrow(ds2), nrow(ds2)*0.66)

###  VALIDATION SET APPROACH

lm.fit_vs1 <- lm(Unit_Ticket~Seat+PriorBoard+Luggage+Fidelity+Departure, data = ds2, subset = train)
summary(lm.fit_vs1)
mean((Unit_Ticket-predict(lm.fit_vs1,ds2))[-train]^2) # [227.34] 
sqrt(227.34) #--> #RMSE 15.07
lm.fit_vs2 <- lm(Unit_Ticket~Seat+PriorBoard+Luggage+Fidelity, data = ds2, subset = train)
mean((Unit_Ticket-predict(lm.fit_vs2,ds2))[-train]^2) # [230.171] 
sqrt(230.171) #--> #RMSE 15.17

#risultati simili a quelli testati sul training set per il validation set approach

# K-FOLD CV 

set.seed(123)
ds=ds2

k=10
#2) assegnare un fold casuale a tutte le osservazioni e  creiamo un vettore per salvare gli MSE e calcore quest'ultimo

fold=sample(1:k,nrow(ds),replace = T) 
mse_folds=rep(NA,k)
for(i in 1:k){
  test=fold==i
  training=fold!=i
  modello=lm(Unit_Ticket~Seat+PriorBoard+Luggage+Fidelity+Departure,data=ds,subset=training)
  pred.modello =predict(modello ,newdata=ds[test,])
  
  mse_folds[i]=mean((pred.modello- ds$Unit_Ticket[test])^2)
}

mse.modello=mean(mse_folds)
mse.modello                     # mse 227.87 con Departure nel modello 
sqrt(227.87)   #RMSE --> 15.09

for(i in 1:k){
  test=fold==i
  training=fold!=i
  modello=lm(Unit_Ticket~Seat+PriorBoard+Luggage+Fidelity,data=ds,subset=training)
  pred.modello =predict(modello ,newdata=ds[test,])
  
  mse_folds[i]=mean((pred.modello- ds$Unit_Ticket[test])^2)
}

mse.modello=mean(mse_folds)
mse.modello                #[230,60] --> mse che sale leggermente ma in linea con i risultati precedenti in termini di performance
sqrt(230.60) # RMSE --> 15.18552

#### BAGGING E RANDOM FOREST 

library(randomForest)
set.seed(123)
ds2_train = sample_frac(ds2, .66)
ds2_test = setdiff(ds2, ds2_train) #altro modo per suddividere in training set e test set 


## 1) Bagging
bag_ds2=randomForest(Unit_Ticket~Seat+PriorBoard+Luggage+Fidelity+Departure, data=ds2_train, mtry=5, importance=TRUE)


bag_ds2
#scarsa performance del bagging che spiega solo 32% della variabilità 

importance(bag_ds2)
varImpPlot(bag_ds2) 
#nella variable importance vediamo in ordine: Fidelity - Luggage - PriorBoard - Seat e Departure 

#provo le predict 

bagged_estimate=predict(bag_ds2,newdata=ds2_test)
ds2_test$Unit_Ticket[1]
bagged_estimate[1]
mean((bagged_estimate-ds2_test$Unit_Ticket)^2) 
#calcoliamo il MSE --> 279.67, continuano a performare meglio i modelli lineari
sqrt(279.67) #RMSE 16.72

####   2) RANDOM FOREST
#con random forest vado a selezionare casualmente gli split e non partendo sempre 
#dal più significativo come fa il bagging.

set.seed(123)
rf_ds2=randomForest(Unit_Ticket~Seat+PriorBoard+Luggage+Fidelity+Departure, data=ds2_train, importance=TRUE)
rf_ds2
#come possiamo vedere con il RandomForest andiamo a spiegare una percentuale maggiore di variabilità (35.6% rispetto al 32% di Bagging) perchè va a prendere
#quella percentuale di info che il bagging ignorava andando a splittare sempre sul più significativo


importance(rf_ds2)
varImpPlot(rf_ds2) 
random_forest_estimate=predict(rf_ds2, newdata=ds2_test)
mean((random_forest_estimate-ds2_test$Unit_Ticket)^2)  #calcoliamo il MSE --> 263.5225 
sqrt(263.5225) #16.23338 RMSE 

#######################################################################################
## APPROCCIO PER SEGMENTARE LA CLIENTELA 
######################################################################################

#+++ prova a suddividere il prezzo in 3 fasce e crea una variabile, dopo di che fai una LDA
# creo una variabile e segmento la clientela su 3 fasce di prezzo: LowCost - Standard - Premium 


ds3 <- ds2 %>% mutate(price_range = 'Standard')
str(ds3)                                                         

ds3$price_range <- ifelse(ds3$Unit_Ticket < 100, 'LowCost', ifelse((ds3$Unit_Ticket > 100) & (ds3$Unit_Ticket < 200), 'Standard', 'Premium'))
ds3$price_range <- as.factor(ds3$price_range)
levels(ds3$price_range)                          

#A questo punto proverò a classificare tramite LDA oppure tramite SVM con metodo OVO, 
#usando price_range come variabile di risposta

ds3 %>%
  count(price_range) %>% 
  mutate(proportion=n/sum(n)) %>%  
  ggplot(aes(price_range, proportion)) + geom_bar(stat='identity')

#suddividendo in 3 fasce di prezzo in bins da 100€, circa il 98% delle osservazioni è classificata su standard
#Provo perciò a suddividere in standard e premium con 150€ come soglia 

ds3$price_range <- ifelse(ds3$Unit_Ticket < 150, 'Standard', 'Premium')
ds3$price_range <- as.factor(ds3$price_range)
levels(ds3$price_range)  

#riplotto per vedere la suddivisione 

ds3 %>%
  count(price_range) %>% 
  mutate(proportion=n/sum(n)) %>%  
  ggplot(aes(price_range, proportion)) + geom_bar(stat='identity')

#notiamo ora come oltre il 50% della clientela è premium, disposta a pagare un prezzo superiore ai 150€ per un biglietto 
#aereo

#Provo ora a fittare una Regressione Logistica e una SVM:

####################################
########  ++++  GLM  ++++  #########
####################################
glm_full <- glm(price_range~.-Unit_Ticket, data=ds3,family="binomial")
summary(glm_full)

#Possiamo notare come anche in questo caso la significatività sulla targetizzazione basata sulla fascia di prezzo
#considera le stesse variabili usate nella regressione lineare. Fitto la GLM con gli stessi predittori usati per i 
#linear models 


glm.fit1 <- glm(price_range~Seat+PriorBoard+Luggage+Fidelity+Departure-Unit_Ticket, data=ds3,family="binomial")
summary(glm.fit1)

glm.prob1 <- predict(glm.fit1, type="response")
contrasts(ds3$price_range)
glm.pred1 <- rep('Premium', 20001)
glm.pred1[glm.prob1>0.5] = "Standard"
table(glm.pred1,ds3$price_range)
(7959+6748)/20001 
#Accuracy del 73.5%, buono con soglia a 0.5, mantengo questo modello e testo la performance suddividendo in test e training set

set.seed(315)
ds3_train = sample_frac(ds3, .66)
ds3_test = setdiff(ds3, ds3_train)

glm.fit2train <- glm(price_range~Seat+PriorBoard+Luggage+Fidelity+Departure-Unit_Ticket, data=ds3_train,family="binomial")
glm.prob2 <- predict(glm.fit2train,type = 'response',newdata = ds3_test) 
glm.pred2 <- rep('Premium', 6800)
glm.pred2[glm.prob2>0.5] = "Standard"
table(glm.pred2,ds3_test$price_range)
(2635+2356)/6800          
#Accuracy sul test set del 73.39% --> buono, ho perso poco rispetto al training set

### +++ SVM +++ ###

library(e1071)
svm_model <-svm(price_range~Seat+PriorBoard+Luggage+Fidelity+Departure-Unit_Ticket, 
                 data=ds3_train, 
                 method="C-classification", 
                 kernel="linear")

svm_model
#come primo modello SVM provo un kernel lineare con Budget di default a 1



# ++++ predicts sul training set +++ #

pred_train <-predict(svm_model,ds3_train)
pred_train

table("truth"= ds3_train$price_range, "pred"=pred_train)

mean(pred_train== ds3_train$price_range) #[1] 0.7369896 --> circa 2 punti decimali percentuali in più rispetto alla logistica

# ++++ predicts sul test set ++++ #

pred_test <-predict(svm_model,ds3_test)
table("truth"=ds3_test$price_range, "pred"=pred_test)

mean(pred_test==ds3_test$price_range) #[1] 0.7333824 --> sul test set possiamo paragonarlo al modello logistico, stessa performance 

#### provo con kernel radiale 

svm_model2 <-svm(price_range~Seat+PriorBoard+Luggage+Fidelity+Departure-Unit_Ticket, 
                data=ds3_train, 
                method="C-classification", 
                kernel="radial")
svm_model2

svm_model2$cost
svm_model2$gamma

pred_test2 <-predict(svm_model2,ds3_test)
table("truth"=ds3_test$price_range, "pred"=pred_test2)

mean(pred_test2==ds3_test$price_range)    #1] performance sul kernel radiale  è inferiore al kernel lineare --> 0.7311765


















