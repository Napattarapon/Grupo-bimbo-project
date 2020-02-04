if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("diegovalle/mxmaps")
require(mxmaps)

##-------------------------------------------------
##variable
##-------------------------------------------------

week <- trainSmp$Semana
seller.id <- trainSmp$Agencia_ID
canal.id <- trainSmp$Canal_ID
route.id <- trainSmp$Ruta_SAK
client.id <- trainSmp$Cliente_ID
product.id <- trainSmp$Producto_ID
unit.sell <- trainSmp$Venta_uni_hoy
rev.sell <- trainSmp$Venta_hoy
unit.return <- trainSmp$Dev_uni_proxima
rev.return <- trainSmp$Dev_proxima
unit.need <- trainSmp$Demanda_uni_equil

##-------------------------------------------------
##town
##-------------------------------------------------

town2 <- c()
town1 <- unique(town_state$Town)

for (i in 1:(length(town1))){
  town2 <- c(town2,substr(town1[i], 1, 4),substring(town1[i], 6))
}

town <- t(matrix(data = town2, nrow = 2 ))

##-------------------------------------------------
##link seller.id and town
##-------------------------------------------------

require(dplyr)
require(data.table)
table1 <- inner_join(trainSmp,town_state,by="Agencia_ID")
table1 <- inner_join(table1,NewBreadData,by=c("Producto_ID"="ID"))
town.id <- table1$Town
state.id <- table1$State
product.type <- table1$Type

##-------------------------------------------------
##sum unit demands
##-------------------------------------------------
##my_data <- table1 %>% select(1,11,13) %>% 
##  group_by(State,Semana) %>% 
##  summarise(Demanda_uni_equil = sum(Demanda_uni_equil))
##my_data$State <- as.character(my_data$State)
##my_data$Type <- as.character(my_data$Type)

my_data <- table1 %>% select(1,11,13,24) %>% 
  group_by(State,Semana,Type) %>% 
  summarise(Demanda_uni_equil = sum(Demanda_uni_equil))
my_data$State <- as.character(my_data$State)
my_data$Type <- as.character(my_data$Type)
##-------------------------------------------------
##change name of states in my_data and group by state again
##-------------------------------------------------

my_data$State[which(my_data$State=="QUERETARO")]<-"Querétaro"
my_data$State[which(my_data$State=="Queretaro de Arteaga")]<-"Querétaro"
my_data$State[which(my_data$State=="AGUASCALIENTES")]<-"Aguascalientes"
my_data$State[which(my_data$State=="BAJA CALIFORNIA NORTE")]<-"Baja California"
my_data$State[which(my_data$State=="BAJA CALIFORNIA SUR")]<-"Baja California Sur"
my_data$State[which(my_data$State=="CAMPECHE")]<-"Campeche"
my_data$State[which(my_data$State=="COAHUILA")]<-"Coahuila"
my_data$State[which(my_data$State=="COLIMA")]<-"Colima"
my_data$State[which(my_data$State=="CHIAPAS")]<-"Chiapas"
my_data$State[which(my_data$State=="CHIHUAHUA")]<-"Chihuahua"
my_data$State[which(my_data$State=="ESTADO DE MÉXICO")]<-"Ciudad de México"
my_data$State[which(my_data$State=="DURANGO")]<-"Durango"
my_data$State[which(my_data$State=="GUANAJUATO")]<-"Guanajuato"
my_data$State[which(my_data$State=="GUERRERO")]<-"Guerrero"
my_data$State[which(my_data$State=="HIDALGO")]<-"Hidalgo"
my_data$State[which(my_data$State=="JALISCO")]<-"Jalisco"
my_data$State[which(my_data$State=="MÉXICO, D.F.")]<-"México"
my_data$State[which(my_data$State=="MICHOACÁN")]<-"Michoacán"
my_data$State[which(my_data$State=="MORELOS")]<-"Morelos"
my_data$State[which(my_data$State=="NAYARIT")]<-"Nayarit"
my_data$State[which(my_data$State=="NUEVO LEÓN")]<-"Nuevo León"
my_data$State[which(my_data$State=="OAXACA")]<-"Oaxaca"
my_data$State[which(my_data$State=="PUEBLA")]<-"Puebla"
my_data$State[which(my_data$State=="QUINTANA ROO")]<-"Quintana Roo"
my_data$State[which(my_data$State=="SAN LUIS POTOSÍ")]<-"San Luis Potosí"
my_data$State[which(my_data$State=="SINALOA")]<-"Sinaloa"
my_data$State[which(my_data$State=="SONORA")]<-"Sonora"
my_data$State[which(my_data$State=="TABASCO")]<-"Tabasco"
my_data$State[which(my_data$State=="TAMAULIPAS")]<-"Tamaulipas"
my_data$State[which(my_data$State=="TLAXCALA")]<-"Tlaxcala"
my_data$State[which(my_data$State=="VERACRUZ")]<-"Veracruz"
my_data$State[which(my_data$State=="YUCATÁN")]<-"Yucatán"
my_data$State[which(my_data$State=="ZACATECAS")]<-"Zacatecas"

my_data <- my_data %>% group_by(State,Semana,Type) %>% 
  summarise(Demanda_uni_equil = sum(Demanda_uni_equil))
my_data$Demanda_uni_equil <- as.numeric(my_data$Demanda_uni_equil)
my_data <- as.data.frame(my_data)

##-------------------------------------------------
##Function for product type
##-------------------------------------------------

biscuit <- function(data){
  data <- filter(data,data$Type == 'Biscuit and cracker')
  return(data)
}

bun <- function(data){
  data <- filter(data,data$Type == 'Bun')
  return(data)
}

cake <- function(data){
  data <- filter(data,data$Type == 'Cake')
  return(data)
}

cookie <- function(data){
  data <- filter(data,data$Type == 'Cookie')
  return(data)
}

donut <- function(data){
  data <- filter(data,data$Type == 'Donut')
  return(data)
}

nacho <- function(data){
  data <- filter(data,data$Type == 'Nacho')
  return(data)
}

pie <- function(data){
  data <- filter(data,data$Type == 'Pie')
  return(data)
}

roll <- function(data){
  data <- filter(data,data$Type == 'Roll')
  return(data)
}

short <- function(data){
  data <- filter(data,data$Type == 'Short bread')
  return(data)
}

snack <- function(data){
  data <- filter(data,data$Type == 'Snack')
  return(data)
}

tortilla <- function(data){
  data <- filter(data,data$Type == 'Tortilla')
  return(data)
}

white <- function(data){
  data <- filter(data,data$Type == 'White bread')
  return(data)
}

grain <- function(data){
  data <- filter(data,data$Type == 'Whole grain')
  return(data)
}

bigote <- function(data){
  data <- filter(data,data$Type == 'Bigote')
  return(data)
}

crumb <- function(data){
  data <- filter(data,data$Type == 'Bread crumbs')
  return(data)
}

croissant <- function(data){
  data <- filter(data,data$Type == 'Croissant')
  return(data)
}

muffin <- function(data){
  data <- filter(data,data$Type == 'Muffin')
  return(data)
}

pizza <- function(data){
  data <- filter(data,data$Type == 'Pizza')
  return(data)
}

hotdog <- function(data){
  data <- filter(data,data$Type == 'Hot dog')
  return(data)
}

##-------------------------------------------------
##unit demand in each week
##-------------------------------------------------

data3 <- filter(my_data,my_data$Semana == '3')
data4 <- filter(my_data,my_data$Semana == '4')
data5 <- filter(my_data,my_data$Semana == '5')
data6 <- filter(my_data,my_data$Semana == '6')
data7 <- filter(my_data,my_data$Semana == '7')
data8 <- filter(my_data,my_data$Semana == '8')
data9 <- filter(my_data,my_data$Semana == '9')

##-------------------------------------------------
##unit demand in each product per week
##-------------------------------------------------

data_week <- function(data,a){
  if(a == 'bis'){result <- biscuit(data)}
  if(a == 'bun'){result <- bun(data)}
  if(a == 'cake'){result <- cake(data)}
  if(a == 'cookie'){result <- cookie(data)}
  if(a == 'bigote'){result <- bigote(data)}
  if(a == 'crumb'){ result <- crumb(data)}
  if(a == 'croissant'){result <- croissant(data)}
  if(a == 'donut'){result <- donut(data)}
  if(a == 'hotdog'){result<- hotdog(data)}
  if(a == 'muffin'){result <- muffin(data)}
  if(a == 'nacho'){result <- nacho(data)}
  if(a == 'pie'){result<- pie(data)}
  if(a == 'pizza'){result <- pizza(data)}
  if(a == 'roll'){result <- roll(data)}
  if(a == 'short'){result <-short(data)}
  if(a == 'snack'){result <- snack(data)}
  if(a == 'tortilla'){result <- tortilla(data)}
  if(a == 'white'){result <- white(data)}
  if(a == 'grain'){result <- grain(data)}
  return(result)
}


##-------------------------------------------------
##most demanding product type
##-------------------------------------------------

count_demand <- table1 %>% select(11,24) %>% 
  group_by(Type) %>% 
  summarise(Demanda_uni_equil = sum(Demanda_uni_equil))

count_demand[order(-count_demand$Demanda_uni_equil),]

##-------------------------------------------------
##select type to visulaize
##-------------------------------------------------
word <- "tortilla" 

d_w3 <- data_week(data3,word)
d_w4 <- data_week(data4,word)
d_w5 <- data_week(data5,word)
d_w6 <- data_week(data6,word)
d_w7 <- data_week(data7,word)
d_w8 <- data_week(data8,word)
d_w9 <- data_week(data9,word)

##-------------------------------------------------
##Sync demand value in df_mxstate
##-------------------------------------------------

##str(df_mxstate)
data_w3 <- merge(df_mxstate,unique(d_w3),by.x = "state_name",by.y = "State")
data_w4 <- merge(df_mxstate,d_w4,by.x = "state_name",by.y = "State")
data_w5 <- merge(df_mxstate,d_w5,by.x = "state_name",by.y = "State")
data_w6 <- merge(df_mxstate,d_w6,by.x = "state_name",by.y = "State")
data_w7 <- merge(df_mxstate,d_w7,by.x = "state_name",by.y = "State")
data_w8 <- merge(df_mxstate,d_w8,by.x = "state_name",by.y = "State")
data_w9 <- merge(df_mxstate,d_w9,by.x = "state_name",by.y = "State")

##-------------------------------------------------
##heatmap week3
##-------------------------------------------------

data_w3$value <- data_w3$Demanda_uni_equil
mxstate_choropleth(data_w3,
                   title = "Total Demand of Tortilla, by state, week 3") 


##-------------------------------------------------
##heatmap week4
##-------------------------------------------------

data_w4$value <- data_w4$Demanda_uni_equil
mxstate_choropleth(data_w4,
                   title = "Total Demand of Biscuit, by state, week 4") 

##-------------------------------------------------
##heatmap week5
##-------------------------------------------------

data_w5$value <- data_w5$Demanda_uni_equil
mxstate_choropleth(data_w5,
                   title = "Total Demand of Bread, by state, week 5") 

##-------------------------------------------------
##heatmap week6
##-------------------------------------------------

data_w6$value <- data_w6$Demanda_uni_equil
mxstate_choropleth(data_w6,
                   title = "Total Demand of Tortilla, by state, week 6") 

##-------------------------------------------------
##heatmap week7
##-------------------------------------------------

data_w7$value <- data_w7$Demanda_uni_equil
mxstate_choropleth(data_w7,
                   title = "Total Demand of Bread, by state, week 7") 

##-------------------------------------------------
##heatmap week8
##-------------------------------------------------

data_w8$value <- data_w8$Demanda_uni_equil
mxstate_choropleth(data_w8,
                   title = "Total Demand of Bread, by state, week 8") 

##-------------------------------------------------
##heatmap week9
##-------------------------------------------------

data_w9$value <- data_w9$Demanda_uni_equil
mxstate_choropleth(data_w9,
                   title = "Total Demand of Tortilla, by state, week 9") 



