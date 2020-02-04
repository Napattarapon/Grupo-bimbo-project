##require(usethis)
##usethis::edit_r_environ()

##----------------------------------------------------------
## create percentRT for 'percentage of returned product'
##----------------------------------------------------------

require(ggplot2)
require(dplyr)

data <- table1
##pairs(sapply(data[,c(1,2,3,5,6,7,8,9,10)],FUN = jitter),col = data$Ruta_SAK)
data['percentRT'] <- as.numeric(data$Dev_uni_proxima)*100 / (as.numeric(data$Dev_uni_proxima)+as.numeric(data$Venta_uni_hoy))
data <- as.data.frame(data)
data[order(-data$percentRT),]
data['Ruta_2digit'] <- substr(data$Ruta_SAK,1,2)

##----------------------------------------------------------
## plot train between Ruta_SAK and % Return
##----------------------------------------------------------

route <- data %>% select(2,4,7,9) %>% group_by(Ruta_SAK) %>%
  summarise(Dev_uni_proxima = sum(Dev_uni_proxima),Venta_uni_hoy=sum(Venta_uni_hoy))

route['percentRT'] <- route$Dev_uni_proxima*100 / (route$Dev_uni_proxima+route$Venta_uni_hoy)

ggplot(route[with(route,order(-Ruta_SAK)),] , aes(x = Ruta_SAK, y = percentRT)) + geom_bar(stat="identity") + 
  labs(x = "Ruta_SAK", y = "%return",
       title="Bar Chart", 
       subtitle="route vs %return ", 
       caption="source: Bimbo")

##----------------------------------------------------------
## find max percentRT
##----------------------------------------------------------

r <- as.data.frame(route)
maxpercent <- r[order(-r$percentRT),]
r_have_RT <- maxpercent$Ruta_SAK[which(maxpercent$percentRT>10)]
RT_route <- data[which(data$Ruta_2digit == 34),]
unique(substr(RT_route$Ruta_SAK,1,2))

RT_non_zero <- RT_route[which(RT_route$percentRT!=0),]

client_have_RT <- unique(RT_route$Cliente_ID)
##client_787528 <- data[which(data$Cliente_ID == 787528),] ##route 3414
client_24146 <- data[which(data$Cliente_ID == 24146),]
ggqqplot(RT_non_zero$percentRT)

##----------------------------------------------------------
## examine Ruta_2digit
##----------------------------------------------------------
route_2digit <- data %>% select(2,26,7,9) %>% group_by(Ruta_2digit) %>%
  summarise(Dev_uni_proxima = sum(Dev_uni_proxima),Venta_uni_hoy=sum(Venta_uni_hoy))

route_2digit['percentRT'] <- route_2digit$Dev_uni_proxima*100 / (route_2digit$Dev_uni_proxima+route_2digit$Venta_uni_hoy)
a <- route_2digit[order(-route_2digit$percentRT),]

ggplot(a[with(a,order(-percentRT)),] , aes(x = Ruta_2digit, y = percentRT)) + geom_bar(stat="identity") + 
  labs(x = "main Ruta_SAK", y = "%return",
       title="Bar Chart", 
       subtitle="main route vs %return ", 
       caption="source: Bimbo")

sort(table(data$Ruta_2digit))

##----------------------------------------------------------
## examine Ruta$SAK percentRT != 0
##----------------------------------------------------------

route.RT <- maxpercent[which(maxpercent$percentRT!=0),]
route.RT['Ruta_2digit'] <- substr(route.RT$Ruta_SAK,1,2)
str(route.RT)

topRT <- data[which(data$Ruta_SAK %in% route.RT$Ruta_SAK),]
##topRT <- route[which(route$Ruta_SAK %in% route.RT$Ruta_SAK),]
##str(topRT)
##pairs(sapply(topRT[,c(1,2,3,4,5,6,7,9)],FUN = jitter),col = topRT$Semana)
unique(topRT$Canal_ID)
##unique(data$Canal_ID)
unique(topRT$Producto_ID)
unique(topRT$Agencia_ID)
unique(topRT$Type)
unique(topRT$State)
unique(topRT$brand)
unique(topRT$Ruta_SAK)
unique(topRT$Ruta_2digit)


##----------------------------------------------------------
## examine weight vs percent return
##----------------------------------------------------------

data.RT <- data[which(data$percentRT!=0),] %>% select(18,7,9) %>% group_by(weight) %>%
  summarise(Dev_uni_proxima = sum(Dev_uni_proxima),Venta_uni_hoy=sum(Venta_uni_hoy))
data.RT[is.na(data.RT)] <- 0
data.RT[which(data.RT$weight == "")] <- 0
data.RT['percentRT'] <- data.RT$Dev_uni_proxima*100 / (data.RT$Dev_uni_proxima+data.RT$Venta_uni_hoy)
data.RT[which(data.RT$weight > 1000),] <- 0


ggplot(data.RT[with(data.RT,order(-weight)),] , aes(x = weight, y = percentRT)) + geom_point() + 
  labs(x = "weight", y = "%return",
       title="Dot plot", 
       subtitle="weight vs %return ", 
       caption="source: Bimbo")

ggdensity(data.RT$percentRT)
ggqqplot(data.RT$percentRT)

##----------------------------------------------------------
## examine client vs percent return
##----------------------------------------------------------

client.RT <- data[which(data$percentRT!=0),] %>% select(5,7,9) %>% group_by(Cliente_ID) %>%
  summarise(Dev_uni_proxima = sum(Dev_uni_proxima),Venta_uni_hoy=sum(Venta_uni_hoy))
client.RT[is.na(client.RT)] <- 0
client.RT['percentRT'] <- client.RT$Dev_uni_proxima*100 / (client.RT$Dev_uni_proxima+client.RT$Venta_uni_hoy)
client.RT[order(-client.RT$Dev_uni_proxima),]
client.RT <- client.RT[order(client.RT$percentRT),]
client.RT1 <- client.RT[which(client.RT$percentRT>50),]
-client.RT1 <-inner_join(client.RT1,clientSmp,by="Cliente_ID")
client.RT2 <- client.RT1[which(client.RT1$NombreCliente == "NO IDENTIFICADO"),]
no_id_client <- clientSmp[which(clientSmp$NombreCliente == "NO IDENTIFICADO"),]
client_clear <- clientSmp[which(clientSmp$Cliente_ID %in% client.RT$Cliente_ID),]

require(dplyr) 
client.RT <- inner_join(client.RT,clientSmp,by="Cliente_ID")
ggplot(client.RT1 , aes(x = Cliente_ID, y = Dev_uni_proxima)) + geom_point() + 
  labs(x = "Cliente_ID", y = "returned unit",
       title="Dot plot", 
       subtitle="Client vs Return unit ", 
       caption="source: Bimbo")

require(dplyr)
require(ggpubr)
ggdensity(client.RT$percentRT)
ggqqplot(client.RT$percentRT)
                         
client.RT['count'] <- 1:length(client.RT$percentRT)
plot(client.RT$count,client.RT$percentRT)
qqnorm(client.RT$percentRT)
qqline(client.RT$percentRT)


data.lm <- data
data.lm$Semana <- as.factor(data.lm$Semana)
data.lm$Ruta_SAK <- as.factor(data.lm$Ruta_SAK)
data.lm$Ruta_2digit <- as.factor(data.lm$Ruta_2digit)
data.lm$Type <- as.factor(data.lm$Type)
data.lm$State <- as.factor(data.lm$State)
data.lm$brand <- as.factor(data.lm$brand)
data.lm$Cliente_ID <- as.factor(data.lm$Cliente_ID)
data.lm$Agencia_ID <- as.factor(data.lm$Agencia_ID)
data.lm$Canal_ID <- as.factor(data.lm$Canal_ID)

plot(data.lm$Ruta_2digit,data.lm$percentRT)

str(data.lm)
##result <- lm(percentRT~Canal_ID+Ruta_2digit+Type+Semana+Agencia_ID+State+brand,data.lm)
result <- aov(percentRT~Ruta_2digit,data.lm) ##sign
result <- aov(percentRT~Canal_ID,data.lm) ##sign
result <- aov(percentRT~Ruta_SAK,data.lm) 
result <- aov(percentRT~Type,data.lm) ##sign
result <- aov(percentRT~Semana,data.lm) ##sign
result <- aov(percentRT~Agencia_ID,data.lm) 
result <- aov(percentRT~Client_ID,data.lm)
result <- aov(percentRT~brand,data.lm) ##sign
result <- aov(percentRT~State,data.lm) ##sign
summary(result)



##----------------------------------------------------------
## examine Ruta_SAK 34XX
##----------------------------------------------------------

route34 <- data[which(data$Ruta_2digit == 34),]


##Type
route34_type <- route34 %>% select(17,4,7,9) %>% group_by(brand,Ruta_SAK) %>%
  summarise(Dev_uni_proxima = sum(Dev_uni_proxima),Venta_uni_hoy=sum(Venta_uni_hoy))
route34_type['percentRT'] <- route34_type$Dev_uni_proxima*100 / (route34_type$Dev_uni_proxima+route34_type$Venta_uni_hoy)
ggplot(route34_type , aes(x = Type, y = percentRT)) + geom_bar(stat = "identity") + 
  labs(x = "Type", y = "%return",
       title="bar plot", 
       subtitle="Type vs %Return", 
       caption="source: Bimbo")

route34_type$brand <- as.factor(route34_type$brand)
route34_type$Ruta_SAK <- as.factor(route34_type$Ruta_SAK)

route34_type[order(-route34_type$percentRT),]
route34_type[order(-route34_type$Venta_uni_hoy),]

ggplot(route34_type,aes(x= brand,y = Ruta_SAK,fill = route34_type$percentRT,label = Dev_uni_proxima)) +
  geom_tile() + scale_fill_gradient(low="green", high="red") +
  geom_text()

mean_type <- aggregate(route34_type[,5],list(route34_type$brand),mean)
head(mean_type[order(-mean_type$percentRT),])

##Semana
route34_semana <- route34 %>% select(1,7,9) %>% group_by(Semana) %>%
  summarise(Dev_uni_proxima = sum(Dev_uni_proxima),Venta_uni_hoy=sum(Venta_uni_hoy))
route34_semana['percentRT'] <- route34_semana$Dev_uni_proxima*100 / (route34_semana$Dev_uni_proxima+route34_semana$Venta_uni_hoy)
ggplot(route34_semana , aes(x = Semana, y = percentRT)) + geom_bar(stat = "identity") + 
  labs(x = "Semana", y = "%return",
       title="bar plot", 
       subtitle="Week vs %Return", 
       caption="source: Bimbo")


##Route
route34_Ruta <- route34 %>% select(4,7,9) %>% group_by(Ruta_SAK) %>%
  summarise(Dev_uni_proxima = sum(Dev_uni_proxima),Venta_uni_hoy=sum(Venta_uni_hoy))
route34_Ruta['percentRT'] <- route34_Ruta$Dev_uni_proxima*100 / (route34_Ruta$Dev_uni_proxima+route34_Ruta$Venta_uni_hoy)
route34_Ruta$Ruta_SAK <- as.factor(route34_Ruta$Ruta_SAK)
ggplot(route34_Ruta , aes(x = Ruta_SAK, y = percentRT)) + geom_bar(stat = "identity") + 
  labs(x = "Ruta_SAK", y = "%return",
       title="bar plot", 
       subtitle="Route vs %Return", 
       caption="source: Bimbo")


##Agent
route34_agent <- route34 %>% select(2,7,9) %>% group_by(Agencia_ID) %>%
  summarise(Dev_uni_proxima = sum(Dev_uni_proxima),Venta_uni_hoy=sum(Venta_uni_hoy))
route34_agent['percentRT'] <- route34_agent$Dev_uni_proxima*100 / (route34_agent$Dev_uni_proxima+route34_agent$Venta_uni_hoy)
route34_agent[order(-route34_agent$percentRT),]
route34_agent$Agencia_ID <- as.factor(route34_agent$Agencia_ID)
ggplot(route34_agent , aes(x = Agencia_ID, y = percentRT)) + geom_bar(stat = "identity") + 
  labs(x = "Agencia_ID", y = "%return",
       title="bar plot", 
       subtitle="Agent vs %Return", 
       caption="source: Bimbo")



##State
route34_state <- route34 %>% select(13,7,9) %>% group_by(State) %>%
  summarise(Dev_uni_proxima = sum(Dev_uni_proxima),Venta_uni_hoy=sum(Venta_uni_hoy))
route34_state['percentRT'] <- route34_state$Dev_uni_proxima*100 / (route34_state$Dev_uni_proxima+route34_state$Venta_uni_hoy)
route34_state[order(-route34_state$percentRT),]
route34_state$State <- as.factor(route34_state$State)
ggplot(route34_state , aes(x = State, y = percentRT)) + geom_bar(stat = "identity") + 
  labs(x = "State", y = "%return",
       title="bar plot", 
       subtitle="State vs %Return", 
       caption="source: Bimbo")



##Brand
route34_brand <- route34 %>% select(17,7,9) %>% group_by(brand) %>%
  summarise(Dev_uni_proxima = sum(Dev_uni_proxima),Venta_uni_hoy=sum(Venta_uni_hoy))
route34_brand['percentRT'] <- route34_brand$Dev_uni_proxima*100 / (route34_brand$Dev_uni_proxima+route34_brand$Venta_uni_hoy)
route34_brand[order(-route34_brand$percentRT),]
ggplot(route34_brand , aes(x = brand, y = percentRT)) + geom_bar(stat = "identity") + 
  labs(x = "Brand", y = "%return",
       title="bar plot", 
       subtitle="Brand vs %Return", 
       caption="source: Bimbo")



##Client
route34_client <- route34 %>% select(5,7,9) %>% group_by(Cliente_ID) %>%
  summarise(Dev_uni_proxima = sum(Dev_uni_proxima),Venta_uni_hoy=sum(Venta_uni_hoy))
route34_client['percentRT'] <- route34_client$Dev_uni_proxima*100 / (route34_client$Dev_uni_proxima+route34_client$Venta_uni_hoy)
route34_client[order(-route34_client$percentRT),]
route34_client$Cliente_ID <- as.factor(route34_client$Cliente_ID)
ggplot(route34_client[which(route34_client$percentRT>50),] , aes(x = Cliente_ID, y = percentRT)) + geom_bar(stat = "identity") + 
  labs(x = "Cliente_ID", y = "%return",
       title="bar plot", 
       subtitle="Client vs %Return", 
       caption="source: Bimbo")

require(treemap)
set.seed(1234)
treemap(route34_client,index = "Cliente_ID",vSize = "Venta_uni_hoy",
        vColor="percentRT", 
        palette=c("#FFFFFF","#FFFFFF","#FF0000"),
        type="value", title.legend="Units return %", title="Clients in Route 34XX")


h.plot <- data[which(data$Ruta_2digit %in% c(34,17,15,22,18)),]

routes.agencias <- data %>% group_by(Ruta_SAK, Agencia_ID) %>%
  summarise(count=n(),
            n_Clients = n_distinct(Cliente_ID),
            Units=sum(Venta_uni_hoy),
            Return_Units = sum(Dev_uni_proxima)) %>%
  mutate(Return_Rate = Return_Units / (Units+Return_Units)) %>%
  arrange(desc(Units))

ggplot(routes.agencias)+
  geom_point(aes(x=as.character(Ruta_2digit), 
                 y=as.character(Agencia_ID), 
                 size=Units,color=Return_Rate))+
  scale_x_discrete(name="Routes")+
  scale_y_discrete(name="Agencies")+
  scale_color_gradient(name="Return Rate", low="blue", high="red")+
  ggtitle("Top 100 agencies & routes")+
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

require(hrbrthemes)
for_heat <- h.plot %>% select(1,4,7,9) %>% group_by(Semana, Ruta_SAK) %>%
  summarise(Dev_uni_proxima = sum(Dev_uni_proxima),Venta_uni_hoy=sum(Venta_uni_hoy))

for_heat['percentRT'] <- for_heat$Dev_uni_proxima*100 / (for_heat$Dev_uni_proxima+for_heat$Venta_uni_hoy)

for_heat$Semana <- as.factor(for_heat$Semana)
for_heat$Ruta_SAK <- as.factor(for_heat$Ruta_SAK)

ggplot(for_heat,aes(for_heat$Ruta_SAK,for_heat$Semana,fill = for_heat$percentRT)) +
  geom_tile() + scale_fill_gradient(low="green", high="red")







