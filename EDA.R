library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(treemap)

train <- read_csv("C:/Users/terte/Desktop/R/Project02/trainSmp.csv")
client <- read_csv("C:/Users/terte/Desktop/R/Project02/clientSmp.csv")
product <- read_csv("C:/Users/terte/Desktop/R/Project02/productSmp.csv")
town <- read_csv("C:/Users/terte/Desktop/R/Project02/town_state.csv")
##Week ----
head(train[train$Ruta_SAK == '1203',])
ggplot(train %>% sample_frac(0.005))+
  geom_histogram(aes(x=Semana), color="white", fill="blue", alpha=0.5)+
  scale_x_continuous(breaks=1:10)+
  scale_y_continuous(name="Client / Product deliveries")+
  theme_bw()

dim(train)
## https://www.kaggle.com/fabienvs/grupo-bimbo-data-analysis
agencias <- train %>%
  group_by(Agencia_ID) %>%
  summarise(Units = sum(Venta_uni_hoy),
            Pesos = sum(Venta_hoy),
            Return_Units = sum(Dev_uni_proxima),
            Return_Pesos = sum(Dev_proxima),
            Net = sum(Demanda_uni_equil)) %>%
  mutate(Net_Pesos = Pesos - Return_Pesos,
         Return_Rate = Return_Units / (Units+Return_Units)) %>%
  arrange(desc(Units)) %>%
  inner_join(town, by="Agencia_ID")

agencias_return <- train %>%
  group_by(Agencia_ID) %>%
  summarise(Units = sum(Venta_uni_hoy),
            Pesos = sum(Venta_hoy),
            Return_Units = sum(Dev_uni_proxima),
            Return_Pesos = sum(Dev_proxima),
            Net = sum(Demanda_uni_equil)) %>%
  mutate(Net_Pesos = Pesos - Return_Pesos,
         Return_Rate = Return_Units / (Units+Return_Units)) %>%
  arrange(desc(Return_Units)) %>%
  inner_join(town, by="Agencia_ID")
head(agencias_return)
##---- Route 1203
temp <- train[train$Ruta_SAK == '1203',]
agencias_return_V2 <- train[train$Ruta_SAK == '1203',] %>%
  group_by(Agencia_ID) %>%
  summarise(Units = sum(Venta_uni_hoy),
            Pesos = sum(Venta_hoy),
            Return_Units = sum(Dev_uni_proxima),
            Return_Pesos = sum(Dev_proxima),
            Net = sum(Demanda_uni_equil)) %>%
  mutate(Net_Pesos = Pesos - Return_Pesos,
         Return_Rate = Return_Units / (Units+Return_Units)) %>%
  arrange(desc(Return_Units)) %>%
  inner_join(town, by="Agencia_ID")

#agencias_return_V2[,1] <- sapply(agencias_return_V2[,1], as.factor)
temp[,2] <- sapply(temp[,2], as.factor)
head(agencias_return_V2)
str(temp)
dim(temp)

treemap(agencias_return_V2, 
        index=c("Agencia_ID"), vSize="Units", vColor="Return_Rate", 
        palette=c("#FFFFFF","#FFFFFF","#FF0000"),
        type="value", title.legend="Units return %", title="Top 100 agencias")

agent.aov <- aov(Dev_uni_proxima ~ Agencia_ID, data = temp)
summary(agent.aov)
##Assumption Check
# 1. Homogeneity of variances
plot(agent.aov, 1)
#library(car)
#leveneTest(Return_Units ~ Agencia_ID, data = agencias_return_V2)
# Normal Plot
plot(agent.aov, 2)

library(multcomp)
summary(glht(agent.aov, linfct = mcp(group = "Tukey")))

##----
#3734/284095 = 0.013
# Return by Agent 
treemap(agencias[1:100, ], 
        index=c("Agencia_ID"), vSize="Units", vColor="Return_Rate", 
        palette=c("#FFFFFF","#FFFFFF","#FF0000"),
        type="value", title.legend="Units return %", title="Top 100 agencias")

# Top 50 Return by Agent 
treemap(agencias_return[1:50, ], 
        index=c("Agencia_ID"), vSize="Return_Rate",vColor="Return_Rate", 
        palette=c("#FFFFFF","#FFFFFF","#FF0000"),
        type="value", title.legend="Units return %", title="Top 100 agencias")
#agencias[agencias$Agencia_ID == 2214,]  ## return 0.0461
#agencias[agencias$Agencia_ID == 2059,]  ## 0.372
#agencias[agencias$Agencia_ID == 1143,]  ## 0.121

sort(table(train$Ruta_SAK),decreasing = TRUE)
head(train)

# http://www.sthda.com/english/wiki/one-way-anova-test-in-r
# Compute the analysis of variance
r.aov <- aov(Dev_uni_proxima ~ Ruta_SAK, data = train)
# Summary of the analysis
summary(r.aov)
plot(r.aov, 2)
##we can conclude that there are significant differences between the Ruta_SAK highlighted with "*" in the model summary.