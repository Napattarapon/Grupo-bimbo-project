
##------- extract name --------------

##x <- preprocessed_products

preprocess_product <- function(product_data) {
  
  product_names <- as.character(product_data$product_name)
  name <- unlist(lapply(product_names, extract_name))
  
  data.frame(
    ID=product_data$ID,
    product_name=product_names,
    name=name
  )
}

extract_name <- function(namelist){
  
  tokens <- strsplit(namelist, " ")[[1]]
  tokens[1]
}

##a <- preprocess_product(x)

##------- non-bread filter -------------------------

require(dplyr)

supplier <- preprocessed_products$brand
unique(supplier)

bread_brand <- c('AM','BIM','BRE','DH','DIF','DH','GV','GBI'
                 ,'KOD','LAR','LON','MLA','MP','MR','ORO','PUL'
                 ,'SAN','SL','SUA','SUN','THO','TR','TRI','WON')
bread_data <- preprocessed_products %>% filter(as.character(supplier) %in% bread_brand)

unique(NewBreadData$Type)

NewBreadData$Type[which(NewBreadData$Type =="Nacho ")]<-"Nacho"

##write.csv(NewBreadData,file = "FinalBreadData.csv")

##--------- Check test for non-bread product ----------
ptest_id <- testSmp$Producto_ID
b_id <- bread_data$ID
c_id <- clientSmp$Cliente_ID
a_id <- town_state$Agencia_ID

test_clear1 <- testSmp %>% filter(ptest_id %in% b_id)

test_clear2 <- inner_join(test_clear1,NewBreadData,by = c("Producto_ID"="ID"))

test_clear2 <- test_clear2[which(test_clear2$Type != 'Other'),]

##--------- Check train for non-bread product ---------
ptrain_id <- trainSmp$Producto_ID

train_clear1 <- trainSmp %>% filter(ptrain_id %in% b_id)

train_clear2 <- inner_join(train_clear1,NewBreadData,by = c("Producto_ID"="ID"))

train_clear2 <- train_clear2[which(train_clear2$Type != 'Other'),]

train_clear3 <- train_clear2 %>% filter(train_clear2$Cliente_ID %in% c_id)

train_clear4 <- train_clear2 %>% filter(train_clear2$Agencia_ID %in% a_id)

##write.csv(train_clear2,file = "FinalTrain.csv")

 ##--------------- bread grouping ----------------------

a <- NewBreadData

##----------------BIM--------------------

## Get text vector
##BIM_txt <- a[a$brand == "BIM", "product_name"] %>% paste(collapse = " ")
BIM_txt <- a$product_name %>% paste(collapse = " ")
## Segmenting words

BIM_tok <- strsplit(BIM_txt, " ")
BIM_tok3 <- (lapply(BIM_tok, function(x) x[!grepl("\\d+", x)]))
BIM_tok2 <- (lapply(BIM_tok3, function(x) x[!grepl("^[A-Z]+$", x)]))[[1]]
BIM_tok_clean <- BIM_tok2[BIM_tok2 != " "]
BIM_tok_clean <- BIM_tok_clean[BIM_tok_clean != ""]
words.freq <- sort(table(unlist(BIM_tok_clean)))
BIM_tok_df <- as.data.frame(table(BIM_tok_clean))
BIM_tok_df <- BIM_tok_df[BIM_tok_df$Freq > 1, ] #word count more than 1
colnames(BIM_tok_df)[c(1,2)] <- c("word", "n")

## unique counts
BIM_tok_df2 <-
  BIM_tok_df %>% 
  group_by(word) %>% 
  summarise(n = sum(n)) %>% 
  arrange(-n) %>% 
  as.data.frame()

##-------------------------------------------



