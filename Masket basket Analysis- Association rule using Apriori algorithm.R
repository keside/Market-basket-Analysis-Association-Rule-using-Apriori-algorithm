#Loading Datasets
line_item <-read.csv('pricehub.line_item.csv', header = T, sep=",")
products<- read.csv("pricehub.products.csv", header = T, sep = ",")
orders<- read.csv("pricehub.orders.csv", header = T, sep = ",")

# Loading Library 
# library(dplyr)
# library(ggplot2)
# library(arules)
# library(skimr)
# library(mbar)
# library(plyr)

# Detaching "dplyr" and Loading "plyr"

if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
  detach(package:dplyr, unload=TRUE)
}



#EDA
head(line_item)
head(orders)
head(products)

anyNA(line_item)
anyNA((orders))
anyNA(products)

sum(is.na(line_item))
sum(is.na(products))
sum(is.na(orders))



## Check that all orders in line_item are present in our orders dataset. 
# Exclude from line_item any rows that do not meet that condition

New_line_item <- inner_join(line_item, orders, by = "id_order")

glimpse(New_line_item)

head(New_line_item)

skim(New_line_item)



## Creating a new column called totalQuantitypaid (unit_price*product_quanity)
Total_itemqtyprice <- New_line_item %>% 
  mutate(PQ_price = unit_price * product_quantity) 

head(Total_itemqtyprice)


## Grouping and summarising using id_order $ total_paid
Sum_Total_Paid <-Total_itemqtyprice %>% 
  group_by(id_order) %>% 
  summarise(total_paid_li = sum(PQ_price))


head(Sum_Total_Paid)


# Explore the relationship between prices in line_item and order.
# Do the prices of the orders and the sum of the prices of each item in the order match? 
# If not, can you make some assumptions that explain the differences?

## Joining orders, New_line_sum and grouping by "id_order"

Final<- inner_join(orders, Sum_Total_Paid, by = "id_order")



head(Final)
dim(Final)
is.na(Final)
glimpse(Final)


## Relationship without Prices in Order and lineitems using GGPLOT

ggplot(data=Final, aes(total_paid_li, total_paid)) + 
  geom_point(aes(col="pink", fill= '.count..')) 


# Using Qplot of GGPLOT  (Line Item Total Price)

qplot(Final$total_paid_li, geom = "histogram", binwidth = 50, 
      main = "Histogram of Total price of Line Item", xlab = "Total Line Item price Paid", 
      fill = ..count.., xlim=c(0,3000)) + scale_fill_gradient("Count", low = "green", high = "red")




## Exclude from line_item any rows from orders that are not "Completed".

New_line_item_completed <- filter(Final, state == "Completed")


completedstate <- Final %>% select(state) %>% filter(state =="Completed")
head(completedstate)

CancelledState <-Final %>% select(state) %>% filter(state =="Cancelled")

head(CancelledState)

# Exclude from line_item any rows that do not meet that condition.
Excluded_row <- nrow(line_item) - nrow(New_line_item)

head(Excluded_row)


## Explore the relationship between prices in line_item and product

# Do the prices of the items sold and the product prices match? If not,
# can you make some assumptions that explain the differences?

#EDA 
tbl_df(products)
data.frame(products)
dim(products)

# Checking that all sku in New_line_item and products dataset. 
Product_line_item<- inner_join(New_line_item, products, by = "sku")

head(Product_line_item)


## Creating a column Called PQ_Paid in line_item of products
Product_line_item <- Product_line_item %>% 
  mutate(Paid_PQ = unit_price*product_quantity)


## Exporing relationship between the prices of line_item and products using ggplot

Product_line_item %>% ggplot(aes(total_paid, Paid_PQ)) +  geom_point()

Product_line_item %>% ggplot(aes(total_paid, price, col= "unit_price" , size=1)) +  
  geom_point() +  theme_bw()


# CLEANING AND PREPROCESSING MY FINAL DATAFRAME 

Product_line_item$created_date <- as.Date(Product_line_item$created_date)
Product_line_item$time <- format(as.POSIXct(Product_line_item$date),format = "%H:%M:%S")

Product_line_item$created_date <- NULL
Product_line_item$time <- NULL


Final_data<- select(Product_line_item, sku, name_en, short_desc_en, price, brand, 
                            manual_categories,total_paid, Paid_PQ, 
                    product_quantity, unit_price,
                    id_order, created_date, date)



skim(Final_data)
summary(Final_data)
glimpse(Final_data)

# REMOVING OUTLIERS FROM OUR PRICE DATASET

boxplot(Final_data$total_paid)$out
boxplot(Final_data$unit_price)$out
boxplot(Final_data$price)$out
boxplot(Final_data$Paid_PQ)$out


Outlier_Unit_price <- boxplot(Final_data$unit_price, plot = FALSE)$out

Outlier_Paid_PQ <- boxplot(Final_data$Paid_PQ, plot = FALSE)$out

Outlier_price <- boxplot(Final_data$price, plot = FALSE)$out

Outlier_total_paid <- boxplot(Final_data$total_paid, plot = FALSE)$out

# FINDING THE OUTLIERS

Final_data[which(Final_data$unit_price %in% 
                   Outlier_Unit_price),]


Final_data[which(Final_data$price %in% 
                   Outlier_price),]


Final_data[which(Final_data$total_paid %in% 
                   Outlier_total_paid),]

Final_data[which(Final_data$Paid_PQ %in% 
                   Outlier_Paid_PQ),]

# REMOVING PRICES OUTLIERS FROM THE DATASET

Final_data <-Final_data$unit_price[-which(Final_data$unit_price %in% Outlier_Unit_price),]

Final_data <- Final_data$price[-which(Final_data$price %in% Outlier_price),]

Final_data <- Final_data$total_paid[-which(Final_data$total_paid %in% Outlier_total_paid),]

Final_data <- Final_data$Paid_PQ[-which(Final_data$Paid_PQ %in% Outlier_Paid_PQ),]


# CHECKING THE DATASET AFTER REMOVING PRICE OUTLIERS


# CHECKING RELATIONSHIP BLW PRICES IN OUR DATASET 
ggplot(Final_data, aes(total_paid, Paid_PQ, color= "blue")) + geom_point()



# CREATING A TRANSACTIONAL FILE

#Since the structure of the data is not in the format necessary to find association rules, 
#we have to perform some data manipulations before finding the relationships.

# Lets first make sure that the "id_order" are of numeric data type 
# and then sort the dataframe based on the "id_order".

df_sorted <- Final_data[order(Final_data$id_order),]

df_sorted$id_order <- as.numeric(df_sorted$id_order)

# The next step is to actually convert the dataframe into basket format,
# based on the id_order.

df_itemlist <- ddply(df_sorted,c("id_order" ,'created_date'),
                     function(df1)paste(df1$sku,collapse = ",")) %>% 
  rename(SKU = V1)

head(df_itemlist)
dim(df_itemlist)

# Once we have the transactions, we no longer need the date
# in our analysis. Go ahead and delete those columns

df_itemlist$created_date <- NULL

#Rename column headers for ease of use
colnames(df_itemlist) <- c("itemlist")

# Write a csv file out of the line_item dataframe (the one that you've been cleaning),
# containing only the columns id_order and sku


write.csv( df_itemlist, "itemlist.csv", row.names = T)


# Using Single as my format

get_data <- read.transactions("itemlist.csv", 
                              format = "single", header= T,
                              sep = ",",
                              cols = c("id_order", "SKU"))
summary(get_data)

inspect(get_data)

length(get_data)

size(get_data)

itemLabels(get_data)



# The most frequent item in our dataset can be seen by plotting "itemFrequencyPlot"
# absolute and relative plot

itemFrequencyPlot(get_data, topN = 10, type = 'absolute')

itemFrequencyPlot(get_data, topN = 10, type = 'relative') 

# Using Image() function to visualize

image(get_data)



# EDA
# How many items are purchased on an average?
items <- df_itemlist %>% group_by(id_order) %>%
  summarize(count = n()) %>%
  pull(count) 

mean(items)

median(items)

# Most Purchased SKU

df_sorted %>%
  group_by(sku) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Most short_en product
df_sorted %>%
  group_by(short_desc_en) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Most name_en product
df_sorted %>%
  group_by(name_en) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


# APPLYING APRIORI ALGORITHM

summary(get_data)

Apriorirules <- apriori(get_data,parameter = list(sup = 0.01, conf = 0.5, target= "rules",
                                                     maxlen = 10))

inspect(Apriorirules)



