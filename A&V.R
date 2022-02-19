
###############################
# Ucitavanje potrebnih paketa #
###############################


library(tidyverse)
library(caret)
library(ggplot2)
library(broom)
library(dplyr)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(ISLR)
library(readr)
library(e1071)
library(ISOweek)
library(tibble)
library(readxl)
library(writexl)
library(lubridate)
library(corrplot)

###############################
# Ucitavanje sirovih podataka #
###############################

# Podaci se odnose na vremenske prilike u Beogradu izmedju 01.01.2016. i 31.12.2020.
# Skrejpovani su pomocu Pajton skripte u prilogu (Jupyter Notebook) i besplatne 
# verzije API-ja: https://www.worldweatheronline.com/developer/ 

raw_data <- read_excel("Belgrade_raw_2016_2020.xlsx")
view(raw_data)
str(raw_data)

################################
# Priprema podataka za analizu #
################################

# Cilj projekta je naci najbolji decision tree klasifikacioni algoritam za predvidjanje
# da li ce odredenog dana padati kisa spram ostalih vremenskih varijabli.
# Za te potrebe cemo prvo srediti podatke i konstruisati
# zavisnu varijablu "rain" kao binarnu varijablu sa dva nivoa 1/0), kao i par dodatnih nezavisnih.

# Selekcija i reimenovanje kolona (varijabli)
daily_weather_df <- as_tibble(raw_data)

daily_weather_df <- daily_weather_df %>%
  select(
    date_time,
    maxtempC,
    mintempC,
    humidity,
    windspeedKmph,
    WindGustKmph,
    DewPointC,
    cloudcover,
    precipMM,
    totalSnow_cm, 
    pressure
  ) %>%
  rename(
    date = date_time,
    max_temp_c = maxtempC,
    min_temp_c = mintempC,
    wind_speed_kmph = windspeedKmph,
    wind_gust_kmph = WindGustKmph,
    dew_point_c = DewPointC,
    cloud_cover = cloudcover,
    precip_mm = precipMM,
    total_snow_cm = totalSnow_cm, 
  )

# Konstrukcija zavisne varijable
daily_weather_df$rain <- ifelse(daily_weather_df$precip_mm > 0, 1, 0)
table(daily_weather_df$rain) #0:819; 1:1008

# Konstrukcija novih nezavisnih varijabli
daily_weather_df$mean_temp_c <- (daily_weather_df$max_temp_c + daily_weather_df$min_temp_c) / 2
daily_weather_df$temp_difference <- daily_weather_df$max_temp_c - daily_weather_df$min_temp_c
daily_weather_df$temp_difference_extreme <- ifelse(daily_weather_df$temp_difference > 10, 1, 0)
daily_weather_df$year <- year(daily_weather_df$date)
daily_weather_df$month <- month(daily_weather_df$date)
daily_weather_df$iso_week <- isoweek(daily_weather_df$date)

# Eksportovanje pripremljenih podataka u eksel fajl
clean_data <- daily_weather_df[, c(12, 1, 16, 17, 18, 13, 2, 3, 14, 15, 4, 5, 6, 7, 8, 10, 11)]
write_xlsx(clean_data, "clean_data.xlsx") 
read_excel('clean_data.xlsx')

#########################
# Eksploratorna analiza #
#########################

# Bar chart kisnih dana
rain_tab <- data.frame(table(clean_data$rain))
colnames(rain_tab) <- c('Rain', 'Frequency')
rain_tab$Rain <- c('No', 'Yes')

ggplot(rain_tab, aes(x = Frequency, y = reorder(Rain, Frequency), fill = Rain)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label= round(Frequency)), vjust=0) +
  labs(y = 'Yes or no', x = "Frequency", title= "Rainy days") +
  theme(legend.position="none")+
  theme_bw()

# Bar chart kisnih dana po godinama
rain_year <- clean_data %>% group_by(year) %>%
             summarise(rainy_days_sum = sum(rain))

ggplot(rain_year, aes(x = year, y = rainy_days_sum, fill = factor(year))) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label= rainy_days_sum), vjust=0) +
  labs(y= "Number of rainy days", x = "Year", title= "Rainy days by year", fill = "Year") +
  theme(legend.position="none") +
  theme_bw()

# Bar chart kisnih dana po mesecima
rain_month <- clean_data %>% group_by(month) %>%
  summarise(rainy_days_sum = sum(rain))
rain_month$month_name <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug',
                      'Sep', 'Oct', 'Nov', 'Dec')


ggplot(rain_month, aes(x = reorder(month_name, month), y = rainy_days_sum, fill = month_name)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label= rainy_days_sum), vjust=0) +
  labs(y= "Number of rainy days", x = "Month", title= "Rainy days by month", fill = 'Month') +
  theme(legend.position="none") +
  theme_bw()

# Histogram prosecne temperature tokom kisnih i suvih dana

ggplot(clean_data, aes(x = mean_temp_c)) +
  geom_histogram(binwidth = 10, fill = "rosybrown3", colour = "black") + 
  facet_grid(clean_data$rain) +
  labs(y= "Count (days)", x = "Average temperature", title= "Average temperature vs. no rain (0) / rain (1)") +
  theme(legend.position="none") +
  theme_bw()

# Histogram oblacnosti tokom kisnih i suvih dana

ggplot(clean_data, aes(x = cloud_cover)) +
  geom_histogram(binwidth = 10, fill = "lightblue3", colour = "black") + 
  facet_grid(clean_data$rain) +
  labs(y= "Count (days)", x = "Cloud cover", title= "Cloud cover vs. no rain (0) / rain (1)") +
  theme(legend.position="none") +
  theme_bw()

# Histogram vlaznosti tokom kisnih i suvih dana

ggplot(clean_data, aes(x = humidity)) +
  geom_histogram(binwidth = 10, fill = "aquamarine4", colour = "black") + 
  facet_grid(clean_data$rain) +
  labs(y= "Count (days)", x = "Humidity", title= "Humidity vs. no rain (0) / rain (1)") +
  theme(legend.position="none") +
  theme_bw()


# Historgam vazdusnog pritiska tokom kisnih i suvih dana

ggplot(daily_weather_df, aes(x = pressure)) +
  geom_histogram(binwidth = 10, fill = "salmon3", colour = "black") + 
  facet_grid(clean_data$rain) +
  labs(y= "Count (days)", x = "Pressure", title= "Pressure vs. no rain (0) / rain (1)") +
  theme(legend.position="none") +
  theme_bw()

########################
# Decision Tree Models #
########################

#Izbacivanje datuma kao varijable koje nece biti u modelu i transformacija binarne varijable "raine" u kategorijsku varijablu sa dva nivoa (Yes/No) 
ready_data <- clean_data[,-2]
for (i in 1:1827) {
  if (ready_data$rain[i] == 1) {ready_data$rain[i] <- 'Yes'}
  if (ready_data$rain[i] == 0) {ready_data$rain[i] <- 'No'}
}




#Podela seta na deo za treniranje (75%) i deo za testiranje (25%) pomocu stratified partitioning
set.seed(123)

train.indices <- createDataPartition(ready_data$rain,
                                     p = .75, 
                                     list = FALSE)
train.data <- ready_data[train.indices,]
test.data <- ready_data[-train.indices,]

#Provera da li su klase jednako zastupljene u train i test setu
prop.table(table(train.data$rain))
prop.table(table(test.data$rain))


#Pravljenje funkcije evaluaciju performansa drveta
compute.eval.metrics <- function(cmatrix) {
  TP <- cmatrix[1,1] # true positive
  TN <- cmatrix[2,2] # true negative
  FP <- cmatrix[2,1] # false positive
  FN <- cmatrix[1,2] # false negative
  acc <- sum(diag(cmatrix)) / sum(cmatrix)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2*precision*recall / (precision + recall)
  c(accuracy = acc, precision = precision, recall = recall, F1 = F1)
}    

#Pravljenje i testiranje modela tree1 sa niskim / standardnim parametrima kontrole (default: minsplit = 20, cp = 0.01)

tree1 <- rpart(rain ~ ., data = train.data, method = "class")
print(tree1)
fancyRpartPlot(tree1)

tree1.pred <- predict(object = tree1, newdata = test.data, type = "class")

tree1.cm <- table(test.data$rain, tree1.pred)
tree1.cm

tree1.eval <- compute.eval.metrics(tree1.cm)  
tree1.eval

#Pravljenje i testiranje modela tree2 sa smanjenim parametrima kontrole: minsplit = 10, cp = 0.005 


tree2 <- rpart(rain ~ ., data = train.data, method = "class", control = rpart.control(minsplit = 10, cp = 0.005))
print(tree2)
fancyRpartPlot(tree2)

tree2.pred <- predict(object = tree2, newdata = test.data, type = "class")

tree2.cm <- table(test.data$rain, tree2.pred)
tree2.cm

tree2.eval <- compute.eval.metrics(tree2.cm) 
tree2.eval

#Pravljenje dejtafrejma za uporedivanje performansa razlicitih modela

model.performance.comparison <- data.frame(rbind(tree1.eval, tree2.eval),
                                           row.names = c("tree 1", "tree 2"))
model.performance.comparison

##################################################################################

#Sistematsko trazenje parametra cp naoptimalnijeg za preciznost predvidanja modela (Cross-validation)
set.seed(123)

numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp = seq(0.001, to = 0.05, by = 0.001)) 

set.seed(1)
dt.cv <- train(rain ~ .,
               data = train.data,
               method = "rpart",
               control = rpart.control(minsplit = 10),
               trControl = numFolds,
               tuneGrid = cpGrid)
dt.cv
plot(dt.cv)

###################################################################################

#Pravljenje i testiranje modela tree3 sa optimalnom vrednoscu cp = 0.01

tree3 <- prune(tree2, cp = 0.01)
print(tree3)
fancyRpartPlot(tree3)

tree3.pred <- predict(object = tree3, newdata = test.data, type = "class")

tree3.cm <- table(test.data$rain, tree3.pred)
tree3.cm

tree3.eval <- compute.eval.metrics(tree3.cm) 
tree3.eval

###################################################################################

#Pravljenje dejtafrejma za uporedivanje performansa sva 3 modela

model.performance.comparison <- data.frame(rbind(tree1.eval, tree2.eval, tree3.eval),
                                           row.names = c("tree 1", "tree 2", "tree 3"))
model.performance.comparison

#Vizualizacija performansa modela

df_all <- data.frame()


eval_res <- compute.eval.metrics(tree1.cm)

df_tree <- data.frame(eval_res)
df_tree$tree <- "tree1"
df_tree$metric <- row.names(df_tree)
row.names(df_tree) <- NULL
names(df_tree)[1] <- "value"

df_all <- rbind(df_all, df_tree)

eval_res_1 <- compute.eval.metrics(tree2.cm)

df_tree1 <- data.frame(eval_res_1)
df_tree1$tree <- "tree2"
df_tree1$metric <- row.names(df_tree1)
row.names(df_tree1) <- NULL
names(df_tree1)[1] <- "value"


df_all <- rbind(df_all,df_tree1)

eval_res_2 <- compute.eval.metrics(tree3.cm)

df_tree2 <- data.frame(eval_res_2)
df_tree2$tree <- "tree3"
df_tree2$metric <- row.names(df_tree2)
row.names(df_tree2) <- NULL
names(df_tree2)[1] <- "value"

df_all <- rbind(df_all,df_tree2)


df_all$metric <- factor(df_all$metric, levels = c("accuracy","precision","recall","F1"))
df_all$tree <- as.factor(df_all$tree)

ggplot(df_all, 
       aes(x = metric, y = value, fill = tree)) + 
  geom_bar(position = "dodge", stat = "identity") + ylim(0, 1) +
  theme_bw()





