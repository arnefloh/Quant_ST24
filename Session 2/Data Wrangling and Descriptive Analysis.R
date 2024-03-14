### Load Packages ----
library(tidyverse)


### Load Data ----
# object <- function (argument1, argument2,...)
# short cut assign operator: ALT + "-"
# loads csv file and save it as "my.data"
my.data <- read_csv("Data Wrangling and Descriptive Statistics/airbnb_vienna.csv")


### Explore Data ----
view(my.data)

head(my.data)
tail(my.data)
glimpse(my.data)
str(my.data)


### Clean Data ----
#### Change Class of Variable "neighbourhood" ----
my.data <- my.data |> 
            mutate (neighbourhood_fct = as_factor(neighbourhood))


#my.data$price_new <- na_if(my.data$price, 0)

#my.data$price_new2 <- if_else(my.data$price_new < 1000, my.data$price_new, NA_real_)

#my.data$price_night <- my.data %>% mutate(price_night = price/minimum_nights)


### Descriptive Statistics ----

#### Descriptive Statistics: Base R ----

##### Base R: Categorical variable ----

table(my.data$neighbourhood)

##### Base R: Metric Variable ----

#mean(dataname$variablename)
mean(my.data$price)
mean(my.data$price, trim = 0.05)

#median(dataname$variablename)
median(my.data$price)

#range(dataname$variablename)
range(my.data$price)

#variance(dataname$variablename)
var(my.data$price)

#standard deviation(dataname$variablename)
sd(my.data$price)



#### Descriptive Statistics: Tidyverse ----

##### Tidyverse: Categorical variable ----

my.data |> count(neighbourhood_fct)

##### Tidyverse: Metric Variable ----

my.data |> summarise(price_av = mean(price))


my.data |> summarise (price_av = mean(price),
                    price_av_trim = mean(price, trim = 0.05),    
                    price_med = median(price),
                    price_max = max(price),
                    price_min = min(price),
                    price_var = var(price),
                    price_sd = sd(price))


my.data |>  group_by(neighbourhood) %>% summarise(price_median = median(price)) %>% print(n = 33)


my.data |>  group_by(neighbourhood) %>% summarise(price_median = median(price)) %>% arrange (price_median) |> print(n = 33)

my.data |>  group_by(neighbourhood) %>% summarise(price_median = median(price)) %>% arrange (desc(price_median)) |> print(n = 33)

my.data  |>  group_by(neighbourhood) %>% summarise(price_max = max(price)) %>% print(n = 33)


my.data |> group_by(neighbourhood_fct)|> 
           summarise (price_av = mean(price),
                      price_av_trim = mean(price, trim = 0.05),    
                      price_med = median(price),
                      price_max = max(price),
                      price_min = min(price),
                      price_var = var(price),
                      price_sd = sd(price))



### Visualization of Descriptive Results----

#### Base R ----

plot(my.data$neighbourhood_fct)
hist(my.data$price)


#### ggplot ----
my.data |> ggplot (aes (y = neighbourhood_fct)) +
           geom_bar()

my.data |> ggplot (aes (y = price)) + geom_boxplot()




### Excercise
# Go to http://insideairbnb.com/get-the-data.html and download airbnb data 
# from your next holiday destination and search for the cheapest areas 
# use command min


### Hypothesis Testing
# Is the price correlated with the number of reviews?

plot(my.data$price_new2, my.data$number_of_reviews)

ggplot(data = my.data, mapping = aes(x = price_new2, y = number_of_reviews)) +
  geom_point()


cor.test(my.data$price_new2, my.data$number_of_reviews)


### Save New Data
write_csv(my.data, file = "Data/airbnb_london_new.csv")
