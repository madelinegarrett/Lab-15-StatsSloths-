---
title: "Lab-15-StatisticalSloths"
output: html_document
---
Madeline Garrett, Zandy Boone, Kevin Luth, Katie Stewart

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

![ ](avocado.png)

```{r, include = FALSE}
library(tidyverse)
library(lubridate)
library(modelr)

Adf <- read_csv("avocado.csv")
Adf$X1 <- NULL
Adf$"XLarge Bags" <- NULL
Adf$"4046" <- NULL
Adf$"4225" <- NULL
names(Adf)[3]<-"Vol"
Adf$"4770" <- NULL

Adata <- Adf %>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-", convert = TRUE)

```

# Have avocado prices increased since 2015? Is is different for Conventional and Organic? 

### Domain Expert 
* The Domain Expert for this project is a Millennial Magazine that wants to publish an article about the increased price of avocados and what that means for avocado toast. They are interested in knowing if avocado prices have increased since 2015 and if they have increased for both conventional and organic. They then want to use our findings to help support their articles. We will support our answer and go above and beyond by also answering these subqestions: ****ENTER SUB QUESTIONS HERE*****

### Question Background
* This question is interesting because it can tell and predict how avocado prices will increase, this is interesting for almost everyone who likes avocados. It is also important information for shop owners and supermarket owners to know if avocado prices were increasing. 

## Data 
* The data we are using is avocado data from 2015 to March 2018. https://www.kaggle.com//avocado-prices 
* This is a graph of Average Avocado Prices over time. It is seperated for both Convetional and Organic

# Findings 
```{r}
Adata
Atime <- Adata %>% 
    group_by(Year, Month, type) %>% 
    select(Year, Month, type, AveragePrice) %>%
    summarise(averagePrice = mean(AveragePrice))

ggplot(data= Atime, aes(x=Month, y=averagePrice, color=as.factor(Year)))+
    geom_line() + 
    facet_grid( ~Atime$type)+
    scale_color_manual(values=c("darkgreen", "yellowgreen",  "palegreen3", "yellow2"))
```



* This is a graph of Avocado Prices distributed over each year. 
```{r}
ggplot(data = Adata) + 
  stat_bin(mapping = aes(x = AveragePrice, fill = as.factor(year)), color = "black",bins =  55) +
  facet_wrap( ~Adata$year) +
  scale_fill_manual(values=c("darkgreen", "yellowgreen",  "palegreen3", "olivedrab2"))
```


This graph of the distribution of Avocado Prices for Conventional and Organic. 
```{r}
ggplot(data = Adata) + 
  stat_bin(mapping = aes(x = AveragePrice, fill = type), color = "darkBlue" ,bins =  55) +
  facet_wrap( ~Adata$type)+
  scale_fill_manual(values=c("darkgreen", "yellowgreen"))
```



This is a graph of avocado prices over time. 
```{r}
Adf$Date <- as.Date(Adf$Date, "%Y-%m-%d")

Price <- Adf %>% select(Date, AveragePrice) %>%
ggplot(aes(x=Date, y=AveragePrice, color = "darkgreen" )) + 
  geom_smooth() +
  scale_color_manual(values=c("darkgreen"))

Price
```

```{r}
Price2015 <- Adata %>%
  filter(Year == 2015)
mean(Price2015[["AveragePrice"]])


Price2017 <- Adata %>%
  filter(Year == 2017)
mean(Price2017[["AveragePrice"]])

1.515128- 1.37559 
# Mean difference is-0.139538
```


```{r, include = FALSE}
perm_mean <- function(perms = 1000, values, n1)
{
  ## Variables ##
  # perms: The number of permutations 
  # values (num): 
  # n1 (int): Size of group 1
  ###############
  
  # Step 1:
  # Create vector of zeroes of length "perms" to store
  # permuted mean differnces
  means <- vector(mode = "double", length = perms)
  
  # Loop throught number of permutations
  for (i in c(1:perms))
  {
    # Step 2
    # Shuffle them using smample
    # Randomly separate vector "values" into disjoint 
    # groups of size "n1" and "length(values) - n1" respectively
    group_one <- sample(values, n1)
    group_two <- sample(values, length(values)-n1)
    
    # Step 3:
    # Compute the sample means for the two groups from
    g1mean <- mean(group_one)
    g2mean <- mean(group_two)
    
    # Step 4: 
    # Compute the difference in sample means, store the
    # value in the vector from step 1
    diff <- g1mean - g2mean
    means[i] <- diff
  }
  
  # Step 5:
  # Return new updated vector, created in step 1
  means
  
}

```

```{r}
preBabies <- Adata %>%
  filter(Year == 2015 | Year == 2017)
mean_vals <- perm_mean(1000, preBabies$AveragePrice, 500)
mean_data <- tibble(mean_vals)
```

```{r}
ggplot(data = mean_data) +
  geom_histogram(mapping = aes(x = mean_vals), color = "palegreen4", binwidth = .002)+ 
   geom_vline(xintercept =0.139538, col=c("darkgreen")) +
  ggtitle("")
```

### Conclusions 
* We can conclude that avocado prices have increased since 2015. Specifically there has been a spike in prices sicne 2017. Prices also tend to increase the most during certain months of the year. Our permutation test supports that fact showing that the value that we got from our data is not very common, since it lies very far outside of the realm of the rest of the distibution. We also see that prices for organic and for conventional are very different but more or less increase and decrease at the same rate. 



# SubQuestions

## Where is the total volume of avocados the highest? 
Madeline's Subquestion 

* Importance: 

This question is important because the price could be increasing/decreasing becase there are more/less avocados in the market. Knowing the answer to this question will help us to be able to fully know whether or not avocado prices have raised. This question also raises an important look in to how the demand for avocados has increased in recent years.  

* New Tools: 

I used the lm and add predictions functions to help me to make this graph. These added predictions to my data and made it easier to see. I also used data_grid to help me when graphing the data. 

* Conclusion: 

I conclude that South Central has the highest volume of avocados in the country in comparison to the other regions, with California close behind. This relates to the overall question because we know that there is a supply and demand so areas that are growing large amounts of avocados are doing so because of such a high demand. 

```{r}
#View(Adata)
Areg <- Adata %>%
  filter(region == "TotalUS" | region == "Southeast" | region == "Northeast" | region == "Midsouth" | region == "SouthCentral" | region == "Plains" | region == "California")

mod <- lm(Vol ~ region, data = Areg)
grid <- Areg %>% 
  data_grid(region) %>% 
  add_predictions(mod, "Vol")

bp <- ggplot(Areg, aes(region, Vol, color = region)) + 
  geom_boxplot() +
  geom_point(data = grid, colour = "darkgreen", size = 4)

p <- bp+ coord_flip() +
  ggtitle("Volumes and Predictions in Regions Across the US")
p

```


## How does the number total bags vary between regions?
Katie's Section
```{r}
colnames(Adata)[5] <- "totalbags"
bags <- lm(totalbags ~ region, data = Adata)
plot <- Adata %>%
  data_grid(region) %>%
  add_predictions(bags, "Bags")
```


## Does location matter when looking at price changes for avocados?
Kevin's Subquestion
* Importance: This question helps us to answer our main question because it can tell us whether or not the price change was consistent throughout the country or if there were different changes based on the region.
* Tools: I used a permutation test to see whether the regional labels matter when looking at average price changes in avocados from 2015 to 2017.
* Conclusion: The results of my permutation test show that the original mean difference between avocado prices in 2015 to 2017 is rare compared the sampled mean differences. This means that the regions do matter when looking at price differences which indicates that while the price may have increased overall in the country, individual regions likely had differing price changes.
```{r}
orig15 <- avocados %>% #1.38
  filter(year == 2015) %>%
  summarise(mean(AveragePrice))
orig17 <- avocados %>% #1.52
  filter(year == 2017) %>%
  summarise(mean(AveragePrice))
orig_diff <- 1.52 - 1.38

perm_mean <- function(perms = 1000, values, n1)
{
  ## Variables ##
  # perms: The number of permutations 
  # values (num): 
  # n1 (int): Size of group 1
  ###############
  
  # Step 1:
  # Create vector of zeroes of length "perms" to store
  # permuted mean differnces
  means <- vector(mode = "double", length = perms)
  
  # Loop throught number of permutations
  for (i in c(1:perms))
  {
    # Step 2
    # Shuffle them using smample
    # Randomly separate vector "values" into disjoint 
    # groups of size "n1" and "length(values) - n1" respectively
    # group_one <- sample(values, n1)
    # group_two <- sample(values, length(values)-n1)
    sampled <- sample(values)
    group_one <- sampled[1:n1]
    group_two <- sampled[n1:length(values)]
    
    # Step 3:
    # Compute the sample means for the two groups from
    g1mean <- mean(group_one)
    g2mean <- mean(group_two)
    
    # Step 4: 
    # Compute the difference in sample means, store the
    # value in the vector from step 1
    diff <- g1mean - g2mean
    means[i] <- diff
  }
  
  # Step 5:
  # Return new updated vector, created in step 1
  means
}

values <- perm_mean(1000, avocados$AveragePrice, 54)
mean_data <- data_frame(values)
ggplot(data = mean_data) +
  geom_histogram(mapping = aes(x = values), binwidth = .005, fill = "dark green") +
  geom_vline(xintercept = orig_diff, color = "blue") +
  ggtitle("Distribution of Mean Differences for Average Price") +
  labs(x = "Price Change")
```
## is there a difference over time of organic vs conventional avocados sold and how does it differe in region?
Zandy's subquestion
* Importance: This question is important because it can help determine which regions more  covential or organic avocados are sold/ where people like to eat those avocados and then determine if prices in those regions have changed for avocados between the two types.


```{r}
filtered_Adata <- Adata %>%
  select(1, 6, 9,  11)

filtered_2015_Adata_conventional <- filtered_Adata %>%
  filter(Year == 2015 & type == "conventional") %>%
  group_by(region) %>%
  summarise(sum(`Total Bags`))


filtered_2015_Adata_organic <- filtered_Adata %>%
  filter(Year == 2015 & type == "organic") %>%
  group_by(region) %>%
  summarise(sum(`Total Bags`)) 


filtered_2016_Adata_conventional <- filtered_Adata %>%
  filter(Year == 2016 & type == "conventional") %>%
  group_by(region) %>%
  summarise(sum(`Total Bags`))


filtered_2016_Adata_organic <- filtered_Adata %>%
  filter(Year == 2016 & type == "organic") %>%
  group_by(region) %>%
  summarise(sum(`Total Bags`)) 


filtered_2017_Adata_conventional <- filtered_Adata %>%
  filter(Year == 2017 & type == "conventional") %>%
  group_by(region) %>%
  summarise(sum(`Total Bags`))


filtered_2017_Adata_organic <- filtered_Adata %>%
  filter(Year == 2017 & type == "organic") %>%
  group_by(region) %>%
  summarise(sum(`Total Bags`)) 


filtered_2018_Adata_conventional <- filtered_Adata %>%
  filter(Year == 2018 & type == "conventional") %>%
  group_by(region) %>%
  summarise(sum(`Total Bags`))


filtered_2018_Adata_organic <- filtered_Adata %>%
  filter(Year == 2018 & type == "organic") %>%
  group_by(region) %>%
  summarise(sum(`Total Bags`)) 

mean_2015_data_conventional <- filtered_Adata %>%
  filter(Year == 2015 & type == "conventional") %>%
  group_by(region) %>%
  summarise(mean(`Total Bags`))

mean_2015_data_organic <- filtered_Adata %>%
  filter(Year == 2015 & type == "organic") %>%
  group_by(region) %>%
  summarise(mean(`Total Bags`))

mean_2016_data_organic <- filtered_Adata %>%
  filter(Year == 2016 & type == "organic") %>%
  group_by(region) %>%
  summarise(mean(`Total Bags`))


mean_2016_data_conventional <- filtered_Adata %>%
  filter(Year == 2016 & type == "conventional") %>%
  group_by(region) %>%
  summarise(mean(`Total Bags`))



mean_2017_data_conventional <- filtered_Adata %>%
  filter(Year == 2017 & type == "conventional") %>%
  group_by(region) %>%
  summarise(mean(`Total Bags`))



mean_2017_data_organic <- filtered_Adata %>%
  filter(Year == 2017 & type == "organic") %>%
  group_by(region) %>%
  summarise(mean(`Total Bags`))



mean_2018_data_conventional <- filtered_Adata %>%
  filter(Year == 2018 & type == "conventional") %>%
  group_by(region) %>%
  summarise(mean(`Total Bags`))



mean_2018_data_organic <- filtered_Adata %>%
  filter(Year == 2018 & type == "organic") %>%
  group_by(region) %>%
  summarise(mean(`Total Bags`))


colnames(filtered_2015_Adata_conventional)[2] <- "totalbags"


mod_2015_conentional <- lm(totalbags ~ region, data = filtered_2015_Adata_conventional)
grid_1 <- filtered_Adata %>% 
  data_grid(region) %>% 
  add_predictions(mod, "Bags")

boxplot_1 <- ggplot(filtered_2015_Adata_conventional, aes(region, totalbags, color = region)) +
  geom_boxplot() +
  geom_point(data = grid_1, color = "red", size = 3)


colnames(filtered_2015_Adata_organic)[2] <- "totalbags"


mod_2015_organic <- lm(totalbags ~ region, data = filtered_2015_Adata_organic)
grid_1 <- filtered_Adata %>% 
  data_grid(region) %>% 
  add_predictions(mod, "Bags")


boxplot_2 <- ggplot(filtered_2015_Adata_organic, aes(region, totalbags, color = region)) +
  geom_boxplot() +
  geom_point(data = grid_1, color = "red", size = 3)


colnames(filtered_2016_Adata_conventional)[2] <- "totalbags"


mod_2016_conentional <- lm(totalbags ~ region, data = filtered_2016_Adata_conventional)
grid_1 <- filtered_Adata %>% 
  data_grid(region) %>% 
  add_predictions(mod, "Bags")

boxplot_3 <- ggplot(filtered_2016_Adata_conventional, aes(region, totalbags, color = region)) +
  geom_boxplot() +
  geom_point(data = grid_1, color = "red", size = 3)


colnames(filtered_2016_Adata_organic)[2] <- "totalbags"


mod_2016_organic <- lm(totalbags ~ region, data = filtered_2016_Adata_organic)
grid_1 <- filtered_Adata %>% 
  data_grid(region) %>% 
  add_predictions(mod, "Bags")


boxplot_4 <- ggplot(filtered_2016_Adata_organic, aes(region, totalbags, color = region)) +
  geom_boxplot() +
  geom_point(data = grid_1, color = "red", size = 3)


colnames(filtered_2017_Adata_conventional)[2] <- "totalbags"


mod_2017_conentional <- lm(totalbags ~ region, data = filtered_2017_Adata_conventional)
grid_1 <- filtered_Adata %>% 
  data_grid(region) %>% 
  add_predictions(mod, "Bags")

boxplot_5 <- ggplot(filtered_2017_Adata_conventional, aes(region, totalbags, color = region)) +
  geom_boxplot() +
  geom_point(data = grid_1, color = "red", size = 3)


colnames(filtered_2017_Adata_organic)[2] <- "totalbags"


mod_2017_organic <- lm(totalbags ~ region, data = filtered_2017_Adata_organic)
grid_1 <- filtered_Adata %>% 
  data_grid(region) %>% 
  add_predictions(mod, "Bags")


boxplot_6 <- ggplot(filtered_2017_Adata_organic, aes(region, totalbags, color = region)) +
  geom_boxplot() +
  geom_point(data = grid_1, color = "red", size = 3)


colnames(filtered_2018_Adata_conventional)[2] <- "totalbags"


mod_2018_conentional <- lm(totalbags ~ region, data = filtered_2018_Adata_conventional)
grid_1 <- filtered_Adata %>% 
  data_grid(region) %>% 
  add_predictions(mod, "Bags")

boxplot_7 <- ggplot(filtered_2018_Adata_conventional, aes(region, totalbags, color = region)) +
  geom_boxplot() +
  geom_point(data = grid_1, color = "red", size = 3)


colnames(filtered_2018_Adata_organic)[2] <- "totalbags"


mod_2018_organic <- lm(totalbags ~ region, data = filtered_2018_Adata_organic)
grid_1 <- filtered_Adata %>% 
  data_grid(region) %>% 
  add_predictions(mod, "Bags")


boxplot_8 <- ggplot(filtered_2018_Adata_organic, aes(region, totalbags, color = region)) +
  geom_boxplot() +
  geom_point(data = grid_1, color = "red", size = 3)
```
## Reflections
Lab 2 Team Goal:
* To learn how to code in R and to work together well as a team by communicating, addressing problems, and working hard.
* We feel that our team has achieved our goal because we were able to complete all labs and tRATS as a team. We all individualy brought something to our team to make us stronger. We communicated well and worked hard to get things done.

Keep Doing:
* Keep starting the labs early. It's better to have more time to submit in case you run into submission errors.

Stop Doing: 
* Stop tring to knit and submit labs at the last minute to avoid computer crashes right when it's due.

Start Doing:
* Start doing review sessions with the team to ensure everyone gets high iRAT and tRAT scores.



### Individual Reflections
Kevin Luth: 
* 6 Months: Six months after graduating I would like to be working in the sports industry in some capacity.
* 5 Years: Five years after graduating I would like to be working for a professional sports team using data science to help with roster building. My ideal sport to work with would be football, but I would enjoy working with basketball as well.
* Above were the goals I had from lab 2. My goals have not really changed much from the start of this course to now. I would actually say I am even more sure of them now than before because I have been able to see different ways I could use data science techniques in sports. Before I did not have any concrete ideas of how to use data science in sports but now I can see how some of the tools we learned this semester can be put towards helping a team succeed. I learned a lot in this course as I had no experience with any of the things we did coming in, but now I feel like I at least have a grasp on the concepts we used and have a good basis moving forward in data science. Some advice I would give myself would be to keep doing the labs earlier in the week, start trying to understand why a certain tool might be used in different contexts rather than just because the instructions say to, and stop reading the chapters at the last minute so I have more time to comprehend them.

Katie Stewart:
* 6 Months: Six months after graduation I want to be traveling while working on app development. 
* 5 years: Five years after graduation I want to be working as a data scientist as well as have a fully working a developed app available to the world. 
* Above were the goals I had at the beginning of this semester. My goals have not changed much since then but I am more interested in applying statistics to more things I do in my life and I can do that with the knowledge from this course. When I came into this course I had some programming experience and just a little bit of r experience. Looking back at myself then I truly knew nothing about r. I have learned so much about this program and I know there is still much more to learn. If I could give myself advice I would say to try more of the reading exercises and work harder to learn the concepts. There is a lot to learn in only a few months and spending more time to learn will be helpful in the long run. Keep collaborating with your team. They really are there to help generate ideas and communication is key. Stop pushing off the readings until too late. Make sure to give yourself plenty of time. Start doing more exercises and pushing yourself to learn more.


