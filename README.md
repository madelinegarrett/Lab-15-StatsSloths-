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
* The Domain Expert for this project is a Millennial Magazine that wants to publish an article about the increased price of avacados and what that means for avodcado toast. They are interested in knowing if avacado prices have increased since 2015 and if they have increased for both conventional and organic. They then want to use our findings to help support their articles. We will support our answer and go above and beyond by also answering these subqestions: ****ENTER SUB QUESTIONS HERE*****

### Question Background
* This question is interesting because it can tell and predict how avacado prices will increase, this is interesting for almost everyone who likes avocados. It is also important information for shop owners and supermarket owners to know if avacado prices were increasing. 

## Data 
* The data we are using is avacado data from 2015 to March 2018. https://www.kaggle.com//avocado-prices 
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



* This is a graph of Avacado Prices distributed over each year. 
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



This is a graph of avacado prices over time. 
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
* We can coclude that avacado prices have increased since 2015. Specifically there has been a spike in prices sicne 2017. Prices also tend to increase the most during certain months of the year. Our permutation test supports that fact showing that the value that we got from our data is not very common, since it lies very far outside of the realm of the rest of the distibution. We also see that prices for organic and for conventional are very different but more or less increase and decrease at the same rate. 



# SubQuestions

## Where is the total volume of avocados the highest? 
Madeline's Subquestion 

* Importance: 

This question is important because the price could be increasing/decreasing becase there are more/less avocados in the market. Knowing the answer to this question will help us to be able to fully know whether or not avacado prices have raised. This question also raises an important look in to how the demand for avocados has increased in recent years.  

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


## Has the region with the most volume of avocados changed since 2015?
Katie's Section

## Does location matter when looking at price changes for avocados? For conventional vs organic?
Kevin's Subquestion
* Importance: This question helps us to answer our main question because it can tell us whether or not the price change was consistent throughout the country or if there were different changes based on the region.
* Tools: I used a permutation test to see whether the regional labels matter when looking at average price changes in avocados from 2015 to 2017.
* Conclusion: The results of my permutation test show that the original mean difference between avocado prices in 2015 to 2017 is rare compared the sampled mean differences. This means that the regions do matter when looking at price differences which indicates that while the price may have increased overall in the country, individual regions likely had differing price changes.


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
