# Lab-15-StatsSloths-



```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
# Import and Tidy the Data
library(tidyverse)
library(lubridate)
Adf <- read_csv("avocado.csv")
Adf$X1 <- NULL
Adf$"XLarge Bags" <- NULL
Adf$"4046" <- NULL
Adf$"4225" <- NULL
Adf$"4770" <- NULL

Adata <- Adf %>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-", convert = TRUE)

```

# Have avacado prices increased for both Conventional and Organic? 

```{r}
Atime <- Adata %>% 
    group_by(Year, Month, type) %>% 
    select(Year, Month, type, AveragePrice) %>%
    summarise(averagePrice = mean(AveragePrice))

ggplot(data= Atime, aes(x=Month, y=averagePrice, color=as.factor(Year)))+
    geom_line() + 
    facet_grid( ~Atime$type)+
    scale_color_manual(values=c("darkgreen", "yellowgreen",  "palegreen3", "yellow2"))
```

```{r}
ggplot(data = Adata) + 
  stat_bin(mapping = aes(x = AveragePrice, fill = as.factor(year)), color = "black",bins =  55) +
  facet_wrap( ~Adata$year) +
  scale_fill_manual(values=c("darkgreen", "yellowgreen",  "palegreen3", "olivedrab2"))
```
```{r}
ggplot(data = Adata) + 
  stat_bin(mapping = aes(x = AveragePrice, fill = type), color = "darkBlue" ,bins =  55) +
  facet_wrap( ~Adata$type)+
  scale_fill_manual(values=c("darkgreen", "yellowgreen"))
```




```{r}
Adf$Date <- as.Date(Adf$Date, "%Y-%m-%d")

price_trend <- Adf %>% select(Date, AveragePrice, type) %>%
ggplot(aes(x=Date, y=AveragePrice)) + 
  geom_line(aes(color=type, fill=type), position = position_dodge(0.8)) +
  scale_fill_manual(values=c("darkgreen", "yellowgreen"))

price_trend
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


# SubQuestions 
