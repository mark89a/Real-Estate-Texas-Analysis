#1 UPLOAD DATASET
realestate_texas=read.csv("realestate_texas.csv", sep=",")  
attach(realestate_texas)
summary( realestate_texas)
library(dplyr)
library(moments)
library(ggplot2)
gini.index=function(x){
  ni=table(x)
  fi=ni/length(x)
  fi2=fi^2 
  j=length(table(x))
  
  gini=1-sum(fi2)
  gini_normalizzato=gini/((j-1)/j)
  return(gini_normalizzato)
}
#__________________________________________________________________________________________________


#3 POSITION INDEXIS

#-MODA

moda_city=table(city)      #variable with 4 modes all with absolute frequency 60 
moda_year= table(year)     #variable with 5 modes all with absolute frequency 48
moda_month=table(month)    #variable with 12 modes all with absolute frequency 20
moda_sales=table(sales)
sum(volume)*1000000

#OTHER POSITION INDEXES
#summary:

#     city                year          month           sales           volume        median_price    listings       months_inventory

#Length:240         Min.   :2010   Min.   : 1.00   Min.   : 79.0   Min.   : 8.166   Min.   : 73800    Min.   : 743   Min.   : 3.400
#Class :character   1st Qu.:2011   1st Qu.: 3.75   1st Qu.:127.0   1st Qu.:17.660   1st Qu.:117300    1st Qu.:1026   1st Qu.: 7.800  
#Mode  :character   Median :2012   Median : 6.50   Median :175.5   Median :27.062   Median :134500    Median :1618   Median : 8.950  
#                   Mean   :2012   Mean   : 6.50   Mean   :192.3   Mean   :31.005   Mean   :132665    Mean   :1738   Mean   : 9.193 
#                   3rd Qu.:2013   3rd Qu.: 9.25   3rd Qu.:247.0   3rd Qu.:40.893   3rd Qu.:150050    3rd Qu.:2056   3rd Qu.:10.950 
#                   Max.   :2014   Max.   :12.00   Max.   :423.0   Max.   :83.547   Max.   :180000    Max.   :3296   Max.   :14.900  

#Skewness and kurtosis
#sales
View(table(sales))#moda is 124....192.3>175.5>124, MEDIA>MEDIANA>MODA, then we know that
#sale's distribution has a positive skewness
skewness(sales)   #0.718  positive asymmetric distribution
kurtosis(sales)   #2.68   leptokurtic distribution

#median_price
View(table(median_price)) # moda is 130.000...132.665<134.500<
skewness(median_price)#-0.364 negative asymmetric distribution
kurtosis(median_price) #2.377 leptokurtic distribution

#volume
View(table(volume)) # moda is 14.003...31.005>27.06>14.003
skewness(volume)    #0.884 positive asymmetric distribution
kurtosis(volume)    #3.17  leptokurtic distribution

#listings
View(table(listings))  # moda is 1.581.....1.738>1.618>1.581
skewness(listings)  #0.649 positive asymmetric distribution
kurtosis(listings)  #2.2 leptokurtic distribution

#months_inventory
View(table(months_inventory)) # moda is 8.1.....9.19>8.95>8.1
skewness(months_inventory)  #0.04  positive asymmetric distribution
kurtosis(months_inventory)  #2.82  leptokurtic distribution

#Gini di city
gini.index(city)         #Gini index for City is 1, therefore there is maximum heterogeneity

#___________________________________________________________________________________________________
#median_price ANALYSIS

#for the variable median_price, I divide in 4 classes to find the class with the highest
#absolute frequency
range_median_price=c( 50000, 100000, 150000, max(median_price))
labels_median_price=c( "50.000 - 100.000", 
                          "100.000 - 150.000", "150.000 - 180.000")
median_price_cl=cut(median_price, breaks = range_median_price, labels = labels_median_price)

table(median_price_cl)    #the modal class is between 100.000 and 150.000, with 154 observations

ggplot(data = realestate_texas)+
  geom_bar(aes(x=median_price_cl),
           stat = "count",
           col="black",
           fill="lightblue")+
  labs(title = "Median price classes distribution",
       x="Median price",
       y="Number of observations")+
  scale_y_continuous(breaks=seq(0, 160, 10))


#relative , cumulative frequences
n=length(median_price)
ni_median_price=table(median_price_cl)
fi_median_price=table(median_price_cl)/n
Ni_median_price=cumsum(ni_median_price)
Fi_median_price=Ni_median_price/n


distr_freq_median_price=as.data.frame(cbind(ni_median_price, fi_median_price, Ni_median_price, Fi_median_price))
distr_freq_median_price
#modal class is from 100.000 to 150.000 euro, with 154 observations
#(64%). This is also the median class
median(median_price)
mean(median_price)   #132.655,4
range(median_price)
range_median_price=max(median_price)-min(median_price) # median_price range is 106.200 dollars
interquartile_range_median_price=IQR(median_price)  #IQR is 32750
var(median_price)   #513.572.983
sd(median_price)    #22.662,15

#Gini index

gini_median_price=gini.index(median_price)
gini_median_price                          # median_price Gini index is 0.99,  
#the variable is very heterogeneous


#______________________________________________________________________________________________________

#AVERAGE SALES PRICE

mean_price=(realestate_texas$volume/realestate_texas$sales)*1000000
realestate_texas$mean_price=mean_price
max(mean_price)
min(mean_price)
range_mean_price=c(0, 100000, 150000, 200000, max(mean_price))
labels_mean_price=c("under 100.000", "100.000 - 150.000", 
                    "150.000 - 200.000", "over 200.000")
mean_price_cl=cut(mean_price, breaks = range_mean_price, labels = labels_mean_price)

table(mean_price_cl)    #modal class is 150.000 - 200.000, with 127 obserations

#relative frequences, cumulative and relative cumulative
n=length(mean_price)
ni_mean_price=table(mean_price_cl)
fi_mean_price=table(mean_price_cl)/n
Ni_mean_price=cumsum(ni_mean_price)
Fi_mean_price=Ni_mean_price/n


distr_freq_mean_price=as.data.frame(cbind(ni_mean_price, fi_mean_price, Ni_mean_price, Fi_mean_price))
distr_freq_mean_price
#modal class is from 150.000 to 200.000 euro, with 127 observations
#( 53%). This is also the median class

median(mean_price)  #156.588,5
mean(mean_price)   #154.320,4
range(mean_price)  #
range_mean_price=max(mean_price)-min(mean_price) # median_price range is  116223.7 dollars
interquartile_range_mean_price=IQR(mean_price)  #IQR= 40.976,22
interquartile_range_mean_price
var(mean_price)   
sd(mean_price)    #27.147,46

#The average sales prices (mean_price) are higher than the median prices (median_price).
#frequency distributions also show higher values in the average price of the classes
#The average price (or arithmetic mean) of sales represents the sum of
#all the values of the "volumes" column divided by the total sales of the "sales" column.
#The median price instead represents the central value of the distribution of sales.
#If the average sales price is much higher than the median price, it means there are some
#"volume" values that are very high compared to most values in the distribution.
#In practice, a few very expensive sales push up the average sales price,
#while a lower median price indicates that the majority of sales are of lower value.
#For example, if a store regularly sold many cheap products, but then sold a
#only luxury car at a very high price, this will change the average of sales,
#but not the median value.


#_____________________________________________________________________________________________________

#DOUBLE FREQUENCY DISTRIBUTION CITY-VOLUMES
city_volume <- aggregate(volume ~ city , data = realestate_texas, sum)
View(city_volume)

#relative frequencies
freq_rel_volume <- transform(city_volume, freq_rel = volume / sum(volume))

#cumulative frequencies
freq_cum_volume <- transform(freq_rel_volume, freq_cum = cumsum(freq_rel))

dev_st_volume<-transform(city_volume, dev_st=sd(volume))
mean_volume<-transform(city_volume, mean_v=mean(volume))

# final dataframe for sales volumes by city
distr_freq_volume <- data.frame(city = city_volume$city, 
                                ni_volume = city_volume$volume*1000000,
                                fi_volume = freq_rel_volume$freq_rel,
                                Ni_volume=cumsum(city_volume$volume*1000000),
                                Fi_volume = freq_cum_volume$freq_cum,
                                dev_st_volume=dev_st_volume$dev_st,
                                mean_volume=mean_volume$mean_v*1000000)
distr_freq_volume
# city                     ni_volume     fi_volume  Ni_volume       Fi_volume
#1 Beaumont               1.567.896.000  0,2107035  1.567.896.000   0.2107035
#2 Bryan-College Station  2.291.496.000  0,3079452  3.859.392.000   0.5186487
#3 Tyler                  2.746.043.000  0,3690300  6.605.435.000   0.8876787
#4 Wichita Falls            835.810.000  0,1123213  7.441.245.000   1.0000000


#the highest sales volumes are in Tyler, with 37% of the total volumes
#the city with the median sales volume is Bryan-College Station
median(volume)*1000000   #27.062.500
mean(volume)*1000000     #31.005.188
sd(volume)
mean(volume)
#___________________________________________________________________________________________________________

#VARIABILITY INDEXES
CV=function(x){
  return(sd(x)/mean(x)*100)
}


CV(sales)            #41.42
CV(volume)           #53.7
CV(median_price)     #17.08
CV(listings)         #43.3
CV(months_inventory) #25.06
CV(mean_price)       #17.59

#the highest CV is for volume (53.7), it means 53.7% of the mean

#_____________________________________________________________________________________________________________________________

#DATA AGGREGATION CONDITIONAL ON CITY AND YEARS
agg_real_estate_volume_year <- realestate_texas %>% 
  group_by(city, year) %>% 
  summarise(sum_volume=sum(volume)*1000000,
            mean_volume = mean(volume)*1000000,
            sd_volume = sd(volume)*1000000)



agg_real_estate_volume <- realestate_texas %>% 
  group_by(city) %>% 
  summarise(sum_volume=sum(volume),
            mean_volume = mean(volume),
            sd_volume = sd(volume),
            min_volume=min(volume),
            max_volume=max(volume),
            median_volume=median(volume))
agg_real_estate_volume



View(table(agg_real_estate_volume))


ggplot(data = realestate_texas, aes(x=city, y=volume, fill=factor(year)))+
  geom_boxplot()+
  scale_y_continuous(breaks = seq(0, 80, 5))+
  scale_fill_brewer(palette = "Set1")+
  labs(title = "Volumi totali delle vendite per città",
       y="Volume vendite in milioni di $")+
  scale_fill_discrete(name="Year")

#From the boxplot we see the sales volumes divided by city and year.
#At a glance you can see from the boxplot that the maximum sales volume
#registered at Bryan-College Station in 2014, with volume exceeding
#the 80 million. The sales volumes are evidently lower
#register in Wichita Falls, where the lowest volume was about 8 million
#and the maximum volume of approximately 21 million
#the interquartile range of sales volumes in Bryan-College Station results
#very large, unlike the one in Wichita Falls, which instead is very large
#restricted. A smaller interquartile range may indicate higher concentration
#of data around the median, i.e. less variation in volumes
#sales compared to other cities. However, this is not always the case and they might
#there are other factors to consider.
#So, if the interquartile range of a city's sales volume
#is much lower than in other cities, it could mean that
#real estate listings in that city have less variation in terms of
#sales volumes compared to other cities.
#A Bryan-College Station is also noted as almost every year the median value
#tends towards the bottom of the distribution, which could mean that the
#most sales have prices much lower than the value
#maximum, but some far more expensive home sales, make sure to
#extend the distribution and range of sales volume
#___________________________________________________________________________________________________________________

#EFFECTIVENESS OF SALES ADVERTISEMENTS

listing_effectivness=(realestate_texas$sales/realestate_texas$listings)
realestate_texas$listing_effectivness=listing_effectivness

agg_listing_effectivness <- realestate_texas %>% 
  group_by(city) %>% 
  summarise(mean_listing_effectivness=mean(listing_effectivness))

agg_listing_effectivness
#city                  mean_listing_effectivness

#1 Beaumont                                 0.106 
#2 Bryan-College Station                    0.147 
#3 Tyler                                    0.0935
#4 Wichita Falls                            0.128 



#dividing the number of sales by the number of active ads, you can get a percentage of the ads
#translate into sales. By grouping this type of data by city, it appears that the ads
#were most effective in the city of Bryan-College Station, with 14.7% effective ads
#the least effective ones were those of Tyler
#_____________________________________________________________________________________________________________________

#PROBABILITY

#PROBABILITY  BEAUMONT
#Considering that the total observations are 240 and the absolute frequency of each city is 60, the
#probability that the city of Beaumont comes out randomly is equal to 60/240m = 25%

probability_beaumont=60/length(city)
probability_beaumont


#PROBABILITY JULY

View(table(month))  #20 observations for each of 12 months
probability_july=20/length(month)
probabilità_july   #0.083  

#The probability that an observation was made in July is 8.33%

#PROBABILITY DECEMBER 2021
View(table(year, month))  #4 observations for each month, 48 every year, for 5 years
probability_dec_2021=4/length(month)
probability_dec_2021    #0.016
#The probability that the observation was made in December 2012 is 1.66%

#_____________________________________________________________________________________________________________________

#OVERLAPPING BARCHART FOR SALES

ggplot(data=realestate_texas, aes(x=month, y=volume ,fill=city))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(~year, scales="free_x") +
  labs(title="Confronto volume per città, anno e mese") +
  xlab("Mese") + ylab("Valore")+
  scale_x_continuous(breaks = seq(0, 12, 1))+
  theme(panel.spacing = unit(1, "cm"),
        legend.position = c(0.85, 0.25),
        legend.direction = "vertical")  


#from the stacked bar graph of sales volumes, it stands out
#immediately notice that sales increased in 2014
#especially at Tyler and Bryan_College Station, after suffering a
#gradual increase over the years since 2010
#Sales volumes in Wichita Falls, however, always remain the same
#lower for each year, while in Beaumont they seem to grow
#over the years but in a more moderate way than Tyler and Bryan
#College Station, which reaches its absolute maximum value in 2014

#NORMALIZED OVERLAPPING BARCHART
ggplot(data=realestate_texas, aes(x=month, y=volume ,fill=city))+
  geom_bar(stat = "identity", position = "fill")+
  facet_wrap(~year, scales="free_x") +
  labs(title="Confronto volume per città, anno e mese") +
  xlab("Mese") + ylab("Valore")+
  scale_x_continuous(breaks = seq(0, 12, 1))+
  theme(panel.spacing = unit(1, "cm"),
        legend.position = c(0.85, 0.25),
        legend.direction = "vertical") 

#The normalized bar graph indicates how the volume value is distributed in percentage
#sales by city, for every year and every month. It is noted that Wichita Falls has the lowest percentage of
#sales volumes, while Tyler has the highest, followed by Bryan-College Station

#_______________________________________________________________________________________________________________________

#LINECHART MEDIAN PRICE



df_means <- realestate_texas %>% 
  group_by(city, year) %>% 
  summarise(m_price = mean(mean_price)) 


ggplot(data = df_means, aes(x = year, y = m_price, group = city, color = city)) + 
  geom_hline(yintercept = mean(realestate_texas$mean_price), linetype="dashed", color="black") +
  geom_line(lwd=1) + 
  geom_point()+
  ggtitle("Prezzi Medi di Vendita per Città, Anno") + 
  xlab("Anno") + 
  ylab("Prezzo Medio di Vendita")+
  scale_y_continuous(breaks=seq(0, 300000, 10000), labels=function(x) format(x, big.mark = "."))


#the line graph represents the trend of the average sales price for each year and for each city
#we see that Wichita Falls has the lowest average sales price, which also justifies the fact
#which also has the lowest sales volume. Bryan-College Station on the other hand has the highest average prices
#high compared to other cities, and you can see how prices are increasing from year to year until
#reach the maximum price recorded in 2014.











