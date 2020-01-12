
library(tidyr)

Data <- read.csv(file.choose(),header=TRUE)
summary(Data)
str(Data)

# which one from the neighbourhood group variables should we choose?
identical(Data$neighbourhood_group,Data$neighbourhood_group_cleansed)


# filter to Manhattan only 
Manhattan <- Data[Data$neighbourhood_group == "Manhattan",]

# take a look at the variables 
str(Manhattan)   


# Inspection of similar variables 

   # which listings count variable to choose?
identical(Data$host_listings_count, Data$host_total_listings_count)  # cheack if variables are identical


   # which neighbourhood variable to choose?
identical(Data$neighbourhood,Data$neighbourhood_cleansed)  

length(unique(Manhattan$neighbourhood))  # count unique
length(unique(Manhattan$neighbourhood_cleansed))

unique(Manhattan$neighbourhood_cleansed)                          
unique(Manhattan$neighbourhood)    # this has "Manhattan" and "The Bronx" as neighbourhoods. Thus, the cleaned varibale will be used instead.


# keep variables of interst only (loads faster in QGIS)

vars <- c("id", "host_id","host_since","host_is_superhost","host_listings_count", 
          "neighbourhood_cleansed","latitude", "longitude", "zipcode",
          "room_type", "price", "minimum_nights", 
          "number_of_reviews", "first_review", "last_review")

Manhattan <- Manhattan[vars]


# Check the number of listings with reviews
sum(Manhattan$number_of_reviews != "0")

# Check the maximum number of listings per host 
sum(is.na(Manhattan$host_listings_count))

Manhattan <- Manhattan %>%
  drop_na(host_listings_count)               # remove NA raws 

max(Manhattan$host_listings_count)

# filter the data to include only listings with reviews and maximum of 4 listing per host. 
sum(Manhattan$host_listings_count <= "4" & Manhattan$number_of_reviews != "0")
Manhattan <- Manhattan[Manhattan$host_listings_count <= "4" & Manhattan$number_of_reviews != "0" , ]


# export the file 
write.csv(Manhattan, file = "Manhattan_clean.csv",row.names=FALSE)



###################### Manipulate Data structure #####################


# New data frame with same data 

#Manhattan2 <- Manhattan
Manhattan2 <- read.csv(file.choose(),header=TRUE)


# Creat 2 columns of first and last reviw year 

str(Manhattan2)   # first and last review column are "Factor" with  format  %Y/%m/%d 

Manhattan2$first_year <- as.numeric(substring(Manhattan2$first_review,1,4))    #keep the characters 1-4  and save to new column
Manhattan2$last_year <- as.numeric(substring(Manhattan2$last_review,1,4))

# Check the data
View(Manhattan2)

# Find the year of the first review in all data to identify the time frame
min(Manhattan2$first_year)
max(Manhattan2$last_year)     #the time frame is 2009 - 2019


# Lifetime of  listings (number of years active)
Manhattan2$lifetime  <- Manhattan2$last_year - Manhattan2$first_year

sum(Manhattan2$lifetime == "0")   # how many lasted / has been active less than a year 
hist(Manhattan2$lifetime)


# Create binary column for each year in the period |2009 - 2019| =  11 columns 

#2009
Manhattan2$Y_2009 <- as.numeric(ifelse(Manhattan2$first_year == "2009","1","0"))

#2010 - 2019
Manhattan2$Y_2010 <- as.numeric(ifelse(Manhattan2$first_year <= "2010" & Manhattan2$last_year >= "2010","1","0"))
Manhattan2$Y_2011 <- as.numeric(ifelse(Manhattan2$first_year <= "2011" & Manhattan2$last_year >= "2011","1","0"))
Manhattan2$Y_2012 <- as.numeric(ifelse(Manhattan2$first_year <= "2012" & Manhattan2$last_year >= "2012","1","0"))
Manhattan2$Y_2013 <- as.numeric(ifelse(Manhattan2$first_year <= "2013" & Manhattan2$last_year >= "2013","1","0"))
Manhattan2$Y_2014 <- as.numeric(ifelse(Manhattan2$first_year <= "2014" & Manhattan2$last_year >= "2014","1","0"))
Manhattan2$Y_2015 <- as.numeric(ifelse(Manhattan2$first_year <= "2015" & Manhattan2$last_year >= "2015","1","0"))
Manhattan2$Y_2016 <- as.numeric(ifelse(Manhattan2$first_year <= "2016" & Manhattan2$last_year >= "2016","1","0"))
Manhattan2$Y_2017 <- as.numeric(ifelse(Manhattan2$first_year <= "2017" & Manhattan2$last_year >= "2017","1","0"))
Manhattan2$Y_2018 <- as.numeric(ifelse(Manhattan2$first_year <= "2018" & Manhattan2$last_year >= "2018","1","0"))
Manhattan2$Y_2019 <- as.numeric(ifelse(Manhattan2$first_year <= "2019" & Manhattan2$last_year >= "2019","1","0"))

# Check the columns
head(Manhattan2[,19:29])
str(Manhattan2)


# Sum the binary rows and compare to lifetime _ it should be = liftime + 1
Manhattan2$totalyears <- rowSums(Manhattan2[,19:29])

min(Manhattan2$totalyears)
max(Manhattan2$totalyears)

identical(Manhattan2$totalyears, Manhattan2$lifetime + 1)


hist(Manhattan2$totalyears)
plot(colSums(Manhattan2[,19:29]))


# Add price column without dollar signs
Manhattan2$price2 <- as.numeric(gsub("[^0-9.]", "", Manhattan2$price))  # remove anything that is not digit or a decimal

sum(is.na(Manhattan2$price2))   # make sure no value are lost 

class(Manhattan2$price2)
hist(Manhattan2$price2)


# export the file 
write.csv(Manhattan2, file = "Manhattan_reconstructed_2.csv",row.names=FALSE)


