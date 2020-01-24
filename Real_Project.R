# Seting Directory
setwd("C:/Users/HOME10/Desktop/Project")

# Loading important libraries
library(plotly)
library(dplyr)
library(ggplot2)
library(ISLR)
library(formattable)
library(doBy)
library(sqldf)
library(gganimate)
library(htmltools)
library(webshot)
library(devtools)
library(tidyr)
library(data.table)


#------------colour customization---------
customGreen0 = "#DeF7E9"

customGreen = "#71CA97"

customRed = "#ff7f7f"
custombluemagnate="#544f9a"
custombluemagnate0="#69638c"
customsnow="#fffafa"

# reading data
df<-read.csv('master.csv')

# To show the first few rows
head(df)

# To show the data structure
str(df)

#To show the number of column
length(df)

# To show number of rows
length(df[,1])

#country is wrongly spelt so let's correct it
colnames(df)[which(names(df) == "ï..country")] <- "country"

# Filter out suicide only data
clean_df <- df %>% 
  filter(suicides_no != "NA" & suicides_no!=0) 

#To show summarry of data

summary(clean_df)
# Transform year from int to factor
clean_df[,"year"] <- as.factor(clean_df[,"year"])

# check number of unique variables in each column
sapply(clean_df, function(x) length(unique(x)))


# Check the unique values of year
levels(clean_df$year)

#check unique value of country
levels(clean_df$country)

# Aggregate the suicide by country
suicide_stat_country <- aggregate(. ~country+year, data=clean_df, sum, na.rm=TRUE)

# Select some variables
selected_df <- clean_df %>% select(country, year, suicides_no, population)

# transfor year to numeric
selected_df$year <- as.numeric(as.character(selected_df$year)) 

#Check for missing values
colSums(is.na(clean_df)) # Identify the features with missing values

# drop HDI.for.year
clean_df$HDI.for.year <- NULL

# Check for outliers
clean_df$Suicide_Rate = (clean_df$suicides_no/clean_df$population)*100000 #create new variable

clean_df %>% boxplot(Suicide_Rate ~ year, data = ., main="Boxplot of Suicide Rate", ylab = "Suicide Rate per 100,000", xlab = "Year", col="lightblue")
grid()

# Aggregating suicide per year and country
total_suicides <- aggregate(suicides_no~country+year,clean_df,sum)

# Aggregating suicide per country
total_suicides_country <- aggregate(suicides_no~country,clean_df,sum)

# Suicides per year in Canada
canada <- clean_df[clean_df$country == 'Canada',]   # selecting only Canada
canada_suicides <- aggregate(suicides_no~year, canada, sum)
#
canada_gdp<- aggregate(suicides_no~gdp_for_year...., canada, sum)


#To know if there is a significant difference in suicide rate across different years

suicides_year <- aggregate(suicides_no~year,clean_df,sum)


#Highlighting  some rows
formattable=formattable(clean_df,
                        align=c("l","c","c","c","c","c","c","c","c","c","c","c"),
                        list(`country` = formatter(
                          "span", style = ~ style(color = "blue",font.weight = "bold")
                        )))

#-----------------------------------------------------------------------------------
tab_reduce = clean_df[, c(1, 2, 3, 4, 6, 5, 7, 9, 10, 11,12)] 

#------------------------------------------------------------------------------
widget_formattable = formattable(tab_reduce) 
#------------------------------------------------------------------------------
widget_formattable
formattable(tab_reduce, list(
  suicides_no= color_tile('lightblue', 'white'),
 `gdp_per_capita ($)`= color_tile('red', 'white', na.rm = TRUE),
 population = color_tile('orange', 'white'),
  area(col=c(`suicides.100k.pop`)) ~ normalize_bar("pink")
))

#--------------------------------------------------------------------------------
strand = formatter('span', style=style(font.weight = "bold"))

#creating function using formatter
none_formatter <- function() {
  formatter("span", 
            style = ~ style(color = ifelse(x== "none", "blue", "black"))
  )	 	 
} 

#-----------------------------------------------------------------
icon_formatter <- function() {
  formatter("span", 
            style = x ~ style(color = ifelse(x, "green", "red")), x ~ icontext(ifelse(x, "ok", "remove"), "")
  )	 	 
}

#-------------------------------------------------------------------
improvement_formatter_arrow2 <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold", 
              color = ifelse(x > 100, customRed, ifelse(x < 100, customGreen, "black"))),
            x ~ icontext(ifelse(x>100, "arrow-up", "arrow-down"), x)
            )
#....................................
improvement_formatter2 <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold", 
              color = ifelse(x > 100, customRed, ifelse(x < 100, customGreen, "black"))))
#....................................
improvement_formatter_arrow <- formatter("span", 
                                   style = x ~ style(font.weight = "bold", 
                                                     color = ifelse(x > 5, customRed, ifelse(x < 5, customGreen, "black"))), 
                                   x ~ icontext(ifelse(x>5, "arrow-up", "arrow-down"), x)
)
#changing colour of specific Rows
#1)
formattable(clean_df, align =c("l","c","c","c","c", "c", "c", "c","c", "c", "c","c" ), list(
  `country` = 
    formatter("span", style = ~ style(color = "pink",font.weight = "bold")), 
  `year`= color_tile(customGreen, customGreen0),
  `population`= color_tile(customsnow,custombluemagnate),
  `suicides_no` = improvement_formatter_arrow
))

formattable(suicide_stat_country, align =c("l","c","c","c","c", "c", "c", "c","c", "c", "c","c" ), list(
  `country` = 
    formatter("span", style = ~ style(color = "pink",font.weight = "bold")), 
  `year`= color_tile(customGreen, customGreen0),
  `population`= color_tile(customsnow,custombluemagnate),
  `suicides_no` = improvement_formatter_arrow2
))

#Barchart showing the suicide rate  for canada from 1985 to 2016
par(las=1, mar=c(5,4,3,1))
with(canada_suicides,barplot(suicides_no,main="suicide rate per year in Canada",
  names.arg = year,xlab="Year",ylab="Number of Suicides", col=c("darkblue","red","green","purple","pink","white","blue","grey","orange","yellow"),
                             horiz = FALSE))

# TO get the countries with suicide number over 1,000,000 between 1965 to 2016
Naw=sqldf("select * from total_suicides_country where suicides_no >1000000")

#Barchart showing the suicide rate across differnt countries
par(las=1, mar=c(3,5,1,1))
with(total_suicides_country,barplot(suicides_no,main="suicide rate across different countries",
                             names.arg = country,xlab="Country", col=c("darkblue","red","green","purple","pink","white","blue","grey","orange","yellow"),
                             horiz = FALSE))


#Barchart showing the suicide rate across differnt year
par(las=1, mar=c(5,7,3,1))
with(suicides_year,barplot(suicides_no,main="suicide rate across different Years Globally",
                                    names.arg = year,xlab="Years", col=c("darkblue","red","green","purple","pink","white","blue","grey","orange","yellow"),
                                    horiz = FALSE))

# shows the relationship between sex and suicide number

plot=ggplot(clean_df, aes(suicides_no,sex,color= generation))+
  geom_point(aes(size=sex,frame=year))
plot

#shows the relationship between age, year, generation and suicide number
plot2=ggplot(clean_df, aes(suicides_no,age,color= year))+
  geom_point(aes(size=generation,frame=year))
plot2


#shows if GDP affects the suicide rate using Canada as sample population
plot3=ggplot(canada_gdp, aes(gdp_for_year....,suicides_no,color= suicides_no))+
  geom_point()
             
plot3
plot3 + theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                     size = 12, angle = 45),
          axis.text.y = element_text(face = "bold", color = "blue", 
                                     size = 12, angle = 45),
          axis.ticks.length = unit(0.1, "pt"))

# Vertical rotation of x axis text
p + theme(axis.text.x = element_text(angle = 90))
#

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

print(suicides_year)

# to know the country with highest suicide rate in 1995

suicide_rate_1995<- clean_df[clean_df$year == '1995',] 

# Aggregating suicide per country in 1995
total_suicides_country_1995 <- aggregate(suicides_no~country,suicide_rate_1995,sum)

#ggplot of suicide rate per country in 1995
plot4=ggplot(total_suicides_country_1995, aes(suicides_no,country,color= country))+
  geom_point()
plot4
