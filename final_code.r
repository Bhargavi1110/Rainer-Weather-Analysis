climbing_data <- read.csv("climbing_statistics.csv", header=TRUE, sep=',')
weather_data<- read.csv("Rainier_Weather.csv", header=TRUE, sep=',')
library(dplyr)
library(lubridate)
library(ggplot2)

climbing_data=climbing_statistics
weather_data=Rainier_Weather

climbing_attempt=as.numeric(climbing_data$Attempted)
climbing_succ=as.numeric(climbing_data$Succeeded)
climbing_suceess = as.numeric(climbing_data$`Success Percentage`)
climbing_month = c()

toString(climbing_data$Date)

some_date=as.Date(climbing_data$Date,"%m/%d/%Y")

climbing_month=months(some_date)
climbing_month=match(climbing_month,month.name)
climbing_month



climbing_year=year(some_date)
climbing_data=mutate(climbing_data,year=climbing_year)
climbing_data=mutate(climbing_data,month=climbing_month)

#calculating the total entries in each month
month_freq=c()
for(i in 1:12){
  month_freq[i]=0
}

for(i in 1:4077){
  month_freq[climbing_month[i]]=month_freq[climbing_month[i]]+1;
}
month_freq

print("RAINIER DATA:")
paste("Data Representation")

print("Battery Statistics:")
print(paste("Mean: ",mean(weather_data$`Battery Voltage AVG`)))
print(paste("Median: ",median(weather_data$`Battery Voltage AVG`)))

print("Average TEmperature Statistics:")
print(paste("Mean: ",mean(weather_data$`Temperature AVG`)))
print(paste("Median: ",median(weather_data$`Temperature AVG`)))

print("Relative Humidity Statistics:")
print(paste("Mean: ",mean(weather_data$`Relative Humidity AVG`)))
print(paste("Median: ",median(weather_data$`Relative Humidity AVG`)))

print("Wind Speed Statistics:")
print(paste("Mean: ",mean(weather_data$`Wind Speed Daily AVG`)))
print(paste("Median: ",median(weather_data$`Wind Speed Daily AVG`)))

print("Wind Direction Statistics:")
print(paste("Mean: ",mean(weather_data$`Wind Direction AVG`)))
print(paste("Median: ",median(weather_data$`Wind Direction AVG`)))

print("Solar radiation Statistics:")
print(paste("Mean: ",mean(weather_data$`Solare Radiation AVG`)))
print(paste("Median: ",median(weather_data$`Solare Radiation AVG`)))


# Attempted vs Succeeded in each month
attempted = c(0,0,0,0,0,0,0,0,0,0,0,0)
success = c(0,0,0,0,0,0,0,0,0,0,0,0)

for(i in 1:4077){
  attempted[climbing_month[i]] = attempted[climbing_month[i]]+climbing_data$Attempted[i]
  success[climbing_month[i]] = success[climbing_month[i]]+climbing_data$Succeeded[i]
}

for(i in 1:12){
  attempted[i]=attempted[i]/month_freq[i]
  success[i]=success[i]/month_freq[i]
}
print(attempted)
print(success)

count_table=matrix(c(attempted,success), nrow = 2, byrow = TRUE)
print(count_table)
barplot(count_table, main="Attempted vs Succeeded",
        xlab="Months", col=c("darkblue","red"),
        legend = c("Attempted","Successs"), beside=TRUE)


# plotting the attempts and successes for each month and year wise
climbing_data %>% ggplot(aes(x=climbing_data$month,y=climbing_data$Attempted))+
  geom_bar(stat="identity",fill="darkorchid4")+facet_wrap(~climbing_data$year,ncol=3)+
  labs(title="Total monthly attempts",x="month",y="Attempts")+theme_bw(base_size = 15)

climbing_data %>% ggplot(aes(x=climbing_data$month,y=climbing_data$Succeeded))+
  geom_bar(stat="identity",fill="darkorchid4")+facet_wrap(~climbing_data$year,ncol=3)+
  labs(title="Total monthly successes",x="month",y="Attempts")+theme_bw(base_size = 15)

plot(climbing_data$month,climbing_data$Attempted,type="h",col="red",lwd="4",xlab="Month",ylab="No.of attempts",main="Maximum attempts in a month")
plot(climbing_data$month,climbing_data$Succeeded,type="h",col="orange",lwd="4",xlab="Month",ylab="No.of successes",main="Maximum successes in a month")
plot(climbing_data$month,climbing_data$`Success Percentage`,type="h",col="darkblue",lwd="4",xlab="Month",ylab="No.of successes",main="Success percentage in each month")


#time series plot for month vs temperature
rainier_date <- weather_data$Date
rainier_month = c()
toString(rainier_date)
some_date=as.Date(rainier_date,"%m/%d/%Y")
rainier_month=months(some_date)
rainier_month=match(rainier_month,month.name)
rainier_month
weather_data=mutate(weather_data,month=rainier_month)

ggplot(data = weather_data, aes(x = weather_data$month, y = weather_data$`Temperature AVG`))+geom_point(color = "red", size = 2)+labs(title="Month vs Temperature Average",x="Month",y="Temperature Average")

#wind direction vs wind speed
wind_relation = lm(weather_data$`Wind Speed Daily AVG` ~ weather_data$`Wind Direction AVG`)
print(wind_relation)
plot(weather_data$`Wind Speed Daily AVG`, weather_data$`Wind Direction AVG`, col = "orange", main = "Wind Speed vs Wind Direction", xlab = "Wind Direction",ylab = "Wind Speed",cex = 1.3,pch = 16,)
abline(lm(weather_data$`Wind Direction AVG` ~ weather_data$`Wind Speed Daily AVG`), col="darkgreen", lwd=3)

#solar radiation for every month
solarRad = c()
count = c(0,0,0,0,0,0,0,0,0,0,0,0)
for(i in 1:4077){
  solarRad[rainier_month[i]] = weather_data$`Solare Radiation AVG`[i]
  count[rainier_month[i]] = count[rainier_month[i]]+1
}
avg_solarRad = solarRad/count
plot(avg_solarRad, type = "o", col = "red", xlab = "Month", ylab = "Solar Radiation",main = "Average Solar Radiation per Month")

#solar rad given output the avg temp
summary(group_by(weather_data,weather_data$`Solare Radiation AVG`),vars(weather_data$`Temperature AVG`),funs(mean(.,na.rm=TRUE)))

#route given, predict success rate
getinfo = function(){
  option=readline(prompt="Enter route number: ")
  return(as.integer(option))
}

route_vs_success=aggregate(climbing_data$`Success Percentage`,by=list(route=climbing_data$Route),FUN=sum)
print("List of available routes")
print(route_vs_success[1])
val=getinfo()
print(paste("The success rate for route ",val," - ",route_vs_success[[1]][[val]]," is :",route_vs_success[[2]][[val]]))

# Multiple regression for predicting
y <- weather_data$`Temperature AVG`
x1 <- weather_data$`Battery Voltage AVG`
x2 <- weather_data$`Relative Humidity AVG`
x3 <- weather_data$`Wind Speed Daily AVG`
x4 <- weather_data$`Wind Direction AVG`
x5 <- weather_data$`Solare Radiation AVG`

model1 <- lm(y ~ x1 + x2 + x3 + x4 + x5 , data = weather_data)

a0 <- model1$coefficients[1]
a1 <- model1$coefficients[2]
a2 <- model1$coefficients[3]
a3 <- model1$coefficients[4]
a4 <- model1$coefficients[5]
a5 <- model1$coefficients[6]
#for example give values to x1,x2,x3,x4,x5
x1 <- 13
x2 <- 19
x3 <- 21
x4 <- 60
x5 <- 10

y <- a0 + x1*a1 + x2*a2 + x3*a3 + x4*a4 + x5*a5       #to find temp. avg
cat("Y value is:")
print(y)

# checking weather solar radiation and average temperature are independent using Chi-square test
# H0: They are independent
# Ha: They are dependent
obs_temp=weather_data$`Temperature AVG`
obs_solar=weather_data$`Solare Radiation AVG`

sum_temp=sum(obs_temp)
sum_solar=sum(obs_solar)
add_ts=c()
for(j in 1:length(obs_solar))
{
  add_ts[j] <- obs_temp[j]+obs_solar[j]
}
total=sum(add_ts)

exp_temp=c()
exp_solar=c()
for(i in 1:length(obs_temp)){
  exp_temp[i] = (sum_temp*add_ts[i])/total
  exp_solar[i] = (sum_solar*add_ts[i])/total
}

val_temp=c()
val_solar=c()
for(i in 1:length(obs_solar)){
  val_temp[i]=((obs_temp[i] - exp_temp[i])^2)/exp_temp[i]
  val_solar[i]=((obs_solar[i] - exp_solar[i])^2)/exp_solar[i]
}

chi_test=sum(val_temp)+sum(val_solar)
m=length(obs_temp)
n=2
dof=(m-1)*(n-1)
chi_value=qchisq(0.95,df=dof)

cat("Value of Chi-test is",chi_test)
cat("\nChi-value is",chi_value)

#ifelse(chi_test < chi_value,cat("H0 is accepted"),cat("h0 is rejected"))

if(chi_test < chi_value)
  cat("\nH0 is accepted")
if(chi_test > chi_value)
  cat("\nH0 is rejected")
