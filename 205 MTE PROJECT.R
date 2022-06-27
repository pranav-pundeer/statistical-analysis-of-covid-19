
#using the covid19inida library to fetch data
library(covid19india)

#storing the aggregate data into data1
data1<-covid19india::get_all_data()

#obtaining various parameters of the data from 2021-06-01 to 2021-10-10
cases=data1[8250:8381]$daily_cases
doses=data1[8250:8381]$total_doses
deaths=data1[8250:8381]$daily_deaths


#under the assumption that after the initiation of the vaccine drive in India,
#overall cases count in the country would depreciate with time and vaccine doses:

#scatter plotting daily covid cases in India since June 2021
plot(doses,cases,xlab="no. of vaccine doses(double and single)",ylab = "no. of daily cases")
#plotting a linear regression model 
mod_cases=lm(cases~doses)
summary(mod_cases)
#obtaining the best fit line for the plot
abline(mod_cases,col="red",lwd=2)
#obtaining correlation coeff to see how the two parameters are related
cor(doses,cases,method = "pearson",use = "complete.obs")


#under the assumption that after the initiation of the vaccine drive in India, 
#overall death count in the country would depreciate with time and vaccine doses:

#scatter plotting daily deaths due to covid in India since June 2021
plot(doses,deaths,xlab="no. of vaccine doses(double and single)",ylab = "no. of daily deaths")
#plotting a linear regression model 
mod_deaths=lm(deaths~doses)
summary(mod_deaths)
#obtaining the best fit line for the plot
abline(mod_deaths,col="purple",lwd=3)
#obtaining correlation coeff to see how the two parameters are related
cor(doses,deaths,method = "pearson",use = "complete.obs")




#it is also of the opinion that the number of deaths also depends upon the factors
#such as age and gender of a population, more specifically, that older people
#are more likely to die due to the infection that the younger people and that
#a death of a male is more likely to occur over demise of a female patient

#the following file fetches data for various new cases at the onset of the 
#virus in various parts of the world, taking into account the factors in which
#we are interested ie, gender and age.

data2 <- read.csv("C:/Users/pranav pundeer/Desktop/desk/covid.csv")

# converting the character values in death column into numbers
data2$death=as.integer(data2$death != 0)



# AGE
# claim: older and younger people are equally likely to die (null hypothesis)


dead = subset(data2, death== 1)
alive = subset(data2, death == 0)

#obtaining correlation to see the dependency of parameters on one another
cor(data2$death,data2$age,method = "pearson",use = "complete.obs")

# testing if our hypothesis is statistically significant
t.test(alive$age, dead$age, alternative="less", conf.level = 0.95)



# normally, if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and 
# conclude that this is statistically significant and conclude that
#older people are at a higher risk of dying due to contracting
#the virus.

# GENDER
# claim: gender has no effect(null hypothesis)
men = subset(data2, gender =="male")
women = subset(data2, gender == "female")
mean(men$death, na.rm = TRUE) 
mean(women$death, na.rm = TRUE)
# testing if our hypothesis is statistically significant
t.test(men$death, women$death, alternative="greater", conf.level = 0.99)
