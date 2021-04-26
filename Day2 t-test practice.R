##barley
barley<-read.csv("C:\\Users\\nick\\Documents\\Newcastle\\Fourth Year\\BIO8068 Management and Visualisation of Data\\test_BIO8068_1\\Day2 barley.csv")
head(barley)#looking at top few rows
str(barley)#so you can check the characteristics of the variables
barley$year<-as.factor(barley$year)#converts continuous to a factor
is.factor(barley$year)#checks if year is now a factor
ggplot(barley, aes(x=year, y=Yield)) + xlab("Year") + ylab("Yield kg/ha") + 
  geom_boxplot()
leveneTest(Yield~year, data=barley)
byf.shapiro(Yield~year, data=barley)
hist(barley$Yield)
t.test(Yield~year, paired=TRUE, data=barley)
mean(Yield~year, data=barley)
#if you were to run an independent t-test
t.test(Yield~year, data=barley)

##cars
mtcars#will show you the whole file
str(mtcars)#will show you the variable characteristics
head(mtcars)#will show you the top 6 rows 
mtcars$am<-as.factor(mtcars$am)#converts am(transmission) to factor
is.factor(barley$year)#check it worked
ggplot(mtcars, aes(x=am, y=mpg)) + xlab("transmission") + ylab("mpg") + 
  geom_boxplot()
leveneTest(mpg~am, data=mtcars)
byf.shapiro(mpg~am, data=mtcars)
hist(mtcars$mpg)
t.test(mpg~am, data=mtcars)

##bottles
bottles<-read.csv("Day2 bottles.csv")
str(bottles)#will show you the variable characteristics
head(bottles)#will show you the top 6 rows 
shapiro.test(bottles$Volume)
hist(bottles$Volume)
t.test(bottles$Volume, mu=500)

