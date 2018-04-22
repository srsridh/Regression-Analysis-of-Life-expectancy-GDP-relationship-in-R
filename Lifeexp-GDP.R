#install.packages("gapminder")
#install.packages("broom")
#install.packages("GGally")
library(GGally)
library(gapminder)
library(ggplot2)
library(broom)
library(MASS)
library(tidyr)
library(dplyr)

summary(gapminder) 

data_2007 <- gapminder[which(gapminder$year == 2007 & gapminder$continent != 'Oceania'), ]

#Question 1.11 TREND LINE (FIRST OF THE THREE BIVARIATE GRAPHS)
ggplot(data_2007, aes(x = log(gdpPercap), y = lifeExp)) + geom_point() + 
geom_smooth(method="rlm", formula = y~x, aes(colour = "Robust Linear  Curve")) + 
#geom_smooth(method="loess", aes(colour = "Loess Curve")) + 
#geom_smooth(method="lm", formula = y~x+I(x^2), aes(colour = "Linear  Curve")) +
geom_smooth(method="lm", formula = y~x, aes(colour = "Linear  Curve")) +
xlab("Log of GDP per capita") + 
ylab("Life expectancy") + 
ggtitle("Trend line: Life Expectency VS GDP per capita (2007)") +
scale_colour_manual(name="TrendLines", values=c("red", "blue", "green"))


#Quesion 1.12 AUGMENT, RESIDUALS vs GDP PER CAPITA GRAPH (SECOND OF THE THREE BIVARIATE GRAPHS)
data_2007_lm <- rlm(lifeExp ~ log(gdpPercap),  data = data_2007)
data_2007_lm_df <- augment(data_2007_lm)
summary(data_2007_lm_df)
ggplot(data_2007_lm_df, aes(x = log.gdpPercap., y = .resid)) + 
geom_point() + geom_smooth(method = "loess") + geom_abline(slope = 0) +
xlab("GDP per capita") + 
ylab("Residuals") + 
ggtitle("Residuals VS GDP per capita (2007)")  

#Question 1.13 SPREAD LOCATION GRAPH (THIRD OF THE THREE BIVARIATE GRAPHS)
ggplot(data_2007_lm_df, aes(x = .fitted, y = abs(.resid))) + 
geom_smooth() +
xlab("Fitted") + 
ylab("Absolute of Residuals") + 
ggtitle("Spread Location graph- Life Expectancy")

#Question 1.2 GDP VS life expectancy trends for different continents (2007)
ggplot(data_2007, aes(x = (gdpPercap), y = lifeExp, col = continent)) + geom_text(aes(label=country),check_overlap = TRUE) + geom_smooth(method = 'loess', se = FALSE) +
xlab("Log of GDP per capita (in US Dollars)") + ylab("Life expectancy (Years)") + ggtitle("GDP VS log(life expectancy) trends for continents")

asia <- data_2007$lifeExp[data_2007$continent == "Asia" & data_2007$gdpPercap < 30000 & 6000 < data_2007$gdpPercap]
europe <- data_2007$lifeExp[data_2007$continent == "Europe" & data_2007$gdpPercap < 30000 & 6000 < data_2007$gdpPercap]

#Question 1.3 ADDITIVE SHIFT
qqplot(asia,europe, title=("Additive shift for differences in Asia and Europe"))
abline(0,1)

#R square value
r_square <- var((abs(data_2007_lm_df$.fitted)))/var(log(data_2007$gdpPercap))




#------------------------------------------------QUESTION 2----------------------------------------------#
data <- gapminder
data<-subset(data,continent!="Oceania")
ggplot(data,aes(x = year, y = lifeExp,group=continent, color = continent))+
  geom_smooth(aes(weight = pop),span = 0.1,se=FALSE)+
  geom_point(aes(size=pop,alpha=0.5))+
  ggtitle("Changes in Life Expectency over time -  Graph for Trend")

ggplot(data,aes(x = year, y = lifeExp,group=continent, color = continent))+
  geom_smooth(aes(weight = pop),span = 0.1,se=FALSE)+
  ggtitle("Changes in Life Expectency over time - Graph for influencers")+
  geom_text(aes(size=pop,label=country),check_overlap = TRUE)+
  facet_wrap(~continent,ncol=2,scales="free")

k<-lm(lifeExp~year+pop,subset(data,continent=="Americas"))
k<-augment(k)
ggplot(k,aes(x=year, y=.resid))+geom_smooth(method="loess")+geom_abline(slope = 0)
ggplot(k,aes(x=.fitted, y=abs(.resid)))+geom_smooth(method="loess")+geom_abline(slope = 0)

k<-lm(lifeExp~year+pop,subset(data,continent=="Asia"))
k<-augment(k)
ggplot(k,aes(x=year, y=.resid))+geom_smooth(method="loess")+geom_abline(slope = 0)
ggplot(k,aes(x=.fitted, y=abs(.resid)))+geom_smooth(method="loess")+geom_abline(slope = 0)

k<-lm(lifeExp~year+pop,subset(data,continent=="Europe"))
k<-augment(k)
ggplot(k,aes(x=year, y=.resid))+geom_smooth(method="loess")+geom_abline(slope = 0)
ggplot(k,aes(x=.fitted, y=abs(.resid)))+geom_smooth(method="loess")+geom_abline(slope = 0)

k<-lm(lifeExp~year+pop,subset(data,continent=="Africa"))
k<-augment(k)
ggplot(k,aes(x=year, y=.resid))+geom_smooth(method="loess")+geom_abline(slope = 0)
ggplot(k,aes(x=.fitted, y=abs(.resid)))+geom_smooth(method="loess")+geom_abline(slope = 0)





#------------------------------------------------QUESTION 3----------------------------------------------#
gapminder_4 <- gapminder[which(gapminder$continent != "Oceania"),]
#Question 3.1 RELATIONSHIP BETWEEN GDP AND LIFE EXPECTANCY OVER DIFFERENT CONTINENTS
ggplot(gapminder_4, aes(x = gdpPercap, y = lifeExp)) + geom_point() + geom_smooth(method = 'loess') + facet_grid(~continent)

ggplot(gapminder_4,aes(x=gdpPercap,y=lifeExp,color=continent, label = country)) + 
  geom_point() + geom_smooth(aes(weight = pop),se=FALSE, method = "lm", formula = y~x+I(x^2)) + 
  facet_wrap(continent~cut_interval(year,n=5),ncol=5) + xlim(0,60000)

#Question 3.2 CORRELATION BETWEEN LIFE EXPECTANCY AND GDP PER CAPITA, YEAR AND CONTINENT
ggpairs(gapminder_4, columns = c("lifeExp", "gdpPercap","year"), aes(col = continent, alpha = 0.40))

#Question 3.3 TIME EFFECT ON LIFE EXPECTANCY IN ADDITION TO GDP EFFECT

wireframe_plot<-function(dataset,cont){
  gapminder.loess = loess(lifeExp ~ gdpPercap + year, data = dataset,weights = pop)
  gapminder.grid = expand.grid(year = seq(1952,2007), gdpPercap = seq(6000,30000,436.3636))
  gapminder.predict = predict(gapminder.loess, gapminder.grid)
  gapminder.plot.df =data.frame(gapminder.grid, fit =as.vector(gapminder.predict))
  wireframe(fit ~ year*gdpPercap, data = gapminder.plot.df,panel = function(...)
  {
    panel.wireframe(...)
    grid::grid.text(cont, 0, 0, default.units = "native",x=unit(0.45, "npc"), y=unit(0.95, "npc"))
  },
  col.regions = colorRampPalette(c("blue", "pink"))(100),drape=TRUE)
}

l=c('Asia','Americas','Europe','Africa')
for( cont in l){
  print(wireframe_plot(subset(gapminder_4,continent==cont),cont))
  
}
#QUESTION 3.4 CONVERGENCE

ggplot(gapminder_4,aes(x = log(gdpPercap), y = lifeExp,color=continent))+ 
  geom_point(aes(alpha=0.5))+
  geom_smooth(method = "lm",aes(weight=pop),se=FALSE)+
  facet_grid(~cut_number(year, n = 5))+
  labs(title = "Gapminder data split by year")


