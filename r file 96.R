library(readr)
salesF <- read_csv("C:/Users/Dell/Downloads/salesF.csv")
View(salesF)

library(ggplot2)
library(dplyr)
library(tidyr)

summary(salesF)
str(salesF)

#1.What is the overall total of sales trend for each of the 3 supermarkets ?

by_total = group_by(salesF, Branch)
R=summarise(by_total, avg_total = mean(Total), qty = n())
View(R)

a1=filter(salesF,Branch=="A")
plot1=ggplot(a1,aes(Total))+geom_boxplot()
print(plot1)

a2=filter(salesF,Branch=="B")
plot2=ggplot(a2,aes(Total))+geom_boxplot()
print(plot2)

a3=filter(salesF,Branch=="C")
plot3=ggplot(a3,aes(Total))+geom_boxplot()
print(plot3)

dev.off()
library(grid)
viewport<- viewport(
  layout=grid.layout(2,2))
pushViewport(viewport)
print(x=plot1,
      vp=viewport(
        layout.pos.row = 1,
        layout.pos.col=1))
print(x=plot2,
      vp=viewport(
        layout.pos.row = 1,
        layout.pos.col=2))
print(x=plot3,
      vp=viewport(
        layout.pos.row = 2,
        layout.pos.col=1))

#2.Find the trend of rating between cities and whether the customers are satisfied or not ?

p=ggplot(salesF,aes(x=City,y=Rating,fill=Rating))+geom_bar(stat="identity")
print(p)

AB=filter(salesF,Rating>7)
View(AB)
p1=ggplot(AB,aes(x=Rating,y=City,fill=Rating))+geom_bar(stat="identity")
print(p1)


#3.Which is the most popular payment method used by customers?

BC=ggplot(salesF,aes(Payment,fill=Payment))+geom_bar()
print(BC)

BC=ggplot(salesF,aes(Payment,fill=Branch))+geom_bar()
print(BC)

#4.Which branch is the most profitable?

BA=ggplot(salesF,aes(Branch,gross_income,fill=Branch))+geom_boxplot()
print(BA)

#5. Which day of the week has maximum sales?

BD=ggplot(salesF,aes(Day,fill=Branch))+geom_bar()
print(BD)

#6.Which product line gives the highest gross income ?


df1=group_by(salesF,Product_line) %>% summarise(Mean.Gross_income=mean(gross_income), Sd.Gross_income=sd(gross_income))

b1=filter(salesF,Product_line=="Health and beauty")
plot1=ggplot(b1,aes(gross_income))+geom_boxplot()
print(plot1)

b2=filter(salesF,Product_line=="Electronic accessories")
plot2=ggplot(b2,aes(gross_income))+geom_boxplot()
print(plot2)

b3=filter(salesF,Product_line=="Home and lifestyle")
plot3=ggplot(b3,aes(gross_income))+geom_boxplot()
print(plot3)

b4=filter(salesF,Product_line=="Sports and travel")
plot4=ggplot(b4,aes(gross_income))+geom_boxplot()
print(plot4)

b5=filter(salesF,Product_line=="Food and beverages")
plot5=ggplot(b5,aes(gross_income))+geom_boxplot()
print(plot5)

b6=filter(salesF,Product_line=="Fashion accessories")
plot6=ggplot(b6,aes(gross_income))+geom_boxplot()
print(plot6)


dev.off()
library(grid)
viewport<- viewport(
  layout=grid.layout(3,2))
pushViewport(viewport)
print(x=plot1,
      vp=viewport(
        layout.pos.row = 1,
        layout.pos.col=1))
print(x=plot2,
      vp=viewport(
        layout.pos.row = 1,
        layout.pos.col=2))
print(x=plot3,
      vp=viewport(
        layout.pos.row = 2,
        layout.pos.col=1))
print(x=plot4,
      vp=viewport(
        layout.pos.row = 2,
        layout.pos.col=2))
print(x=plot5,
      vp=viewport(
        layout.pos.row = 3,
        layout.pos.col=1))
print(x=plot6,
      vp=viewport(
        layout.pos.row = 3,
        layout.pos.col=2))

#7.Find the product line and branch trend.

ABC=ggplot(salesF, aes(y=Product_line))+geom_bar(aes(fill = Branch))
print(ABC)



#cor relation

A10=select(salesF,Unit_price,Quantity,Rating)
View(A10)

cor(A10)
A11<-cor(A10)

corrplot(A11,method = "pie")

#there is no any relation between rating and unit price or quantity .

ggplot(salesF,aes(Time,fill= "Quantity"))+geom_bar()

#8.which branch is most busiest?

PlotAB = ggplot(salesF, aes(x = Branch))+geom_bar(aes(fill = Branch)) 
print(PlotAB)  