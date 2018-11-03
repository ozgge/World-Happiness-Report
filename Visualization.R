Happiness2015 <- read.csv("C:/Users/Özge/Desktop/2015.csv")
Happiness2016 <- read.csv("C:/Users/Özge/Desktop/2016.csv")

library(ggalt)
library(ggplot2) 
library(readr) 
library(dplyr)
library(gridExtra)
library(RColorBrewer)
library(repr)

#Changing the name of columns
colnames (Happiness2015) <- c("Country", "Region", "Happiness.Rank", "Happiness.Score",
                              "Standard.Error", "Economy", "Family",
                              "Life.Expectancy", "Freedom", "Trust", "Generosity",
                              "Dystopia.Residual")
colnames(Happiness2016) <- c("Country", "Region", "Happiness.Rank", "Happiness.Score",
                             "Lower.Confidence.Interval", "Upper.Confidence.Interval", "Economy", "Family",
                             "Life.Expectancy", "Freedom", "Trust", "Generosity",
                             "Dystopia.Residual")

#remove the columns that are not necessary for visualization
Happiness2015 <- Happiness2015[, -c(5)]
Happiness2016 <- Happiness2016[, -c(5,6)]

#Check for null values
sapply(Happiness2015, function(x) sum(is.na(x)))
sapply(Happiness2016, function(x) sum(is.na(x)))

library(skimr)
Happiness2015 %>% skim() %>% kable()
Happiness2016 %>% skim() %>% kable()

str(Happiness2015)
str(Happiness2016)

library(corrplot)

#Correlation

#Find the corr between only numerical columns for 2015 dataset
Num.cols <- sapply(Happiness2015, is.numeric)
Cor.data <- cor(Happiness2015[, Num.cols])
corrplot(Cor.data, method = 'color')

#Find the corr between only numerical columns for 2016 dataset
Num.cols <- sapply(Happiness2016, is.numeric)
Cor.data <- cor(Happiness2016[, Num.cols])
corrplot(Cor.data, method = 'color')

#Find the corr between only numerical columns for 2015 dataset as method number without Happiness Rank
Num.cols <- sapply(Happiness2015, is.numeric)
newdatacor = cor(Happiness2015[c(4:11)])
corrplot(newdatacor, method = "number")

#Find the corr between only numerical columns for 2016 dataset as method number without Happiness Rank
Num.cols <- sapply(Happiness2016, is.numeric)
newdatacor = cor(Happiness2016[c(4:11)])
corrplot(newdatacor, method = "number")

#Pearson Correlation
ds15 <- Happiness2015 %>% select(Happiness.Score,Economy,Family,Freedom,Trust, Life.Expectancy, Generosity, Dystopia.Residual)
ds15_corr <- cor(ds15, use = "complete", method = "pearson")
corrplot(ds15_corr,title = 'Happiness Correlation Plot - 2015')


library(ellipse)
data=cor(Happiness2015[, 3:11])
mycolors <- brewer.pal(7, "Greens")
mycolors=colorRampPalette(mycolors)(100)

# Order the correlation matrix
ordr <- order(data[1, ])
dataordr = data[ordr, ordr]
plotcorr(dataordr , col=mycolors[dataordr*50+50] , mar=c(1,1,1,1)  )




#country happiness score increased:2015 vs 2016
d15<-Happiness2015 %>% select(Country, Region,HS15=Happiness.Score)
d16<-Happiness2016 %>% select(Country, Region,HS16=Happiness.Score)

library(ellipse)
data=cor(Happiness2015[, 3:11])
mycolors <- brewer.pal(7, "Greens")
mycolors=colorRampPalette(mycolors)(100)

# Order the correlation matrix
ordr <- order(data[1, ])
dataordr = data[ordr, ordr]
plotcorr(dataordr , col=mycolors[dataordr*50+50] , mar=c(1,1,1,1)  )

library(gridExtra)
library(grid)
library(ggthemes)
library(ggpubr)

#PLOTS

gg1 <- ggplot(Happiness2015 , aes(x = Region, y = Happiness.Score)) +
  geom_boxplot(aes(fill=Region)) + theme_bw() +
  theme(axis.title = element_text(family = "Calibri", size = (8)))
gg1


stable <- desc_statby(Happiness2015, measure.var = "Happiness.Score",
                      grps = "Region")
stable <- stable[, c("Region","mean","median")]
names(stable) <- c("Region", "Mean of happiness score","Median of happiness score")
# Summary table plot
stable.p <- ggtexttable(stable,rows = NULL, 
                        theme = ttheme("classic"))
stable.p

ggarrange(gg1, stable.p, ncol = 1, nrow = 2)




head(Happiness2015)
str(Happiness2015)
summary(Happiness2015)


#which countries that ranks top 15
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
library(gridExtra)
library(RColorBrewer)
library(formattable)


top15_2015<-Happiness2015 %>% select(Country,Region,Happiness.Rank,Happiness.Score) %>% head(n=15)
top15_2016<-Happiness2016 %>% select(Country,Region,Happiness.Rank,Happiness.Score) %>% head(n=15)

plt1<-ggplot(top15_2015,aes(x=factor(Country,levels=Country),y=Happiness.Rank))+geom_bar(stat="identity",width=0.5,fill="pink")+theme(axis.text.x = element_text(angle=90, vjust=0.6))+labs(title="Top15 Happiest Countries-2015",x="Country",y="Rank")
plt2<-ggplot(top15_2016,aes(x=factor(Country,levels=Country),y=Happiness.Rank))+geom_bar(stat="identity",width=0.5,fill="red")+theme(axis.text.x = element_text(angle=90, vjust=0.6))+labs(title="Top15 Happiest Countries-2016",x="Country",y="Rank")
grid.arrange(plt1, plt2,  ncol = 2, nrow=1)
# For both 2015 and 2016, Countries are the same, only the score has got changed.

# Which countries that ranks bottom 15
bottom15_2015<-Happiness2015 %>% select(Country,Region,Happiness.Rank,Happiness.Score) %>% tail(n=15)
bottom15_2016<-Happiness2016 %>% select(Country,Region,Happiness.Rank,Happiness.Score) %>% tail(n=15)

plt1<-ggplot(bottom15_2015,aes(x=factor(Country,levels=Country),y=Happiness.Score))+geom_bar(stat="identity",width=0.5,fill="purple")+theme(axis.text.x = element_text(angle=90, vjust=0.6))+labs(title="Bottom 15 Happiest Countries-2015",x="Country",y="Score")+coord_flip()
plt2<-ggplot(bottom15_2016,aes(x=factor(Country,levels=Country),y=Happiness.Score))+geom_bar(stat="identity",width=0.5,fill="darkred")+theme(axis.text.x = element_text(angle=90, vjust=0.6))+labs(title="Bottom 15 Happiest Countries-2016",x="Country",y="Score")+coord_flip()
grid.arrange(plt1, plt2,  ncol = 2,nrow=1)
#For both 2015 and 2016, Countries are the same, only the score has got changed like the previous one.


#country happiness score increased:2015 vs 2016
d15<-Happiness2015 %>% select(Country, Region,HS15=Happiness.Score)
d16<-Happiness2016 %>% select(Country, Region,HS16=Happiness.Score)


library(flexdashboard)
score<-inner_join(d15,d16)%>% mutate(score_diff= HS16-HS15)%>% filter(score_diff>0)



score$Country <- factor(score$Country, levels=as.character(score$Country))
gg <- ggplot(score, aes(x=HS15, xend=HS16, y=Country, group=Country)) + geom_dumbbell(size=2, color="#e3e2e1", 
                                                                                      colour_x = "#5b8124", colour_xend = "#bad744",
                                                                                      dot_guide=TRUE, dot_guide_size=0.25) + 
  labs(x=NULL, 
       y=NULL, 
       
       title=" Country Happiness Scores Increased: 2015 vs 2016"
  ) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())
plot(gg)







library(rworldmap)
#Compare economy of all countries in year 2015 & 2016
#2015
a <- data.frame(
  country=Happiness2015$Country,
  value=Happiness2015$Economy)
b <- joinCountryData2Map(a, joinCode="NAME", nameJoinColumn="country")
mapCountryData(b, nameColumnToPlot="value", mapTitle="2015 Economy World Map",colourPalette=RColorBrewer::brewer.pal(9,'RdBu') )


#2016
a2 <- data.frame(
  country=Happiness2016$Country,
  value=Happiness2016$Economy)

b2 <- joinCountryData2Map(a2, joinCode="NAME", nameJoinColumn="country")
mapCountryData(b2, nameColumnToPlot="value", mapTitle="2016 Economy World Map",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'))


#Compare freedom of all countries in year 2015 & 2016
#2015
cp <- RColorBrewer::brewer.pal(7,'RdPu')

a3 <- data.frame(
  country=Happiness2015$Country,
  value=Happiness2015$Freedom)

b3 <- joinCountryData2Map(a3, joinCode="NAME", nameJoinColumn="country")
mapCountryData(b3, nameColumnToPlot="value", mapTitle="Freedom In The World in 2015",colourPalette=cp,borderCol='black')

#2016
a4 <- data.frame(
  country=Happiness2016$Country,
  value=Happiness2016$Freedom)

b4 <- joinCountryData2Map(a4, joinCode="NAME", nameJoinColumn="country")
mapCountryData(b4, nameColumnToPlot="value", mapTitle="Freedom In The World in 2016",colourPalette=cp,borderCol='black')






library(plotly)
library(repr)
library(maps)
library(treemap)

df2015<- Happiness2015 %>% select(Country,Region,Happiness_Rank=Happiness.Rank,Happiness_Score=Happiness.Score,GDP=Economy,Family,Life_Expectancy=Life.Expectancy,Government_Corruption=Trust)
df2016 <- Happiness2016 %>% select(Country,Region,Happiness_Rank=Happiness.Rank,Happiness_Score=Happiness.Score,GDP=Economy,Family,Life_Expectancy=Life.Expectancy,Government_Corruption=Trust)
dim(df2015)

dropm2016 <- df2016$Country[!df2016$Country %in% df2015$Country] #missing country filter
dropm2015 <- df2015$Country[!df2015$Country %in% df2016$Country] #missing country filter
filtdf2015 <- df2015 %>% filter(!Country %in% dropm2015) #apply filter
filtdf2016 <- df2016 %>% filter(!Country %in% dropm2016) #apply filter
filtdf2016$Country %in% filtdf2015$Country
unique(filtdf2015$Region)
mydata<-merge(filtdf2015[,], filtdf2016[,], by.x = "Country", by.y = "Country") %>% select(-Region.y)
colnames(mydata)[9:14] <- gsub('.{2}$', '.2016', colnames(mydata)[9:14])
colnames(mydata)[1:8] <- gsub('.{2}$','.2015',colnames(mydata)[1:8])
colnames(mydata)[1] = "Country"
colnames(mydata)[2] = 'Region'
mydata <- mydata %>% mutate(Happiness_Rank_Change=Happiness_Rank.2016 - Happiness_Rank.2015,Happiness_Score_Change=Happiness_Score.2016 - Happiness_Score.2015, GDP_Change = GDP.2016 - GDP.2015, Government_Corruption_Change = Government_Corruption.2016 - Government_Corruption.2015, Life_Expectancy_Change = Life_Expectancy.2016 - Life_Expectancy.2015)
top30inc <- mydata %>% arrange(desc(Happiness_Rank_Change)) %>%  mutate(Country=factor(Country,Country)) %>% head(30)
head(top30inc)
top30dec <- mydata %>% arrange(Happiness_Rank_Change) %>% mutate(Country = factor(Country,Country))%>% head(20)
head(top30dec)


h <- ggplot(top30dec,aes(x=Country,Happiness_Rank_Change,fill=Region))
h + geom_histogram(stat='identity',alpha=.5) + theme_minimal() + scale_fill_brewer(palette='RdBu') + theme(axis.text.x=element_text(angle=60,hjust=1)) + xlab('') + ylab('Decrease in Happiness Ranking 2015 & 2016') + ggtitle('Top 30 Countries with Declining Happiness Ranking 2015 & 2016')
h + geom_histogram(stat='identity',alpha=.5) + theme_minimal() + scale_fill_brewer(palette='RdBu') + theme(axis.text.x=element_text(angle=60,hjust=1)) + xlab('') + ylab('Decrease in Happiness Ranking 2015 & 2016') + ggtitle('Top 30 Countries with Declining Happiness Ranking 2015 & 2016') + coord_polar()


ggplot(mydata,aes(Region,Happiness_Score_Change,fill=Region),alpha=.5) + geom_violin(alpha=0.5) + theme_minimal()  + scale_fill_brewer(palette = 'RdBu')+ theme(axis.text.x=element_text(angle=60,hjust=1)) + xlab('') + ylab('Change in Happiness Score in 2015 and 2016')
ggplot(mydata,aes(Region,GDP_Change,fill=Region),alpha=.5) + geom_violin(alpha=0.5) + theme_minimal()  + scale_fill_brewer(palette = 'RdBu')+ theme(axis.text.x=element_text(angle=60,hjust=1)) + xlab('') + ylab('Change in Economy in 2015 and 2016')
ggplot(mydata,aes(Region,Life_Expectancy_Change,fill=Region),alpha=.5) + geom_violin(alpha=0.5) + theme_minimal()  + scale_fill_brewer(palette = 'RdBu')+ theme(axis.text.x=element_text(angle=60,hjust=1)) + xlab('') + ylab('Change in Health (Life Expectancy) in 2015 and 2016')
ggplot(mydata,aes(Region,Government_Corruption_Change,fill=Region),alpha=.5) + geom_violin(alpha=0.5) + theme_minimal()  + scale_fill_brewer(palette = 'RdBu')+ theme(axis.text.x=element_text(angle=60,hjust=1)) + xlab('') + ylab('Change in Trust (Government Corruption) in 2015 and 2016')


ggplot(mydata,aes(Happiness_Score.2015,Government_Corruption.2015,color=Region)) + geom_point(shape=18,size=4,alpha=.5) + theme_minimal() + scale_color_brewer(palette = 'RdBu') + xlab('Happiness Score 2015') + ylab('Trust (Government Corruption) Change 2015') + ggtitle('Happiness Score Vs Trust (Government Corruption) 2015')
ggplot(mydata,aes(Happiness_Score.2016,Government_Corruption.2016,color=Region)) + geom_point(shape=18,size=4,alpha=.5) + theme_minimal() + scale_color_brewer(palette = 'RdBu') + xlab('Happiness Score 2016') + ylab('Trust (Government Corruption) Change 2016') + ggtitle('Happiness Score Vs Trust (Government Corruption) 2016') 
ggplot(mydata,aes(Happiness_Score_Change,Government_Corruption_Change,color=Region)) + geom_point(shape=18,size=4,alpha=.5) + theme_minimal() + scale_color_brewer(palette = 'RdBu') + xlab('Happiness Score Change in 2015 and 2016') + ylab('Trust (Government Corruption) Change in 2015 and 2016') + ggtitle('Happiness Score Change Vs Trust (Government Corruption) Change in 2015 and 2016')

ggplot(mydata,aes(Life_Expectancy_Change,GDP_Change,color=Region)) + geom_point(shape=18,size=4,alpha=.5) + theme_minimal() + scale_color_brewer(palette = 'RdBu') + xlab('Health (Life Expectancy) Change in 2015 and 2016') + ylab('Economy Change in 2015 and 2016') + ggtitle('Health (Life Expectancy) Change vs Economy Change in 2015 and 2016') + geom_smooth(method=lm,se=FALSE)
ggplot(mydata,aes(Life_Expectancy.2015,GDP.2015,color=Region,size=Happiness_Score.2015)) + geom_point(shape=18,alpha=.5) + theme_minimal() + scale_color_brewer(palette = 'RdBu') + xlab('Health (Life Expectancy) 2015') + ylab('Economy 2015') + ggtitle('Health (Life Expectancy) Vs Economy 2015') 
ggplot(mydata,aes(Life_Expectancy.2015,GDP.2016,size=Happiness_Score.2016)) + geom_point(aes(size=GDP.2016,color=GDP.2016),shape=18,alpha=.8) + theme_minimal() + scale_color_brewer(palette = 'RdBu') + xlab('Health (Life Expectancy) 2016') + ylab('Economy 2016') + ggtitle('Health (Life Expectancy) Vs Economy 2016') + geom_smooth(method='lm',formula = y ~ splines::bs(x, 3)) + scale_colour_gradientn(colours =colorRampPalette(rev(brewer.pal(7, "RdBu")))(100), values=seq(0, 100, length.out=100)/100) + 
  
  geom_text(aes(label=Country),nudge_y = -.05, size=2.5,color="black",alpha=.95)+labs(title="Economy vs Health (Life Expectancy) - 2015",x="Health (Life Expectancy) - 2015") + theme(legend.position='none')

df1_ <- joinCountryData2Map(mydata, joinCode="NAME", nameJoinColumn="Country")
mapCountryData(df1_, nameColumnToPlot="Happiness_Score.2015", mapTitle="World Map for Happiness Score -2015",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')
mapCountryData(df1_, nameColumnToPlot="Happiness_Score.2016", mapTitle="World Map for Happiness Score -2016",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')
mapCountryData(df1_, nameColumnToPlot="Happiness_Rank_Change", mapTitle="World Map for Change in Happiness Rank 2015-2016",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')



#economy
mapCountryData(df1_, nameColumnToPlot="GDP.2015", mapTitle="World Map for GDP -2015",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')
mapCountryData(df1_, nameColumnToPlot="GDP.2016", mapTitle="World Map for GDP -2016",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')
mapCountryData(df1_, nameColumnToPlot="GDP_Change", mapTitle="World Map for Change in GDP 2015-2016",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')

#health
mapCountryData(df1_, nameColumnToPlot="Life_Expectancy.2015", mapTitle="World Map for Life Expectancy -2015",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')
mapCountryData(df1_, nameColumnToPlot="Life_Expectancy.2016", mapTitle="World Map for Life Expectancy -2016",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')
mapCountryData(df1_, nameColumnToPlot="Life_Expectancy_Change", mapTitle="World Map for Change in Life Expectancy 2015-2016",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')

#trust
mapCountryData(df1_, nameColumnToPlot="Government_Corruption.2015", mapTitle="World Map for Government Corruption -2015",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')
mapCountryData(df1_, nameColumnToPlot="Government_Corruption.2016", mapTitle="World Map for Government Corruption -2016",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')
mapCountryData(df1_, nameColumnToPlot="Government_Corruption_Change", mapTitle="World Map for Change in Government Corruption 2015-2016",colourPalette=RColorBrewer::brewer.pal(9,'RdBu'),borderCol = 'black')



