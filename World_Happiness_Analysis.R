#File download:https://www.kaggle.com/unsdsn/world-happiness?select=2019.csv

#read file
happiness <- read.csv("/Users/devanshi/Downloads/archive (1)/2019.csv", header = TRUE)
happiness

#Q1
#this is the correlation test between the happiness scores in countries and GDP Per Capita:
gdp <- cor.test(happiness$Score, happiness$GDP.per.capita)
gdp

#this is a correlation test betweeen happiness scores in countries and Social support
social_supp <- cor.test(happiness$Score, happiness$Social.support)
social_supp

#this is a correlation test between happiness scores in countries and Healthy life expectancy
life_expectancy <- cor.test(happiness$Score, happiness$Healthy.life.expectancy)
life_expectancy

#this is a correlation test between happiness scores in countries and Freedom to make life choices
freedom <- cor.test(happiness$Score, happiness$Freedom.to.make.life.choices)
freedom

#this is a correlation test between happiness scores in countries and Generosity
generosity <- cor.test(happiness$Score, happiness$Generosity)
generosity

#this is a correlation test between happiness scores in countries and Perceptions of corruption
corruption <- cor.test(happiness$Score, happiness$Perceptions.of.corruption)
corruption

#plots of correlation tests

install.packages("ggpubr")
library("ggpubr")

#score and gdp
grp = rep(happiness$Country.or.region), stringsAsFactors = TRUE)

ggscatter(happiness, x = "Score", y = "GDP.per.capita", color = 'Overall.rank',
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Happiness Score", ylab = "GDP Per Capita")

#score and social support
ggscatter(happiness, x = "Score", y = "Social.support", color = 'Overall.rank', 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Happiness Score", ylab = "Social Support")

#happiness scores in countries and Healthy life expectancy
ggscatter(happiness, x = "Score", y = "Healthy.life.expectancy", color = 'Overall.rank',
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Happiness Score", ylab = "Social Support")

#happiness scores in countries and Freedom to make life choices
ggscatter(happiness, x = "Score", y = "Freedom.to.make.life.choices", color = 'Overall.rank',
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Happiness Score", ylab = "Freedom to make Life Choices")

#happiness scores in countries and Generosity
ggscatter(happiness, x = "Score", y = "Generosity", color = 'Overall.rank',
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Happiness Score", ylab = "Generosity")

#happiness scores in countries and Perceptions of corruption
ggscatter(happiness, x = "Score", y = "Perceptions.of.corruption", color = 'Overall.rank',
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Happiness Score", ylab = "Perceptions of Corruption")


#Q2 
happ_2015 = read.csv("/Users/devanshi/Downloads/archive (1)/2015.csv", header = TRUE)
happ_2016 = read.csv("/Users/devanshi/Downloads/archive (1)/2016.csv", header = TRUE)
happ_2017 = read.csv("/Users/devanshi/Downloads/archive (1)/2017.csv", header = TRUE)
happ_2018 = read.csv("/Users/devanshi/Downloads/archive (1)/2018.csv", header = TRUE)
happ_2019 = read.csv("/Users/devanshi/Downloads/archive (1)/2019.csv", header = TRUE)


lm_15 <- lm(happ_2015$Happiness.Score ~ happ_2015$Family, data = happ_2015)
lm_15

#x = happ_2015$Family
#y = happ_2015$Happiness.Score
p <- ggplot(happ_2015 %>% mutate(resid=abs(resid(lm_15)), fitted=fitted(lm_15)))  +  geom_line(aes(happ_2015$Family, fitted))+ geom_point(aes(happ_2015$Family, happ_2015$Happiness.Score, colour=resid)) + scale_colour_gradient(low="yellow", high="red") +theme_classic() +labs(x="Family", y="Happiness Score", colour="Residuals")
p

#Q3

df_19 <- data.frame('2019', happ_2019$Country.or.region, happ_2019$Score, header = FALSE)
names(df_19) <- c("Year", "Country", "Happiness Score")
df_19

df_18 <- data.frame('2018', happ_2018$Country.or.region, happ_2018$Score)
names(df_18) <- c("Year", "Country", "Happiness Score")

df_17 <- data.frame('2017', happ_2017$Country, happ_2017$Happiness.Score)
names(df_17) <- c("Year", "Country", "Happiness Score")

df_16 <- data.frame('2016', happ_2016$Country, happ_2016$Happiness.Score)
names(df_16) <- c("Year", "Country", "Happiness Score")

df_15 <- data.frame('2015', happ_2015$Country, happ_2015$Happiness.Score)
names(df_15) <- c("Year", "Country", "Happiness Score")

#US data for Health and Happiness throughout the Years

US_15 <- subset(happ_2015, Country %in% c("United States"))
US_15_score <- US_15$Happiness.Score
US_15_health <- US_15$Health..Life.Expectancy.
US_15_score
US_15_health

US_16 <- subset(happ_2016, Country %in% c("United States"))
US_16_score <- US_16$Happiness.Score
US_16_health <- US_16$Health..Life.Expectancy.
US_16_score
US_16_health

US_17 <- subset(happ_2017, Country %in% c("United States"))
US_17_health <- US_17$Health..Life.Expectancy.
US_17_score <- US_17$Happiness.Score
US_17_score
US_17_health

US_18 <- subset(happ_2018, Country.or.region %in% c("United States"))
US_18_score <- US_18$Score
US_18_health <- US_18$Healthy.life.expectancy
US_18_score
US_18_health

US_19 <- subset(happ_2019, happ_2019$Country.or.region %in% c("United States"))
US_19_score <- US_19$Score
US_19_health <- US_19$Healthy.life.expectancy
US_19_score
US_19_health

Years <- list('2015', '2016', '2017', '2018', '2019')
Country <- list(US_15$Country, US_16$Country, US_17$Country, US_18$Country.or.region, US_19$Country.or.region)
Scores <- list(US_15_score, US_16_score, US_17_score, US_18_score, US_19_score)
Health <- list(US_15_health, US_16_health, US_17_health, US_18_health, US_19_health)

#combining into dataframe
df <- data.frame(unlist(Years), unlist(Country), unlist(Scores), unlist(Health)) 
df

#renaming dataframes
names(df) <- c("Year", "Country", "Happiness Score", "Health")
df


#anova test 
library(rstatix)
#res.aov <- anova_test(data = df_15, dv = 'Happiness Score', wid = 'Country', within = 'Year')
#summary(res.aov)

res.aov <- aov(df$`Happiness Score` ~ df$Health, data = df)
summary(res.aov)
#since he P value is 0.166 > 0.05, there is no significant difference between the two groups.

#checking normality
plot(res.aov, 2)

#line plot of US Happiness scores throughout the Years
ggline(df, x = "Year", y = "Happiness Score", color = "red",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))

#line plot of US Health scores throughout the Years
ggline(df, x = "Year", y = "Health", color = "red",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))



