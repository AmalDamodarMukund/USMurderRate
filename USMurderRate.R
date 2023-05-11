str(state.x77)
class(state.x77)

#convert matrix to dataframe
states <- as.data.frame(state.x77)
str(states)


#pre-processing

#plotting and analyzing
attach(states)

pairs(states)

scatter.smooth(x = Population,
               y = Murder,
               main = "murder rate per population"
                 )

cor(Murder, Population)
#-0.2 < x < 0.2 then most of the response variable is not predicted by the predictor variable


#outlier

boxplot(Population, main = "population" , 
        sub = paste("Outlier rows:", boxplot.stats(Population)$out))

outlier_population <- boxplot.stats(Population)$out
outlier_population

states$modified_population <- subset(states, 
                                     states$Population != 21198
                                     & states$Population != 11197
                                     & states$Population != 18076
                                     & states$Population != 11860
                                     & states$Population != 12237  
                                       )



