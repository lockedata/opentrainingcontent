## ------------------------------------------------------------------------
library(DBI)
library(odbc)
library(tidyverse)

if(!require(devtools)) install.packages("devtools")
if(!require(dplyrXdf)) devtools::install_github("RevolutionAnalytics/dplyrXdf")
library(dplyrXdf)

## ------------------------------------------------------------------------
if(!require(ggplot2movies)) install.packages("ggplot2movies")
library(ggplot2movies)

imdb_movies <- rxDataStep(movies,"movies.xdf", overwrite=TRUE)

flights_xdf<-rxDataStep(nycflights13::flights,"flights.xdf", overwrite=TRUE)
library(dplyrXdf)
flights_xdf %>% 
  mutate(arr_delay=ifelse(is.na(arr_delay),0,abs(arr_delay))) %>% 
  group_by(carrier) %>% 
  summarise(flights=n(), 
            avg_arr_delay=mean(arr_delay, na.rm=TRUE), 
            planes=n_distinct(tailnum)
            ) %>% 
  View()
## ------------------------------------------------------------------------
rxGetInfo(imdb_movies, verbose = 1)

## ------------------------------------------------------------------------
imdb_movies %>% 
  filter(length < 60*5 ) %>% 
  group_by(Action) %>% 
  summarise(mean(rating)) %>% 
  View()

## ------------------------------------------------------------------------
imdb_movies %>% 
  filter(length < 60*5 ) %>% 
  group_by(Action, Romance) %>% 
  summarise(avg=mean(rating)) %>% 
  as_data_frame() %>% 
  spread(Romance,avg)

## ------------------------------------------------------------------------
imdb_movies %>% 
  rxDataStep(outFile="../samples/movies_filtered.xdf",
         rowSelection=(length < 60*5),
         transforms=list(Action=factor(Action))  , overwrite=TRUE
            ) %>% 
  head()
  

## ------------------------------------------------------------------------
imdb_movies %>% 
  filter(length < 60*5 ) %>% 
  RevoScaleR::rxHistogram(~rating | F(Action) + F(Romance), data=.)

## ------------------------------------------------------------------------
imdb_movies %>% 
  filter(length < 60*5 ) %>%
  mutate(rnd_rating=factor(floor(rating)),
         Action=factor(Action)) %>% 
  group_by(Action, rnd_rating)  %>% 
  summarise(budget=mean(budget,na.rm=TRUE)) %>%
  RevoScaleR::rxLinePlot(budget~rnd_rating | Action , data=.)

flights_xdf %>%
  rxHistogram(~month | year, data=.)
flights_xdf %>%
  rxHistogram(~day | month, data=.)

flights_xdf %>% 
  group_by(month,year) %>% 
  summarise(count=n(), days=n_distinct(day)) %>%
  mutate(dailyvol=count/days) %>% 
  arrange(month) %>% 
  rxLinePlot(dailyvol~month, data=.)
## ------------------------------------------------------------------------
imdb_movies %>% 
  sample_frac(0.7) ->
  movies_training

imdb_movies %>% 
  anti_join(movies_training) ->
  movies_testing

flights_xdf %>% 
  sample_frac(0.2) ->
  flights_testing


flights_xdf %>% 
  anti_join(flights_testing) ->
  flights_training

##Bad lines that destroy the xdf. Follow up with Hong from MSFT!!
# flights_training %>% 
#  select( -time_hour, - flight, -tailnum) %>%
#   filter(!is.na(dep_time)) %>% 
#   View()

# imdb_movies %>% 
#   # as_data_frame() %>% 
#   modelr::resample_partition(c("train"=.7,"test"=.3))
## ------------------------------------------------------------------------
create_partition <- function(xdf,
    partition_size = 0.7) {
    splitDS <- rxSplit(
    inData = xdf,
    transforms = list(traintest = factor(ifelse(
    rbinom(.rxNumRows,
    size = 1, prob = splitperc),
    "train",
    "test"
    ))),
    transformObjects = list(splitperc = partition_size),
    outFileSuffixes = c("train", "test"),
    splitByFactor = "traintest",
    overwrite = TRUE
    )
    
    return(splitDS)
    
}
                             
movies_splits <- create_partition(imdb_movies)
names(movies_splits) <- c("train", "test")

## ------------------------------------------------------------------------
rxCor(~votes + length + budget , data = movies_training)

## ------------------------------------------------------------------------
library(caret)
movies_training %>% 
  rxCor(~votes + length + budget , data = .) %>% 
  caret::findCorrelation()

## ------------------------------------------------------------------------
movies_training %>% 
  as_data_frame() %>% 
  caret::nearZeroVar()

## ------------------------------------------------------------------------
movies_training %>% 
  rxLinMod(rating~ year + budget + length + F(Comedy) + F(Action) + F(Romance) + F(Short) + F(Documentary) + F(Animation) + F(Drama), 
           data=.
           )

## ------------------------------------------------------------------------
movies_training %>% 
  rxDTree(rating~ year + budget + length + Comedy + Action + Romance + Short + Documentary + Animation + Drama, 
           data=.
           ) -> 
  movies_rdtree

movies_rdtree

flights_training %>% 
  mutate(carrier=as.factor(carrier)) %>% 
  rxLinMod(arr_delay~month+day+hour+distance+carrier, 
           data=.)
## ------------------------------------------------------------------------
library(RevoTreeView)
movies_rdtree %>% 
  rxAddInheritance() %>% 
  plot()

## ------------------------------------------------------------------------
movies_training %>% 
  rxLogit(Comedy~ rating +year + budget + length + F(Action) + F(Romance) + F(Short) + F(Documentary) + F(Animation) + F(Drama), 
           data=.
           ) ->
  movies_logit

movies_logit

## ------------------------------------------------------------------------
movies_training %>% 
  rxDTree(Comedy~ rating +year + budget + length +  Action + Romance + Short + Documentary + Animation + Drama, 
           data=. , method = "class"
           ) ->
  movies_dtree

movies_dtree

## ------------------------------------------------------------------------
movies_dtree %>% 
  rxAddInheritance() %>% 
  plotcp()

## ------------------------------------------------------------------------
movies_rdtree %>% 
  rxDTreeBestCp() ->
  best_cp

movies_dtree %>% 
 prune(cp=best_cp) ->
  movies_dtree

movies_dtree %>% 
  rxAddInheritance() %>% 
  plot()

text(rxAddInheritance(movies_dtree))
## ------------------------------------------------------------------------
library(RevoTreeView)
movies_dtree %>% 
  createTreeView() %>% 
  plot()

## ------------------------------------------------------------------------
movies_training %>% 
  rxBTrees(Comedy~ rating +year + budget + length +  Action + Romance + Short + Documentary + Animation + Drama, 
           data=.
           )

## ------------------------------------------------------------------------
library(RevoTreeView)
movies_training %>% 
  rxDTree(Comedy~ rating +year + budget + length ,
           data=., class
           ) -> 
  movies_dtree

movies_dtree %>% 
  rxAddInheritance() %>% 
  plot()

## ------------------------------------------------------------------------
movies_training %>% 
  rxKmeans(~ rating +year  + length + Comedy+ Action + Romance + Short + Documentary + Animation + Drama,
           data=.,numClusters = 10) ->
  kmeansmod

movies_training %>% 
  rxKmeans(~ rating +year  + length ,
           data=.,numClusters = 5, outFile="clusters.xdf", overwrite=TRUE) 

movies_clustered<-rxDataStep("clusters.xdf")

# library(plotly)
# movies_training %>% 
#   mutate(id=seq_len(.rxNumRows)) %>% 
#   inner_join({
#     movies_clustered %>% 
#       mutate(id=1:.rxNumRows)
#   }) %>% 
#   View()
#   ggplot(aes(x=rating, y=year, z= length)) +
#   geom_point()
## ------------------------------------------------------------------------
movies_testing %>% 
  rxPredict(movies_rdtree, .) 

## ------------------------------------------------------------------------
movies_testing %>% 
  rxPredict(movies_rdtree, .) ->
  movies_testing

plot(movies_testing$rating, movies_testing$rating_Pred)
movies_testing %>% 
  as_data_frame() %>% 
  ggplot(aes(x=rating,y=rating_Pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, colour="red")

## ------------------------------------------------------------------------
movies_testing %>% 
  rxPredict(movies_dtree, data=., 
            predVarNames="Comedy_Pred",
             type="vector") %>% 
  rxRocCurve(actualVarName="Comedy", predVarNames = "Comedy_Pred", data=.)


## ------------------------------------------------------------------------
movies_testing %>% 
  rxLorenz("Comedy_Pred","Comedy", data=.) %>% 
  plot()

## ------------------------------------------------------------------------
movies_dtree %>% 
  rxVarImpPlot()

