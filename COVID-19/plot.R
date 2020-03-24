library(R0)
library(tidyverse)
# Load the data
cases_by_country <- read.csv( url('https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_19-covid-Confirmed.csv'), header = TRUE)
# Exclude 'Others'
cases_by_country <- subset(cases_by_country, Country.Region != 'Others')
# Aggregate by country for last day available
total_by_country <- tapply( cases_by_country[,ncol(cases_by_country)], cases_by_country$Country.Region, sum )
# Get 'top' 18 countries with most cases
countries <- rownames( sort(total_by_country, decreasing = TRUE)[1:18] )
# set up plot window / panels
par(mfrow=c(3,6))
# calculate fit for each of the top 18 countrys
# using R0 package and sequential bayesian method
# See https://bmcmedinformdecismak.biomedcentral.com/articles/10.1186/1472-6947-12-147
gt <- generation.time('gamma', c(1, 3))
for( country in countries ){
   # extract data for country
   ts <- apply( subset(cases_by_country, Country.Region == country)[,5:ncol(cases_by_country)], 2, sum)
   # trim to period where we have cases
   ts <- ts[which( ts != 0)[1]:length(ts)] 
   # estimate R0
   SB <- est.R0.SB( ts , gt)
   # get lengths used in plotting
   n <- length(SB$R)
   # plot fitted R value
   plot( 1:n, SB$R, ylim=c(0,6), main = country, type='l', xlab='Time', ylab='R(t)')
   # now confidence intervals
   polygon( c(1:n,n:1), c(SB$conf.int[,1], rev(SB$conf.int[,2])), col=rgb(0.3,0.3,0.3,0.5), border = NA)
   # add R = 1 line
   abline(h=1,lty=2)
   # now other lines to help read
   abline(h=0:10, lty=3)
}

