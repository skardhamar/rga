#rm(rga, ga)

rga.open(instance = 'ga')

profile <- 20108692
from <- ga$getFirstDate(profile)
from.one <- one$getFirstDate(one.profile)

to <- ga$getData(profile, max=10000, start.date=from, dimensions="ga:date,ga:medium,ga:source")

rga.open(instance = 'one')
one.profile <- 1863364
from.one <- one$getFirstDate(one.profile)

se2 <- one$getData(one.profile, batch = T, walk = T,
                  start.date='2012-10-01',
                  end.date='2012-10-13',
                  dimensions="ga:keyword",
                  filter="ga:country==Sweden;ga:medium==organic",
                  metrics='ga:visits,ga:transactions')


sec <- ga$getData(profile, batch=T, max=2, start.date=from, dimensions="ga:date,ga:medium,ga:source")

batch <- 10;
runs <- 4;
for (i in 0:(runs - 1)) {
  start <- i * batch + 1;
  end <- start + batch - 1;
  cat(paste('From ', start, ' to ', end, '\n'));
}