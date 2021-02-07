#reading data from EPA

pm0 <- read.table("ad_viz_plotval_data.csv", comment.char = "#", header = FALSE, sep = "|", na.strings = "")
#checking dataset
dim(pm0) #dimentions
head(pm0) #first 5 rows
#get columns names from file
cnames <- readLines("ad_viz_plotval_data.csv", 1)
cnames
#spliting cnames 
cnames <- strsplit(cnames, "|", fixed = TRUE)
#asigning names to columns of dataset
names(pm0) <- cnames[[1]]
#making names
names(pm0) <- make.names(cnames[[1]])
#locking at sample values
x0 <- pm0$Sample.Value
class(x0)
str(x0)
summary(x0)
#checking persentage of missing values
mean(is.na(x0))*100

pm1 <- read.table("ad_viz_plotval_data2015.csv", comment.char = "#", header = FALSE, sep = "|", na.strings = "")
dim(pm1)

names(pm1) <- make.names(cnames[[1]])

head(pm1)

#grab pm2.5 values

x1 <- pm1$Sample.Value
str(x1)
#check na
mean(is.na(x1))*100

#compare x0 and x1
summary(x1)
summary(x0)

#taking a look
boxplot(x0,x1)

boxplot(log10(x0), log10(x1))

#negative values of pm2.5 
summary(x1)

negative <- x1 < 0

sum(negative, na.rm = TRUE)
mean(negative, na.rm = TRUE)
# we see, that negative values is only 2-3% of all values, but it is very interesting why there are
# hip1 negatives depends on dates?
dates <- pm1$Date
str(dates)
dates <- as.Date(as.character(dates), "%Y%m%d")
hist(dates, "month")
hist(dates[negative], "month")
# there no clear reletion between negative values of pm2.5 and date of observation. It seems like error
#to answer the question is pm lvl decrease or not we should choose same monitors both in 1999 and 2015
site0 <- subset(pm0, State.Code == 36, c(County.Code, Site.ID))
site1 <- subset(pm1, State.Code == 36, c(County.Code, Site.ID))
head(site0)
site0 <- paste(site0[,1], site0[,2], sep = ".")
site1 <- paste(site1[,1], site1[,2], sep = ".")
#finding intersection betw 2 vectors site0 and site1
both <- intersect(site0, site1)

pm0$country.site <- with(pm0, paste(Conty.Code, Site.ID, sep = "."))
pm1$country.site <- with(pm1, paste(Conty.Code, Site.ID, sep = "."))
# choosing monitors which exsist in both datasets with maximum number of observations
cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)

head(cnt0)
sapply(split(cnt0, cnt0$county.site), nrow)
sapply(split(cnt1, cnt1$county.site), nrow)
#choose conty = 63 , id = 2008
pm0sub <- subset(pm0, State.Code == 36 & County.COde == 63 & Site.ID == 2008)
pm1sub <- subset(pm1, State.Code == 36 & Conty.Code == 63 & Site.ID == 2008)
dim(pm0sub)
dim(pm1sub)
#plotting the result
dates1 <- pm1sub$Date
x1sub <- pm1sub$Sample.Value
dates1 <- as.Date(as.character(dates1), "%Y%m%d")
plot(dates1,x1sub)

dates0 <- pm0sub$Date
dates0 <- as.Date(as.character(dates0), "%Y%m%d")
x0sub <- pm0sub$Sample.Value
plot(dates0,x0sub)
#building panel plot
par(mfrow = c(1,2), mar = c(4,4,2,1))
plot(dates0,x0sub, pch= 20)
abline(h = median(x0sub), na.rm = T)
plot(dates1, x1sub, pch = 20)
abline(h = median(x1sub), na.rm = T)
#calc rande of both datasets
rng <- range(x0sub, x1sub, na.rm = T)
par(mfrow = c(1,2))
plot(dates0, x0sub, pch =20, ylim = rng)
abline(h = median(x0sub), na.rm = T)
plot(dates1, x1sub, pch =20, ylim = rng)
abline(h = median(x1sub), na.rm = T)
# result is avarage val of polution dercrese and critical val( max and min) decr to. Now discower whole county by plotting avarage val of polution by each state
mn0 <- with( pm0, tapply(Sample.Value, State.Code, mean, na.rm = T))
str(mn0)
summary(mn0)
mn1 <- with( pm1, tapply(Sample.Value, State.Code, mean, na.rm = T))
str(mn1)
summary(mn1)

d0 <- data.frame(state = names(mn0), mean = mn0)
d1 <- data.frame(state = names(mn1), mean = mn1)
head(d0)
head(d1)
mrg <- merge(d0, d1, by = "state")
dim(mrg)
#creating final plot
par(mfrow = c(1,1))
with(mrg, plot(rep(1999, 52), mrg[,2], xlim = c(1998,2013)))
with(mrg, points(rep(2015,52), mrg[,3]))
segments(rep(1999,52), mrg[,2], rep(2015,52), mrg[,3])

