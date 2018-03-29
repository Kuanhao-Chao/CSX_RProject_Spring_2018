
library(tidyverse)

diamonds

ggplot(data = diamonds, aes(x = cut)) + geom_bar(fill = "yellow", color = "black")

ggplot( data = diamonds, aes(x = price)) + geom_histogram(binwidth = 10)

ggplot( data = diamonds, aes(x = table, y = depth)) + geom_point()

ggplot(data = diamonds, aes(x = clarity, y = depth)) + geom_boxplot()

library(ggplot2)
library(GGally)
library(scales)
library(memisc)

# 從數據集獲取10000個樣本數據進行分析
set.seed(20022012)
# lists that may contain the variables 'continuous', 'combo', 'discrete', and 'na'
diamond_samp <- diamonds[sample(1:length(diamonds$price), 10000), ]
ggpairs(diamond_samp,lower= list(continuous = wrap("points", shape = I('.'))),
        upper = list(combo = wrap("box", outlier.shape = I('.'))))

library(plotly)
packageVersion('plotly')

set.seed(1234)

dat <- data.frame(cond = factor(rep(c("A", "B"), each=200)), rating = c(rnorm(200)))

dat

p <- ggplot(dat, aes(x = rating)) + 
  geom_histogram(binwidth = .25, fill = "grey", color = "black") + 
  geom_vline(aes(xintercept = mean(rating, na.rm=T)), color = "red", linetype = "dashed", size = .5)
## na.rm=T ==> remove na value

p <- ggplotly(p)

p

require(graphics)

typeof(UCBAdmissions)

UCBAdmissions[]

UCBAdmissions

apply(UCBAdmissions, c(1, 2), sum)

## Data aggregated over departments
# iterate through data
apply(UCBAdmissions, c(1, 2), sum)
mosaicplot(apply(UCBAdmissions, c(1, 2), sum),
           main = "Student admissions at UC Berkeley")
## Data for individual departments
opar <- par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))
for(i in 1:6)
  mosaicplot(UCBAdmissions[,,i],
    xlab = "Admit", ylab = "Sex",
    main = paste("Department", LETTERS[i]))
mtext(expression(bold("Student admissions at UC Berkeley")),
      outer = TRUE, cex = 1.5)
par(opar)

require(stats)
require(graphics) # for mosaicplot

summary(esoph)

model1 <- glm(cbind(ncases, ncontrols) ~ agegp + tobgp * alcgp,
              data = esoph, family = binomial())

model1

anova(model1)

model2 <- glm(cbind(ncases, ncontrols) ~ agegp + unclass(tobgp)
                                         + unclass(alcgp),
              data = esoph, family = binomial())

model2

summary(model2)

ttt <- table(esoph$agegp, esoph$alcgp, esoph$tobgp)
o <- with(esoph, order(tobgp, alcgp, agegp))
ttt[ttt == 1] <- esoph$ncases[o]
tt1 <- table(esoph$agegp, esoph$alcgp, esoph$tobgp)
tt1[tt1 == 1] <- esoph$ncontrols[o]
tt <- array(c(ttt, tt1), c(dim(ttt),2),
            c(dimnames(ttt), list(c("Cancer", "control"))))
mosaicplot(tt, main = "esoph data set", color = TRUE)

library(ggplot2)
library(maptools)
library(rgeos)
library(Cairo)
library(ggmap)
library(scales)
library(RColorBrewer)
library(mapproj)

map <- get_map(location = 'Taiwan', zoom = 7)
ggmap(map)

# use language to set the word on the map
map <- get_map(location = 'Taiwan', zoom = 7,
  language = "zh-TW")
ggmap(map)

# to set accurate location
map <- get_map(location = c(lon = 121.533937, lat = 25.03933), zoom = 12, language = "zh-TW")

ggmap(map)

# Different type
map <- get_map(location = c(lon = 121.533937, lat = 25.03933), zoom = 13, language = "zh-TW", maptype = "roadmap")
ggmap(map)

# Great to show data on the map
map <- get_map(location = c(lon = 121.533937, lat = 25.03933), zoom = 13, language = "zh-TW", maptype = "toner-lite")
ggmap(map)

data <- read.csv("UV_20180319014346.csv")

data

lon.deg <- sapply((strsplit(as.character(data$WGS84Lon), ",")), as.numeric)
data$lon <- lon.deg[1, ] + lon.deg[2, ]/60 + lon.deg[3, ]/3600
lat.deg <- sapply((strsplit(as.character(data$WGS84Lat), ",")), as.numeric)
data$lat <- lat.deg[1, ] + lat.deg[2, ]/60 + lat.deg[3, ]/3600

lon.deg

data$lat

map <- get_map(location = 'Taiwan', zoom = 7)

ggmap(map) + geom_point(aes(x = lon, y = lat, size = UVI), data = data)

ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = UVI), data = data) +
  facet_grid( ~ PublishAgency)

d <- function(x=-95.36, y=29.76, n,r,a){
  round(data.frame(
    lon = jitter(rep(x,n), amount = a),
    lat = jitter(rep(y,n), amount = a)
  ), digits = r)
}
df <- d(n = 50,r = 3,a = .3)
map <- get_googlemap(markers = df, path = df,, scale = 2)
ggmap(map)

mu <- c(-95.3632715, 29.7632836)
nDataSets <- sample(4:10,1)
chkpts <- NULL
for(k in 1:nDataSets){
  a <- rnorm(2); b <- rnorm(2);
  si <- 1/3000 * (outer(a,a) + outer(b,b))
  chkpts <- rbind(chkpts,
    cbind(MASS::mvrnorm(rpois(1,50), jitter(mu, .01), si), k))
}
chkpts <- data.frame(chkpts)
names(chkpts) <- c("lon", "lat","class")
chkpts$class <- factor(chkpts$class)
qplot(lon, lat, data = chkpts, colour = class)

ggmap(get_map(maptype = "satellite"), extent = "device") +
stat_density2d(aes(x = lon, y = lat, colour = class), data = chkpts, bins = 5)

# only violent crimes
violent_crimes <- subset(crime,
    offense != "auto theft" &
    offense != "theft" &
    offense != "burglary"
    )

# rank violent crimes
violent_crimes$offense <-
factor(violent_crimes$offense,
    levels = c("robbery", "aggravated assault",
      "rape", "murder")
    )

# restrict to downtown
violent_crimes <- subset(violent_crimes,
    -95.39681 <= lon & lon <= -95.34188 &
    29.73631 <= lat & lat <=  29.78400
    )

library(grid)
theme_set(theme_bw(16))
HoustonMap <- qmap("houston", zoom = 14, color = "bw")

# a contour plot
HoustonMap +
stat_density2d(aes(x = lon, y = lat, colour = offense),
    size = 3, bins = 2, alpha = 3/4, data = violent_crimes) +
scale_colour_discrete("Offense", labels = c("Robery","Aggravated Assault","Rape","Murder")) +
theme(
    legend.text = element_text(size = 15, vjust = .5),
    legend.title = element_text(size = 15,face="bold"),
    legend.key.size = unit(1.8,"lines")
    )

# 二維的 histogram
HoustonMap +
stat_bin2d(aes(x = lon, y = lat, colour = offense, fill = offense),
    size = .5, bins = 30, alpha = 2/4, data = violent_crimes) +
scale_colour_discrete("Offense",
    labels = c("Robery","Aggravated Assault","Rape","Murder"),
    guide = FALSE) +
scale_fill_discrete("Offense", labels = c("Robery","Aggravated Assault","Rape","Murder")) +
theme(
    legend.text = element_text(size = 15, vjust = .5),
    legend.title = element_text(size = 15,face="bold"),
    legend.key.size = unit(1.8,"lines")
    )

HoustonMap +
stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
    size = 2, bins = 4, data = violent_crimes, geom = "polygon") +
scale_fill_gradient("Violent\nCrime\nDensity") +
  scale_alpha(range = c(.4, .75), guide = FALSE) +
guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))

houston <- get_map("houston", zoom = 14)
overlay <- stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), bins = 4, geom = "polygon", data = violent_crimes)

HoustonMap +
stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
    bins = 4, geom = "polygon", data = violent_crimes) +
scale_fill_gradient("Violent\nCrime\nDensity") +
scale_alpha(range = c(.4, .75), guide = FALSE) +
guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10)) +
inset(
    grob = ggplotGrob(ggplot() + overlay +
      scale_fill_gradient("Violent\nCrime\nDensity") +
      scale_alpha(range = c(.4, .75), guide = FALSE) +
      theme_inset()
      ),
    xmin = attr(houston,"bb")$ll.lon +
    (7/10) * (attr(houston,"bb")$ur.lon - attr(houston,"bb")$ll.lon),
    xmax = Inf,
    ymin = -Inf,
    ymax = attr(houston,"bb")$ll.lat +
    (3/10) * (attr(houston,"bb")$ur.lat - attr(houston,"bb")$ll.lat)
    )

df <- data.frame(
    x = rnorm(10*100, -95.36258, .05),
    y = rnorm(10*100,  29.76196, .05),
    year = rep(paste("year",format(1:10)), each = 100)
    )
for(k in 0:9){
  df$x[1:100 + 100*k] <- df$x[1:100 + 100*k] + sqrt(.05)*cos(2*pi*k/10)
    df$y[1:100 + 100*k] <- df$y[1:100 + 100*k] + sqrt(.05)*sin(2*pi*k/10)
}

ggmap(get_map(),
    base_layer = ggplot(aes(x = x, y = y), data = df)) +
stat_density2d(aes(fill = ..level.., alpha = ..level..),
    bins = 4, geom = "polygon") +
scale_fill_gradient2(low = "white", mid = "orange", high = "red", midpoint = 10) +
  scale_alpha(range = c(.2, .75), guide = FALSE) +
facet_wrap(~ year)
