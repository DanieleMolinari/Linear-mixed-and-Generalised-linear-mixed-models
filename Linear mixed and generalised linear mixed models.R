data <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/rioolympics.csv"))
set.seed(10)
#checking the structure of data
str(data)
#some variables need to be transformed into numeric and factors
data$gdp16 <- as.numeric(as.character(data$gdp16))
data$gdp00 <- as.numeric(as.character(data$gdp00))
data$country <- as.character(data$country)
data$soviet <- as.factor(data$soviet)
data$comm <- as.factor(data$comm)
data$muslim <- as.factor(data$muslim)
data$oneparty <- as.factor(data$oneparty)
data$ssoviet <- as.factor(data$soviet)

#checking values of variables
summary(data)
#totgold and totmedals won in a year will be removed as they are the sum of medals won by all 
#countries in a year
data <- data[, -c(27:36)]
#the wide range of some variables suggest to log-transform them
#athletes has some values equal to zero so I add a small amount in oreder to not get any NANs
data$athletes00[data$athletes00==0] <- 0.1
data$athletes04[data$athletes04==0] <- 0.1
data$athletes08[data$athletes08==0] <- 0.1
data$athletes12[data$athletes12==0] <- 0.1
data$athletes16[data$athletes16==0] <- 0.1

data[3:12] <- log(data[, 3:12])
data[28:32] <- log(data[, 28:32])

#The data are divided into training and test data sets
#I will reshape the data set in order to use the whole data set as train set
#I first devide the data set in years and then I will put them together again when I create the 
#train set
data00 <- data[,(names(data) %in% c("gdp00", "pop00", "gold00", "tot00", "totgold00", 
                                    "totmedals00", "athletes00", "country", "country.code",
                                    "soviet", "comm", "muslim", "oneparty", "altitude",
                                    "host"))]
data00$year <- rep(2000, 108)
data04 <- data[,(names(data) %in% c("gdp04", "pop04", "gold04", "tot04", "totgold04", 
                                    "totmedals04", "athletes04", "country", "country.code",
                                    "soviet", "comm", "muslim", "oneparty", "altitude",
                                    "host"))]
data04$year <- rep(2004, 108)
data08 <- data[,(names(data) %in% c("gdp08", "pop08", "gold08", "tot08", "totgold08", 
                                    "totmedals08", "athletes08", "country", "country.code",
                                    "soviet", "comm", "muslim", "oneparty", "altitude",
                                    "host"))]
data08$year <- rep(2008, 108)
data12 <- data[,(names(data) %in% c("gdp12", "pop12", "gold12", "tot12", "totgold12", 
                                    "totmedals12", "athletes12", "country", "country.code",
                                    "soviet", "comm", "muslim", "oneparty", "altitude",
                                    "host"))]
data12$year <- rep(2012, 108)
# I now create the train set. What I want is having the variables with all observations from all years
# and not divided by years. This will also make the prediction phase easier.
train.data <- data.frame(c(data00$country, data04$country, data08$country, data12$country),
                         c(data00$gdp00, data04$gdp04, data08$gdp08, data12$gdp12),
                         c(data00$pop00, data04$pop04, data08$pop08, data12$pop12),
                         c(data00$soviet, data04$soviet, data08$soviet, data12$soviet),
                         c(data00$comm, data04$comm, data08$comm, data12$comm),
                         c(data00$muslim, data04$muslim, data08$muslim, data12$muslim),
                         c(data00$oneparty, data04$oneparty, data08$oneparty, data12$oneparty),
                         c(data00$gold00, data04$gold04, data08$gold08, data12$gdp12),
                         c(data00$tot00, data04$tot04, data08$tot08, data12$tot12),
                         c(data00$altitude, data04$altitude, data08$altitude, data12$altitude),
                         c(data00$athletes00, data04$athletes04, data08$athletes08, data12$athletes12),
                         c(data00$host, data04$host, data08$host, data12$host),
                         c(data00$year, data04$year, data08$year, data12$year))
colnames(train.data) <- c("country","gdp", "pop", "soviet", "comm", "muslim", "oneparty", "gold", "tot",
                          "altitude", "athletes", "host", "year")
train.data$country <- as.factor(train.data$country)

#I create the test data and I will rename the columns eliminating the year from the names in order
#to have the same variables' names in the test set and train set.  
test.data <- data[,(names(data) %in% c("gdp16", "pop16", "gold16", "tot16", "totgold16", 
                                       "totmedals16", "athletes16", "country", "country.code",
                                       "soviet", "comm", "muslim", "oneparty", "altitude",
                                       "host"))]
colnames(test.data) <- c("country", "country.code", "gdp", "pop", "soviet", "comm", "muslim", 
                         "oneparty", "gold", "tot", "altitude", "athletes", "host")


# I now need to check NAs.
# I want to see how many Nas there are in the two data sets. If there are not too many I will 
# eliminate them.
sum(is.na(train.data))
#There is only one NA, so it wil be removed
train.data <- na.omit(train.data)
sum(is.na(test.data))
#There are only two NAs so they will be removed
test.data <- na.omit(test.data)

library(ggplot2)
library(ggrepel)
ggplot(train.data, aes(year, tot)) +
  geom_point(aes(colour = country), show.legend = FALSE) +
  geom_path(aes(group = country, colour = country), show.legend = FALSE) +
  geom_text(aes(label = country, colour = country), show.legend = FALSE, check_overlap = TRUE, size = 3.5)


library(lme4)
#I will start with a general LMM
LMM1 <- lmer(tot ~ gdp + pop + athletes + host + soviet + comm + 
                 muslim + oneparty + (1|country), data = train.data)
summary(LMM1)
#checking the significance of variables
confint(LMM1)
confint(LMM1, method = "boot")

#take away gdp
LMM2 <- lmer(tot ~ pop + athletes + host + soviet + comm + 
                   muslim + oneparty + (1|country), data = train.data)
summary(LMM2)
confint(LMM2)
confint(LMM2, method = "boot")

#take away soviet
LMM3 <- lmer(tot ~ pop + athletes + host + comm + 
                   muslim + oneparty + (1|country), data = train.data)
summary(LMM3)
#checking the significance of variables
confint(LMM3)
confint(LMM3, method = "boot")

#take away muslim
LMM4 <- lmer(tot ~ pop + athletes + host + comm + 
                   oneparty + (1|country), data = train.data)
summary(LMM4)
#checking the significance of variables
confint(LMM4)
confint(LMM4, method = "boot")

#I create a model considering random effect for intercept and slope uncorrelated
LMM5 <- lmer(tot ~ pop + athletes + host + comm +oneparty + (1|country) 
                 + (0+athletes|country), data = train.data)

#I create a model considering random effect for intercept and slope correlated
LMM6 <- lmer(tot ~ pop + athletes + host + comm + 
               oneparty + (1+athletes|country), data = train.data)

#compare all the models created
anova(LMM1, LMM2, LMM3, LMM4)

#diagnostic plots
par(mfrow = c(1,2))
qqnorm(resid(LMM4), main = "")
qqline(resid(LMM4))
plot(fitted(LMM4), resid(LMM4), xlab = "Fitted", ylab = "Residuals")
abline(0,0)


library(lme4)
#I create a general GLMM
GLMM1 <- glmer(tot ~ gdp + pop + athletes + host + soviet + comm + 
                 muslim + oneparty + (1|country), data = train.data, family = poisson)
summary(GLMM1)

#take away gdp
GLMM2 <- glmer(tot ~ pop + athletes + host + soviet + comm + 
                 muslim + oneparty + (1|country), data = train.data, family = poisson)
summary(GLMM2)

#take away soviet
GLMM3 <- glmer(tot ~ pop + athletes + host + comm + 
                 muslim + oneparty + (1|country), data = train.data, family = poisson)
summary(GLMM3)

#take away oneparty
GLMM4 <- glmer(tot ~ pop + athletes + host + comm + 
                 muslim + (1|country), data = train.data, family = poisson)
summary(GLMM4)

#take away muslim
GLMM5 <- glmer(tot ~ pop + athletes + host + comm + 
                  (1|country), data = train.data, family = poisson)
summary(GLMM5)

#I create a model considering random effect for intercept and slope uncorrelated
GLMM6 <- glmer(tot ~ pop + athletes + host + comm + (1|country)
               + (0+athletes|country), data = train.data, family = poisson)
summary(GLMM6)

#I create a model considering random effect for intercept and slope correlated
GLMM7 <- glmer(tot ~ pop + athletes + host + comm + 
                 (1+athletes|country), data = train.data, family = poisson)
summary(GLMM7)

#I compare mdoel 5 and 7
anova(GLMM7, GLMM5)

#diagnostic plots
par(mfrow = c(1,2))
qqnorm(resid(GLMM5), main = "")
qqline(resid(GLMM5))
plot(fitted(GLMM5), resid(GLMM5), xlab = "Fitted", ylab = "Residuals")
abline(0,0)


#I am going to use the two models I compared in assignment 1 and I will
#compare them with models of this assignment
library(MASS)

model3 <- glm(tot ~ pop*muslim + athletes*muslim, family = poisson, data = train.data)
model4 <- glm.nb(tot ~ athletes + pop + comm, data = train.data)

#making predictions with models of assignment 1 and 3
test.data$comm <- as.numeric(test.data$comm)
test.data$muslim <- as.numeric(test.data$muslim)
test.data$host <- as.numeric(test.data$host)
p1 <- predict(LMM4, newdata = test.data, type = "response")
p2 <- predict(GLMM5, newdata = test.data, type = "response")
pred.model.3 <- predict(model3, newdata = test.data, type = "response")
pred.model.4 <- predict(model4, newdata = test.data, type = "response")

#I calculate the root mean squared error for the four models
RMSEp1 <- sqrt(mean((p1 - test.data$tot)^2))
RMSEp2 <- sqrt(mean((p2 - test.data$tot)^2))
RMSE.model.3 <- sqrt(mean((pred.model.3 - test.data$tot)^2))
RMSE.model.4 <- sqrt(mean((pred.model.4 - test.data$tot)^2))

#I create a matrix with RMSE results
RMSE.matrix <- matrix(c(RMSE.model.3, RMSE.model.4, RMSEp1, RMSEp2), 1, 4)
colnames(RMSE.matrix) <- c("Poisson", "Negative Binomial", "LMM", "GLMM")
RMSE.matrix

#I now create plots to compare the performance of the the models
#the first one shows the number of medals vs athletes and I will
#superimpose the trend of the four predictions
graph <- data.frame("poisson" = pred.model.3,  "NegativeBinomial" = pred.model.4, 
                    "LMM" = p1, "GLMM" = p2, "country" = test.data$country,
                    "tot" = test.data$tot, "athletes" = test.data$athletes)
colours <- c("poisson" = "#0CFB01", "Negative Binomial" = "#013EFB", "LMM" = "#FB0145", "GLMM" = "#F7FB01")
ggplot(graph, aes(athletes, tot)) +
  geom_point() +
  geom_line(aes(y=poisson, colour = "poisson")) +
  geom_line(aes(y=NegativeBinomial, colour = "Negative Binomial" )) + 
  geom_line(aes(y=LMM, colour = "LMM")) +
  geom_line(aes(y=GLMM, colour = "GLMM")) +
  labs(x = "Number of Athletes",
       y = "Total Medals",
       color = "Legend") +
  scale_color_manual(values = colours)

#this plot is similar to the previous one but I will compare only
#the final models chosen in assignment 1 and 3
colours2 <- c("poisson" = "#0195FB", "GLMM" = "#DD01FB")
ggplot(graph, aes(athletes, tot)) +
  geom_point() +
  geom_line(aes(y=poisson, colour = "poisson")) +
  geom_line(aes(y=GLMM, colour = "GLMM")) +
  labs(x = "Number of Athletes",
       y = "Total Medals",
       color = "Legend") +
  scale_color_manual(values = colours2)

#this model again compare the four models performance but it has fitted values vs observed values
colours3 <- c("LMM" = "blue", "GLMM" = "red", "GLM" = "yellow", "GLM.NB" = "green")
model_test_LMM <- data.frame(cbind(observed = test.data$tot, fitted = p1))
plot_check <- ggplot(model_test_LMM, aes(observed, fitted)) +
  geom_point(aes(colour = "LMM"), alpha = 0.5) +
  geom_abline() +
  xlab("Observed Value") +
  ylab("Fitted Value")
  

model_test_GLM <- data.frame(cbind(observed = test.data$tot, fitted = pred.model.3))
plot_check <- plot_check +
  geom_point(data = model_test_GLM, aes(observed, fitted, colour = "GLM"), alpha = 0.5)

model_test_GLM.NB <- data.frame(cbind(observed = test.data$tot, fitted = pred.model.4))
plot_check <- plot_check +
  geom_point(data = model_test_GLM.NB, aes(observed, fitted, colour = "GLM.NB"), alpha = 0.5)

model_test_GLMM <- data.frame(cbind(observed = test.data$tot, fitted = p2))
plot_check <- plot_check +
  geom_point(data = model_test_GLMM, aes(observed, fitted, colour = "GLMM"), alpha = 0.5)

plot_check <- plot_check +
  labs(colour = "legend") +
  scale_color_manual(values = colours3)
plot_check

