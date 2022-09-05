library(dplyr)
library(ggplot2)

ANES20 <- read.csv("Desktop/ANES.csv")
ANES20 <- ANES20 %>% select(V201215, V201157)
View(ANES20)

#clean up feeling thermometer
V201157[V201157 < 0] <- NA
V201157[V201157 > 100] <- NA
summary(V201157)
trumpft <- V201157

#clean up perceive honest (1=honest, 5=not honest at all)
V201215[V201215 < 0] <- NA
V201215[V201215 > 5] <- NA
summary(V201215)
trumphonest <- V201215

#fitting a straight line
ggplot(ANES20 %>% drop_na(V201215, V201157), aes(x=trumpft, y=trumphonest)) +
  geom_smooth(se = FALSE, method = lm)

#window 0.9
window9 <- ggplot(ANES20 %>% drop_na(V201215, V201157), aes(x=trumpft, y=trumphonest)) +
  geom_smooth(se = FALSE, method = "loess", span=0.9)

print(window9 + ggtitle("Feeling Thermometer for the Republican Party and Perceptions of Trump's Honesty") +
        labs(y="Donald Trump is Honest", x="Feeling Thermometer for the Republican Party"))

#window 0.1
window1 <- ggplot(ANES20 %>% drop_na(V201215, V201157), aes(x=trumpft, y=trumphonest)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.1, method.args= list(surface="direct",cell=0))

print(window1 + ggtitle("Feeling Thermometer for the Republican Party and Perceptions of Trump's Honesty") +
        labs(y="Donald Trump is Honest", x="Feeling Thermometer for the Republican Party"))

#question 2

incumbents <- matrix(c(28, 6, 29, 53), nrow=2, byrow = TRUE,
              dimnames = list(c("heard of non-incumbent", "not heard of non-incumbent"),
              c( "heard of incumbent", "not heard of incumbent")))
incumbents <-as.table(incumbents)
incumbents

chisq_incumbents <-chisq.test(incumbents, correct = FALSE)
chisq_incumbents
