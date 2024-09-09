library(dplyr)
ANES <- read.csv("Desktop/ANES.csv")
ANES <- ANES %>% select(V201228, V201229, V202230, V201510, V201151, V201152)
View(ANES)

#create education variable
ANES <- ANES %>% mutate(educ = NA, educ= case_when(V201510==1 | V201510==2 | V201510==3~TRUE,
        V201510==4 | V201510==5 | V201510==6 | V201510==7 | V201510==8~FALSE)) 

#create party variable
ANES <- ANES %>% mutate(party = NA, party= case_when
        (V201228 == 1 | V201228 == 3 & V201230 == 3~TRUE,
        V201228 == 2 | V201228 == 3 & V201230 == 1~FALSE))

#Biden feeling thermometer
V201151[V201151 < 0] <- NA
V201151[V201151 > 100] <- NA
summary(V201151)

#Trump feeling thermometer
V201152[V201152 < 0] <- NA
V201152[V201152 > 100] <- NA

#creating 2 datasets from reps and dems
democrats <- ANES %>% filter(party==TRUE)
republicans <- ANES %>% filter(party==FALSE)

#1c
t.test(V201152~educ, data=republicans, var.equal=TRUE)

#1d
t.test(V201152~educ, data = democrats, var.equal = TRUE)

#1e
t.test(V201151~educ, data = republicans, var.equal = TRUE)

#1f
t.test(V201151~educ, data = democrats, var.equal = TRUE)

#2b

#unpooled
2*pt(-8.075, 8147)

#pooled
2*pt(-9.499, 8147)
