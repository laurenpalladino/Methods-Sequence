#things to know for finals

source("http://mperess.cc.stonybrook.edu/r0.r")
library(haven)
legprod <- read_dta("Downloads/legprod.dta")
View(legprod)
attach(legprod)


#dickey fuller test
adf.test0(Gridlock3, Obs)

#augmented df test
adf.test0(Gridlock3, Obs, k=trunc((length(Obs)-1)^(1/3)))

#phillips perron
pp.test0(Gridlock3, Obs)

#kpss
kpss.test0(Gridlock3, Obs)
