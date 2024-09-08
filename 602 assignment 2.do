use "C:\Users\lrpalladino\Downloads\ANES2016 for Assignment 2.dta"

* feeling thermometer for the rich

* clean up data 
tab V162105
drop if V162105 >100
drop if V162105 <0
tab V162105
summarize V162105, detail

* clean up data for gender
tab V161342
drop if V161342==-9

* break feeling thermometer for the rich up by gender
by V161342, sort : summarize V162105, detail

* feeling thermometer for the poor

* clean up the data
drop if V162099 >100
drop if V162099 <0
tab V162099
summarize V162099, detail

* break feeling thermometer for the poor up by gender
by V161342, sort : summarize V162099, detail

*correlate feeling thermometers 
correlate V162099 V162105

* make a scatter plot, no jitter
scatter V162105 V162099, ytitle(Feeling Thermometer for the Rich) xtitle(Feeling Thermometer for the Poor)

* smake a scatter plot with jitter
scatter V162105 V162099, jitter(5) ytitle(Feeling Thermometer for the Rich) xtitle(Feeling Thermometer for the Poor)

* create an education variable with 5 categories

* clean up data for education
drop if V161270 >89
drop if V161270 <0

* make new education labels
gen educ=V161270
recode educ 1=1 2=1 3=1 4=1 5=1 6=1 7=1 8=1 9=2 10=3 11=3 12=3 13=4 14=5 15=5 16=5
label define educlabel 1 "Less than HS diploma" 2 "High School Diploma or Equivalent" 3 "Some College" 4 "4 Year Undergraduate Degree" 5 "Graduate Degree"
label values educ educlabel
tab educ

* create vote variable with 4 categories
gen votechoice = 1 if V162031x==1 & V162062x==2
replace votechoice = 2 if V162031x==1 & V162062x==1
replace votechoice = 3 if V162031x==1 & V162062x==3
replace votechoice = 3 if V162031x==1 & V162062x==4
replace votechoice = 3 if V162031x==1 & V162062x==5
replace votechoice = 4 if V162031x==0 & V162062x==-1
tab votechoice

*make new vote choice labels
label define votelab 1 "Voted for Trump" 2 "Voted for Clinton" 3 "Voted for Other" 4 "Did Not Vote"
label values votechoice votelab
tab votechoice

*crosstab vote choice and education
tab educ votechoice, row nofreq

*crosstab for educ AND votechoice %s
tab educ votechoice, cell