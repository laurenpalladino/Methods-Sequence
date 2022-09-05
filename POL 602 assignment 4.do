use "\\mysbfiles.campus.stonybrook.edu\lrpalladino\anes_timeseries_2020_stata_20210719.dta" 
gen educ = .
replace educ = 0 if V201510==1
replace educ = 0 if V201510==2
replace educ = 0 if V201510==3
replace educ = 1 if V201510==4
replace educ = 1 if V201510==5
replace educ = 1 if V201510==6
replace educ = 1 if V201510==7
replace educ = 1 if V201510==8
tab educ
ttest V201151, by(educ)
ttest V201152, by(educ)