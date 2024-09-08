use "\\mysbfiles.campus.stonybrook.edu\lrpalladino\anes_timeseries_2020_stata_20210719.dta" 

**1= democrat, 2= independent or other, 3= republican (removed if no response or no party ID)
gen party = .
replace party = 1 if V201228==1 & V201229==1
replace party = 1 if V201228==1 & V201229==2
replace party = 1 if V201228==3 & V201230==3
replace party = 2 if V201228==3 & V201230==2
replace party = 2 if V201228==0 & V201230==2
replace party = 3 if V201228==2 & V201229==1
replace party = 3 if V201228==2 & V201229==2
replace party = 3 if V201228==3 & V201230==1
tab party

**renaming and cleaning ANES data to merge with state data
rename V201014b fips
replace fips = . if fips >56
replace fips = . if fips <1

**merge ANES with state data
merge m:1 fips using "stateinfo.dta"

**create a categorizing variable
gen whitegroups = .
replace whitegroups = 1 if whitepct <50
replace whitegroups = 2 if whitepct <75 & whitepct >50.1
replace whitegroups = 3 if whitepct >75.1

**cross tab to make sure it categorizes correctly
tab fips whitegroups

**add sample weight and cross tab with party (using cell %s)
svyset[pweight=V200010a]
svy: tab party whitegroups, cell