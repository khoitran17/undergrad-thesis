*Parental Extended Coverage - MAIN RESULTS
foreach x in privhi pubhi insured group nongroup {

di "Full Sample"
di "Mean for Eligibles only"
summ `x' if  a_age>=19 & a_age<=24  & year>=2000 & eligible==1
di "Mean for Entire sample"
summ `x' if  a_age>=19 & a_age<=24  & year>=2000
di "DD using full sample"
xi: reg `x' law i.stfips i.year i.a_age married student female ur povratio povratio2 if a_age>=19 & a_age<=24  & year>=2000, cluster(stfips)
di "DD using eligibles only"
xi: reg `x' law i.stfips i.year i.a_age married student female ur povratio povratio2 if a_age>=19 & a_age<=24  & year>=2000 & eligible==1, cluster(stfips)
di "DDD, eligibles vs. ineligibles in full sample"
xi: reg `x' i.eligible*law i.eligible*i.stfips i.eligible*i.year i.eligible*i.a_age i.eligible*married i.eligible*student i.eligible*female i.eligible*ur i.eligible*povratio i.eligible*povratio2 if a_age>=19 & a_age<=24  & year>=2000, cluster(stfips)

di "Non-Students only"
di "Mean for Eligible Non-students only"
summ `x' if  a_age>=19 & a_age<=24  & year>=2000 & eligible==1 & student==0
di "Mean for Entire sample of non-students"
summ `x' if  a_age>=19 & a_age<=24  & year>=2000 & student==0
di "DD using full sample of non-students"
xi: reg `x' law i.stfips i.year i.a_age married student female ur povratio povratio2 if a_age>=19 & a_age<=24  & year>=2000 & student==0, cluster(stfips)
di "DD using eligible non-students only"
xi: reg `x' law i.stfips i.year i.a_age married student female ur povratio povratio2 if a_age>=19 & a_age<=24  & year>=2000 & eligible==1 & student==0, cluster(stfips)
di "DDD, eligibles vs. ineligibles in non-student sample"
xi: reg `x' i.eligible*law i.eligible*i.stfips i.eligible*i.year i.eligible*i.a_age i.eligible*married i.eligible*student i.eligible*female i.eligible*ur i.eligible*povratio i.eligible*povratio2 if a_age>=19 & a_age<=24  & year>=2000 & student==0, cluster(stfips)

di "Full Sample, with parents"
summ `x' if a_parent>0 & a_age>=19 & a_age<=24  & year>=2000 & eligible==1
xi: reg `x' law i.stfips i.year i.a_age married student female ur povratio povratio2 withparent if a_parent>0 & a_age>=19 & a_age<=24  & year>=2000 & eligible==1, cluster(stfips)

di "<150% pov"
summ `x' if  a_parent>0 & pov==1.5 & a_age>=19 & a_age<=24  & year>=2000 & eligible==1
xi: reg `x' law i.stfips i.year i.a_age married student female ur povratio povratio2 withparent if a_parent>0 & pov==1.5 & a_age>=19 & a_age<=24  & year>=2000 & eligible==1, cluster(stfips)

di "150-300% pov"
summ `x' if  a_parent>0 & pov==3 & a_age>=19 & a_age<=24  & year>=2000 & eligible==1
xi: reg `x' law i.stfips i.year i.a_age married student female ur povratio povratio2 withparent if a_parent>0 & pov==3 & a_age>=19 & a_age<=24  & year>=2000 & eligible==1, cluster(stfips)

di ">300% pov"
summ `x' if  a_parent>0 & pov==4 & a_age>=19 & a_age<=24  & year>=2000 & eligible==1
xi: reg `x' law i.stfips i.year i.a_age married student female ur povratio povratio2 withparent if a_parent>0 & pov==4 & a_age>=19 & a_age<=24  & year>=2000 & eligible==1, cluster(stfips)

di "Parents have group insurance"
summ `x' if a_parent>0 & grouphi_any==1 & a_age>=19 & a_age<=24  & year>=2000 & eligible==1
xi: reg `x' law i.stfips i.year i.a_age married student female ur povratio povratio2 withparent if a_parent>0 & grouphi_any==1 & a_age>=19 & a_age<=24  & year>=2000 & eligible==1, cluster(stfips)

di "Parents have group insurance and firmsize<100"
summ `x' if a_parent>0 & grouphi_any==1 & noemp_insured<=3 & a_age>=19 & a_age<=24  & year>=2000 & eligible==1
xi: reg `x' law i.stfips i.year i.a_age married student female ur povratio povratio2 withparent if a_parent>0 & grouphi_any==1 & noemp_insured<=3 & a_age>=19 & a_age<=24  & year>=2000 & eligible==1, cluster(stfips)

di "Parents have group insurance and firmsize>100"
summ `x' if a_parent>0 & grouphi_any==1 & noemp_insured>3 & a_age>=19 & a_age<=24  & year>=2000 & eligible==1
xi: reg `x' law i.stfips i.year i.a_age married student female ur povratio povratio2 withparent if a_parent>0 & grouphi_any==1 & noemp_insured>3 & a_age>=19 & a_age<=24  & year>=2000 & eligible==1, cluster(stfips)

di "Parents have group insurance and firmsize<500"
summ `x' if a_parent>0 & grouphi_any==1 & noemp_insured<=4 & a_age>=19 & a_age<=24  & year>=2000 & eligible==1
xi: reg `x' law i.stfips i.year i.a_age married student female ur povratio povratio2 withparent if a_parent>0 & grouphi_any==1 & noemp_insured<=4 & a_age>=19 & a_age<=24  & year>=2000 & eligible==1, cluster(stfips)

di "Parents have group insurance and firmsize>500"
summ `x' if a_parent>0 & grouphi_any==1 & noemp_insured>4 & a_age>=19 & a_age<=24  & year>=2000 & eligible==1
xi: reg `x' law i.stfips i.year i.a_age married student female ur povratio povratio2 withparent if a_parent>0 & grouphi_any==1 & noemp_insured>4 & a_age>=19 & a_age<=24  & year>=2000 & eligible==1, cluster(stfips)

di "Parents don't have group insurance"
summ `x' if a_parent>0 & grouphi_any==0 & a_age>=19 & a_age<=24  & year>=2000 & eligible==1
xi: reg `x' law i.stfips i.year i.a_age married student female ur povratio povratio2 withparent if a_parent>0 & grouphi_any==0 & a_age>=19 & a_age<=24  & year>=2000 & eligible==1, cluster(stfips)
}
