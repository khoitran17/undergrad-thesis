foreach x in  insured pubhi privhi pub_ng privonly group nongroup{

di "Full Sample"
summ `x' if a_age>=16 & a_age<=22 & year<1997
xi: reg `x' elig_schip  i.stfips i.year i.a_age ur povratio povratio2 withparent married student female if a_age>=16 & a_age<=22, cluster(a_age)
xi: reg `x' elig_1991-elig_2008  i.stfips i.year i.a_age ur povratio povratio2 withparent married student female if a_age>=16 & a_age<=22, cluster(a_age)
xi: reg `x' elig_9193-elig_0608  i.stfips i.year i.a_age ur povratio povratio2 withparent married student female if a_age>=16 & a_age<=22, cluster(a_age)

di "<150% pov (full sample)"
summ `x' if pov==1.5 & a_parent>0 & a_age>=16 & a_age<=22 & year<1997
xi: reg `x' elig_schip i.stfips i.year i.a_age ur povratio povratio2 withparent married student female if pov==1.5 & a_age>=16 & a_age<=22, cluster(a_age)
xi: reg `x' elig_9193-elig_0608  i.stfips i.year i.a_age ur povratio povratio2 withparent married student female if pov==1.5 & a_age>=16 & a_age<=22, cluster(a_age)

di "150-300% pov (full sample)"
summ `x' if  pov==3 & a_parent>0 & a_age>=16 & a_age<=22 & year<1997
xi: reg `x' elig_schip i.stfips i.year i.a_age ur povratio povratio2 withparent married student female if pov==3 & a_age>=16 & a_age<=22, cluster(a_age)
di ">300% pov (full sample)"
summ `x' if  pov==4 & a_parent>0 & a_age>=16 & a_age<=22 & year<1997
xi: reg `x' elig_schip i.stfips i.year i.a_age ur povratio povratio2 withparent married student female if pov==4 & a_age>=16 & a_age<=22, cluster(a_age)

di "Full Sample, with parents"
summ `x' if a_age>=16 & a_age<=22 & a_parent>0 & year<1997
xi: reg `x' elig_schip  i.stfips i.year i.a_age ur povratio povratio2 withparent married student female if a_parent>0 & a_age>=16 & a_age<=22, cluster(a_age)
xi: reg `x' elig_9193-elig_0608 i.stfips i.year i.a_age ur povratio povratio2 withparent married student female if a_parent>0 & a_age>=16 & a_age<=22, cluster(a_age)
di "<150% pov"
summ `x' if pov==1.5 & a_parent>0 & a_age>=16 & a_age<=22 & year<1997
xi: reg `x' elig_schip i.stfips i.year i.a_age ur povratio povratio2 withparent married student female if pov==1.5 & a_parent>0 & a_age>=16 & a_age<=22, cluster(a_age)
di "150-300% pov"
summ `x' if  pov==3 & a_parent>0 & a_age>=16 & a_age<=22 & year<1997
xi: reg `x' elig_schip i.stfips i.year i.a_age ur povratio povratio2 withparent married student female if pov==3 & a_parent>0 & a_age>=16 & a_age<=22, cluster(a_age)
di ">300% pov"
summ `x' if  pov==4 & a_parent>0 & a_age>=16 & a_age<=22 & year<1997
xi: reg `x' elig_schip i.stfips i.year i.a_age ur povratio povratio2 withparent married student female if pov==4 & a_parent>0 & a_age>=16 & a_age<=22, cluster(a_age)
di "Full sample, exclude 19-year-olds"
xi: reg `x' elig_schip  i.stfips i.year i.a_age ur povratio povratio2 withparent married student female if a_age>=16 & a_age<=22 & a_age~=19, cluster(a_age)

}
