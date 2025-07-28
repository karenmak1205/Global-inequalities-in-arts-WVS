********************************************************************************
*Title: Global inequalities in arts, music or educational organization membership: an epidemiological analysis of 73,825 adults from 51 countries.

*Authors: Hei Wan Mak*, Jessica K. Bone, Taiji Noguchi, Joeun Kim, Rina So, Emma Walker, Calum Smith, Marlee Bower, Ferdi Botha, Nisha L Sajnani, Nils Fietje, Daisy Fancourt
********************************************************************************

use "WVS_Cross-National_Wave_7_Stata_v5_0.dta", clear

************** 
** Arts groups
**************
mvdecode Q96R Q96, mv(-5 -2 -1)

gen art = Q96 
lab def art 0"don't belong" 1"inactive member" 2"active member", replace
lab val art art

gen     art_bin = 1 if art == 1 | art==2 
replace art_bin = 0 if art==0

gen     art_active = 1 if art ==2 
replace art_active = 0 if art==0     // removed inactive

gen     art_active2 = 1 if art==2
replace art_active2 = 0 if art==1 | art==0

************************
** Individual Predictors
************************
// Age
gen     age = Q262 if Q262>=18 & Q262!=. // aged 18+
replace age =  89 if age >=89 & age!=.  // top-coded at 5%
lab var age "Age"

recode age (18/34=1 "Aged 18-34") (35/54=2 "Aged 35-54 (ref: Aged 18-34)") (55/max=3 "Aged 55+ (ref: Aged 18-34)"), into (age_cate)

gen age_sq = age^2

// Gender
gen     female = 1 if Q260==2 
replace female = 0 if Q260==1
lab var female "Female (ref male)"
lab def female 0"Male" 1"Female (ref male)", replace
lab val female female

// respondents' immigration status
gen     minority = 1 if Q263==2  
replace minority = 0 if Q263==1 
lab var minority "Immigrant (ref native born)"
lab def minority 0"Native born" 1"Immigrant (ref native born)", replace
lab val minority minority

// Educaiton
recode Q275 (-5 -2 -1 =.) (0 1 =1 "Education: None or primary education only (ref degree or above)") (2 3 =2 "Education: Secondary education (ref degree or above)") (4 5=3 "Education: Post-secondary education (ref degree or above)") (6/max = 4 "Education: Degree or above"), into (edu)

// Employment status
recode Q279 (-5 -2 -1=.) (1/3=1 "Employment: Employed/Self-employed") (4 5 6 8=2 "Employment: Not in labour force (ref employed/self-employed)") (7=3 "Employment: Unemployed (ref employed/self-employed)"), into (emp)

// income in quartile 
mvdecode Q288, mv(-5 -2 -1)
xtile income = Q288, n(4)
lab def income 1"Income: Lowest quartile (ref highest quartile)" 2"Income: Second quartile (ref highest quartile)" 3"Income: Third quartile (ref highest quartile)" 4"Income: Highest quartile", replace
lab val income income

// country label 
gen country_abbrev = B_COUNTRY
lab def country_abbrev  ///
20	"	AD	"	///
32	"	AR	"	///
36	"	AU	"	///
50	"	BD	"	///
51	"	AM	"	///
68	"	BO	"	///
76	"	BR	"	///
104	"	MM	"	///
124	"	CA	"	///
152	"	CL	"	///
156	"	CN	"	///
158	"	TW	"	///
170	"	CO	"	///
196	"	CY	"	///
203	"	CZ	"	///
218	"	EC	"	///
231	"	ET	"	///
276	"	DE	"	///
300	"	GR	"	///
320	"	GT	"	///
344	"	HK	"	///
356 "   IN  "   ///
360	"	ID	"	///
364	"	IR	"	///
368	"	IQ	"	///
392	"	JP	"	///
398	"	KZ	"	///
400	"	JO	"	///
404	"	KE	"	///
410	"	KR	"	///
417	"	KG	"	///
422	"	LB	"	///
434	"	LY	"	///
446	"	MO	"	///
458	"	MY	"	///
462	"	MV	"	///
484	"	MX	"	///
496	"	MN	"	///
504	"	MA	"	///
528	"	NL	"	///
554	"	NZ	"	///
558	"	NI	"	///
566	"	NG	"	///
586	"	PK	"	///
604	"	PE	"	///
608	"	PH	"	///
630	"	PR	"	///
642	"	RO	"	///
643	"	RU	"	///
688	"	RS	"	///
702	"	SG	"	///
703	"	SK	"	///
704	"	VN	"	///
716	"	ZW	"	///
762	"	TJ	"	///
764	"	TH	"	///
788	"	TN	"	///
792	"	TR	"	///
804	"	UA	"	///
818	"	EG	"	///
826	"	GB	"	///
840	"	US	"	///
858	"	UY	"	///
860 "   UZ  "   ///
862	"	VE	"	///
909	"	NI	", replace
lab val country_abbrev country_abbrev

************************
** Country Predictors
************************
mvdecode  compulseduc womenparl lifeexpectHDI lifeexpect dgi migrationrate unemploytotal giniWB democ, mv(-9999 -999)

lab var lifeexpect "Life expectancy at birth" 
lab var womenparl "Proportion of women in national parliaments (%)" 
lab var migrationrate "Net migration rate (%)" 
lab var compulseduc "Compulsory education years" 
lab var unemploytotal "Unemployment rate (%)" 
lab var giniWB "Gini income inequality index"
lab var democ "Institutionalized democracy"


************************************
** MAIN ANALYSES N=73,825
************************************
svyset  [pweight= S018],  strata(B_COUNTRY)

gen nomiss = 1 if art_bin !=. & age !=. &female!=. & minority!=. & edu !=. & emp !=. & income !=. & lifeexpect !=. &womenparl !=. &migrationrate!=. & compulseduc!=. & unemploytotal !=. &giniWB !=. &democ !=. &B_COUNTRY!=.

save "WVS.dta"

// DESCRIPTIVE TABLE - INDIVIDUAL LEVEL PREDICTORS
dtable age, svy subpop (if nomiss==1) by(B_COUNTRY) export(table1.xlsx, replace)
dtable age, svy subpop (if nomiss==1 & art_bin==1) by(B_COUNTRY) export(table1.xlsx, replace)
dtable age, svy subpop (if nomiss==1 & art_bin==0) by(B_COUNTRY) export(table1.xlsx, replace)

dtable female, svy subpop (if nomiss==1) by(B_COUNTRY) export(table1.xlsx, replace)
dtable female, svy subpop (if nomiss==1 & art_bin==1) by(B_COUNTRY) export(table1.xlsx, replace)
dtable female, svy subpop (if nomiss==1 & art_bin==0) by(B_COUNTRY) export(table1.xlsx, replace)

dtable minority, svy subpop (if nomiss==1) by(B_COUNTRY) export(table1.xlsx, replace)
dtable minority, svy subpop (if nomiss==1 & art_bin==1) by(B_COUNTRY) export(table1.xlsx, replace)
dtable minority, svy subpop (if nomiss==1 & art_bin==0) by(B_COUNTRY) export(table1.xlsx, replace)

recode edu (1 2 3=0 "no degree") (4=1 "Degree"), into (degree)
dtable degree, svy subpop (if nomiss==1) by(B_COUNTRY) export(table1.xlsx, replace)
dtable degree, svy subpop (if nomiss==1 & art_bin==1) by(B_COUNTRY) export(table1.xlsx, replace)
dtable degree, svy subpop (if nomiss==1 & art_bin==0) by(B_COUNTRY) export(table1.xlsx, replace)

recode emp (1=1 "employed/self-employed") (2 3=0 "not employed/unemployed"), into (emp_v2)
dtable emp_v2, svy subpop (if nomiss==1) by(B_COUNTRY) export(table1.xlsx, replace)
dtable emp_v2, svy subpop (if nomiss==1 & art_bin==1) by(B_COUNTRY) export(table1.xlsx, replace)
dtable emp_v2, svy subpop (if nomiss==1 & art_bin==0) by(B_COUNTRY) export(table1.xlsx, replace)

dtable, svy subpop (if nomiss==1) continuous (income, statistics (mean sd)) by(B_COUNTRY) export(table1.xlsx, replace) nformat(%9.2f)
dtable, svy subpop (if nomiss==1 & art_bin==1) continuous (income, statistics (mean sd)) by(B_COUNTRY) export(table1.xlsx, replace) nformat(%9.2f)
dtable, svy subpop (if nomiss==1 & art_bin==0) continuous (income, statistics (mean sd)) by(B_COUNTRY) export(table1.xlsx, replace) nformat(%9.2f)


// MAIN ANALYSIS 
melogit art_bin i.age_cate female minority ib4.edu ib1.emp ib4.income lifeexpect womenparl migrationrate compulseduc unemploytotal giniWB democ [pweight= S018] if nomiss==1 || B_COUNTRY: female i.age_cate, or cov(unstructured)
 
outreg2 using myfile, excel replace dec(2) sideway pdec(3) stats(coef ci) noas eform

estat icc
estat ic
		
coefplot,  ///
   eform  ///
   drop(_cons) ///
   title ("Predictors of arts organization membership", pos(12) size(med))  /// 
   xtitle("Odds Ratio", size(small)) ///
   xline(1, lpattern(dash) lcolor(gs7)) ///
   xlabel (0(0.5)2) xscale(r(0 2)) ///
   headings(age = "{bf:Individual-level predictors}" lifeexpect= "{bf:Country-level predictors}", labgap(0)) ///
   legend(off)  ///
   graphregion(color(white)) ylabel(,labsize(small)) name(graph,replace) saving(graph, replace)


// Predicted values against country level predictors
** LIFE EXPECTANCY
melogit art_bin i.age_cate female minority ib4.edu ib1.emp ib4.income lifeexpect womenparl migrationrate compulseduc unemploytotal giniWB democ [pweight= S018] if nomiss==1 || B_COUNTRY: female i.age_cate, or cov(unstructured)

predict fe
collapse (mean) femean = fe, by(country_abbrev lifeexpect womenparl migrationrate compulseduc unemploytotal giniWB democ regionWB)

grstyle init
grstyle set graphsize 8cm 11cm
grstyle set size 12pt: heading

twoway (scatter femean lifeexpect if regionWB==1 & !mi(femean), color(dkorange) mlabcolor (dkorange) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(60(5)90) xtitle(,size(medlarge))) ///
(scatter femean lifeexpect if regionWB==2 & !mi(femean), color(stgreen) mlabcolor (stgreen) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(60(5)90)) ///
(scatter femean lifeexpect if regionWB==3 & !mi(femean), color(dkgreen) mlabcolor (dkgreen) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(60(5)90)) ///
(scatter femean lifeexpect if regionWB==4 & !mi(femean), color(stred) mlabcolor (stred) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(60(5)90)) ///
(scatter femean lifeexpect if regionWB==5 & !mi(femean), color(purple) mlabcolor (purple) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(60(5)90)) ///
(scatter femean lifeexpect if regionWB==6 & !mi(femean), color(stblue) mlabcolor (stblue) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(60(5)90)) ///
(scatter femean lifeexpect if regionWB==7 & !mi(femean), color(styellow) mlabcolor (styellow) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(60(5)90)) ///
(lfit femean lifeexpect, color (gs1)), legend (off) graphregion(color(white)) name(graph,replace) saving(graph, replace) title("(a)", pos(11) size(med))


** % SEATS HELD BY WOMEN IN NATIONAL PARLIAMENT
grstyle init
grstyle set graphsize 8cm 11cm
grstyle set size 12pt: heading

twoway (scatter femean womenparl if regionWB==1, color(dkorange) mlabcolor (dkorange) mlabel(country_abbrev) mlabposition(7) mlabgap(*-5) ytitle("Predicted value of arts group")  xtitle(,size(medlarge))) ///
(scatter femean womenparl if regionWB==2, color(stgreen) mlabcolor (stgreen) mlabel(country_abbrev) mlabposition(7) mlabgap(*-5) ytitle("Predicted value of arts group")) ///
(scatter femean womenparl if regionWB==3, color(dkgreen) mlabcolor (dkgreen) mlabel(country_abbrev) mlabposition(7) mlabgap(*-5) ytitle("Predicted value of arts group")) ///
(scatter femean womenparl if regionWB==4, color(stred) mlabcolor (stred) mlabel(country_abbrev) mlabposition(7) mlabgap(*-5) ytitle("Predicted value of arts group")) ///
(scatter femean womenparl if regionWB==5, color(purple) mlabcolor (purple) mlabel(country_abbrev) mlabposition(7) mlabgap(*-5) ytitle("Predicted value of arts group")) ///
(scatter femean womenparl if regionWB==6, color(stblue) mlabcolor (stblue) mlabel(country_abbrev) mlabposition(7) mlabgap(*-5) ytitle("Predicted value of arts group")) ///
(scatter femean womenparl if regionWB==7, color(styellow) mlabcolor (styellow) mlabel(country_abbrev) mlabposition(7) mlabgap(*-5) ytitle("Predicted value of arts group")) ///
(lfit femean womenparl, color (gs1)), legend (off) graphregion(color(white)) name(graph,replace) saving(graph, replace)title("(b)", pos(11) size(med))
 
 
** Net migration rate
grstyle init
grstyle set graphsize 8cm 11cm
grstyle set size 12pt: heading

twoway (scatter femean migrationrate if regionWB==1 & !mi(femean), color(dkorange) mlabcolor (dkorange) mlabel(country_abbrev) mlabposition(11) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(-10(5)10)  xtitle(,size(medlarge))) ///
(scatter femean migrationrate if regionWB==2 & !mi(femean), color(stgreen) mlabcolor (stgreen) mlabel(country_abbrev) mlabposition(11) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(-10(5)10)) ///
(scatter femean migrationrate if regionWB==3 & !mi(femean), color(dkgreen) mlabcolor (dkgreen) mlabel(country_abbrev) mlabposition(11) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(-10(5)10)) ///
(scatter femean migrationrate if regionWB==4 & !mi(femean), color(stred) mlabcolor (stred) mlabel(country_abbrev) mlabposition(11) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(-10(5)10)) ///
(scatter femean migrationrate if regionWB==5 & !mi(femean), color(purple) mlabcolor (purple) mlabel(country_abbrev) mlabposition(11) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(-10(5)10)) ///
(scatter femean migrationrate if regionWB==6 & !mi(femean), color(stblue) mlabcolor (stblue) mlabel(country_abbrev) mlabposition(11) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(-10(5)10)) ///
(scatter femean migrationrate if regionWB==7 & !mi(femean), color(styellow) mlabcolor (styellow) mlabel(country_abbrev) mlabposition(11) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(-10(5)10)) ///
(lfit femean migrationrate, color (gs1)), legend (off) graphregion(color(white)) name(graph,replace) saving(graph, replace) title("(c)", pos(11) size(med))
 

** Compulsory education
grstyle init
grstyle set graphsize 8cm 11cm
grstyle set size 12pt: heading

twoway (scatter femean compulseduc if regionWB==1, color(dkorange) mlabcolor (dkorange) mlabel(country_abbrev) mlabposition(1) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(5(5)17)  xtitle(,size(medlarge))) ///
(scatter femean compulseduc if regionWB==2, color(stgreen) mlabcolor (stgreen) mlabel(country_abbrev) mlabposition(1) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(5(5)17)) ///
(scatter femean compulseduc if regionWB==3, color(dkgreen) mlabcolor (dkgreen) mlabel(country_abbrev) mlabposition(1) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(5(5)17)) ///
(scatter femean compulseduc if regionWB==4, color(stred) mlabcolor (stred) mlabel(country_abbrev) mlabposition(1) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(5(5)17)) ///
(scatter femean compulseduc if regionWB==5, color(purple) mlabcolor (purple) mlabel(country_abbrev) mlabposition(1) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(5(5)17)) ///
(scatter femean compulseduc if regionWB==6, color(stblue) mlabcolor (stblue) mlabel(country_abbrev) mlabposition(1) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(5(5)17)) ///
(scatter femean compulseduc if regionWB==7, color(styellow) mlabcolor (styellow) mlabel(country_abbrev) mlabposition(1) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(5(5)17)) ///
(lfit femean compulseduc, color (gs1)), legend (off) name(graph,replace) saving(graph, replace) title("(d)", pos(11) size(med))


** Unemployment rate
grstyle init
grstyle set graphsize 8cm 11cm
grstyle set size 12pt: heading

twoway (scatter femean unemploytotal if regionWB==1, color(dkorange) mlabcolor (dkorange) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group")  xtitle(,size(medlarge))) ///
(scatter femean unemploytotal if regionWB==2, color(stgreen) mlabcolor (stgreen) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group")) ///
(scatter femean unemploytotal if regionWB==3, color(dkgreen) mlabcolor (dkgreen) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group")) ///
(scatter femean unemploytotal if regionWB==4, color(stred) mlabcolor (stred) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group")) ///
(scatter femean unemploytotal if regionWB==5, color(purple) mlabcolor (purple) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group")) ///
(scatter femean unemploytotal if regionWB==6, color(stblue) mlabcolor (stblue) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group")) ///
(scatter femean unemploytotal if regionWB==7, color(styellow) mlabcolor (styellow) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group")) ///
(lfit femean unemploytotal, color (gs1)), legend (off) graphregion(color(white)) name(graph,replace) saving(graph, replace) title("(e)", pos(11) size(med))


** Gini
grstyle init
grstyle set graphsize 8cm 11cm
grstyle set size 12pt: heading

twoway (scatter femean giniWB if regionWB==1, color(dkorange) mlabcolor (dkorange) mlabel(country_abbrev) mlabposition(11) mlabgap(*-5) ytitle("Predicted value of arts group")  xtitle(,size(medlarge))) ///
(scatter femean giniWB if regionWB==2, color(stgreen) mlabcolor (stgreen) mlabel(country_abbrev) mlabposition(11) mlabgap(*-5) ytitle("Predicted value of arts group")) ///
(scatter femean giniWB if regionWB==3, color(dkgreen) mlabcolor (dkgreen) mlabel(country_abbrev) mlabposition(11) mlabgap(*-5) ytitle("Predicted value of arts group")) ///
(scatter femean giniWB if regionWB==4, color(stred) mlabcolor (stred) mlabel(country_abbrev) mlabposition(11) mlabgap(*-5) ytitle("Predicted value of arts group")) ///
(scatter femean giniWB if regionWB==5, color(purple) mlabcolor (purple) mlabel(country_abbrev) mlabposition(11) mlabgap(*-5) ytitle("Predicted value of arts group")) ///
(scatter femean giniWB if regionWB==6, color(stblue) mlabcolor (stblue) mlabel(country_abbrev) mlabposition(11) mlabgap(*-5) ytitle("Predicted value of arts group")) ///
(scatter femean giniWB if regionWB==7, color(styellow) mlabcolor (styellow) mlabel(country_abbrev) mlabposition(11) mlabgap(*-5) ytitle("Predicted value of arts group")) ///
(lfit femean giniWB, color (gs1)), legend (off) graphregion(color(white)) name(graph,replace) saving(graph, replace) title("(f)", pos(11) size(med))


** Democracy
grstyle init
grstyle set graphsize 8cm 11cm
grstyle set size 12pt: heading

twoway (scatter femean democ if regionWB==1, color(dkorange) mlabcolor (dkorange) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(0(2)12)  xtitle(,size(medlarge))) ///
(scatter femean democ if regionWB==2, color(stgreen) mlabcolor (stgreen) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(0(2)12)) ///
(scatter femean democ if regionWB==3, color(dkgreen) mlabcolor (dkgreen) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(0(2)12)) ///
(scatter femean democ if regionWB==4, color(stred) mlabcolor (stred) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(0(2)12)) ///
(scatter femean democ if regionWB==5, color(purple) mlabcolor (purple) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(0(2)12)) ///
(scatter femean democ if regionWB==6, color(stblue) mlabcolor (stblue) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(0(2)12)) ///
(scatter femean democ if regionWB==7, color(styellow) mlabcolor (styellow) mlabel(country_abbrev) mlabposition(5) mlabgap(*-5) ytitle("Predicted value of arts group") xlabel(0(2)12)) ///
(lfit femean democ, color (gs1)), legend (off) graphregion(color(white)) name(graph,replace) saving(graph, replace)title("(g)", pos(11) size(med))


// MAPPING ARTS GROUP %
spshape2dta "WB_countries_Admin0_10m", replace saving(world)

use "world.dta", clear

gen     B_COUNTRY =. 
replace B_COUNTRY = 20 if NAME_EN == "Andorra"
replace B_COUNTRY = 32 if NAME_EN == "Argentina"
replace B_COUNTRY = 51 if NAME_EN == "Armenia"
replace B_COUNTRY = 36 if NAME_EN == "Australia"
replace B_COUNTRY = 50 if NAME_EN == "Bangladesh"
replace B_COUNTRY = 68 if NAME_EN == "Bolivia"
replace B_COUNTRY = 76 if NAME_EN == "Brazil"
replace B_COUNTRY = 124 if NAME_EN == "Canada"
replace B_COUNTRY = 152 if NAME_EN == "Chile"
replace B_COUNTRY = 170 if NAME_EN == "Colombia"
replace B_COUNTRY = 196 if NAME_EN == "Cyprus"
replace B_COUNTRY = 203 if NAME_EN == "Czech Republic"
replace B_COUNTRY = 218 if NAME_EN == "Ecuador"
replace B_COUNTRY = 818 if NAME_EN == "Egypt"
replace B_COUNTRY = 231 if NAME_EN == "Ethiopia"
replace B_COUNTRY = 276 if NAME_EN == "Germany"
replace B_COUNTRY = 300 if NAME_EN == "Greece"
replace B_COUNTRY = 320 if NAME_EN == "Guatemala"
replace B_COUNTRY = 344 if NAME_EN == "Hong Kong"
replace B_COUNTRY = 356 if NAME_EN == "India"
replace B_COUNTRY = 360 if NAME_EN == "Indonesia"
replace B_COUNTRY = 364 if NAME_EN == "Iran"
replace B_COUNTRY = 368 if NAME_EN == "Iraq"
replace B_COUNTRY = 392 if NAME_EN == "Japan"
replace B_COUNTRY = 400 if NAME_EN == "Jordan"
replace B_COUNTRY = 398 if NAME_EN == "Kazakhstan"
replace B_COUNTRY = 404 if NAME_EN == "Kenya"
replace B_COUNTRY = 417 if NAME_EN == "Kyrgyzstan"
replace B_COUNTRY = 422 if NAME_EN == "Lebanon"
replace B_COUNTRY = 434 if NAME_EN == "Libya"
replace B_COUNTRY = 446 if NAME_EN == "Macau"
replace B_COUNTRY = 458 if NAME_EN == "Malaysia"
replace B_COUNTRY = 462 if NAME_EN == "Maldives"
replace B_COUNTRY = 484 if NAME_EN == "Mexico"
replace B_COUNTRY = 496 if NAME_EN == "Mongolia"
replace B_COUNTRY = 504 if NAME_EN == "Morocco"
replace B_COUNTRY = 104 if NAME_EN == "Myanmar"
replace B_COUNTRY = 528 if NAME_EN == "Netherlands"
replace B_COUNTRY = 554 if NAME_EN == "New Zealand"
replace B_COUNTRY = 558 if NAME_EN == "Nicaragua"
replace B_COUNTRY = 566 if NAME_EN == "Nigeria"
replace B_COUNTRY = 586 if NAME_EN == "Pakistan"
replace B_COUNTRY = 156 if NAME_EN == "People's Republic of China"
replace B_COUNTRY = 604 if NAME_EN == "Peru"
replace B_COUNTRY = 608 if NAME_EN == "Philippines"
replace B_COUNTRY = 630 if NAME_EN == "Puerto Rico"
replace B_COUNTRY = 642 if NAME_EN == "Romania"
replace B_COUNTRY = 643 if NAME_EN == "Russia"
replace B_COUNTRY = 688 if NAME_EN == "Serbia"
replace B_COUNTRY = 702 if NAME_EN == "Singapore"
replace B_COUNTRY = 703 if NAME_EN == "Slovakia"
replace B_COUNTRY = 410 if NAME_EN == "South Korea"
replace B_COUNTRY = 762 if NAME_EN == "Tajikistan"
replace B_COUNTRY = 764 if NAME_EN == "Thailand"
replace B_COUNTRY = 788 if NAME_EN == "Tunisia"
replace B_COUNTRY = 792 if NAME_EN == "Turkey"
replace B_COUNTRY = 804 if NAME_EN == "Ukraine"
replace B_COUNTRY = 826 if NAME_EN == "United Kingdom"
replace B_COUNTRY = 840 if NAME_EN == "United States of America"
replace B_COUNTRY = 858 if NAME_EN == "Uruguay"
replace B_COUNTRY = 860 if NAME_EN == "Uzbekistan" 
replace B_COUNTRY = 862 if NAME_EN == "Venezuela"
replace B_COUNTRY = 704 if NAME_EN == "Vietnam"
replace B_COUNTRY = 716 if NAME_EN == "Zimbabwe"

save world.dta, replace

use "WVS.dta", clear
collapse (mean) art_bin  [pweight= S018], by(B_COUNTRY)

merge 1:m B_COUNTRY using world.dta

spmap art_bin using world_shp, id(_ID) fcolor(Blues2) ///
     title("", size(5)) ///
	 note("Source: World Bank map data", size(2.5)) clmethod(custom) clbreaks(0.00 0.1 0.2 0.3 0.4 0.55)

svy: ta B_COUNTRY art_bin 


// MAPPING ARTS ACTIVE GROUP %
spshape2dta "WB_countries_Admin0_10m", replace saving(world)

use "world.dta", clear

gen     B_COUNTRY =. 
replace B_COUNTRY = 20 if NAME_EN == "Andorra"
replace B_COUNTRY = 32 if NAME_EN == "Argentina"
replace B_COUNTRY = 51 if NAME_EN == "Armenia"
replace B_COUNTRY = 36 if NAME_EN == "Australia"
replace B_COUNTRY = 50 if NAME_EN == "Bangladesh"
replace B_COUNTRY = 68 if NAME_EN == "Bolivia"
replace B_COUNTRY = 76 if NAME_EN == "Brazil"
replace B_COUNTRY = 124 if NAME_EN == "Canada"
replace B_COUNTRY = 152 if NAME_EN == "Chile"
replace B_COUNTRY = 170 if NAME_EN == "Colombia"
replace B_COUNTRY = 196 if NAME_EN == "Cyprus"
replace B_COUNTRY = 203 if NAME_EN == "Czech Republic"
replace B_COUNTRY = 218 if NAME_EN == "Ecuador"
replace B_COUNTRY = 818 if NAME_EN == "Egypt"
replace B_COUNTRY = 231 if NAME_EN == "Ethiopia"
replace B_COUNTRY = 276 if NAME_EN == "Germany"
replace B_COUNTRY = 300 if NAME_EN == "Greece"
replace B_COUNTRY = 320 if NAME_EN == "Guatemala"
replace B_COUNTRY = 344 if NAME_EN == "Hong Kong"
replace B_COUNTRY = 356 if NAME_EN == "India"
replace B_COUNTRY = 360 if NAME_EN == "Indonesia"
replace B_COUNTRY = 364 if NAME_EN == "Iran"
replace B_COUNTRY = 368 if NAME_EN == "Iraq"
replace B_COUNTRY = 392 if NAME_EN == "Japan"
replace B_COUNTRY = 400 if NAME_EN == "Jordan"
replace B_COUNTRY = 398 if NAME_EN == "Kazakhstan"
replace B_COUNTRY = 404 if NAME_EN == "Kenya"
replace B_COUNTRY = 417 if NAME_EN == "Kyrgyzstan"
replace B_COUNTRY = 422 if NAME_EN == "Lebanon"
replace B_COUNTRY = 434 if NAME_EN == "Libya"
replace B_COUNTRY = 446 if NAME_EN == "Macau"
replace B_COUNTRY = 458 if NAME_EN == "Malaysia"
replace B_COUNTRY = 462 if NAME_EN == "Maldives"
replace B_COUNTRY = 484 if NAME_EN == "Mexico"
replace B_COUNTRY = 496 if NAME_EN == "Mongolia"
replace B_COUNTRY = 504 if NAME_EN == "Morocco"
replace B_COUNTRY = 104 if NAME_EN == "Myanmar"
replace B_COUNTRY = 528 if NAME_EN == "Netherlands"
replace B_COUNTRY = 554 if NAME_EN == "New Zealand"
replace B_COUNTRY = 558 if NAME_EN == "Nicaragua"
replace B_COUNTRY = 566 if NAME_EN == "Nigeria"
replace B_COUNTRY = 586 if NAME_EN == "Pakistan"
replace B_COUNTRY = 156 if NAME_EN == "People's Republic of China"
replace B_COUNTRY = 604 if NAME_EN == "Peru"
replace B_COUNTRY = 608 if NAME_EN == "Philippines"
replace B_COUNTRY = 630 if NAME_EN == "Puerto Rico"
replace B_COUNTRY = 642 if NAME_EN == "Romania"
replace B_COUNTRY = 643 if NAME_EN == "Russia"
replace B_COUNTRY = 688 if NAME_EN == "Serbia"
replace B_COUNTRY = 702 if NAME_EN == "Singapore"
replace B_COUNTRY = 703 if NAME_EN == "Slovakia"
replace B_COUNTRY = 410 if NAME_EN == "South Korea"
replace B_COUNTRY = 762 if NAME_EN == "Tajikistan"
replace B_COUNTRY = 764 if NAME_EN == "Thailand"
replace B_COUNTRY = 788 if NAME_EN == "Tunisia"
replace B_COUNTRY = 792 if NAME_EN == "Turkey"
replace B_COUNTRY = 804 if NAME_EN == "Ukraine"
replace B_COUNTRY = 826 if NAME_EN == "United Kingdom"
replace B_COUNTRY = 840 if NAME_EN == "United States of America"
replace B_COUNTRY = 858 if NAME_EN == "Uruguay"
replace B_COUNTRY = 860 if NAME_EN == "Uzbekistan" 
replace B_COUNTRY = 862 if NAME_EN == "Venezuela"
replace B_COUNTRY = 704 if NAME_EN == "Vietnam"
replace B_COUNTRY = 716 if NAME_EN == "Zimbabwe"

save world.dta, replace

use "WVS.dta", clear
collapse (mean) art_active2  [pweight= S018], by(B_COUNTRY)

merge 1:m B_COUNTRY using world.dta

spmap art_active2 using world_shp, id(_ID) fcolor(RdPu) ///
     title("", size(5)) ///
	 note("Source: World Bank map data", size(2.5)) clmethod(custom) clbreaks(0.00 0.05 0.1 0.15 0.2 0.25 0.3)

svy: ta B_COUNTRY art_active2

 
// Logistic regression for each continent - drop migration & add B_COUNTRY 
svyset  [pweight= S018], strata(B_COUNTRY)

**Sub Saharan Africa N=3,589 
svy, subpop (if regionWB==1 & nomiss==1): logit art_bin i.age_cate female ib4.edu ib1.emp ib4.income i.B_COUNTRY, or

outreg2 using myfile, excel replace dec(2) sideway pdec(3) stats(coef ci) noas eform

coefplot, keep (2.age_cate 3.age_cate female *.edu *.emp *.income)  ///
   mcolor(dkorange) ciopts(lcol(dkorange)) ///
   eform  ///
   drop(_cons) ///
   title ("Sub-Saharan Africa (N=3,589)", pos(12) size(med))  /// 
   xtitle("Odds Ratio", size(small)) ///
   xline(1, lpattern(dash) lcolor(gs7)) ///
   xlabel (0(0.5)2) ///
   headings(age = "{bf:Individual-level predictors}", labgap(0)) ///
   legend(off)  ///
   graphregion(color(white)) ylabel(,labsize(small)) name(graph,replace) saving(graph, replace)
	

** South Asia N=4,155 
svy, subpop (if regionWB==2 & nomiss==1): logit art_bin i.age_cate female ib4.edu ib1.emp ib4.income i.B_COUNTRY, or
		
outreg2 using myfile, excel replace dec(2) sideway pdec(3) stats(coef ci) noas eform
		
coefplot, keep (2.age_cate 3.age_cate female *.edu *.emp *.income) ///
   eform  ///
   mcolor(stgreen) ciopts(lcol(stgreen)) ///
   drop(_cons) ///
   title ("South Asia (N=4,155)", pos(12) size(med))  /// 
   xtitle("Odds Ratio", size(small)) ///
   xline(1, lpattern(dash) lcolor(gs7)) ///
   xlabel (0(0.5)2) ///
   headings(age = "{bf:Individual-level predictors}", labgap(0)) ///
   legend(off)  ///
   graphregion(color(white)) ylabel(,labsize(small)) name(graph,replace) saving(graph, replace)


** North America  N=6,459 
svy, subpop (if regionWB==3 & nomiss==1): logit art_bin i.age_cate female ib4.edu ib1.emp ib4.income i.B_COUNTRY, or
		
outreg2 using myfile, excel replace dec(2) sideway pdec(3) stats(coef ci) noas eform
		
coefplot, keep (2.age_cate 3.age_cate female *.edu *.emp *.income)  ///
   mcolor(dkgreen) ciopts(lcol(dkgreen)) ///
   eform  ///
   drop(_cons) ///
   title ("North America (N= 6,459)", pos(12) size(med))  /// 
   xtitle("Odds Ratio", size(small)) ///
   xline(1, lpattern(dash) lcolor(gs7)) ///
   xlabel (0(0.5)2) ///
   headings(age = "{bf:Individual-level predictors}", labgap(0)) ///
   legend(off)  ///
   graphregion(color(white)) ylabel(,labsize(small)) name(graph,replace) saving(graph, replace)


** Middle East and North Africa  N=7,274
svy, subpop (if regionWB==4 & nomiss==1): logit art_bin i.age_cate female ib4.edu ib1.emp ib4.income i.B_COUNTRY, or
		
outreg2 using myfile, excel replace dec(2) sideway pdec(3) stats(coef ci) noas eform
		
coefplot, keep (2.age_cate 3.age_cate female *.edu *.emp *.income) ///
   mcolor(stred) ciopts(lcol(stred)) ///
   eform  ///
   drop(_cons) ///
   title ("Middle East and North Africa (N=7,274)", pos(12) size(med))  /// 
   xtitle("Odds Ratio", size(small)) ///
   xline(1, lpattern(dash) lcolor(gs7)) ///
   xlabel (0(0.5)2) ///
   headings(age = "{bf:Individual-level predictors}", labgap(0)) ///
   legend(off)  ///
   graphregion(color(white)) ylabel(,labsize(small)) name(graph,replace) saving(graph, replace)


** Latin America and Caribbean   N=14,261
svy, subpop (if regionWB==5 & nomiss==1): logit art_bin i.age_cate female ib4.edu ib1.emp ib4.income i.B_COUNTRY, or
		
outreg2 using myfile, excel replace dec(2) sideway pdec(3) stats(coef ci) noas eform
		
coefplot, keep (2.age_cate 3.age_cate female *.edu *.emp *.income) ///
   mcolor(purple) ciopts(lcol(purple)) ///
   eform  ///
   drop(_cons) ///
   title ("Latin America and Caribbean (N=14,261)", pos(12) size(med))  /// 
   xtitle("Odds Ratio", size(small)) ///
   xline(1, lpattern(dash) lcolor(gs7)) ///
   xlabel (0(0.5)2) ///
   headings(age = "{bf:Individual-level predictors}", labgap(0)) ///
   legend(off)  ///
   graphregion(color(white)) ylabel(,labsize(small)) name(graph,replace) saving(graph, replace)


** Europe and Central Asia  N=20,054
svy, subpop (if regionWB==6 & nomiss==1): logit art_bin i.age_cate female ib4.edu ib1.emp ib4.income i.B_COUNTRY, or
		
outreg2 using myfile, excel replace dec(2) sideway pdec(3) stats(coef ci) noas eform
				
coefplot, keep (2.age_cate 3.age_cate female *.edu *.emp *.income) ///
   mcolor(stblue) ciopts(lcol(stblue)) ///
   eform  ///
   drop(_cons) ///
   title ("Europe and Central Asia (N=20,054)", pos(12) size(med))  /// 
   xtitle("Odds Ratio", size(small)) ///
   xline(1, lpattern(dash) lcolor(gs7)) ///
   xlabel (0(0.5)2) ///
   headings(age = "{bf:Individual-level predictors}", labgap(0)) ///
   legend(off)  ///
   graphregion(color(white)) ylabel(,labsize(small)) name(graph,replace) saving(graph, replace)


** East Asia and Pacific  N= 18,033
svy, subpop (if regionWB==7 & nomiss==1): logit art_bin i.age_cate female ib4.edu ib1.emp ib4.income i.B_COUNTRY, or

outreg2 using myfile, excel replace dec(2) sideway pdec(3) stats(coef ci) noas eform

		
coefplot, keep (2.age_cate 3.age_cate female *.edu *.emp *.income) ///
   mcolor(styellow) ciopts(lcol(styellow)) ///
   eform  ///
   drop(_cons) ///
   title ("East Asia and Pacific (N=18,033)", pos(12) size(med))  /// 
   xtitle("Odds Ratio", size(small)) ///
   xline(1, lpattern(dash) lcolor(gs7)) ///
   xlabel (0(0.5)2) ///
   headings(age = "{bf:Individual-level predictors}", labgap(0)) ///
   legend(off)  ///
   graphregion(color(white)) ylabel(,labsize(small)) name(graph,replace) saving(graph, replace)


*** Sample description
ta B_COUNTRY // N=94278
ta B_COUNTRY if art_bin !=. //N=96,006
ta B_COUNTRY if art_bin !=. & age !=. &female!=. & minority!=. & edu !=. & emp !=. & income !=. // N=90,890
ta B_COUNTRY if art_bin !=. & age !=. &female!=. & minority!=. & edu !=. & emp !=. & income !=. & lifeexpect !=. &womenparl !=. &migrationrate!=. & compulseduc!=. & unemploytotal !=. &giniWB !=. &democ !=. &B_COUNTRY!=. 


********************************************************************************
*** Random slopes - AGE & FEMALE
********************************************************************************
melogit art_bin i.age_cate female minority ib4.edu ib1.emp ib4.income lifeexpect womenparl migrationrate compulseduc unemploytotal giniWB democ [pweight= S018] if nomiss==1 || B_COUNTRY: i.age_cate female, or cov(unstructured)

predict b* if nomiss==1, reffects    // predicting the 'residual' slopes
rename (b1 b2 b3) (agec2_slope agec3_slope female_slope)     // the order may change
  
melogit art_bin i.age_cate female minority ib4.edu ib1.emp ib4.income lifeexpect womenparl migrationrate compulseduc unemploytotal giniWB democ [pweight= S018] if nomiss==1 || B_COUNTRY: i.age_cate female, or cov(unstructured)

return list

matrix list r(table)

display _b[art_bin: female]    
display _b[art_bin: 2.age_cate]   
display _b[art_bin: 3.age_cate]    

gen rmfemale_slope =_b[art_bin: female]+ female_slope
gen rmage2_slope   =_b[art_bin: 2.age_cate]   + agec2_slope
gen rmage3_slope   =_b[art_bin: 3.age_cate]   + agec3_slope

gen rmfemale_or=exp(rmfemale_slope)
gen rmage2_or=exp(rmage2_slope)
gen rmage3_or=exp(rmage3_slope)

collapse (mean) rmfemale_or rmage2_or rmage3_or if nomiss==1, by(B_COUNTRY)

graph dot rmfemale_or, over (B_COUNTRY, sort(rmfemale_or) descending label(labsize(tiny)))  yline (1, lpattern(solid)) ytitle ("Odds Ratios") title ("Association between females (vs males) and arts organization membership across countries", size(small))

graph dot rmage2_or, over (B_COUNTRY, sort(rmage2_or) descending label(labsize(tiny))) yline (1, lpattern(solid))  ytitle ("Odds Ratios") title ("Association between aged 35-54 (vs aged 18-34) and arts organization membership across countries", size(small)) 

graph dot rmage3_or, over (B_COUNTRY, sort(rmage3_or) descending label(labsize(tiny))) yline (1, lpattern(solid))  ytitle ("Odds Ratios") title ("Association between aged 55+ (vs aged 18-34) and arts organization membership across countries", size(small)) 


********************************************************************************
*** Sensitivity analysis 1: active vs inactive/do not belong
********************************************************************************
melogit art_active2 i.age_cate female minority ib4.edu ib1.emp ib4.income lifeexpect womenparl migrationrate compulseduc unemploytotal giniWB democ [pweight= S018] if nomiss==1 || B_COUNTRY: female i.age_cate, or
 
outreg2 using myfile, excel replace dec(2) sideway pdec(3) stats(coef ci) noas eform

estat icc
estat ic
		
coefplot,  ///
   eform  ///
   drop(_cons) ///
   title ("Predictors of active arts organization membership", pos(12) size(med))  /// 
   xtitle("Odds Ratio", size(small)) ///
   xline(1, lpattern(dash) lcolor(gs7)) ///
   xlabel (0(0.5)2) xscale(r(0 2)) ///
   headings(age = "{bf:Individual-level predictors}" lifeexpect= "{bf:Country-level predictors}", labgap(0)) ///
   legend(off)  ///
   graphregion(color(white)) ylabel(,labsize(small)) name(graph,replace) saving(graph, replace)

   
********************************************************************************
*** Sensitivity analysis 2: adding year of survey as an additional covariate
********************************************************************************
melogit art_bin i.age_cate female minority ib4.edu ib1.emp ib4.income lifeexpect womenparl migrationrate compulseduc unemploytotal giniWB democ A_YEAR [pweight= S018] if nomiss==1 || B_COUNTRY: female i.age_cate, or
 
outreg2 using myfile, excel replace dec(2) sideway pdec(3) stats(coef ci) noas eform

estat icc
estat ic
		
coefplot,  ///
   eform  ///
   drop(_cons A_YEAR) ///
   title ("Predictors of arts organization membership: adding the survey year", pos(12) size(small))  /// 
   xtitle("Odds Ratio", size(small)) ///
   xline(1, lpattern(dash) lcolor(gs7)) ///
   xlabel (0(0.5)2) xscale(r(0 2)) ///
   headings(age = "{bf:Individual-level predictors}" lifeexpect= "{bf:Country-level predictors}", labgap(0)) ///
   legend(off)  ///
   graphregion(color(white)) ylabel(,labsize(small)) name(graph,replace) saving(graph, replace)


********************************************************************************
*** Sensitivity analysis 3: using ologit model 
********************************************************************************
meologit art i.age_cate female minority ib4.edu ib1.emp ib4.income lifeexpect womenparl migrationrate compulseduc unemploytotal giniWB democ [pweight= S018] if nomiss==1 || B_COUNTRY: female i.age_cate, or cov(unstructured)
 
outreg2 using myfile, excel replace dec(2) sideway pdec(3) stats(coef ci) noas eform

estat icc
estat ic

grstyle init
grstyle set graphsize 8cm 11cm
grstyle set size 12pt: heading
		
coefplot,  ///
   eform  ///
   drop(_cons) ///
   title ("Predictors of arts organization membership: using ordinal logistic regression model", pos(12) size(vsmall))  /// 
   xtitle("Odds Ratio", size(vsmall)) ///
   xline(1, lpattern(dash) lcolor(gs7)) ///
   xlabel (0(0.5)2) xscale(r(0 2)) ///
   headings(age = "{bf:Individual-level predictors}" lifeexpect= "{bf:Country-level predictors}", labgap(0)) ///
   legend(off)  ///
   graphregion(color(white)) ylabel(,labsize(vsmall)) name(graph,replace) saving(graph, replace)
   

********************************************************************************
*** Prevalence of arts group across survey years
********************************************************************************
use "WVS_Time_Series_1981-2022_stata_v5_0.dta", clear

// Survey weight
svyset  [pweight= S018]

// Recoding
recode A100 (0=0 "Not a member") (1 2=1 "Member") if A100 >=0 & A100!=., into (arts_binALL)
drop if S018<0

recode S020 (1981/1989=1 "1980s") (1990/1999=2 "1990s") (2000/2009=3 "2000s") (2010/2019=4 "2010s") (2020/max=5 "2020s"), into (year_cohort)


collapse (mean) arts_binALL [pweight= S018], by(S020 COW_NUM)
drop if COW_NUM==41
drop if COW_NUM==42
drop if COW_NUM==90
drop if COW_NUM==92
drop if COW_NUM==93
drop if COW_NUM==130
drop if COW_NUM==145
drop if COW_NUM==202
drop if COW_NUM==220
drop if COW_NUM==260
drop if COW_NUM==325
drop if COW_NUM==339
drop if COW_NUM==341
drop if COW_NUM==343
drop if COW_NUM==344
drop if COW_NUM==346
drop if COW_NUM==350
drop if COW_NUM==367
drop if COW_NUM==368
drop if COW_NUM==432
drop if COW_NUM==439
drop if COW_NUM==446
drop if COW_NUM==500
drop if COW_NUM==501
drop if COW_NUM==510
drop if COW_NUM==551
drop if COW_NUM==615
drop if COW_NUM==616
drop if COW_NUM==660
drop if COW_NUM==666
drop if COW_NUM==667
drop if COW_NUM==670
drop if COW_NUM==679
drop if COW_NUM==690
drop if COW_NUM==694
drop if COW_NUM==702
drop if COW_NUM==705
drop if COW_NUM==712
drop if COW_NUM==775
drop if COW_NUM==781

grstyle init
grstyle set graphsize 8cm 11cm
grstyle set size 12pt: heading

twoway scatter arts_binALL S020, by(COW_NUM, compact) ///
xtitle("Survey year", size(medium)) ytitle("Arts organization membership (%)", size(medium)) ///
ylabel(0.2"20" 0.4"40" 0.6"60" 0.8"80" 1"100") xlabel(,angle(90)) ///
legend(off) graphregion(color(white)) 