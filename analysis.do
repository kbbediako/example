///**  Data analysis. **////
use "/Users/kbbediako/Dropbox/Kwabena_Bruno_Expropriation/myfinaldata.dta", clear

gen EEZ_2more=0
replace EEZ_2more=1 if Country=="Yemen"| Country=="United States"| Country=="Turkey"|Country=="Thailand" ///
|Country=="Sweden"|Country=="Spain"|Country=="South Africa"|Country=="Saudi Arabia"|Country=="Russia" ///
|Country=="Oman"|Country=="Morocco"|Country=="Mexico"|Country=="Malaysia"|Country=="Japan" /// 
|Country=="Israel"|Country=="Iran"|Country=="Indonesia"|Country=="Germany"|Country=="France" ///
|Country=="Egypt"|Country=="Denmark"|Country=="Cyprus"|Country=="Colombia"|Country=="Canada"
label variable EEZ_2more "(0/1) Country with 2 or more EEZs is 1, otherwise 0"
la def EEZ_2more 0 "1 EEZ" 1 "More than 1 EEZ"
la val EEZ_2more EEZ_2more

egen id = group(Code)
move id year
sort id year
xtset id year

gen Trade = Export+Import
label variable Trade "Openness"
move Trade GDPpc_WDI


gen L1Overexploited = L.Overexploited
gen L1Collapsed = L.Collapsed
gen H_EEZ = ln(harvest/EEZarea)
gen LH_EEZ = L.H_EEZ

label variable L1Overexploited "Overexploited share"
label variable L1Collapsed "Collapsed share"
label variable LH_EEZ "Log(Harvest/EEZ)"
label variable H_EEZ "Log(Harvest/EEZ)"


gen L1GDPpc_WDI = L.GDPpc_WDI
gen L1Enrollment = L.Enrollment
gen L1Trade = L.Trade
gen L1Unemployment = L.Unemployment

label variable L1GDPpc_WDI "GDP per capita"
label variable L1Enrollment "Enrollment"
label variable L1Trade "Openness"
label variable L1Unemployment "Unemployment"


eststo: quietly xtreg I_WEO voice stability goveff regulation rolaw corruption L1GDPpc_WDI L1Enrollment L1Trade L1Unemployment i.year, fe
esttab using L1econ_investment.tex, keep(voice stability goveff regulation rolaw corruption L1GDPpc_WDI L1Enrollment L1Trade L1Unemployment) replace label star(* 0.05) b(%9.5f) se(%9.5f) title(Investment Model using estimates of governance indicators) booktabs 


gen theta = (_b[voice]*voice) + (_b[stability]*stability) + (_b[goveff]*goveff) + (_b[regulation]*regulation) + (_b[rolaw]*rolaw) + (_b[corruption]*corruption)
gen thetatrade = Trade*theta
label variable theta "Property rights security"
label variable thetatrade "Property rights security x Openness"
eststo clear

///////////** Overexploited share as dependent variable **/////////////
eststo: quietly xtreg Overexploited L1Overexploited theta Trade thetatrade GDPpc_WDI Pdensity_WDI i.year, fe robust


///////////** Marginal Effects **//////////////////////////////////////
quietly xtreg Overexploited L1Overexploited theta Trade c.theta#c.Trade GDPpc_WDI Pdensity_WDI i.year, fe robust
margins, dydx(Trade) at(theta=(-8 -4 -2 2 4 8)) vsquish
marginsplot

///////////** System GMM estimation **/////////////
eststo: quietly xtdpdgmm L(0/1).Overexploited theta Trade thetatrade GDPpc_WDI Pdensity_WDI, model(diff) gmm(Overexploited, lag(2 2)) gmm(Trade, lag(2 2)) gmm(thetatrade, lag(2 2)) gmm(Overexploited, lag(1 1) diff model(level)) gmm(Trade, lag(1 1) diff model(level)) gmm(thetatrade, lag(1 1) diff model(level)) two noconstant nofootnote vce(r)
estat serial, ar(1/3)
estat overid
esttab using L1econ_harvest.tex, keep(L1Overexploited theta Trade thetatrade GDPpc_WDI Pdensity_WDI) replace label star(* 0.05) b(%9.5f) se(%9.5f) title(Effect of Property rights security on Overexploitation) booktabs 
eststo clear

///////////** Overexploited share as dependent variable (Robustness check, by droping some explanatory variables) **/////////////


//////////** Population density **////////
eststo: quietly xtreg Overexploited L1Overexploited theta Trade thetatrade GDPpc_WDI i.year, fe robust
////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////** System GMM estimation **/////////////
eststo: quietly xtdpdgmm L(0/1).Overexploited theta Trade thetatrade GDPpc_WDI, model(diff) gmm(Overexploited, lag(2 2)) gmm(Trade, lag(2 2)) gmm(thetatrade, lag(2 2)) gmm(Overexploited, lag(1 1) diff model(level)) gmm(Trade, lag(1 1) diff model(level)) gmm(thetatrade, lag(1 1) diff model(level)) two noconstant nofootnote vce(r)
estat serial, ar(1/3)
estat overid
esttab using L1econ_harvest_nopdens.tex, keep(L1Overexploited theta Trade thetatrade GDPpc_WDI) replace label star(* 0.05) b(%9.5f) se(%9.5f) title(Effect of Property rights security on Overexploitation) booktabs 
eststo clear

//////////** GDP per capita **////////
eststo: quietly xtreg Overexploited L1Overexploited theta Trade thetatrade Pdensity_WDI i.year, fe robust
////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////** System GMM estimation **/////////////
eststo: quietly xtdpdgmm L(0/1).Overexploited theta Trade thetatrade Pdensity_WDI, model(diff) gmm(Overexploited, lag(2 2)) gmm(Trade, lag(2 2)) gmm(thetatrade, lag(2 2)) gmm(Overexploited, lag(1 1) diff model(level)) gmm(Trade, lag(1 1) diff model(level)) gmm(thetatrade, lag(1 1) diff model(level)) two noconstant nofootnote vce(r)
estat serial, ar(1/3)
estat overid
esttab using L1econ_harvest_noGDP.tex, keep(L1Overexploited theta Trade thetatrade Pdensity_WDI) replace label star(* 0.05) b(%9.5f) se(%9.5f) title(Effect of Property rights security on Overexploitation) booktabs 
eststo clear

notes: Results of the above robustness checks indicate no qualitative change in the coefficients, however, significance is lost on some variables of interest.

///////////** Collapse share as dependent variable **/////////////
eststo: quietly xtreg Collapsed L1Collapsed theta Trade thetatrade GDPpc_WDI Pdensity_WDI i.year, fe
////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////** System GMM estimation **/////////////
eststo: quietly xtdpdgmm L(0/1).Collapsed theta Trade thetatrade GDPpc_WDI Pdensity_WDI, model(diff) gmm(Collapsed, lag(2 2)) gmm(Trade, lag(2 2)) gmm(thetatrade, lag(2 2)) gmm(Collapsed, lag(1 1) diff model(level)) gmm(Trade, lag(1 1) diff model(level)) gmm(thetatrade, lag(1 1) diff model(level)) two noconstant nofootnote
estat serial, ar(1/3)
estat overid
esttab using L1econ_harvest1.tex, keep(L1Collapsed theta Trade thetatrade GDPpc_WDI Pdensity_WDI) replace label star(* 0.05) b(%9.5f) se(%9.5f) title(Effect of Property rights security on stock collapse) booktabs 
eststo clear


///////////** Log(Harvest/EEZ) as dependent variable **/////////////
eststo: quietly xtreg H_EEZ LH_EEZ theta Trade thetatrade GDPpc_WDI Pdensity_WDI i.year, fe robust
xtcsd, pesaran abs
////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////** System GMM estimation **/////////////
eststo: quietly xtdpdgmm L(0/1).H_EEZ theta Trade thetatrade GDPpc_WDI Pdensity_WDI, model(diff) gmm(H_EEZ, lag(2 2)) gmm(Trade, lag(2 2)) gmm(thetatrade, lag(2 2)) gmm(H_EEZ, lag(1 1) diff model(level)) gmm(Trade, lag(1 1) diff model(level)) gmm(thetatrade, lag(1 1) diff model(level)) two noconstant nofootnote vce(r)
estat serial, ar(1/3)
estat overid
esttab using L1econ_harvest2.tex, keep(LH_EEZ theta Trade thetatrade GDPpc_WDI Pdensity_WDI) replace label star(* 0.05) b(%9.5f) se(%9.5f) title(Effect of property rights security on harvest) booktabs 
eststo clear


////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////** Fixed effects model with countries with only 1 EEZ **/////////////

eststo: quietly xtreg I_WEO voice stability goveff regulation rolaw corruption L1GDPpc_WDI L1Enrollment L1Trade L1Unemployment i.year if EEZ_2more==0, fe
esttab using L1investment_EEZ.tex, keep(voice stability goveff regulation rolaw corruption L1GDPpc_WDI L1Enrollment L1Trade L1Unemployment) replace label star(* 0.10 ** 0.05 *** 0.01) b(%9.5f) se(%9.5f) title(Investment Model using estimates of governance indicators, excluding countries with more than one(1) EEZ) booktabs 

drop theta thetatrade
gen theta = (_b[voice]*voice) + (_b[stability]*stability) + (_b[goveff]*goveff) + (_b[regulation]*regulation) + (_b[rolaw]*rolaw) + (_b[corruption]*corruption)
gen thetatrade = Trade*theta
label variable theta "Property rights security"
label variable thetatrade "Property rights security x Openness"
eststo clear

eststo: quietly xtreg Overexploited L1Overexploited theta Trade thetatrade GDPpc_WDI Pdensity_WDI i.year if EEZ_2more==0, fe
*eststo: quietly xtreg Overexploited theta Trade thetatrade GDPpc_WDI Pdensity_WDI i.year, fe
esttab using L1harvest_EEZ.tex, keep(L1Overexploited theta Trade thetatrade GDPpc_WDI Pdensity_WDI) replace label star(* 0.10 ** 0.05 *** 0.01) b(%9.5f) se(%9.5f) title(Effect of Property righs security on Overexploitation, excluding countries with more than one(1) EEZ) booktabs 
eststo clear




//////////////////////////////////////////////////////////////////////////////
///////////**Robustness check with percentile rank as wgi data**/////////////
use "/Users/kbbediako/Dropbox/Kwabena_Bruno_Expropriation/myfinaldata.dta", clear

gen EEZ_2more=0
replace EEZ_2more=1 if Country=="Yemen"| Country=="United States"| Country=="Turkey"|Country=="Thailand" ///
|Country=="Sweden"|Country=="Spain"|Country=="South Africa"|Country=="Saudi Arabia"|Country=="Russia" ///
|Country=="Oman"|Country=="Morocco"|Country=="Mexico"|Country=="Malaysia"|Country=="Japan" /// 
|Country=="Israel"|Country=="Iran"|Country=="Indonesia"|Country=="Germany"|Country=="France" ///
|Country=="Egypt"|Country=="Denmark"|Country=="Cyprus"|Country=="Colombia"|Country=="Canada"
label variable EEZ_2more "(0/1) Country with 2 or more EEZs is 1, otherwise 0"
la def EEZ_2more 0 "1 EEZ" 1 "More than 1 EEZ"
la val EEZ_2more EEZ_2more

egen id = group(Code)
move id year
sort id year
xtset id year
gen Trade = Export+Import
label variable Trade "Openness"
move Trade GDPpc_WDI

///////////////////////////////////////////////
///////////**Investment model**/////////////
gen L1GDPpc_WDI = L.GDPpc_WDI
gen L1Enrollment = L.Enrollment
gen L1Trade = L.Trade
gen L1Unemployment = L.Unemployment
label variable L1GDPpc_WDI "GDP per capita"
label variable L1Enrollment "Enrollment"
label variable L1Trade "Openness"
label variable L1Unemployment "Unemployment"


eststo: quietly xtreg I_WEO voiceR stabilityR goveffR regulationR rolawR corruptionR L1GDPpc_WDI L1Enrollment L1Trade L1Unemployment i.year, fe

gen theta = (_b[voiceR]*voiceR) + (_b[stabilityR]*stabilityR) + (_b[goveffR]*goveffR) + (_b[regulationR]*regulationR) + (_b[rolawR]*rolawR) + (_b[corruptionR]*corruptionR)
label variable theta "Property rights security"
lgraph theta year

esttab using investmentR.tex, keep(voiceR stabilityR goveffR regulationR rolawR corruptionR L1GDPpc_WDI L1Enrollment L1Trade L1Unemployment) replace label star(* 0.10 ** 0.05 *** 0.01) b(%9.5f) se(%9.5f) title(Investment Model using percentile rank of governance indicators) booktabs 
eststo clear

///////////**Model of Fish harvest**/////////////
gen thetatrade = Trade*theta
gen L1Overexploited = L.Overexploited

label variable L1Overexploited "Overexploited share"
label variable thetatrade "Property rights security x Openness"

////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////**Regression with rank data wgi**/////////////

*eststo: quietly xtreg LogHarverst theta Trade thetatrade GDPpc_WDI Pdensity_WDI i.year if EEZ_2more == 0, fe
eststo: quietly xtreg Overexploited L1Overexploited theta Trade thetatrade GDPpc_WDI Pdensity_WDI i.year, fe 
*eststo: quietly xtreg collapsed theta Trade thetatrade GDPpc_WDI Pdensity_WDI i.year if EEZ_2more == 0, fe
esttab using harvestR.tex, keep(L1Overexploited theta Trade thetatrade GDPpc_WDI Pdensity_WDI) replace label star(* 0.05) b(%9.5f) se(%9.5f) title(Effect of Property righs security on Overexploitation) booktabs 
eststo clear

////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////** Fixed effects model with countries with only 1 EEZ **/////////////

eststo: quietly xtreg I_WEO voiceR stabilityR goveffR regulationR rolawR corruptionR L1GDPpc_WDI L1Enrollment L1Trade L1Unemployment i.year if EEZ_2more==0, fe
esttab using investmentR_EEZ.tex, keep(voiceR stabilityR goveffR regulationR rolawR corruptionR L1GDPpc_WDI L1Enrollment L1Trade L1Unemployment) replace label star(* 0.10 ** 0.05 *** 0.01) b(%9.5f) se(%9.5f) title(Investment Model using percentile rank of governance indicators, excluding countries with more than one(1) EEZ) booktabs 

drop theta thetatrade
gen theta = (_b[voiceR]*voiceR) + (_b[stabilityR]*stabilityR) + (_b[goveffR]*goveffR) + (_b[regulationR]*regulationR) + (_b[rolawR]*rolawR) + (_b[corruptionR]*corruptionR)
gen thetatrade = Trade*theta


label variable theta "Property rights security"
label variable thetatrade "Property rights security x Openness"
label variable thetatrade "Property rights security x Openness"

eststo clear

eststo: quietly xtreg Overexploited L1Overexploited theta Trade thetatrade GDPpc_WDI Pdensity_WDI i.year if EEZ_2more==0, fe
*eststo: quietly xtreg Overexploited theta Trade thetatrade GDPpc_WDI Pdensity_WDI i.year, fe
esttab using harvestR_EEZ.tex, keep(L1Overexploited theta Trade thetatrade GDPpc_WDI Pdensity_WDI) replace label star(* 0.10 ** 0.05 *** 0.01) b(%9.5f) se(%9.5f) title(Effect of Property righs security on Overexploitation, excluding countries with more than one(1) EEZ) booktabs 
eststo clear


//////////////////////////////////////////////////////////////////////////////
///////////** Regression with governance as an explanatory variable in the exploitation model **/////////////
use "/Users/kbbediako/Dropbox/Kwabena_Bruno_Expropriation/myfinaldata.dta", clear

gen EEZ_2more=0
replace EEZ_2more=1 if Country=="Yemen"| Country=="United States"| Country=="Turkey"|Country=="Thailand" ///
|Country=="Sweden"|Country=="Spain"|Country=="South Africa"|Country=="Saudi Arabia"|Country=="Russia" ///
|Country=="Oman"|Country=="Morocco"|Country=="Mexico"|Country=="Malaysia"|Country=="Japan" /// 
|Country=="Israel"|Country=="Iran"|Country=="Indonesia"|Country=="Germany"|Country=="France" ///
|Country=="Egypt"|Country=="Denmark"|Country=="Cyprus"|Country=="Colombia"|Country=="Canada"
label variable EEZ_2more "(0/1) Country with 2 or more EEZs is 1, otherwise 0"
la def EEZ_2more 0 "1 EEZ" 1 "More than 1 EEZ"
la val EEZ_2more EEZ_2more

egen id = group(Code)
move id year
sort id year
xtset id year
gen Trade = Export+Import
label variable Trade "Openness"
move Trade GDPpc_WDI


///////////////////////////////////////////////
///////////**Investment model**/////////////
gen L1GDPpc_WDI = L.GDPpc_WDI
gen L1Enrollment = L.Enrollment
gen L1Trade = L.Trade
gen L1Unemployment = L.Unemployment
label variable L1GDPpc_WDI "GDP per capita"
label variable L1Enrollment "Enrollment"
label variable L1Trade "Openness"
label variable L1Unemployment "Unemployment"

quietly xtreg I_WEO voice stability goveff regulation rolaw corruption L1GDPpc_WDI L1Enrollment L1Trade L1Unemployment i.year, fe

gen theta = (_b[voice]*voice) + (_b[stability]*stability) + (_b[goveff]*goveff) + (_b[regulation]*regulation) + (_b[rolaw]*rolaw) + (_b[corruption]*corruption)
label variable theta "Property rights security"
lgraph theta year

*esttab using investment.tex, keep(voiceR stabilityR goveffR regulationR rolawR corruptionR L1GDPpc_WDI L1Enrollment L1Trade L1Unemployment) replace label star(* 0.10 ** 0.05 *** 0.01) b(%9.5f) se(%9.5f) title(Investment Model using percentile rank of governance indicators) booktabs 
*eststo clear

///////////**Model of Fish harvest**/////////////
gen thetatrade = Trade * theta
gen L1Overexploited = L.Overexploited
egen governance = rowmean(voice - corruption)
gen opengove = Trade * governance

label variable opengove "Openness x Governance"
label variable L1Overexploited "Overexploited share"
label variable thetatrade "Property rights security x Openness"
label variable governance "Governance"


////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////**Regression with rank data wgi**/////////////

*eststo: quietly xtreg LogHarverst theta Trade thetatrade GDPpc_WDI Pdensity_WDI i.year if EEZ_2more == 0, fe
eststo: quietly xtreg Overexploited L1Overexploited theta Trade governance thetatrade opengove GDPpc_WDI Pdensity_WDI i.year, fe 
*eststo: quietly xtreg collapsed theta Trade thetatrade GDPpc_WDI Pdensity_WDI i.year if EEZ_2more == 0, fe
esttab using harvestG.tex, keep(L1Overexploited theta Trade governance thetatrade opengove GDPpc_WDI Pdensity_WDI) replace label star(* 0.05) b(%9.5f) se(%9.5f) title(Effect of Property rights security on Overexploitation) booktabs 
eststo clear
