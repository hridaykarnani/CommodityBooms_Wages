clear all

global maindir "C:/Users/Hriday/Documents/R/fentareas/MIC_III/Research"
use"${maindir}/data/casen1996.dta"
keep comu
export excel ${maindir}/data/comunas1996.xlsx, firstrow(varlabels) replace

clear all
use"${maindir}/data/casen1998.dta"
keep comu
export excel ${maindir}/data/comunas1998.xlsx, firstrow(varlabels) replace

clear all
use"${maindir}/data/casen2000.dta"
keep comu
export excel ${maindir}/data/comunas2000.xlsx, firstrow(varlabels) replace

clear all
use"${maindir}/data/casen2003.dta"
keep comu
export excel ${maindir}/data/comunas2003.xlsx, firstrow(varlabels) replace

clear all
use"${maindir}/data/casen2006.dta"
keep comu
export excel ${maindir}/data/comunas2006.xlsx, firstrow(varlabels) replace

clear all
use"${maindir}/data/casen2009.dta"
keep comu
export excel ${maindir}/data/comunas2009.xlsx, firstrow(varlabels) replace

clear all
use"${maindir}/data/casen2011.dta"
keep comu
export excel ${maindir}/data/comunas2011.xlsx, firstrow(varlabels) replace

clear all
use"${maindir}/data/casen2013.dta"
keep comu
export excel ${maindir}/data/comunas2013.xlsx, firstrow(varlabels) replace

clear all
use"${maindir}/data/casen2015.dta"
keep comu
export excel ${maindir}/data/comunas2015.xlsx, firstrow(varlabels) replace

clear all
use"${maindir}/data/casen2017.dta"
keep comu
export excel ${maindir}/data/comunas2017.xlsx, firstrow(varlabels) replace
