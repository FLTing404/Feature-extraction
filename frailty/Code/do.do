///Table 2
use "G:\2025\SESDEP\data\HRS.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses
stcox i.cses i.sex age i.marital i.alive
stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise

stcox i.ases
stcox i.ases i.sex age i.marital i.alive
stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise

stcox i.ac
stcox i.ac i.sex age i.marital i.alive
stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\CHARLS.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses
stcox i.cses i.sex age i.marital i.alive
stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise

stcox i.ases
stcox i.ases i.sex age i.marital i.alive
stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise

stcox i.ac
stcox i.ac i.sex age i.marital i.alive
stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\SHARE.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses i.cid
stcox i.cses i.sex age i.marital i.alive i.cid
stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid

stcox i.ases i.cid
stcox i.ases i.sex age i.marital i.alive i.cid
stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid

stcox i.ac i.cid
stcox i.ac i.sex age i.marital i.alive i.cid
stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid
clear


use "G:\2025\SESDEP\data\ELSA.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses
stcox i.cses i.sex age i.marital i.alive
stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise

stcox i.ases
stcox i.ases i.sex age i.marital i.alive
stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise

stcox i.ac
stcox i.ac i.sex age i.marital i.alive
stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\MHAS.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses
stcox i.cses i.sex age i.marital i.alive
stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise

stcox i.ases
stcox i.ases i.sex age i.marital i.alive
stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise

stcox i.ac
stcox i.ac i.sex age i.marital i.alive
stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\KLoSA.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses
stcox i.cses i.sex age i.marital i.alive
stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise

stcox i.ases
stcox i.ases i.sex age i.marital i.alive
stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise

stcox i.ac
stcox i.ac i.sex age i.marital i.alive
stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\pooled.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses i.cid
stcox i.cses i.sex age i.marital i.alive i.cid
stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid

stcox i.ases i.cid
stcox i.ases i.sex age i.marital i.alive i.cid
stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid

stcox i.ac i.cid
stcox i.ac i.sex age i.marital i.alive i.cid
stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid
clear


///Supplementary Table S14
use "G:\2025\SESDEP\data\SHARE.dta"
keep if base==1
stset time, failure(depression==1)
by group, sort : stcox i.cses i.cid
by group, sort : stcox i.cses i.sex age i.marital i.alive i.cid
by group, sort : stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid

by group, sort : stcox i.ases i.cid
by group, sort : stcox i.ases i.sex age i.marital i.alive i.cid
by group, sort : stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid

by group, sort : stcox i.ac i.cid
by group, sort : stcox i.ac i.sex age i.marital i.alive i.cid
by group, sort : stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid
clear


///Supplementary Table S15
use "G:\2025\SESDEP\data\HRS.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\CHARLS.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\SHARE.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid
clear


use "G:\2025\SESDEP\data\ELSA.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\MHAS.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\KLoSA.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\pooled.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid
clear


///Supplementary Figure S10
use "G:\2025\SESDEP\data\HRS.dta"
keep if median==1
sem (cses -> frailty, ) (cses -> social, ) (cses -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent

sem (ases -> frailty, ) (ases -> social, ) (ases -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent

sem (ac -> frailty, ) (ac -> social, ) (ac -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent
clear


///Supplementary Figure S11
use "G:\2025\SESDEP\data\CHARLS.dta"
keep if median==1
sem (cses -> frailty, ) (cses -> social, ) (cses -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent

sem (ases -> frailty, ) (ases -> social, ) (ases -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent

sem (ac -> frailty, ) (ac -> social, ) (ac -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent
clear


///Supplementary Figure S12
use "G:\2025\SESDEP\data\SHARE.dta"
keep if median==1
sem (cses -> frailty, ) (cses -> social, ) (cses -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, )(cid -> frailty, ) (cid -> social, ) (cid -> depression, ), nocapslatent

sem (ases -> frailty, ) (ases -> social, ) (ases -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, )(cid -> frailty, ) (cid -> social, ) (cid -> depression, ), nocapslatent

sem (ac -> frailty, ) (ac -> social, ) (ac -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, )(cid -> frailty, ) (cid -> social, ) (cid -> depression, ), nocapslatent
clear


///Supplementary Figure S13
use "G:\2025\SESDEP\data\ELSA.dta"
keep if median==1
sem (cses -> frailty, ) (cses -> social, ) (cses -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent

sem (ases -> frailty, ) (ases -> social, ) (ases -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent

sem (ac -> frailty, ) (ac -> social, ) (ac -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent
clear


///Supplementary Figure S14
use "G:\2025\SESDEP\data\MHAS.dta"
keep if median==1
sem (cses -> frailty, ) (cses -> social, ) (cses -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent

sem (ases -> frailty, ) (ases -> social, ) (ases -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent

sem (ac -> frailty, ) (ac -> social, ) (ac -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent
clear


///Supplementary Figure S15
use "G:\2025\SESDEP\data\KLoSA.dta"
keep if median==1
sem (cses -> frailty, ) (cses -> social, ) (cses -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent

sem (ases -> frailty, ) (ases -> social, ) (ases -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent

sem (ac -> frailty, ) (ac -> social, ) (ac -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent
clear


///Figure 1 
use "G:\2025\SESDEP\data\pooled.dta"
keep if median==1
sem (cses -> frailty, ) (cses -> social, ) (cses -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, )(cid -> frailty, ) (cid -> social, ) (cid -> depression, ), nocapslatent

sem (ases -> frailty, ) (ases -> social, ) (ases -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, )(cid -> frailty, ) (cid -> social, ) (cid -> depression, ), nocapslatent

sem (ac -> frailty, ) (ac -> social, ) (ac -> depression, ) (frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, )(cid -> frailty, ) (cid -> social, ) (cid -> depression, ), nocapslatent
clear


/// Supplementary Table S19
use "G:\2025\SESDEP\data\HRS.dta"
keep if median==1
tabulate cses , gen(dummy)
sem (dummy2 -> frailty, ) (dummy2 -> social, ) (dummy2 -> depression, ) (dummy3 -> frailty, ) (dummy3 -> social, ) (dummy3-> depression, )(frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent
clear

use "G:\2025\SESDEP\data\HRS.dta"
keep if median==1
tabulate ases , gen(dummy)
sem (dummy2 -> frailty, ) (dummy2 -> social, ) (dummy2 -> depression, ) (dummy3 -> frailty, ) (dummy3 -> social, ) (dummy3-> depression, )(frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent
clear

use "G:\2025\SESDEP\data\HRS.dta"
keep if median==1
tabulate ac, gen(dummy)
sem (dummy2 -> frailty, ) (dummy2 -> social, ) (dummy2 -> depression, ) (dummy3 -> frailty, ) (dummy3 -> social, ) (dummy3-> depression, )(dummy4 -> frailty, ) (dummy4 -> social, ) (dummy4 -> depression, ) (dummy5 -> frailty, ) (dummy5 -> social, ) (dummy5-> depression, )(frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent
clear


use "G:\2025\SESDEP\data\CHARLS.dta"
keep if median==1
tabulate cses , gen(dummy)
sem (dummy2 -> frailty, ) (dummy2 -> social, ) (dummy2 -> depression, ) (dummy3 -> frailty, ) (dummy3 -> social, ) (dummy3-> depression, )(frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent
clear

use "G:\2025\SESDEP\data\CHARLS.dta"
keep if median==1
tabulate ases , gen(dummy)
sem (dummy2 -> frailty, ) (dummy2 -> social, ) (dummy2 -> depression, ) (dummy3 -> frailty, ) (dummy3 -> social, ) (dummy3-> depression, )(frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent
clear

use "G:\2025\SESDEP\data\CHARLS.dta"
keep if median==1
tabulate ac, gen(dummy)
sem (dummy2 -> frailty, ) (dummy2 -> social, ) (dummy2 -> depression, ) (dummy3 -> frailty, ) (dummy3 -> social, ) (dummy3-> depression, )(dummy4 -> frailty, ) (dummy4 -> social, ) (dummy4 -> depression, ) (dummy5 -> frailty, ) (dummy5 -> social, ) (dummy5-> depression, )(frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent
clear


use "G:\2025\SESDEP\data\SHARE.dta"
keep if median==1
tabulate cses , gen(dummy)
sem (dummy2 -> frailty, ) (dummy2 -> social, ) (dummy2 -> depression, ) (dummy3 -> frailty, ) (dummy3 -> social, ) (dummy3-> depression, )(frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, )(cid -> frailty, ) (cid -> social, ) (cid -> depression, ), nocapslatent
clear

use "G:\2025\SESDEP\data\SHARE.dta"
keep if median==1
tabulate ases , gen(dummy)
sem (dummy2 -> frailty, ) (dummy2 -> social, ) (dummy2 -> depression, ) (dummy3 -> frailty, ) (dummy3 -> social, ) (dummy3-> depression, )(frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, )(cid -> frailty, ) (cid -> social, ) (cid -> depression, ), nocapslatent
clear

use "G:\2025\SESDEP\data\SHARE.dta"
keep if median==1
tabulate ac, gen(dummy)
sem (dummy2 -> frailty, ) (dummy2 -> social, ) (dummy2 -> depression, ) (dummy3 -> frailty, ) (dummy3 -> social, ) (dummy3-> depression, )(dummy4 -> frailty, ) (dummy4 -> social, ) (dummy4 -> depression, ) (dummy5 -> frailty, ) (dummy5 -> social, ) (dummy5-> depression, )(frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, )(cid -> frailty, ) (cid -> social, ) (cid -> depression, ), nocapslatent
clear


use "G:\2025\SESDEP\data\ELSA.dta"
keep if median==1
tabulate cses , gen(dummy)
sem (dummy2 -> frailty, ) (dummy2 -> social, ) (dummy2 -> depression, ) (dummy3 -> frailty, ) (dummy3 -> social, ) (dummy3-> depression, )(frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent
clear

use "G:\2025\SESDEP\data\ELSA.dta"
keep if median==1
tabulate ases , gen(dummy)
sem (dummy2 -> frailty, ) (dummy2 -> social, ) (dummy2 -> depression, ) (dummy3 -> frailty, ) (dummy3 -> social, ) (dummy3-> depression, )(frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent
clear

use "G:\2025\SESDEP\data\ELSA.dta"
keep if median==1
tabulate ac, gen(dummy)
sem (dummy2 -> frailty, ) (dummy2 -> social, ) (dummy2 -> depression, ) (dummy3 -> frailty, ) (dummy3 -> social, ) (dummy3-> depression, )(dummy4 -> frailty, ) (dummy4 -> social, ) (dummy4 -> depression, ) (dummy5 -> frailty, ) (dummy5 -> social, ) (dummy5-> depression, )(frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent
clear


use "G:\2025\SESDEP\data\MHAS.dta"
keep if median==1
tabulate cses , gen(dummy)
sem (dummy2 -> frailty, ) (dummy2 -> social, ) (dummy2 -> depression, ) (dummy3 -> frailty, ) (dummy3 -> social, ) (dummy3-> depression, )(frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent
clear

use "G:\2025\SESDEP\data\MHAS.dta"
keep if median==1
tabulate ases , gen(dummy)
sem (dummy2 -> frailty, ) (dummy2 -> social, ) (dummy2 -> depression, ) (dummy3 -> frailty, ) (dummy3 -> social, ) (dummy3-> depression, )(frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent
clear

use "G:\2025\SESDEP\data\MHAS.dta"
keep if median==1
tabulate ac, gen(dummy)
sem (dummy2 -> frailty, ) (dummy2 -> social, ) (dummy2 -> depression, ) (dummy3 -> frailty, ) (dummy3 -> social, ) (dummy3-> depression, )(dummy4 -> frailty, ) (dummy4 -> social, ) (dummy4 -> depression, ) (dummy5 -> frailty, ) (dummy5 -> social, ) (dummy5-> depression, )(frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent
clear


use "G:\2025\SESDEP\data\KLoSA.dta"
keep if median==1
tabulate cses , gen(dummy)
sem (dummy2 -> frailty, ) (dummy2 -> social, ) (dummy2 -> depression, ) (dummy3 -> frailty, ) (dummy3 -> social, ) (dummy3-> depression, )(frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent
clear

use "G:\2025\SESDEP\data\KLoSA.dta"
keep if median==1
tabulate ases , gen(dummy)
sem (dummy2 -> frailty, ) (dummy2 -> social, ) (dummy2 -> depression, ) (dummy3 -> frailty, ) (dummy3 -> social, ) (dummy3-> depression, )(frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent
clear

use "G:\2025\SESDEP\data\KLoSA.dta"
keep if median==1
tabulate ac, gen(dummy)
sem (dummy2 -> frailty, ) (dummy2 -> social, ) (dummy2 -> depression, ) (dummy3 -> frailty, ) (dummy3 -> social, ) (dummy3-> depression, )(dummy4 -> frailty, ) (dummy4 -> social, ) (dummy4 -> depression, ) (dummy5 -> frailty, ) (dummy5 -> social, ) (dummy5-> depression, )(frailty -> social, ) (frailty -> depression, ) (social -> depression, ) (support -> depression, ) (sex -> frailty, ) (sex -> social, ) (sex -> depression, ) (age -> frailty, ) (age -> social, ) (age -> depression, ) (marital -> frailty, ) (marital -> social, ) (marital -> depression, ) (alive -> frailty, ) (alive -> social, ) (alive -> depression, ) (smoke -> frailty, ) (smoke -> social, ) (smoke -> depression, ) (drink -> frailty, ) (drink -> social, ) (drink -> depression, ) (exercise -> frailty, ) (exercise -> social, ) (exercise -> depression, ) (sf -> depression, ), nocapslatent
clear


///Supplementary Table S16
use "G:\2025\SESDEP\data\HRS.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses i.sex age1 i.marital i.alive i.smoke i.drink i.exercise
stcox i.ases i.sex age1 i.marital i.alive i.smoke i.drink i.exercise
stcox i.ac i.sex age1 i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\CHARLS.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses i.sex age1 i.marital i.alive i.smoke i.drink i.exercise
stcox i.ases i.sex age1 i.marital i.alive i.smoke i.drink i.exercise
stcox i.ac i.sex age1 i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\SHARE.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses i.sex age1 i.marital i.alive i.smoke i.drink i.exercise i.cid
stcox i.ases i.sex age1 i.marital i.alive i.smoke i.drink i.exercise i.cid
stcox i.ac i.sex age1 i.marital i.alive i.smoke i.drink i.exercise i.cid
clear


use "G:\2025\SESDEP\data\ELSA.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses i.sex age1 i.marital i.alive i.smoke i.drink i.exercise
stcox i.ases i.sex age1 i.marital i.alive i.smoke i.drink i.exercise
stcox i.ac i.sex age1 i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\MHAS.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses i.sex age1 i.marital i.alive i.smoke i.drink i.exercise
stcox i.ases i.sex age1 i.marital i.alive i.smoke i.drink i.exercise
stcox i.ac i.sex age1 i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\KLoSA.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses i.sex age1 i.marital i.alive i.smoke i.drink i.exercise
stcox i.ases i.sex age1 i.marital i.alive i.smoke i.drink i.exercise
stcox i.ac i.sex age1 i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\pooled.dta"
keep if base==1
stset time, failure(depression==1)
stcox i.cses i.sex age1 i.marital i.alive i.smoke i.drink i.exercise i.cid
stcox i.ases i.sex age1 i.marital i.alive i.smoke i.drink i.exercise i.cid
stcox i.ac i.sex age1 i.marital i.alive i.smoke i.drink i.exercise i.cid
clear


///Supplementary Table S17
use "G:\2025\SESDEP\data\HRS.dta"
keep if m3==1
stset time, failure(depression==1)
stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise
stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise
stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\CHARLS.dta"
keep if m3==1
stset time, failure(depression==1)
stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise
stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise
stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\SHARE.dta"
keep if m3==1
stset time, failure(depression==1)
stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid
stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid
stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid
clear


use "G:\2025\SESDEP\data\ELSA.dta"
keep if m3==1
stset time, failure(depression==1)
stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise
stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise
stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\MHAS.dta"
keep if m3==1
stset time, failure(depression==1)
stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise
stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise
stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\KLoSA.dta"
keep if m3==1
stset time, failure(depression==1)
stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise
stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise
stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\pooled.dta"
keep if m3==1
stset time, failure(depression==1)
stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid
stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid
stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid
clear


///Supplementary Table S18
use "G:\2025\SESDEP\data\HRS.dta"
keep if m4==1
stset time1, failure(depression1==1) scale(1)
stcrreg i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise, compete(depression1==2)
stcrreg i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise, compete(depression1==2)
stcrreg i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise, compete(depression1==2)
clear


use "G:\2025\SESDEP\data\CHARLS.dta"
keep if m4==1
stset time1, failure(depression1==1) scale(1)
stset time1, failure(depression1==1) scale(1)
stcrreg i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise, compete(depression1==2)
stcrreg i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise, compete(depression1==2)
stcrreg i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise, compete(depression1==2)
clear


use "G:\2025\SESDEP\data\SHARE.dta"
keep if m4==1
stset time1, failure(depression1==1) scale(1)
stcrreg i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid, compete(depression1==2)
stcrreg i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid, compete(depression1==2)
stcrreg i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid, compete(depression1==2)
clear


use "G:\2025\SESDEP\data\ELSA.dta"
keep if m4==1
stset time1, failure(depression1==1) scale(1)
stcrreg i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise, compete(depression1==2)
stcrreg i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise, compete(depression1==2)
stcrreg i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise, compete(depression1==2)
clear


use "G:\2025\SESDEP\data\MHAS.dta"
keep if m4==1
stset time1, failure(depression1==1) scale(1)
stset time1, failure(depression1==1) scale(1)
stcrreg i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise, compete(depression1==2)
stcrreg i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise, compete(depression1==2)
stcrreg i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise, compete(depression1==2)
clear


use "G:\2025\SESDEP\data\KLoSA.dta"
keep if m4==1
stset time1, failure(depression1==1) scale(1)
stset time1, failure(depression1==1) scale(1)
stcrreg i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise, compete(depression1==2)
stcrreg i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise, compete(depression1==2)
stcrreg i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise, compete(depression1==2)
clear


use "G:\2025\SESDEP\data\pooled.dta"
keep if m4==1
stset time1, failure(depression1==1) scale(1)
stcrreg i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid, compete(depression1==2)
stcrreg i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid, compete(depression1==2)
stcrreg i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid, compete(depression1==2)
clear


///Figure 3
use "G:\2025\SESDEP\data\HRS.dta"
keep if base==1
stset time, failure(depression==1)
by sex, sort : stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise
by sex, sort : stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise
by sex, sort : stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\CHARLS.dta"
keep if base==1
stset time, failure(depression==1)
by sex, sort : stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise
by sex, sort : stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise
by sex, sort : stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\SHARE.dta"
keep if base==1
stset time, failure(depression==1)
by sex, sort : stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid
by sex, sort : stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid
by sex, sort : stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid
clear


use "G:\2025\SESDEP\data\ELSA.dta"
keep if base==1
stset time, failure(depression==1)
by sex, sort : stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise
by sex, sort : stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise
by sex, sort : stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\MHAS.dta"
keep if base==1
stset time, failure(depression==1)
by sex, sort : stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise
by sex, sort : stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise
by sex, sort : stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\KLoSA.dta"
keep if base==1
stset time, failure(depression==1)
by sex, sort : stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise
by sex, sort : stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise
by sex, sort : stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise
clear


use "G:\2025\SESDEP\data\pooled.dta"
keep if base==1
stset time, failure(depression==1)
by sex, sort : stcox i.cses i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid
by sex, sort : stcox i.ases i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid
by sex, sort : stcox i.ac i.sex age i.marital i.alive i.smoke i.drink i.exercise i.cid
clear