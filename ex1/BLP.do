
clear

cls

cd "C:/Users/Giovanni Cavalcanti/OneDrive - Insper - Institudo de Ensino e Pesquisa/trimestre_6/fgv_io/list_01/io_fgv_list_1/ex1"

use "ta02_database_01.dta"


*-----------------------------------------*
* BLP
*-----------------------------------------*

use ta03_database_01_demo, clear

use ta03_database_01, clear

blp share sugar mushy, markets(city) endog(price = z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16 z17 z18 z19 z20) stochastic(price)

blp share sugar mushy, markets(city) demofile(ta03_database_01_demo) endog(price = z1 z2 z3 z4 z5 z6 z7 z8 z9 z10 z11 z12 z13 z14 z15 z16 z17 z18 z19 z20) stochastic(price = income income_sq child, sugar=income age) elast(price,1)

matrix list e(elast)








