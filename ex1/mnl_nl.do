
clear

cls

cd "C:/Users/Giovanni Cavalcanti/OneDrive - Insper - Institudo de Ensino e Pesquisa/trimestre_6/fgv_io/list_01/io_fgv_list_1/ex1"

use "ta02_database_01.dta"


*-----------------------------------------*
* MULTINOMIAL LOGIT
*-----------------------------------------*


reg log_diff_share price x1 x2 x3 x4, vce(cluster market)

ivreg2 log_diff_share x1 x2 x3 x4 (price = iv1 iv2 iv3 iv4 iv5 iv6), first cluster(product)



*-----------------------------------------*
* NESTED LOGIT
*-----------------------------------------*

gen nest=firm
replace nest=9 if firm==3 | firm==6

egen aux = sum(share), by(city nest)
gen aux2 = share/aux
gen ln_share_nest = ln(aux2)
drop aux aux2

reg y ln_share_nest sugar mushy price i.brand, vce(cluster city)

ivreg2 y ln_share_nest sugar mushy (price = z*) i.brand, first cluster(city)

reg y c.ln_share_nest#i.nest sugar mushy price i.brand, vce(cluster city)

ivreg2 y c.ln_share_nest#i.nest sugar mushy (price = z*) i.brand, first cluster(city)

