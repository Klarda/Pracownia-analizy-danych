# reda produkuje 3t, rumia produkuje 4t, 
# gdańsk potrzebuje 5t, gdynia potrzebuje 2t
#koszt transportu Reda-Gdańsk 100zl, Reda-Gdynia- 150zl, Rumia-Gdansk-130zl, Rumia-Gdynia- 170zl

#Import bibliotek
library(GA)

Evaluation<-function(x){
  penalty=0
  if(x[1]+x[2]-3>0) penalty=penalty+x[1]+x[2]-3
  if(x[3]+x[4]-4>0) penalty=penalty+x[3]+x[4]-4
  
  if(5-x[1]-x[3]>0) penalty=penalty+5-x[1]-x[3]
  if(2-x[2]-x[4]>0) penalty=penalty+2-x[2]-x[4]
  return(sum(c(100,150,130,170)*x+(10^(9))*penalty))
  } #funkcja z kosztami transportu
#ograniczenia

GA<-ga(type="real-valued", fitness=function(x)-Evaluation(x), lower=c(0,0,0,0), upper=c(3,3,4,4), popSize=100, maxiter=100) #lower aby nie byy ujemny
# upper aby nie bylo transportu wiekszego niz produkcja

summary(GA)

#x1+x2 to suma wyprowadzonego twoaru z redy

#Praca domowa
#Reda produkuje 3t, Rumia produkuje  4t
#Gdańsk potrzebuje 3t, Gdynia i Sopot potrzebuj 2t
#koszt transportu z 
#redy: do Gdańska-100zl, do Gdyni- 150zl, do Sopotu- 130zl
#rumi: do Gdańska-130zl, do Gdyni- 170zl, do Sopotu- 150zl
Evaluation<-function(x){
  penalty=0
  if(x[1]+x[2]+x[3]-3>0) penalty=penalty+x[1]+x[2]+x[3]-3
  if(x[4]+x[5]+x[6]-4>0) penalty=penalty+x[4]+x[5]+x[6]-4
  if(3-x[1]-x[4]>0) penalty=penalty+3-x[1]-x[4]
  if(2-x[2]-x[5]>0) penalty=penalty+2-x[2]-x[5]
  if(2-x[3]-x[6]>0) penalty=penalty+2-x[3]-x[6]
  return(sum(c(100,150,130,130,170,150)*x+(10^(9))*penalty))
}
GA<-ga(type="real-valued", fitness=function(x)-Evaluation(x), lower=c(0,0,0,0,0,0), upper=c(3,3,3,4,4,4), popSize=100, maxiter=100) 
summary(GA)
