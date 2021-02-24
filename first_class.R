#1st Class

12%%5 # division remainder # 12 nin 10 a bölümünden kalaný veriyor.
12%/%5 # integer part of the division # üsttekiye ayný ??
12^5 # 12 üzeri 5
121^(1/2) # kök iþlemi



abs(-15) # mutlak deðer
log(12,2) # logaritma // örnek: log2(12)
exp(5) # exponential
log(12, exp(1)) # natural logarithm
factorial(12) # factorial

vec1 <- 1:12
vec2 <- seq(from=0, to=20, by=2)
vec3 <- seq(0,20,length=10)

# removing objects
rm(vec1)

# the rep() function allows to create vectors from the pattern
vec4 <- rep(1,15) # 15 tane 1
vec5 <- rep(1:5,2) # 2 kere 1 den 5 e kadar
vec6 <- rep(c(1,4,7), c(2,1,2)) #¶ 2 adet 1 1 adet 4 2 adet 7
vec7 <- rep(1:3, 2, length= 5) # 1 den 3 e kadar 2 kere ama uzunluk 6 dan kýsaysa length neyse orda durur


vec1 + 2 # vectordeki her bir elemaný 2 ile toplar
vec8 <- vec1/3 # vectordeki her bir elemaný 3 e böler

