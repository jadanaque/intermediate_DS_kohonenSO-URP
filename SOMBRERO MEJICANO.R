#SENTENCIA PERSP
#Regresión logística
par(bg="lightgreen")
x = seq(-8, 8, length= 30)
y = x
f = function(x, y) 
r = 1/(1+exp(-(0.8*x+1.2*y-0.2*x*y)))
z = outer(x, y, f)
op = par(bg = "white")
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")

#Normal Estándar Bivariante
x = seq(-3, 3, length= 15)
y = x
f = function(x, y) 
{ r = (1/(2*pi))*exp(-0.5*(x^2+y^2))}
z = outer(x, y, f)
op = par(bg = "red")
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightgreen")

#Sombrero mejicano
x = seq(-10, 10, length= 30)
y = x
f = function(x, y) 
{ r = sqrt(x^2+y^2);
10 * sin(r)/r }
z = outer(x, y, f)
op = par(bg = "white")
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "gold")

