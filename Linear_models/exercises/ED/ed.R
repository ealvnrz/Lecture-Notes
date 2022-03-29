library(daewr)
library(gmodels)
library(MASS)
library(lsmeans)

# Completely Randomized Designs with One Factor
# Diseño completamente aleatorio con un factor

data(bread)

mod0<-lm( height ~ time, data = bread)
summary(mod0)

# Test de Hipótesis para los efectos de tratamientos

mod1<-aov( height ~ time, data = bread )
summary(mod1)

# Verificación de Supuestos

par(mfrow = c(2,2))
plot(mod1, which=5)
plot(mod1, which=1)
plot(mod1, which=2)
plot(residuals(mod1) ~ loaf, main="Residuals vs Exp. Unit",font.main=1,data=bread)
abline(h = 0, lty = 2)

# Box-Cox 
# Y=y^lambda

par(mfrow=c(1,1))
bc<-boxcox(mod1)
lambda<-bc$x[which.max(bc$y)]
lambda

# Nuevo modelo

tbread<-transform(bread, theight = height^(-.5050505))
mod2<-aov(theight~time, data = tbread)
summary(mod2)

# Verificación de Supuestos

par(mfrow = c(2,2))
plot(mod2, which=5)
plot(mod2, which=1)
plot(mod2, which=2)
plot(residuals(mod1) ~ loaf, main="Residuals vs Exp. Unit",font.main=1,data=bread)
abline(h = 0, lty = 2)

## Tukey
data(sugarbeet)
mod4 <- aov( yield ~ treat, data = sugarbeet )
summary(mod4)
mod4.tukey <- TukeyHSD( mod4, ordered = T )
mod4.tukey

# Diseño factorial con dos factores

data(COdata)
mod1<-aov( CO ~ Eth * Ratio, data = COdata )
summary(mod1)
model.tables( mod1, type = "means", se = T )
with(COdata, (interaction.plot(Eth, Ratio, CO, type = "b",pch = c(18,24,22), leg.bty = "o",
                               main = "Interaction Plot of Ethanol and air/fuel ratio",
                               xlab = "Ethanol",ylab = "CO emissions")))

# Diseño factorial con múltiples factores

data(web)

modb<-glm( cbind(signup, visitors-signup) ~ A * B * C * D, data = web, family = binomial )
summary(modb)
anova(update(modb, .~ A+B + A:B + C + A:C + B:C + A:B:C + D + A:D + B:D + A:B:D + C:D + A:C:D + B:C:D + A:B:C:D ),test = "Chisq")

prop<-web$signup / web$visitors
webp<-data.frame(web,prop)
par ( mfrow = c(1,3) )
webp1<-subset(webp, A == 1)
interaction.plot(webp1$C, webp1$D, webp1$prop, type = "l",legend=FALSE, ylim = c(.015,.0275), main = "Background = 1", 
                 xlab = "Text Color", ylab = "Proportion Signing-up")
webp2<-subset(webp, A == 2 )
interaction.plot( webp2$C, webp2$D, webp2$prop, type = "l",legend = FALSE, ylim = c(.015,.0275), main = "Background = 2",
                  xlab = "Text Color", ylab = " ")
lines(c(1.7,1.85), c(.016,.016), lty = 2)
lines(c(1.7,1.85), c(.017,.017), lty = 1)
text(1.3, .017, "Sign-up link ")
text(1.3, .016, "Sign-up Button" )
text(1.4, .018, "LEGEND" )
webp3<-subset(webp, A == 3)
interaction.plot(webp3$C, webp3$D, webp3$prop, type = "l",legend=FALSE, ylim = c(.015,.0275), main="Background = 3",
                 xlab = "Text Color", ylab = " ")

# Diseño de bloques completos aleatorizados

data(drug)
mod1<-aov(rate ~ rat + dose, data = drug)
summary(mod1)

# Diseño factorial con bloques

data(bha)
mod3 <- aov( y ~ block + strain * treat, data = bha)
summary(mod3)

with(bha,interaction.plot(treat, strain, y, type = "b", leg.bty = "o",
                 main = "BHA effect for each strain",
                 xlab = "BHA",ylab = "Response"))

# Cuadrados Latinos

data(bioeqv)
mod6 <- aov( AUC ~ Subject + Period + Treat, data = bioeqv)
summary(mod6)

model.tables( mod6, type = "means" )$tables$Treat
TukeyHSD( mod6, "Treat")


# Diseño por bloques incompletos

data(taste)
mod1 <- aov( score ~ panelist + recipe, data = taste)
summary(mod1)
lsmeans(mod1, pairwise ~ recipe, adjust = ("tukey"))
