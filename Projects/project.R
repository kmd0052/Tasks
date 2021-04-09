setwd("~/Desktop/Evolution/Tasks/Projects")

karell <- read.csv("karell_data.csv")

par(las=1, mgp=c(2.5, 0.25, 0), tck=-0.01)
plot(karell$Parasite.intensity, karell$TL, xlab="parasite intensity (% infected red blood cells)", ylab="telomere length (T/S ratio)")

siskin <- read.csv("siskin.csv")

head(siskin)
unique(siskin$IND) # how many different individuals there are
nrow(siskin) # how many observations


boxplot(karell$Parasite.intensity~karell$Sex, boxwex=0.25, xlab="sex (male and female) ", ylab="parasite intensity (% infected red blood cells)")
boxplot(karell$TL ~ karell$Age.class, boxwex=0.25, xlab="age class (years)", ylab="telomere length (T/S ratio)")

plot(siskin$X..parasitemia~siskin$TL.Telomere.length., boxwex=0.25, xlab="parasitemia", ylab="Telomere length (T/S ratio)")

plot(siskin$TL.Telomere.length., siskin$)

plot()
plot(siskin$BM..Body.mass.~siskin$TL.Telomere.length., boxwex=0.25, xlab="Body mass", ylab="Telomere length (T/S ratio)")
library(ids)