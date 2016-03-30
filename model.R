#test

eddypro=read.csv("eddypro.csv", skip = 1)[-1, c(1:69, 71:77)]
eddypro = eddypro[ , c(-32, -33, -37, -38, -63)]
eddypro$daytime = as.double(eddypro$daytime) 
for(i in c(4, 6:length(eddypro))){
  eddypro[, i] = as.double(as.vector(eddypro[, i]))
}
eddypro[eddypro == -9999] = NA
eddypro[3:875, ] -> eddypro
eddypro[eddypro$daytime == "3", ] -> eddypro
eddypro2=na.exclude(eddypro) 
cor.matrix = round(cor(eddypro2[, c(-1, -2, -3, -4)]), 2)
# Я посмотрел таблицу корреляции, и выбрал все значимые факторы
model1=lm(RH~(es+air_temperature+air_molar_volume+LE+h2o_flux+rand_err_LE+rand_err_h2o_flux+e+specific_humidity+Tdew+water_vapor_density+h2o_molar_density+air_density)^13, data=eddypro)
anova(model1)
# Спустя несколько часов анализа от RSudio, я убрал менее значимые факторы
model2=lm(RH~(es+air_temperature+LE+h2o_flux+rand_err_LE+e+Tdew+h2o_molar_density+air_density)^9, data=eddypro)
anova(model2)
summary(model2)
# Убрал еще несколько факторов
model3=lm(RH~(es+air_temperature+LE+h2o_flux+e+h2o_molar_density)^6, data=eddypro)
anova(model3)
summary(model3)
# С помошью анализа anova были убранны группы факторов, коротые между собой были мало значимы
model4=lm(RH~(es+air_temperature+LE+h2o_flux+e+h2o_molar_density)^6-LE:h2o_flux-LE:e-LE:h2o_molar_density, data=eddypro)
anova(model4)
model5=lm(RH~(es+air_temperature+LE+h2o_flux+e+h2o_molar_density)^6-LE:h2o_flux-LE:e-LE:h2o_molar_density-h2o_flux:e-e:h2o_molar_density -es:h2o_flux:h2o_molar_density-air_temperature:e:h2o_molar_density-air_temperature:LE:h2o_flux:e-air_temperature:LE:h2o_flux:e:h2o_molar_density , data=eddypro)
anova(model5)
model6=lm(RH~(es+air_temperature+LE+h2o_flux+e+h2o_molar_density)^6-LE:h2o_flux-LE:e-LE:h2o_molar_density-h2o_flux:e-e:h2o_molar_density -es:h2o_flux:h2o_molar_density-air_temperature:e:h2o_molar_density-air_temperature:LE:h2o_flux:e-air_temperature:LE:h2o_flux:e:h2o_molar_density-h2o_flux:h2o_molar_density-es:h2o_flux:e-air_temperature:LE:e-air_temperature:LE:h2o_flux:h2o_molar_density-es:LE:h2o_flux:e:h2o_molar_density, data=eddypro)
anova(model6)
# Кажется, что с каждым удалением становиться все больше незначимых между собой факторов. Продолжаем убирать
model7=lm(RH~(es+air_temperature+LE+h2o_flux+e+h2o_molar_density)^6-LE:h2o_flux-LE:e-LE:h2o_molar_density-h2o_flux:e-e:h2o_molar_density -es:h2o_flux:h2o_molar_density-air_temperature:e:h2o_molar_density-air_temperature:LE:h2o_flux:e-air_temperature:LE:h2o_flux:e:h2o_molar_density-h2o_flux:h2o_molar_density-es:h2o_flux:e-air_temperature:LE:e-air_temperature:LE:h2o_flux:h2o_molar_density-es:LE:h2o_flux:e:h2o_molar_density-es:LE:h2o_molar_density-air_temperature:LE:h2o_molar_density-air_temperature:LE:h2o_molar_density, data=eddypro)
anova(model7)
model8=lm(RH~(es+air_temperature+LE+h2o_flux+e+h2o_molar_density)^6-LE:h2o_flux-LE:e-LE:h2o_molar_density-h2o_flux:e-e:h2o_molar_density -es:h2o_flux:h2o_molar_density-air_temperature:e:h2o_molar_density-air_temperature:LE:h2o_flux:e-air_temperature:LE:h2o_flux:e:h2o_molar_density-h2o_flux:h2o_molar_density-es:h2o_flux:e-air_temperature:LE:e-air_temperature:LE:h2o_flux:h2o_molar_density-es:LE:h2o_flux:e:h2o_molar_density-es:LE:h2o_molar_density-air_temperature:LE:h2o_molar_density-air_temperature:LE:h2o_molar_density-air_temperature:h2o_flux:e -h2o_flux:e:h2o_molar_density-es:air_temperature:LE:e-es:air_temperature:h2o_flux:h2o_molar_density-es:LE:h2o_flux:h2o_molar_density-es:h2o_flux:e:h2o_molar_density -es:air_temperature:LE:h2o_flux:h2o_molar_density, data=eddypro)
anova(model8)
model9=lm(RH~(es+air_temperature+LE+h2o_flux+e+h2o_molar_density)^6-LE:h2o_flux-LE:e-LE:h2o_molar_density-h2o_flux:e-e:h2o_molar_density -es:h2o_flux:h2o_molar_density-air_temperature:e:h2o_molar_density-air_temperature:LE:h2o_flux:e-air_temperature:LE:h2o_flux:e:h2o_molar_density-h2o_flux:h2o_molar_density-es:h2o_flux:e-air_temperature:LE:e-air_temperature:LE:h2o_flux:h2o_molar_density-es:LE:h2o_flux:e:h2o_molar_density-es:LE:h2o_molar_density-air_temperature:LE:h2o_molar_density-air_temperature:LE:h2o_molar_density-air_temperature:h2o_flux:e -h2o_flux:e:h2o_molar_density-es:air_temperature:LE:e-es:air_temperature:h2o_flux:h2o_molar_density-es:LE:h2o_flux:h2o_molar_density-es:h2o_flux:e:h2o_molar_density -es:air_temperature:LE:h2o_flux:h2o_molar_density-air_temperature:h2o_flux:h2o_molar_density-LE:h2o_flux:e  -es:air_temperature:LE:h2o_flux -es:air_temperature:LE:h2o_molar_density-es:air_temperature:h2o_flux:e -es:air_temperature:e:h2o_molar_density-es:LE:e:h2o_molar_density -es:air_temperature:LE:h2o_flux:e:h2o_molar_density, data=eddypro)
anova(model9)
model10=lm(RH~(es+air_temperature+LE+h2o_flux+e+h2o_molar_density)^6-LE:h2o_flux-LE:e-LE:h2o_molar_density-h2o_flux:e-e:h2o_molar_density -es:h2o_flux:h2o_molar_density-air_temperature:e:h2o_molar_density-air_temperature:LE:h2o_flux:e-air_temperature:LE:h2o_flux:e:h2o_molar_density-h2o_flux:h2o_molar_density-es:h2o_flux:e-air_temperature:LE:e-air_temperature:LE:h2o_flux:h2o_molar_density-es:LE:h2o_flux:e:h2o_molar_density-es:LE:h2o_molar_density-air_temperature:LE:h2o_molar_density-air_temperature:LE:h2o_molar_density-air_temperature:h2o_flux:e -h2o_flux:e:h2o_molar_density-es:air_temperature:LE:e-es:air_temperature:h2o_flux:h2o_molar_density-es:LE:h2o_flux:h2o_molar_density-es:h2o_flux:e:h2o_molar_density -es:air_temperature:LE:h2o_flux:h2o_molar_density-air_temperature:h2o_flux:h2o_molar_density-LE:h2o_flux:e  -es:air_temperature:LE:h2o_flux -es:air_temperature:LE:h2o_molar_density-es:air_temperature:h2o_flux:e -es:air_temperature:e:h2o_molar_density-es:LE:e:h2o_molar_density -es:air_temperature:LE:h2o_flux:e:h2o_molar_density-LE:h2o_flux:h2o_molar_density-LE:e:h2o_molar_density  -es:LE:h2o_flux:e-air_temperature:LE:e:h2o_molar_density-air_temperature:h2o_flux:e:h2o_molar_density -es:air_temperature:LE:h2o_flux:e -es:air_temperature:h2o_flux:e:h2o_molar_density , data=eddypro)
anova(model10)
#И последний рывочек
model11=lm(RH~(es+air_temperature+LE+h2o_flux+e+h2o_molar_density)^6-LE:h2o_flux-LE:e-LE:h2o_molar_density-h2o_flux:e-e:h2o_molar_density -es:h2o_flux:h2o_molar_density-air_temperature:e:h2o_molar_density-air_temperature:LE:h2o_flux:e-air_temperature:LE:h2o_flux:e:h2o_molar_density-h2o_flux:h2o_molar_density-es:h2o_flux:e-air_temperature:LE:e-air_temperature:LE:h2o_flux:h2o_molar_density-es:LE:h2o_flux:e:h2o_molar_density-es:LE:h2o_molar_density-air_temperature:LE:h2o_molar_density-air_temperature:LE:h2o_molar_density-air_temperature:h2o_flux:e -h2o_flux:e:h2o_molar_density-es:air_temperature:LE:e-es:air_temperature:h2o_flux:h2o_molar_density-es:LE:h2o_flux:h2o_molar_density-es:h2o_flux:e:h2o_molar_density -es:air_temperature:LE:h2o_flux:h2o_molar_density-air_temperature:h2o_flux:h2o_molar_density-LE:h2o_flux:e  -es:air_temperature:LE:h2o_flux -es:air_temperature:LE:h2o_molar_density-es:air_temperature:h2o_flux:e -es:air_temperature:e:h2o_molar_density-es:LE:e:h2o_molar_density -es:air_temperature:LE:h2o_flux:e:h2o_molar_density-LE:h2o_flux:h2o_molar_density-LE:e:h2o_molar_density  -es:LE:h2o_flux:e-air_temperature:LE:e:h2o_molar_density-air_temperature:h2o_flux:e:h2o_molar_density -es:air_temperature:LE:h2o_flux:e -es:air_temperature:h2o_flux:e:h2o_molar_density -LE:h2o_flux:e:h2o_molar_density-es:air_temperature:LE:e:h2o_molar_density , data=eddypro)
anova(model11)
# Финиш!! Для красоты еще можно убрать es:LE:h2o_flux  и air_temperature:LE:h2o_flux, но и model11 можно считать конечным результатом! :)
