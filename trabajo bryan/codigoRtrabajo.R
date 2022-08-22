library(papaja)
library(readxl)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(summarytools)
library(kableExtra)
library(agricolae)
library(nortest)
library(PerformanceAnalytics)
r_refs("r-references.bib")
Base<- read_excel("baseTrabajo.xlsx")
attach(Base)

getwd()
setwd(dir = "trabajo bryan")

######################################
titulo="Edad (en años)"
gEdad=ggplot(data=Base ,aes(x=as.factor(`Edad (Años)`), fill=as.factor(`Edad (Años)`)))
gEdad +geom_bar( )+ggtitle(titulo)+theme(plot.title = element_text(hjust = 0.5))+
scale_fill_grey(start = 0.25, end = 0.75)+ theme(legend.position="none")+labs(x="Edad",y="Número de personas")
ggplot(data=Base ,aes(x=0, y=`Edad (Años)`))+geom_boxplot()+ggtitle(titulo)+theme(plot.title = element_text(hjust = 0.5))

descripcion=data.frame(Promedio=mean(`Edad (Años)`)
                       ,Mediana=median(`Edad (Años)`),
                       Varianza=var(`Edad (Años)`),
                       SD=sd(`Edad (Años)`),
                       CV=sd(`Edad (Años)`)/ mean(`Edad (Años)`)*100,
                       Q1=quantile(`Edad (Años)`,probs=0.25),
                       Q2=quantile(`Edad (Años)`,probs=0.5),
                       Q3=quantile(`Edad (Años)`,probs=0.75),
                       Minimo=min(`Edad (Años)`),
                       Maximo=max(`Edad (Años)`))
row.names(descripcion)="Datos"
knitr::kable(t(descripcion),"latex",digits = 2)

############################################

titulo="Tiempo de estudio semanal(en horas)"
gVar=ggplot(data=Base ,aes(x=as.factor(`Tiempo de estudio semanal (h)`), fill=as.factor(`Tiempo de estudio semanal (h)`)))

gVar +geom_bar( )+ggtitle(titulo)+theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_grey(start = 0.25, end = 0.75)+ theme(legend.position="none")+labs(x="Tiempo de estudio ",y="Número de personas")

ggplot(data=Base ,aes(x=0, y=`Tiempo de estudio semanal (h)`))+geom_boxplot()+ggtitle(titulo)+theme(plot.title = element_text(hjust = 0.5))
descripcion=data.frame(Promedio=mean(`Tiempo de estudio semanal (h)`)
                       ,Mediana=median(`Tiempo de estudio semanal (h)`),
                       Varianza=var(`Tiempo de estudio semanal (h)`),
                       SD=sd(`Tiempo de estudio semanal (h)`),
                       CV=sd(`Tiempo de estudio semanal (h)`)/ mean(`Tiempo de estudio semanal (h)`)*100,
                       Q1=quantile(`Tiempo de estudio semanal (h)`,probs=0.25),
                       Q2=quantile(`Tiempo de estudio semanal (h)`,probs=0.5),
                       Q3=quantile(`Tiempo de estudio semanal (h)`,probs=0.75),
                       Minimo=min(`Tiempo de estudio semanal (h)`),
                       Maximo=max(`Tiempo de estudio semanal (h)`))
row.names(descripcion)="Datos"
knitr::kable(t(descripcion),"latex",digits = 2)

############################################

titulo="Número de Tutorias "
gVar=ggplot(data=Base ,aes(x=as.factor(`¿A cuántas tutorias asistió durante el semestre?`), fill=as.factor(`¿A cuántas tutorias asistió durante el semestre?`)))

gVar +geom_bar( )+ggtitle(titulo)+theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_grey(start = 0.25, end = 0.75)+ theme(legend.position="none")+labs(x="Número de tutorias ",y="Número de personas")

ggplot(data=Base ,aes(x=0, y=`¿A cuántas tutorias asistió durante el semestre?`))+geom_boxplot()+ggtitle(titulo)+theme(plot.title = element_text(hjust = 0.5))

descripcion=data.frame(Promedio=mean(`¿A cuántas tutorias asistió durante el semestre?`)
                       ,Mediana=median(`¿A cuántas tutorias asistió durante el semestre?`),
                       Varianza=var(`¿A cuántas tutorias asistió durante el semestre?`),
                       SD=sd(`¿A cuántas tutorias asistió durante el semestre?`),
                       CV=sd(`¿A cuántas tutorias asistió durante el semestre?`)/ mean(`¿A cuántas tutorias asistió durante el semestre?`)*100,
                       Q1=quantile(`¿A cuántas tutorias asistió durante el semestre?`,probs=0.25),
                       Q2=quantile(`¿A cuántas tutorias asistió durante el semestre?`,probs=0.5),
                       Q3=quantile(`¿A cuántas tutorias asistió durante el semestre?`,probs=0.75),
                       Minimo=min(`¿A cuántas tutorias asistió durante el semestre?`),
                       Maximo=max(`¿A cuántas tutorias asistió durante el semestre?`))
row.names(descripcion)="Datos"
knitr::kable(t(descripcion),"latex",digits = 2)

#########################################################
titulo="Promedio del Primer Semestre "

gVar=ggplot(data=Base ,aes(x=Base$`Promedio  Semestre`))
gVar +geom_histogram(bins = 10)+ggtitle(titulo)+theme(plot.title = element_text(hjust = 0.5))+
scale_fill_grey(start = 0.25, end = 0.75)+ theme(legend.position="none")+labs(x="Puntaje",y="Número de personas")

ggplot(data=Base ,aes(x=0, y=Base$`Promedio  Semestre`))+geom_boxplot()+ggtitle(titulo)+theme(plot.title = element_text(hjust = 0.5))

descripcion=data.frame(Promedio=mean(`Promedio Primer Semestre`)
                       ,Mediana=median(`Promedio Primer Semestre`),
                       Varianza=var(`Promedio Primer Semestre`),
                       SD=sd(`Promedio Primer Semestre`),
                       CV=sd(`Promedio Primer Semestre`)/ mean(`Promedio Primer Semestre`)*100,
                       Q1=quantile(`Promedio Primer Semestre`,probs=0.25),
                       Q2=quantile(`Promedio Primer Semestre`,probs=0.5),
                       Q3=quantile(`Promedio Primer Semestre`,probs=0.75),
                       Minimo=min(`Promedio Primer Semestre`),
                       Maximo=max(`Promedio Primer Semestre`))
row.names(descripcion)="Datos"
knitr::kable(t(descripcion),"latex",digits = 2)

################################################################################ 
##Tiempo de estudio semanal (horas) : mínimo 30 horas y máximo 55 horas[razón]


t.test(Base$`Tiempo de estudio semanal (h)`, y = NULL,
       alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)$conf.int


##Interpretación: al extraer 100 veces la muestra cada una con 221
##individuos , se espera que aproximadamente 95 de estas muestras tendrán
#una media para el tiempo de estudio semanal entre 42 y 44 horas cabe
#aclarar que la distribución del tiempo semanal resulto aproximadamente
#uniforme .


#############################################################################

# Hipótesis estadística para la diferencia de medias :

#el equipo en psicologia desea saber si hay un diferencia significativa
#entre el promedio de primer semestre de las personas que si realizaron
#actividades curriculares y las que no .

      
#primero realizamos el test para comparar varianzas :
      
mSi=filter(Base,`¿Realiza actividades extracurriculares?`=="SI")
mNO=filter(Base,`¿Realiza actividades extracurriculares?`=="NO")
    
var.test(mSi$`Promedio  Semestre`,mNO$`Promedio  Semestre`)
    
   ## con un pvalor de 0.6768 no se rechaza $H_0$ , es decir las varianzas son
    #iguales .
    
  
    #Realizamos el test de muestras no pareadas y varianzas iguales :
      
    mSi=filter(Base,`¿Realiza actividades extracurriculares?`=="SI")
    mNO=filter(Base,`¿Realiza actividades extracurriculares?`=="NO")
    
    t.test(x=mSi$`Promedio  Semestre`, y=mNO$`Promedio  Semestre`, alternative="two.sided", mu=0, 
           paired=FALSE, var.equal=TRUE, conf.level=0.95)
    
   ## Existe evidencia estadística para no rechazar $H_0$ , pues el p-valor es
    ##ded 0.8125 es demasiado alto para el valor de $\alpha=0.05$ entonces no
    ##hay diferencia significativa entre las medias de las personas que
    #ealizan actividades extracurriculares .
    
