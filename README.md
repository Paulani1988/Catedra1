# TRABAJO CATEDRA 01 PROGRAMACION EN R
## DATA OPERACIONAL SISTEMA DE TRANSPORTE DE CONCENTRADO POR LINEA PRESURIZADA 

**Realizado por: Paula Alvarez - Marcelo Carmona**

### Contexto 
<p style="text-align: center;">
El concentrado de cobre es transportado a través de cañerias de acero al carbono de alta resistencia API 5L X65 con revestimiento interior de HDPE. La impulsion inicia en el pk 0,0 a partir de los estanques de almacenamiento de concentrado "Holding Tank" y ubicados en la cota de terreno 2.235 msnm, en la estación de bombeo (EB). Desde este punto, y hasta el km 22 la tubería continúa por zona de pendientes relativamente altas. Desde su inicio y hasta el km 15,9 (km 15,916), la tubería tiene un diámetro nominal de 7,00 pulgadas. A partir de este punto, y hasta el pk 36,6 (km 36,587) la tubería cambia su diámetro nominal a 8,00 pulgadas, producto de una modificación en el diseño original ejecutado el año 2016. Desde el km 22, el trazado presenta un tramo largo por un valle de bajas pendientes hasta llegar a al kilómetro 91,9 donde se encuentra emplazada la estación de válvulas N°1, EV1. Entre la estación de válvulas EV1 y EV2 ubicada en el pk 134,9 se encuentra una depresión que finalmente se empina hasta llegar a la estación disipadora terminal EDT. Desde la estación EV2 hasta la estación EDT, el Concentraducto tiene un diámetro de 5,56 pulgadas nominales.
En resumen, el sistema de transporte de concentrado contempla un ducto de revestido, una estación de bombeo (EB), dos estaciones de válvulas (EV1 y EV2), cuatro estaciones de monitoreo ubicadas en puntos altos (EM1B, EM2, EM3 Y EM4) y una estación disipadora terminal (EDT) ubicada en instalaciones de Muelle. Cada estación de monitoreo, válvulas y disipadora terminakl dispone de instrumentación donde se registra la presión en linea del transporte de concentrado.
</p>

### Objetivos

-   Automatizar un informe en rmarkdown o quarto que muestre graficos exploratorios y conclusiones

-   Acotar la variables a analizar a máximo 05, buscando métodos de imputación o extracción de datos faltantes

-   Obtener de analisis exploratorio de al menos 5 variables , min, máx, media

### Cargar librerias

```{r}
library(tidyverse)
library(tinytex)
library(readxl)
```

### Cargar base de datos a utilizar

```{r, warning=FALSE}
#oculto los warning para cuando se aplique Render no aparezcan en informe 
BD <- read_excel("Dataset_sistema.xlsx") #Archivo excel con BD operación de concentraducto
PD <- data.frame(BD) #Se transforma la base datos a clase dataframe 
str(PD)
```

## Análisis exploratorio de datos (EDA)

¿Que tipo de variación existe dentro de cada una de mis variables?.

```{r}
ggplot(data = PD) + geom_histogram(mapping= aes(x= EV01), binwidth = 390, na.rm = TRUE)
```

Referente a la variable EV01 la barra mas alta de este histograma tiene más de 1750 observaciones con rango app sobre 6000 Kpa de presión en la línea.

```{r}
ggplot(data = PD) + geom_histogram(mapping= aes(x= EV.02), binwidth = 390, na.rm = TRUE)
```

::: {style="text-align: justify;"}
Referente a la variable EV02, la barra más alta de este histograma tiene más de 2500 observaciones con rango de presión sobre 4000 Kpa app. En esta variables existen 131 datos no finitos por lo que se debera evaluar su imputacion o extracción respectiva.
:::

```{r}
# Gráfico de dispersión con ggplot2
ggplot(data = PD, aes(x =Fecha , y = EV.02)) +
  geom_point(na.rm = TRUE) +
  labs(title = "Gráfico de Dispersión Estación EV02",
       x = "Fecha",
       y = "Presión, Kpa") +
  theme_minimal() # Un tema limpio para el gráfico
```

```{r}
# DE VARIABLE EV02 CALCULAMOS SU COEFICIENTE DE VARIANZA PARA EVALUAR SU GRADO DE       # DISPERSION
s <- sd(PD$EV.02, na.rm = TRUE) #Calcula la desviacion estandar de variable EV02
m <- mean(PD$EV.02, na.rm=TRUE) #Calcula la media de variable EV02
CV <- (s/m) *100 #Calcula coeficiente de variacion de variable EV02
CV
#El valor del coeficiente de variación de 23 indica una variabilidad moderada en los #datos, pero no es lo suficientemente alto como para considerarlos altamente #heterogéneos.Se puede indicar que CV de 23 sugiere que la media aritmética es una #medida de tendencia central razonable, aunque no tan representativa como con un CV más #bajo
```

```{r}
ggplot(data = PD) + geom_histogram(mapping= aes(x= SM1), binwidth = 390, na.rm= TRUE)
#Grafica histograma para variable SM1
```


Referente a la variable SM1, la barra más alta de este histograma tiene más de 1000 observaciones con rango de presión sobre 15250 Kpa app. En esta variables existen 52 datos no finitos por lo que se deberá evaluar su imputación o extracción respectiva.


### Valores inusuales (outliers)

Se utiliza graficos del tipo caja y bigotes para determinar valores atipicos o outliers en variables de estudio EV01, EV02 y SM1

```{r}

par(mfrow = c(1, 3)) # Divide la ventana gráfica en 1 fila y 3 columnas para 3 boxplots 

boxplot(PD$EV01,
        main = "EV01",
        ylab = "Kpa", #Unidad de presión
        col = "skyblue",
        border = "blue")

boxplot(PD$EV.02,
        main = "EV02",
        ylab = "Kpa", #Unidad de presión
        col = "lightgreen",
        border = "darkgreen")

boxplot(PD$SM1,
        main = "SM1",
        ylab = "Kpa", #Unidad de presión
        col = "salmon",
        border = "darkred")

par(mfrow = c(1, 1)) # Restaura la configuración de la ventana gráfica
```


Los valores registrados en variable EV02 de esta grafica de caja y bigote, además de dispersión nos indica que entre mes de marzo y abril operacionalmente se produce un empaquetado o cierre del concentraducto, como también ocurren maniobras de despresurizacion en zona de mayor pendiente( ver imagen de trazado), lo que se debe indagar con mas profundidad para revisar si se debe a un tema de cambios de fase o error de operación.


```{r}
summary(PD$EV01)
summary(PD$EV.02)
summary(PD$SM1)
```

::: {style="text-align: justify;"}
De acuerdo a los resultados de este resumen de parámetros estadisticos de las tres variables a analizar, es posible visualizar que en algún periodo ocurre una despresurización del tramo de linea esto debido a valor minimo alcanzado en estación SM1, EV01 y Ev02.

Otra información relevante que es posible extraer de estos datos de variable SM1, es revisar si hubo exceso de presión detectado en instrumentos comparando registro con dato de ingenieria como valor limite máximo permitido, para ello, valor max se compara con valor limite para determinar si ocurrió o no exceso de presión.
:::

```{r}
valor_maxSM1= max(PD$SM1,na.rm = TRUE) #Muestra el valor maximo alcanzado de presión en SM1 excluyendo valores NA
valor_limSM1 <- 22500 #valor 22500 Kpa indicado por diseño ingenieria como maxima presion
if (valor_limSM1 >  valor_maxSM1) {
  print ("No Hubo exceso de Presión. Bajo el limite máximo")
    } else {  
    print("Hubo exceso de presión. Precaución!!")
  
}
```

### Analisis de imputacion o extraccion de datos

```{r}

# Usar sapply para contar los NAs en cada variable
conteo_nas_por_columna <- sapply(PD, function(x) sum(is.na(x)))

# Mostrar solo las columnas que tienen NA´s
columnas_con_nas <- conteo_nas_por_columna[conteo_nas_por_columna > 0]
if (length(columnas_con_nas) > 0) {
  print("Columnas con NAs y su conteo:")
  print(columnas_con_nas)
} else {
  print("\n¡No hay NAs en ninguna columna de este dataframe!")
}
```


De acuerdo a la revisión de la data de variables EV01, EV02, SM1 notamos que existen 131 registros en variable EV02, y 52 registros en variable SM01 que no son numericas por lo que en esta etapa revisaremos el tipo de dato, y como se procederá para imputarlo de acuerdo a criterios operacionales establecidos.

Para analizar sobre que criterio se imputarán los datos NAs calculamos el % de datos de este tipo en cada columna para corroborar su representatividad.


```{r}
calcular_porcentaje_na_por_columna <- function(dataframe) {
  # Verificar si la entrada es un dataframe
  if (!is.data.frame(dataframe)) {
    stop("La entrada debe ser un dataframe.")
  }

  # Obtener el número total de registros (filas) en el dataframe
  total_registros <- nrow(dataframe)

  # Si el dataframe está vacío, no hay NAs que contar
  if (total_registros == 0) {
    message("El dataframe no tiene registros.")
    return(data.frame(
      Columna = character(0),
      NA_Count = numeric(0),
      NA_Porcentaje = numeric(0)
    ))
  }

  # Calcular el número de NAs por columna
  na_count_por_columna <- colSums(is.na(dataframe))

  # Calcular el porcentaje de NAs por columna
  # Usamos pmax(1, total_registros) para evitar división por cero si total_registros es 0,
  # aunque ya lo manejamos antes, es una buena práctica.
  na_porcentaje_por_columna <- (na_count_por_columna / total_registros) * 100

  # Crear un dataframe con los resultados
  resultados_na <- data.frame(
    Columna = names(PD),
    NA_Count = na_count_por_columna,
    NA_Porcentaje = round(na_porcentaje_por_columna, 2) # Redondear a 2 decimales
  )

  # Opcional: Ordenar los resultados para ver las columnas con más NAs primero
  #resultados_na <- resultados_na[order(-resultados_na$NA_Porcentaje), ]

  

  return(resultados_na)
  
}
calcular_porcentaje_na_por_columna(PD)
```

::: {style="text-align: justify;"}
Se calcularon los porcentajes de NAs de cada variable donde podemos observar en todos los casos valores inferiores al 1%, lo que nos lleva a determinar y eliminar dichos datos ya que podrian considerarse despreciables en dicho analisis sin impactar los valores totales, ademas por las variables en estudio que corresponden a valores y presiones reales del concentraducto no se podrian asumir valores para los NAs en estudio. Por lo tanto se decide eliminar los registros con valores NAs.


#### Eliminación de datos

```{r}
#Se crea una replica del data frame original para trabajar en #la eliminación de datos
PA <- na.omit(PD)
if (nrow(PA) > 0) {
  str(PA)
  cat("\nDimensiones del dataframe PA son:", nrow(PA), "filas x", ncol(PA), "columnas\n") 
  cat ("\nSe eliminaron", nrow(PD)-nrow(PA), "registros.")
} else {
  print("¡Advertencia! Todas las filas fueron eliminadas. El dataframe está vacío.")
}
```

```{r}

print("\n--- Verificación de NAs en el nuevo dataframe ---")
nas_restantes <- sum(is.na(PA))
if (nas_restantes == 0) {
  print("¡Éxito! No quedan NAs en el dataframe limpio.")
} else {
  cat("Aún quedan", nas_restantes, "NAs en el dataframe PA.\n")
}
```

```{r}
summary(PA$EV01)
summary(PA$EV.02)
summary(PA$SM1)
```
