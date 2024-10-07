data<-read.csv2(file="Datos_modelos.csv",sep=";")
modname<-c("fecha","ACCESS-CM2","NorESM2-LM","MPI-ESM1-2-LR","CanESM5")

names(data) = modname

# Filtramos los datos entre 2015 y 2025
datos_filtrados <- data %>%
  filter(fecha >= as.Date("2025-01-01") & fecha <= as.Date("2034-12-01"))


# Transformamos la tabla para tener dos columnas: temperatura y método
datos_largos <- datos_filtrados  %>%
  gather(key = "metodo", value = "temperatura", -fecha)

# Mostramos el resultado
print(datos_largos)
dim(datos_largos)
