## LECTURA DE FACTURA DE AFIP EN FORMATO PDF

# Utiliza la libreria "pdftools" version 22.04.0 para extraer texto de un archivo PDF.
# Utiliza la libreria "stringr" version 1.4.0 de tidyverse para trabajar con cadenas de caracteres.
# Utiliza la libreria "xlsx" version 0.6.0 para exportar un data frame de R a un archivo excel.
# Uso operador %>% pipe para simplificar el codigo, que envia un valor, o el resultado de una expresión, a la siguiente llamada de función o expresión.

library(tidyverse)
library(stringr)
library(pdftools)
library(xlsx)

# Cargo el path donde se encuentran todos los archivos para leer 
setwd("C:/Users/Facu/Dropbox/Ciencia datos/4. Proyecto Final/Tere-Boutique/2. Mineria texto - Facturas afip/Facturas")
archivos <- list.files(path = "C:/Users/Facu/Dropbox/Ciencia datos/4. Proyecto Final/Tere-Boutique/2. Mineria texto - Facturas afip/Facturas")

# Creo una lista para guardar los datos de los clientes que se van a minar: dni, razon social, domicilio, fecha, punto venta
prueba <- list(Dni = character(),
               RazonSocial = character(),
               Domicilio = character(),
               FechaEmision = character(),
               PuntoVenta = character(),
               Articulo = character(),
               Precio = character())

# Recorro la carpeta con todos los archivos en formato pdf cuyos nombres estan en la lista "archivos"
for (nombre in archivos) {
  # Leo los archivos pdf y los convierto a texto "pdf_text" luego separo por renglones "str_split"
  # Cada elemento de la lista "factura" es una pagina.
  factura <- pdf_text(nombre) %>% str_split("\n")
  
  # Armo un data frame solo con los datos de la primera pagina y quito los espacios en blanco "str_squish"
  aux<-unlist(factura[1]) %>% str_squish()

  # Extraigo la informacion de interes utilizando expresiones regulares "str_extract".
  # El nombre del cliente esta precedido por un numero y un espacio y la oracion que comienza con "Apellido ... :"
  # El domicilio son letras y numeros luego de la oracion "Domicilio: " y un espacio y que finaliza con un enter "\n"
  dni <- aux %>% str_extract("(?<=\\:\\s)[[:digit:]]+(?=\\sApellido)") %>% na.omit() %>% str_to_lower()
  punto_venta <- aux %>% str_extract("(?<=Punto de Venta\\:\\s)[[:digit:]]+(?=\\s[[:upper:]])") %>% na.omit() %>% str_to_lower()
  fecha_emision <- aux %>% str_extract("(?<=Fecha de Emisión\\:\\s)[[:graph:]]+(?=\\z)") %>% na.omit() %>% str_to_lower()
  razon_social <- aux %>% str_extract("(?<=/ Razón Social\\:\\s)[[:print:]]+(?=\\z)") %>% na.omit() %>% str_to_lower()
  domicilio <- aux %>% str_extract("(?<=Domicilio\\:\\s)[[:print:]]+(?=\\z)") %>% na.omit() %>% str_to_lower()
  articulo <- aux %>% str_extract("(?<=1\\s)[[:print:]]+(?=\\sunidades)") %>% na.omit() %>% str_to_lower()
  articulo <- str_c(articulo, collapse = "; ")
  precio <- aux %>% str_extract("(?<=Subtotal:\\s)[[:print:]]+(?=,)") %>% na.omit() %>% str_to_lower()
  
  dniOK <- !identical(dni, character(0))
  puntoOK <- !identical(punto_venta, character(0))
  fechaOK <- !identical(fecha_emision, character(0))
  razonOK <- !identical(razon_social, character(0))
  domicilioOK <- !identical(domicilio, character(0))
  articuloOK <- !identical(articulo, character(0))
  precioOK <- !identical(precio, character(0))
  
  # Cargo los datos al data frame
  if (dniOK) {
    prueba$Dni <- append(prueba$Dni, dni)
  }else{
    prueba$Dni <- append(prueba$Dni, "0")
  }
  
  if (puntoOK) {
    prueba$PuntoVenta <- append(prueba$PuntoVenta, punto_venta)
  }else{
    prueba$PuntoVenta <- append(prueba$PuntoVenta, "0")
  }
  if (fechaOK) {
    prueba$FechaEmision <- append(prueba$FechaEmision, fecha_emision)
  }else{
    prueba$FechaEmision <- append(prueba$FechaEmision, "0")
  }
  if (razonOK) {
    prueba$RazonSocial <- append(prueba$RazonSocial, razon_social)
  }else{
    prueba$RazonSocial <- append(prueba$RazonSocial, "0")
  }
  if (domicilioOK) {
    prueba$Domicilio <- append(prueba$Domicilio, domicilio)  
  }else{
    prueba$Domicilio <- append(prueba$Domicilio, "0")  
  }
  if (articuloOK) {
    prueba$Articulo <- append(prueba$Articulo, articulo)  
  }else{
    prueba$Articulo <- append(prueba$Articulo, "0")  
  }
  if (precioOK) {
    prueba$Precio <- append(prueba$Precio, precio)
  }else{
    prueba$Precio <- append(prueba$Precio, "0")
  }
}

# Convierto la lista de datos de cada cliente en un data frame
df <- data.frame(prueba)
head(df,100)

# Exporto el data frame a formato "csv"
write.csv(df, "clientes_tereboutique.csv")

#Exporto el data frame a formato excell "xlsx"

write.xlsx(df,                    # Data frame a ser exportado
           file = "Clientes_tereboutique.xlsx",
           sheetName = "Cliente", # Nombre de la hoja de Excel
           col.names = TRUE,     # Incluir los nombres de las columnas (TRUE) o no (FALSE)
           row.names = FALSE,     # Incluir los nombres de las filas (TRUE) o no (FALSE)
           append = FALSE,       # Agregar a un archivo existente (TRUE) o no (FALSE)
           showNA = TRUE,        # Si TRUE, los NA serán celdas vacías
           password = NULL)      # Contraseña como cadena de caracteres

#str_view_all(aux,"(?<=Subtotal:\\s)[[:print:]]+(?=,)")


