# Web scrapping pagina web nahana jeans.
# Obtener links de todas las categorias de productos.

"""
Created on Mon Nov 21 15:49:31 2022
@author: Facundo Juliá

Librerias:  "requests" version 2.27.1 para enviar solicitudes HTTP.
            "beautifulsoup4" version 4.11.1 para extraer informacion de paginas web en formato HTML o XML

Fuentes:    https://www.freecodecamp.org/
            https://www.py4e.com/
            https://realpython.com/python-requests/
            https://www.crummy.com/software/BeautifulSoup/bs4/doc/
"""

import requests 
from bs4 import BeautifulSoup as bs

# =============================================================================
# CATEGORIAS Y LINKS PARA REALIZAR WEB SCRAPPING
# =============================================================================
categorias = [
    "/pantalon/chupin",
    "/pantalon/oxford",
    "/pantalon/recto",
    "/pantalon/mom",
    "/pantalon/wide-leg",
    "/remeras",
    "/musculosa",
    "/camisa",
    "/campera",
    "/short",
    "/pollera",
    "/bermuda",
    "/capri",
    "/enterito",
    ]


    # Dentro de la pagina principal voy obteniendo los enlaces para ingresar a cada categoria de producto. Ej: pantalon, remera, musculosa.
enlaces = []

for i in categorias:
    url = "https://nahanajeans.com/online/index.php/{}.html?p=1".format(i)

        # Envio una solicitud "get" a la URL especificada la respuesta se almacena en la variable "html"
    html = requests.get(url)    

        # Almaceno el contenido de la respuesta (la informacion) en bytes en la variable "content"
    content = html.content

        # Analizo y extraigo la informacion con la libreria bs4 en formato "lxml"
    soup = bs(content, "lxml") #"html.parser"
    
    enlaces.append(url)

# Obtengo los links que tienen mas de 1 pagina, cada pagina tiene 12 articulos
    try:
        paginas = soup.find("p",{"class":"amount"})
        paginas = int(paginas.text.split()[-1])
        if paginas > 12:
            pag = round((paginas/12)) + 1
            for j in range(2,pag):
                url_pag = "https://nahanajeans.com/online/index.php/{}.html?p={}".format(i,j)
                enlaces.append(url_pag)
    except:
        pass

# =============================================================================
# 
# =============================================================================
