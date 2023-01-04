# Web scrapping pagina web nahana jeans.
# Obtener informacion de cada categoria (articulo, descripcion, talles, precio).

"""
Created on Sat Nov 19 18:37:36 2022
@author: Facundo Juliá

Librerias:  "requests" version 2.27.1 para enviar solicitudes HTTP.
            "beautifulsoup4" version 4.11.1 para extraer informacion de paginas web formato HTML o XML
            "pandas" version 1.4.2 para convertir la lista de datos en data frame y exportarlo en formato "xlsx"

Fuentes:    https://www.freecodecamp.org/
            https://www.py4e.com/
            https://realpython.com/python-requests/
            https://www.crummy.com/software/BeautifulSoup/bs4/doc/
"""

import requests 
from bs4 import BeautifulSoup as bs
import pandas as pd

# =============================================================================
# CATEGORIAS Y LINKS PARA REALIZAR WEB SCRAPPING
# =============================================================================
enlaces = [ 'https://nahanajeans.com/online/index.php//pantalon/chupin.html?p=1', 
            'https://nahanajeans.com/online/index.php//pantalon/chupin.html?p=2', 
            'https://nahanajeans.com/online/index.php//pantalon/chupin.html?p=3', 
            'https://nahanajeans.com/online/index.php//pantalon/chupin.html?p=4', 
            'https://nahanajeans.com/online/index.php//pantalon/chupin.html?p=5', 
            'https://nahanajeans.com/online/index.php//pantalon/chupin.html?p=6', 
            'https://nahanajeans.com/online/index.php//pantalon/chupin.html?p=7', 
            'https://nahanajeans.com/online/index.php//pantalon/chupin.html?p=8', 
            'https://nahanajeans.com/online/index.php//pantalon/oxford.html?p=1', 
            'https://nahanajeans.com/online/index.php//pantalon/oxford.html?p=2', 
            'https://nahanajeans.com/online/index.php//pantalon/recto.html?p=1', 
            'https://nahanajeans.com/online/index.php//pantalon/mom.html?p=1', 
            'https://nahanajeans.com/online/index.php//pantalon/wide-leg.html?p=1', 
            'https://nahanajeans.com/online/index.php//remeras.html?p=1', 
            'https://nahanajeans.com/online/index.php//musculosa.html?p=1', 
            'https://nahanajeans.com/online/index.php//camisa.html?p=1', 
            'https://nahanajeans.com/online/index.php//campera.html?p=1', 
            'https://nahanajeans.com/online/index.php//short.html?p=1', 
            'https://nahanajeans.com/online/index.php//short.html?p=2', 
            'https://nahanajeans.com/online/index.php//pollera.html?p=1', 
            'https://nahanajeans.com/online/index.php//bermuda.html?p=1', 
            'https://nahanajeans.com/online/index.php//capri.html?p=1', 
            'https://nahanajeans.com/online/index.php//enterito.html?p=1']

lista = []

for url in enlaces:
    html = requests.get(url)
    content = html.content
    soup = bs(content, "lxml") #"html.parser"
    
# Extraigo la categoria del producto "find"
    categoria = soup.find("div",{"class":"page-title category-title"})
    categoria = categoria.text.rsplit()[0]
    print(categoria)
    
# Extraigo el articulo, descripcion, talle y precio del producto "find" y "findAll"
    for item in soup.findAll("li",{"class":"item product-item"}):    
        articulo = item.find("h2",{"class":"product-name"})
        descripcion = item.find("div",{"class":"desc product-item-description"})
        precio = item.find("p",{"class":"minimal-price"})
        link = item.find("a").get("href")
        talles = ""
        
        for nums in item.findAll("li",{"class":"option"}):
            talle = nums.find("font",{"size":1})
            talles = talle.text + " - " + talles
        
        articulo = articulo.text.split()[-1]
        descripcion = descripcion.text
        precio = precio.text.rsplit()[0].split("$")[1]
        
        nueva_fila = [articulo, descripcion, talles, precio]
        lista.append(nueva_fila)
        
        print(articulo)
        print(descripcion)
        print(precio)
        print(talles)
        #print(link)
        
# Convierto la informacion en un data frame
df = pd.DataFrame(lista, columns=["Articulo", "Descripcion", "Talles", "Precio"])

# Exporto el data frame a formato "xlsx"
df.to_excel("Nahana.xlsx")




    