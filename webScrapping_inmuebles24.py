from playwright.sync_api import sync_playwright
from bs4 import BeautifulSoup as bs
import math 
import numpy as np
import re
import pandas as pd
from datetime import datetime

def normalize(s: str) -> str:

    """ normalize quita los acentos del string y remplaza los espacios en blanco por -.
     
    Parámetros:
        s: str
    Salida:
        s: str sin acentos y con - en vez de espacios ' '
       """
    replacements = (
        ("á", "a"),
        ("é", "e"),
        ("í", "i"),
        ("ó", "o"),
        ("ú", "u"),
    )
    for a, b in replacements:
        s = s.replace(a, b).replace(a.upper(), b.upper()).lower().replace(' ', '-')
    return s
   
def extraerAtributos(soup: bs) -> list:
    """ extraerAtributos escrapea la información de los siguientes atributos:
        Parámetros:
        soup: Código html. Tipo de objeto: Beautiful Soup

        Salida: Todas las salidas son str

        constriudos: metros cuadrados construidos.
        total: metros cuadrados totales. 
        estacionamiento: número de lugares de estacionamiento
        recamaras: número de recámaras
        wc: número de baños
        antiguo: Antigüedad
        precio: Precio de la vivienda. MN son pesos mexicanos y USD son dólares americanos
        direccion: calle y colonia 
        dia: fecha de publicación y visualizaciones del post
        consulta: fecha en la cual se consultó el post
    """
    #Creamos variables
    construidos = None
    total = None
    estacionamiento = None
    recamaras = None
    wc = None
    antiguo = None
    precio = None
    direccion = None
    dia = None
    consulta = datetime.now()

    #Escrapeamos la zona donde se encuentra la info
    datos = [j.text for j in soup.find_all('li', {'class': 'icon-feature'})]

    #Determinamos qué tipo de atributo se escrapeo 
    for j in datos:
        if re.search(r'\bConstruido\b',j):
            construidos = re.search(r'\d+',j).group(0)
        elif re.search(r'\bTotal\b',j):
            total = re.search(r'\d+',j).group(0)
        elif re.search(r'\bBaño\b',j) or re.search(r'\bBaños\b',j):
             wc = re.search(r'\d+',j).group(0)
        elif re.search(r'\bEstacionamiento\b',j) or re.search(r'\bEstacionamientos\b',j):
            estacionamiento = re.search(r'\d+',j).group(0)
        elif re.search(r'\bRecámara\b',j) or re.search(r'\bRecámaras\b',j):
            recamaras = re.search(r'\d+',j).group(0)
        elif re.search(r'\bAntigüedad\b',j):
            antiguo = re.search(r'\d+',j).group(0)
        elif re.search(r'\bA estrenar\b',j):
            antiguo = 'A estrenar'
    
    precios = [i.text for i in soup.find_all('div', {'class': 'price-items'})]
    #Revisamos si solo tiene precio de renta, si hay de compra, sólo se extrae el de renta
    if len(precios) <= 1:
        precio = precios[0].replace('\n','').replace('\t','')
    else:
        precio = precios[1].replace('\n','').replace('\t','')
    
    address = soup.find('h2', {'class': 'title-location'})
    if address:
            direccion = address.text.replace('Ver en mapa', '').replace('\t','').replace('\n','')
    day = soup.find('div', {'id':'user-views'})
    if day:
        dia = day.text
    return [construidos, total, estacionamiento, recamaras, wc, antiguo, precio, direccion, dia, consulta]

def crearBaseDatos(link:str, muestra = 20, delay = 3000) -> pd.DataFrame:
    """
    crearBaseDatos crea la base de datos.
    Entrada:
        link: link con la lista de propiedades
    Opcionales:
        muestra: viviendas por página
        delay: espera para cerrar y abrir la siguiente página
    Salida: pandas Dataframe con los siguientes atributos, todos siendo str
        Post ID: ID del post
        Fecha de consulta: Fecha de consulta
        Link al post: Link a la propiedad
        Dirección: Dirección de la propiedad (calle, colonia)
        Fecha de publicación: fecha de publicación (en días) y número de visualizaciones
        Metros cuadrados totales: Metros cuadrados totales de la propiedad
        Metros cuadrados construidos: Metros cuadrados construidos de la propiedad
        Número de recámaras: Número de recámaras de la propiedad
        Número de baños: Número de baños de la propiedad
        Número de lugares de estacionamiento: Número de lugares de estacionamiento de la propiedad
        Fecha de antigüedad: Años de antigüedad de la propiedad. A estrenar significa que es nueva
        Descripción: Descripción con características generales, por ejemplo, amenidades, y servicios que ofrece la propiedad 
        Precio: Precio de la propiedad. MN son pesos mexicanos y USD son dólares americanos
        

    """
    with sync_playwright() as p:
        #Iniciamos instancia de navegación
        browser = p.chromium.launch(headless = False)
        page = browser.new_page()
        page.goto(link+'.html')
        soup = bs(page.content(), 'html.parser')

        #Calculamos el número de páginas que hay para esta búsqueda
        numero = soup.find('h1').text
        numero = int(numero[0:numero.find(' ')].replace(',',''))
        if numero < 20:
            muestra = numero
            numero_pags = 1
        else:
            numero_pags = math.ceil(numero/muestra)

        #Variables a almacenar en la base de datos
        dataid = list(np.zeros(numero, dtype = 'str')) #id del post, lo sacamos con data id del html
        metrosct = list(np.zeros(numero, dtype = 'str')) #metros cuadrados totales
        metrosco = list(np.zeros(numero, dtype = 'str')) #metros cuadrados construidos
        recamaras = list(np.zeros(numero, dtype = 'str')) #recamaras
        wc = list(np.zeros(numero, dtype = 'str')) #baños
        estacionamiento = list(np.zeros(numero, dtype = 'str')) # número de lugares de estacionamiento
        antiguo = list(np.zeros(numero, dtype = 'str')) # años de antigüedad
        propiedades = list(np.zeros(numero, dtype = 'str')) #Link con las propiedades
        publicacion = list(np.zeros(numero, dtype = 'str')) # dias desde que fue publicado
        descripcion = list(np.zeros(numero, dtype = 'str')) # descripcion de la propiedad
        precios = list(np.zeros(numero, dtype = 'str')) # precio de la propiedad
        direccion = list(np.zeros(numero, dtype = 'str')) # precio de la propiedad
        fecha_consulta = list(np.zeros(numero, dtype = 'str')) # precio de la propiedad
        
        #Se extraen links y ID de la propiedad
        
        dataid[0:muestra] = [i['data-id'] for i in soup.find_all("div") if i.has_attr('data-id')]
        propiedades[0:muestra] = ['https://www.inmuebles24.com'+i["data-to-posting"] for i in soup.find_all("div") if i.has_attr("data-to-posting")]
        page.wait_for_timeout(delay)
        page.close()

        #Escrapeamos cada propiedad
        for n,i in enumerate(propiedades[0:muestra]):
            page = browser.new_page()
            page.goto(i)
            soup = bs(page.content(), 'html.parser')
            metrosco[n], metrosct[n], estacionamiento[n], recamaras[n], wc[n], antiguo[n], precios[n], direccion[n], publicacion[n], fecha_consulta[n] = extraerAtributos(soup)
            descripcion[n] = "".join([j.text for j in soup.find_all('div', {'data-element': 'accordionContent'})])
            page.wait_for_timeout(delay)
            page.close()

        #Revisamos que exista más de una página
        if numero_pags > 1:
            #Se extraen links y Id de la propiedad
            for i in range(2,numero_pags-1):
                page = browser.new_page()
                page.goto(link+ f'pagina-{i}.html')
                soup = bs(page.content(), 'html.parser')
                idx = 20*(i-1)
                dataid[idx:idx+muestra] = [j['data-id'] for j in soup.find_all("div") if j.has_attr('data-id')]
                propiedades[idx:idx+muestra] = ['https://www.inmuebles24.com'+i["data-to-posting"] for i in soup.find_all("div") if i.has_attr( "data-to-posting")]
                page.wait_for_timeout(delay)
                page.close()
                
                #Se extraen atributos
                for n, k in zip(range(idx,idx+muestra), propiedades[idx:idx+muestra]):
                    page = browser.new_page()
                    page.goto(k)
                    soup = bs(page.content(), 'html.parser')
                    metrosco[n], metrosct[n], estacionamiento[n], recamaras[n], wc[n], antiguo[n], precios[n], direccion[n], publicacion[n], fecha_consulta[n] = extraerAtributos(soup)
                    descripcion[n] = "".join([j.text for j in soup.find_all('div', {'data-element': 'accordionContent'})])
                    page.wait_for_timeout(delay)
                    page.close()
        
        #Extrae atributos de la última página
        page = browser.new_page()
        page.goto(link+ f'pagina-{numero_pags}.html')
        soup = bs(page.content(), 'html.parser')
        start = 20*(numero-1)
        dataid[start:numero] = [i['data-id'] for i in soup.find_all("div") if i.has_attr('data-id')]
        propiedades[start:numero] = ['https://www.inmuebles24.com'+i["data-to-posting"] for i in soup.find_all("div") if i.has_attr("data-to-posting")]
        page.wait_for_timeout(delay)
        page.close()

        for n,i in zip(range(start,numero), propiedades[start:numero]):
            page = browser.new_page()
            page.goto(i)
            soup = bs(page.content(), 'html.parser')
            metrosco[n], metrosct[n], estacionamiento[n], recamaras[n], wc[n], antiguo[n], precios[n], direccion[n], publicacion[n], fecha_consulta[n] = extraerAtributos(soup)
            descripcion[n] = "".join([j.text for j in soup.find_all('div', {'data-element': 'accordionContent'})])
            page.wait_for_timeout(delay)
            page.close()
        browser.close()
        #Crea base de datos
        df = pd.DataFrame({'Post ID': dataid, 'Fecha de consulta': fecha_consulta, 'Link al post':propiedades, 'Dirección':direccion, 
                           'Fecha de publicación': publicacion, 'Metros cuadrados totales': metrosct,'Metros cuadrados construidos': metrosco, 
                           'Número de recámaras': recamaras, 'Número de baños': wc, 'Número de lugares de estacionamiento':estacionamiento, 
                           'Fecha de antigüedad': antiguo, 'Descripción': descripcion,'Precio': precios})
        return df

if __name__ == '__main__':
    #Transforma los municipios al formato para hacer la búsqueda en inmuebles24
    df = pd.read_csv('Mun_Alcal2018.csv')
    df = df.filter(['NOM_ENT', 'NOM_MUN'])
    ciudades = df['NOM_ENT'].value_counts().index.tolist()[0:3]
    df = df[df['NOM_ENT'].isin(ciudades)]
    df2 = df.copy()
    df2['NOM_MUN'] = df['NOM_MUN'].apply(normalize)
    for mun in df2['NOM_MUN'].replace(' ', '-'):
        link = f'https://www.inmuebles24.com/departamentos-en-renta-en-{mun}.html'
        try:
            df = crearBaseDatos(link)
            df.to_parquet(f'{datetime.now().year}-{datetime.now().month}-{datetime.now().day} Departamentos en renta en {mun}.parquet')
        except Exception as e:
            continue