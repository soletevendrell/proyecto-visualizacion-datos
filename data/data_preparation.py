
import pandas as pd
import glob
from tqdm import tqdm
from pandasgui import show


archivo = "data\Contratos_crudo\Contratos_2006.csv"
df = pd.read_csv(archivo, sep=";")
# show(df)


df_sincolumnas = df.drop(columns=['mes', 'Código mes'], errors='ignore') 
show(df_sincolumnas)

# df_agrupado = df.groupby('Municipio', as_index=False).mean()

# print(df_agrupado)













































































