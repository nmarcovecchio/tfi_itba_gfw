
import pandas as pd
import psycopg2

datasets = ['fixed_gear_fe_recortado','trollers_fe_recortado','trawlers_fe_recortado', 'drifting_longlines_fe_recortado', 'purse_seines_fe_recortado' ]

from sqlalchemy import create_engine
engine = create_engine('postgresql://postgres:postgres@localhost:5432/pesqueros')


for dataset in datasets:
    df = pd.read_csv(f'C:/Users/nico_/Desktop/ITBA/TFI/github/df_procesado/{dataset}.csv')
    df.columns = [c.lower() for c in df.columns] # PostgreSQL doesn't like capitals or spaces
    print(df.columns)
    df.to_sql(f'{dataset}', engine, if_exists='replace')

conn = psycopg2.connect(database="pesqueros", user='postgres', password='postgres', host='127.0.0.1', port= '5432')
cursor = conn.cursor()
for dataset in datasets:
    cursor.execute(f"alter table {dataset} add column geom geometry(Geometry,4326);")
    cursor.execute(f"update {dataset} set geom = ST_SetSRID(ST_MakePoint(lon::double precision, lat::double precision),4326) where lat is not null;")
conn.commit() # <--- makes sure the change is shown in the database
conn.close()
cursor.close()
