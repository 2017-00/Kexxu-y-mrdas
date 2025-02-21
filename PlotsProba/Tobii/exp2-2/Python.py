import pandas as pd

df = pd.read_csv('exp2-2_tobii.csv')

promedio_dif_x = df['dif_x'].abs().mean()
promedio_dif_y = df['dif_y'].abs().mean()

gr_x = promedio_dif_x / 26.98
gr_y = promedio_dif_y / 26.98

print(f"Promedio de la columna dif_x: {promedio_dif_x}")
print(f"Promedio de la columna dif_y: {promedio_dif_y}")
print(f"Promedio de la columna dif_x en grados: {gr_x}")
print(f"Promedio de la columna dif_y en grados: {gr_y}")

