# Analysis-of-subjects-US
Analysis of subjects in Universidad de Sevilla
Source of data: https://transparencia.us.es/resultados-academicos, https://transparencia.us.es/sites/transparencia/files/doc/info-institucional/Datos-Grado-Master-17-18-a-23-24.xls
## Cleaning
Delete records 'Prácticas', 'movilidad', 'trabajos'. 
Subject coded with Cod_Tit: 171-1710001.
Columns from file: Nombre_Titulo, Cod_Asig, Nom_Asig, Cod_Tit, Num_Mat, Num_Pres, Num_Superan, Curso.
Computed columns: Num_No_Pres, Num_No_Superan.
Delete records Num_Mat == 0.
## Difficulty
Two dimensions:
- Passing.
- Dropping-out.
## Programs
split_and_Clean.py

load_data.R
