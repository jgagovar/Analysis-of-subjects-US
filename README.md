# Analysis-of-subjects-US
Analysis of subjects in Universidad de Sevilla
Source of data: 
- https://transparencia.us.es/resultados-academicos
- https://transparencia.us.es/sites/transparencia/files/doc/info-institucional/Datos-Grado-Master-17-18-a-23-24.xls

The file contains aggregated data of all the subjects in US from academic year 2017-18 to 2023-24. The file is updated every year with consolidated records. 
## Cleaning
- Delete records 'Prácticas', 'movilidad', 'trabajos'. 
- Subject coded with Cod_Tit: 171-1710001.
- Columns from file: Nombre_Titulo, Cod_Asig, Nom_Asig, Cod_Tit, Num_Mat, Num_Pres, Num_Superan, Curso.
- Computed columns: Num_No_Pres, Num_No_Superan.
- Delete records Num_Mat == 0.
## Difficulty
Two dimensions:
- Passing.
- Dropping-out.
## Programs
- split_and_clean.py. Cleaning and format of data.
- load_data.R. Statistical analysis of data.
