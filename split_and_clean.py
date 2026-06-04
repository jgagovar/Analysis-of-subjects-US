import pandas as pd


def process_csv_v2(input_file, output_file):
    # 1. Load the CSV file
    df = pd.read_csv(input_file, sep=";", dtype=str, keep_default_na=False)

    # -------------------------------------------------------------
    # CONDITION 1: Split Cod_Nombre_Asignatura
    # -------------------------------------------------------------
    #df["Cod_Asig"] = df["Cod_Nombre_Asignatura"].str[:7]
    #df["Nom_Asig"] = df["Cod_Nombre_Asignatura"].str[8:]
    df[["Cod_Asig", "Nom_Asig"]] = df["Cod_Nombre_Asignatura"].str.split(
        "-", n=1, expand=True
    )
    # Strip any accidental whitespace just in case (e.g., "1234567 - SubjectName")
    df["Cod_Asig"] = df["Cod_Asig"].str.strip()
    df["Nom_Asig"] = df["Nom_Asig"].str.strip()

    # -------------------------------------------------------------
    # CONDITION 2: Dynamic Cod_Tit calculation per Nombre_Titulo
    # -------------------------------------------------------------
    # We will build a mapping dictionary: {Nombre_Titulo: Calculated_Cod_Tit}
    title_mapping = {}

    # Group by Nombre_Titulo to calculate the mode prefix
    for title, group in df.groupby("Nombre_Titulo"):
        if title.startswith(("Grado", "Doble G")):
            # Extract first 3 characters of Cod_Asig
            prefixes = group["Cod_Asig"].str[:3]
            # .value_counts().idxmax() finds the one with the maximum occurrences
            if not prefixes.empty:
                title_mapping[title] = prefixes.value_counts().idxmax()

        elif title.startswith(("M", "Doble M")):
            # Extract first 4 characters of Cod_Asig
            prefixes = group["Cod_Asig"].str[:4]
            if not prefixes.empty:
                title_mapping[title] = prefixes.value_counts().idxmax()
        else:
            # Fallback for unexpected titles just in case
            title_mapping[title] = ""

    # Map the calculated values back to the dataframe
    df["Cod_Tit"] = df["Nombre_Titulo"].map(title_mapping)

    # -------------------------------------------------------------
    # CONDITION 3 (Part 1): Update Cod_Asig by concatenating Cod_Tit
    # -------------------------------------------------------------
    df["Cod_Asig"] = df["Cod_Tit"] + '-' + df["Cod_Asig"]

    # -------------------------------------------------------------
    # CONDITION 3 (Part 2): Delete original field
    # -------------------------------------------------------------
    df = df.drop(columns=["Cod_Nombre_Asignatura"])

    # -------------------------------------------------------------
    # CONDITION 3 (Part 3): Remove duplicates for Cod_Asig starting with '2' per Curso
    # Note: Checking if Cod_Asig starts with '2' *after* concatenation.
    # If you meant it originally started with '2', check the note below code.
    # -------------------------------------------------------------
    mask_starts_with_2 = df["Cod_Asig"].str.startswith("2")

    df_to_dedup = df[mask_starts_with_2]
    df_rest = df[~mask_starts_with_2]

    # Drop duplicates within the same Curso for Cod_Asig
    df_deduped = df_to_dedup.drop_duplicates(subset=["Curso", "Cod_Asig"])

    # Combine back together
    df_final = pd.concat([df_deduped, df_rest], ignore_index=True)

    # -------------------------------------------------------------
    # Clean up column order (Placing new columns cleanly)
    # -------------------------------------------------------------
    cols = list(df_final.columns)
    # Move new columns into a logical position near the title/subject info
    for col in ["Cod_Tit", "Nom_Asig", "Cod_Asig"]:
        cols.insert(3, cols.pop(cols.index(col)))
    df_final = df_final[cols]

    # Save to file
    df_final.to_csv(output_file, sep=";", index=False)
    print(f"File processed successfully! Saved as: {output_file}")


# --- Execution ---
# --- Execution ---
# Replace these with your actual file paths
input_filename = 'Datos-G-M-2017-2024-v01.csv'
output_filename = 'Datos-G-M.csv'

process_csv_v2(input_filename, output_filename)