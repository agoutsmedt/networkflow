# Todolist - Push vers dev

Voila l'eval de GPT qui a acces au package complet.

Le package est globalement bien structure (API claire, workflow coherent, docs presentes), mais il reste plusieurs points qualite importants avant une version vraiment robuste.

## Points principaux releves

- Pas de suite de tests (`tests/` absent): risque de regression eleve.
- CI incoherente: le README affiche un badge `R-CMD-check`, mais le workflow correspondant n'est pas present dans `.github/workflows/` (seulement `pkgdown`).
- Incoherences de deprecation:
  - `filter_components()` est documentee comme "deprecated" mais avec badge `experimental`.
  - `tbl_main_component()` annonce un remplacement par `extract_main_component()` qui n'existe pas.
- `NEWS.md` mentionne `layout_clusters()` alors que la fonction n'existe pas dans `R/`.
- Dette technique legere:
  - duplication de `mixcolor()` dans deux fichiers,
  - `rename_at()` encore utilise (fonction `dplyr` superseded).
