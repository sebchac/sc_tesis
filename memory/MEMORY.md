# sc_tesis – Project Memory

## Build System
- **WAF runner**: `waf.py` (custom, no external dependencies) + `wscript`
- Run: `python waf.py configure` then `python waf.py build`
- `--force` flag: `python waf.py build --force` rebuilds everything
- Climate tasks use `only_if_missing=True` (raw NetCDF data not in repo)

## Pipeline Order (wscript)
1. `climatev4.R` → `bld/data/df_gdd_complete_1961_2020_weighted.rds`
2. `climate_pr.R` → `bld/data/df_pr_complete_1961_2019.rds`
3. `climate_by_type.R` → `bld/data/df_gdd_complete_1961_2020_by_species.rds`
4. `data.R` → `bld/data/data.rds` (uses 1+2)
5. `data_by_type.R` → `bld/data/data_by_type.rds` (uses 3)
6. `estimation_y.R` → `bld/data/results_y.rds` etc. (uses 5)
7. `stats_beamer.R` → `beamer/figures/`, `beamer/tables/` (uses 4+6)
8. `lualatex` → `beamer/slides_meat_30min_vpost.pdf` (uses 7)

## Key Paths
- R scripts: `src/scripts/`
- Raw data: `src/original_data/` (large files, not in repo)
- Intermediate RDS: `bld/data/`
- Beamer assets: `beamer/figures/`, `beamer/tables/`
- Paper assets: `paper/figures/`, `paper/tables/` (written by estimation_y.R)

## Known Issues
- `estimation_y.R` has a pre-existing bug: "se intenta usar un nombre de variable de longitud cero" (zero-length variable name). Not related to WAF.
- R scripts use hardcoded absolute paths (`/Users/sebastianchacon/Desktop/sc_tesis/`). Not portable but works on this machine.
- `stats.R` is not included in the pipeline (only `stats_beamer.R` is needed).
