# Thesis Pipeline Memory

## Build system
- `waf.py` (custom Python runner, no external deps) + `wscript` (pipeline config) at repo root
- Run: `python waf.py build` (always run from repo root)
- Key flags: `--force` (rebuild all), `--from=PATTERN` (skip earlier tasks)
- `python waf.py configure` checks Rscript + xelatex + lualatex

## Task order (wscript)
| Task | Script | Output | Notes |
|------|--------|--------|-------|
| 1a | climatev4.R | bld/data/df_gdd_complete_1961_2020_weighted.rds | `only_if_missing=True` |
| 1b | climate_pr.R | bld/data/df_pr_complete_1961_2019.rds | `only_if_missing=True` |
| 2  | data.R | bld/data/data.rds | |
| 3  | estimation_y.R | bld/data/results_y.rds, results_yc.rds, summary.rds, summary_c.rds, data_cf.rds | |
| 4  | stats_beamer.R | beamer/figures/vietnam_qe.png | stats.R excluded (has bugs) |
| 5a | xelatex ×2 | beamer/slides_meat_30min_vpost.pdf | beamer uses xelatex (documentclass[xetex]) |
| 5b | lualatex ×2 | paper/main.pdf | |

## Common skip patterns
- `--from=data` → skip climate (steps 1a, 1b)
- `--from=estimation` → skip climate + data
- `--from=stats` → skip everything up to stats_beamer.R
- `--from=latex` → skip to LaTeX compilation only

## Key paths
- R scripts: `src/scripts/`
- Data outputs: `bld/data/`
- Beamer figures: `beamer/figures/`
- Paper figures: `paper/figures/`
- Paper tables: `paper/tables/`

## Bibliography
- Paper uses `biblatex` + `biber` with `style=apa` (APA 7th edition) — `biblatex-apa` installed at `~/Library/texmf/tex/latex/biblatex-apa/`
- Compile sequence: `lualatex → biber main → lualatex → lualatex`
- `\citep` / `\citet` work via `natbib=true` in biblatex options
- Beamer has its own bibliography (not using biblatex; separate from paper)
- Fixed in `bibliografia.bib`: stray `}` after bresnahan1989empirical; month fields Sept→9 and May→5

## Known issues / fixes applied
- `estimation_y.R` line 35: added `fig_path <- ruta_figures` alias (used in ggsave calls)
- `stats.R` excluded from pipeline: has `heatExp`/`type` column bugs; paper figures exist from prior runs
- Beamer must use `xelatex`, NOT lualatex (header.tex: `\documentclass[xetex,...]`)
- Paper uses `lualatex` (umemoria.cls with inputenc)
- Always run `python waf.py` from repo root `/Users/sebastianchacon/Desktop/sc_tesis/`
