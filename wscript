# wscript – Build configuration for sc_tesis
#
# Defines the full pipeline from raw climate/trade data to compiled beamer PDF.
# Run with:
#   python waf.py configure   # once, to verify tools
#   python waf.py build       # builds only what changed
#   python waf.py build --force  # rebuilds everything
#   python waf.py clean       # deletes all generated files

# ─── Directory shortcuts ──────────────────────────────────────────────────────

SRC  = "src/scripts"          # R scripts
DATA = "bld/data"             # intermediate RDS files
BMR  = "beamer"               # beamer source + assets


# ─── Configure: check required tools ─────────────────────────────────────────

def configure(ctx):
    ctx.find_program("Rscript",  mandatory=True)
    ctx.find_program("lualatex", mandatory=False)   # preferred LaTeX engine
    ctx.find_program("pdflatex", mandatory=False)   # fallback


# ─── Build: full pipeline ─────────────────────────────────────────────────────

def build(ctx):
    """
    Pipeline (each step depends on the previous ones):

      Step 1 – Climate pre-processing   (slow; skip if RDS files already exist)
        1a  climatev4.R      → bld/data/df_gdd_complete_1961_2020_weighted.rds
        1b  climate_pr.R     → bld/data/df_pr_complete_1961_2019.rds
        1c  climate_by_type.R→ bld/data/df_gdd_complete_1961_2020_by_species.rds

      Step 2 – Data assembly
        2a  data.R           → bld/data/data.rds        (uses 1a, 1b)
        2b  data_by_type.R   → bld/data/data_by_type.rds(uses 1c)

      Step 3 – Annual estimation
            estimation_y.R   → bld/data/results_y.rds … (uses 2b)

      Step 4 – Beamer figures & tables
            stats_beamer.R   → beamer/figures/*.png
                               beamer/tables/*.tex       (uses 2a, step 3)

      Step 5 – Compile presentation
            lualatex ×2      → beamer/slides_meat_30min_vpost.pdf (uses step 4)
    """

    # ── 1a: Weighted GDD by coffee type ──────────────────────────────────────
    # Input:  src/original_data/tasmax/, tasmin/ (NetCDF), SPAM .tif masks
    # Output: bld/data/df_gdd_complete_1961_2020_weighted.rds
    # NOTE: only_if_missing=True because the raw NetCDF files are large and
    #       not stored in the repo. Re-run manually if you update the data.
    ctx.add_task(
        name            = "1a · Climate: weighted GDD  (climatev4.R)",
        rule            = f"Rscript {SRC}/climatev4.R",
        source          = [f"{SRC}/climatev4.R"],
        target          = [f"{DATA}/df_gdd_complete_1961_2020_weighted.rds"],
        only_if_missing = True,
    )

    # ── 1b: Precipitation ────────────────────────────────────────────────────
    # Input:  src/original_data/pr/ (NetCDF), SPAM masks
    # Output: bld/data/df_pr_complete_1961_2019.rds
    ctx.add_task(
        name            = "1b · Climate: precipitation  (climate_pr.R)",
        rule            = f"Rscript {SRC}/climate_pr.R",
        source          = [f"{SRC}/climate_pr.R"],
        target          = [f"{DATA}/df_pr_complete_1961_2019.rds"],
        only_if_missing = True,
    )

    # ── 1c: GDD split by Arabica / Robusta ───────────────────────────────────
    # Input:  src/original_data/tasmax/, tasmin/, SPAM per-species hectares
    # Output: bld/data/df_gdd_complete_1961_2020_by_species.rds
    ctx.add_task(
        name            = "1c · Climate: GDD by coffee species  (climate_by_type.R)",
        rule            = f"Rscript {SRC}/climate_by_type.R",
        source          = [f"{SRC}/climate_by_type.R"],
        target          = [f"{DATA}/df_gdd_complete_1961_2020_by_species.rds"],
        only_if_missing = True,
    )

    # ── 2a: Main monthly panel (country × year × month) ──────────────────────
    # Input:  src/original_data/ (PSD, ICIP, GDP, ONI …) + climate from 1a, 1b
    # Output: bld/data/data.rds
    ctx.add_task(
        name   = "2a · Data: main panel  (data.R)",
        rule   = f"Rscript {SRC}/data.R",
        source = [
            f"{SRC}/data.R",
            f"{DATA}/df_gdd_complete_1961_2020_weighted.rds",
            f"{DATA}/df_pr_complete_1961_2019.rds",
        ],
        target = [f"{DATA}/data.rds"],
    )

    # ── 2b: Panel split by coffee type ───────────────────────────────────────
    # Input:  src/original_data/ + climate from 1c
    # Output: bld/data/data_by_type.rds
    ctx.add_task(
        name   = "2b · Data: by coffee type  (data_by_type.R)",
        rule   = f"Rscript {SRC}/data_by_type.R",
        source = [
            f"{SRC}/data_by_type.R",
            f"{DATA}/df_gdd_complete_1961_2020_by_species.rds",
        ],
        target = [f"{DATA}/data_by_type.rds"],
    )

    # ── 3: Annual estimation + counterfactuals ────────────────────────────────
    # Input:  bld/data/data_by_type.rds  (Arabica subsample)
    # Output: bld/data/results_y.rds, results_yc.rds, summary.rds, summary_c.rds
    #         paper/tables/*.tex, paper/figures/*.png  (written by the script)
    ctx.add_task(
        name   = "3  · Estimation: annual model  (estimation_y.R)",
        rule   = f"Rscript {SRC}/estimation_y.R",
        source = [
            f"{SRC}/estimation_y.R",
            f"{DATA}/data_by_type.rds",
        ],
        target = [
            f"{DATA}/results_y.rds",
            f"{DATA}/results_yc.rds",
            f"{DATA}/summary.rds",
            f"{DATA}/summary_c.rds",
        ],
    )

    # ── 3b: Differentiated estimation (Arabica + Robusta as substitutes) ────────
    # Input:  bld/data/data_by_type.rds
    # Output: results_y_arabica.rds, results_y_robusta.rds, demand_diff.rds, …
    ctx.add_task(
        name   = "3b · Estimation: differentiated model  (estimation_diff.R)",
        rule   = f"Rscript {SRC}/estimation_diff.R",
        source = [
            f"{SRC}/estimation_diff.R",
            f"{DATA}/data_by_type.rds",
        ],
        target = [
            f"{DATA}/results_y_arabica.rds",
            f"{DATA}/results_y_robusta.rds",
        ],
    )

    # ── 4: Beamer figures & tables ────────────────────────────────────────────
    # Input:  bld/data/data.rds + results from step 3
    # Output: beamer/figures/*.png,  beamer/tables/*.tex
    # Tracking: uses two representative outputs as proxies
    ctx.add_task(
        name   = "4  · Stats: beamer figures & tables  (stats_beamer.R)",
        rule   = f"Rscript {SRC}/stats_beamer.R",
        source = [
            f"{SRC}/stats_beamer.R",
            f"{DATA}/data.rds",
            f"{DATA}/results_y.rds",
            f"{DATA}/results_yc.rds",
            f"{DATA}/summary.rds",
        ],
        target = [
            f"{BMR}/figures/costs_strat.png",   # proxy – last ggsave in script
            f"{BMR}/tables/results_1.tex",       # proxy – representative table
        ],
    )

    # ── 5: Compile beamer presentation (two LaTeX passes) ────────────────────
    # Input:  beamer/slides_meat_30min_vpost.tex + figures + tables from step 4
    # Output: beamer/slides_meat_30min_vpost.pdf
    #
    # We cd into beamer/ so that \input{figures/…} and \input{tables/…}
    # resolve relative to the .tex file's own directory.
    ctx.add_task(
        name   = "5  · LaTeX: compile beamer slides  (lualatex ×2)",
        rule   = (
            "cd beamer && "
            "lualatex -shell-escape -interaction=nonstopmode "
            "  slides_meat_30min_vpost.tex && "
            "lualatex -shell-escape -interaction=nonstopmode "
            "  slides_meat_30min_vpost.tex"
        ),
        source = [
            f"{BMR}/slides_meat_30min_vpost.tex",
            f"{BMR}/figures/costs_strat.png",
            f"{BMR}/tables/results_1.tex",
        ],
        target = [f"{BMR}/slides_meat_30min_vpost.pdf"],
    )
