# wscript — thesis pipeline configuration
# Loaded automatically by waf.py.
# Edit task sources/targets here when R scripts change their inputs or outputs.

SRC  = "src/scripts"
DATA = "bld/data"
BFG  = "beamer/figures"   # beamer figure outputs
PFG  = "paper/figures"    # paper figure outputs
BMR  = "beamer"
PPR  = "paper"


def configure(ctx):
    ctx.find_program("Rscript")
    ctx.find_program("xelatex")   # beamer requires xelatex (documentclass[xetex])
    ctx.find_program("lualatex")  # paper uses lualatex
    ctx.find_program("biber")     # paper bibliography (biblatex-apa)


def build(ctx):

    # ── 1a. Climate: GDD ──────────────────────────────────────────────────────
    # Slow (~hours). Runs only when output file is absent; never rebuilt by mtime.
    ctx.add_task(
        name   = "1a · Climate: GDD  (climatev4.R)",
        rule   = f"Rscript {SRC}/climatev4.R",
        source = [f"{SRC}/climatev4.R"],
        target = [f"{DATA}/df_gdd_complete_1961_2020_weighted.rds"],
        only_if_missing = True,
    )

    # ── 1b. Climate: Precipitation ────────────────────────────────────────────
    ctx.add_task(
        name   = "1b · Climate: Precipitation  (climate_pr.R)",
        rule   = f"Rscript {SRC}/climate_pr.R",
        source = [f"{SRC}/climate_pr.R"],
        target = [f"{DATA}/df_pr_complete_1961_2019.rds"],
        only_if_missing = True,
    )

    # ── 2. Data processing ────────────────────────────────────────────────────
    ctx.add_task(
        name   = "2 · Data processing  (data.R)",
        rule   = f"Rscript {SRC}/data.R",
        source = [
            f"{SRC}/data.R",
            f"{DATA}/df_gdd_complete_1961_2020_weighted.rds",
            f"{DATA}/df_pr_complete_1961_2019.rds",
        ],
        target = [f"{DATA}/data.rds"],
    )

    # ── 3. Estimation ─────────────────────────────────────────────────────────
    ctx.add_task(
        name   = "3 · Estimation  (estimation_y.R)",
        rule   = f"Rscript {SRC}/estimation_y.R",
        source = [f"{SRC}/estimation_y.R", f"{DATA}/data.rds"],
        target = [
            f"{DATA}/results_y.rds",
            f"{DATA}/results_yc.rds",
            f"{DATA}/summary.rds",
            f"{DATA}/summary_c.rds",
            f"{DATA}/data_cf.rds",
        ],
    )

    # ── 4. Stats: beamer figures & tables ─────────────────────────────────────
    ctx.add_task(
        name   = "4 · Stats: beamer figures & tables  (stats_beamer.R)",
        rule   = f"Rscript {SRC}/stats_beamer.R",
        source = [
            f"{SRC}/stats_beamer.R",
            f"{DATA}/data.rds",
            f"{DATA}/results_y.rds",
            f"{DATA}/results_yc.rds",
            f"{DATA}/summary.rds",
            f"{DATA}/summary_c.rds",
            f"{DATA}/data_cf.rds",
        ],
        target = [f"{BFG}/vietnam_qe.png"],
    )

    # ── 5a. Compile beamer ────────────────────────────────────────────────────
    # Two xelatex passes so cross-references resolve correctly.
    # (sty/header.tex uses \documentclass[xetex,...]{beamer} — xelatex required.)
    ctx.add_task(
        name   = "5a · LaTeX: compile beamer  (xelatex ×2)",
        rule   = (
            f"cd {BMR} && "
            f"xelatex -shell-escape -interaction=nonstopmode slides_meat_30min_vpost.tex && "
            f"xelatex -shell-escape -interaction=nonstopmode slides_meat_30min_vpost.tex"
        ),
        source = [
            f"{BMR}/slides_meat_30min_vpost.tex",
            f"{BFG}/vietnam_qe.png",
        ],
        target = [f"{BMR}/slides_meat_30min_vpost.pdf"],
    )

    # ── 5b. Compile paper ─────────────────────────────────────────────────────
    # Full biblatex sequence: lualatex → biber → lualatex → lualatex
    ctx.add_task(
        name   = "5b · LaTeX: compile paper  (lualatex + biber + lualatex ×2)",
        rule   = (
            f"cd {PPR} && "
            f"lualatex -shell-escape -interaction=nonstopmode main.tex && "
            f"biber main && "
            f"lualatex -shell-escape -interaction=nonstopmode main.tex && "
            f"lualatex -shell-escape -interaction=nonstopmode main.tex"
        ),
        source = [f"{PPR}/main.tex", f"{PPR}/bibliografia.bib"],
        target = [f"{PPR}/main.pdf"],
    )
