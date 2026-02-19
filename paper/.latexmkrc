# .latexmkrc — paper/
# Tells latexmk to compile with lualatex and run biber for biblatex.

$pdf_mode = 4;          # 4 = lualatex
$lualatex = 'lualatex -shell-escape -interaction=nonstopmode %O %S';
$biber    = 'biber %O %S';

# Output directory (optional — comment out if you prefer files in paper/)
# $out_dir = 'build';
