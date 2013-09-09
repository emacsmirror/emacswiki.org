#!/bin/bash

input="$1"
output="$2"

# $1 is input filename to which the latex content must be printed
function print_latex_content ()
{
   if (head -n 1 "$1" | grep -q '^[\\@]f[\[\$]')
   then
       # \f$ .. \f$ or \f[ .. \f]
       sed -e 's/^[ *\t]*//' -e 's/\\f\$/$/g' -e 's/\\f[][]/$$/g' "$1"
   elif (head -n 1 "$1" | grep -q '^[\\@]f{[^}]\+}')
   then
       # \f{environment} .. \f}
       local environment=$(head -n 1 "$1" | sed -e 's/^[ *\t]*//' -e 's/\\f{\([^}]\+\)}.*/\1/')
       sed '/^[ \t]*$/d' <<- EOF
       \\begin{${environment}}
       $(sed -e 's/^[ *\t]*//' -e 's/\\f{[^}]\+}{\?//' -e 's/\\f}//' "$1")
       \\end{${environment}}
EOF
   else
       echo "ERROR: invalid content for latex."
   fi
}


if (head -n 1 "$input" | grep -q '^[\\@]dot')
then
    # dot command
    sed -e 's/^[ *\t]*//' \
	-e '/^\(\\\|@\)dot/d'    \
	-e '/^\(\\\|@\)enddot/d' "$input" | dot -Tpng -o "$output"

    
elif (head -n 1 "$input" | grep -q '^[\\@]msc')
then
    # mscgen command
    sed -e 's/^[ *\t]*//' \
	-e '/^\(\\\|@\)msc/d'    \
	-e '/^\(\\\|@\)endmsc/d' "$input" | mscgen -Tpng -o "$output"


elif (head -n 1 "$input" | grep -q '^[\\@]f[\[\${]')
then
    
    # \f$ .. \f$ or \f[ .. \f] or \f{environment} .. \f} latex formula
    tex_dir=$(mktemp -d)
    tex_main_file="${tex_dir}/main.tex"
 
cat > "${tex_main_file}" <<- EOF
	\\documentclass{article}
	\\pagestyle{empty}
	\\usepackage{amssymb}
	\\begin{document}
	$(print_latex_content "${input}")
	\\par
	\\end{document}
EOF

    (
      latex -output-directory="${tex_dir}" "${tex_main_file}"   
      dvipng -T tight -x 1728 -o "${output}" "${tex_main_file%%.tex}.dvi"
    ) &> /dev/null
    
    rm -r "${tex_dir}"


elif (head -n 1 "$input" | grep -q '^[\\@]image')
then
    # image command
    cp $(awk '(NR == 1) {print $3}' "$input") "$output" 


fi


## decomment the following line to revert image colors (so to have white
## on black images). This require the installation of imagemagick.

#convert "${output}" -negate "${output}"
