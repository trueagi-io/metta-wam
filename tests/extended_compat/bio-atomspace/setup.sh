cls
find -name "*qlf*" -print -delete
find -name "*datalog*" -print -delete
find . -name "*.gz" -exec sh -c 'if [ ! -f "${1%.gz}" ]; then gunzip --keep "$1"; fi' sh {} \;
echo " mettalog --debug --log --html pln-xp.metta --repl"
mettalog --debug --log --html pln-xp.metta --repl

