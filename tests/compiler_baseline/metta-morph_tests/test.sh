echo "Testing:" $1
sh run.sh $1 2> /dev/null | grep "!=" && echo "FAILED:" $1
