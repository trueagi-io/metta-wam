
file=$1

sed -E "s/'mc_n_0__([^(]*)\(/mc_n0('\\1,/" -i $file
sed -E "s/'mc_n_1__([^(]*)\(/mc_n1('\\1,/" -i $file
sed -E "s/'mc_n_2__([^(]*)\(/mc_n2('\\1,/" -i $file
sed -E "s/'m([a-z])__1_[0-9]_([^(]*)\(/m\\1('\\2,/" -i $file



sed -E "s/mc_n_0__([^(]*)\(/mc_n0(\\1,/" -i $file
sed -E "s/mc_n_1__([^(]*)\(/mc_n1(\\1,/" -i $file
sed -E "s/mc_n_2__([^(]*)\(/mc_n2(\\1,/" -i $file
sed -E "s/m([a-z])__1_[0-9]_([^(]*)\(/m\\1('\\2',/" -i $file

