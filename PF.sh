if [ $1 == "clean" ];
then 
make clean
rm -f make.log
rm -f testfiles/*.out
rm -f testfiles/*.output
rm -f testfiles/*.cpp
else 

if [ ! -f "./pf" ]; then
make -f Makefile &> make.log
fi

PF="./pf"
basename=`echo $1 | sed 's/.*\///
s/.jo//'`  
basedir="`echo $1 | sed 's/\/[^\/]*$//'`/"
reffile=`echo $1 | sed 's/.pf$//'`
echo $basedir

if [ -s "$1" ]; then
cat $1 | $PF > "${reffile}.cpp" && echo "Ocaml to C++ of $1 succeeded!"
else
echo "No such file or directory : $1"
exit 1;
fi

if [ ! -s "${reffile}.cpp" ]; then
echo "Ocaml to C++ of $1 failed!"
rm ${reffile}.cpp
exit 1;
else
g++ -I /usr/local/include -L /usr/local/lib -o "${reffile}.out" ${reffile}.cpp ${basedir}address.pb.cc -lprotobuf -pthread && echo "C++ to binary of ${reffile}.cpp successfully!"
fi

if [ -s "${reffile}.out" ]; then
./${reffile}.out > ${reffile}.output && echo "Ran $1 successfully" 
cat ${reffile}.output
else
echo "C++ to binary of ${reffile}.cpp failed"
fi
fi

