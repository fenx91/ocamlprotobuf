if [ $1 == "clean" ];
then 
rm -f testfiles/*.cpp
else 

if [ ! -f "./pf" ]; then
make -f Makefile &> make.log
fi

PF="./pf"
basename=`echo $1 | sed 's/.*\///
s/.jo//'`  
basedir="`echo $1 | sed 's/\/[^\/]*$//'`/"
reffile=`echo $1 | sed 's/.jo$//'`
echo $basedir

if [ -s "$1" ]; then
cat $reffile | $PF > "${reffile}.cpp" && echo "Ocaml to C++ of $1 succeeded!"
else
echo "No such file or directory : $1"
exit 1;
fi

if [ ! -s "${reffile}.cpp" ]; then
echo "Ocaml to C++ of $1 failed!"
rm ${reffile}.cpp
exit 1;
else
g++ -I /usr/local/include -L /usr/local/lib -o "${reffile}.out" ${reffile}.cpp ${basedir}person.pb.cc -lprotobuf -pthread && echo "C++ to binary of ${reffile}.cpp successfully!"
fi

if [ -s "${reffile}.out" ]; then
./${reffile}.out > ${reffile}.output && echo "Ran $1 successfully" 
else
echo "C++ to binary of ${reffile}.cpp failed"
fi
fi

