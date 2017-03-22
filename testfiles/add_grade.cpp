
#include <iostream>
#include <fstream>
#include <string>
#include "information.pb.h"
using namespace std;

int main() {
Grade grade_amy ;
grade_amy.add_homework(97) ;
grade_amy.add_homework(84) ;
grade_amy.add_homework(93) ;
grade_amy.add_homework(89) ;
grade_amy.mutable_exam()->set_score(100) ;
grade_amy.mutable_exam()->set_weight(3) ;
int total ;
int i ;
total = 0 ;
i = 0 ;
while (i < 4) {
total = (total + grade_amy.homework(i)) ;
i = (i + 1) ;
} ;
total = (total + (grade_amy.mutable_exam()->score() * grade_amy.mutable_exam()->weight())) ;
total = (total / 7) ;
grade_amy.set_total(total) ;
string str ;
str = grade_amy.DebugString() ;
cout << str << endl ;
fstream output("testfiles/grade_amy",ios::out | ios::binary);
grade_amy.SerializeToOstream(&output);
return 0;
}