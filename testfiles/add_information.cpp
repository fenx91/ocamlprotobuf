
#include <iostream>
#include <fstream>
#include <string>
#include "information.pb.h"
#include "information.pb.h"

using namespace std;

int main() {
Students_information stu_info ;
Grade grade_amy ;
fstream input1("testfiles/stu_info",ios::in | ios::binary);
stu_info.ParseFromIstream(&input1) ;
fstream input2("testfiles/grade_amy",ios::in | ios::binary);
grade_amy.ParseFromIstream(&input2) ;
if ((stu_info.mutable_person(0)->name() == "Amy")) { if ((stu_info.mutable_person(0)->has_registed() && stu_info.mutable_person(0)->registed())) { stu_info.mutable_person(0)->add_grades() ;
int temp ;
int index ;
index = (stu_info.mutable_person(0)->grades_size() - 1) ;
temp = grade_amy.total() ;
stu_info.mutable_person(0)->mutable_grades(index)->set_total(temp) ;
temp = grade_amy.mutable_exam()->weight() ;
stu_info.mutable_person(0)->mutable_grades(index)->mutable_exam()->set_weight(temp) ;
temp = grade_amy.mutable_exam()->score() ;
stu_info.mutable_person(0)->mutable_grades(index)->mutable_exam()->set_score(temp) ;
int i ;
i = 0 ;
while (i < grade_amy.homework_size()) {
temp = grade_amy.homework(i) ;
stu_info.mutable_person(0)->mutable_grades(index)->add_homework(temp) ;
i = (i + 1) ;
} ; } ; } 
else { cout << "Cannot_add_information" << endl ; } ;
string str ;
str = stu_info.DebugString() ;
cout << str << endl;
return 0;
}