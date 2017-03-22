
#include <iostream>
#include <fstream>
#include <string>
#include "information.pb.h"
using namespace std;

int main() {
Students_information stu_info ;
if ((stu_info.person_size() == 0)) { stu_info.add_person() ;
stu_info.add_person() ; } ;
stu_info.mutable_person(0)->set_name("Amy") ;
stu_info.mutable_person(1)->set_name("Shawn") ;
stu_info.mutable_person(0)->set_age(26) ;
stu_info.mutable_person(0)->set_registered(true) ;
stu_info.mutable_person(0)->add_id(2013210746) ;
string str ;
str = stu_info.DebugString() ;
cout << str << endl ;
fstream output("testfiles/stu_info",ios::out | ios::binary);
stu_info.SerializeToOstream(&output);
return 0;
}