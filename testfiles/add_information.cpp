
#include <iostream>
#include <fstream>
#include <string>
#include "address.pb.h"
using namespace std;

int main() {
Students_information stu_info ;
if ((stu_info.person_size() == 0)) { stu_info.add_person() ; } ;
stu_info.mutable_person(0)->set_name("Lily") ;
stu_info.mutable_person(0)->set_age(26) ;
stu_info.mutable_person(0)->set_registed(true) ;
stu_info.mutable_person(0)->add_id(2013210746) ;
string str ;
str = stu_info.DebugString() ;
cout << str << endl;
return 0;
}