
#include <iostream>
#include <fstream>
#include <string>
#include "person.pb.h"
using namespace std;

int main() {
int age ;
age = 4 ;
int name ;
name = (3 % 4) ;
if (true) { Person person ;
fstream input("testfiles/result",ios::in | ios::binary);
person.ParseFromIstream(&input) ;
person.set_age(18) ;
person.add_names(4) ;
person.add_names(3) ;
person.mutable_home()->set_address(10086) ;
person.add_phones() ;
person.add_phones() ;
person.mutable_phones(0)->set_number(3538) ;
person.mutable_phones(1)->set_number(2100) ;
string str ;
str = person.DebugString() ;
cout << str << endl ;
fstream output("testfiles/result2",ios::out | ios::binary);
person.SerializeToOstream(&output) ; };
return 0;
}