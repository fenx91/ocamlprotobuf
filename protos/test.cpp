#include <iostream>
#include <fstream>
#include <string>
using namespace std;
#include "person.pb.h"
#include "person.pb.h"

int main() {
int age ;
age = 4 ;
int name ;
name = (3 % 4) ;
if (true) { Person person ;
person.set_age(24) ;
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
fstream output("result",ios::out | ios::binary);
person.SerializeToOstream(&output); };
return 0;
}
