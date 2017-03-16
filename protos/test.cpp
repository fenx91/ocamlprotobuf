#include <iostream>
#include <fstream>
#include <string>
using namespace std;

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
<<<<<<< HEAD
person.mutable_home()->set_address(10086) ;
person.add_phones() ;
person.add_phones() ;
person.mutable_phones(0)->set_number(3538) ;
person.mutable_phones(1)->set_number(2100) ;
string str ;
str = person.DebugString(); } ;
cout << str << endl;
=======
age = person.age() ;
name = person.names(0) ;
person.mutable_home()->set_address(4) ;
address = person.mutable_home()->address();

cout << age << ' ' << name << ' ' << address << endl;
>>>>>>> 3480840bbf788dbdc00bfc87591b190245498d20
return 0;
}
