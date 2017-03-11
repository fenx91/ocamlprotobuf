#include <fstream>
#include <iostream>
#include "person.pb.h"
using namespace std;


int main() {
int age ;
age = 4 ;
int name ;
name = (3 / 4) ;
cout << age << ' ' << name << endl;
int address ;
Person person ;
person.set_age(24) ;
person.add_names(3) ;
age = person.age() ;
name = person.names(0) ;
person.mutable_home()->set_address(4) ;
address = person.mutable_home()->address();

cout << age << ' ' << name << ' ' << address << endl;
return 0;
}




