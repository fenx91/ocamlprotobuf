#include <fstream>
#include <iostream>
#include "person.pb.h"
using namespace std;


int main() {
int age ;
age = 4 ;
int name ;
name = (3 % 4) ;
cout << age << ' ' << name << endl;

Person person ;
person.set_age(24) ;
person.add_names(3) ;
age = person.age() ;
name = person.names(0) ;

cout << age << ' ' << name << endl;
return 0;
}




