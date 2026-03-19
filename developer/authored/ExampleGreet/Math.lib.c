#ifndef ExampleGreet·Math·ONCE
#define ExampleGreet·Math·ONCE

int ExampleGreet·Math·add(int a ,int b);

#ifdef ExampleGreet·Math
  int ExampleGreet·Math·add(int a ,int b){
    return a + b;
  }
#endif // ExampleGreet·Math

#endif // ExampleGreet·Math·ONCE
