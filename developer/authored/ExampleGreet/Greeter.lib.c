#ifndef ExampleGreet·Greeter·ONCE
#define ExampleGreet·Greeter·ONCE

#include "Math.lib.c"

void ExampleGreet·Greeter·hello_loop(int count);

#ifdef ExampleGreet·Greeter
  #include <stdio.h>

  void ExampleGreet·Greeter·hello_loop(int count){ 
    for(int TM = 0; TM < count; ++TM){
      int current_count = ExampleGreet·Math·add(TM ,1);
      printf("Hello iteration: %d\n" ,current_count);
    }
  }

#endif // ExampleGreet·Greeter

#endif // ExampleGreet·Greeter·ONCE
