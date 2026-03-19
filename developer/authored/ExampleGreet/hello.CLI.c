#include <stdlib.h>
#include <stdio.h>

#include "Math.lib.c"
#include "Greeter.lib.c"

void CLI(void){ 
  int base_count = ExampleGreet·Math·add(1 ,2);
  printf("Calculated base loop count: %d\n" ,base_count);
  ExampleGreet·Greeter·hello_loop(base_count);
}

int main(int argc ,char **argv){ 
  (void)argc;
  (void)argv;
  
  CLI();
  
  return EXIT_SUCCESS;
}
