// commas and simple tight brackets
int g(){
  int a=0 ,
    b=1 ,
    c=2; 
  return h(a ,b ,c);
}

// balanced outermost-with-nesting -> pad inside outer ()
int f(){ return outer(inner(a ,b)); }

// strings and comments must be unchanged
int s(){ printf("x ,y ,z (still a string)"); /* a ,b ,c */ return 1; }

// unbalanced open-right with nesting -> pad after first unmatched '('
int u(){if(doit(foo(1 ,2)  // missing )) 
  return 0;}

// arrays / subscripts stay tight; commas still RT-style
int a(int i ,int j){ return M[i ,j] + V[i] + W[j]; }
