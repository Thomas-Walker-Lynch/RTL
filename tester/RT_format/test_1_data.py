# commas and spacing in defs / calls
def f ( x , y , z ):
    return dict( a =1 , b= 2 ), [ 1, 2 ,3 ], ( (1,2) )

# outermost-with-nesting -> pad inside outer ()
val = outer( inner( a,b ) )

# strings/comments untouched
s = "text, with , commas ( not to touch )"  # a ,b ,c

# unbalanced: open-left (closing without opener) -> no padding unless inner bracket before it
def g():
    return result)  # likely unchanged

# unbalanced: open-right (first unmatched opener) with inner bracket following
k = compute(x, f(y
