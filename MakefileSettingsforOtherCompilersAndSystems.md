Here we add some information as to how to compile on other platforms
with non-gfortran compilers.   Please feel free to add your info, if it differs substantially from our information.  Please try and be concise, and use our naming conventions

_One thing to remember when compiling our fortran source with another compiler:  we often use lines > 80 characters, so any compiler used must have the 132 character line length invoked._


---


**OS:  Mac OS X 10.6.6** Compiler:  Intel fortran 2011 (v12) 32 bit

```
FC   = ifort -m32 -extend_source
CC   = gcc
CFLAGS = -m32 -c -g -I. -DLONG32 -DUNDERSCORE -DLITTLE
```


---


**OS:  Mac OS X 10.6.6** Compiler:  Gfortran / gcc version 4.3.0 20071026

```
FC   = gfortran -m64 -g -O0 -Wl,-stack_size,10000000 -ffixed-line-length-132 -Wunused -Wuninitialized
CC   = gcc
CFLAGS = -m64 -c -g -I. -DLONG32 -DUNDERSCORE -DLITTLE -Wunused -Wuninitialized
```


---
