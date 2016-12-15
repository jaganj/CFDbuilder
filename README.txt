Computational Fluid Dynamics is an important area in scientific computing.
The weak scaling of codes is well understood with about two decades of
experiences using MPI. As a result, per-node performance has become very
crucial to the overall machine performance. However, despite the use of
multi-threading, obtaining good performance at each core is still extremely
challenging. The challenges are primarily due to memory bandwidth limitations
and difficulties in using short SIMD engines effectively. This work is about
the techniques and a tool to improve in-core performance. Fundamental to the
strategy is a hierarchical data layout made of small cubical structures of
the problem states that can fit well in the cache hierarchy. The difficulties
in computing the spatial derivatives (also called nearneighbor computation
in the literature) in a hierarchical data layout are well known, hence, such
a data layout has rarely been used in finite difference codes. We can program
the hierarchical data layout in a relatively simple way at the cost of
overheads.

The key technique to eliminate the overheads is called pipeline-for-reuse.
It is followed by a storage optimization called maximal array contraction.
Both pipeline-for-reuse and maximal array contraction are highly tedious and
error-prone. Therefore, we built a source-to-source translator called 
CFD Builder to automate the transformations using directives. The directive
based approach leverages domain experts’ knowledge about the code, and
eliminates the need for complex analysis before program transformations.
We demonstrated the effectiveness of this approach using three different
applications on two different architectures and two different compilers.
We see up to 6.92xperformance improvement using such an approach [1].
We believe such an approach could enable library and application writers
to build efficient CFD libraries.

Please refer to INSTALL.txt for instructions to build and run CFDbuilder.
This document contains the input *restrictions* for CFDbuilder and the
supported directives. Please refer to [1], [2], [3], [4], and [5] for
more on CFDbuilder, pipelining, and each of the directives below.

==------==
Directives
==------==
 cPPM$ PARSE [BEGIN | END]
    PARSE BEGIN and END statements mark the code which needs to be 
    parsed and transformed. The code outside this region is left
    untouched except for macro preprocessing.

 cPPM$ PIPELINE
   Identifies the loop to update a pencil of briquettes; the code region
   to be pipelined.    

 cPPM$ DOUBLEBUFFER <name>
    Identify variables to be double-buffered and prefetched. We need
    double buffering to support prefetching data from the global arrays
    in the main memory to the stack variables in the on-chip memory. 
    The double-buffering provides a place-holder to insert architecture
    specific prefetching instructions, either hints or commands.

 cPPM$ PREFETCH [BEGIN | END]
    PREFETCH BEGIN and END statements mark the prefetch region. 
    Depending on the implementation phase chosen, the double-buffered 
    variables are either prefetched here or the prefetch addresses are
    computed here, but the actual prefetch copy instructions are
    sprinkled throughout the code.

 cPPM$ COPYBUFFER <integer constant> TIMES
    This directive specifies the data transfer size for the sprinked
    prefetch copy instructions for a loop. The copy instructions are
    specified as loops in the input code. Each loop is then split in 
    chunks of the number of iterations specified in this directive and 
    distributed throughout the compute region. 

 cPPM$ DEDOUBLEBUFFER <name>
    Remove double-buffering of big temporaries which are no longer
    needed after pipelining. The double-buffering is put in by the user
    explicitly to cache large blocks of problem state between passes.
    Since pipelining is efficient even at a briquette level, we do not
    need to cache these big temporary arrays, and can be de-doublebuffered.

 cPPM$ ELIMINATE REDUNDANT ITERATIONS
    This directive identifies the loops which construct the mini-domains
    in the input With Briquettes (WB), also known as Fortran-W, input 
    code expression. The mini-domains are constructed with multiple 
    adjacent briquettes. Pipelining does not need the mini-domains to
    be explicitly constructed. After pipelining, a briquette needs to be
    read only once. The redudant reads of briquettes between adjacent
    mini-domains are eliminated.

 cPPM$ ELIMINATE REDUNDANT SUBSCRIPTEXP <name>
    This directive eliminates the specified expression from all the array
    subscripts. Such subscript expressions are only necessary in WB,
    not after pipelining.
    
 cPPM$ LONGITUDINAL LOOP
    Identifies the computational loops which need to be merged and 
    pipelined.

 cPPM$ REPACK LOOP
    Loops which pack the results of computations back into briquettes.

 cPPM$ INLINE
   Subroutine call sites to be inlined



==-------------------==
CFDbuilder restrictions:
==-------------------==
CFDbuilder relies on a restricted form of input Fortran code with 
user-specified directives.  The restrictions enable us to simplify the 
tool building by eliminating code analysis that would otherwise be necessary.
These restrictions do not impair programmability; they merely define a 
limited working subset of the very rich Fortran language.  

It does not support the following Fortran 77 constructs:
1.	assumed size array allocators
2.	entry statement
3.	complex data type
4.	any variable declaration of a specific size should on a separate line. 
5.  floating-point declarations:
      Valid:   
        real*4 a
        real*8 b
      Invalid:
        real a*4, b*8
6.	data statement for now
7.	computed goto statement
8.	Tokens cannot have white-space characters in them. Fortran standard 
    allows it, but it is disallowed here.
9.	Empty sub-script lists are not supported. i.e. a() is not allowed.
10. Not sure if Fortran allows [+-]+ in front of variables or constants.
    They are disallowed here: eg: + -+-10.0; 
                              redundant + signs such as + +10.0, - +10.0
    Allowed: legitimate adds such as 5.0 + -10.0
11.	String concatenation operator ‘//’ is disallowed.
12.	Label references are disallowed in a call argument.
13.	String constants which occur outside of the format statement must 
    not contain quotes (either single or double).
14.	Arithmetic IF statements cannot be recognized.
15.	Character sub-string expressions not supported.
16.	Intrinsic statements are not allowed.
17.	The range for the list DMA outer loop must be specified by integer 
    constants. Even parameters are not allowed.
18.	No executable statements are allowed between the two ‘do’ statements
    in a list DMA.
19.	Intrinsic functions allowed so far include log, exp, abs, sqrt,
    achar, max, and min. 
20.	Unary plus is not supported
21.	Common block statements must appear after all its members have been
    defined.
22.	Type statements only support constant integer lengths; no 
    embellishments and expressions. 
      Eg: ‘real*8’ is supported, but not ‘real*(8)’ or ‘real*(3+4)’.
23.	Unnamed common blocks are not supported.
24.	The sizes of the types must match between the mapping arguments in 
    inlining.
      Eg: Cannot re-dimension a floating point array as a character array 
    in the callee.
25.	iplaneFE is a reserved word for the plane by plane pipeline 
    implementation.  It should not be used as a variable name.


==-----------------------==
Transformation restrictions
==-----------------------==
1.	The loop bounds must be constants in the transform region. They can
    be parameters, but not variables.
2.	Since fully parsing the Fortran i/o statements is very complicated, 
    they are left untouched. They are parsed here with a rule such as
    ‘write^ (~EOS)+ seos!’. 
3.	Inlined subroutines must not contain COMMON statements.
4.	Negative offsets are disallowed in equivalences. ie. a smaller object 
    cannot start from a negative offset of a larger object. 
5.	Not supporting inlining within functions yet.
6.	The parser now supports the volatile keyword.  However, the volatile 
    statements are not inlined if the callee variable references are 
    replaced by the caller arguments.  The solution is to declare the 
    variable as volatile in the caller.
7.	CFDbuilder can support double precision if keywords 'double precision' 
    is used, or if the size of the floating point is explicitly defined 
    using the length specification.  
    eg:  real*8 array1(10)
          double precision array2(5)       
    It cannot handle the case where the precision is set outside of the 
    program in the compile line.  It is possible to extend the translator
    to accept the precision option as an argument.
**  Caution ** : CFD Builder hasn't been tested extensively with 
    double precision. -  04/17/2014


==------==
References
==------==
[1] J. Jayaraj, P.-H. Lin, P. R. Woodward,  and P.-C. Yew, "CFD Builder:
A Library Builder for Computational Fluid Dynamics," 2014 IEEE 
International Parallel & Distributed Processing Symposium Workshops, 
Phoenix, AZ, 2014, pp. 1029-1038.

[2] J. Jayaraj, “A strategy for high performance in computational fluid
dynamics,” Ph.D. dissertation, University of Minnesota, August 2013.

[3] P.-H. Lin, “Performance portability strategies for computational fluid
dynamics (CFD) applications on HPC systems,” Ph.D. dissertation,
University of Minnesota, June 2013.

[4] P. R. Woodward, J. Jayaraj, P.-H. Lin, and P.-C. Yew, “Moving scientific
codes to multicore microprocessor cpus,” Computing in Science and
Engineering, vol. 10, no. 6, pp. 16–25, 2008.

[5] P. R. Woodward, J. Jayaraj, P.-H. Lin, and W. Dai, “First experience
of compressible gas dynamics simulation on the los alamos roadrunner
machine,” Concurr. Comput. : Pract. Exper., vol. 21, no. 17, pp. 2160–
2175, Dec. 2009.