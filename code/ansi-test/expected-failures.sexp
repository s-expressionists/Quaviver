#+(or allegro ccl clasp cmucl ecl sbcl) :NIL-VECTORS-ARE-STRINGS
#+(or clasp ecl) :ALLOW-NIL-ARRAYS
#+(or clasp ecl) :MAKE-CONDITION-WITH-COMPOUND-NAME
#+(or clasp ecl) :NO-FLOATING-POINT-UNDERFLOW-BY-DEFAULT

#+(or abcl ccl clasp cmucl ecl sbcl) FORMAT.E.26

#+clasp PRINT.DOUBLE-FLOAT.RANDOM
#+clasp PRINT.LONG-FLOAT.RANDOM
