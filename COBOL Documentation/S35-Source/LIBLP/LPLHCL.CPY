      * COPYMEMBER: LIBLP/LPLHCL
      ******************************************************************
      *
      *    C L E A R - L H - F I L E
      *
      * CS 190501 CLEAR FUTURE FIELDS
      * BAH 20210427 REMOVED FUTURE FIELDS
      ******************************************************************
       CLEAR-LH-FILE SECTION.

           MOVE SPACES       TO LH-REC.

           MOVE ZEROES TO LH-CLASS       LH-LAWCD       LH-LOANDATE
                          LH-POFFDATE    LH-LNAMT       LH-ORGTERM
                          LH-POTERMS     LH-PLDATE      LH-JDDATE
                          LH-PYPROFILE   LH-DLNO     
                          LH-LTOUCH-DATE.

           MOVE 999999999999 TO LH-PYPROFILE.

       EXIT-CLEAR-LH-FILE.
           EXIT.
