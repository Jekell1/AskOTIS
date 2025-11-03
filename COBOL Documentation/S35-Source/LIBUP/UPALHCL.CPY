      * COPYMEMBER: LIBLP/LPALHCL
      ******************************************************************
      *
      * CLEAR ASCII LOAN HISTORY
      ******************************************************************
       CLEAR-ALH-FILE SECTION.

           MOVE ALL X"09" TO ALH-REC.

           MOVE 0 TO ALH-CLASS       ALH-LAWCD       ALH-LOANDATE
                     ALH-POFFDATE    ALH-LNAMT       ALH-ORGTERM
                     ALH-POTERMS     ALH-PLDATE      ALH-JDDATE
                     ALH-PYPROFILE   ALH-DLNO     
                     ALH-LTOUCH-DATE.

           MOVE 999999999999 TO ALH-PYPROFILE.

           MOVE SPACES TO ALH-COLLCD ALH-MAKERCD  ALH-POCD
                          ALH-STATUS ALH-ARCHIVE-FG.
           
       EXIT-CLEAR-ALH-FILE.
           EXIT.
