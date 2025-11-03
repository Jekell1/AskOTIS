      * COPYMEMBER: LIBSP/SPFDSORT
      ******************************************************************
      *
      * IDENTIFICATION:  LIBSP/SPFDSORT
      *
      * DESCRIPTION   :  STANDARD SORT FILE (FD)
      *
      *=================================================================
      * REV:
      * KEC 2003.1023 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *               ***NEW***
      *
      ******************************************************************
       FD  SORT-FILE
           LABEL RECORDS ARE STANDARD.

      *=================================================================
      *01  SORT-REC.
      *
      *    03  SORT-KEY.
      *
      *        05  SORT-BRNO                         PIC 9(05).
      *        05  SORT-CLASS                        PIC 9(02).
      *        05  SORT-DLNO                         PIC 9(05)V99.
      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    03  SORT-REC-INFO                         PIC X(76).

