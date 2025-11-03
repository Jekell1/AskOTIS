      * COPYMEMBER: LIBLP/LPEDPATH
      ******************************************************************
      *   EOD SPOOL AND FLAG PATHS
      *   NOTE, OPEN-PATH IS USED IN NOTES PAYABLE BUT
      *   NOT FROM THIS COPY MEMBER DUE TO CONFLICTS
      ******************************************************************
       01  ED-PATH                PIC X(20) VALUE " ".

      *=================================================================
      *    /USR/DATA/R#/DP/OP####D######
      *                      ^     ^
      *                      BRNO MMDDYY
      *=================================================================
       01  OPEN-PATH-OVERRIDE           PIC X(01)     VALUE SPACES.
       01  OPEN-PATH                    PIC X(50)     VALUE SPACES.
       01  OPEN-PATH-FORMAT.
           03  OPEN-PATH-BASE           PIC X(09).
           03  FILLER                   REDEFINES OPEN-PATH-BASE.
               05  OPEN-PATH-BASE-USR   PIC X(05).
               05  OPEN-PATH-BASE-DIR   PIC X(04).
           03  FILLER                   PIC X(01)   VALUE "/".
           03  OPEN-PATH-MACHINE        PIC X(02).
           03  FILLER                   PIC X(06)   VALUE "/DP/OP".
           03  OPEN-PATH-BRNO           PIC 9(04).
           03  OPEN-BR                  REDEFINES OPEN-PATH-BRNO
                                        PIC 9(04).
           03  FILLER                   PIC X(01)   VALUE "D".
           03  OPEN-PATH-TRDATE         PIC 9(06).
           03  OPEN-TRANS-DATE          REDEFINES OPEN-PATH-TRDATE
                                        PIC 9(06).
           03  FILLER                   PIC X(14).
