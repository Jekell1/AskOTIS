      * COPYMEMBER: LIBLP/LPWSFD
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LPWSFD
      *
      * DESCRIPTION   :  FDFILE PATH WORKERS
      *
      *=================================================================
      * REV:
      * KEC 2005.0809 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               ADDED 'LPWSFD-PREFIX'
      *
      * KEC 2007.1031 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               ADDED 'LPWSFD-FORM-NAM'.
      *
      * KEC 2011.1003 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *               ADDED 'PST' (POSTSCRIPT) OPTION.
      *
      ******************************************************************
      *    /USR/DATA/R#/%%/%%FILE
      *-----------------------------------------------------------------
       01  FD-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  FD-PATH                   PIC X(35)   VALUE SPACES.
       01  FD-ALL-PATH.
           03  FD-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES FD-ALL-BASE.
               05  FD-ALL-BASE-USR   PIC X(05).
               05  FD-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  FD-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/LP/FDFILE".

      *=================================================================
      *78  LPWSFD---PREFIX-LONP                      VALUE 0.
      *78  LPWSFD---PREFIX-PCL                       VALUE 1.
      *78  LPWSFD---PREFIX-PRT                       VALUE 2.
      *78  LPWSFD---PREFIX-PST                       VALUE 3.
      *-----------------------------------------------------------------
      *77  LPWSFD-PREFIX               PIC 9(01)     VALUE ZEROES.
      *    88  LPWSFD-PREFIX-LONP                    VALUE 0.
      *    88  LPWSFD-PREFIX-PCL                     VALUE 1.
      *    88  LPWSFD-PREFIX-PRT                     VALUE 2.
      *    88  LPWSFD-PREFIX-PST                     VALUE 3.

      *=================================================================
      *78  LPWSFD-FORM-NAM---PCL                     VALUE "FP/PCL###".
      *78  LPWSFD-FORM-NAM---PRT                     VALUE "FP/PRT###".
      *78  LPWSFD-FORM-NAM---PST                     VALUE "FP/PST###".
      *78  LPWSFD-FORM-NAM---LONP                    VALUE "LP/LONP##".
      *-----------------------------------------------------------------
      *01  LPWSFD-FORM-NAM             PIC X(09)     VALUE SPACES.
      *01  FILLER                      REDEFINES LPWSFD-FORM-NAM.
      *    03  FILLER                  PIC X(03).
      *    03  LPWSFD-FORM-PROGX       PIC X(06).
      *01  FILLER                      REDEFINES LPWSFD-FORM-NAM.
      *    03  FILLER                  PIC X(06).
      *    03  LPWSFD-FORM-PROG-PREFIX PIC X(01).
      *    03  LPWSFD-FORM-PROG        PIC X(02).

