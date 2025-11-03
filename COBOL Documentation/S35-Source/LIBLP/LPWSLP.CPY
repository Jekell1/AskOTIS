      * COPYMEMBER: LIBLP/LPWSLP
      *********************************************************
      *    LIBLP/LPWSLP: LPFILE PATH WORKERS
      *
      * LP-PATH = /USR/DATA/R#/XX####/GB/LPFILE
      *           |----BEG----||-MID-||-----FILE----|
      *                                  |---SQL----|
      *
      ******************************************************************
       01  LP-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  LP-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES LP-PATH.
           03  LP-PATH-BEG           PIC X(13).
           03  LP-PATH-OWNST         PIC X(02).
           03  LP-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  LP-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES LP-PATH-FILE.
               05  FILLER            PIC X(03).
               05  LP-PATH-SQL       PIC X(12).

       01  LP-ALL-PATH.
           03  LP-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES LP-ALL-BASE.
               05  LP-ALL-BASE-USR   PIC X(05).
               05  LP-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  LP-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)   VALUE "/".
           03  LP-ALL-DATA           PIC X(06).
           03  FILLER                REDEFINES LP-ALL-DATA.
               05  LP-ALL-STATE      PIC X(02).
               05  LP-ALL-BRNO       PIC 9(04).
           03  FILLER                PIC X(10)   VALUE "/GB/LPFILE".

      **************************************************************
       01  LPWSLP-LP-REFNO             PIC X(5).

       01  LPWSLP-LP-POOLID   REDEFINES LPWSLP-LP-REFNO
                                       PIC  9(5).

      * THIS IS USED FOR DUE DATE CHANGES AND ALSO TRANSFER TO SERVICE/PURCHASE
      * THE TRANSFERS ARE FOR MEMO ONLY, TOCL AND FROMCL ARE THEIR OWN FIELDS
       01  LPWSLP-LP-DDNO     REDEFINES LPWSLP-LP-REFNO.
           03  LPWSLP-LP-FROMDD        PIC 99.
           03  LPWSLP-LP-T             PIC X.
           03  LPWSLP-LP-TODD          PIC 99.

       01  LPWSLP-LP-ADDON    REDEFINES LPWSLP-LP-REFNO.
           03  LPWSLP-LP-ADDON-TEXT    PIC X(3).
           03  LPWSLP-LP-ADDON-TYPE    PIC X(2).

       01  FILLER             REDEFINES LPWSLP-LP-REFNO.
           03  LPWSLP-LP-DLRVNO        PIC 9.
           03  FILLER                  PIC X(4).

       01  FILLER             REDEFINES LPWSLP-LP-REFNO.
           03  LPWSLP-LP-P4P5-TRCD     PIC XX.
           03  LPWSLP-LP-P4P5-DLRVNO   PIC 9.
           03  FILLER                  PIC X(2).
