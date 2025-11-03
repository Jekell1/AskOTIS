      ******************************************************************
      *    LIBLP/LPWSTRP: TRPFILE (PAYMENT TRFILE ) PATH WORKERS
      *
      * TRP-PATH = /USR/DATA/R#/XX####/ED/TRPFILE
      *           |----BEG----||-MID-||-----FILE----|
      *                                  |---SQL----|
      *
      * BLM 20210810 REMOVE LPWSTRP-TP-CODE & LPWSTRP-TP-POOLID 
      ******************************************************************
       01  TRP-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  TRP-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES TRP-PATH.
           03  TRP-PATH-BEG           PIC X(13).
           03  TRP-PATH-OWNST         PIC X(02).
           03  TRP-PATH-OWNBR         PIC 9(04).
           03  FILLER                 PIC X(01).
           03  TRP-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES TRP-PATH-FILE.
               05  FILLER             PIC X(03).
               05  TRP-PATH-SQL       PIC X(12).

       01  TRP-ALL-PATH.
           03  TRP-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES TRP-ALL-BASE.
               05  TRP-ALL-BASE-USR   PIC X(05).
               05  TRP-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRP-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRP-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)   VALUE "/LP/TRPFILE".


       01  LPWSTRP-TRP1-KEY.
           03  LPWSTRP-TP-BRNO        PIC 9(4).
           03  LPWSTRP-TP-DATE        PIC 9(8).
           03  LPWSTRP-TP-CLASS       PIC 9(3).
           03  LPWSTRP-TP-ACCTNO      PIC 9(8).
           03  LPWSTRP-TP-LINE        PIC 9(3).
