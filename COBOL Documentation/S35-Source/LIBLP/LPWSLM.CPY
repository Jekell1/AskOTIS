      * COPYMEMBER: LIBLP/LPWSLM
      ******************************************************************
      *
      *        LIBLP/LPWSLM: LMFILE PATH WORKERS
      * LM-PATH = /USR/DATA/XX/XX####/LP/LMFILE
      *           |----BEG----||-MID-||-----FILE----|
      *
      *=================================================================
      * REV:
      * JKC 2022-0810 ADDED LM1-CURSOR-SC-STAT
      *
      ******************************************************************
       01  LM-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.

       01  LM-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES LM-PATH.
           03  LM-PATH-BEG           PIC X(13).
           03  LM-PATH-OWNST         PIC X(02).
           03  LM-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  LM-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES LM-PATH-FILE.
               05  FILLER            PIC X(03).
               05  LM-PATH-SQL       PIC X(12).

       01  LM-ALL-PATH.
           03  LM-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES LM-ALL-BASE.
               05  LM-ALL-BASE-USR   PIC X(05).
               05  LM-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LM-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LM-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/LMFILE".

       01  WS-LM-LTOUCH-TIME         PIC 9(8).
       01  FILLER REDEFINES WS-LM-LTOUCH-TIME.
           03  WS-LM-LTOUCH-HHMM     PIC 9(4).
           03  FILLER                PIC 9(4).
