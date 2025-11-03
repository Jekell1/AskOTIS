      * COPYMEMBER: LIBLP/LPWSBW
      *********************************************************
      *    LIBLP/LPWSBW: BWFILE PATH WORKERS
      *
      *    BW-PATH = /USR/DATA/XX/XX####/GB/BWFILE
      *              |----BEG----||-MID-||-----FILE----|
      *
      ******************************************************************
       01  BW-BW-TEST-GROUP.
           03  BW-BW-TEST-FIELD-A    PIC X(06).
           03  BW-BW-TEST-FIELD-AX   REDEFINES BW-BW-TEST-FIELD-A.
               05  BW-IC-TEST-FIELD-A1   PIC X(01).
               05  BW-IC-TEST-FIELD-A2   PIC X(02).
               05  BW-IC-TEST-FIELD-A3   PIC X(03).

       01  BW-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  BW-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES BW-PATH.
           03  BW-PATH-BEG           PIC X(13).
           03  BW-PATH-OWNST         PIC X(02).
           03  BW-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  BW-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES BW-PATH-FILE.
               05  FILLER            PIC X(03).
               05  BW-PATH-SQL       PIC X(12).

       01  BW-ALL-PATH.
           03  BW-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES BW-ALL-BASE.
               05  BW-ALL-BASE-USR   PIC X(05).
               05  BW-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  BW-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)   VALUE "/".
           03  BW-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)   VALUE "/GB/BWFILE".
