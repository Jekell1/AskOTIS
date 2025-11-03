      ******************************************************************
      *    LIBLP/LPWSTRS: TRSFILE (SALES FINANCE TRFILE ) PATH WORKERS
      *
      * TRS-PATH = /USR/DATA/R#/XX####/ED/TRSFILE
      *           |----BEG----||-MID-||-----FILE----|
      *                                  |---SQL----|
      *
      ******************************************************************
       01  TRS-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  TRS-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES TRS-PATH.
           03  TRS-PATH-BEG           PIC X(13).
           03  TRS-PATH-OWNST         PIC X(02).
           03  TRS-PATH-OWNBR         PIC 9(04).
           03  FILLER                 PIC X(01).
           03  TRS-PATH-FILE          PIC X(15).
           03  FILLER              REDEFINES TRS-PATH-FILE.
               05  FILLER             PIC X(03).
               05  TRS-PATH-SQL       PIC X(12).

       01  TRS-ALL-PATH.
           03  TRS-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES TRS-ALL-BASE.
               05  TRS-ALL-BASE-USR   PIC X(05).
               05  TRS-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRS-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRS-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)   VALUE "/LP/TRSFILE".

       01  TRS-BASE-ARCHIVE           PIC X(04) VALUE " ".
       01  LPWSTRS-TRS1-KEY.
           03  LPWSTRS-TS-BRNO        PIC 9(4).
           03  LPWSTRS-TS-DATE        PIC 9(8).
           03  LPWSTRS-TS-CLASS       PIC 9(3).
           03  LPWSTRS-TS-ACCTNO      PIC 9(8).
           03  LPWSTRS-TS-LAWCODE     PIC 9(2).
           03  LPWSTRS-TS-LINE        PIC 9(3).

       01  TRSSUB                     PIC 9(3).
