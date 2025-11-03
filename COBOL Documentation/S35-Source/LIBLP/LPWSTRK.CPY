      ******************************************************************
      *    LIBLP/LPWSTRK: TRKFILE (CONVERSIONS TRFILE ) PATH WORKERS
      *
      * TRK-PATH = /USR/DATA/R#/XX####/ED/TRKFILE
      *           |----BEG----||-MID-||-----FILE----|
      *                                  |---SQL----|
      *
      ******************************************************************
       01  TRK-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  TRK-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES TRK-PATH.
           03  TRK-PATH-BEG           PIC X(13).
           03  TRK-PATH-OWNST         PIC X(02).
           03  TRK-PATH-OWNBR         PIC 9(04).
           03  FILLER                 PIC X(01).
           03  TRK-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES TRK-PATH-FILE.
               05  FILLER             PIC X(03).
               05  TRK-PATH-SQL       PIC X(12).

       01  TRK-ALL-PATH.
           03  TRK-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES TRK-ALL-BASE.
               05  TRK-ALL-BASE-USR   PIC X(05).
               05  TRK-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRK-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRK-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)   VALUE "/LP/TRKFILE".

       01  TRK-BASE-ARCHIVE           PIC X(04) VALUE " ".
       01  LPWSTRK-TRK1-KEY.
           03  LPWSTRK-TK-BRNO        PIC 9(4).
           03  LPWSTRK-TK-DATE        PIC 9(8).
           03  LPWSTRK-TK-CLASS       PIC 9(3).
           03  LPWSTRK-TK-ACCTNO      PIC 9(8).
           03  LPWSTRK-TK-LAWCODE     PIC 9(2).
           03  LPWSTRK-TK-LINE        PIC 9(3).

       01  TRKSUB                     PIC 9(3).
