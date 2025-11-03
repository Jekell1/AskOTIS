      ******************************************************************
      *    LIBLP/LPWSTRD: TRDFILE (DEPOSITS TRFILE ) PATH WORKERS
      *
      * TRD-PATH = /USR/DATA/R#/XX####/ED/TRDFILE
      *           |----BEG----||-MID-||-----FILE----|
      *                                  |---SQL----|
      *
      ******************************************************************
       01  TRD-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  TRD-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES TRD-PATH.
           03  TRD-PATH-BEG           PIC X(13).
           03  TRD-PATH-OWNST         PIC X(02).
           03  TRD-PATH-OWNBR         PIC 9(04).
           03  FILLER                 PIC X(01).
           03  TRD-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES TRD-PATH-FILE.
               05  FILLER             PIC X(03).
               05  TRD-PATH-SQL       PIC X(12).

       01  TRD-ALL-PATH.
           03  TRD-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES TRD-ALL-BASE.
               05  TRD-ALL-BASE-USR   PIC X(05).
               05  TRD-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRD-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRD-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)   VALUE "/LP/TRDFILE".

       01  TRD-BASE-ARCHIVE           PIC X(04) VALUE " ".
       01  LPWSTRD-TRD1-KEY.
           03  LPWSTRD-TD-BRNO        PIC 9(4).
           03  LPWSTRD-TD-DATE        PIC 9(8).
