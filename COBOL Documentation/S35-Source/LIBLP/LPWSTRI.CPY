      ******************************************************************
      *    LIBLP/LPWSTRI: TRIFILE (INSURANCE CLAIMS TRFILE ) PATH WORKERS
      *
      * TRI-PATH = /USR/DATA/R#/XX####/ED/TRIFILE
      *           |----BEG----||-MID-||-----FILE----|
      *                                  |---SQL----|
      *
      ******************************************************************
       01  TRI-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  TRI-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES TRI-PATH.
           03  TRI-PATH-BEG           PIC X(13).
           03  TRI-PATH-OWNST         PIC X(02).
           03  TRI-PATH-OWNBR         PIC 9(04).
           03  FILLER                 PIC X(01).
           03  TRI-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES TRI-PATH-FILE.
               05  FILLER             PIC X(03).
               05  TRI-PATH-SQL       PIC X(12).

       01  TRI-ALL-PATH.
           03  TRI-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES TRI-ALL-BASE.
               05  TRI-ALL-BASE-USR   PIC X(05).
               05  TRI-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRI-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRI-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)   VALUE "/LP/TRIFILE".

       01  TRI-BASE-ARCHIVE           PIC X(04) VALUE " ".
       01  LPWSTRI-TRI1-KEY.
           03  LPWSTRI-TI-BRNO        PIC 9(4).
           03  LPWSTRI-TI-KEY-DATE    PIC 9(8).
           03  LPWSTRI-TI-SEQ         PIC 9(5).

       01  TRISUB                     PIC 9(3).
