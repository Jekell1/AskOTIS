      ******************************************************************
      *    LIBLP/LPWSTRJ: TRJFILE (CLASS TRANSFER TRFILE ) PATH WORKERS
      *
      * TRJ-PATH = /USR/DATA/R#/XX####/ED/TRJFILE
      *           |----BEG----||-MID-||-----FILE----|
      *                                  |---SQL----|
      *
      ******************************************************************
       01  TRJ-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  TRJ-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES TRJ-PATH.
           03  TRJ-PATH-BEG           PIC X(13).
           03  TRJ-PATH-OWNST         PIC X(02).
           03  TRJ-PATH-OWNBR         PIC 9(04).
           03  FILLER                 PIC X(01).
           03  TRJ-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES TRJ-PATH-FILE.
               05  FILLER             PIC X(03).
               05  TRJ-PATH-SQL       PIC X(12).

       01  TRJ-ALL-PATH.
           03  TRJ-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES TRJ-ALL-BASE.
               05  TRJ-ALL-BASE-USR   PIC X(05).
               05  TRJ-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRJ-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRJ-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)   VALUE "/LP/TRJFILE".

       01  TRJ-BASE-ARCHIVE           PIC X(04) VALUE " ".
       01  LPWSTRJ-TRJ1-KEY.
           03  LPWSTRJ-TJ-BRNO        PIC 9(4).
           03  LPWSTRJ-TJ-DATE        PIC 9(8).
           03  LPWSTRJ-TJ-CLASS       PIC 9(3).
           03  LPWSTRJ-TJ-ACCTNO      PIC 9(8).
           03  LPWSTRJ-TJ-LAWCODE     PIC 9(2).
           03  LPWSTRJ-TJ-LINE        PIC 9(3).

       01  TRJSUB                     PIC 9(3).
