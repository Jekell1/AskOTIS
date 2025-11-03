      ******************************************************************
      *    LIBLP/LPWSTRW: TRWFILE (CHECKS/CASH TRFILE ) PATH WORKERS
      *
      * TRW-PATH = /USR/DATA/R#/XX####/ED/TRWFILE
      *           |----BEG----||-MID-||-----FILE----|
      *                                  |---SQL----|
      *
      ******************************************************************
       01  TRW-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  TRW-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES TRW-PATH.
           03  TRW-PATH-BEG           PIC X(13).
           03  TRW-PATH-OWNST         PIC X(02).
           03  TRW-PATH-OWNBR         PIC 9(04).
           03  FILLER                 PIC X(01).
           03  TRW-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES TRW-PATH-FILE.
               05  FILLER             PIC X(03).
               05  TRW-PATH-SQL       PIC X(12).

       01  TRW-ALL-PATH.
           03  TRW-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES TRW-ALL-BASE.
               05  TRW-ALL-BASE-USR   PIC X(05).
               05  TRW-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRW-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRW-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)   VALUE "/LP/TRWFILE".

       01  TRW-BASE-ARCHIVE           PIC X(04) VALUE " ".
       01  LPWSTRW-TRW1-KEY.
           03  LPWSTRW-TW-BRNO        PIC 9(4).
           03  LPWSTRW-TW-DATE        PIC 9(8).
           03  LPWSTRW-TW-TILLNO      PIC 9(2).

       01  TRWSUB                     PIC 9(3).
