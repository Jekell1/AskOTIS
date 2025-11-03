      ******************************************************************
      *    LIBLP/LPWSTRX: TRXFILE (CLASS TRANSFER TRFILE ) PATH WORKERS
      *
      * TRX-PATH = /USR/DATA/R#/XX####/ED/TRXFILE
      *           |----BEG----||-MID-||-----FILE----|
      *                                  |---SQL----|
      *
      ******************************************************************
       01  TRX-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  TRX-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES TRX-PATH.
           03  TRX-PATH-BEG           PIC X(13).
           03  TRX-PATH-OWNST         PIC X(02).
           03  TRX-PATH-OWNBR         PIC 9(04).
           03  FILLER                 PIC X(01).
           03  TRX-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES TRX-PATH-FILE.
               05  FILLER             PIC X(03).
               05  TRX-PATH-SQL       PIC X(12).

       01  TRX-ALL-PATH.
           03  TRX-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES TRX-ALL-BASE.
               05  TRX-ALL-BASE-USR   PIC X(05).
               05  TRX-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRX-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  TRX-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)   VALUE "/LP/TRXFILE".

       01  TRX-BASE-ARCHIVE           PIC X(04) VALUE " ".
       01  LPWSTRX-TRX1-KEY.
           03  LPWSTRX-TX-BRNO        PIC 9(4).
           03  LPWSTRX-TX-DATE        PIC 9(8).
           03  LPWSTRX-TX-CLASS       PIC 9(3).
           03  LPWSTRX-TX-ACCTNO      PIC 9(8).
           03  LPWSTRX-TX-LAWCODE     PIC 9(2).
           03  LPWSTRX-TX-LINE        PIC 9(3).

       01  TRXSUB                     PIC 9(3).
