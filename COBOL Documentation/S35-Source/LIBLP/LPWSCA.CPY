      * COPYMEMBER: LIBLP/LPWSCA
      ******************************************************************
      *    LIBLP/LPWSCA: CAFILE PATH WORKERS
      *
      *    CA-PATH = /USR/DATA/XX/XX####/LP/CAFILE
      *              |----BEG----||-MID-||-----FILE----|
      *
      ******************************************************************
       01  CA-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  CA-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES CA-PATH.
           03  CA-PATH-BEG           PIC X(13).
           03  CA-PATH-OWNST         PIC X(02).
           03  CA-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  CA-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES CA-PATH-FILE.
               05  FILLER            PIC X(03).
               05  CA-PATH-SQL       PIC X(12).

       01  CA-ALL-PATH.
           03  CA-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES CA-ALL-BASE.
               05  CA-ALL-BASE-USR   PIC X(05).
               05  CA-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  CA-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)   VALUE "/".
           03  CA-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)   VALUE "/LP/CAFILE".

      *=================================================================
      * IDEAS: PRE-COMPILE EXEC SQL WORKERS (QCA-REC) AND PUT CODE INTO
      *        END OF LIBLP/LPWSLN.CPY
      *=================================================================
