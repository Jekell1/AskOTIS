      * COPYMEMBER: LIBLP/LPWSDM
      *********************************************************
      *        LIBLP/LPWSDM: DMFILE PATH WORKERS
      *
      * DM-PATH = /USR/DATA/XX/XX####/LP/DMFILE
      *           |----BEG----||-MID-||-----FILE----|
      *
      *********************************************************
       01  DM-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.

       01  DM-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES DM-PATH.
           03  DM-PATH-BEG           PIC X(13).
           03  DM-PATH-OWNST         PIC X(02).
           03  DM-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  DM-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES DM-PATH-FILE.
               05  FILLER            PIC X(03).
               05  DM-PATH-SQL       PIC X(12).

       01  DM-ALL-PATH.
           03  DM-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES DM-ALL-BASE.
               05  DM-ALL-BASE-USR   PIC X(05).
               05  DM-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  DM-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  DM-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/DMFILE".

       01  WS-DM-LTOUCH-TIME         PIC 9(8).
       01  FILLER REDEFINES WS-DM-LTOUCH-TIME.
           03  WS-DM-LTOUCH-HHMM     PIC 9(4).
           03  FILLER                PIC 9(4).
