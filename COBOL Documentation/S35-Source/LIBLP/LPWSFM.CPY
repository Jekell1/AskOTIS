      * COPYMEMBER: LIBLP/LPWSFM
      ******************************************************************
      *
      * IDENTIFICATION: LIBLP/LPWSFM
      * DESCRIPTION   : FMFILE PATH WORKERS
      *    FM-PATH = /USR/DATA/XX/XX####/LP/FMFILE
      *              |----BEG----||-MID-||-----FILE----|
      *
      *=================================================================
      * REV:
      * KEC 2016.0121 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FM-REC.
      *         ADDED LPWSFM-FM1-KEY (REDEFINES LPWSFM-EDIT-FM1-KEY).
      *
      * KEC 2018-0823 LOAN APP MODULE REMOVAL FROM A30 [LARM30]
      *
      ******************************************************************
       01  FM-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.

       01  FM-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES FM-PATH.
           03  FM-PATH-BEG           PIC X(13).
           03  FM-PATH-OWNST         PIC X(02).
           03  FM-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  FM-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES FM-PATH-FILE.
               05  FILLER            PIC X(03).
               05  FM-PATH-SQL       PIC X(12).

       01  FM-ALL-PATH.
           03  FM-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES FM-ALL-BASE.
               05  FM-ALL-BASE-USR   PIC X(05).
               05  FM-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  FM-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  FM-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/FMFILE".


      * LPWSFM-EDIT-FM-BRNO2 AND LPWSFM-EDIT-FM-BW-BRNO2 ARE THE OWNING
      * BRANCH FOR CORPORATE PRINT. THE FM RECORD IS CREATED IN THE 
      * CORPORATE BRANCH BUT NEEDS OWNING BRANCH TO FIND THE LOAN IN THE
      * OWNING BRANCH (THIS WAS NEEDED MORE IN THE SEPARATE FILE DAYS)
     
       01  LPWSFM-EDIT-FM-INITKEY.
           03  LPWSFM-EDIT-FM-ACCTNO   PIC 9(8).
           03  LPWSFM-EDIT-FM-FORMNO   PIC 9(4).
           03  FILLER                  PIC X(4).
           03  LPWSFM-EDIT-FM-BRNO2    PIC 9(4).

       01  LPWSFM-EDIT-FM-INITKEY-BW REDEFINES LPWSFM-EDIT-FM-INITKEY.
           03  LPWSFM-EDIT-FM-SSNO     PIC 9(9).
           03  FILLER                  PIC X(7).
           03  LPWSFM-EDIT-FM-BW-BRNO2 PIC 9(4).

       01  LPWSFM-EDIT-FM-INITKEY-CK REDEFINES LPWSFM-EDIT-FM-INITKEY.
           03  LPWSFM-EDIT-FM-BRNO   PIC 9(4).
           03  LPWSFM-EDIT-FM-TYPE   PIC XX.
           03  LPWSFM-EDIT-FM-SEQNO  PIC 9(4).
           03  LPWSFM-EDIT-FM-CKFMNO PIC 9(4).
           03  FILLER                PIC X(6).
