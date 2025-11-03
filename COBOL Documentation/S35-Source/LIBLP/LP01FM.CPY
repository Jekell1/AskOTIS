      * COPYMEMBER: LIBLP/LP01FM
      **************************************************************************
      *                 (FM)
      *       FORM REQUEST FILE   
      *
      *    FM1-KEY  - FM-BRNO, FM-INITKEY, FM-FMNO
      *    FM2-KEY  - NON-CONTIGUOUS - FM-BRNO, FM-FMNO, FM-ZIP
      *
      * CORPORATE FORM PRINTING:
      *        FM-BRNO WILL BE THE CORPORATE BRANCH SO YOU CAN FIND THEM ALL
      *        FM-BRNO2 WILL BE THE OWNING BRANCH TO SET UP THE CORRECT PATHS
      *                 IT IS THE LAST 4 OF FM-INITKEY. IN LPWSFM
      *        01  LPWSFM-EDIT-FM-INITKEY.
      *            03  LPWSFM-EDIT-FM-ACCTNO      PIC 9(8).
      *            03  LPWSFM-EDIT-FM-FMNO        PIC 9(4).
      *            03  FILLER                     PIC X(4).
      * BRNO2 IS USED FOR CORPORATE PRINTING WITH OWNING BRANCH
      *            07  LPWSFM-EDIT-FM-BRNO2       PIC 9(4).
      *
      * REV:
      *  MJD 990126 ----- UPDATED IN FLEXGEN -----
      *  BLV 991206 ADDED REDEFINE OF FM-INITKEY FOR GLOBAL FORM PRINTING:
      *             NEEDED BR # IN PRIMARY KEY TO KEEP UNIQUE - DIDN'T
      *             CHANGE RECORD FORMAT - SHOULDN'T AFFECT ANYTHING
      *  BAH 160317 CHANGED DATE TO 9(8), MOVED FM-BRNO, ADDED FILLER
      *             ADDED FM-USERID FOR FUTURE USE
      *  BAH 160913 INCREASED FM-ZIP FROM 10 TO 15 SO A KEY OF BRNO,CLASS,ACCTNO
      *             FROM LONIW0 WILL FIT IN IT.
      * BAH 181102 GLOBAL FMFILE
      *  CS 190604 ADD FUTURE FIELDS
      * BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * BAH 20200610 ADDED FM-LTOUCH-DATE, CHANGED SIZE
      * BAH 20210427 REMOVED FUTURE FIELDS
      *  BAH 20230600 ADDED FILLER
      **************************************************************************
       78  FM-SIZE                         VALUE 165.
       01  FM-REC.
           03  FM1-KEY.
               05  FM-BRNO                 PIC 9(4).
               05  FM-INITKEY              PIC X(20).
               05  FM-FMNO                 PIC 9(4).

      * INCREASED FM-ZIP FROM 10 TO 15 SO A KEY OF BRNO,CLASS,ACCTNO
      * FROM LONIW0 WILL FIT IN IT.
           03  FM-ZIP                      PIC X(15).

           03  FM-PRINT                    PIC X.
      * USED TO HOLD LAST PMT PRINTED ON STMT FOR REPRINT
           03  FM-STMT-COUNTER             PIC 9(5) COMP-3.
      $XFD DATE=YYYYMMDD
           03  FM-TRDATE                   PIC 9(8) COMP-3.
           03  FM-USERID                   PIC X(10).
      $XFD DATE=YYYYMMDD
           03  FM-LTOUCH-DATE              PIC 9(8) COMP-3.
           03  FILLER                      PIC X(98).
