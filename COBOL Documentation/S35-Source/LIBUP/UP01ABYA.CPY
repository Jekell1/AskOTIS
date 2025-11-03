      * COPYMEMBER: LIBUP/UP01ABYA.CPY
      ******************************************************************
      *
      * DESCRIPTION: BRANCH STATISTICS FILE - A (PRIMARY) AREA
      *
      * FILE PATH  : /USR/DATA/R#/LP/BYAFILE  
      *      SIZE  : BYA-AREA-A - 3702
      *      SIZE  : BYB-AREA-B - 3544
      *      SIZE  : BY-REC     - 7233
      *
      ******************************************************************
       78  ABYA-SIZE                     VALUE 8571.
       01  ABYA-REC.
           03  ABYA-DATE-CCYYMM          PIC 9(6).
           03  FILLER                    PIC X.
           03  ABYA-BRNO                 PIC 9(4).
           03  FILLER                    PIC X.
           03  ABYA-CLASS                PIC 9(3).
           03  FILLER                    PIC X.
           03  ABYA-LTOUCH-DATE          PIC 9(08).
           03  FILLER                    PIC X.
           03  ABYA-PERIOD-TBL              OCCURS 3.
               05  ABYA-ACTIVITY-TBL        OCCURS 2.
                   07  ABYA-TYPE-TBL      OCCURS 11.
                       09  ABYA-NO        PIC +9(7).
                       09  FILLER         PIC X.
                       09  ABYA-AMT       PIC +9(9).99.
                       09  FILLER         PIC X.
                       09  ABYA-O2        PIC +9(9).99.
                       09  FILLER         PIC X.
                       09  ABYA-INT       PIC +9(9).99.
                       09  FILLER         PIC X.

           03  ABYA-REPO-PERIOD-TBL         OCCURS 3.
               05  ABYA-REPO-ACTIVITY-TBL   OCCURS 2.
                   07  ABYA-REPO-TYPE-TBL   OCCURS 5.
                       09  ABYA-NO-REPO     PIC +9(7).
                       09  FILLER           PIC X.
                       09  ABYA-AMT-REPO    PIC +9(9).99.
                       09  FILLER           PIC X.
                       09  ABYA-O2-REPO     PIC +9(9).99.
                       09  FILLER           PIC X.
                       09  ABYA-INT-REPO    PIC +9(9).99.
                       09  FILLER           PIC X.

           03  ABYA-PL-PERIOD-TBL           OCCURS 3.
               05  ABYA-PL-ACTIVITY-TBL     OCCURS 2.
                   07  ABYA-PL-TYPE-TBL     OCCURS 10.
                       09  ABYA-NO-PL       PIC +9(7).
                       09  FILLER           PIC X.
                       09  ABYA-AMT-PL      PIC +9(9).99.
                       09  FILLER           PIC X.
                       09  ABYA-O2-PL       PIC +9(9).99.
                       09  FILLER           PIC X.
                       09  ABYA-INT-PL      PIC +9(9).99.
                       09  FILLER           PIC X.

           03  ABYA-DLR-PERIOD-TBL         OCCURS 3.
               05  ABYA-DLR-ACTIVITY-TBL   OCCURS 2.
                   07  ABYA-DLRVPRIN-TBL   OCCURS 3.
                       09  ABYA-DLRVPRIN   PIC +9(9).99.
                       09  FILLER          PIC X.
                   07  ABYA-DLRVCHGS-TBL   OCCURS 3.
                       09  ABYA-DLRVCHGS   PIC +9(9).99.
                       09  FILLER          PIC X.
                   07  ABYA-DLPARVHELD     PIC +9(9).99.
                   07  FILLER              PIC X.
           03  ABYA-CR                   PIC X.
           03  ABYA-LF                   PIC X.
