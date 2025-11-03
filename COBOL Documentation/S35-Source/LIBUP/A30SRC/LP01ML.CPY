      * COPYMEMBER: LIBLP/LP01ML
      *********************************************************************
      *            ( MAIL LIST FILE )
      *                  LP01ML
      *
      * REV:
      *  171120 BAH REMOVED COMP-3 FROM ML-BRANCH AND INCREASED FILLER
      *********************************************************************
       78  ML-SIZE                     VALUE 270.
       01  ML-REC.
           03  ML-BRANCH               PIC 9(4).
           03  ML1-KEY.
               05  ML-SSNO             PIC 9(9).
           03  ML2-KEY.
               05  ML-LNAME            PIC X(30).
               05  ML-FNAME            PIC X(30).
           03  ML-ADR1                 PIC X(24).
           03  ML-ADR2                 PIC X(24).
           03  ML-CITY                 PIC X(15).
           03  ML-STATE                PIC X(2).
           03  ML-ZIP                  PIC X(10).
      *----------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  ML-REFDATE              PIC 9(8)      COMP-3.
           03  ML-SRCD                 PIC X(2).
           03  ML-SOLICIT              PIC X.
           03  ML-CONFIDENTIAL         PIC X.
      *----------------------------------------------------
           03  ML-LAST-FORM-NO         PIC 9(4)      COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  ML-LAST-FORM-DATE       PIC 9(8)      COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  ML-LTOUCH-DATE          PIC 9(8)      COMP-3.
           03  FILLER                  PIC X(100).
