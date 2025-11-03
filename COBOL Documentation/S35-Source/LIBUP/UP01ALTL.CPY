      ******************************************************************
      *  ASCII  LOAN LIENHOLDER TRAILER FILE 
      *  TAB DELMITED
      *
      ******************************************************************
       78  ALTL-SIZE                      VALUE 749.
       01  ALTL-REC.
           03  ALTL-BRNO                  PIC 9(4).
           03  FILLER                     PIC X.
           03  ALTL-ACCTNO                PIC 9(8).
           03  FILLER                     PIC X.
           03  ALTL-SEQNO                 PIC 999.
           03  FILLER                     PIC X.
           03  ALTL-LTOUCH-DATE           PIC 9(8).
           03  FILLER                     PIC X.
           03  ALTL-LIEN               OCCURS 3.
               05  ALTL-LIEN-TYPE         PIC 9.
               05  FILLER                 PIC X.
               05  ALTL-LIEN-NAME         PIC X(30).
               05  FILLER                 PIC X.
               05  ALTL-LIEN-ADR          PIC X(24).
               05  FILLER                 PIC X.
               05  ALTL-LIEN-CITY         PIC X(15).
               05  FILLER                 PIC X.
               05  ALTL-LIEN-STATE        PIC X(2).
               05  FILLER                 PIC X.
               05  ALTL-LIEN-ZIP          PIC X(10).
               05  FILLER                 PIC X.
               05  ALTL-LIEN-DATE         PIC 9(8).
               05  FILLER                 PIC X.

           03  ALTL-TITLE-RCV-DATE        PIC 9(8).
           03  FILLER                     PIC X.
           03  ALTL-TITLE-LIEN-DATE       PIC 9(8).
           03  FILLER                     PIC X.
           03  ALTL-TITLE-NO              PIC X(20).
           03  FILLER                     PIC X.
           03  ALTL-LIEN-LETTER-SENT      PIC X.
           03  FILLER                     PIC X.
           03  ALTL-LIEN-COMMENT1         PIC X(60).
           03  FILLER                     PIC X.
           03  ALTL-LIEN-COMMENT2         PIC X(60).
           03  FILLER                     PIC X.
           03  ALTL-LIEN-TITLE-STATE      PIC XX.
           03  FILLER                     PIC X.
           03  ALTL-TITLE-STATUS-CODE     PIC XX.
           03  FILLER                     PIC X.
           03  ALTL-TITLE-SEND-FLAG       PIC X.
           03  FILLER                     PIC X.
           03  ALTL-TITLE-NAME            PIC X(25).
           03  FILLER                     PIC X.
           03  ALTL-TITLE-ADR             PIC X(25).
           03  FILLER                     PIC X.
           03  ALTL-TITLE-CITY            PIC X(15).
           03  FILLER                     PIC X.
           03  ALTL-TITLE-STATE           PIC X(2).
           03  FILLER                     PIC X.
           03  ALTL-TITLE-ZIP             PIC X(10).
           03  FILLER                     PIC X.
           03  ALTL-TITLE-DATE            PIC 9(8).
           03  FILLER                     PIC X.
           03  ALTL-TITLE-SENT-BY         PIC X(25).
           03  FILLER                     PIC X.
           03  ALTL-SEND-ASAP             PIC X.
           03  FILLER                     PIC X.
           03  ALTL-TITLE-SEND-FMNO       PIC 9(4).
           03  FILLER                     PIC X.
           03  ALTL-TITLE-ADR2            PIC X(22).
           03  FILLER                     PIC X.
           03  ALTL-TITLE-COMMENT1        PIC X(40).
           03  FILLER                     PIC X.
           03  ALTL-TITLE-COMMENT2        PIC X(40).
           03  FILLER                     PIC X.
           03  ALTL-RELEASE-TYPE          PIC XX.
           03  FILLER                     PIC X.
           03  ALTL-CARRIER-TYPE          PIC X(4).
           03  FILLER                     PIC X.
           03  ALTL-CARRIER-ACCTNO        PIC X(20).
           03  FILLER                     PIC X.

           03  ALTL-CR                    PIC X.
           03  ALTL-LF                    PIC X.
