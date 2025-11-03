      * COPYMEMBER: LIBUP/UP01ATRI.CPY
      ******************************************************************
      *
      * DESCRIPTION:  ASCII TRANSACTION FILE [INSURANCE CLAIMS]
      *
      * FILE INFO  :  INSURANCE CLAIMS, THESE ARE MEMO PURPOSE ONLY
      *               LP/TRIFILE
      *
      ******************************************************************
       78  ATRI-SIZE                    VALUE 239.
       01  ATRI-REC.
           03  ATI-BRNO                 PIC 9(4).
           03  FILLER                   PIC X.
           03  ATI-KEY-DATE             PIC 9(8). 
           03  FILLER                   PIC X.
           03  ATI-SEQ                  PIC 9(5).
           03  FILLER                   PIC X.
           03  ATI-LTOUCH-DATE          PIC 9(8).
           03  FILLER                   PIC X.
           03  ATI-WAIS                 PIC X(4).
           03  FILLER                   PIC X.
           03  ATI-FILLER               PIC X(4).
           03  FILLER                   PIC X.
           03  ATI-TYPE                 PIC 9(3).
           03  FILLER                   PIC X.
           03  ATI-AMOUNT               PIC +9(8).99.
           03  FILLER                   PIC X.
           03  ATI-CLAIM-NUMBER         PIC X(9).
           03  FILLER                   PIC X.
           03  ATI-FILLER2              PIC X(13).
           03  FILLER                   PIC X.
           03  ATI-LNAME                PIC X(15).
           03  FILLER                   PIC X.
           03  ATI-FNAME                PIC X(10).
           03  FILLER                   PIC X.
           03  ATI-DATE                 PIC 9(8).
           03  FILLER                   PIC X.
           03  ATI-DRAFT                PIC 9(8).
           03  FILLER                   PIC X.
           03  ATI-FILLER3              PIC X(40).
           03  FILLER                   PIC X.
           03  ATI-PRODUCER             PIC 9(6).
           03  FILLER                   PIC X.
           03  ATI-USERID               PIC X(8).
           03  FILLER                   PIC X.
           03  ATI-POSTTIME             PIC 9(6).
           03  FILLER                   PIC X.
           03  ATI-POSTDATE             PIC 9(8).
           03  FILLER                   PIC X.
           03  ATI-INSCOMP              PIC 9(2).                   
           03  FILLER                   PIC X.
           03  ATI-DAYS-PAID            PIC 9(6).
           03  FILLER                   PIC X.
           03  ATI-PMT-TYPE             PIC X(10).
           03  FILLER                   PIC X.
           03  ATI-FROM-DATE            PIC 9(8).
           03  FILLER                   PIC X.
           03  ATI-TO-DATE              PIC 9(8).
           03  FILLER                   PIC X.
           03  ATI-CR                  PIC X.
           03  ATI-LF                  PIC X.
