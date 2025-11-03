      * COPYMEMBER: LIBLP/LP01VBY
      ******************************************************************
      *
      * DESCRIPTION: VISION VERSION OF THE DAILY COLLECTOR QUEUE FILE
      *
      * VBY1-KEY   : VBY-DATE-CCYYMM, VBY-BRNO, VBY-CLASS
      *
      *=================================================================
      * THE PURPOSE OF THE VBYFILE IS FOR LOADING A SPECIFIC RANGE OF
      * BY-REC (BYA-REC, BYB-REC) FOR BETTER PERFORMANCE AND HELP
      * RECOVERABILITY OF A PROBLEM WITHIN LP/EOMALL.
      *
      *=================================================================
      * REV:
      * JKC 2024-0829 ***NEW***
      *
      ******************************************************************
       78  VBY-SIZE                                  VALUE 8813.
       78  VBY-BY-SIZE                               VALUE 8800.
       01  VBY-REC.
           03  VBY1-KEY.
               05  VBY-DATE-CCYYMM     PIC 9(6).
               05  VBY-BRNO            PIC 9(4).
               05  VBY-CLASS           PIC 9(3).
           03  VBY-BY-REC              PIC X(VBY-BY-SIZE).

