      * COPYMEMBER: LIBGB/GB01VRC
      ******************************************************************
      *
      * DESCRIPTION: VISION VERSION OF THE DAILY COLLECTOR QUEUE FILE
      *
      * VRC1-KEY   : VRC-BRNO, VRC-TRANS-DATE
      * VRC2-KEY   : VRC-STATUS
      *
      *=================================================================
      * THE PURPOSE OF THE VRCFILE IS FOR LOADING A SPECIFIC RANGE OF
      * RC-REC FOR BETTER PERFORMANCE AND HELP
      * RECOVERABILITY OF A PROBLEM WITHIN LP/EOMALL.
      *
      *=================================================================
      * REV:
      * JKC 2024-0829 ***NEW***
      *
      ******************************************************************
       78  VRC-SIZE                                  VALUE 314.
       78  VRC-RC-SIZE                               VALUE 300.
       01  VRC-REC.
           03  VRC2-KEY.
               05  VRC-STATUS          PIC XX.
               05  VRC1-KEY.
                  07  VRC-BRNO         PIC 9(4).
                  07  VRC-TRANS-DATE   PIC 9(8).
           03  VRC-RC-REC              PIC X(VRC-RC-SIZE).

