      * COPYMEMBER: LIBGL/GLSCANW
      * 2024.0423 BAH CHANGED TO LEVEL 01 AND CALL, NOT WINDOW
      *               REMOVED GLSCAN-SCAN-TYPE
       01  GLSCAN-BUF.
           03  GLSCAN-KEY          PIC X(12).
           03  GLSCAN-NO          REDEFINES GLSCAN-KEY.
               05  GLSCAN-BRANCH   PIC 9(4).
               05  GLSCAN-DASH     PIC X.
               05  GLSCAN-ACCOUNT  PIC 9(7).
           03  GLSCAN-DESC         PIC X(36).
           03  GLSCAN-STATUS       PIC XX.
