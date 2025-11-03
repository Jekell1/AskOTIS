      * COPYMEMBER: LIBCL/VCQUPD_LNK.CPY
      ******************************************************************
      *
      * DESCRIPTION: VCQFILE UPDATE LINK WORKERS
      *
      *=================================================================
      * REV:
      * JKC 2025-0433 ***NEW*** S35Q-200
      *
      ******************************************************************
       01  VCQUPD-LINK-BUF.
           03  VCQUPD-ACTION                         PIC X(1).
               88  VCQUPD-ACTION-CREATE                  VALUE "C".
               88  VCQUPD-ACTION-REWRITE                 VALUE "R".
               88  VCQUPD-ACTION-WRITE                   VALUE "W".
           03  VCQUPD-BRNO                           PIC 9(4).
           03  VCQUPD-CQ-REC                         PIC X(CQ-SIZE).
           03  VCQUPD-VCQ-REC                        PIC X(VCQ-SIZE).
           03  VCQUPD-IO-FG                          PIC 9(1).

