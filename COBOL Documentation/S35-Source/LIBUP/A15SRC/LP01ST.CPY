      * COPYMEMBER: LIBLP/LP01ST
      **************************************************
      *               STATE CODE FILE   
      *      TRAILER FILE = SUFILE LP01SU               
      * REV:
      * 081124 BAH ADDED ST-REINSTATE-WINDOW FOR REGACC PR# 228
      * 090127 BAH ADDED ST-REPO-RENOTICE FOR REGACC PR# 236
      * 120905 BAH ADDED ST-TITLE-RELEASE-TYPE FOR REGACC PR# 320
      * 140421 BAH ADDED ST-AUCTION-DATE-REQ FOR REGACC PR# 352
      **************************************************
       01  ST-REC.
           03  ST1-KEY.
               05  ST-CODE             PIC XX.
           03  ST-DESC                 PIC X(16).
           03  ST-REINSTATE-WINDOW     PIC X.
           03  ST-REPO-RENOTICE        PIC X.
           03  ST-TITLE-RELEASE-TYPE   PIC X.
           03  ST-AUCTION-DATE-REQ     PIC X.
           03  FILLER                  PIC X.
