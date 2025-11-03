      * COPYMEMBER: LIBSP/SPVCQ2RN
      *=================================================================
      * REV:
      * SMR 2025-0318 ***NEW***
      * JKC 2025-0324 S35Q-191 MASSIVE FIX FOR COLLECTIONS; FOR USING
      *         A IDENTICAL A15 VISION STRUCTURE FOR THE VCQFILE; FIX
      *         HANDLING DUPLICATE CQ2-KEY WITHIN THE VCQFILE WHICH YOU
      *         CAN READ LIBSP/SP01VCQ FOR MORE DETAIL INFORMATION.
      * JKC 2025-0401 MOVED SPVCQ2IN TO BE SPVCQ2RN; REMOVED
      *         WRITE-VCQ2-FILE AND REWRITE-VCQ2-FILE.
      * JKC 2025-0424 S35Q-200 ADDED LOAD-VCQ2-KEY.
      *
      ******************************************************************
      *
      *    L O A D - V C Q 2 - K E Y
      *
      *=================================================================
      * IN  : CQ-REC
      * OUT : VCQ2-KEY
      * DESC: LOAD THE VCQ2-KEY BUT NOT ALL FIELDS.  DO NOT LOAD
      *       VCQ-MISC-KEY-FLD SINCE IT IS THE PRIMAY DEFINITION WHICH
      *       VCQ-REC-PDUE-DAYS IS THE REDEFINITION; LOOKING IN SSMS
      *       WE CAN SEE THAT CQ-MISC-KEY-FLD IS ALWAYS SPACES BUT
      *       CQ-REC-PDUE-DAYS IS ALWAYS A NUMBER; CQ-MISC-KEY-FLD
      *       IS NEVER USED IN A15 SO NOT SURE WHAT ITS PURPOSE IS
      *       OTHER THAN A POTENTIAL FUTURE NEED.
      *       VCQ-SEQ CANNOT BE LOADED HERE DUE TO UNIQUE SITUATIONS;
      *       NEED TO LOADED SPECIFICALLY WHEN NEEDED.
      ******************************************************************
       LOAD-VCQ2-KEY.

           MOVE CQ-COLLID             TO VCQ-COLLID.
           MOVE CQ-REV-BEHAVIOR-RISK  TO VCQ-REV-BEHAVIOR-RISK.
           MOVE CQ-SEQ-FG             TO VCQ-SEQ-FG.
           MOVE CQ-DATE               TO VCQ-DATE.
           MOVE CQ-START-TIME         TO VCQ-START-TIME.
           MOVE CQ-REV-DEL            TO VCQ-REV-DEL.
      *KC  MOVE CQ-MISC-KEY-FLD       TO VCQ-MISC-KEY-FLD.
           MOVE CQ-REV-PDUE-DAYS      TO VCQ-REV-PDUE-DAYS.
           MOVE CQ-REV-BAL            TO VCQ-REV-BAL.
      *KC  MOVE CQ-SEQ                TO VCQ-SEQ.

      *-----------------------------------------------------------------
       OPEN-VCQ2-FILE.
           PERFORM OPEN-IT.
           MOVE VCQ-PATH       TO E-FILE.
           OPEN INPUT VCQ-FILE.
           IF IO-FG = 8
             GO TO OPEN-VCQ2-FILE.
           IF IO-FG = 7
             CLOSE VCQ-FILE
             GO TO OPEN-VCQ2-FILE.
           UNLOCK VCQ-FILE.

       READ-VCQ2-FILE.
           PERFORM READ-IT.
           MOVE VCQ-PATH        TO E-FILE.
           MOVE VCQ2-KEY        TO E-KEYX.
           READ VCQ-FILE.
           IF IO-FG = 8
              GO TO READ-VCQ2-FILE.

       READ-VCQ2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE VCQ-PATH        TO E-FILE.
           MOVE VCQ2-KEY        TO E-KEYX.
           READ VCQ-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-VCQ2-FILE-NEXT.

       START-VCQ2-FILE.
           PERFORM START-IT.
           MOVE VCQ-PATH        TO E-FILE.
           MOVE VCQ2-KEY        TO E-KEYX.
           START VCQ-FILE KEY NOT < VCQ2-KEY.
           UNLOCK VCQ-FILE.

       START-VCQ2-FILE-GREATER.
           PERFORM START-IT.
           MOVE VCQ-PATH        TO E-FILE.
           MOVE VCQ2-KEY        TO E-KEYX.
           START VCQ-FILE KEY > VCQ2-KEY.
           UNLOCK VCQ-FILE.

       CLOSE-VCQ2-FILE.
           PERFORM CLOSE-IT.
           CLOSE VCQ-FILE.
