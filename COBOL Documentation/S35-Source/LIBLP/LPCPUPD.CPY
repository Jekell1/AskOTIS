      * COPYMEMBER: LIBLP/LPCPUPD
      **************************************************************************
      *    LIBLP/LPCPUPD:  EXTRACTION UPDATE
      *    COPIED BY:  LP/CPRUN (AUTO UPDATE) &
      *                LP/CPUPD (NO AUTO UPDATE)
      *    EXTRACTION UPDATE SECTION
      * REV:
      * BLV 000817 UPDATE CQ-START-TIME FOR TIME ZONE, TFC PR# 25
      * BLV 000908 UPDATE CQ-TIME-ZONE-ERROR, TFC PR# 25
      * BLV 112800 ADD UPDATE OF GQ-FILE FOR GLOBAL COLLECTION
      *            QUEUING, TFC PR# 60
      * BAH 020520 ADDED TEST OF FD-SMARTPORT = "E" FOR CREATING ALL FORM
      *            REQUESTS IN THE CORPORATE BRNO AND THEN THEY WILL PRINT
      *            THEM INTO AN ASCII FILE EXTRACT, REGACC PR# 1800
      * BLV 040616 UPDATE AW-CQ-BEHAVIOR-RISK TO CQ-REV-BEHAVIOR-RISK,
      *            REGACC PR#59
      * BAH 040813 IF ANY COMAKER IS FLAGGED CONFIDENTIAL, DONT CREATE A
      *            FORM REQUEST, PR# 2343
      * BLF 070906 IF BR-COLL-QUEUE-CON-REC = "N", MOVE 0 TO CQ- OR GQ-REV-DEL,
      *            SO NO DELINQUENCY IN QUEUE ORDERING, PCCREDIT PR# 3054
      * BAH 091002 ADDED FD-SKIP-BAD-ADR TO TEST PRIMARY MAKER ONLY IF BAD
      *            ADDRESS FLAG SET, DO NOT CREATE FORM REQUEST REGACC PR# 256
      *  CS 110228 ADD FD-SKIP-BAD-ADR = 'M', IF 'M' DONT GENERATE ANY FORM
      *            REQUEST IF ACCOUNT IS IN CEASE & DESIST STATUS (MUST LOOP
      *            THRU LN-STATUSFLAGS FOR A 'C' IN ANY OF THE 10 POSITIONS
      *                                               MODERN PR#3720
      *  CS 110705 MOVE AW-EA-DESC TO LN-LPO MODERN PR#3821
      * BAH 160216 EXPANDED AW, PD0003
      * BLM 160915 FIX CQ-DATE, GQ-DATE, PD0003
      * BLM 160915 CHANGED CQ2-KEY & GQ2-KEY TO BE UNIQUE WITH SEQ, SO
      *            FIXED WRITE TO CHECK FOR DUPLICATE 22'S
      *
      * KEC 2016.1005 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED SETTING CQ-BRNO.
      *
      * KEC 2017.0118 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED FD-SMARTPORT TO FD-OPTION [88 LEVELS].
      *
      *  BAH 170525 CHANGE EXTRDT TO EXTRDATE TO RECOGNIZE ITS A DATE
      *  BAH 170525 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181112 GLOBAL FMFILE
      *   CS 190214 REMOVE CQ-TYPE
      *   CS 190301 REMOVED FILE AWFILE, ITS NOW A COPY (LIBLP/LPCPAW)
      *             ALL UPDATES WILL BE AUTOMATIC
      * BAH 190311 MOVED FM-BRNO2 INTO LPWSFM-EDIT-FM-INITKEY TO REMOVE THE
      *            REDEFINE IN THE FM-REC
      *  CS 190314 SEPARATE CQ INTO CQSFILE(SUMMARY) CQFILE(DETAIL)
      *            GGSFILE(SUMMARY), GQFILE(DETAIL)
      *  CS 190603 IN UPDATE-COLLECTOR-QUEUE, SET FUTURE FIELDS TO 0
      *             UPDATE-GLOBAL-COLLECTOR-QUEUE, CLEAR FUTURE FIELDS
      *  CS 190605 ZERO FUTURE-NUMERICS
      * BAH 20190614 ADDED CLEAR-FM-FILE (LPFMCL) AND SET FM-USERID
      * BAH 20190627 FIXED CLEAR OF FUTURE-NUMERICS
      * BAH 20190723 ADDED CLEAR-GQ-FILE SPGLCL
      * BAH 190918 REMOVED ACCESS-CALL ON FMFILE
      * BAH 20200709 CHANGED ALL OPENS TO LOADS, MOVE OPENS TO CPRUN/CQRUN 
      *               #TST001
      * BAH 2024.0201 REMOVED CREATE NOTICE FOR A FORM
      * JKC 2025.0324 S35Q-191 MASSIVE FIX FOR COLLECTIONS; FOR USING
      *         A IDENTICAL A15 VISION STRUCTURE FOR THE VCQFILE; FIX
      *         HANDLING DUPLICATE CQ2-KEY WITHIN THE VCQFILE WHICH YOU
      *         CAN READ LIBSP/SP01VCQ FOR MORE DETAIL INFORMATION.
      ******************************************************************
       EXTRACTION-UPDATE SECTION.

      ****************************************
      *    UPDATE LOAN RECORD:
      ****************************************

      * LN-PROMDATE:
           IF AW-INCPROM NOT = 0
              MOVE AW-TODAYDUE-DATE TO NDTE-DATE
              MOVE AW-INCPROM       TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE TO LN-PROMDATE
              IF BR-SECOND-PROMISE = "Y"
                 IF LN-PROMDATE2 NOT = 0
                    IF LN-PROMDATE > LN-PROMDATE2
                        MOVE LN-PROMDATE2 TO LN-PROMDATE
                        MOVE 0 TO LN-PROMDATE2.

      * LN-DELCNT:
           IF AW-INCCNT = "Y"
              ADD 1 TO LN-DELCNT.

      * LN-ACTIONCD:
           IF AW-SETACTCD NOT = "===="
              MOVE AW-SETACTCD TO TEST-ACTION-TABLE1
              MOVE LN-ACTIONCD TO TEST-ACTION-TABLE2
              MOVE 4 TO SUB
              PERFORM VERIFY-AW1-ACTION
              MOVE TEST-ACTION-TABLE2 TO LN-ACTIONCD.


      * LN-COLLECTOR:
           IF REPLACE-COL-FG = "N"
              IF LN-COLLECTOR NOT = "   "
                 GO TO CHECK-SET-FMNO.
           IF AW-SET-LN-COL = "Y" OR AW-SET-LN-COL = "P"
              IF AW-COLCTID NOT = "==="
                 MOVE AW-COLCTID TO TEST-ACTION-TABLE1
                 MOVE LN-COLLECTOR TO TEST-ACTION-TABLE2
                 MOVE 3 TO SUB
                 PERFORM VERIFY-AW1-ACTION
                 MOVE TEST-ACTION-TABLE2 TO LN-COLLECTOR.

      ***********************************
      *    UPDATE NOTICE FORM REQUESTS:
      ***********************************
       CHECK-SET-FMNO.
           IF AW-SETFMNO = 0 AND AW-SETFM(1) = 0 AND
               AW-SETFM(2) = 0 AND AW-SETFM(3) = 0
              GO TO CHECK-COL-QUEUE.

           IF AW-SETFMNO NOT = 0
              MOVE 1 TO SUB
              MOVE AW-SETFMNO TO WK-FORMNO
              PERFORM UPDATE-NOTICE-FORM-REQ.

           IF AW-SETFM(1) = 0 AND AW-SETFM(2) = 0 AND AW-SETFM(3) = 0
              GO TO CHECK-COL-QUEUE.

           MOVE 2 TO SUB.
           MOVE 1 TO SUB1.
       NEXT-FORM-UPDATE.
           IF SUB > 4
              GO TO CHECK-COL-QUEUE.

           IF AW-SETFM(SUB1) NOT = 0
              MOVE AW-SETFM(SUB1) TO WK-FORMNO
              PERFORM UPDATE-NOTICE-FORM-REQ.

           ADD 1 TO SUB SUB1.
           GO TO NEXT-FORM-UPDATE.

       CHECK-COL-QUEUE.
           IF BR-COLLECTOR-QUEUE = "Y"
              IF AW-CQ-FG = "Y"
                 IF GLOBAL-QUEUING-FG = "Y"
                    PERFORM UPDATE-GLOBAL-COLLECTOR-QUEUE
                 ELSE
                    PERFORM UPDATE-COLLECTOR-QUEUE.

       EXIT-EXTRACTION-UPDATE.
           EXIT.

      *************************************************
       UPDATE-NOTICE-FORM-REQ SECTION.
      *************************************************
      * TEST FOR NOTICE RECORDS:
 
      * OPEN IS IN THE DRIVER PROGRAM CPRUN/CQRUN
           PERFORM LOAD-FD1-FILE.

           MOVE WK-FORMNO TO FD-FMNO.
           PERFORM READ-FD1-FILE.
           IF IO-FG NOT = 0
              GO TO UPDATE-NOTICE-FORM-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO RESET FM-PATH-OVERIDE BECAUSE HOLD-SMARTPORT MAY
      *    CHANGE MULTIPLE TIMES.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                     TO FM-PATH-OVERRIDE.
           IF ( FD-OPTION-EXTRACTION )
              PERFORM GET-FM-PATH.

      * OPEN IS IN THE DRIVER PROGRAM CPRUN/CQRUN
           PERFORM LOAD-FM1-FILE.

      * IF COLLECTION TYPE FORM, DELETE ALL OTHERS:
           IF FD-FMTYPE = "C" AND FIRST-TIME = "Y"
              IF ( NOT FD-OPTION-EXTRACTION )
                 PERFORM FORM-DELETE-ROUTINE
                 MOVE "N" TO FIRST-TIME.

           PERFORM GET-BORROWER.
           IF IO-FG = 0
              IF BW-CONFIDENTIAL = "Y"
                 GO TO UPDATE-NOTICE-FORM-EXIT
              ELSE
                 IF SUB = 1 AND
                    FD-SKIP-BAD-ADR = "Y" AND BW-BAD-ADR-FG = "Y"
                  GO TO UPDATE-NOTICE-FORM-EXIT.

           PERFORM CLEAR-FM-FILE.

           MOVE SPACES TO LPWSFM-EDIT-FM-INITKEY.

           MOVE AW-ACCTNO   TO LPWSFM-EDIT-FM-ACCTNO.
           MOVE WK-FORMNO   TO LPWSFM-EDIT-FM-FORMNO.
           IF ( FD-OPTION-EXTRACTION )
              MOVE LN-OWNBR TO LPWSFM-EDIT-FM-BRNO2.

      * FM-PATH-OWNBR WILL BE CORRECT FOR CORPORATE AND/OR BRANCH
      * IF CORPORATE, IT READ GP-CORP-BRNO AND HAS THAT FMFILE PATH SET
      * SO A BRANCH 1 ACCOUNT WITH 2 IN THE GP-CORP-BRNO WILL HAVE
      * FM-BRNO = 2 (CORP) AND FM-BRNO2 = 1 (OWNING BRANCH)

           MOVE FM-PATH-OWNBR          TO FM-BRNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           MOVE WK-FORMNO              TO FM-FMNO.

           MOVE FM1-KEY             TO HOLD-FM1-KEY.
           PERFORM READ-FM1-FILE.

           MOVE BW-ZIP             TO FM-ZIP.
           MOVE SPACES             TO FM-PRINT.
           MOVE 0                  TO FM-STMT-COUNTER.
           MOVE EXTRDATE           TO FM-TRDATE.
           MOVE EXT-CONAME-USER-ID TO FM-USERID.
           IF IO-FG = 0
              PERFORM REWRITE-FM1-FILE
           ELSE
              MOVE HOLD-FM1-KEY TO FM1-KEY
              PERFORM WRITE-FM1-FILE.

       UPDATE-NOTICE-FORM-EXIT.
           EXIT.

      ****************************************
      *    DELETE COLLECTION FORM REQUESTS:
      ****************************************
       FORM-DELETE-ROUTINE SECTION.

           MOVE FM-PATH-OWNBR TO FM-BRNO.

           MOVE SPACES             TO LPWSFM-EDIT-FM-INITKEY.
           MOVE AW-ACCTNO          TO LPWSFM-EDIT-FM-ACCTNO.
           MOVE 0                  TO LPWSFM-EDIT-FM-FORMNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.

           MOVE 0             TO FM-FMNO.

           PERFORM START-FM1-FILE.
       FORM-FM1-DELETE.
           PERFORM READ-FM1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (FM-BRNO NOT = FM-PATH-OWNBR)
              GO TO FORM-DELETE-ROUTINE-EXIT.
      *    UNLOCK FM-FILE.

           MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY.
           IF LPWSFM-EDIT-FM-ACCTNO NOT = AW-ACCTNO
              GO TO FORM-DELETE-ROUTINE-EXIT.

           MOVE FM-FMNO TO FD-FMNO.
           PERFORM READ-FD1-FILE.
           IF IO-FG NOT = 0
              GO TO FORM-FM1-DELETE.

           IF FD-FMTYPE NOT = "C"
              GO TO FORM-FM1-DELETE.

           PERFORM READ-FM1-FILE.
           PERFORM DELETE-FM1-FILE.

           GO TO FORM-FM1-DELETE.

       FORM-DELETE-ROUTINE-EXIT.
           EXIT.

      * IF FORM IS AN "E" EXTRACTION TYPE FORM (FD-SMARTPORT), THEN
      * PUT THE CORPORATE BRNO PATH IN THE FM-PATH, SO ALL FORM REQUESTS
      * ARE CREATED IN THE HOME OFFICE, THEN WHEN THEY GO "PRINT" THEM ALL
      * IT WILL CREATE AN ASCII FILE
      ************************
       GET-FM-PATH SECTION.
      ************************

           IF GP-CORP-BRNO = 0
              GO TO EXIT-GET-FM-PATH.

           MOVE BR-NO TO HOLD-BR-KEY.
      * BR-FILE IS OPEN FROM GROUP LOGIC
           MOVE GP-CORP-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO END-GET-FM-PATH.

           MOVE EXT-FILPATH-BASE           TO FM-ALL-BASE.
           MOVE BR-MACHINE                 TO FM-ALL-MACHINE.
           MOVE BR-DATA-PATH               TO FM-ALL-DATA.
           MOVE FM-ALL-PATH                TO FM-PATH.
           MOVE "Y"                        TO FM-PATH-OVERRIDE.

       END-GET-FM-PATH.
      * GET ORIGINAL BRANCH BACK
           MOVE HOLD-BR-KEY TO BR-NO.
           PERFORM START-BR-FILE.
           PERFORM READ-BR-FILE-NEXT.

       EXIT-GET-FM-PATH.
           EXIT.

      **********************************
      *    UPDATE COLLECTOR QUEUE
      **********************************
       UPDATE-COLLECTOR-QUEUE SECTION.
           IF AW-CQ-COL-ID = "==="
              GO TO EXIT-UPDATE-COLLECTOR-QUEUE.

           MOVE "N" TO CQ1-EXISTS.

           PERFORM CLEAR-CQ-FILE.

           MOVE BR-NO     TO CQ-BRNO.
           MOVE AW-ACCTNO TO CQ-ACCTNO.
           PERFORM READ-CQ1-FILE.
           IF IO-FG = 0
              GO TO EXIT-UPDATE-COLLECTOR-QUEUE.

           MOVE AW-CQ-COL-ID           TO CQ-COLLID.
           IF BR-COL-QUEUE-BEHAVIOR-FG = "Y"
              MOVE AW-CQ-BEHAVIOR-RISK TO CQ-REV-BEHAVIOR-RISK
           ELSE
              MOVE 0                   TO CQ-REV-BEHAVIOR-RISK.
           MOVE EXTRDATE               TO CQ-DATE CQ-DATE-QUEUED.
           IF AW-CQ-SEQ-FG = "B"
              MOVE LN-PROMDATE         TO CQ-DATE.
           MOVE 0                      TO CQ-START-TIME.
           MOVE AW-CQ-SEQ-FG           TO CQ-SEQ-FG.
           IF BR-COLL-QUEUE-CON-REC = "N"
              MOVE 0                   TO CQ-REV-DEL
           ELSE
              MOVE AW-REV-DEL          TO CQ-REV-DEL.
           MOVE ZEROES                 TO CQ-SEQ.
           MOVE AW-REV-PDUE-DAYS       TO CQ-REV-PDUE-DAYS.
           COMPUTE CQ-REV-BAL = 999999 - (LN-CURBAL - LN-OT2BAL).
           MOVE AW-START-TIME          TO CQ-START-TIME.
           MOVE AW-TIME-ZONE-ERROR     TO CQ-TIME-ZONE-ERROR.

           PERFORM WRITE-CQ1-FILE.

       EXIT-UPDATE-COLLECTOR-QUEUE.
           EXIT.

      **********************************
      * UPDATE GLOBAL COLLECTOR QUEUE
      **********************************
       UPDATE-GLOBAL-COLLECTOR-QUEUE SECTION.
           IF AW-CQ-COL-ID = "==="
              GO TO EXIT-UPDATE-GLOBAL-COLL-QUEUE.

           MOVE "N" TO GQ1-EXISTS.

           PERFORM CLEAR-GQ-FILE.

           MOVE AW-ACCTNO  TO GQ-ACCTNO.
           MOVE AW-LN-BRNO TO GQ-BRNO.
           PERFORM READ-GQ1-FILE.
           IF IO-FG = 0
              GO TO EXIT-UPDATE-GLOBAL-COLL-QUEUE.

           MOVE AW-CQ-COL-ID   TO GQ-COLLID.
           MOVE EXTRDATE       TO GQ-DATE GQ-DATE-QUEUED.
           IF AW-CQ-SEQ-FG = "B"
              MOVE LN-PROMDATE TO GQ-DATE.
           MOVE 0              TO GQ-START-TIME.
           MOVE AW-CQ-SEQ-FG   TO GQ-SEQ-FG.
           IF BR-COLL-QUEUE-CON-REC = "N"
              MOVE 0 TO GQ-REV-DEL
           ELSE
              MOVE AW-REV-DEL TO GQ-REV-DEL.
           MOVE AW-REV-PDUE-DAYS TO GQ-REV-PDUE-DAYS.
           COMPUTE GQ-REV-BAL = 999999 - (LN-CURBAL - LN-OT2BAL).
           MOVE AW-START-TIME TO GQ-START-TIME.
           MOVE AW-TIME-ZONE-ERROR TO GQ-TIME-ZONE-ERROR.
           MOVE 0 TO SAVE-GQ-SEQ.
       WRITE-GQ-REC.
           MOVE SAVE-GQ-SEQ TO GQ-SEQ.
           PERFORM WRITE-GQ1-FILE.
           IF IO-FG NOT = 0
              IF E-CODE = "22"
                 IF SAVE-GQ-SEQ < 999
                    ADD 1 TO SAVE-GQ-SEQ
                    GO TO WRITE-GQ-REC.
       EXIT-UPDATE-GLOBAL-COLL-QUEUE.
           EXIT.

      **********************************
      *    VERIFY ACTION CODES:
      **********************************
       VERIFY-AW1-ACTION SECTION.
           IF TEST-ACTION-TAB1(SUB) NOT = "="
              MOVE TEST-ACTION-TAB1(SUB) TO TEST-ACTION-TAB2(SUB).
           SUBTRACT 1 FROM SUB.
           IF SUB > 0
              GO TO VERIFY-AW1-ACTION.
