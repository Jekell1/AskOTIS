      * COPYMEMBER: LIBLP/CHECKTRANS
      ******************************************************************
      *                   (LIBLP/CHECKTRANS)
      *          UPDATE FILE LOGIC FOR CHECK PRINT PROGRAMS
      *          10/00 ORIGINALLY CKPRINTUPD
      *                USE WITH LIBLP/CHECKTRANSW
      *
      *    USED IN POSTING PROGRAMS:
      *     LONPI0.C      CASH DRAWER RECEIPTS
      *     1ONPI0.C      CASH DISBURSEMENTS
      *     2ONPI0.C      CHECK WRITING
      *     3ONPI0.C      VOID CHECKS
      *     4ONPI0.C      BANK DEPOSITS
      *     5ONPI0.C      BANK WITHDRAWALS
      *     6ONPI0.C      BATCH CHECK PRINT
      *    USED IN CHECK WRITING PROGRAMS:
      *     LONP00.C 070299  NEW GENERIC; REGMGM, SUNBELT;BOTTOM VOUCHER
      *     LONP0A.C 021899  GREATER SUBURBAN
      *     LONP2M.C 021391  MULLEN; SAME AS M2 EXCEPT PRINTS G/L INFO
      *     LONP2Y.C 120496  ACTION, MERCURY(IL), MERCURY (MIDLAND)
      *     LONP2Z.C 062596  NOT VALID ANYMORE (MERCURY MIDLAND)
      *     LONP3T.C 112696  WORLD FINANCE?;SAME AS M2 EXCEPT PRINTS G/L
      *     LONP6J.C 110894  MID-ATLANTIC FIN, ST. PETE/CLEARWATER
      *     LONP6M.C 102097  FREEDOM FIN, PREMIER FIN
      *     LONP6T.C 052699  CHALLENGE FIN, CENTRIX FIN; LASER
      *     LONP72.C 040798  FUTURE FIN
      *     LONP73.C 042790  FAMILY, REGMGM, LENDMARK
      *     LONP8H.C 022597  NEWPORT ACCEPTANCE, CA
      *     LONPM2.C 111584  STANDARD CHECK PRINT
      * REV:
      *  990714 BAH IF BR-ALLOW-SYS-AS-EFFDATE = "Y", MOVE CX-DATE TO CK-DATE
      *             REGMGM PR# 867
      *  GLB  000727 MODIFY/ADD ROUTINES; USE CHECK-PAYEE NOT TEST-PAYEE
      *  GLB  010703 BUG NOT FINDING ORIGINAL DT-REC WHEN REVERSING ON
      *              VOID; FIX 88 LEVEL TEST FOR DEALER-ORIGINAL
      *  GLB  010807 TEST IF BATCH CHECK REQUEST WHEN DELETING CX-RECORD
      *              CANT MATCH TO LN-ACCTNO
      *  GLB 010927  CREATE DS-RECORD IF MISSING (I.E., VX 1ST TRANS)
      *  GLB 011119  SWITCHED ORDER OF UPDATE-CA/UPDATE-TC;UPDATE-TC
      *              REVERSES SIGN ON CX-AMT CAUSING CASH DRAWER PROBLEM
      *  BAH 020220 ADDED ANOTHER IF IN UPDATE-CX1-FILE BEFORE TESTING
      *             IF NOT "B" AND CX-REF = LN-ACCTNO, LN-ACCTNO WAS NEVER
      *             INITIALIZED, CAUSING NON-NUMERIC IN ACU_TCLP
      *             IF YOUR DOING A MANUAL CHECK, SCANNING FROM SYSTEM, OR
      *             BUILDING FROM SCRATCH, THE CX DOES NOT NEED TO GET
      *             DELETED HERE !!!!
      * GLB 020710 YIKES! THIS CERTAINLY WONT WORK FOR MULTIPLE
      *             DEALERS ON ONE CHECK; 10.01, 10.02, 10.03, ETC
      *             IF DT-DLNO NOT = CX-DLNO
      * GLB 030321 WORLD PR# 320; CHECK UPDATES/VOIDS TO INCLUDE NEW
      *            PAYEE TYPE OF "DD" TO WRITE A CHECK AND AUTOMATICALLY
      *             UPDATE THE CASH DRAWER AND CASH ON HAND
      * GLB 030321 BUG IF MANUAL/MISC CHECKS & BR-ALLOW-SYS-AS-EFFDATE
      *             IS SET TO "Y."  CX-DATE WILL BE ZERO.
      *             A60 WORKS - ALWAYS USED TRANS-DATE
      * GLB 030325  CASH DRAWER UPDATE WORKED BY ACCIDENT USING "TILLNO"
      *             BECAUSE IT WAS EMBEDDED IN LONPI0, 1ONPI0;
      *             NEED TO USE CX-TILLNO TO WORK WITH NEW CHECK TYPE
      *             DRAWER DEPOSIT
      * GLB 030326  VOID USES CK-REC; HAD TO ADD "CK-TILLNO" TO VOID
      *             CHECKS WITH NEW PAYEE TYPE OF DRAWER DEPOSIT "DD"
      * BAH 040114 REMOVED OPEN-CX1-FILE AND CLOSE-CX1-FILE
      *            2 PEOPLE WERE ABLE TO PRINT THE SAME CHECK.
      *            1 WAS IN DOC PRINT, ENTER NEXT CHECK, WALKED AWAY TO
      *            ANOTHER SCREEN, WENT TO EXACT SAME PLACE, BUT ENTERED
      *            THE CHECK# AND SHOULD HAVE GOTTEN CX LOCKED, BUT DIDNT,
      *            IT ALLOWED IT TO PRINT, THEN WENT BACK TO OTHER TERMINAL
      *            AND WAS FORCED TO ENTER A CHECK # AGAIN, AND IT PRINTED
      *            AGAIN. SO, CX1 FILE NEEDS TO STAY LOCKED, DO NOT CLOSE
      * BLV 040527 CHANGED UPDATE-TY TO SUBTRACT CX-AMT INSTEAD OF ADD
      *            FOR RR, RP, RC, & RD
      * GLB 040715 CHANGED REVERSE-DT-PROCEEDS, WAS SKIPPING SERVICE
      *            ACCTS BECAUSE THEY WERE NOT NW, PUT OR TX, SO DON'T
      *            CHECK TRANS TYPE IF SERVICE ACCOUNTS SO THEY WILL
      *            GET VOIDED, MAS, PL# 433
      * BLF 050323 CHANGED SETUP-REPLACE-CHECK TO MOVE CX-CKPAID-NEW
      *            TO CX-PAID IF IT'S M OR S, TO SHOW THE PAID TYPE
      *            OF A REPLACEMENT CHECK, MULL PL # 470
      *  CS 060607 ADD UPDATE OF CX-EVENT-CD TO DT-EVENT-CD SECURITY AUTO
      *                                                   PR#2503
      *  CS 060626 UPDATE CX-EVENT-CD TO CK-EVENT-CD PR#2503 SECURITY
      * BLF 060713 UPDATE CX-PAYEE-NO TO CK-PAYEE-NO, DEVELOPMENT PR#2633
      * BLF 060718 UPDATE UPDATE CHECK AMT TO PAYEE YTD AMT. PAID &
      *            UPDATE PAYEE LAST CHECK INFO, DEVELOPMENT PR#2633
      * BAH 060824 UPDATE CK-ADR1-PARTIAL FOR EXTENDED PAYEE NAME
      *            ONLY CAPTURING 10 BYTES OF IT, LENDMARK PR# 2809
      *            ALSO ADDED TC-ADR1 TO STORE THE FULL 25 BYTES
      * BLF 071015 UPDATE CX-PAYEE-NO TO TC-PAYEE-NO, HOME PR# 3078
      * BLF 071214 DO NOT PERFORM CREATE-DT-HISTORY FOR DP CHECK
      *            REQUEST CREATED IN PARTICIPATION HELD EXTRACTION
      *            (SP/PAREXT, CX-PGM = "PA").  SO THESE GUYS WILL
      *            NOT GET A SUPPLEMENTAL DT CHECK RECORD; THE
      *            CHECK DATE & NUMBER WILL BE UPDATED TO THE
      *            DT'S CREATED FOR EACH ACCOUNT IN THE EXTRACTION.
      *            REGIONAL PR# 185
      * BLF 071218 FIXED PREVIOUS CHANGE, CHECK WAS IN WRONG SPOT,
      *            WOULD DO NO HARM HUT INCORRECT FOR NEW PARITICPATION
      *            STUFF, REGACC PR# 185
      *  CS 120215 ADDED NEW PAYEE TYPE OF 'ID'[INTERIM DEPOSIT] WHICH POPULATES
      *            THE GL DISTRIBUTION WITH BR-GLONHAND # & UPDATES THE CASH
      *            DRAWER SO THE USER DOES NOT HAVE TO PERFORM A CASH
      *            WITHDRAWL TRANSACTION. ONLY IF GP-I-AM-WORLD, THE CURSOR
      *            STOPS ON PAYEE TYPE, CAN ENTER 'BK' OR 'ID' WORLD PR#790
      *            [SEE COMMENTS IN 4ONPI0]
      *            ADDED NEW PAYEE TYPE OF 'IW' [INTERIM WITHDRAWL] WHICH
      *            POPULATES GL DISTRIBUTION WITH BR-GLONHAND & PUTS THE
      *            CASH BACK IN THE DRAWER, USE TO OFFSET A 'ID' WORLD PR#790
      *  CS 120222 NEW PAYEE TYPES 'ID' & 'IW' CREATED CK RECORD WHICH IT
      *            SHOULDNT HAVE & ALSO DID NOT DELETE THE 'CX' PR#790
      * BLM 160224 FIXED LOGIC FOR NEW TY DATE FORMAT
      * BAH 160308 EXPANDED CAFILE
      * BAH 160317 EXPANDED CKFILE, REMOVED CK-ADR1-PARTIAL AND ADDED 
      *            CK-ADR1, CK-ADR2, CK-CITY, CK-STATE, CK-ZIP
      * BLM 160511 FIXED LOGIC FOR NEW DS DATE FORMAT, PD0003
      * BAH 160615 ADDED DT-BRNO TO DT1-KEY
      * BAH 160818 ADDED CK-FREE1 AND CK-FREE2
      *
      * KEC 2017.0103 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      * BAH 170313 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181029 GLOBAL DTFILE
      * BAH 190312 MOVED EDIT-FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY 
      * BAH 201123 HARD COPIES OF "BK", "CA", "ID" OR "IW" WERE NOT
      *            DELETING TEMP CX REC, THE CX-REF AND LN-ACCTNO ARE
      *            USED AND THE NEW LN-ACCTNO OF 8 DIGITS HOLDS A
      *            REFERENCE SEQUENCE AND IT WAS TOO LONG, ALSO HAD THE
      *            PAYEE SUB ON THE END OF IT. MADE A WORKING STORAGE
      *            FIELD TO ONLY TEST THE FIRST 6, TEST-ACCTNO,
      *            TEST-REF-ACCTNO
      *            CX-REF = 99001 LN-ACCTNO = 99000118 - HAS PAYEE-SUB
      *            ON THE END, TOO LONG
      * BAH 20210305 SET TC-DEBIT-REFCD FOR CX-TYPE "DB" #1490
      * BAH 20211018 IN A15, IT READS THE CX WITH NOTHING IN THE KEY AND
      *              GOES TO EXIT FOR IO-FG 9 DUE TO MANUAL CHECK NOT
      *              CREATING A CX RECORD, TEST LPWSFM-EDIT-FM-TYPE OF SPACES
      *              TO GO TO EXIT, DONT TRY TO READ CX. DATABASE WILL BOMB
      * BAH 20220803 HARD CODED START
      * BAH 20230908 REMOVED PA-FILE
      ******************************************************************
       UPDATE-FILES-CHECKTRANS SECTION.

           MOVE CX-TYPE TO CHECK-PAYEE.
           MOVE CX-TYPE TO HOLD-DT-TRCD.
           MOVE CX-PAID TO HOLD-CX-PAID.

      * -----------------------------------
      *  C A S H    &    B A N K
      * -----------------------------------
           IF CX-TYPE = "CA" OR "BK" OR "IW" OR "ID"
              GO TO EXIT-UPDATE-FILES-CKPRINT.

      * -----------------------------------
      * D E A L E R   R S V   R E C E I P T S
      * CX-TYPES: RR, RC, RP, RD
      * -----------------------------------
           IF (DEALER-RSV-REC)
              OR (DEALER-PROC-REC)
      * GLB 11/19/01 SWITCHED ORDER OF UPDATE-CA/UPDATE-TC
      * UPDATE-TC REVERSES SIGN ON CX-AMT CAUSING CASH DRAWER PROBLEM
                 PERFORM UPDATE-CA-FILE
                 PERFORM UPDATE-TC-FILE
                 PERFORM UPDATE-TY-FILE
                 PERFORM UPDATE-DT-NOBATCH
                 PERFORM UPDATE-DS-RESERVE
                 MOVE CX-TYPE TO HOLD-DT-TRCD
                 PERFORM CREATE-DT-HISTORY
                 GO TO EXIT-UPDATE-FILES-CKPRINT.

      * -----------------------------------
      *       V   O   I   D   S
      * -----------------------------------
           IF HOLD-CX-PAID = "V"
              PERFORM VOID-CHECK-MASTER
              PERFORM UPDATE-TC-FILE
      * DEALER PROCEEDS CHECKS "DL" "VX"
              IF (DEALER-PROCEEDS)
                 PERFORM REVERSE-DT-PROCEEDS
                 MOVE "V" TO HOLD-DT-TRCD
                 PERFORM CREATE-DT-HISTORY
      * REGULAR VOID
                 IF CX-CKNO-NEW = 0
                    PERFORM UPDATE-DS-UNPAID-CHECK
                    PERFORM UPDATE-TY-FILE
                 END-IF
      * REPRINT/REISSUE VOID
                 IF CX-CKNO-NEW NOT = 0
                    PERFORM SETUP-REPLACE-CHECK
                    PERFORM UPDATE-CK-FILE
                    PERFORM UPDATE-TC-FILE
                    MOVE "CK" TO HOLD-DT-TRCD
                    PERFORM CREATE-DT-HISTORY
                 END-IF
                 GO TO EXIT-UPDATE-FILES-CKPRINT
              ELSE
      * DEALER RESERVE CHECK "DR" "DC" "DP"
              IF (DEALER-RSV)
                 MOVE CX-TYPE TO HOLD-DT-TRCD
                 PERFORM CREATE-DT-HISTORY
      * REGULAR VOID
                 IF CX-CKNO-NEW = 0
                    PERFORM UPDATE-DS-RESERVE
                    PERFORM UPDATE-TY-FILE
                    PERFORM UPDATE-DT-NOBATCH
                 END-IF
      * REPRINT/REISSUE VOID
                 IF CX-CKNO-NEW NOT = 0
                    PERFORM SETUP-REPLACE-CHECK
                    PERFORM UPDATE-CK-FILE
                    PERFORM UPDATE-TC-FILE
                    MOVE CX-TYPE TO HOLD-DT-TRCD
                    PERFORM CREATE-DT-HISTORY
                 END-IF
                 GO TO EXIT-UPDATE-FILES-CKPRINT
              ELSE
      * ALL OTHER CHECKS "MS" 
              IF CX-CKNO-NEW = 0
                 PERFORM UPDATE-TY-FILE
                 IF (DRAWCK)
                    PERFORM UPDATE-CA-FILE
                 END-IF
              END-IF
      * REPRINT/REISSUE VOID
              IF CX-CKNO-NEW NOT = 0
                 PERFORM SETUP-REPLACE-CHECK
                 PERFORM UPDATE-CK-FILE
                 PERFORM UPDATE-TC-FILE
              END-IF
              GO TO EXIT-UPDATE-FILES-CKPRINT.

      * -----------------------------------
      *     N E W    C H E C K S
      * -----------------------------------
           IF (DRAWCK)
              PERFORM UPDATE-CA-FILE.

           PERFORM UPDATE-CK-FILE.
           PERFORM UPDATE-TC-FILE.
           PERFORM UPDATE-TY-FILE.

           IF (NOT DEALER)
              GO TO EXIT-UPDATE-FILES-CKPRINT.

           IF (DEALER-INPUT)
              PERFORM RELEASE-NW-PROCEEDS.

           IF (DEALER-PROCEEDS)
              MOVE "CK" TO HOLD-DT-TRCD
              PERFORM UPDATE-DS-UNPAID-CHECK.

           IF (DEALER-RSV)
              MOVE CX-TYPE TO HOLD-DT-TRCD
              PERFORM UPDATE-DS-RESERVE.

           IF CX-BATCHID = 0
              PERFORM UPDATE-DT-NOBATCH
           ELSE
              PERFORM UPDATE-DT-BATCH.

           IF CX-PGM NOT = "PA"
              PERFORM CREATE-DT-HISTORY.

       EXIT-UPDATE-FILES-CKPRINT.
           PERFORM UPDATE-CX1-FILE.

      ******************************************
      *    UPDATE CHECK TRANSACTION FILE
      ******************************************
       UPDATE-TC-FILE SECTION.
           MOVE "UPDATING CHECK TRANSACTION............" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-TRC1-FILE.

           MOVE 1 TO HOLD-SEQ.
       UPDATE-TC-FILE-NEXT.
           PERFORM CLEAR-TRC-FILE.

           MOVE CX-BRNO    TO TC-BRNO.
           MOVE TRANS-DATE TO TC-KEY-DATE.
           MOVE CX-CKNO    TO TC-CKNO.
           MOVE CX-CLASS   TO TC-CLASS.
           MOVE HOLD-SEQ   TO TC-SEQ.

           MOVE CX-TYPE    TO TC-TYPE.
           MOVE CX-DLNO    TO TC-DLNO.
           IF TC-TYPE = "VX"
              MOVE "DL"    TO TC-TYPE.
           MOVE CX-DLRVNO  TO TC-DLRVNO.
           MOVE TRANS-DATE TO TC-DATE.

           MOVE CX-AMT TO TC-AMT.
           MOVE CX-PAYEE  TO TC-PAYEE.
           MOVE CX-ADR1   TO TC-ADR1.
           MOVE CX-REF    TO TC-REF.
           MOVE CX-REASON TO TC-REASON.
           MOVE CX-PGM    TO TC-PGM.
           MOVE CX-VOID   TO TC-VOID.
           MOVE CX-PAID   TO TC-PAID.
           MOVE CX-ACCTNO TO TC-ACCTNO.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              MOVE CX-GLNO(SUB)  TO TC-GLNO(SUB)
              MOVE CX-GLAMT(SUB) TO TC-GLAMT(SUB)
           END-PERFORM.

           PERFORM SET-POSTDATE.
           MOVE POST-USERID TO TC-USERID.
           MOVE POST-TIME   TO TC-POSTTIME.
           MOVE POST-DATE   TO TC-POSTDATE.
           MOVE CX-TILLNO   TO TC-TILLNO.
           MOVE CX-PAYEE-NO TO TC-PAYEE-NO.
           IF TC-TYPE = "DB"
              MOVE CX-FREE1 TO TC-DEBIT-REFCD.

           PERFORM WRITE-TRC1-FILE.
           IF IO-FG = 9
              IF HOLD-SEQ < 99999
                 ADD 1 TO HOLD-SEQ
                 GO TO UPDATE-TC-FILE-NEXT
              ELSE
                 STRING "CHECK TRANSACTION RECORD (TC-SEQ = 99999),"
                           " UPDATE (TC) SKIPPED !"
                     DELIMITED BY SIZE INTO MESS
                 PERFORM SEND-MESS.

       EXIT-UPDATE-TRC.
           EXIT.

      ******************************************
      *    UPDATE CHECK MASTER FILE
      ******************************************
       UPDATE-CK-FILE SECTION.

           MOVE "UPDATING CHECK MASTER................." TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM OPEN-CK1-FILE.
           PERFORM CLEAR-CK-FILE.

           MOVE CX-BRNO TO CK-BRNO.
           MOVE CX-CKNO TO CK-NO.

           MOVE TRANS-DATE TO CK-DATE.
      * GLB  03/21/03  IF MISC & MANUAL, NO CX-DATE
           IF BR-ALLOW-SYS-AS-EFFDATE = "Y"
              IF CX-DATE NOT = 0
                 MOVE CX-DATE TO CK-DATE.
           MOVE CX-TYPE TO CK-TYPE.
           MOVE CX-AMT TO CK-AMT.
           MOVE CX-PAYEE TO CK-PAYEE.
           MOVE CX-ADR1 TO CK-ADR1.
           MOVE CX-ADR2 TO CK-ADR2.
           MOVE CX-CITY TO CK-CITY.
           MOVE CX-STATE TO CK-STATE.
           MOVE CX-ZIP TO CK-ZIP.
           MOVE CX-REF TO CK-REF.
           IF CX-CKNO-NEW NOT = 0
              MOVE HOLD-CHECK TO CX-REF.
           MOVE CX-REASON TO CK-REASON.
           MOVE CX-PGM TO CK-PGM.
           MOVE CX-PAID TO CK-PAID.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              MOVE CX-GLNO(SUB) TO CK-GLNO(SUB)
              MOVE CX-GLAMT(SUB) TO CK-GLAMT(SUB)
           END-PERFORM.

           MOVE "N"           TO CK-VOID.
           MOVE CX-CLASS      TO CK-CLASS.
           MOVE CX-DLNO       TO CK-DLNO.
           MOVE CX-DLRVNO     TO CK-DLRVNO.
           MOVE CX-ACCTNO     TO CK-ACCTNO.
           MOVE 0             TO CK-VOID-DATE.
           MOVE CX-SSNO       TO CK-SSNO.
           MOVE CX-BATCHID    TO CK-BATCHID.
           MOVE CX-DT-UNPAID  TO CK-UNPAID.
           MOVE CX-TILLNO     TO CK-TILLNO.
           MOVE CX-EVENT-CD   TO CK-EVENT-CD.
           MOVE CX-PAYEE-NO   TO CK-PAYEE-NO.
           MOVE CX-FREE1      TO CK-FREE1.
           MOVE CX-FREE2      TO CK-FREE2.

           PERFORM WRITE-CK1-FILE.
           IF IO-FG = 9
              MOVE "CHECK RECORD NOT CREATED!" TO MESS
              PERFORM SEND-MESS.

       EXIT-UPDATE-CK.
           EXIT.

      ******************************************
      *    CREATE ALIGNMENT CHECK MASTER
      ******************************************
       CREATE-CK-ALIGNMENT SECTION.

           MOVE "UPDATING CHECK MASTER................." TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM OPEN-CK1-FILE.
           PERFORM CLEAR-CK-FILE.

           MOVE BRANCH   TO CK-BRNO.
           MOVE CHECK-NO TO CK-NO.

           MOVE TRANS-DATE TO CK-DATE.
      *    IF BR-ALLOW-SYS-AS-EFFDATE = "Y"
      *       MOVE CX-DATE TO CK-DATE.
           MOVE "AT" TO CK-TYPE.
           MOVE 0 TO CK-AMT.
           MOVE "ALIGNMENT TEST" TO CK-PAYEE.
           MOVE CHECK-NO TO CK-REF.
           MOVE "BATCH CHECK PRINT ALIGNMENT" TO CK-REASON.
           MOVE "XX" TO CK-PGM.
           MOVE "V" TO CK-PAID.

           MOVE "Y"           TO CK-VOID.
           MOVE 0             TO CK-CLASS.
           MOVE 0             TO CK-DLNO.
           MOVE 0             TO CK-DLRVNO.
           MOVE 0             TO CK-ACCTNO.
           MOVE TRANS-DATE    TO CK-VOID-DATE.
           MOVE 0             TO CK-SSNO.
           MOVE 0             TO CK-BATCHID.
           MOVE "N"           TO CK-UNPAID.

           PERFORM WRITE-CK1-FILE.
           IF IO-FG = 9
              MOVE "CHECK RECORD NOT CREATED!" TO MESS
              PERFORM SEND-MESS.

       EXIT-CREATE-ALIGN.
           EXIT.

      ******************************************
      * UPDATE ALIGNMENT CHECK TRANSACTION FILE
      ******************************************
       UPDATE-TC-ALIGNMENT SECTION.

           MOVE "UPDATING CHECK TRANSACTION............" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-TRC1-FILE.

           MOVE 1 TO HOLD-SEQ.
       UPDATE-TC-ALIGN-NEXT.
           PERFORM CLEAR-TRC-FILE.

           MOVE BRANCH     TO TC-BRNO.
           MOVE TRANS-DATE TO TC-KEY-DATE.
           MOVE CHECK-NO   TO TC-CKNO.
           MOVE 0          TO TC-CLASS.
           MOVE HOLD-SEQ   TO TC-SEQ.

           MOVE CK-TYPE    TO TC-TYPE.
           MOVE TRANS-DATE TO TC-DATE.

           MOVE CK-PAYEE   TO TC-PAYEE.
           MOVE CK-REASON  TO TC-REASON.
           MOVE CK-VOID    TO TC-VOID.
           MOVE CX-PAID    TO TC-PAID.

           PERFORM SET-POSTDATE.
           MOVE POST-USERID TO TC-USERID.
           MOVE POST-TIME TO TC-POSTTIME.
           MOVE POST-DATE TO TC-POSTDATE.

           PERFORM WRITE-TRC1-FILE.
           IF IO-FG = 9
              IF HOLD-SEQ < 9999
                 ADD 1 TO HOLD-SEQ
                 GO TO UPDATE-TC-ALIGN-NEXT
              ELSE
                 STRING "CHECK TRANSACTION RECORD (TC-SEQ = 9999),"
                           " UPDATE (TC) SKIPPED !"
                     DELIMITED BY SIZE INTO MESS
                 PERFORM SEND-MESS.

       EXIT-UPDATE-TRC-ALIGN.
           EXIT.

      ******************************************
      *    UPDATE DEALER STATISTICS FILE
      *  CX-TYPES DR, DC, DP RSV CHECK 'TO'   DEALER
      *  CX-TYPES RR, RC, RP RSV CHECK 'FROM'  DEALER
      *  CX-TYPES RD         PROCEED CHECK FROM DEALER
      ******************************************
       UPDATE-DS-RESERVE SECTION.

           MOVE "UPDATING DEALER STATISTICS ............." TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM OPEN-DS1-FILE.

       FIND-DS-RESERVE.
          
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO DS-DATE-CCYYMM.
           MOVE CX-BRNO TO DS-BRANCH.
           MOVE CX-DLNO TO DS-DLNO.
           MOVE CX-CLASS TO DS-CLASS.

           PERFORM READ-DS1-FILE.
           IF IO-FG = 9
              PERFORM CLEAR-DS-FILE
              MOVE TRANS-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-YYYYMM
              MOVE DATE-YYYYMM TO DS-DATE-CCYYMM
              MOVE CX-BRNO TO DS-BRANCH
              MOVE CX-DLNO TO DS-DLNO
              MOVE CX-CLASS TO DS-CLASS
              PERFORM WRITE-DS1-FILE
              IF IO-FG = 9
                 GO TO IO-ERROR
              END-IF
              GO TO FIND-DS-RESERVE.

      *--------------------
      * PAYEE-SUB 6 = DR, RR
      *--------------------
           IF CX-TYPE = "DR"
              SUBTRACT CX-AMT FROM DS-DLRVPRIN(CX-DLRVNO)
      * FROM 3ONPI0.C VOID CHECK
      *       IF CHECK-PAYEE = "DR"
              IF CX-DLRVNO = 3
                 SUBTRACT CX-AMT FROM DS-AFPDUEDL-MEMO.
      * GLB 11/19/01 NOW ALREADY REVERSED FOR G/L
           IF CX-TYPE = "RR"
      *       ADD CX-AMT TO DS-DLRVPRIN(CX-DLRVNO).
              SUBTRACT CX-AMT FROM DS-DLRVPRIN(CX-DLRVNO).

      *--------------------
      * PAYEE-SUB 7 = DC, RC
      *--------------------
           IF CX-TYPE = "DC"
              SUBTRACT CX-AMT FROM DS-DLRVCHGS(CX-DLRVNO).
      * GLB 11/19/01 NOW ALREADY REVERSED FOR G/L
           IF CX-TYPE = "RC"
      *       ADD CX-AMT TO DS-DLRVCHGS(CX-DLRVNO).
              SUBTRACT CX-AMT FROM DS-DLRVCHGS(CX-DLRVNO).
      *--------------------
      * PAYEE-SUB 8 = DP, RP
      *--------------------
           IF CX-TYPE = "DP"
              SUBTRACT CX-AMT FROM DS-DLPARVHELD.
      * GLB 11/19/01 NOW ALREADY REVERSED FOR G/L
           IF CX-TYPE = "RP"
      *       ADD CX-AMT TO DS-DLPARVHELD.
              SUBTRACT CX-AMT FROM DS-DLPARVHELD.

      *--------------------
      *  RECEIPT DEALER PROCEEDS
      *--------------------
      *    IF CX-TYPE = "RD"
      *       IF CX-DT-UNPAID = "N"
      *          GO TO EXIT-UPDATE-DS-RESERVE.
           IF CX-TYPE = "RD"
               IF CX-DT-UNPAID = "M"
               OR
                  CX-DT-UNPAID = "N"
      * GLB 11/19/01 NOW ALREADY REVERSED FOR G/L
      *          ADD CX-AMT TO DS-UNPAID-MANUAL
                 SUBTRACT CX-AMT FROM DS-UNPAID-MANUAL
               ELSE
      *          ADD CX-AMT TO DS-UNPAIDCK.
                 SUBTRACT CX-AMT FROM DS-UNPAIDCK.

           PERFORM REWRITE-DS1-FILE.

       EXIT-UPDATE-DS-RESERVE.
           PERFORM CLOSE-DS1-FILE.

      **********************************************
      *         DTFILE UPDATE BATCH CHECKS
      *
      *  UDPATE OF DT-RECORDS APPLICABLE TO TYPES:
      *    "DL" BATCH FROM BATCH RELEASE
      *
      **********************************************
       UPDATE-DT-BATCH SECTION.

           PERFORM OPEN-DT2-FILE.
           IF (NOT DEALER)
              GO TO EXIT-UPDATE-DT-BATCH.

           MOVE "UPDATING DEALER TRANSACTION FILE........" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE CX-BRNO    TO DT-BRNO
                              QDT2-WBEG-BRNO
                              QDT2-WEND-BRNO.
           MOVE CX-BATCHID TO DT-BATCHID
                              QDT2-WBEG-BATCHID
                              QDT2-WEND-BATCHID.
           MOVE 0          TO DT-BATCHID-SEQ HOLD-BATCHID-SEQ
                              QDT2-WBEG-BATCHID-SEQ.
           MOVE ALL "9"    TO QDT2-WEND-BATCHID-SEQ.

           PERFORM START-DT2-FILE.
       UPDATE-DT-BATCH-NEXT.
           PERFORM READ-DT2-FILE-NEXT.
           IF (IO-FG = 9) OR (DT-BRNO NOT = CX-BRNO)
              GO TO EXIT-UPDATE-DT-BATCH.

           IF DT-BATCHID NOT = CX-BATCHID
              GO TO EXIT-UPDATE-DT-BATCH.

           MOVE CX-DLNO TO HOLD-CX-DLNO.
           IF DT-DL NOT = HOLD-CX-DL
              GO TO UPDATE-DT-BATCH-NEXT.

      *---------------------------------------------------------------
      * DT-STATUS = " "  -  HAS NOT BEEN RELEASED IN BATCH RELEASE
      *           = "R"  -  HAS BEEN RELEASED, WAITING FOR ASSOCIATED
      *                     CHECK TO BE PRINTED
      *           = "P"  -  ACCOCIATED CHECK HAS BEEN PRINTED
           IF DT-STATUS NOT = "R"
              GO TO UPDATE-DT-BATCH-NEXT.

           ADD DT-RELEASED TO DT-PAYMENTS.

      * GLB 07/24/00
           MOVE 0 TO DT-RELEASED.

           MOVE "P"       TO DT-STATUS.
           MOVE CX-CKNO   TO DT-CKNO.
           MOVE CX-DATE   TO DT-CKDATE.
           MOVE CX-PAID   TO DT-PAID.
           MOVE CX-DLRVNO TO DT-DLRVNO.
           MOVE " "       TO DT-CHECK-VOID.
           PERFORM REWRITE-DT2-FILE.

           GO TO UPDATE-DT-BATCH-NEXT.

       EXIT-UPDATE-DT-BATCH.
           PERFORM CLOSE-DT2-FILE.

      *******************************************************
      *         DTFILE UPDATE (NO) BATCH CHECKS
      *
      *  UDPATE PAID AMTS/BALS OF NW/PU DT-RECORDS:
      *     IF "DL" OR "VX"
      *        (+) PROCEEDS PAID, 0 RELEASED
      *
      *    "DR" SYS OR MANUAL (+) RSV PRINCIPAL PAID
      *    "DC" SYS OR MANUAL (+) RSV CHARGES PAID
      *    "DP" SYS OR MANUAL (+) RSV PARTICIPATION PAID
      *    "RR" RECEIPT       (-) RSV PRINCIPAL PAID
      *    "RC" RECEIPT       (-) RSV CHARGES PAID
      *    "RP" RECEIPT       (-) RSV PARTICIPATION PAID
      *    "RD" RECEIPT       (-) RSV PROCEEDS PAID
      *
      *******************************************************
       UPDATE-DT-NOBATCH SECTION.
      *  GLB REPLACED ORIGINAL ROUTINE WITH THE FOLLOWING:

           PERFORM OPEN-DT1-FILE.

           MOVE "UPDATING DEALER TRANSACTION FILE........" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE CX-BRNO      TO DT-BRNO
                                QDT1-WBEG-BRNO
                                QDT1-WEND-BRNO.
           MOVE CX-DLNO      TO DT-DLNO.
           MOVE DT-NUM       TO QDT1-WBEG-NUM
                                QDT1-WEND-NUM.
           MOVE CX-CLASS     TO DT-CLASS
                                QDT1-WBEG-CLASS.
           MOVE ALL "9"      TO QDT1-WEND-CLASS.
           MOVE CX-ACCTNO    TO DT-ACCTNO
                                QDT1-WBEG-ACCTNO
                                QDT1-WEND-ACCTNO.
           MOVE 0            TO DT-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QDT1-WBEG-DATE.
           MOVE EXT-JULIAN-END TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QDT1-WEND-DATE.
           MOVE 0            TO DT-SEQ HOLD-BATCHID-SEQ
                                QDT1-WBEG-SEQ.
           MOVE ALL "9"      TO QDT1-WEND-SEQ.

           PERFORM START-DT1-FILE.
       UPDATE-DT-NOBATCH-NEXT.
           PERFORM READ-DT1-FILE-NEXT.
           IF (IO-FG = 9) OR (DT-BRNO NOT = CX-BRNO)
              GO TO EXIT-UPDATE-DT-NOBATCH.

           IF DT-DLNO NOT = CX-DLNO
              GO TO EXIT-UPDATE-DT-NOBATCH.

      *  EXIT HERE?
           IF DT-ACCTNO NOT = CX-ACCTNO
              GO TO EXIT-UPDATE-DT-NOBATCH.

      *  GLB, ADDED TEST OF TRCD = NW, PU, AV
           MOVE DT-TRCD TO CHECK-TRCD.
           IF NOT (DEALER-ORIGINAL)
                 GO TO UPDATE-DT-NOBATCH-NEXT.
           MOVE DT-SEQ TO HOLD-BATCHID-SEQ.

      *  TEST DEALER RESERVE CHECKS
           IF (DEALER-RSV)
              OR (DEALER-RSV-REC)
              OR (DEALER-PROC-REC)
              PERFORM UPDATE-DT-RESERVE-PAYS
              PERFORM REWRITE-DT1-FILE
              GO TO EXIT-UPDATE-DT-NOBATCH.

      * MULTIPLE CKS MEAN WE MUST MATCH CORRECT DT TO CX BY SEQ
      * THESE ARE SYSTEM CHECKS REQUESTED BY LOAN PROC
           IF DT-SEQ NOT = CX-DT-SEQ
              GO TO UPDATE-DT-NOBATCH-NEXT.

      * DT-STATUS = " "  -  RECORDS NOT RELEASED THRU BATCH RELEASE
      * DT-STATUS = "R"  -  RECORDS HAVE BEEN RELEASED, WAITING TO PRINT
      * DT-STATUS = "P"  -  RECORDS HAVE BEEN PRINTED
      * DT-STATUS = "C"  -  MEMO CHECK RECORDS
      * DT-STATUS = "V"  -  MEMO VOID  RECORDS
           IF DT-STATUS NOT = "R"
              GO TO UPDATE-DT-NOBATCH-NEXT.

           ADD DT-RELEASED TO DT-PAYMENTS.

           MOVE "P"       TO DT-STATUS.
           MOVE CX-CKNO   TO DT-CKNO.
           MOVE CX-DATE   TO DT-CKDATE.
           MOVE CX-PAID   TO DT-PAID.
           MOVE CX-DLRVNO TO DT-DLRVNO.
           MOVE 0         TO DT-BATCHID.
           MOVE " "       TO DT-CHECK-VOID.
           MOVE " "       TO DT-SELECTED.
           MOVE 0 TO DT-RELEASED.

           PERFORM REWRITE-DT1-FILE.

       CONTINUE-DT-NOBATCH.
      *  GLB  08/16/01 DONT NEED TO LOOP ?
      *    GO TO UPDATE-DT-NOBATCH-NEXT.

       EXIT-UPDATE-DT-NOBATCH.
           PERFORM CLOSE-DT1-FILE.

      **********************************************
      *  UPDATE OF NW/PU DT-RECORDS FOR TYPES:     *
      *    "DR" SYSTEM OR MANUAL (WRITE DT-)       *
      *    "DC" SYSTEM OR MANUAL (WRITE DT-)       *
      *    "DP" SYSTEM OR MANUAL (WRITE DT-)       *
      *    "RR" RECEIPT DEALER PRINCIPAL           *
      *    "RC" RECEIPT DEALER CHARGES             *
      *    "RP" RECEIPT DEALER PARTCIP             *
      *    "RD" RECEIPT DEALER PROCEEDS            *
      **********************************************
       UPDATE-DT-RESERVE-PAYS SECTION.

      *--------------------
      * PAYEE-SUB 6 = DR, RR
      *--------------------
           IF CX-TYPE = "DR"
              ADD  CX-AMT TO DT-DLRVPRIN-PAY.
      * GLB 11/19/01 NOW ALREADY REVERSED FOR G/L
           IF CX-TYPE = "RR"
              SUBTRACT  CX-AMT FROM DT-DLRVPRIN(CX-DLRVNO).
      *       SUBTRACT CX-AMT FROM DT-DLRVPRIN-PAY.

      *--------------------
      * PAYEE-SUB 7 = DC, RC
      *--------------------
           IF CX-TYPE = "DC"
              ADD  CX-AMT TO DT-DLRVCHGS-PAY.
      * GLB 11/19/01 NOW ALREADY REVERSED FOR G/L
           IF CX-TYPE = "RC"
              SUBTRACT CX-AMT FROM DT-DLRVCHGS(CX-DLRVNO).
      *       SUBTRACT CX-AMT FROM DT-DLRVCHGS-PAY.

      *--------------------
      * PAYEE-SUB 8 = DP, RP
      *--------------------
           IF CX-TYPE = "DP"
              ADD  CX-AMT TO DT-DLPARVHELD-PAY.
      * GLB 11/19/01 NOW ALREADY REVERSED FOR G/L
           IF CX-TYPE = "RP"
              SUBTRACT CX-AMT FROM DT-DLPARVHELD.
      *       SUBTRACT CX-AMT FROM DT-DLPARVHELD-PAY.

      *--------------------
      *  RD
      *--------------------
      * GLB 11/19/01 NOW ALREADY REVERSED FOR G/L
           IF CX-TYPE = "RD"
      *       SUBTRACT CX-AMT FROM DT-PAYMENTS.
              ADD CX-AMT TO DT-PAYMENTS.

      **********************************************
      * IF "VX" DEALER INPUT, EITHER --            *
      *  1) CREATE A NEW "NW" RECORD               *
      *  2) FIND EXISTING DT-REC SELECTED IN 2ONPI0*
      *     AND TEMPORARILY SET RELEASE STATUS     *
      **********************************************
       RELEASE-NW-PROCEEDS SECTION.
           MOVE "RELEASING DEALER PROCEEDS DETAIL........" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-DT1-FILE.
           MOVE CX-BRNO     TO HOLD-DT-BRNO.
           MOVE CX-DLNO     TO HOLD-DT-DLNO.
           MOVE CX-CLASS    TO HOLD-DT-CLASS.
           MOVE CX-ACCTNO   TO HOLD-DT-ACCTNO.
           MOVE CX-DT-DATE  TO HOLD-DT-DATE.
           MOVE CX-DT-SEQ   TO HOLD-DT-SEQ.
           IF CX-DT-SEQ = 0
              MOVE TRANS-DATE   TO HOLD-DT-DATE.
       FIND-RELEASE-DT.
           PERFORM CLEAR-DT-FILE.
           MOVE HOLD-DT1-KEY TO DT1-KEY.
           PERFORM READ-DT1-FILE.
           IF IO-FG = 0
              MOVE "R" TO DT-STATUS
              MOVE CX-AMT TO DT-RELEASED
              PERFORM REWRITE-DT1-FILE
              GO TO EXIT-RELEASE-NW-PROCEEDS.

           MOVE HOLD-DT-SEQ TO CX-DT-SEQ.

           PERFORM CLEAR-DT-FILE.

           MOVE HOLD-DT1-KEY TO DT1-KEY.
           MOVE "R" TO DT-STATUS.
           IF CX-SERVACCT = "Y"
              MOVE "S" TO DT-TYPE
           ELSE
              MOVE "P" TO DT-TYPE.
           MOVE "NW"        TO DT-TRCD.
           MOVE CX-AMT      TO DT-PROCEEDS.
           MOVE CX-AMT      TO DT-RELEASED.
           MOVE CX-PAID     TO DT-PAID.
           MOVE CX-DLRVNO   TO DT-DLRVNO.
           MOVE "N"         TO DT-UNPAID.
           MOVE " "         TO DT-MEMO.
           PERFORM WRITE-DT1-FILE.
       EXIT-RELEASE-NW-PROCEEDS.
           PERFORM CLOSE-DT1-FILE.

      **********************************************
      * GLB  NEW, NOW EVERYBODY GETS CHECK HISTORY *
      * CREATE DT PAYMENT FOR ALL DEALER CHECKS    *
      * CREATE DT VOID    FOR ALL CHECK  VOIDS     *
      * IF "DL" CHECK      CREATE DT-TRCD "CK"     *
      * IF "VX" CHECK      CREATE DT-TRCD "CK"     *
      * IF "DR" CHECK      CREATE DT-TRCD "DR"     *
      * IF "DC" CHECK      CREATE DT-TRCD "DC"     *
      * IF "DP" CHECK      CREATE DT-TRCD "DP"     *
      * IF "RR" RECEIPT    CREATE DT-TRCD "RR"     *
      * IF "RC" RECEIPT    CREATE DT-TRCD "RC"     *
      * IF "RP" RECEIPT    CREATE DT-TRCD "RP"     *
      * IF "RD" RECEIPT    CREATE DT-TRCD "RD"     *
      * IF "V " VOID       CREATE DT-TRCD "V "     *
      **********************************************
       CREATE-DT-HISTORY SECTION.
           MOVE "CREATING DEALER CHECK DETAIL........" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-DT1-FILE.
           MOVE CX-BRNO     TO HOLD-DT-BRNO.
           MOVE CX-DLNO     TO HOLD-DT-DLNO.
           MOVE CX-CLASS    TO HOLD-DT-CLASS.
           MOVE CX-ACCTNO   TO HOLD-DT-ACCTNO.
           MOVE TRANS-DATE  TO HOLD-DT-DATE.
           MOVE 1           TO HOLD-DT-SEQ.
       UPDATE-DT-CREATE-NEXT.
           PERFORM CLEAR-DT-FILE.
           MOVE HOLD-DT1-KEY TO DT1-KEY.
           PERFORM READ-DT1-FILE.
           IF IO-FG = 0
              ADD 1 TO HOLD-DT-SEQ
              IF HOLD-DT-SEQ < 9999
                 GO TO UPDATE-DT-CREATE-NEXT
              ELSE
                 MOVE "UNABLE TO CREATE DEALER TRANSACTION" TO MESS
                 PERFORM SEND-MESS
                 GO TO EXIT-DT-CREATE-HISTORY.
           PERFORM CLEAR-DT-FILE.
           MOVE HOLD-DT1-KEY TO DT1-KEY.
           PERFORM WRITE-DT1-FILE.

       FINISH-DT-CREATE.
           MOVE CX-CKNO     TO DT-CKNO.
           MOVE TRANS-DATE  TO DT-CKDATE.
           MOVE CX-PAID     TO DT-PAID.
           MOVE CX-DLRVNO   TO DT-DLRVNO.
           MOVE CX-EVENT-CD TO DT-EVENT-CD.
           IF CX-SERVACCT = "Y"
              MOVE "S" TO DT-TYPE
           ELSE
              MOVE "P" TO DT-TYPE.

           MOVE HOLD-DT-TRCD   TO DT-TRCD.

           MOVE CX-BATCHID     TO DT-BATCHID.
           MOVE CX-DT-UNPAID   TO DT-UNPAID.

      *--------------------
      * SET STATUS TO EITHER
      *  "C" = CHECK
      *  "V" = VOID
      *--------------------
           MOVE "C"         TO DT-STATUS.
           IF HOLD-CX-PAID = "V"
              MOVE "V"      TO DT-CHECK-VOID
              MOVE "V"      TO DT-STATUS.

           IF (DEALER-PROCEEDS)
              OR (DEALER-PROC-REC)
                 MOVE "M" TO DT-MEMO.
      *--------------------
      * PAYEE-SUB 2 = DL
      *--------------------
           IF CX-TYPE = "DL"
      * GLB 08/09/01
      *       MOVE  CX-AMT TO DT-PROCEEDS
      *       MOVE  CX-AMT TO DT-PAYMENTS.
              SUBTRACT CX-AMT FROM DT-PAYMENTS.

      *--------------------
      * PAYEE-SUB 9 = VX
      *--------------------
           IF CX-TYPE = "VX"
      * GLB 08/09/01
      *       MOVE  CX-AMT TO DT-PROCEEDS
      *       MOVE  CX-AMT TO DT-PAYMENTS.
              SUBTRACT CX-AMT FROM DT-PAYMENTS.

      *--------------------
      * PAYEE-SUB 6 = DR, RR
      *--------------------
      * GLB 08/07/01
           IF CX-TYPE = "DR"
      *       SUBTRACT  CX-AMT FROM DT-DLRVPRIN-PAY
              SUBTRACT  CX-AMT FROM DT-DLRVPRIN(CX-DLRVNO).
      * GLB 11/19/01 NOW ALREADY REVERSED FOR G/L
           IF CX-TYPE = "RR"
      *       ADD CX-AMT TO DT-DLRVPRIN(CX-DLRVNO).
              SUBTRACT CX-AMT FROM DT-DLRVPRIN(CX-DLRVNO).

      *--------------------
      * PAYEE-SUB 7 = DC, RC
      *--------------------
      * GLB 08/07/01
           IF CX-TYPE = "DC"
      *       SUBTRACT  CX-AMT FROM DT-DLRVCHGS-PAY
              SUBTRACT  CX-AMT FROM DT-DLRVCHGS(CX-DLRVNO).
      * GLB 11/19/01 NOW ALREADY REVERSED FOR G/L
           IF CX-TYPE = "RC"
      *       ADD CX-AMT TO DT-DLRVCHGS(CX-DLRVNO).
              SUBTRACT CX-AMT FROM DT-DLRVCHGS(CX-DLRVNO).

      *--------------------
      * PAYEE-SUB 8 = DP, RP
      *--------------------
      * GLB 08/07/01
           IF CX-TYPE = "DP"
      *       SUBTRACT  CX-AMT FROM DT-DLPARVHELD-PAY
              SUBTRACT  CX-AMT FROM DT-DLPARVHELD.
      * GLB 11/19/01 NOW ALREADY REVERSED FOR G/L
           IF CX-TYPE = "RP"
      *       ADD CX-AMT TO DT-DLPARVHELD.
              SUBTRACT CX-AMT FROM DT-DLPARVHELD.

      *--------------------
      * RD DEALER PROC RECEIPTS
      *--------------------
      * GLB 08/07/01
      * GLB 11/19/01 NOW ALREADY REVERSED FOR G/L
           IF CX-TYPE = "RD"
      *       ADD CX-AMT TO DT-PAYMENTS
              SUBTRACT CX-AMT FROM DT-PROCEEDS.

           PERFORM REWRITE-DT1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

       EXIT-DT-CREATE-HISTORY.
           PERFORM CLOSE-DT1-FILE.

      ***********************************************
      *    UPDATE DSFILE UNPAID CHECKS
      ***********************************************
       UPDATE-DS-UNPAID-CHECK SECTION.

           PERFORM OPEN-DS1-FILE.
      *    IF (NOT DEALER-PROCEEDS)
      *       GO TO EXIT-UPDATE-DS-UNPAID-CHECK.

       FIND-DS-UNPAID.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO DS-DATE-CCYYMM.
           MOVE CX-BRNO TO DS-BRANCH.
           MOVE CX-DLNO TO DS-DLNO.
           MOVE CX-CLASS TO DS-CLASS.
           PERFORM READ-DS1-FILE.
           IF IO-FG = 9
              PERFORM CLEAR-DS-FILE
              MOVE TRANS-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-YYYYMM
              MOVE DATE-YYYYMM TO DS-DATE-CCYYMM
              MOVE CX-BRNO TO DS-BRANCH
              MOVE CX-DLNO TO DS-DLNO
              MOVE CX-CLASS TO DS-CLASS
              PERFORM WRITE-DS1-FILE
              IF IO-FG = 9
                 GO TO IO-ERROR
              END-IF
              GO TO FIND-DS-UNPAID.


           IF IO-FG = 9
              STRING "DEALER STATISTICS MISSING "
                        DS-BRANCH "  "
                         DS-NUM "  "
                          DS-CLASS
                 DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO EXIT-UPDATE-DS-UNPAID-CHECK.

      *------------------------------------------------------------
      * V O I D   "M"ANUAL CHECK,       ADD BACK TO DS-UNPAID-MANUAL
      * V O I D   "M"ANUAL BATCH CHECK, ADD BACK TO DS-UNPAID
      * V O I D   "S"YSTEM CHECK,       ADD BACK TO DS-UNPAID
           IF CK-BATCHID = 0202020202
              MOVE 0 TO CK-BATCHID.
           IF HOLD-CX-PAID = "V"
                 IF CK-UNPAID = "N"
                 IF CK-PGM = "I0"
                    GO TO EXIT-UPDATE-DS-UNPAID-CHECK.
           IF HOLD-CX-PAID = "V"
                 IF CK-UNPAID = "N"
                    OR CK-UNPAID = "M"
                    SUBTRACT CX-AMT FROM DS-UNPAID-MANUAL
                 ELSE
                    SUBTRACT CX-AMT FROM DS-UNPAIDCK
                 END-IF
           PERFORM REWRITE-DS1-FILE
           GO TO EXIT-UPDATE-DS-UNPAID-CHECK.

      * N E W  IF REQUESTED FROM CHECK WRITING, DO NOTHING
      * N E W  IF ORIGINAL REQUEST FROM LOAN PROCESSING,
      *        IF WRITING A REPLACEMENT CHECK FOR A VOIDED MANUAL
      *           CHECK, THEN REUPDATE UNPAID-MANUAL.
      *        IF WRITING A REPLACEMENT CHECK FOR A VOIDED SYSTEM
      *           CHECK, THEN REUPDATE UNPAIDCK.
           IF CX-DT-UNPAID = "N"
              GO TO EXIT-UPDATE-DS-UNPAID-CHECK.
           IF CX-DT-UNPAID = "M"
              SUBTRACT CX-AMT FROM DS-UNPAID-MANUAL
           ELSE
              SUBTRACT CX-AMT FROM DS-UNPAIDCK.

           PERFORM REWRITE-DS1-FILE.

       EXIT-UPDATE-DS-UNPAID-CHECK.
           PERFORM CLOSE-DS1-FILE.

      **************************************************
      * V O I D     C H E C K    M A S T E R
      **************************************************
       VOID-CHECK-MASTER SECTION.
           MOVE CX-BRNO TO CK-BRNO.
           MOVE CX-CKNO TO CK-NO.
           PERFORM OPEN-CK1-FILE.
           PERFORM READ-CK1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE "Y" TO CK-VOID CX-VOID.
           IF CX-CKNO-NEW NOT = 0
              MOVE "R" TO CK-VOID.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           MOVE ALP-MO TO NUM-MO.
           MOVE ALP-DA TO NUM-DA.
           MOVE ALP-YR TO NUM-YR.
           MOVE NUM-DATE TO CK-VOID-DATE.

           PERFORM REWRITE-CK1-FILE.
           PERFORM CLOSE-CK1-FILE.

      * CK MOVED TO CX; AMTS REVERSED: NEED WORKERS FOR TRC-FILE
           MOVE CK-CLASS TO CX-CLASS.
           MOVE CK-NO TO CX-CKNO.
           MOVE CK-DLNO TO CX-DLNO.
           MOVE CK-DLRVNO TO CX-DLRVNO.
           MOVE CK-DATE TO CX-DATE.
           MOVE CK-PGM TO CX-PGM.
           MOVE CK-PAID TO CX-PAID.
           MOVE CK-TYPE TO CX-TYPE.
           MOVE CK-AMT TO CX-AMT.
           MOVE CK-PAYEE TO CX-PAYEE.
           MOVE CK-REF TO CX-REF.
           IF CX-CKNO-NEW NOT = 0
              MOVE CX-CKNO-NEW TO CX-REF.
           MOVE CK-ACCTNO TO  CX-ACCTNO.
           MOVE CX-REASON TO HOLD-CX-REASON.
           MOVE CK-REASON TO CX-REASON.
           MOVE CK-SSNO TO CX-SSNO.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              MOVE CK-GLNO(SUB) TO CX-GLNO(SUB)
              COMPUTE CX-GLAMT(SUB) = CK-GLAMT(SUB) * -1
           END-PERFORM.

           MULTIPLY -1 BY CX-AMT.


      **************************************************
      *  R E V E R S E   A F T E R     V O I D
      *  SIGNS PREVIOUSLY REVERSED WHEN CHECK VOIDED,
      *  RETURN TO POSITIVE CONDITION.
      *  USE THE NEW CHECK NO. TO CREATE CK-FILE
      *  REMOVE THE VOID FLAG
      **************************************************
       SETUP-REPLACE-CHECK SECTION.
           MOVE HOLD-CX-REASON TO CX-REASON.
           MOVE CX-CKNO TO HOLD-CHECK.
           MOVE CX-CKNO-NEW TO CX-CKNO.
           MOVE CX-PAID     TO HOLD-CX-PAID.

           MULTIPLY -1 BY CX-AMT.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              MULTIPLY -1 BY  CX-GLAMT(SUB)
           END-PERFORM.
           MOVE "N" TO CX-VOID.
           IF CX-CKPAID-NEW = "M" OR "S"
              MOVE CX-CKPAID-NEW TO CX-PAID.


      ******************************************************
      *    VOID OF DT-RECORDS APPLICABLE TO TYPES:
      *         DEALER = "DL", "VX"
      *
      *    "DL" SYSTEM OR MANUAL FROM LONPB2, S.F. INPUT
      *                          (REWRITE DT-)
      *    "DL" BATCH FROM LONPT0, BATCH RELEASE
      *                          (REWRITE DT-)
      ******************************************************
       REVERSE-DT-PROCEEDS SECTION.

           PERFORM OPEN-DT4-FILE.
           IF (NOT DEALER)
              GO TO EXIT-REVERSE-DT-PROCEEDS.

           MOVE "REVERSING DEALER TRANSACTIONS..........." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE CX-BRNO TO DT-BRNO
                           QDT4-WBEG-BRNO
                           QDT4-WEND-BRNO.
           MOVE CX-CKNO TO DT-CKNO
                           QDT4-WBEG-CKNO
                           QDT4-WEND-CKNO.

           PERFORM START-DT4-FILE.

       REVERSE-DT-PROCEEDS-NEXT.
           PERFORM READ-DT4-FILE-NEXT.
           IF (IO-FG = 9) OR (DT-BRNO NOT = CX-BRNO)
              GO TO EXIT-REVERSE-DT-PROCEEDS.

           IF DT-CKNO NOT = CX-CKNO
              GO TO EXIT-REVERSE-DT-PROCEEDS.

      * GLB  07/10/02 YIKES! THIS CERTAINLY WONT WORK FOR MULTIPLE
      *      DEALERS ON ONE CHECK; 10.01, 10.02, 10.03, ETC
      *    IF DT-DLNO NOT = CX-DLNO
           IF DT-BATCHID NOT = 0
              MOVE CX-DLNO TO HOLD-CX-DLNO
              IF DT-DL NOT = HOLD-CX-DL
      *          UNLOCK DT-FILE
                 GO TO REVERSE-DT-PROCEEDS-NEXT
              END-IF
           ELSE
              IF DT-DLNO NOT = CX-DLNO
      *          UNLOCK DT-FILE
                 GO TO REVERSE-DT-PROCEEDS-NEXT.

      *  NOT SURE ABOUT THIS; JUST TESTING RELEASED NOW
           IF DT-STATUS  NOT = "P"
      *       UNLOCK DT-FILE
              GO TO REVERSE-DT-PROCEEDS-NEXT.

      * ??? WHAT ABOUT PAYMENT POSTING CODES SERVICE ???
      *  TEST FOR NW, PU, AV
           MOVE DT-TRCD TO CHECK-TRCD.
      * GLB  07/13/04 SKIP TEST IF SERVICE
           IF DT-TYPE NOT = "S"
              IF (DEALER-PROCEEDS)
                 IF NOT (DEALER-ORIGINAL)
      *          IF DT-TRCD NOT = "NW" "PU" "TX"
      *             UNLOCK DT-FILE
                    GO TO REVERSE-DT-PROCEEDS-NEXT.

           MOVE DT-SEQ TO HOLD-BATCHID-SEQ.

      *-----------------------------------------------
      *  IF VOIDING, BUT ISSUING A REPLACEMENT CHECK,
      *  ONLY CHANGE THE CHECK NO.
      *-----------------------------------------------
           IF CX-CKNO-NEW NOT = 0
              IF CX-CKNO-NEW NOT = 202020
                 MOVE CX-CKNO-NEW TO DT-CKNO
                 PERFORM REWRITE-DT4-FILE
                 GO TO REVERSE-DT-PROCEEDS-NEXT.


      * MULTIPLE CKS MEAN WE MUST MATCH CORRECT DT TO CX BY SEQ
      * THESE ARE SYSTEM CHECKS REQUESTED BY LOAN PROC
      *  N O T     M E A N I N G F U L   H E R E  09/12/00
      *    IF DT-SEQ NOT = CX-DT-SEQ
      *          GO TO REVERSE-DT-PROCEEDS-NEXT.

           IF CX-BATCHID = 0
              ADD CX-AMT TO DT-PAYMENTS
           ELSE
              MOVE 0 TO DT-PAYMENTS.

           MOVE " " TO DT-STATUS.
           MOVE " " TO DT-SELECTED.
           MOVE "V" TO DT-CHECK-VOID.
      *    MOVE " " TO DT-UNPAID.
           IF DT-BATCHID NOT = 0
              MOVE "B" TO DT-PAID.

           PERFORM REWRITE-DT4-FILE.

           GO TO REVERSE-DT-PROCEEDS-NEXT.

       EXIT-REVERSE-DT-PROCEEDS.
           PERFORM CLOSE-DT4-FILE.


      **************************************************
      *    UPDATE TODAYS TOTALS
      **************************************************
       UPDATE-TY-FILE SECTION.

           MOVE "UPDATING TODAY'S TOTAL FILE............." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-TY1-FILE.
       FIND-TY1-FILE.
           MOVE CX-BRNO TO TY-BRNO.
           MOVE TRANS-DATE TO TY-DATE.
           MOVE CX-CLASS TO TY-CLASS.
           PERFORM READ-TY1-FILE.
           IF IO-FG = 0
              GO TO TY-UPDATE-REC.

           PERFORM CLEAR-TY-FILE.
           MOVE CX-BRNO TO TY-BRNO.
           MOVE TRANS-DATE TO TY-DATE.
           MOVE CX-CLASS TO TY-CLASS.
           PERFORM WRITE-TY1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           GO TO FIND-TY1-FILE.

      *----------------------------------------------
      * TY-CKWRITTEN TABLE OF 8 :
      *    1  =  LOAN   CHECKS  [LN]
      *    2  =  DEALER CHECKS  [DL, DR, DP, DC, VX]
      *    3  =  MISC   CHECKS  [MS]
      *    4  =  OVERPAYMENTS   [  FUTURE]
      *    5  =  NSF            [  FUTURE]
      *    6  =  WITHDRAWALS    [  ]
      *    7  =  INTEREST DEP   [  ]
      *    8  =  NOTES PAYABLE  [NP]
      * GLB  03/21/03
      *    3  =  DRAWER DEPOSIT CHECKS  [DD]
      *----------------------------------------------
       TY-UPDATE-REC.
      *    MOVE PAYEE-SUB TO SUB.
           MOVE 0                 TO SUB.
           IF (LOANCK   )       MOVE 1 TO SUB.
           IF (DEALER)          MOVE 2 TO SUB.
           IF (MISCCK)          MOVE 3 TO SUB.
           IF (DRAWCK)          MOVE 3 TO SUB.
           IF (DEALER-RSV-REC)  MOVE 0 TO SUB.
           IF (DEALER-PROC-REC) MOVE 0 TO SUB.
           IF (BANKTR)
              IF CX-DEPOSIT-WITHDR = "D"
                 MOVE 7 TO SUB
              ELSE
                 MOVE 6 TO SUB.

           IF (INTERDEP)
              MOVE 7 TO SUB.
           IF (INTERWITH)
              MOVE 6 TO SUB.

           IF SUB NOT = 0
              ADD CX-AMT TO TY-CKWRITTEN(SUB).

      *--------------------
      * PAYEE-SUB 6 = DR, RR
      * 5-27-04 CHANGED TO SUBTRACT NOT ADD FOR RR
      *--------------------
           IF CX-TYPE = "DR"
              SUBTRACT CX-AMT FROM TY-DLRVPRIN(TYNET CX-DLRVNO).
           IF CX-TYPE = "RR"
              SUBTRACT CX-AMT FROM TY-DLRVPRIN(1 CX-DLRVNO)
                                   TY-DLRVPRIN(2 CX-DLRVNO).
      *--------------------
      * PAYEE-SUB 7 = DC, RC
      * 5-27-04 CHANGED TO SUBTRACT NOT ADD FOR RC
      *--------------------
           IF CX-TYPE = "DC"
              SUBTRACT CX-AMT FROM TY-DLRVCHGS(TYNET CX-DLRVNO).
           IF CX-TYPE = "RC"
              SUBTRACT CX-AMT FROM TY-DLRVCHGS(1 CX-DLRVNO)
                                   TY-DLRVCHGS(2 CX-DLRVNO).
      *--------------------
      * PAYEE-SUB 8 = DP, RP
      * 5-27-04 CHANGED TO SUBTRACT NOT ADD FOR RP
      *--------------------
           IF CX-TYPE = "DP"
              SUBTRACT CX-AMT FROM TY-DLPARVHELD(TYNET).
           IF CX-TYPE = "RP"
              SUBTRACT CX-AMT FROM TY-DLPARVHELD(1)
                                   TY-DLPARVHELD(2).

      *--------------------
      *? RD, DEALER PROCEEDS
      * 5-27-04 CHANGED TO SUBTRACT NOT ADD FOR RD
      *--------------------
           IF CX-TYPE = "RD"
              SUBTRACT CX-AMT FROM TY-PROCEEDS.

           PERFORM REWRITE-TY1-FILE.

       EXIT-UPDATE-TY1-FILE.
           PERFORM CLOSE-TY1-FILE.

      ***********************************************
      *    UPDATE TEMPORARY CHECK FILE
      ***********************************************
       UPDATE-CX1-FILE SECTION.
           MOVE "DELETING CHECK REQUEST FILE......." TO MESS.
           PERFORM SEND-LEGEND.

      * IN A15, IT READS THE CX WITH NOTHING IN THE KEY AND JUST GOES TO
      * THE EXIT FOR IO-FG 9 DUE TO MANUAL CHECK NOT CREATING A CX RECORD.
      * HORRIBLE WAY TO DO THIS!
           IF LPWSFM-EDIT-FM-TYPE = SPACES
              GO TO EXIT-UPDATE-CX1-FILE.

           MOVE LPWSFM-EDIT-FM-BRNO  TO CX-BRNO.
           MOVE LPWSFM-EDIT-FM-TYPE  TO CX-TYPE.
           MOVE LPWSFM-EDIT-FM-SEQNO TO CX-SEQNO.
           PERFORM READ-CX1-FILE.
           IF IO-FG = 9
              GO TO EXIT-UPDATE-CX1-FILE.

           IF CX-PAID = "B"
              IF CX-REF = CK-REF
                 PERFORM DELETE-CX1-FILE
              ELSE
                 IF CX-TYPE = "BK" OR "CA" OR "ID" OR "IW"
      * CX-REF = 99001 AND LN-ACCTNO = 99000118 - HAS PAYEE-SUB ON THE END
      *   ITS TOO LONG TO TEST, MOVE TO A 9(6) AND TEST
                    MOVE LN-ACCTNO TO TEST-ACCTNO
                    IF CX-REF = TEST-REF-ACCTNO
                       PERFORM DELETE-CX1-FILE.
           IF CX-PAID NOT = "B"
      * 2/20/2002 BAH   "V" CAN REPRINT AS SYSTEM, IF ORIGINAL WAS MANUAL
      * 8/18/2016 BAH HAVE TO TEST LOAN-KEY-SEQNO BECAUSE CX-REF IS ONLY
      *           6 CHARACTERS EXAMPLE: 990007 AND LN-NUMBER IS NOW 8 AND THEY
      *           WILL NEVER MATCH, LN-NUMBER WILL BE 99000703 03=MS CHECK
      *           HOWEVER, I STILL WANT TO TEST LN-NUMBER FOR OTHER TYPE CHECKS
      *           SO I AM LEAVING IT AS AN "OR" STATEMENT
              IF HOLD-CX-PAID NOT = "M" OR CX-PAID = "V"
                 IF CX-REF = LN-ACCTNO OR
                     CX-REF = LOAN-KEY-SEQNO
                    PERFORM DELETE-CX1-FILE.
       EXIT-UPDATE-CX1-FILE.
           EXIT.

       UPDATE-CA-FILE SECTION.

           MOVE "UPDATING CASH TILL...................." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-CA1-FILE.
           MOVE CX-BRNO    TO CA-BRNO.
      *  GLB ???? BR-ALLOW-SYS-AS-EFFDATE
           MOVE TRANS-DATE TO CA-DATE.
           MOVE CX-TILLNO  TO CA-TILLNO.
           PERFORM READ-CA1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

      *NOTE: IF INCOMING CASH RECEIPT, CX-AMT WILL
      *      ALREADY BE REVERSED FOR PROPER EOD G/L
           IF (DRAWCK) OR (INTERWITH)
              ADD CX-AMT TO CA-RUNNING
           ELSE
              SUBTRACT CX-AMT FROM CA-RUNNING.
           PERFORM REWRITE-CA1-FILE.
           PERFORM CLOSE-CA1-FILE.
       EXIT-UPDATE-CA.
           EXIT.
