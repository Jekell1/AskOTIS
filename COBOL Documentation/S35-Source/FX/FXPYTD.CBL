       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FXPYTD.
       DATE-WRITTEN.    121187.
      **************************************************************************
      *  DESCRIPTION:  FIX PAYMENT-TO-DATE    DETERMINED BY SPR
      *
      * REV:
      *  JTG 052394 CHANGED TO CONSIDER LP-TRCD = 'AP' IN COMPUTING
      *             LN-TOTPAYMNTD
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  JTG 060497 ADDED TEST FOR "R3" & "C3"
      *  JTG 112597 CORRECTED FOR ALL REBATES, O2 MONIES AND EXCEPTION PAYMENTS
      *  BAH 001018 WALK ACROSS BRANCHES
      *  BAH 021014 ADDED NEW TRANSACTION CODE OF "IN" INSURANCE LOSS, EXACTLY
      *             LIKE "PE", REGACC PR# 0007
      *   CS 131003 INITIAL CALCULATION OF LN-TOTPAYMNTD INCLUDED EXCEPTION
      *             LN-LNAMT - (CURBAL - OT2) AND NEVER GOT SUBTRACTED OUT SO
      *             PAYMNTD OVERSTATED BY EXCEPTION PAYMENTS, SO HARD TO
      *             BELEIVE WE WERE WRONG ALL THIS TIME, PL819 JUDY
      *   CS 131031 FIXED ABOVE PL819,SUBTRACT LPTR-CODES-EXCEPTIONS FROM
      *             LN-TOTPAYMNTD.  AND ADDED THE 'SS' TO LPTR-CODES-AMOUNT
      *                                                         PR#4407
      *   CS 131104 LOANS IN AN IB STATUS WERE NOT CORRECT IF THEY HAD
      *             ANY ACCOUNTS WITH THE LP-TRCODES.  I ADDED A TEST TO SEE
      *             IF LOAN WERE IN IB ADD LP-APINT   PR#4407
      *  BAH 151029 INCLUDE "OP" TRANS CODE, SUPPORT PR#4897
      *  BAH 181218 GLOBAL LNFILE
      *  BAH 181219 GLOBAL LPFILE
      *  BAH 190918 REMOVED ACCESS-CALL ON LNFILE
      * BAH 2022.1004 HARD CODED STARTS
      * BAH 2024.0130 CALL CRNOR2 AND WRITE NOTICE RECORD
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************
      *      WORKING STORAGE SECTION
      ******************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/FXPYTD.CBL S35 01/30/24-08:09:41 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".

       01  BRANCH               PIC 9(4).
       01  HOLD-SYSDATE         PIC 9(8).

       01  LPTR-CODES-AMOUNT       PIC S9(7)V99.
       01  LPTR-CODES-EXCEPTIONS   PIC S9(7)V99.

       01  ERRCD             PIC X VALUE "N".

       01  ACCEPT-BUF        PIC X        VALUE " ".

       01  MESS-LIST         PIC X(5) VALUE "MESS.".
       01  MESS-BUF          PIC X(40) VALUE SPACES.

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       01  FXPYTD-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/FXPYTD_DEF.CPY".
       COPY "LIBFX/FXPYTD_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBFX/FXPYTD_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      **********************************
      *       MAIN PROGRAM CONTROL
      **********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE " " TO ERRCD.

           MOVE "FXPYTD" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FXPYTD-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           MOVE EXT-CONAME-SYSDATE TO  ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYSDATE.

       ENTER-BRANCH.
           MOVE "ENNN040BRNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "16"
              MOVE 0 TO BRANCH
              MOVE "S  A040BRNO." TO VDU
              MOVE "*ALL" TO VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-ACCEPT.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-BRANCH.
           MOVE VDUNUM0 TO BRANCH.
           IF BRANCH = 0
              GO TO ENTER-BRANCH.

       ENTER-ACCEPT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-BRANCH.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ACCEPT.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N"
              GO TO END-ROUTINE.
           IF NOT (ACCEPT-BUF = "Y" OR "S")
              GO TO ENTER-ACCEPT.

           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-BR-FILE.
           IF BRANCH = 0
              MOVE 0 TO BR-NO
           ELSE
              MOVE BRANCH TO BR-NO.

           PERFORM START-BR-FILE.

       NEXT-BR.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF BRANCH NOT = 0
              IF BR-NO NOT = BRANCH
                 GO TO END-ROUTINE.

       NEXT-BR-ACCEPT.
           IF ACCEPT-BUF = "S"
              MOVE "A" TO ACCEPT-BUF
           ELSE
           IF ACCEPT-BUF = "A"
              MOVE "E  A010ACCEPT." TO VDU
              PERFORM SCREENIO
              IF VDUSTATUS = "1>"
                 GO TO END-ROUTINE
              ELSE
                 IF NOT (VDUBUF = "Y" OR "S")
                    GO TO NEXT-BR-ACCEPT.


           IF BR-DATA-PATH = " "
                  OR BR-MACHINE = " "
              STRING "BRANCH "
                        BR-NO
                          " MISSING DATA PATH, WILL SKIP"
                                 DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO NEXT-BR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              NO-PATH-OVERRIDE.

           PERFORM GLOBAL-UPDATE.

           GO TO NEXT-BR.

      ******************************************
       GLOBAL-UPDATE SECTION.
      ******************************************

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-NO1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE 0             TO LN-ACCTNO.

           PERFORM START-LN1-FILE.

           STRING "PROGRAM IN PROGRESS.........BRNO: " BR-NO
                  DELIMITED BY SIZE INTO MESS-BUF.
           PERFORM SEND-LEGEND.

       NEXT-LN.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO GLOBAL-UPDATE-EXIT.

      * SKIP DELETED LOANS
           IF ( LN-POCD-NONLOAN )
      *       UNLOCK LN-FILE
              GO TO NEXT-LN.

           MOVE LN-REC TO HOLD-LN-REC.

           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.
           PERFORM READ-SP1-FILE
           IF IO-FG = 9
      *       UNLOCK LN-FILE
              GO TO NEXT-LN.

      * RECOMPUTE PAYMENTS TO DATE
      * ALWAYS ADD INTEREST
      * ONLY ADD LATE CHARGES IF SPR CONTRACTUAL FORMULA = 'A'
           COMPUTE LN-TOTPAYMNTD =
               LN-LNAMT - (LN-CURBAL - LN-OT2BAL) + LN-TOTINT.
           IF SP-CONTRFRMLA = "A"
              ADD LN-TOTLCHG TO LN-TOTPAYMNTD.

           PERFORM GET-LPTR-CODES.
           SUBTRACT LPTR-CODES-AMOUNT FROM LN-TOTPAYMNTD.
           MOVE LPTR-CODES-EXCEPTIONS TO LN-TOTEXCPAYMNTD.

      * REWRITE THE LOAN RECORD IF IT CHANGED

           IF HOLD-LN-REC NOT = LN-REC
              PERFORM REWRITE-LN1-FILE
              PERFORM UPDATE-NOTICE-FILE.

           GO TO NEXT-LN.

       GLOBAL-UPDATE-EXIT.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-NO1-FILE.

      *******************************************
       GET-LPTR-CODES SECTION.
      *******************************************
           MOVE 0 TO LPTR-CODES-AMOUNT
                     LPTR-CODES-EXCEPTIONS.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 0             TO LP-SEQNO
                                 QLP1-WBEG-SEQNO.
           MOVE ALL "9"       TO QLP1-WEND-SEQNO.

           PERFORM START-LP1-FILE.

       GET-LPTR-CODES-NEXT.
           PERFORM READ-LP1-FILE-NEXT.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
                      OR (LN-ACCTNO NOT = LP-ACCTNO)
              GO TO GET-LPTR-CODES-EXIT.

           IF LP-REV = "Y"
              GO TO GET-LPTR-CODES-NEXT.

           IF LP-TRCD = "PE" OR "SS" OR "Z2" OR "IN"
               ADD LP-APCUR TO LPTR-CODES-EXCEPTIONS
               MOVE HOLD-SYSDATE TO IBPC-DATE
               PERFORM IBPC-TEST
               IF IBPC-FG = "I"
                   ADD LP-APINT TO LPTR-CODES-EXCEPTIONS.

           IF LP-TRCD = "AP" OR "RD" OR "PE" OR "IN" OR "SS"
                       OR "RI" OR "RS" OR "RM"
                        OR "RC" OR "RA" OR "RP"
                         OR "CC" OR "CA" OR "CP"
                          OR "C1" OR "C2" OR "C3" OR "C4" OR "C5"
                           OR "R1" OR "R2" OR "R3" OR "R4" OR "R5"
                            OR "OP"
                   ADD LP-APCUR TO LPTR-CODES-AMOUNT
                   MOVE HOLD-SYSDATE TO IBPC-DATE
                   PERFORM IBPC-TEST
                   IF IBPC-FG = "I"
                      ADD LP-APINT TO LPTR-CODES-AMOUNT.

           IF LP-TRCD = "NP" OR "LP" OR "RT" OR "ET" OR "ST"
              SUBTRACT LP-APCUR FROM LPTR-CODES-AMOUNT.

           GO TO GET-LPTR-CODES-NEXT.

       GET-LPTR-CODES-EXIT.
           EXIT.

      ***********************************************
       UPDATE-NOTICE-FILE SECTION.
      ***********************************************
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO CRNOR2-ACCTNO.
           PERFORM READ-NO1-FILE.

           MOVE HOLD-SYSDATE   TO CRNOR2-SYSDATE.
           MOVE "CL/CRNOR2"    TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.
           IF IO-FG = 0
              PERFORM REWRITE-NO1-FILE
           ELSE
              PERFORM WRITE-NO1-FILE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPMDTE.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO FXPYTD-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FXPYTD-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY FXPYTD-SCREEN UPON FXPYTD-WINDOW-HANDLE.
           MOVE FXPYTD-FORM-FIELDS TO WR-BUF.
           MOVE FXPYTD-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE FXPYTD-HELP--FILE TO HELP-LINK-FILE.
           MOVE FXPYTD-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/FXPYTD_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************
       MISC-PARA SECTION.
      ******************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE MESS-BUF TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************
       IO-ROUTINE SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *******************************************
       END-ROUTINE SECTION.
      *******************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "FX/FXMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FXPYTD-SCREEN.
           DESTROY FXPYTD-WINDOW-HANDLE.
           EXIT PROGRAM.
