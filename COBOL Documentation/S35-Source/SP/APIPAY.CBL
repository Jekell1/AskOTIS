       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       APIPAY.
       DATE-WRITTEN. 08/16/2023.
      *************************************************************************
      *  DESC:  SINGLE PAYMENT REFERENCE CODES PROCESSING
      *         $ SIGN RUN, NO SCREENS
      *
      * FOR PROGRAMMERS TO RUN IT;
      *         . SHH
      *         SH ./APIPAY.SH (DATA STRING OF BT-REC)     (REPAY TRANS ID)
      *                        00100786740000001000CUACH090123OT 1234567890
      *
      * RETURN-STATUS = 0 = SUCCESSFUL UPDATE
      * NO REPORTS ARE CREATED, ONLY A LOG
      * CREATES SC9990/EO/APIMMDDYY/0000LOG
      *
      *       2023/08/18 01:04 PM 0010 078674 BEGIN SINGLE BATCH PAYMENT POSTING
      *  PASS 2023/08/18 01:04 PM 0010 078674 SUCCESSFUL UPDATE
      *       2023/08/18 01:04 PM 0010 033333 BEGIN SINGLE BATCH PAYMENT POSTING
      *  FAIL 2023/08/18 01:04 PM 0010 033333 ACCOUNT NOT ON FILE, ABORTED
      *
      * LOGIC WAS TAKEN FROM REFUPD AND LONPW9 TO CREATE THIS PROGRAM
      * LOGIC WAS TAKEN FROM OTHUPD AND LONPWA TO CREATE THIS PROGRAM (OT)
      *
      *  FOR "RP" - NEEDED FOR AN "IN" REBATE FIX (LONPFB)
      *      "PL" "P2" "P3" (LONPF9) - MUST ADD BT-PL-REASON TO INPUT STRING
      *      "RE" (REPO) "O2" (LONPF7)
      *      "RV" "BK" "BD" CALL (LONPF2)
      *      "PY" "PA" ?  (LONPFC)
      *      "OT"         (LONPF7)
      *
      * 2024.0206 BAH #1641
      * BAH 2025.0228 LXE-EARN(4) - (7) WERE NOT GETTING CLEARED LIKE
      *               LONPF1 DOES! S35Q-168
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSWK.CPY".
       COPY "LIBLP/LPFSOP.CPY".
           SELECT LOG-FILE ASSIGN LOG-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBLP/LPFDWK.CPY".
       COPY "LIBLP/LPFDOP.CPY".

      **************************************************
      *  REFCODE BATCH LOG FILE                      *
      *   EO/APIMMDDYY/0000LOG
      **************************************************
       FD  LOG-FILE
           LABEL RECORDS ARE STANDARD.
       01  LOG-REC.
      * PASS OR FAIL
           03  LOG-STATUS    PIC X(4).
           03  FILLER            PIC X.
           03  LOG-RETURN    PIC 9(4).
           03  FILLER            PIC X.
           03  LOG-DATE      PIC 9999/99/99.
           03  FILLER            PIC X.
           03  LOG-TIME      PIC X(8).
           03  FILLER            PIC X.
           03  LOG-BRNO      PIC 9(4).
           03  FILLER            PIC X.
           03  LOG-NUMBER    PIC 9(6).
           03  FILLER            PIC X.
           03  LOG-MSG       PIC X(38).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/APIPAY S35 02/12/24-15:54:03 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LP01LX_SQL.CPY".
       COPY "LIBLP/LPWSLX.CPY".
       COPY "LIBLP/LP01LXE.CPY".
       COPY "LIBLP/LP01LXE_SQL.CPY".
       COPY "LIBLP/LPWSLXE.CPY".
       COPY "LIBLP/LP01LXG.CPY".
       COPY "LIBLP/LP01LXG_SQL.CPY".
       COPY "LIBLP/LPWSLXG.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       COPY "LIBLP/LP01BP.CPY".

       01  WK-IFN              PIC X(2)     VALUE "WK".
       01  LOG-PATH.
           03  LOG-SPOOL-DIR      PIC X(32).
           03  LOG-PATHNAME       PIC X(10) VALUE "/0000LOG".

       01  RETURN-STATUS         PIC 9(6) VALUE 0.

       01  HOLD-LN-TOTPAYMNTD              PIC S9(7)V99 COMP-3.
       01  HOLD-PROG-BUF-BRANCH            PIC 9(4)    COMP.
       01  HOLD-BP-PAYOFF-NONCASH          PIC X VALUE " ".
       01  HOLD-LBOX-PAYOFF-NONCASH        PIC X VALUE " ".
       01  HOLD-BP-ALLOW-PMT-ACCT-OTHBAL   PIC X VALUE " ".
       01  HOLD-LBOX-ALLOW-PMT-ACCT-OTHBAL PIC X VALUE " ".
       01  HOLD-CD-BR-POST-SS-TRCD         PIC X VALUE " ".
       01  WK-KEY              PIC 9(2)     VALUE 0.
       01  POSTING-ERRCD       PIC XX       VALUE " ".
       01  PAYMENT-PROG        PIC X(6)     VALUE SPACES.

       01  TOT-POFF-REBATES    PIC S9(9)V99 COMP.

       01  ORIG-LN-LBOX        PIC X(01)    VALUE " ".
       01  ORIG-LN-ALLOTCD     PIC X(01)    VALUE " ".
       01  ORIG-BP-TRAMT       PIC S9(9)V99 VALUE 0.
       01  ORIG-LN-OT2BAL      PIC S9(9)V99 VALUE 0.

      * EXAMPLE; A0201TST
       01  ORIG-POSTNAME       PIC X(10).

       01  HOLD-EXT-FILPATH.
           03  FILLER                PIC X(13).
           03  HOLD-EXT-FILPATH-DATA PIC X(6).

      *-----------------------------------------------------------------
      * SPOOL DIRECTORY/FILENAME FOR LOG
      * EO/APIMMDDYY/0000LOG
      *-----------------------------------------------------------------
       01  EX-SPOOL.
           03  EX-SPOOL-BASE          PIC X(09).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  EX-SPOOL-MACHINE       PIC X(02).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  EX-SPOOL-DATA-PATH PIC X(6).
           03  FILLER             PIC X(04)  VALUE "/EO/".
           03  EX-SPOOL-PCODE     PIC X(03)  VALUE "API".
           03  EX-SPOOL-DATE      PIC 9(06).
           03  EX-SPOOL-FILE.
               05  FILLER         PIC X(01).
               05  EX-SPOOL-STEP  PIC 9(04).
               05  EX-SPOOL-NAME  PIC X(06).
               05  EX-SPOOL-SUF   PIC X(02).

       01  SAVE-EX-SPOOL-NAME     PIC X(06).
       01  STEP-COUNT             PIC 9999 VALUE 100.

       01  WORK-USERID.
           03  WORK-SECURITY        PIC X.
           03  WORK-BR              PIC 9(4).
           03  WORK-NAME            PIC X(5).

       01  PAYOFF-TRANS-FG      PIC X        VALUE " ".

       01  SUB                 PIC 9(6)      COMP-3 VALUE 0.
       01  SUB1                PIC 9(6)      COMP-3 VALUE 0.
       01  OVPAID-FLAG         PIC X         VALUE " ".
       01  OVPAID-AMT          PIC S9(9)V99.
       01  DISPLAY-OVPAID-AMT  PIC Z(6)9.99-.

       01  TEST-AMT                PIC S9(9)V99  COMP   VALUE 0.
       01  NET-POFF-PRIN           PIC S9(9)V99  COMP   VALUE 0.
       01  WILL-POFF-ACCOUNT       PIC S9(9)V99  COMP   VALUE 0.
       01  VALID-POSTING-NOPOFF    PIC S9(9)V99  COMP   VALUE 0.

       01  DF-TEST             PIC X         VALUE " ".
           88 DF-POSTING                     VALUE "Y".

      * REVERSAL WORKERS
       01  VALID-REVERSAL-FG           PIC X VALUE " ".
       01  REVERSE-AFTER-RESCHED-FG    PIC X VALUE "N".
       01  REV-REFNO           PIC X(5)             VALUE " ".
       01  REV-TRCD            PIC XX.
           88  AD-POSTING      VALUE "AD" "A2" "A3" "A4"
                                     "A5" "A6" "A7" "A8"
                                     "A9".
           88  DF-POSTINGX     VALUE "DF" "D2" "D3" "D4"
                                     "D5" "D6" "D7" "D8"
                                     "D9".
           88  ADDON-POSTING   VALUE "LP" "NP" "ET" "RT" "ST".
           88  REBATE-POSTING  VALUE "RC" "RA" "RP" "R1"
                                     "R2" "R3" "RI" "RS"
                                     "RM" "RD".
       01  HOLD-REV-TRCD      REDEFINES REV-TRCD.
           03 FILLER           PIC X.
           03  TRCD-FTST       PIC X.
           03  TRCD-NODF      REDEFINES TRCD-FTST
                               PIC 9.

       01  CDV-BUF           VALUE " ".
           03  CDV-CD        PIC X(2) OCCURS 83.
       01  CDV-SUB           PIC 9(6) COMP-3.
           88 CDV-NO-MATCH   VALUE 0.
           88 CDV-MATCH      VALUE 1 THRU 100.
       01  CDV-17            PIC X(40) VALUE
           "OTWOAPJDPLP2P3P4P5PIPWTSTPSPACO2W2Z2BKBD".

       COPY "LIBLP/LPBP_CMD.CPY".
       COPY "LIBLP/LPBPPATH.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/DEFPOLW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPEDPATH.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       01  BT-REC.
           03  BT-BRANCH             PIC 9(04).
           03  BT-ACCTNO             PIC 9(06).
           03  BT-AMT                PIC 9(08)V9(02).
           03  FILLER REDEFINES BT-AMT.
               05  BT-SIGN           PIC X.
               05  BT-AMT2           PIC 9(07)V9(02).
           03  BT-SIGNED-AMT REDEFINES BT-AMT
                                     PIC S9(08)V9(02).
           03  BT-REFCD              PIC X(05).
           03  BT-PAYDATE            PIC 9(06).
           03  BT-TRCD               PIC X(2).


       01  FORM-PATH.
           03  FILLER         PIC X.
           03  FORM-REL       PIC X(7).
           03  FILLER REDEFINES FORM-REL.
               05  FILLER     PIC X(4).
               05  LOGUID-REL PIC X(3).
           03  FILLER         PIC X.
           03  FORM-OBJ       PIC XX.
           03  FILLER         PIC X.
           03  FORM-NAM.
               05  FORM-DIRX  PIC XX.
               05  FILLER     PIC X.
               05  FORM-PROGX PIC X(6).
           03  FILLER         PIC X.
       01  FORM-PATHNAME     PIC X(22).
       01  EXIT-PATHNAME     PIC X(22).
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       01  INPUT-STRING              PIC X(33).
       01  REPAY-TRANS-ID            PIC 9(10).


       PROCEDURE DIVISION 
             CHAINING INPUT-STRING REPAY-TRANS-ID.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              WK-FILE OP-FILE LOG-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LXE1-FILE.
           PERFORM CLOSE-LXG1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
           CLOSE WK-FILE OP-FILE LOG-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *  PROGRAM CONTROL
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

      D    STOP MESS.

      * NEED TO GET EXTERNAL VARIABLES SET
           MOVE "CO" TO FORM-OBJ.
           MOVE "GB/SETENV" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE 99 TO RETURN-STATUS.

           PERFORM GET-GPENV.

           MOVE EXT-FILPATH TO HOLD-EXT-FILPATH.
           ACCEPT ORIG-POSTNAME FROM ENVIRONMENT "POSTNAME".

      * THIS COULD CHANGE TO USE THE BP-REC WITH MORE FIELDS. I USED SIMPLEST TO
      * START OFF WITH. LOOK AT WHAT IS IN BATPAY.SH IN SH DIRECTORY

           MOVE INPUT-STRING TO BT-REC.

           MOVE BT-ACCTNO    TO BP-LNNO.
           MOVE 0            TO BP-SSNO.
           MOVE BT-AMT       TO BP-TRAMT.
           MOVE BT-REFCD     TO BP-REFCD.
           MOVE BT-PAYDATE   TO DATE-MMDDYY
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BP-PAYDATE.
           MOVE BT-TRCD      TO BP-TRCD.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE BT-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              MOVE 2                TO RETURN-STATUS
              MOVE "BRANCH &&&& NOT ON FILE, ABORTED" TO LOG-MSG
              INSPECT LOG-MSG REPLACING FIRST "&&&&" BY BT-BRANCH
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

           PERFORM OPEN-CD1-FILE.
           MOVE "BR"      TO CD-TYPE.
           MOVE BP-REFCD  TO CD-CODE.
           PERFORM READ-CD1-FILE.
           IF (IO-BAD) OR (CD-BR-FILE-TYPE = "LC" OR "C")
              MOVE 3                TO RETURN-STATUS
              MOVE "REF CODE &&&&& NOT ON FILE, ABORTED" TO LOG-MSG
              INSPECT LOG-MSG REPLACING FIRST "&&&&&" BY BP-REFCD
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

           IF CD-BR-PAYOFF-FG = "Y"
              MOVE "N" TO HOLD-BP-ALLOW-PMT-ACCT-OTHBAL
                          HOLD-LBOX-ALLOW-PMT-ACCT-OTHBAL
              MOVE "Y" TO HOLD-BP-PAYOFF-NONCASH
                          HOLD-LBOX-PAYOFF-NONCASH
           ELSE
              MOVE BR-BP-ALLOW-PMT-ACCT-OTHBAL TO
                                    HOLD-BP-ALLOW-PMT-ACCT-OTHBAL
              MOVE BR-LBOX-ALLOW-PMT-ACCT-OTHBAL TO
                                    HOLD-LBOX-ALLOW-PMT-ACCT-OTHBAL
              MOVE BR-BP-PAYOFF-NONCASH TO
                                    HOLD-BP-PAYOFF-NONCASH
              MOVE BR-LBOX-PAYOFF-NONCASH TO
                                    HOLD-LBOX-PAYOFF-NONCASH.

           MOVE CD-BR-LC-FG TO BATCH-REFCD-LC-FG.

      * ONLY PROCESS FILES FOR THE MACHINE YOU ARE ON

           IF BR-MACHINE NOT = EXT-FILPATH-MACHINE
              MOVE 4                TO RETURN-STATUS
              MOVE "BRANCH NOT ON THIS MACHINE, ABORTED" TO LOG-MSG
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              BP-PATH-OVERRIDE
                                              GB-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              LXE-PATH-OVERRIDE
                                              LXG-PATH-OVERRIDE.


      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              MOVE 5                       TO RETURN-STATUS
              MOVE "GLOBAL REC MISSING FOR &&&&, ABORTED" TO LOG-MSG
              INSPECT LOG-MSG REPLACING FIRST "&&&&" BY BT-BRANCH
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.

      * CHANGE THE EXTERNAL FILPATH TO THAT OF THE CURRENT BRANCH.

           MOVE BR-MACHINE            TO EXT-FILPATH-MACHINE.
           MOVE BR-DATA-PATH          TO EXT-FILPATH-DATA.
           SET ENVIRONMENT "FIL"      TO EXT-FILPATH-FIL.
           MOVE ORIG-POSTNAME         TO WORK-USERID.
           MOVE BR-NO                 TO WORK-BR.
           SET ENVIRONMENT "POSTNAME" TO WORK-USERID.
           MOVE BR-NO                 TO BRANCH.

      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      * WARNING * WARNING * WARNING * WARNING * WARNING * WARNING * WARNING *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                                     *
      * FROM THIS POINT FORWARD THE USER HAS BE PLACED, LOGICALLY, INTO     *
      * THE BRANCH FOR WHICH HE/SHE IS POSTING FOR.                         *
      *                                                                     *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      * WARNING * WARNING * WARNING * WARNING * WARNING * WARNING * WARNING *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

      * TEST RC-STATUS FOR OPEN DAY

           MOVE "A1"       TO RC-STATUS.
           MOVE BR-NO      TO RC-BRNO.
           MOVE TRANS-DATE TO RC-TRANS-DATE.

           PERFORM OPEN-RC2-FILE.
           PERFORM READ-RC2-FILE.
           PERFORM CLOSE-RC2-FILE.
           IF IO-FG NOT = 0
              MOVE 6                       TO RETURN-STATUS
              MOVE "DAY NOT OPEN, ABORTED" TO LOG-MSG
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

      * DAY IS OPEN, RESERVE OP FILE SO THE DAY CANNOT BE CLOSED.

           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE BR-NO       TO OPEN-PATH-BRNO.
           MOVE DATE-MMDDYY TO OPEN-PATH-TRDATE.
           PERFORM LOAD-OPEN-FILE.
           MOVE OPEN-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "COULD NOT OPEN DAY" TO LOG-MSG
              MOVE 7                       TO RETURN-STATUS
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

           PERFORM OPEN-OP-FILE.

           IF (BP-PAYDATE = 20202020 OR BP-PAYDATE = 02020202
                 OR BP-PAYDATE = 0)
              MOVE TRANS-DATE TO BP-PAYDATE.

           MOVE " "      TO OVPAID-FLAG.
           MOVE 0        TO OVPAID-AMT HOLD-OVPAID-CHECK.
           MOVE BP-TRAMT TO ORIG-BP-TRAMT.
           MOVE " "      TO POSTING-ERRCD.

           IF (BR-AUTO-PL-REF-AC = "Y" OR "2" OR "3")
              IF (BP-TRCD = "PL" OR "P2" OR "P3")
                 MOVE "AUTO ACCELERATE FG:REJECT" TO LOG-MSG
                 MOVE 8                           TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * DONT ALLOW THE BACKDATE TO A PRIOR MONTH FOR PL TYPE TRANSACTION
           IF BP-TRCD =  "PL" OR "P2" OR "P3"
              MOVE BP-PAYDATE TO NUM-DATE
              MOVE TRANS-DATE TO SYS-DATE
              IF NOT (NUM-MO = S-MM AND NUM-CCYY = S-CCYY)
                 MOVE 9                              TO RETURN-STATUS
                 MOVE "BACK DATE IN PRIOR MO, ABORTED" TO LOG-MSG
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

           MOVE BR-NO   TO LN-OWNBR.
           MOVE BP-LNNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE 10                             TO RETURN-STATUS
              MOVE "ACCOUNT NOT ON FILE, ABORTED" TO LOG-MSG
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

           IF LN-ESCROW-FG = "Y"
              MOVE "ACCOUNT HAS ESCROW"  TO LOG-MSG
              MOVE 11                    TO RETURN-STATUS
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

           IF BP-TRCD = "RV"
              PERFORM VALIDATE-REVERSAL
              IF VALID-REVERSAL-FG NOT = "Y"
                 GO TO END-ROUTINE.

           IF LN-BNKRPTDATE NOT = 0
              IF BP-TRCD = "BK"
                 MOVE "BK: ACCT ALREADY BANKRUPT" TO LOG-MSG
                 MOVE 12                          TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

           IF LN-BNKRPTDATE = 0
              IF BP-TRCD = "BD"
                 MOVE "BD: ACCT NOT BANKRPT STAT" TO LOG-MSG
                 MOVE 13                          TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

           IF BP-TRCD =  "PL" OR "P2" OR "P3"
              IF LN-PLCD = "P"
                 MOVE "ACCT ALREADY ACTIVE P&L" TO LOG-MSG
                 MOVE 14                        TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

           IF BP-TRCD = "RE"
               IF GP-REPO-CODE NOT = "A"
                  MOVE "MUST HAVE REPO OPTIONS TURNED ON" TO
                                                          LOG-MSG
                  MOVE 15                            TO RETURN-STATUS
                  PERFORM CREATE-LOG
                  GO TO END-ROUTINE.

           IF BP-TRCD = "RE"
               IF NOT (LN-REPOCD = "X" OR "S")
                  MOVE "RE: ACCT NOT REPO STATUS!" TO LOG-MSG
                  MOVE 16                          TO RETURN-STATUS
                  PERFORM CREATE-LOG
                  GO TO END-ROUTINE.

           IF BP-TRCD = "RE"
              MOVE BP-PAYDATE TO IBPC-DATE
              PERFORM IBPC-TEST
              IF NOT (IBPC-FG = "I" OR LN-LOANTYPE = "I")
                 MOVE "RE: ACCT NOT INTEREST BEARING!" TO LOG-MSG
                 MOVE 17                             TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * "R" REJECT

           IF HOLD-CD-BR-POST-SS-TRCD = "R"
               IF LN-REPOCD = "X"
                  MOVE "REPO BATCH RECORD"   TO LOG-MSG
                  MOVE 18                    TO RETURN-STATUS
                  PERFORM CREATE-LOG
                  GO TO END-ROUTINE.

           IF HOLD-CD-BR-POST-SS-TRCD = "Y"
               IF LN-REPOCD NOT = "X"
                  MOVE "NON REPO BATCH RECORD!"  TO LOG-MSG
                  MOVE 19                        TO RETURN-STATUS
                  PERFORM CREATE-LOG
                  GO TO END-ROUTINE.

      * SAVE ORIG VALUES

           MOVE LN-LBOX    TO ORIG-LN-LBOX.
           MOVE LN-ALLOTCD TO ORIG-LN-ALLOTCD.
           MOVE LN-OT2BAL  TO ORIG-LN-OT2BAL.

      * SETUP LBOX/ALLOTCD HERE FOR CORRECT LATE CHARGE CALCULATIONS

           MOVE "N"        TO LN-LBOX.
           MOVE "Y"        TO LN-ALLOTCD.

      * CLEAR HOLD-BP-TRCD HERE:
      *  - IF AN EXCEPTION CAUSING A NON CASH TRASACTION REQUIREMENT
      *       IS FOUND AND BR-????-PAYOFF-NONCASH = "Y", THAN "99"
      *       WILL BE PUT INTO HOLD-BP-TRCD TO INDICATE A CALL TO
      *       LONPFA.C SHOULD BE DONE AND NOT A CALL TO LONPFC.C

           MOVE " " TO HOLD-BP-TRCD.

           MOVE BP-REFCD TO LP-REFNO.

      * ADDED ABILITY TO SEND NEGATIVES, IF 1ST BYTE OF AMT  IS "-",
      * BUT BP-ALLOW-NEGATIVE FLAG MUST ALSO BE Y, NEEDED FOR RE'S
      * (REGACC PR#382). ERROR IF NEG BUT FLAG NOT SET:

           IF BP-TRAMT < 0
              IF BP-ALLOW-NEGATIVE NOT = "Y"
                 MOVE "NEGATIVE BUT FLAG NOT SET" TO LOG-MSG
                 MOVE 20                          TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * TEST FOR PAYMENT AMOUNT = 0.00

           IF NOT (CD-BR-FILE-TYPE = "B" OR "P")
              IF BP-ALLOW-NEGATIVE NOT = "Y"
                 IF BP-TRAMT NOT > 0
                    IF (BP-TRCD = "DF" OR "D2" OR "D3" OR "D4"
                               OR "D5" OR "D6" OR "D7" OR "D8" OR "D9")
                        IF BP-ALLOW-ZERO-DF NOT = "Y"
                           MOVE "PAYMENT MUST BE > 0" TO LOG-MSG
                           MOVE 21                    TO RETURN-STATUS
                           PERFORM CREATE-LOG
                           GO TO END-ROUTINE
                        END-IF
                    ELSE
                        MOVE "PAYMENT MUST BE > 0" TO LOG-MSG
                        MOVE 21                    TO RETURN-STATUS
                        PERFORM CREATE-LOG
                        GO TO END-ROUTINE.

           IF HOLD-CD-BR-POST-SS-TRCD = "Y"
              IF BP-AUCTION-FEES = ZEROS
                 MOVE "MISSING AUCTION FEES" TO LOG-MSG
                 MOVE 22                     TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * TEST IF ODD PAYMENTS ARE NOT ALLOWED:
      * CHECK BR-BP-ODDPAY (NOT LBOX) IF ALLOTMENT

           IF ( (CD-BR-ALLOT-OPTION = "Y") AND
                (BR-BP-ODDPAY NOT = "Y"  )
              )
              OR
              ( (CD-BR-LBOX-OPTION = "Y" ) AND
                (BR-LBOX-ODDPAY NOT = "Y")
              )
              IF LN-1STPYAMT NOT = 0
                 IF LN-TOTPAYMNTD = 0
                    IF BP-TRAMT NOT = LN-1STPYAMT
                       MOVE "IRREGULAR PAYMENT AMOUNT" TO LOG-MSG
                       MOVE 23                       TO RETURN-STATUS
                       PERFORM CREATE-LOG
                       GO TO END-ROUTINE
                    ELSE
                       NEXT SENTENCE
                 ELSE
                    IF BP-TRAMT NOT = LN-REGPYAMT
                       MOVE "IRREGULAR PAYMENT AMOUNT" TO LOG-MSG
                       MOVE 23                         TO RETURN-STATUS
                       PERFORM CREATE-LOG
                       GO TO END-ROUTINE
                    ELSE
                       NEXT SENTENCE
              ELSE
                 IF BP-TRAMT NOT = LN-REGPYAMT
                    MOVE "IRREGULAR PAYMENT AMOUNT" TO LOG-MSG
                    MOVE 23                         TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.
    
      * WHEN BR-BP/LBOX-ALLOW-PMT-ACCT-OTHBAL = "Y", ALLOW THE BATCH
      * PAYMENT TO BE APPLIED TO THE ACCOUNT BUT NOT TO OTHER BALANCE

      * WHEN CD-BR-OT-GL-ACCT NOT = 0, ALLOW POSTING TO ACCOUNTS WITH
      * LN-OTHBAL, OTHERWISE, TEST IF OTHER CHARGES EXIST 

           IF ( CD-BR-OT-GL-ACCOUNTS = " " ) OR
              ( CD-BR-OT-GL-ACCT = 0       )
              IF LN-OTHBAL NOT = 0
                 IF ( ( CD-BR-ALLOT-OPTION = "Y"              ) AND
                      ( HOLD-BP-ALLOW-PMT-ACCT-OTHBAL NOT = "Y" )
                    )
                    OR
                    (
                      ( CD-BR-LBOX-OPTION = "Y"                 ) AND
                      ( HOLD-LBOX-ALLOW-PMT-ACCT-OTHBAL NOT = "Y" )
                    )
                    MOVE "LOAN HAS OTHER CHARGES" TO LOG-MSG
                    MOVE 24                       TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.


           IF LN-OT2BAL NOT = 0
              IF ( ( CD-BR-ALLOT-OPTION = "Y"              ) AND
                   ( HOLD-BP-ALLOW-PMT-ACCT-OTHBAL = "2" )
                 )
                 OR
                 (
                   ( CD-BR-LBOX-OPTION = "Y"                 ) AND
                   ( HOLD-LBOX-ALLOW-PMT-ACCT-OTHBAL = "2" )
                 )
                 MOVE "LOAN HAS OTHER 2 CHARGES" TO LOG-MSG
                 MOVE 25                         TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * TEST IF ACCOUNT BELONGS TO POSTING BRANCH:

           IF LN-OWNBR NOT = BR-NO
              MOVE "ACCOUNT OWNED BY OTHER BR" TO LOG-MSG
              MOVE 26                         TO RETURN-STATUS
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

           IF BP-FROZEN-OVERRIDE NOT = "Y"
              IF LN-ACCT-FROZEN = "Y"
                 MOVE "ACCOUNT IS FROZEN" TO LOG-MSG
                 MOVE 27                  TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * TEST IF PAYMENT WILL ONLY COVER LC AND INTEREST:
      * CHECK BR-BP-NOPRIN (NOT LBOX) IF ALLOTMENT

           IF ( (CD-BR-ALLOT-OPTION = "Y") AND
                (BR-BP-NOPRIN NOT = "Y"  ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y" ) AND
                (BR-LBOX-NOPRIN NOT = "Y") )
                 COMPUTE TEST-AMT = LN-INTBAL + LN-LCBAL
                 IF BP-TRAMT NOT > TEST-AMT
                    MOVE "APPLIES WITH NO PRINCIPAL" TO 
                                                  LOG-MSG
                    MOVE 28                    TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

      * TEST IF PL ACCOUNTS ARE ALLOWED:

           IF ( (CD-BR-ALLOT-OPTION = "Y") AND
                ( NOT (BR-BP-PL = "Y" OR "P") ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y" ) AND
                ( NOT (BR-LBOX-PL = "Y" OR "P") ) )
                 IF LN-PLDATE NOT = 0
                    MOVE "P&L ACCOUNT " TO LOG-MSG
                    MOVE 29             TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

           IF LN-PLCD = "I"
              MOVE "INACTIVE P&L ACCOUNT " TO LOG-MSG
              MOVE 30                      TO RETURN-STATUS
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

      * TEST IF JD ACCOUNTS ARE ALLOWED:
      * CHECK BR-BP-JD (NOT LBOX) IF ALLOTMENT

           IF ( (CD-BR-ALLOT-OPTION = "Y") AND
                (BR-BP-JD NOT = "Y"      ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y" ) AND
                (BR-LBOX-JD NOT = "Y"    ) )
              IF LN-JDDATE NOT = 0
                 MOVE "JUDGEMENT ACCOUNT " TO LOG-MSG
                 MOVE 31                   TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * TEST IF ACCOUNTS WITH MESSAGE ARE ALLOWED:
      * CHECK BR-BP-FLASH (NOT LBOX) IF ALLOTMENT

           IF ( (CD-BR-ALLOT-OPTION = "Y") AND
                (BR-BP-FLASH NOT = "Y"   ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y" ) AND
                (BR-LBOX-FLASH NOT = "Y" ) )
              IF LN-MESSAGE NOT = " "
                 MOVE "FLASHING MESSAGE  " TO LOG-MSG
                 MOVE 32                   TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * TEST IF ACCOUNTS WITH ACTION CODE ARE ALLOWED:

           IF ( (CD-BR-ALLOT-OPTION = "Y"  ) AND
                (BR-BP-ACTIONCD NOT = "Y"  ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y"   ) AND
                (BR-LBOX-ACTIONCD NOT = "Y") )
              IF LN-ACTIONCD NOT = " "
                 MOVE "ACTION CODE ON ACCOUNT" TO LOG-MSG
                 MOVE 33                       TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

           PERFORM GET-SPR.
           IF IO-FG NOT = 0
              MOVE "INVALID SPR RECORD" TO LOG-MSG
              MOVE 34                   TO RETURN-STATUS
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

      * REJECT IF THE INTEREST PAID THRU ON AN IB LOAN IS GREATER
      * THAN THE POSTING DATE

           MOVE BP-PAYDATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           IF (IBPC-FG = "I" OR LN-LOANTYPE = "I")
               IF LN-INTPDTH-DATE > BP-PAYDATE
                  MOVE "INTEREST PAID THRU DATE" TO LOG-MSG
                  MOVE 35                        TO RETURN-STATUS
                  PERFORM CREATE-LOG
                  GO TO END-ROUTINE.

           IF BP-TRCD = "DF" OR "D2" OR "D3" OR "D4" OR "D5"
                             OR "D6" OR "D7" OR "D8" OR "D9"
              MOVE "Y" TO DF-TEST
           ELSE
              MOVE " " TO DF-TEST.

           IF DF-POSTING
              IF BP-TRAMT < SP-DEFMIN OR
                      BP-TRAMT > SP-DEFMAX
                 MOVE "SPR DEFERMENT MIN/MAX" TO LOG-MSG
                 MOVE 36                      TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * ONLY ALLOWING REGIONAL'S SP-DEFFRMLA 0,2,3,96,97,99

           IF DF-POSTING
              IF NOT (SP-DEFFRMLA = 0 OR 2 OR 3 OR 96 OR 97 OR 99)
                 MOVE "DF FRMLA INVALID FOR BATCH" TO LOG-MSG
                 MOVE 37                         TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * NO MULTIPLES WITH 96 OR D7
           IF DF-POSTING
              IF SP-DEFFRMLA = 96 OR 97
                 IF BP-TRCD NOT = "DF"
                    MOVE "MULTIPLE DF'S NOT ALLOWED" TO LOG-MSG
                    MOVE 38                        TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

           IF DF-POSTING
             IF BP-IGNORE-DEFPOL NOT = "Y"
                MOVE BP-TRCD    TO DEFPOL-TRCD
                MOVE TRANS-DATE TO DEFPOL-DATE
                PERFORM VALIDATE-DF-POLICY
                IF DEFPOL-MAXIMUM = "X"
                   MOVE "DEFERMENTS WILL EXCEED ORIGINAL TERM" TO
                                                      LOG-MSG
                   MOVE 39                        TO RETURN-STATUS
                   PERFORM CREATE-LOG
                   GO TO END-ROUTINE
                ELSE
                IF DEFPOL-MAXIMUM = "R"
                   MOVE "MINIMUM MONTHS HAVE NOT ELAPSED" TO 
                                                  LOG-MSG
                   MOVE 40                    TO RETURN-STATUS
                   PERFORM CREATE-LOG
                   GO TO END-ROUTINE
                ELSE
                IF DEFPOL-MAXIMUM = "Y"
                   MOVE "DF NOT ALLOWED" TO LOG-MSG
                   MOVE 41               TO RETURN-STATUS
                   PERFORM CREATE-LOG
                   GO TO END-ROUTINE.

      * TEST IF MAXIMUM CONTRACTUAL IS EXCEEDED:

           MOVE BP-PAYDATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY.

            IF BP-TRCD = "PL" OR "P2" OR "P3"
               IF CONTRACTUAL < 30
                   MOVE "CONTRACTUAL < 30" TO LOG-MSG
                   MOVE 42                 TO RETURN-STATUS
                   PERFORM CREATE-LOG
                   GO TO END-ROUTINE.

            IF BP-TRCD = "PL" OR "P2" OR "P3"
               IF RECENCY < 30
                   MOVE "RECENCY < 30" TO LOG-MSG
                   MOVE 43             TO RETURN-STATUS
                   PERFORM CREATE-LOG
                   GO TO END-ROUTINE.

           IF CD-BR-ALLOT-OPTION = "Y"
              IF BR-BP-MAXCON NOT = 999
                IF CONTRACTUAL > BR-BP-MAXCON
                   MOVE "EXCEEDS MAX CONTRACTUAL" TO LOG-MSG
                   MOVE 44                        TO RETURN-STATUS
                   PERFORM CREATE-LOG
                   GO TO END-ROUTINE.

           IF CD-BR-LBOX-OPTION = "Y"
              IF BR-LBOX-MAXCON NOT = 999
                 IF CONTRACTUAL > BR-LBOX-MAXCON
                    MOVE "EXCEEDS MAX CONTRACTUAL" TO LOG-MSG
                    MOVE 44                        TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

      * TEST IF MAXIMUM RECENCY IS EXCEEDED:

           IF CD-BR-ALLOT-OPTION = "Y"
              IF BR-BP-MAXREC NOT = 999
                IF RECDEL > BR-BP-MAXREC
                   MOVE "EXCEEDS MAX RECENCY" TO LOG-MSG
                   MOVE 45                    TO RETURN-STATUS
                   PERFORM CREATE-LOG
                   GO TO END-ROUTINE.

              IF CD-BR-LBOX-OPTION = "Y"
                 IF BR-LBOX-MAXREC NOT = 999
                    IF RECDEL > BR-LBOX-MAXREC
                       MOVE "EXCEEDS MAX RECENCY" TO LOG-MSG
                       MOVE 45                   TO RETURN-STATUS
                       PERFORM CREATE-LOG
                       GO TO END-ROUTINE.

      * DON'T ALLOW POSTING WHEN LOAN HAS ZERO BALANCE
      * THIS CHECK WAS DOWN AFTER ALL THE PAYOFF STUFF,
      * NOT SURE WHY, I LEFT IT THERE BUT WORLD WANTS TO SEE ZERO
      * BALANCE MESSAGE INSTEAD OF PMT EXCEED PAYOFF MESSAGE, SO
      * YOU CAN LOOK AT EXCEPTION REPORT & SEE PAID OUTS VS.
      * LOANS WITH BALANCE TOO LOW FOR BATCH PMT.  BARB

           IF LN-CURBAL = 0
              IF CD-BR-ALLOT-OPTION = "Y"
                 IF HOLD-BP-PAYOFF-NONCASH NOT = "Y"
                    MOVE "ALREADY ZERO BALANCE" TO LOG-MSG
                     MOVE 46                    TO RETURN-STATUS
                     PERFORM CREATE-LOG
                     GO TO END-ROUTINE.

           IF LN-CURBAL = 0
              IF CD-BR-LBOX-OPTION = "Y"
                 IF HOLD-LBOX-PAYOFF-NONCASH NOT = "Y"
                    MOVE "ALREADY ZERO BALANCE" TO LOG-MSG
                    MOVE 46                     TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

           IF CD-BR-FILE-TYPE = "B" OR "P"
              GO TO SKIP-PAYOFF-CALC.

           IF VALID-REVERSAL-FG = "Y" OR BP-TRCD = "OT"
              GO TO SKIP-PAYOFF-CALC.

      * SET REB-LPTRCD FOR LPPOFF WHICH CALLS REBATE
      * NEED TO KNOW 'PB', 'RN', AND 'SC'
      * FOR SUN FINANCE SPOPT1 = 2:

           MOVE "PO"              TO REB-LPTRCD
                                     POFF-LPTRCD.
           MOVE BP-PAYDATE        TO POFF-PAYDATE.
           MOVE LN-MAKERCD(1)     TO POFF-MAKERCD.
           MOVE BATCH-REFCD       TO POFF-LCAP-BATCH-REFCD.
           MOVE BATCH-REFCD-LC-FG TO POFF-LCAP-BATCH-REFCD-LC-FG.
           PERFORM PAYOFF-LOAN-ROUTINE.

      * COMPILE TOTAL PAYOFF REBATES FOR FURTHER TESTING:

           MOVE 0 TO TOT-POFF-REBATES.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 13
              ADD POFF-REBATE(SUB) TO TOT-POFF-REBATES
           END-PERFORM.

      * DETERMINE NET PAYOFF PRINCIPAL:
           COMPUTE NET-POFF-PRIN =
              LN-CURBAL - LN-OT2BAL + LN-OTHBAL - TOT-POFF-REBATES.

      * DETERMINE AMOUNT THAT WILL PAYOFF ACCOUNT:

           COMPUTE WILL-POFF-ACCOUNT =
              NET-POFF-PRIN + POFF-INTDUE + POFF-LCDUE.

      * DETERMINE TRAMT THAT CAN BE POSTED LEAVING $0.01 IN PRINCIPAL:

           COMPUTE VALID-POSTING-NOPOFF = WILL-POFF-ACCOUNT - 0.01.

      * TEST IF PAYMENT WILL BRING BALANCE BELOW A NET PAYOFF:

           IF BP-TRAMT > POFF-NETDUE
              IF LN-PLDATE NOT = 0
                 IF ( CD-BR-ALLOT-OPTION = "Y" AND
                      BR-BP-PL = "P" )
                    OR
                    ( CD-BR-LBOX-OPTION = "Y" AND
                      BR-LBOX-PL = "P" )
                    MOVE "PAY EXCEEDS PAYOFF AMT" TO LOG-MSG
                    MOVE 47                      TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

      * TEST IF PAYMENT WILL BRING BALANCE BELOW THE CURRENT LOAN
      * BALANCE

           IF BP-TRAMT > (LN-CURBAL - LN-OT2BAL) - .01
              IF LN-PLDATE NOT = 0
                 IF ( CD-BR-ALLOT-OPTION = "Y" AND
                      BR-BP-PL = "P" )
                    OR
                    ( CD-BR-LBOX-OPTION = "Y" AND
                      BR-LBOX-PL = "P" )
                    MOVE "PAY EXCEEDS PRINCIPAL " TO LOG-MSG
                    MOVE 48                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

           IF BP-TRAMT > POFF-NETDUE
              IF ( (CD-BR-ALLOT-OPTION = "Y"    ) AND
                   (HOLD-BP-PAYOFF-NONCASH = "Y"  ) )
                 OR
                 ( (CD-BR-LBOX-OPTION = "Y"     ) AND
                   (HOLD-LBOX-PAYOFF-NONCASH = "Y") )
                 MOVE "99" TO HOLD-BP-TRCD
              ELSE
                 MOVE "PAY EXCEEDS PAYOFF AMT" TO LOG-MSG
                 MOVE 47                 TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

           IF GP-DONT-COLL-INT-ON-SS = "Y"
              IF BP-TRCD = "SS"
                 IF BP-TRAMT > (LN-CURBAL -  .01)                  
                    MOVE "AMT > BALANCE - .01"   TO LOG-MSG
                    MOVE 49                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

      * TEST IF PAYMENT WILL PAYOFF LOAN PREMATURLY:
      *
      * N O T E:   THIS TEST IS NOT CORRECT, IT DOESN'T CONSIDER
      *            LN-LASTPYAMT OR PAYMENT SCHEDULES
      * 7/14/08 OK, I UNDERSTAND BP-PAYOFF-NONCASH DOESN'T ALLOW
      *         BATCH PAYOFF IF OTHER CHARGES ON ACCT, OR PAYMENT
      *         EXCEEDS PAYOFF, OR PAYOFF PRIOR TO MATURITY.
      *         DON'T GET WHY THIS FLAG IS CALL GP-PAYOFF-NONCASH.
      *         ANYWAY, WORLD ACCTS WILL BE PAID OFF EARLY WITH
      *         INSURANCE CLAIMS, SO ALLOW EARLY PAYOFF IF BATCH
      *         PMT = PAYOFF.  WORLD PR# 573.  BARB

           MOVE "N" TO PAYOFF-TRANS-FG.
           IF BP-TRAMT = POFF-NETDUE
              MOVE "Y" TO PAYOFF-TRANS-FG
              MOVE "99" TO HOLD-BP-TRCD
              GO TO SKIP-EARLY-PAYOFF-TEST.

           IF BP-TRAMT = POFF-NETDUE
              MOVE "Y" TO PAYOFF-TRANS-FG
              IF BP-TRAMT NOT = LN-REGPYAMT
                 IF ( (CD-BR-ALLOT-OPTION = "Y"    ) AND
                      (HOLD-BP-PAYOFF-NONCASH = "Y"  )
                    )
                    OR
                    ( (CD-BR-LBOX-OPTION = "Y"     ) AND
                      (HOLD-LBOX-PAYOFF-NONCASH = "Y")
                    )
                    MOVE "99" TO HOLD-BP-TRCD
                 ELSE
                    MOVE "ACCT WILL PAYOFF EARLY" TO LOG-MSG
                    MOVE 50                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

      * WHEN BR-BP/LBOX-ALLOW-PMT-ACCT-OTHBAL = "Y"
      *      THAN ALLOW THE BATCH PAYMENT TO BE APPLIED
      *      TO THE ACCOUNT BUT NOT TO OTHER BALANCE
      *
      * 4/12/07  WE THINK THE CHECK BELOW MAKES SENSE IF THERE IS
      *          AN OTHER BAL ON THE ACCOUNT.  IF THERE IS NO OTHER
      *          BAL, THIS STOPS A PAYOFF FROM POSTING AND GIVE
      *          STUPID CUTOFF MESSAGE.   SO WE WILL ONLY DO THIS
      *          CHECK IF THERE IS AN OTHER BAL.  BARB & CINDY
      *

       SKIP-EARLY-PAYOFF-TEST.
           IF LN-OTHBAL = 0
              GO TO TEST-PAYOFF-ALLOWED.

           IF ( ( CD-BR-ALLOT-OPTION = "Y"          ) AND
                ( HOLD-BP-ALLOW-PMT-ACCT-OTHBAL = "Y" ) )
              OR
              ( ( CD-BR-LBOX-OPTION = "Y"             ) AND
                ( HOLD-LBOX-ALLOW-PMT-ACCT-OTHBAL = "Y" ) )
              IF BP-TRAMT > (VALID-POSTING-NOPOFF - LN-OTHBAL)
                 MOVE "AMT > POFF - .01 WITH OTH" TO LOG-MSG
                 MOVE 51                 TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * TEST IF PAYOFF IS ALLOWED:

       TEST-PAYOFF-ALLOWED.
           IF ( (CD-BR-ALLOT-OPTION = "Y"  ) AND
                (BR-BP-NOPAYOFF NOT = "Y"  ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y"   ) AND
                (BR-LBOX-NOPAYOFF NOT = "Y") )
              IF BP-TRAMT = POFF-NETDUE
                 MOVE "ACCOUNT WILL PAYOFF" TO LOG-MSG
                 MOVE 52                 TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

           IF ( (CD-BR-ALLOT-OPTION = "Y") AND
                (BR-BP-LC NOT = "Y"      ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y" ) AND
                (BR-LBOX-LC NOT = "Y"    ) )
              MOVE BP-PAYDATE TO IBPC-DATE
              PERFORM IBPC-TEST
              IF (IBPC-FG = "P" OR LN-LOANTYPE = "I")
                                        AND (LN-JDDATE = 0)
                                         AND (LN-ACCELDATE = 0)
                 MOVE BP-TRAMT     TO LCAP-TRAMT
                 MOVE BP-PAYDATE   TO LCAP-PAYDATE
                 MOVE LN-1STPYDATE TO LCAP-1STPYDATE
                 MOVE "PY"         TO LCAP-LPTRCD
                 MOVE 0            TO LCAP-LPAPLC LCAP-LPAPINT
                 MOVE CD-CODE      TO LCAP-BATCH-REFCD
                 MOVE CD-BR-LC-FG  TO LCAP-BATCH-REFCD-LC-FG
                 PERFORM LATE-CHARGE-APPLY
                 COMPUTE TEST-AMT  = LCAP-APP + LCAP-OWE - LN-LCBAL
                 IF TEST-AMT NOT = 0
                    MOVE "LATE CHARGE IS REQUIRED" TO LOG-MSG
                    MOVE 53                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

      * TEST IF PAYMENT WILL ONLY COVER LC AND INTEREST:

           IF ( (CD-BR-ALLOT-OPTION = "Y") AND
                (BR-BP-NOPRIN NOT = "Y"  ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y" ) AND
                (BR-LBOX-NOPRIN NOT = "Y") )
              COMPUTE TEST-AMT = POFF-INTDUE + POFF-LCDUE
              IF BP-TRAMT NOT > TEST-AMT
                 MOVE "PAY APPLIES WITH NO PRIN" TO LOG-MSG
                 MOVE 28                 TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * TEST IF PAYMENT WILL CAUSE NEGATIVE CURRENT BALANCE:
      *
      * WHEN OT2BAL EXISTS, INSURE AT LEAST ($0.01 + LN-OT2BAL)
      * REMAINS IN LN-CURBAL:

           COMPUTE TEST-AMT = BP-TRAMT - POFF-INTDUE - POFF-LCDUE.
           IF LN-OT2BAL NOT = 0
              IF TEST-AMT >= (LN-CURBAL - TOT-POFF-REBATES)
                 IF ( (CD-BR-ALLOT-OPTION = "Y"    ) AND
                      (HOLD-BP-PAYOFF-NONCASH = "Y"  ) )
                    OR
                    ( (CD-BR-LBOX-OPTION = "Y"     ) AND
                      (HOLD-LBOX-PAYOFF-NONCASH = "Y") )
                    MOVE "99" TO HOLD-BP-TRCD
                 ELSE
                    MOVE "PAY EXCEEDS OTHER 2 BAL." TO LOG-MSG
                    MOVE 55                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE
                 END-IF
              END-IF
           ELSE
      * WHEN NO OT2BAL, DON'T LET LN-CURBAL GO NEGATIVE:
              IF TEST-AMT > LN-CURBAL
                 IF ( (CD-BR-ALLOT-OPTION = "Y"    ) AND
                      (HOLD-BP-PAYOFF-NONCASH = "Y"  )
                    )
                    OR
                    ( (CD-BR-LBOX-OPTION = "Y"     ) AND
                      (HOLD-LBOX-PAYOFF-NONCASH = "Y")
                    )
                    MOVE "99" TO HOLD-BP-TRCD
                 ELSE
                    MOVE "PAY EXCEEDS CURBAL" TO LOG-MSG
                    MOVE 56                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

      * 5/4/12:  IF THERE'S AN O2 BALANCE, IF IT'S NOT ENOUGH TO
      *          PAYOFF, BUT MORE THAN (CURBAL - O2BAL), THEN
      *          REJECT TO HANDLE MANUALLY.  FOR EXAMPLE, LN-CURBAL=70,
      *          20 OF THAT IS LN-OT2BAL.  BP-TRAMT = 50, REBATES = 15.
      *          SO TEST-AMT (50) IS LESS THAN CURBAL-REBATES (55),
      *          BUT > OR = THE BALANCE LESS O2 (50).  THE PROBLEM
      *          HERE IS THERE'S NOT ENOUGH TO PAYOFF(WOULD NEED 55),
      *          BUT WOULD NEED TO APPLY SOME OR ALL TO O2 BALANCE,
      *          AND STILL LEAVE AT LEAST 1 CENT IN BALANCE.  BETTER
      *          TO REQUIRE MANUAL POSTING.  PL 771.    BARB & CINDY

           IF LN-OT2BAL NOT = 0
              COMPUTE TEST-AMT = BP-TRAMT - POFF-INTDUE - POFF-LCDUE
              IF TEST-AMT < (LN-CURBAL - TOT-POFF-REBATES)
                 IF TEST-AMT >= (LN-CURBAL - LN-OT2BAL)
                    MOVE "REQUIRES MANUAL Z2" TO LOG-MSG
                    MOVE 57                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

       SKIP-PAYOFF-CALC.

      * DON'T ALLOW POSTING WHEN LOAN HAS ZERO BALANCE

           IF LN-CURBAL = 0
              MOVE "ALREADY ZERO BALANCE" TO LOG-MSG
              MOVE 46                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

      * TEST FOR VALID GENERAL LEDGER INTERFACE RECORD:

           MOVE LN-OWNBR TO GI-BRANCH.
           MOVE LN-CLASS TO GI-CLASS.
           PERFORM READ-GI1-FILE.
           IF IO-FG NOT = 0
              MOVE "MISSING G/L INTERFACE " TO LOG-MSG
              MOVE 58                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.

           MOVE " " TO POSTING-ERRCD.

           IF CD-BR-FILE-TYPE = "B"
              PERFORM BANKRUPT-POSTING
           ELSE
           IF BP-TRCD = "RV"
              PERFORM REVERSAL-POSTING
           ELSE
           IF BP-TRCD = "OT"
              PERFORM OTHER-CHG-POSTING
           ELSE
              IF (BP-TRAMT > VALID-POSTING-NOPOFF) AND
                 (HOLD-BP-TRCD = "99"            )
                 PERFORM PAYOFF-POSTING
              ELSE
                 PERFORM PAYMENT-POSTING.

           IF NOT (POSTING-ERRCD = " " OR "O")
              MOVE "GENERIC POSTING ERROR" TO LOG-MSG
              MOVE 59                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              GO TO END-ROUTINE.
            
           MOVE 0                   TO RETURN-STATUS.

           MOVE "SUCCESSFUL UPDATE" TO LOG-MSG
           PERFORM CREATE-LOG.

           PERFORM CLOSE-OP-FILE.

       END-ROUTINE.
           MOVE TEMP-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           PERFORM CLOSE-OP-FILE.

           GO TO END-PROGRAM.

      ************************************
       VALIDATE-REVERSAL SECTION.
      ************************************
           MOVE "Y" TO VALID-REVERSAL-FG.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "9"       TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF (IO-FG NOT = 0) OR (LP-ACCTNO NOT = LN-ACCTNO)
              GO TO INVALID-REVERSAL.

           IF LP-TRCD = "RV" OR LP-REV = "Y"
              GO TO INVALID-REVERSAL.

           IF LP-TRCD NOT = "PA"
              GO TO INVALID-REVERSAL.

           IF LP-PAYDATE NOT = BP-PAYDATE
              OR LP-TRAMT NOT = BP-TRAMT
                 OR LP-REFNO NOT = BP-REFCD
                    GO TO INVALID-REVERSAL.

           MOVE 0      TO LP-TILLNO.

           MOVE LP-REC TO WK-LP-REC.

           GO TO EXIT-VALIDATE-REVERSAL.
       INVALID-REVERSAL.
           MOVE "N" TO VALID-REVERSAL-FG.
           MOVE "NO VALID PMT FOR REVERSAL" TO LOG-MSG.
           MOVE 60                 TO RETURN-STATUS.
           IF LP-PAYDATE NOT = BP-PAYDATE
              MOVE 61                 TO RETURN-STATUS
              MOVE "NO REVERSE: NO DATE MATCH" TO LOG-MSG.
           IF LP-TRAMT NOT = BP-TRAMT
              MOVE 62                 TO RETURN-STATUS
              MOVE "NO REVERSE: NO AMT MATCH"  TO LOG-MSG.
           IF LP-REFNO NOT = BP-REFCD
              MOVE 63                 TO RETURN-STATUS
              MOVE "NO REVERSE:NO REFNO MATCH" TO LOG-MSG.
           IF LP-TRCD = "RV" OR LP-REV = "Y"
              MOVE 64                 TO RETURN-STATUS
              MOVE "NO REVERSE:LAST PMT IS RV" TO LOG-MSG.
           IF LP-TRCD NOT = "PA"
              MOVE 65                 TO RETURN-STATUS
              MOVE "NO REVERSE: NOT A 'PA'   " TO LOG-MSG.
           PERFORM CREATE-LOG.

       EXIT-VALIDATE-REVERSAL.
           PERFORM CLOSE-LP1-FILE.

      ******************************************
       CREATE-LOG SECTION.
      ******************************************
           MOVE LOG-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              PERFORM OPEN-LOG-FILE-EXTEND
           ELSE
              PERFORM OPEN-LOG-FILE-OUTPUT.

           IF RETURN-STATUS = 99
              MOVE SPACES     TO LOG-STATUS
           ELSE
           IF RETURN-STATUS = 0
              MOVE "PASS"     TO LOG-STATUS
           ELSE
              MOVE "FAIL"     TO LOG-STATUS.

           MOVE RETURN-STATUS TO LOG-RETURN.
           MOVE TRANS-DATE    TO LOG-DATE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT     TO LOG-TIME.
           MOVE BT-BRANCH     TO LOG-BRNO.
           MOVE BP-LNNO       TO LOG-NUMBER.
           PERFORM WRITE-LOG-FILE.
           PERFORM CLOSE-LOG-FILE.

      ******************************************************************
       PAYMENT-POSTING SECTION.
      ******************************************************************

      * RESTORE LOAN TO ITS ORIG STATE ELSE WILL ERROR OUT OF LONPF2
      * (LN-REC == WK-SAVE-REC)

           MOVE ORIG-LN-LBOX    TO LN-LBOX.
           MOVE ORIG-LN-ALLOTCD TO LN-ALLOTCD.

      * SETUP-LP-RECORD
           PERFORM SETUP-LP-RECORD.
           MOVE BP-TRAMT TO LP-TRAMT.

      * SETUP-LONPF-BUFFER
           MOVE 0 TO HOLD-OVPAID-CHECK.

           IF PAYOFF-TRANS-FG = "Y"
              MOVE "PO" TO HOLD-POCD.

      * AUTOMATIC BATCH TRCD = "PA" FROM BPBKBR.C
      * MANUAL BATCH MAY BE "PA" OR "PY"

           IF HOLD-BP-TRCD = " "
              MOVE BP-TRCD TO HOLD-BP-TRCD LP-TRCD
           ELSE
              MOVE "PA" TO LP-TRCD.

           PERFORM SETUP-LONPF-BUFFER.

      * PAYMENT POSTING

           IF (HOLD-BP-TRCD = "RE" OR "O2")
              MOVE "LP/LONPF7" TO FORM-NAM
           ELSE
           IF HOLD-BP-TRCD = "RP"
              MOVE "LP/LONPFB" TO FORM-NAM
           ELSE
           IF (HOLD-BP-TRCD = "PL" OR "P2" OR "P3")
              MOVE "LP/LONPF9" TO FORM-NAM
           ELSE
              MOVE "LP/LONPFC" TO FORM-NAM.

           MOVE FORM-PROGX TO PAYMENT-PROG.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                PROG-BUF  UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPFC DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD = "P"
              IF PAYMENT-PROG = "LONPFC"
                 MOVE "LONPFC: ALT PREPAYMENT    " TO LOG-MSG
                 PERFORM CREATE-LOG
                 MOVE 66                 TO RETURN-STATUS
                 MOVE "E" TO POSTING-ERRCD
                 GO TO PAYMENT-POSTING-EXIT.
           
           IF PAYMENT-PROG = "LONPFB"
              IF NOT (ERRCD = " " OR "P")
                 MOVE "######: PMT NOT APPLIED:&" TO LOG-MSG
                 INSPECT LOG-MSG REPLACING FIRST "######"
                                            BY PAYMENT-PROG
                 INSPECT LOG-MSG REPLACING FIRST "&"
                                            BY ERRCD
                 MOVE 67                 TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 MOVE "E" TO POSTING-ERRCD
                 GO TO PAYMENT-POSTING-EXIT
              END-IF
           ELSE
              IF ERRCD NOT = " "
                 MOVE "######: PMT NOT APPLIED:& " TO LOG-MSG
                 INSPECT LOG-MSG REPLACING FIRST "######"
                                            BY PAYMENT-PROG
                 INSPECT LOG-MSG REPLACING FIRST "&"
                                            BY ERRCD
                 MOVE 68                 TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 MOVE "E" TO POSTING-ERRCD
                 GO TO PAYMENT-POSTING-EXIT.

      * PAYMENT UPDATE

           MOVE 0 TO HOLD-PRIOR-DF-POSTDATE.

           MOVE "LP/LONPF2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                PROG-BUF UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPF2 DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD = "E" OR "X" OR "M"
              MOVE "LONPF2: PMT NOT APPLIED  " TO LOG-MSG
              MOVE 69                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              MOVE "E" TO POSTING-ERRCD
              GO TO PAYMENT-POSTING-EXIT.

       PAYMENT-POSTING-EXIT.
           EXIT.

      ******************************************************************
       PAYOFF-POSTING SECTION.
      ******************************************************************

      * RESTORE LOAN TO ITS ORIG STATE ELSE WILL ERROR OUT
      * OF LONPF2 (LN-REC == WK-SAVE-REC)

           MOVE ORIG-LN-LBOX    TO LN-LBOX.
           MOVE ORIG-LN-ALLOTCD TO LN-ALLOTCD.
           MOVE 0               TO HOLD-OVPAID-CHECK.

      * NO O2 MONIES

           IF LN-OT2BAL = 0
              IF ( (CD-BR-ALLOT-OPTION = "Y"      ) AND
                   (BR-BP-PAYOFF-OV = "O" OR "M"  ) )
                 OR
                 ( (CD-BR-LBOX-OPTION = "Y"       ) AND
                   (BR-LBOX-PAYOFF-OV = "O" OR "M") )
                 SUBTRACT WILL-POFF-ACCOUNT FROM BP-TRAMT
                                      GIVING HOLD-OVPAID-CHECK
                                             OVPAID-AMT
                 IF OVPAID-AMT NOT = 0
                    MOVE OVPAID-AMT TO DISPLAY-OVPAID-AMT
                    MOVE "OVER PAID = ###########" TO LOG-MSG
                    INSPECT LOG-MSG REPLACING
                          FIRST "###########" BY DISPLAY-OVPAID-AMT
                    MOVE 70                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                 END-IF
                 MOVE WILL-POFF-ACCOUNT TO BP-TRAMT
                 PERFORM POST-PO
                 IF POSTING-ERRCD NOT = " "
                    GO TO PAYOFF-POSTING-EXIT
                 END-IF
              ELSE
                 MOVE 0 TO HOLD-OVPAID-CHECK
                 SUBTRACT WILL-POFF-ACCOUNT FROM BP-TRAMT
                                        GIVING OVPAID-AMT
                 MOVE WILL-POFF-ACCOUNT TO BP-TRAMT
                 IF BP-TRAMT NOT = 0
                    PERFORM POST-PO
                    IF POSTING-ERRCD NOT = " "
                       GO TO PAYOFF-POSTING-EXIT
                    END-IF
                 END-IF
                 IF OVPAID-AMT NOT = 0
                    MOVE OVPAID-AMT TO DISPLAY-OVPAID-AMT
                    MOVE "OVER PAID = ###########" TO LOG-MSG
                    INSPECT LOG-MSG REPLACING
                          FIRST "###########" BY DISPLAY-OVPAID-AMT
                    MOVE "O" TO POSTING-ERRCD
                    MOVE 70                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                 END-IF
              END-IF
           ELSE
      * O2 MONIES EXIST SO EITHER PAYOFF OR REFUND LEAVING $0.01 IN PRIN
              PERFORM POST-Z2
              IF POSTING-ERRCD NOT = " "
                 GO TO PAYOFF-POSTING-EXIT
              END-IF
              SUBTRACT TEST-AMT FROM BP-TRAMT
              PERFORM READ-LN1-FILE
              IF IO-FG NOT = 0
                 MOVE "AFTER Z2, LOAN MISSING" TO LOG-MSG
                 MOVE 71                 TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 MOVE "E" TO POSTING-ERRCD
                 GO TO PAYOFF-POSTING-EXIT
              END-IF
              IF ( (CD-BR-ALLOT-OPTION = "Y"      ) AND
                   (BR-BP-PAYOFF-OV = "O" OR "M"  ) )
                 OR
                 ( (CD-BR-LBOX-OPTION = "Y"       ) AND
                   (BR-LBOX-PAYOFF-OV = "O" OR "M") )
                 IF WILL-POFF-ACCOUNT >= BP-TRAMT
                    MOVE 0 TO HOLD-OVPAID-CHECK
                              OVPAID-AMT
      * REFUNDS + "PY"
                    IF BP-TRAMT > 0
                       PERFORM POST-REFUNDS-PLUS-PY
                    END-IF
                    GO TO PAYOFF-POSTING-EXIT
                 ELSE
                    SUBTRACT WILL-POFF-ACCOUNT FROM BP-TRAMT
                                      GIVING HOLD-OVPAID-CHECK
                                             OVPAID-AMT
                    MOVE OVPAID-AMT TO DISPLAY-OVPAID-AMT
                    MOVE "OVER PAID = ###########" TO LOG-MSG
                    INSPECT LOG-MSG REPLACING
                          FIRST "###########" BY DISPLAY-OVPAID-AMT
                    MOVE 70                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                 END-IF
                 PERFORM POST-PO
                 IF POSTING-ERRCD NOT = " "
                    GO TO PAYOFF-POSTING-EXIT
                 END-IF
              ELSE
                 MOVE 0 TO HOLD-OVPAID-CHECK
                 IF WILL-POFF-ACCOUNT >= BP-TRAMT
                    MOVE 0 TO OVPAID-AMT
      * REFUNDS + "PY"
                    IF BP-TRAMT > 0
                       PERFORM POST-REFUNDS-PLUS-PY
                    END-IF
                    GO TO PAYOFF-POSTING-EXIT
                 ELSE
                    SUBTRACT WILL-POFF-ACCOUNT FROM BP-TRAMT
                                        GIVING OVPAID-AMT
                    IF OVPAID-AMT NOT = 0
                      MOVE OVPAID-AMT TO DISPLAY-OVPAID-AMT
                      MOVE "OVER PAID = ###########" TO LOG-MSG
                      INSPECT LOG-MSG REPLACING
                           FIRST "###########" BY DISPLAY-OVPAID-AMT
                       MOVE 70                 TO RETURN-STATUS
                       PERFORM CREATE-LOG
                    END-IF
                 END-IF
                 PERFORM POST-PO
              END-IF.

       PAYOFF-POSTING-EXIT.
           EXIT.

      *********************************************************
       POST-PO SECTION.
      *********************************************************

      * SETUP-LP-RECORD

           PERFORM SETUP-LP-RECORD.
           MOVE BP-TRAMT TO LP-TRAMT.
           MOVE "PO"     TO LP-TRCD.

      * SETUP-LONPF-BUFFER

           MOVE "PO" TO HOLD-POCD.

           MOVE "99" TO HOLD-BP-TRCD.

           PERFORM SETUP-LONPF-BUFFER.

      * PAYOFF POSTING

           MOVE "LP/LONPFA" TO FORM-NAM.

           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                PROG-BUF UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPFC DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD NOT = " "
              MOVE "LONPFA: PMT NOT APPLIED   " TO LOG-MSG
              MOVE 72                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              MOVE "E" TO POSTING-ERRCD
              GO TO POST-PO-EXIT.

      * PAYOFF UPDATE

           MOVE "LP/LONPF2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                PROG-BUF  UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPF2 DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD = "E" OR "X" OR "M"
              MOVE "LONPF2: PMT NOT APPLIED  " TO LOG-MSG
              MOVE 69                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              MOVE "E" TO POSTING-ERRCD
              GO TO POST-PO-EXIT.

       POST-PO-EXIT.
           EXIT.

      *********************************************************
       POST-Z2 SECTION.
      *********************************************************

      * SETUP-LP-RECORD

           PERFORM SETUP-LP-RECORD.

           MOVE "Z2" TO LP-TRCD.

      * ASSURE THAT $0.01 REMAININGS IN PRINCIPAL

           IF LN-OT2BAL > (BP-TRAMT - VALID-POSTING-NOPOFF)
              COMPUTE TEST-AMT
                      LP-TRAMT
                      LP-APOT2 = BP-TRAMT - VALID-POSTING-NOPOFF
           ELSE
              MOVE LN-OT2BAL TO TEST-AMT
                                LP-TRAMT
                                LP-APOT2.

      * SETUP-LONPF-BUFFER
           MOVE " "  TO HOLD-POCD.
           MOVE "Z2" TO HOLD-BP-TRCD.

           PERFORM SETUP-LONPF-BUFFER.

      * Z2 POSTING
           MOVE "LP/LONPFC" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME PROG-BUF
                                           UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPFC DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD NOT = " "
              MOVE "LONPFC: PMT NOT APPLIED   " TO LOG-MSG
              MOVE 73                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              MOVE "E" TO POSTING-ERRCD
              GO TO POST-Z2-EXIT.

      * Z2 UPDATE

           MOVE "LP/LONPF2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                PROG-BUF UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPF2 DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD = "E" OR "X" OR "M"
              MOVE "LONPF2: PMT NOT APPLIED  " TO LOG-MSG
              MOVE 69                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              MOVE "E" TO POSTING-ERRCD
              GO TO POST-Z2-EXIT.

       POST-Z2-EXIT.
           EXIT.

      *********************************************************
       POST-REFUNDS-PLUS-PY SECTION.
      *********************************************************

           MOVE 0 TO SUB1.
       POST-REFUNDS-PLUS-PY-AGAIN.
           ADD 1 TO SUB1.
           IF SUB1 > 13
              GO TO POST-REFUNDS-PLUS-PY-NOW.

           IF SUB1 = 1
              IF POFF-REBATE(1) NOT = 0
                 MOVE "RC" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB1) NOT = 0 AND
                    LN-REBATE(SUB1) = " "
                    MOVE "RC" TO HOLD-BP-TRCD
                 ELSE
                    GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 2
              IF POFF-REBATE(2) NOT = 0
                 MOVE "RA" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB1) NOT = 0 AND
                    LN-REBATE(SUB1) = " "
                    MOVE "RA" TO HOLD-BP-TRCD
                 ELSE
                    GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 3
              IF POFF-REBATE(3) NOT = 0
                 MOVE "RP" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB1) NOT = 0 AND
                    LN-REBATE(SUB1) = " "
                    MOVE "RP" TO HOLD-BP-TRCD
                 ELSE
                    GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 4
              IF POFF-REBATE(4) NOT = 0
                 MOVE "RI" TO HOLD-BP-TRCD
              ELSE
                 GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 5
              IF POFF-REBATE(5) NOT = 0
                 MOVE "RS" TO HOLD-BP-TRCD
              ELSE
                 GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 6
              IF POFF-REBATE(6) NOT = 0
                 MOVE "RM" TO HOLD-BP-TRCD
              ELSE
                 GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 7
              IF POFF-REBATE(7) NOT = 0
                 MOVE "RD" TO HOLD-BP-TRCD
              ELSE
                 GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 8
              GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 9
              IF POFF-REBATE(9) NOT = 0
                 MOVE "R1" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB1 - 5) NOT = 0 AND
                    LN-REBATE(SUB1) = " "
                    MOVE "R1" TO HOLD-BP-TRCD
                 ELSE
                    GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 10
              IF POFF-REBATE(10) NOT = 0
                 MOVE "R2" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB1 - 5) NOT = 0 AND
                    LN-REBATE(SUB1) = " "
                    MOVE "R2" TO HOLD-BP-TRCD
                 ELSE
                    GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 11
              IF POFF-REBATE(11) NOT = 0
                 MOVE "R3" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB1 - 5) NOT = 0 AND
                    LN-REBATE(SUB1) = " "
                    MOVE "R3" TO HOLD-BP-TRCD
                 ELSE
                    GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 12
              IF POFF-REBATE(12) NOT = 0
                 MOVE "R4" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB1 - 5) NOT = 0 AND
                    LN-REBATE(SUB1) = " "
                    MOVE "R4" TO HOLD-BP-TRCD
                 ELSE
                 GO TO POST-REFUNDS-PLUS-PY-AGAIN.
           IF SUB1 = 13
              IF POFF-REBATE(13) NOT = 0
                 MOVE "R5" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB1 - 5) NOT = 0 AND
                    LN-REBATE(SUB1) = " "
                    MOVE "R5" TO HOLD-BP-TRCD
                 ELSE
                    GO TO POST-REFUNDS-PLUS-PY-AGAIN.

           MOVE " " TO ERRCD.
           PERFORM SETUP-LP-RECORD.
           MOVE POFF-REBATE(SUB1) TO LP-TRAMT LP-APCUR.
           MOVE HOLD-BP-TRCD      TO LP-TRCD.

      * SETUP LONPF BUFFER

      * THIS SET OF 'F9' TRIGGERS LONPFB.C TO DO A REFUND OF INTEREST
      * WITH "PO" IN REB-LPTRCD, LIKE CRNOR2.C

           IF HOLD-BP-TRCD = "RI"
              MOVE "F9" TO HOLD-BP-TRCD.

           PERFORM SETUP-LONPF-BUFFER.

           MOVE "LP/LONPFB" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME PROG-BUF
                                          UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPFB DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD NOT = " "
              MOVE "LONPFB: REFUND NOT APPLIED" TO LOG-MSG
              MOVE 67                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              MOVE "E" TO POSTING-ERRCD
              GO TO POST-REFUNDS-PLUS-PY-EXIT.

      * UPDATE PROGRAM
           MOVE "LP/LONPF2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                PROG-BUF UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPF2 DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD = "E" OR "X" OR "M"
              MOVE "LONPF2: REFUND NOT UPDATED" TO LOG-MSG
              MOVE 74                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              MOVE "E" TO POSTING-ERRCD
              GO TO POST-REFUNDS-PLUS-PY-EXIT.

      * READ UPDATED LOAN
           PERFORM READ-LN1-FILE.

           GO TO POST-REFUNDS-PLUS-PY-AGAIN.

       POST-REFUNDS-PLUS-PY-NOW.

      * READ UPDATED LOAN (ONE MORE TIME, IN CASE THERE WERE NO REBATES)

           PERFORM READ-LN1-FILE.

      * MUST SET TO " " TO GET DEFAULT BP-TRCD SET:
           MOVE " " TO HOLD-BP-TRCD.

      * MERCURY RE:256X
      * MUST SET ORIG-LN-.... TO LN-..... VALUES SINCE THEY HAVE
      * CHANGED WHEN LONPF2.C WAS CALLED ABOVE AND PAYMENT-POSTING
      * ROUTINE SET THEM TO THE ORIGINAL VALUES WHEN THE LOAN WAS
      * FIRST PROCESSED.

           MOVE LN-LBOX    TO ORIG-LN-LBOX.
           MOVE LN-ALLOTCD TO ORIG-LN-ALLOTCD.

           PERFORM PAYMENT-POSTING.

           MOVE WK-LP-REC TO LP-REC.

       POST-REFUNDS-PLUS-PY-EXIT.
           EXIT.

      ******************************************************************
       OTHER-CHG-POSTING SECTION.
      ******************************************************************
      * RESTORE LOAN TO ITS ORIG STATE ELSE WILL ERROR OUT OF LONPF2
      * (LN-REC == WK-SAVE-REC)

           MOVE ORIG-LN-LBOX    TO LN-LBOX.
           MOVE ORIG-LN-ALLOTCD TO LN-ALLOTCD.

      * SETUP-LP-RECORD
           MOVE "OT"     TO BP-TRCD.
           PERFORM SETUP-LP-RECORD.

      * SETUP-LONPF-BUFFER
           MOVE 0 TO HOLD-OVPAID-CHECK.
           MOVE BP-TRCD TO HOLD-BP-TRCD LP-TRCD.

           PERFORM SETUP-LONPF-BUFFER.

      * OTHER CHARGES POSTING
           MOVE "LP/LONPF7" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                 PROG-BUF UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPF7 DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD NOT = " "
              MOVE "LONPF7: 'OT' NOT APPLIED   " TO LOG-MSG
              PERFORM CREATE-LOG
              MOVE 82                            TO RETURN-STATUS
              MOVE "E" TO POSTING-ERRCD
              GO TO OTHER-CHG-POSTING-EXIT.

      * UPDATE

           MOVE 0 TO HOLD-PRIOR-DF-POSTDATE.

           MOVE "LP/LONPF2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                 PROG-BUF UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPF2 DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD = "E" OR "X" OR "M"
              MOVE "LONPF2: PMT NOT APPLIED  " TO LOG-MSG
              MOVE 69 TO RETURN-STATUS
              PERFORM CREATE-LOG
              MOVE "E" TO POSTING-ERRCD
              GO TO OTHER-CHG-POSTING-EXIT.

       OTHER-CHG-POSTING-EXIT.
           EXIT.

      ******************************************************************
       BANKRUPT-POSTING SECTION.
      ******************************************************************

      * RESTORE LOAN TO ITS ORIG STATE ELSE WILL ERROR OUT OF LONPF2
      * (LN-REC == WK-SAVE-REC)

           MOVE ORIG-LN-LBOX    TO LN-LBOX.
           MOVE ORIG-LN-ALLOTCD TO LN-ALLOTCD.

           PERFORM SETUP-LP-RECORD.

           MOVE BP-TRAMT          TO LP-TRAMT.
           MOVE BP-BNKRPT-CASE-NO TO HOLD-BNKRPT-CASE-NO.
           MOVE BP-BNKRPT-STATUS  TO HOLD-BNKRPT-STATUS.
           MOVE LN-OTHBAL         TO LP-OTHBAL.
           MOVE LN-OT2BAL         TO LP-OT2BAL.
           MOVE LN-INTBAL         TO LP-INTBAL.
           MOVE LN-LCBAL          TO LP-LCBAL.
           MOVE LN-CURBAL         TO LP-CURBAL.
           MOVE LP-PAYDATE        TO IBPC-DATE.
           PERFORM IBPC-TEST.
           MOVE IBPC-FG           TO LP-IBPC.
           MOVE LN-CURBAL         TO LP-INTDUE.
           MOVE LN-1STPYDATE      TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL    TO LP-PDTH-DATE.
           ADD LP-APLC            TO HOLD-LN-TOTERN HOLD-LN-TOTLCHG.
           ADD LP-APINT           TO HOLD-LN-TOTERN HOLD-LN-TOTINT.

      * SETUP-LONPF-BUFFER
           MOVE 0 TO HOLD-OVPAID-CHECK.

      * AUTOMATIC BATCH TRCD = "PA" FROM BPBKBR.C
      * MANUAL BATCH MAY BE "PA" OR "PY"

           MOVE BP-TRCD TO HOLD-BP-TRCD LP-TRCD.

           PERFORM SETUP-LONPF-BUFFER.

      * PAYMENT UPDATE

           MOVE "LP/LONPF2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                PROG-BUF UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPF2 DID NOT REJECT
      * THE PROPOSED TRANSACTION

           IF ERRCD = "E" OR "X" OR "M"
              MOVE "LONPF2: PMT NOT APPLIED  " TO LOG-MSG
              MOVE 69                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              MOVE "E" TO POSTING-ERRCD.

       BANKRUPT-POSTING-EXIT.
           EXIT.

      ******************************************************************
       REVERSAL-POSTING SECTION.
      ******************************************************************

      * SOME LOGIC FROM SETUP-LONPF-BUFFER:

           MOVE 0   TO ENTRY-FLAG BYPASS-AMT-ENT-FG.
           MOVE "F" TO FULL-DISPLAY.
           MOVE " " TO REV-REBATE FULL-PAGE-FG ENTER-AMT-FG
                       HOLD-DISPLAY-ERN
                       HOLD1-KEY-DOWN HOLD2-KEY-UP.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.

           MOVE 0 TO HOLD-PARTIALS
                     HOLD-LCPAID
                     HOLD-DLPARVHELD-ADJ
                     HOLD-AFPDLLP
                     HOLD-OVPAID-CHECK
                     HOLD-REPO-AUCTION-EXP
                     ADDON-INT-REBATE
                     ADDON-INT-EARNED
                     HOLD-PRIOR-DF-POSTDATE.

           MOVE LN-TOTINT     TO HOLD-LN-TOTINT.
           MOVE LN-TOTERN     TO HOLD-LN-TOTERN.
           MOVE LN-TOTDEF     TO HOLD-LN-TOTDFMNTS.
           MOVE LN-TOTLCHG    TO HOLD-LN-TOTLCHG.
           MOVE LN-TOTSERACCT TO HOLD-LN-TOTSERACCT.
           MOVE LN-JDRATE     TO HOLD-LN-JDRATE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 13
              MOVE 0 TO REB-AMOUNT(SUB)
              IF SUB < 7
                 MOVE LN-ACCERN(SUB) TO HOLD-LN-ACCERN(SUB)
              END-IF
              IF SUB < 9
                 MOVE " " TO INS-RB-TBL(SUB)
              END-IF
           END-PERFORM.

      * RESTORE LOAN TO ITS ORIG STATE ELSE WILL ERROR OUT
      * OF LONPF2 (LN-REC == WK-SAVE-REC)
      
           MOVE ORIG-LN-LBOX    TO LN-LBOX.
           MOVE ORIG-LN-ALLOTCD TO LN-ALLOTCD.

           MOVE LX-REC  TO WK-LX-REC.
           MOVE LXE-REC TO WK-LXE-REC.
           MOVE LXG-REC TO WK-LXG-REC.
           PERFORM CLOSE-LP1-FILE.

           PERFORM READ-EARNINGS.

      * GET THE GL FOR THIS REVERSAL
           PERFORM OPEN-LXG1-FILE.
           MOVE LP-BRNO   TO LXG-BRNO.
           MOVE LP-ACCTNO TO LXG-ACCTNO.
           MOVE LP-SEQNO  TO LXG-SEQNO.
           PERFORM READ-LXG1-FILE.
           PERFORM CLOSE-LXG1-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO LXG-GLNO(1)  LXG-GLNO(2)  LXG-GLNO(3)
                        LXG-GLAMT(1) LXG-GLAMT(2) LXG-GLAMT(3).

       REVERSAL-LP1-FILE.
           MOVE LP-TRCD  TO REV-TRCD.
           MOVE LP-REFNO TO REV-REFNO.

           ADD LN-INTBAL LP-APINT GIVING LP-INTBAL.
           ADD LN-OTHBAL LP-APOTH GIVING LP-OTHBAL.
           ADD LN-LCBAL LP-APLC GIVING LP-LCBAL.

           MOVE LN-CURBAL TO LP-CURBAL.

      * "OTWOAPJDPLPIPWRPTSTPSPBKBD" (NOT).
      *  PA NOT IN LIST SO WILL BE CDV-NO-MATCH
           MOVE CDV-17 TO CDV-BUF.
           PERFORM CDV-VERIFY.

      * THE "DF" HAS THE AMOUNT IN INTDUE SO INTBAL COMES OUT RIGHT:
           IF (CDV-NO-MATCH OR (REV-TRCD = "Z2"))
              IF ( REV-TRCD NOT = "WL" ) AND
                 ( SP-CAL-RATETYPE(1) NOT = "Z" ) AND
                 ( NOT (REV-TRCD = "RE" AND LN-LOANTYPE = "I") )
                   SUBTRACT LP-INTDUE FROM LP-INTBAL.

      * ADDON-POSTING, LP-APINTOWE HOLDS LN-INTPDTH BEFORE ADDON:
           IF (CDV-NO-MATCH OR (REV-TRCD = "Z2"))
              IF NOT ADDON-POSTING
                 SUBTRACT LP-APINTOWE FROM LP-LCBAL.

           MULTIPLY -1 BY
              LP-TRAMT LP-APOTH LP-APINT LP-APLC LP-APCUR
              LP-INTDUE LP-DLPROC LP-EARNED(1) LP-EARNED(2)
              LP-EARNED(3) LXG-GLAMT(1) LXG-GLAMT(2) LXG-GLAMT(3)
              LP-INTPAID LXE-EARN(1) LXE-EARN(2) LXE-EARN(3)
              LXE-EARN(4) LXE-EARN(5) LXE-EARN(6) LXE-EARN(7)
              LXE-WORKER LXE-WORKER2 LP-NOLCHG LP-PREPAY-PENALTY
              LP-PRORATED-INS-REB LP-EXTRA-PRIN-LFP LP-APPLIED-TO-ESCROW.

      * ADDON-POSTING, LP-APINTOWE HOLDS LN-INTPDTH BEFORE ADDON
           IF REV-TRCD NOT = "SP" AND NOT = "DC" AND
                        NOT ADDON-POSTING
              MULTIPLY -1 BY LP-APINTOWE.

           IF REB-AMOUNT(1) NOT = 0
              IF LN-ADDON-FG = "Y"
                 ADD LN-ANTICADJ(1) REB-AMOUNT(1)
                                         GIVING ADDON-INT-REBATE.

      * FOR PC ACCOUNTS, 'RI' MAY HAVE A VALUE:
           MOVE SPACES TO HOLD-DL-RVCHGS-CODE(1)
                          HOLD-DL-RVCHGS-CODE(2)
                          HOLD-DL-RVCHGS-CODE(3)
                          HOLD-DL-PARVHELD-CODE
                          HOLD-DL-PARVPAID-CODE.

           IF LN-SERVACCT NOT = "Y"
              PERFORM CALC-HOLD-LN-ACCERN.

           COMPUTE HOLD-LN-TOTERN =
                LN-TOTERN
                    + LP-EARNED(1) + LP-EARNED(2) + LP-EARNED(3)
                     + LXE-EARN(1) + LXE-EARN(2)  + LXE-EARN(3)
                     + LXE-EARN(4) + LXE-EARN(5)  + LXE-EARN(6)
                     + LXE-EARN(7).

           IF LN-SERVACCT NOT = "Y"
              IF NOT ADDON-POSTING
                  IF REV-TRCD NOT = "WI" AND NOT = "WL"
                     IF REV-TRCD NOT = "RD"
                        ADD LP-APINT LP-APLC TO HOLD-LN-TOTERN
                     ELSE
                        SUBTRACT LP-TRAMT FROM HOLD-LN-TOTERN.

           MOVE LN-TOTDEF TO HOLD-LN-TOTDFMNTS.
           IF DF-POSTING
              IF SP-DEFFRMLA = 97 OR 96 OR 95
                 COMPUTE HOLD-LN-TOTINT = LN-TOTINT + LP-APINT
              ELSE
                 COMPUTE HOLD-LN-TOTDFMNTS = LN-TOTDEF + LP-APINT
              END-IF
           ELSE
              IF REV-TRCD = "RD"
                 COMPUTE HOLD-LN-TOTDFMNTS = LN-TOTDEF - LP-TRAMT
              ELSE
                 IF REB-AMOUNT(7) NOT = 0
                   COMPUTE HOLD-LN-TOTDFMNTS = LN-TOTDEF - REB-AMOUNT(7)
                   SUBTRACT REB-AMOUNT(7) FROM HOLD-LN-TOTERN.

           IF NOT (DF-POSTING AND (SP-DEFFRMLA = 97 OR 96 OR 95))
              MOVE LN-TOTINT TO HOLD-LN-TOTINT.
           MOVE LN-TOTLCHG TO HOLD-LN-TOTLCHG.
           MOVE LN-TOTSERACCT TO HOLD-LN-TOTSERACCT.

      * PACESETTER DEFFRMLA 8 CAN HAVE LATE CHARGES
           IF REV-TRCD NOT = "WL"
              IF REVERSE-AFTER-RESCHED-FG = "N"
                 COMPUTE HOLD-LN-TOTLCHG = LN-TOTLCHG + LP-APLC.

           IF REV-TRCD NOT = "WI" AND
                        NOT DF-POSTING AND
                         NOT ADDON-POSTING
              COMPUTE HOLD-LN-TOTINT = LN-TOTINT + LP-APINT.

      * UCCC LATE CHARGES
      * CAN'T TAKE LATE CHARGE ON MONIES PAID WITHIN GRACE DAYS OF DUE DAY

           IF ( SP-LCFRMLA = "F" OR "H" OR "I" OR "J" OR "K"
                                 OR "L" OR "M" OR "P" OR "S" OR "T")
              IF REV-TRCD = "PY" OR "PA" OR "AL" OR "PE" OR "SS"
                         OR "PO" OR "PB" OR "SC" OR "RN" OR "IN"
                                 OR "RB" OR "RO" OR "PR" OR "PC"
                                 OR "PP" OR "AH" OR "PN" OR "PZ"
                 MOVE LP-LCPARTIALS TO HOLD-PARTIALS
                 MOVE LP-LCPAID TO HOLD-LCPAID.

           IF LN-SERVACCT NOT = "Y"
              GO TO BYPASS-SERVICE.

           ADD LP-EARNED(1) TO HOLD-LN-TOTSERACCT.

      * EARNED INTEREST
           ADD LP-EARNED(2) TO HOLD-LN-TOTSERACCT.
      * EARNED LCHG
           ADD LP-EARNED(3) TO HOLD-LN-TOTSERACCT.

       BYPASS-SERVICE.
           MOVE "RV" TO LP-TRCD.

      * LET REVERSAL 'RV' RECORD HAVE THE LP-PAYDATE OF THE
      * RECORD BEING REVERSED:
           MOVE TRANS-DATE TO LP-TRDATE LP-PAYDATE.

      ***********************************************************
      * LOGIC FROM BEGINNING OF DISPLAY-ENTRY-LINE IN LONPF8:
      ***********************************************************
           SUBTRACT LP-APCUR FROM LP-CURBAL.

      * CANT CHANGE LN-REC IN HERE! LONPF2 WILL EXIT IF LN-REC DOESNT
      * MATCH WK-SAVE-RECORD
           MOVE LN-TOTPAYMNTD TO HOLD-LN-TOTPAYMNTD.

      * UPDATE CONTRACTUAL PAYMENTS TO DATE:
           ADD LP-APCUR TO LN-TOTPAYMNTD
           IF SP-CAL-RATETYPE(1) = "Z"
              SUBTRACT LP-EXTRA-PRIN-LFP FROM LN-TOTPAYMNTD
              ADD LP-EXTRA-PRIN-LFP        TO LN-TOTEXCPAYMNTD.
           IF SP-CONTRFRMLA = "A"
              ADD LP-APLC TO LN-TOTPAYMNTD.
           ADD LP-APINT TO LN-TOTPAYMNTD.

      * COMPUTE PAID THRU DATE
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO LP-PDTH-DATE.

      * CANT CHANGE LN-REC IN HERE! LONPF2 WILL EXIT IF LN-REC DOESNT
      * MATCH WK-SAVE-RECORD
           MOVE HOLD-LN-TOTPAYMNTD TO LN-TOTPAYMNTD.

           MOVE BP-TRCD TO HOLD-BP-TRCD LP-TRCD.
           MOVE BP-REFCD TO LP-REFNO HOLD-LP-REFNO.

      * PAYMENT UPDATE

           MOVE LN-ACCTNO TO LOAN-KEY.
           MOVE LN-SSNO(1) TO BORROWER-KEY.

       REVERSAL-SAVE-SCREEN.
           MOVE 3 TO WK-KEY.
           PERFORM OPEN-WK-FILE.

           MOVE 0 TO WK-ELE.

      * THIS DOESNT GET DONE IN LONPF8, BUT LONPF1 DOES SET IT, SO IT MUST
      * GET DONE HERE, WITH NO CHANGES MADE TO THE LN-REC IN THIS PROGRAM!
           MOVE LN-REC TO WK-SAVE-RECORD.
           MOVE LP-REC TO WK-LP-REC.
           MOVE LX-REC TO WK-LX-REC.
           MOVE LXE-REC TO WK-LXE-REC.
           MOVE LXG-REC TO WK-LXG-REC.
           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-WK-FILE.
           PERFORM CLOSE-WK-FILE.

       CALL-LONPF2-FOR-REVERSAL.

           MOVE "LP/LONPF2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                 PROG-BUF  UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPF2 DID NOT REJECT
      * THE PROPOSED TRANSACTION
      
           IF ERRCD = "E" OR "X" OR "M"
              MOVE "LONPF2: PMT NOT APPLIED  " TO LOG-MSG
              MOVE 69                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              MOVE "E" TO POSTING-ERRCD.

       REVERSAL-RTN-EXIT.
           EXIT.


      **************************************************
       SETUP-LP-RECORD SECTION.
      **************************************************
           PERFORM CLEAR-LP-FILE.

           MOVE LN-STATUSFG    TO LP-STATUSFG.
           MOVE LN-ACCTNO      TO LP-ACCTNO.
           MOVE TRANS-DATE     TO LP-TRDATE.
           MOVE BP-PAYDATE     TO LP-PAYDATE.
           MOVE LN-SSNO(1)     TO LP-SSNO.
           MOVE LN-LCPDTH-DATE TO LP-LCPDTH-DATE.
      * BECAUSE CASH DRAWER IS NEVER REALY UPDATED DURING THE UPDATE
      * OF A "PA",ETC TRANSACTION, WE DECIDED TO DEFAULT THE DRAWER
      * NUMBER TO ZERO RATHER THAN MAKE THE USER ENTER A UNUSED #.
           MOVE 0              TO LP-TILLNO.
           MOVE BATCH-REFCD    TO LP-REFNO.

           IF REPAY-TRANS-ID NUMERIC
              MOVE REPAY-TRANS-ID TO LP-REPAY-TRANS-ID
           ELSE
              MOVE 0              TO LP-REPAY-TRANS-ID.

           PERFORM CLEAR-LXG-FILE.

           MOVE LP-BRNO   TO LXG-BRNO.
           MOVE LP-ACCTNO TO LXG-ACCTNO.

      **************************************************
       SETUP-LONPF-BUFFER SECTION.
      **************************************************
           MOVE 0   TO ENTRY-FLAG BYPASS-AMT-ENT-FG.
           MOVE "F" TO FULL-DISPLAY.
           MOVE " " TO REV-REBATE FULL-PAGE-FG ENTER-AMT-FG
                       HOLD-DISPLAY-ERN
                       HOLD1-KEY-DOWN HOLD2-KEY-UP.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.

           MOVE 0 TO HOLD-PARTIALS
                     HOLD-LCPAID
                     HOLD-DLPARVHELD-ADJ
                     HOLD-AFPDLLP
                     HOLD-OVPAID-CHECK
                     HOLD-REPO-AUCTION-EXP
                     ADDON-INT-REBATE
                     ADDON-INT-EARNED
      * ADDED 10/15/2021, WRITE ERROR TRPFILE LONPF2
      * CONVERSION FAILED WHEN CONVERTING DATE (22007) 
                     HOLD-PRIOR-DF-POSTDATE.

           IF BP-TRCD = "SS"
              MOVE BP-AUCTION-FEES TO HOLD-REPO-AUCTION-EXP
              MOVE BP-AUCTION-NAME TO HOLD-REPO-AUCTION-NAME.

           IF BP-TRCD = "PL" OR "P2" OR "P3"
              MOVE BP-PL-REASON TO HOLD-PLREASON.

           MOVE LN-TOTINT     TO HOLD-LN-TOTINT.
           MOVE LN-TOTERN     TO HOLD-LN-TOTERN.
           MOVE LN-TOTDEF     TO HOLD-LN-TOTDFMNTS.
           MOVE LN-TOTLCHG    TO HOLD-LN-TOTLCHG.
           MOVE LN-TOTSERACCT TO HOLD-LN-TOTSERACCT.
           MOVE LN-JDRATE     TO HOLD-LN-JDRATE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 13
              MOVE 0 TO REB-AMOUNT(SUB)
              IF SUB < 7
                 MOVE LN-ACCERN(SUB) TO HOLD-LN-ACCERN(SUB)
              END-IF
              IF SUB < 9
                 MOVE " " TO INS-RB-TBL(SUB)
              END-IF
           END-PERFORM.

           MOVE LN-ACCTNO TO LOAN-KEY.
           MOVE LN-SSNO(1) TO BORROWER-KEY.
           IF BP-TRCD = "OT"
              MOVE BP-TRAMT TO LP-INTDUE.
           PERFORM SAVE-SCREEN.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE " " TO ERRCD.

           MOVE EXT-CONAME-USER-LOC TO BRANCH HOLD-PROG-BUF-BRANCH.

      *    ACCEPT ORIG-FIL   FROM ENVIRONMENT "FIL".
           ACCEPT TRANS-DATE FROM CENTURY-DATE.

      * WANT DAILY PROCESS PROGRAMS TO THINK YOU ARE IN BATCH AND NOT
      * TO DISPLAY ANYTHING
           MOVE "70" TO EXT-ROUTE-BUF.

      *  CREATE WK-FILE, IF RUNNING FROM EOCRON, WORK FILE IN /USR/TMP
      *  WAS NOT CREATED IN LONPA0
           PERFORM CREATE-WK-FILE.

           MOVE USER-PID TO TEMP-PID.

           MOVE BT-REFCD TO BATCH-REFCD.

      * CREATE SPOOL DIRECTORY AND OPEN SPOOL FILE IN 'EO'
      
      * XXXXXXXXXXXXXXXXXXXXXXXXXX
      * THIS CREATES HAVOC WHEN MULTIPLE USERS TRYING TO CREATE SAME REPORT
      *  EO/APIXXXXXX/0100CUACH AND REPOEX, THE LOG IS OPEN EXTEND
      * HANDLE THIS IN API??
      * XXXXXXXXXXXXXXXXXXXXXXXXXX

           MOVE BATCH-REFCD TO SAVE-EX-SPOOL-NAME.

      * CREATE EO DIRECTORY - HARD CODED RIGHT TO "API"
      * SC9990/EO/APIMMDDYY
           PERFORM CREATE-SPOOL-DIR.
           IF ERRCD NOT = " "
              GO TO END-INIT.

      * AT THIS POINT EX-SPOOL = EO/APIMMDDYY/0100CUACH (BATCH-REFCD)

      *    MOVE EX-SPOOL TO SUMMARY-SPOOL PRU-FILE.
      *    MOVE PRU-FILE TO LOG-SPOOL-DIR.

      * THIS IS TRYING TO CREATE 0100CUACH WHICH WE CAN NOT DO FOR MULTIPLE
      * PROCESSES
      *    PERFORM OPEN-PR-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO END-INIT.
      *    PERFORM CLOSE-PR-FILE.

      * XXXX  MULTIPLE PROCESSES CAN NOT OPEN THIS AT SAME TIME

      * CREATE STEP EO/APIMMDDYY/0100EXCEPT FOR MAIN EXCEPTIONS REPORT
      *    MOVE "EXCEPT" TO SAVE-EX-SPOOL-NAME.
      *    PERFORM CREATE-SPOOL-DIR.
      *    IF ERRCD NOT = " "
      *       GO TO END-INIT.
      *    MOVE EX-SPOOL TO EXCEPT-SPOOL PRU-FILE.
      *    PERFORM OPEN-PR-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO END-INIT.
      *    PERFORM CLOSE-PR-FILE.

      * CREATE STEP EO/APIMMDDYY/0100REPOEX FOR REPO EXCEPTIONS REPORT
      *    MOVE "REPOEX"   TO SAVE-EX-SPOOL-NAME.
      *    PERFORM CREATE-SPOOL-DIR.
      *    IF ERRCD NOT = " "
      *        GO TO END-INIT.
      *    MOVE EX-SPOOL TO REPOEX-SPOOL PRU-FILE.
      *    IF STAT = "00"
      *       PERFORM OPEN-PR-FILE-EXTEND
      *    ELSE
      *       PERFORM OPEN-PR-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO END-INIT.
      *    PERFORM CLOSE-PR-FILE.

      *    MOVE SUMMARY-SPOOL TO PRU-FILE.
      *    PERFORM OPEN-PR-FILE-EXTEND.
      *    IF IO-FG NOT = 0
      *       GO TO END-INIT.

      * THIS SETS UP THE FIRST 32 BYTES OF LOG PATH
           MOVE EX-SPOOL TO LOG-SPOOL-DIR.

      * CREATE STEP EO/APIMMDDYY/0000LOG FOR ERRORS
      *      2023/08/18 01:04 PM 0010 078674 BEGIN SINGLE BATCH PAYMENT POSTING
      * PASS 2023/08/18 01:04 PM 0010 078674 SUCCESSFUL UPDATE
      *      2023/08/18 01:04 PM 0010 033333 BEGIN SINGLE BATCH PAYMENT POSTING
      * FAIL 2023/08/18 01:04 PM 0010 033333 ACCOUNT NOT ON FILE, ABORTED

           MOVE "BEGIN SINGLE BATCH PAYMENT POSTING" TO LOG-MSG.
           PERFORM CREATE-LOG.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GI1-FILE.
           PERFORM OPEN-CD1-FILE.
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.

           MOVE BATCH-REFCD TO LP-REFNO.

           MOVE 0 TO HOLD-OVPAID-CHECK.

           GO TO EXIT-INITIALIZE.
       END-INIT.
           MOVE 1 TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       SAVE-SCREEN SECTION.
      ******************************************************************
           MOVE 3 TO WK-KEY.
           PERFORM OPEN-WK-FILE.

           MOVE SPACES TO WK-SAVE-RECORD.
           MOVE 0      TO WK-ELE.

           MOVE LN-REC  TO WK-SAVE-RECORD.
           MOVE LP-REC  TO WK-LP-REC.
           MOVE LX-REC  TO WK-LX-REC.
           MOVE LXG-REC TO WK-LXG-REC.

           MOVE 0       TO LXE-EARN(1)
                           LXE-EARN(2)
                           LXE-EARN(3)
                           LXE-EARN(4)
                           LXE-EARN(5)
                           LXE-EARN(6)
                           LXE-EARN(7).
           IF SP-ERNFRMLA(1) = 15 OR SP-ERNFRMLA(5) = 16
              MOVE LN-ERN-IB-INTOWE TO LXE-WORKER
           ELSE
              MOVE 0                TO LXE-WORKER.
           MOVE 0 TO LXE-WORKER2.
           MOVE LXE-REC TO WK-LXE-REC.

           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-WK-FILE.
       EXIT-SAVE-SCREEN.
           PERFORM CLOSE-WK-FILE.


      ******************************************************************
      *
      *  CREATE SPOOL DIRECTORY IN /XXX/XXXX/R#/EO
      *
      * STORE SPOOL FILE NAME AS /XXX/XXXX/R$/EO/....
      * WE NEED A FULL PATH NAME HERE ELSE DIR WILL CHANGE WHEN WE
      * CHANGE DIRECTORIES TO ANOTHER BRANCH.
      *
      * CHANGED TO ACCESS AS ../../ORIG/R1/EO/STEPBPREF, WITH ORIG
      * BEING ORIG DATA PATH, FULL PATH TOO LONG FOR ACCESS-BUF
      ******************************************************************
       CREATE-SPOOL-DIR SECTION.
           MOVE " " TO ERRCD.
           MOVE EXT-FILPATH-BASE      TO EX-SPOOL-BASE.
           MOVE EXT-FILPATH-MACHINE   TO EX-SPOOL-MACHINE.
           MOVE HOLD-EXT-FILPATH-DATA TO EX-SPOOL-DATA-PATH.
           ACCEPT DATE-YYYYMMDD FROM CENTURY-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY           TO EX-SPOOL-DATE.
           MOVE " "                   TO EX-SPOOL-FILE.
           MOVE EX-SPOOL              TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              CALL "C$MAKEDIR" USING DIR-ACCESS-BUF GIVING STAT-99
              IF STAT NOT = "00"
                 MOVE "CANNOT CREATE SPOOL DIRECTORY" TO LOG-MSG
                 MOVE 78                              TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 MOVE "E" TO ERRCD
                 GO TO EXIT-CREATE-SPOOL-DIR.

           MOVE "/"                TO EX-SPOOL-FILE.
           MOVE SAVE-EX-SPOOL-NAME TO EX-SPOOL-NAME.
           MOVE "   "              TO EX-SPOOL-SUF.

      *****************************************************************
      * THEY ARE ALLOWED TO RUN THIS  MORE THAN ONCE ON THE
      * SAME TRANSACTION DATE, SO CHECK TO SEE IF REPORT EXISTS, IF
      * SO, INCREMENT THE STEP # SO WE DON'T WIPE OUT PRIOR REPORTS
      *****************************************************************
           MOVE 100 TO STEP-COUNT.
       TRY-NEXT-STEP-NO.
           MOVE STEP-COUNT TO EX-SPOOL-STEP.
           MOVE EX-SPOOL TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
             IF (EX-SPOOL-NAME = "REPOEX")
                GO TO EXIT-CREATE-SPOOL-DIR
             ELSE
             IF STEP-COUNT < 999
                ADD 1 TO STEP-COUNT
                GO TO TRY-NEXT-STEP-NO.

       EXIT-CREATE-SPOOL-DIR.
           EXIT.

      ******************************************************************
       CREATE-WK-FILE SECTION.
      ******************************************************************

      *  CREATE WK-FILE, IF RUNNING FROM EOCRON, WORK FILE IN /USR/TMP
      *  WAS NOT CREATED IN LONPA0

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.

      * WK-FILE FOR LOAN PROCESSING USED PROCESS ID XL(PROCESS ID)

           MOVE PROCESS-ID-BUF TO USER-PID TEMP-PID.
           MOVE 9 TO IO-FG.
           PERFORM OPEN-IT.
           MOVE WK-IFN TO E-FILE.
       OPEN-WK-OUTPUT.
           OPEN OUTPUT WK-FILE.
           IF ( IO-FG = 8 )
              GO TO OPEN-WK-OUTPUT.
           IF ( IO-FG = 7 )
              PERFORM CLOSE-WK-FILE
              GO TO OPEN-WK-OUTPUT.
           PERFORM CLOSE-WK-FILE.

      *********************************
      *   NAME: CDV-VERIFY
      *   DESC: VERIFY LP-TRCD & YIELD SUBSCRIPT
      *   IN  : LP-TRCD, CDV-NN
      *   OUT : CDV-SUB; 0 = NOT_FOUND
      *********************************
       CDV-VERIFY SECTION.
           MOVE 0 TO CDV-SUB.
       CDV-VERIFY-NEXT.
           ADD 1 TO CDV-SUB.
           MOVE CDV-CD(CDV-SUB) TO STAT.
           IF STAT = LP-TRCD
              GO TO CDV-VERIFY-EXIT.
           IF STAT NOT = " "
              GO TO CDV-VERIFY-NEXT.
           MOVE 0 TO CDV-SUB.
       CDV-VERIFY-EXIT.
           EXIT.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/DEFPOL.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBLP/LPLPCL.CPY".
       COPY "LIBLP/LPLXGCL.CPY".

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/RMFILE.CPY".
       GET-SPR.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           PERFORM READ-SP1-FILE.
           MOVE SP1-KEY TO HOLD-SP1-KEY.
       CALC-HOLD-LN-ACCERN.
           MOVE LN-EARN-ORDER(1) TO SUB.
           IF SUB > 3
              COMPUTE HOLD-LN-ACCERN(1) = LN-ACCERN(1) +
                                          LXE-EARN(SUB - 3)
           ELSE
             COMPUTE HOLD-LN-ACCERN(1) = LN-ACCERN(1) + LP-EARNED(SUB).

           MOVE LN-EARN-ORDER(2) TO SUB.
           IF SUB > 3
              COMPUTE HOLD-LN-ACCERN(2) = LN-ACCERN(2) +
                                          LXE-EARN(SUB - 3)
           ELSE
             COMPUTE HOLD-LN-ACCERN(2) = LN-ACCERN(2) + LP-EARNED(SUB).

           MOVE LN-EARN-ORDER(3) TO SUB.
           IF SUB > 3
              COMPUTE HOLD-LN-ACCERN(3) = LN-ACCERN(3) +
                                          LXE-EARN(SUB - 3)
           ELSE
             COMPUTE HOLD-LN-ACCERN(3) = LN-ACCERN(3) + LP-EARNED(SUB).

           MOVE LN-EARN-ORDER(4) TO SUB.
           IF SUB > 3
              COMPUTE HOLD-LN-ACCERN(4) = LN-ACCERN(4) +
                                          LXE-EARN(SUB - 3)
           ELSE
             COMPUTE HOLD-LN-ACCERN(4) = LN-ACCERN(4) + LP-EARNED(SUB).

           MOVE LN-EARN-ORDER(5) TO SUB.
           IF SUB > 3
              COMPUTE HOLD-LN-ACCERN(5) = LN-ACCERN(5) +
                                          LXE-EARN(SUB - 3)
           ELSE
             COMPUTE HOLD-LN-ACCERN(5) = LN-ACCERN(5) + LP-EARNED(SUB).

           MOVE LN-EARN-ORDER(6) TO SUB.
           IF SUB > 3
              COMPUTE HOLD-LN-ACCERN(6) = LN-ACCERN(6) +
                                          LXE-EARN(SUB - 3)
           ELSE
             COMPUTE HOLD-LN-ACCERN(6) = LN-ACCERN(6) + LP-EARNED(SUB).

       READ-EARNINGS.
      * GET THE LXE-EARN'S FOR THIS REVERSAL
           PERFORM OPEN-LXE1-FILE.
           MOVE LP-BRNO   TO LXE-BRNO.
           MOVE LP-ACCTNO TO LXE-ACCTNO.
           MOVE LP-SEQNO  TO LXE-SEQNO.
           PERFORM READ-LXE1-FILE.
           PERFORM CLOSE-LXE1-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO LXE-EARN(1) LXE-EARN(2) LXE-EARN(3)
                        LXE-EARN(4) LXE-EARN(5) LXE-EARN(6)
                        LXE-EARN(7) LXE-WORKER LXE-WORKER2.
           IF REV-REBATE NOT = "Y"
              MOVE LXE-REC TO WK-LXE-REC.


      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       OPEN-LOG-FILE-EXTEND.
           OPEN EXTEND LOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-LOG-FILE-EXTEND.
           IF IO-FG = 7
              CLOSE LOG-FILE
              GO TO OPEN-LOG-FILE-EXTEND.

       OPEN-LOG-FILE-OUTPUT.
           OPEN OUTPUT LOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-LOG-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE LOG-FILE
              GO TO OPEN-LOG-FILE-OUTPUT.
       OPEN-LOG-FILE.
           OPEN INPUT LOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-LOG-FILE.
           IF IO-FG = 7
              CLOSE LOG-FILE
              GO TO OPEN-LOG-FILE.
       WRITE-LOG-FILE.
           WRITE LOG-REC.
       CLOSE-LOG-FILE.
           CLOSE LOG-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLXGGS_SQL.CPY".
       COPY "LIBLP/LPLXEGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPEDPATHIO.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGL/GLGI1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLXE1RN.CPY".
       COPY "LIBLP/LPLXG1RN.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBGB/GBRC2RN.CPY".
       COPY "LIBLP/LPOPIN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

       END-PROGRAM.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.

           MOVE RETURN-STATUS TO RETURN-CODE.

           EXIT PROGRAM RETURNING RETURN-CODE.
